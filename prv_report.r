
if (f_exc == "N") {
  # Define the path to the Excel file
  listings_to_include <-readxl::read_excel(glue(file.path(spec_path, "{toupper(trial)}_{mrl_spec}.xlsx")), sheet = 'Spec')
  
  excel_file <- file.path(rev_path, paste0(toupper(trial), "_MRL_", toupper(prv_date), ".xlsx"))
  
  # Read all sheet names from the Excel file
  sheet_names <- excel_sheets(excel_file)
  
  listings_to_include <- listings_to_include %>%
    mutate(Filtered_Listing = str_remove(Listing, "^d_")) %>% 
    filter(toupper(Include)=="X")
  # Filter sheets based on `listings_to_include`
  listings_prv <- tibble(memname = sheet_names)
  
  # Filter sheets that match `listings_to_include`
  prv_report <- listings_prv %>%
    filter(toupper(str_trim(memname)) %in% 
             toupper(str_trim(listings_to_include$Filtered_Listing)))
  prv_report_com <- prv_report %>%
    mutate(memname = toupper(str_trim(memname))) %>%
    inner_join(
      listings_to_include %>% mutate(Filtered_Listing = toupper(str_trim(Filtered_Listing))),
      by = c("memname" = "Filtered_Listing")
    )
  
  # Process regular sheets
  tot_com <- nrow(prv_report_com)
  
  for (p in seq_len(tot_com)) {
    sheet_name <- prv_report_com$memname[p]
    
    matched_sheet <- sheet_names[str_to_lower(sheet_names) == str_to_lower(sheet_name)]      
    # Import data from the Excel sheet
    data_com <- read_excel(excel_file, sheet = matched_sheet)
    #data_com[is.na(data_com)] <- ""
    data_com <- data_com %>%
      mutate(across(where(is.character), ~replace(., is.na(.), "")))
    
    assign(
      as.character(glue(paste0(toupper(matched_sheet), "_{prv_date}"))),
      data_com,
      envir = .GlobalEnv
    )
    
    #assign(glue(paste0(toupper(matched_sheet),"_{reviewed_date}"),data_com,envir = .GlobalEnv))
    # Get column names for processing
    com_tb <- colnames(data_com)
    
    expanded_com_col_list <- unlist(str_split(listings_to_include$com_col_list, ","))
    expanded_com_col_list <- toupper(str_trim(expanded_com_col_list))
    if (toupper(sheet_name) == "FEEDBACK") {
      com_tb1 <- com_tb[!str_detect(toupper(com_tb), "KEY")]
    } else {
      com_tb1 <- com_tb[com_tb %in% expanded_com_col_list]
    }
    
    
    # Keep only relevant columns
    if (toupper(sheet_name) == "FEEDBACK") {
      data_com <- data_com %>% select(all_of(com_tb1))
    } else {
      data_com <- data_com %>% 
        select(KEY, all_of(com_tb1)) %>%
        mutate(KEY_PRV = str_replace_all(KEY, "\\|","")) %>% 
        select(-KEY, everything(), KEY_PRV) 
    }
    
    assign(paste0(toupper(sheet_name), "_COM"), data_com, envir = .GlobalEnv)
  }
} else {
  cat("First execution - STEP: Executing prv_report step\n")
}






