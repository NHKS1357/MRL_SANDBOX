

if (f_exc == "N") {
  
  filtered_listings <- listings_to_include %>%
    mutate(SheetName = str_remove(Listing, "^d_")) %>%  # Remove "d_" prefix
    filter(Include == "X") %>%                          # Keep only included sheets
    mutate(com_col_list_clean = map(com_col_list, ~ {
      if(.x == "Not Applicable") character(0) 
      else str_split(.x, ",", simplify = TRUE) %>% as.character()
    }))
  
  all_sheets <- excel_sheets(excel_file)
  
  for (i in seq_along(all_sheets)) {
    listing <- all_sheets[i]
    com_dataset <- paste0(toupper(listing), "_COM")

        # Check if the comments dataset exists
    if (exists(com_dataset, envir = .GlobalEnv)) {
      # Process non-Feedback datasets
      if (toupper(listing) != "FEEDBACK") {
        # Sort original and comments datasets by KEY
        original <- get(toupper(listing)) %>% arrange(KEY)
        comments <- get(com_dataset) %>% arrange(KEY)
        colnames(comments) <- toupper(colnames(comments))
        
        # Merge original and comments datasets
        merged <- left_join(original , comments, 
                            by = "KEY")  %>% distinct()
                            # suffix = c("", "_COM"))

        assign(paste0(toupper(listing), "_WC"), merged, envir = .GlobalEnv)
      }else{
        # Create new WC dataset if no comments exist
        base_df <- get(toupper(com_dataset))
        #mutate(across(all_of(ccl1[[i]]), as.character))
        assign(paste0(toupper(listing), "_WC"), base_df, envir = .GlobalEnv)
      }
      
    } else {
      # Create new WC dataset if no comments exist
      base_df <- get(toupper(com_dataset))
        #mutate(across(all_of(ccl1[[i]]), as.character))
      assign(paste0(toupper(listing), "_WC"), base_df, envir = .GlobalEnv)
    }
  }
  message("Previous MRL Report dated ", prv_date, 
          " exists - STEP: COLLECTING COMMENTS")
}


















