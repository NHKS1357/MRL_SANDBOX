
options(warn = -1)

#################################################  

if (f_exc == "N"){
  
  current_date <- toupper(format(Sys.Date(), "%d%b%Y"))
  listings <- sub("^d_", "", filtered_listings$Listing) # Example listings
  
  for(i in seq_along(listings)) {
    listing_name <- listings[i]
    sheet_name <- listing_name
    
    # Handle Feedback special case
    if(toupper(sheet_name) == "FEEDBACK") {

      current_df <- get(paste0(toupper(sheet_name),"_WC")) 

      assign(paste0(sheet_name, "_WC_WS"), current_df, envir = .GlobalEnv)
      next  # Skip to next iteration
    }
    
    # Construct dataset names
    prev_df_name <- paste0(toupper(sheet_name),"_",toupper(prv_date))# "_", prv_date
    current_df_name <- paste0(toupper(sheet_name), "_WC")
    
    # Check if previous dataset exists
    if(exists(prev_df_name, envir = .GlobalEnv)) {
      current_df <- get(current_df_name, envir = .GlobalEnv)
      prev_df <- get(prev_df_name, envir = .GlobalEnv)
      colnames(prev_df) <- gsub("\r\n", "_", colnames(prev_df))
      colnames(prev_df) <- gsub(" ", "_", colnames(prev_df))
      colnames(current_df) <- gsub(" ", "_", colnames(current_df))
      names(prev_df) <- toupper(names(prev_df))
      current_df <- current_df %>%
        mutate(across(where(is.Date), as.character))
      
      current_df <- current_df %>%
        mutate(REPORT_STATUS = case_when(
          !KEY %in% prev_df$KEY ~ "New",        # KEY is in current_df but not in prev_df
          KEY %in% prev_df$KEY & !KEY %in% current_df$KEY ~ "Removed", # KEY is in prev_df but not in current_df
          TRUE ~ "Modified"                # Default case (no change)
        ))
      
      # View the updated dataset
      
      # Merge current and previous data
      merged_df <- current_df %>%
        left_join(prev_df, by = "KEY", suffix = c("", "_PRV"))
      
      
      # Get columns to compare from spec data
      skip_comp_vector <- toupper(gsub("'", "", skip_comp1[[i]]) %>% strsplit(",") %>% unlist())
      spec_cols <- get(paste0(sheet_name, "_SPEC"), envir = .GlobalEnv) %>%
        filter(!is.na(keep))
      spec_cols <- spec_cols %>%
        mutate(final_labels = coalesce(labels, variables)) %>%
        select(variables, labels, final_labels, keep)
      spec_cols <- spec_cols$final_labels
      spec_cols <- gsub(" ", "_", spec_cols)
      spec_cols <- toupper(spec_cols)
      
      # Calculate comp_cols
      comp_cols <- setdiff(spec_cols, skip_comp_vector)
      
      for (col in comp_cols) {
        prv_col <- paste0(col, "_PRV")
        flag_col <- paste0(col, "_flag")
        # Check if both columns exist in merged_df
        if (all(c(col, prv_col) %in% names(merged_df))) {
          merged_df <- merged_df %>%
            mutate(
              !!flag_col := ifelse(
                REPORT_STATUS == "Modified" &
                  !is.na(.data[[col]]) & !is.na(.data[[prv_col]]) & .data[[col]] != .data[[prv_col]],
                "Y",
                "N"
              )
            )
        }
      }
      
      
      
      # Cleanup and finalize
      final_df <- merged_df %>%
        select(-ends_with("_PRV")) %>% #, -source, -source_PRV
        relocate(REPORT_STATUS, .before = 1)
      
      # Identify columns ending with '_flag'
      flag_cols <- grep("_flag$", names(final_df), value = TRUE)
      
      # Update REPORT_STATUS while ignoring rows with NA in _flag columns
      final_dataset <- final_df %>%
        mutate(
          REPORT_STATUS = ifelse(
            REPORT_STATUS == "Modified" & 
              rowSums(select(., all_of(flag_cols)) == "N", na.rm = TRUE) == length(flag_cols),
            "No change",  # All `_flag` columns are "N" and no NA values
            REPORT_STATUS  # Retain existing values
          )
        )
      
      final_dataset <- final_dataset %>%
        relocate(REPORT_STATUS, .before = 1) %>%  # Move REPORT_STATUS to first
        relocate(KEY, .after = last_col())         # Move KEY to last
      
      
      # Save results to global environment
      assign(paste0(toupper(sheet_name), "_WC_WS"), final_dataset, envir = .GlobalEnv)
      assign(paste0(toupper(sheet_name), "_r_", current_date),
             filter(final_dataset, REPORT_STATUS == "Removed"),
             envir = .GlobalEnv)
      
    } else {
      # Handle case with no previous data
      current_df <- get(current_df_name, envir = .GlobalEnv) %>%
        mutate(REPORT_STATUS = "New")
      
      assign(paste0(toupper(sheet_name), "_WC_WS"), current_df, envir = .GlobalEnv)
    }
  }
  message("Previous MRL Report dated ", prv_date, 
          " exists - STEP: COLLECTING STATUS") 
}







