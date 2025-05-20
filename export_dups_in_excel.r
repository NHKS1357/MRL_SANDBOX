#Total Time Taken for code generation : 0:00:13.561573
#GENAI LLM used : gpt-4 

#START-CHUNK:1 UID:e94caa8f-fd2a-4f2e-9452-5c6cb3e23194

#another block
exportvar_dups_in_excel <- function() {
  for (kk in 1:tot_list) {
    # Extract the name after the underscore
    lname <- substr(list_k[[kk]], regexpr("_", list_k[[kk]]) + 1, nchar(list_k[[kk]]))
    
    if (exists(toupper(lname))) {
      # Get the "_dup" dataset
      dup_data <- get(toupper(lname))
      data <- dup_data %>%
        group_by(KEY) %>%
        mutate(is_duplicate = n() > 1) %>%
        ungroup()
      
      # Create a new dataset with only the duplicates
      duplicates <- data %>% filter(is_duplicate)
      data <- data %>% select(-is_duplicate)
      duplicates <- duplicates %>% select(-is_duplicate)
      assign(paste0(toupper(lname), "_DUP"), duplicates)
      assign(paste0(toupper(lname)), data)
      
    }
      
    # Check if it has observations
    nobsll <- nrow(duplicates)
    
    if (nobsll != 0) {
      # Export the dataset to an Excel file
      file_name <- paste0(dup_path, "/", lname, "_dup_", Sys.Date(), ".xlsx")
      write_xlsx(duplicates, file_name)
      
      cat("Exported:", file_name, "\n")
    }
  }
  
}

exportvar_dups_in_excel()







