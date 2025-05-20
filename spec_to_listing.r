#Total Time Taken for code generation : 0:00:53.972955
#GENAI LLM used : gpt-4 

#START-CHUNK:1 UID:952a539e-76a7-409e-ad86-c3b82e713060
input_files_listings <- readxl::read_excel(file.path(spec_path, base::paste0(toupper(trial), "_MRL_SPEC.xlsx")), sheet = 'Spec')
input_files_listings <- input_files_listings %>% filter(Include == "X")
list_k <- input_files_listings %>%
  pull(Listing)
key <- input_files_listings %>%
  pull(Key)
key <- toupper(key)
# Print the extracted program list

#another block

for (k in seq_along(list_k)) {
  
  if (strsplit(list_k[k], "_")[[1]][1] == "d") {
    # Check if the file exists in the primary path
    if (file.exists(file.path(prgm_path, paste0(list_k[k], ".r")))) {
      source(file.path(prgm_path, paste0(list_k[k], ".r")))
    } else {
      # If not found in the primary path, include it from the global QA program path
      source(file.path(gbl_qa_prgm_path, paste0(list_k[k], ".r")))
    }
  } else {
    cat("Skipping invalid element at position:", k, "\n")
  }
  
  if (strsplit(list_k[k], "_")[[1]][1] == "m") {
    # Include the file from gbl_prgm_path
    source(file.path(gbl_prgm_path, paste0(list_k[k], ".r")))
  }
  

  
  # Extract the sheet name
  sheet_name <- substr(list_k[[k]], regexpr("_", list_k[[k]]) + 1, nchar(list_k[[k]]))
  
  # Construct the spec_name
  spec_name <- paste0(sheet_name, "_SPEC")
  
  assign(spec_name, read_xlsx(spec_mrl, sheet = sheet_name))

  spec_data <- get(spec_name)
  spec_data <- spec_data %>%
    mutate(labels = ifelse(labels == "", variables, labels)) %>%
    filter(!is.na(keep))

  ds_var <- list(toupper(spec_data$variables))

  # Append the current date to the dataset name
  current_date <- format(Sys.Date(), "%d%b%Y")  # Equivalent to &sysdate9. in SAS
  output_filename <- paste0(sheet_name, "_", toupper(current_date)) #, ".xlsx"
  
  if (exists(toupper(substr(list_k[[k]], regexpr("_", list_k[[k]]) + 1, nchar(list_k[[k]]))))) {
    
    nobsl <- nrow(get(toupper(substr(list_k[[k]], regexpr("_", list_k[[k]]) + 1, nchar(list_k[[k]])))))
    print(nobsl)
    if (nobsl == 0) {
      ds_var <- unlist(ds_var)
      empty_data <- data.frame(KEY = "No observations found", stringsAsFactors = FALSE)
      empty_data[, ds_var] <- NA
      empty_data <- empty_data %>%
        select(KEY, everything())
      
      assign(toupper(sheet_name), empty_data, envir = .GlobalEnv)
      output_path <- file.path(ds_lib, paste0(output_filename, ".rds"))
      saveRDS(empty_data, file = output_path)
      cat("Dataset saved to:", output_path, "\n")
      
    } else {
      data <- get(toupper(substr(list_k[[k]], regexpr("_", list_k[[k]]) + 1, nchar(list_k[[k]]))))
      
      key_columns <- unlist(strsplit(key[[k]], ","))
      rename_vector <- setNames(spec_data$labels, spec_data$variables)
      
      missing_columns <- setdiff(names(data), names(rename_vector))
      
      rename_vector <- c(rename_vector, setNames(missing_columns, missing_columns))
      # Standardize column names in data and rename_vector
      names(data) <- toupper(names(data))
      names(rename_vector) <- toupper(names(rename_vector))
      
      # Convert spec_data$variables to uppercase, if needed
      vars_upper <- toupper(spec_data$variables)
      
      # Find which columns exist in data
      existing_vars <- vars_upper[vars_upper %in% names(data)]
      
      data <- data %>%
        mutate(KEY = do.call(paste, c(select(., all_of(key_columns)), sep = "|"))) %>%
        select(KEY, all_of(existing_vars)) %>% 
        distinct() %>%
        mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .))) %>% 
        relocate(KEY, .after = last_col()) # Move KEY column to the last position
      
      data <- data %>% rename_with(~ ifelse(!is.na(rename_vector[.x]), toupper(rename_vector[.x]), .x), .cols = names(data))

      data$KEY <- trimws(data$KEY)
      
      data <- data %>%
        mutate(KEY = str_replace_all(KEY, "\\bNA\\b", ""))
      #data[is.na(data)] <- ""
      
      assign(toupper(sheet_name), data, envir = .GlobalEnv)
      output_path <- file.path(ds_lib, paste0(output_filename, ".rds"))
      saveRDS(data, file = output_path)
      cat("Dataset saved to:", output_path, "\n")
      
      }
    
  } else {
    
    empty_data <- data.frame(KEY = "Either DS empty or Variables missing or DS missing", stringsAsFactors = FALSE)
    empty_data[, ds_var] <- NA
    empty_data <- empty_data %>%
      select(KEY, everything())
    empty_data[is.na(empty_data)] <- ""
    
    assign(toupper(sheet_name), empty_data, envir = .GlobalEnv)
    output_path <- file.path(ds_lib, paste0(output_filename, ".rds"))
    saveRDS(empty_data, file = output_path)
    cat("Dataset saved to:", output_path, "\n")

  }
  
  
  
}



















