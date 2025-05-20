

read_input_files <- function() {
  # Ensure necessary variables exist
  if (!exists("ip_file_path") || !exists("input_files_md") || !exists("trial")) {
    stop("ERROR: Required variables (ip_file_path, input_files_md, trial) are missing.")
  }
  
  # Read input files
  input_files <- data.frame(FNAME = list.files(ip_file_path, full.names = FALSE), stringsAsFactors = FALSE)
  
  # Filter input files based on metadata
  included_ip_files <- input_files %>%
    filter(toupper(file_path_sans_ext(FNAME)) %in% toupper(input_files_md$filename)) %>%
    distinct()
  
  if (!"filename" %in% colnames(input_files_md)) {
    stop("ERROR: Column 'FILENAME' not found in input_files_md")
  }
  
  included_ip_files <- included_ip_files %>%
    mutate(FNAME = str_to_upper(trimws(file_path_sans_ext(basename(FNAME)))))  
  
  input_files_md <- input_files_md %>%
    mutate(filename = str_to_upper(trimws(filename)))
  
  input_file_final <- included_ip_files %>%
    mutate(extension = tools::file_ext(FNAME)) %>%
    inner_join(input_files_md, by = c("FNAME" = "filename"))
  
  # Extract relevant file information
  fname_list <- input_file_final$FNAME
  fext_list <- input_file_final$extension
  fdsn_list <- input_file_final$dsn
  tot_file <- length(fname_list)
  
  # If no files, stop execution
  if (tot_file == 0) {
    stop("No files to process. Check if metadata file matches input directory files.")
  }
  
  # Initialize list to store sheet data
  sheet_list_to_save <- list()
  
  for (i in seq_len(tot_file)) {
    file_path <- file.path(ip_file_path, paste0(fname_list[i], ".xlsx"))
    
    if (!file.exists(file_path)) {
      message(paste("The file", file_path, "doesn't exist"))
      next  # Skip to the next file
    }
    
    file_name <- file_path_sans_ext(basename(file_path))
    
    if (file_name == paste0(trial, "_MRL_INPUT")) {
      data_index <- readxl::read_excel(file_path, sheet = "Index")
      included_sheets <- data_index %>%
        filter(Include == "X") %>%
        pull(Sheet)
      
      tot_filex <- length(included_sheets)
      
      for (j in seq_len(tot_filex)) {
        sheet_name <- included_sheets[j]
        temp_sheet_df <- readxl::read_excel(file_path, sheet = sheet_name)
        temp_sheet_df <- temp_sheet_df %>% filter(!is.na(trial))
        
        sheet_list_to_save[[sheet_name]] <- temp_sheet_df
        assign(sheet_name, temp_sheet_df, envir = .GlobalEnv)
      }
    } else {
      temp_df <- readxl::read_excel(file_path)
      assign(fdsn_list[i], temp_df, envir = .GlobalEnv)
    }
    
  }
  
  return()
}


read_input_files()





