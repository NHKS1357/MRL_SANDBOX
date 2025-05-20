

if (f_exc == "Y"){
  
  output_filename <- paste0("EZEF_MRL_", format(Sys.Date(), "%d%b%Y"), ".xlsx")
  listings_to_include <-readxl::read_excel(file.path(spec_path, base::paste0(toupper(trial), "_MRL_SPEC.xlsx")), sheet = 'Spec') %>% 
    filter(!is.na(Include))
  output_file <- paste0(out_path,"/",output_filename)
  current_date <- format(Sys.Date(), "%d%b%Y")
  # Extract study name dynamically from the file path
  study_name <- strsplit(ds_lib, "/")[[1]][5]
  list_l <- listings_to_include$Listing
  
  # Step 1: List all .rds files in the data directory
  rds_files <- list.files(path = ds_lib, pattern = "\\.rds$", full.names = TRUE)
  r_files <- rds_files[grepl(current_date, rds_files)]
  file_names <- basename(r_files)
  
  # Remove date suffix and extension from file names for matching
  cleaned_file_names <- toupper(str_remove(file_names, "_\\d{2}\\w{3}\\d{4}\\.rds"))
  print(cleaned_file_names)
  # Remove "D_" prefix from listings_to_include$Listing for matching
  cleaned_listing <- toupper(str_remove(LISTINGS_TO_INCLUDE$Listing, "^d_"))
  
  matching_indices <- match(cleaned_file_names, cleaned_listing)
  print(matching_indices)
  valid_indices <- !is.na(matching_indices)
  sorted_r_files <- r_files[order(matching_indices[valid_indices])]
  print(sorted_r_files)
  
  
  com_col_list <- listings_to_include$com_col_list
  print(com_col_list[5])
  # Check if any .rds files found
  if (length(r_files) == 0) {
    stop("No .rds files found in the specified data path.")
  }
  
  # Step 2: Read each .rds file into R as a dataset
  datasets <- list()
  for (i in seq_along(list_l)) {
    
    # Extract sheet name from filename (without date and extension)
    sheet_name <- sub("_\\d{2}[A-Za-z]{3}\\d{4}\\.rds$", "", basename(list_l[i]))
    
    # Read the .rds file
    data <- readRDS(sorted_r_files[i])
    
    # Extract corresponding column list and split into individual column names
    new_columns <- str_split(com_col_list[i], ",")[[1]] %>% str_trim()
    
    # Add new columns initialized with NA (or empty strings if preferred)
    for (col in new_columns) {
      data[[col]] <- NA_character_  # Use "" if you prefer empty strings instead of NA
    }
    
    data$REPORT_STATUS <- "New"
    data <- data %>% select(REPORT_STATUS, everything(), -KEY, KEY)
    
    # Store processed dataset in the list with sheet_name as key
    datasets[[sheet_name]] <- data
    
    cat("Loaded and updated dataset:", sheet_name, "\n")
  }
  
  wb <- createWorkbook()
  
  # Define styles
  title_style <- createStyle(fontSize = 18, textDecoration = "bold", halign = "left")
  header_style <- createStyle(fontSize = 11, textDecoration = "bold", halign = "center", fgFill = "#D9E1F2")
  green_fill_style <- createStyle(bgFill = "#90EE90")  # Green color for conditional formatting
  
  # Titles to be added at the top of each sheet
  current_date <- format(Sys.Date(), "%d%b%Y")
  first_date_of_month <- format(floor_date(Sys.Date(), unit = "month"), "%d%b%Y")
  titles <- c(
    paste("Study:", toupper(study_name)),
    "Report Name - Medical Review Listings",
    paste("Report Execution Date -", current_date),
    paste("Listing compared against : First Run - No Comparision")
  )
  
  # Loop through each dataset and add it to a new sheet with formatting
  for (sheet_name in names(datasets)) {
    
    clean_sheet_name <- sub("^d_", "", sheet_name)
    
    # Add worksheet with dataset name
    addWorksheet(wb, clean_sheet_name)
    
    # Merge cells for all titles into one row spanning across columns A:F (1:6)
    mergeCells(wb, clean_sheet_name, cols = c(1:8), rows = c(1))
    mergeCells(wb, clean_sheet_name, cols = c(1:8), rows = c(2))
    mergeCells(wb, clean_sheet_name, cols = c(1:8), rows = c(3))
    mergeCells(wb, clean_sheet_name, cols = c(1:8), rows = c(4))
    
    # Write each title into its respective merged row
    writeData(wb, clean_sheet_name, titles[1], startRow = 1, startCol = 1)
    writeData(wb, clean_sheet_name, titles[2], startRow = 2, startCol = 1)
    writeData(wb, clean_sheet_name, titles[3], startRow = 3, startCol = 1)
    writeData(wb, clean_sheet_name, titles[4], startRow = 4, startCol = 1)
    
    # Apply title style to all title rows
    addStyle(wb, clean_sheet_name, style = title_style,
             rows = c(1:4), cols = c(1:8), gridExpand = TRUE)
    
    # Write dataset starting from row after titles (row=7)
    writeDataTable(wb, clean_sheet_name, x = datasets[[sheet_name]], startRow = 7, startCol = 1,
                   tableStyle = "TableStyleMedium2")
    
    # Apply conditional formatting for rows where REPORT_STATUS == 'New'
    new_rows <- which(datasets[[sheet_name]]$REPORT_STATUS == "New")
    
    if (length(new_rows) > 0) {
      excel_rows <- new_rows + 7 # Account for header row
      
      conditionalFormatting(
        wb,
        sheet = clean_sheet_name,
        cols = seq_len(ncol(datasets[[sheet_name]])),
        rows = excel_rows,
        rule="TRUE",
        style=green_fill_style
      )
      
      cat("Conditional formatting applied on sheet:", clean_sheet_name, "\n")
    }
  }
  # Save workbook to output path
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("Workbook successfully saved at:", output_file)
  
  
}


#############################################################################

if (f_exc == "N") {
  
  study_name <- strsplit(ds_lib, "/")[[1]][5]
  # Titles to be added
  current_date <- format(Sys.Date(), "%d-%b-%Y")
  titles <- c(
    paste("Study:", toupper(study_name)),                      # Replace with dynamic study name if available
    "Report Name - Medical Review Listings",
    paste("Report Execution Date -", current_date),
    paste("Listing compared against:", prv_date, "dated Data")
  )
  
  # Define styles for conditional formatting
  title_style <- createStyle(fontSize = 18, textDecoration = "bold", halign = "left")
  green_style <- createStyle(fgFill = "#C6EFCE")  # Green for "New"
  yellow_style <- createStyle(fgFill = "#FFFF00")  # Yellow for "No change"
  red_style <- createStyle(fgFill = "#FFC7CE")  # Red for "Removed"
  orange_style <- createStyle(fgFill = "#FFD966")  # Orange for Modified flags
  header_style <- createStyle(fgFill = "#ADD8E6", textDecoration = "bold", wrapText = TRUE, halign = "center")  # Gold for column headers
  date_style <- createStyle(numFmt = "yyyy-mm-dd")  # Date format
  
  # Define the output path and filename
  # Use the current working directory as output path
  output_filename <- paste0("EZEF_MRL_", format(Sys.Date(), "%d%b%Y"), ".xlsx")
  output_file <- file.path(out_path, output_filename)
  
  # Ensure the output directory exists
  if (!dir.exists(out_path)) {
    dir.create(out_path, recursive = TRUE)
  }
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Get list of datasets ending with "_WC_WS" in the environment
  datasets <- ls(pattern = "_WC_WS$")
  reordered_datasets <- datasets[match(tolower(listings), tolower(sub("_WC_WS$", "", datasets)))]
  # Iterate through all datasets
  for (dataset_name in reordered_datasets) {
    
    #if (dataset_name == "FEEDBACK_WC_WS") next
    # Load the dataset
    data <- get(dataset_name)
    
    # Identify flag columns
    flag_cols <- grep("_flag$", names(data), value = TRUE)
    
    # Create a copy of the data without flag columns
    data_no_flags <- data %>%
      select(-one_of(flag_cols))
    
    sheet_name <- gsub("_WC_WS$", "", dataset_name)
    
    # Add a worksheet named after the dataset
    addWorksheet(wb, sheetName = sheet_name)
    
    
    for (i in seq_along(titles)) {
      mergeCells(wb, sheet = sheet_name, cols = 1:20, rows = i)  # Merge columns A to H for each title row
      writeData(wb, sheet = sheet_name, x = titles[i], startRow = i, startCol = 1, colNames = FALSE)  # Write titles only in column A
    }
    
    # Apply title style to all title rows
    addStyle(wb, sheet_name, style = title_style, rows = 1:length(titles), cols = 1:8, gridExpand = TRUE)
    
    
    # Write the dataset starting from row 7 (after titles)
    writeData(wb, sheet = sheet_name, x = data_no_flags, startRow = 7,colNames = TRUE)
    
    addStyle(wb, sheet_name, style = header_style, rows = 7, cols = 1:ncol(data_no_flags), gridExpand = TRUE)
    
    addFilter(wb, sheet = sheet_name, rows = 7, cols = 1:ncol(data_no_flags))
    
    
    # Apply conditional formatting based on REPORT_STATUS
    if ("REPORT_STATUS" %in% colnames(data)) {
      # Green for "New"
      new_rows <- which(data$REPORT_STATUS == "New") + 7
      addStyle(wb, sheet_name, style = green_style, rows = new_rows, cols = 1:ncol(data_no_flags), gridExpand = TRUE)
      
      # Yellow for "No change"
      no_change_rows <- which(data$REPORT_STATUS == "No change") + 7
      addStyle(wb, sheet_name, style = yellow_style, rows = no_change_rows, cols = 1:ncol(data_no_flags), gridExpand = TRUE)
      
      # Red for "Removed"
      removed_rows <- which(data$REPORT_STATUS == "Removed") + 7
      addStyle(wb, sheet_name, style = red_style, rows = removed_rows, cols = 1:ncol(data_no_flags), gridExpand = TRUE)
      
      # Orange for Modified flags
      flag_cols <- grep("_flag$", colnames(data), value = TRUE)
      for (flag_col in flag_cols) {
        base_col <- gsub("_flag$", "", flag_col)  # Get base column name
        if (base_col %in% colnames(data_no_flags)) {
          modified_rows <- which(data[[flag_col]] == "Y" & data$REPORT_STATUS == "Modified") + 7
          col_index <- which(colnames(data_no_flags) == base_col)
          addStyle(wb, sheet_name, style = orange_style, rows = modified_rows, cols = col_index, gridExpand = TRUE)
        }
      }
    }
    
  }
  
  # Save the workbook with all sheets in the output file
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("Excel file saved successfully at:", output_file)
  
  
}


