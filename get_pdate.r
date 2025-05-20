if (f_exc == "N") {
  # List files in the directory
  filenames <- list.files(ds_lib, full.names = TRUE)
  # Extract date from filenames
  FILE_R1 <- data.frame(FNAME = basename(filenames)) %>%
    mutate(
      PDATE = str_extract(FNAME, "\\d{2}[A-Za-z]{3}\\d{4}"),
      PDATE = dmy(PDATE)
    )  %>%
    filter(!is.na(PDATE), PDATE != Sys.Date()) %>%
    distinct(FNAME, .keep_all = TRUE) %>%
    arrange(PDATE)
  
  # Get the most recent date (excluding today)
  if (nrow(FILE_R1) > 0) {
    prv_date <- format(max(FILE_R1$PDATE), "%d%b%Y")
    # prv_file <- file_r1$fname[which.max(file_r1$pdate)]
    
    cat("Previous date:", prv_date, "\n")
    # cat("Previous file:", prv_file, "\n")
  } else {
    cat("No previous dates found.\n")
  }
} else {
  cat("First execution - STEP : Executing get_date macro\n")
}
