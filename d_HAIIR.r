#' @title Filter Hypersensitivity Reaction Data
#' 
#' @description
#' This function filters a dataset containing hypersensitivity reaaction data from clinicl trials.
#' The function first verifies the existence of the required column \code{haiirr1} in the input dataframe.
#' It then filters the data to include only rows where the \code{formeid} column matches any of the provided form ids.
#' 
#' @author Lucus Wassira
#' @maintainer Lucus Wassira
#' @date  2025-03-20
#' @version 1.0
#' @repository https://github.com/manishboyina-lilly/SAS2R_MRL/tree/main
#' 
#' @param df A string specifying the name of the dataset to retrieve from the global environment. Default is "HAIIR1001".
#' @param form_ids A vector of string values specifying the target form IDS to filter on.
#' 
#' @return A dataframe (.rds) with distinct rows that meet the filtering criteria, or \code{NULL} if the required column is missing.
#' 
#' @examples
#' \dontrun{
#' # assuming a dataframe \code{HAIIR1001} exists in the global environment:
#' filtered_data <- filter_hypersensitivity_data(c("HAIIR1001_LV1"))
#' }
#' 
#' @export
get_global_dataset <- function(df) {
  if (!exists(df, envir = .GlobalEnv)) {
    message(glue::glue("Dataset '{df}' not found in the global environment. Exiting function."))
    return(NULL)
  }
  return(get(df, envir = .GlobalEnv))
}


filter_hypersensitivity_data <- function(form_ids, df = "HAIIR1001") {
  data <- get_global_dataset(df)
  if (is.null(data)) return(NULL) 
  
  colnames(data) <- tolower(colnames(data))
  
  if (!"haiirr1" %in% colnames(data)) {
    message(glue::glue("Column 'HAIIRR1' not found in {dataset_name}. Exiting function."))
    return(NULL)
  }
  
  filtered_data <- data[data$formeid %in% form_ids & data$row_ilb != "Y", ]
  haiir_out <- unique(filtered_data)
  colnames(haiir_out) <- toupper(colnames(haiir_out))
  
  return(haiir_out)
}


# usage 
HAIIR <- filter_hypersensitivity_data("HAIIR1001_LV1")