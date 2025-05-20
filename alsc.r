


# Define the source directory (equivalent to libname)
source_dir <- "/lillyce/qa/ly3819469/j3l_mc_ezef/prelock/data/raw/shared"

sas_files <- list.files(source_dir, pattern = "\\.sas7bdat$", full.names = TRUE, ignore.case = TRUE)

#either give only select datasets or exclude datasets
select_datasets <- c()
exclude_datasets <- c()

sas_dataset_names <- toupper(tools::file_path_sans_ext(basename(sas_files)))

# Logic to filter sas_files
if (length(select_datasets) > 0) {
  # If select_datasets is not empty, take only those datasets
  filtered_datasets <- sas_files[sas_dataset_names %in% toupper(select_datasets)]
} else if (length(exclude_datasets) > 0) {
  # If exclude_datasets is not empty, exclude those datasets
  filtered_datasets <- sas_files[!sas_dataset_names %in% toupper(exclude_datasets)]
} else {
  # If both are empty, take all datasets
  filtered_datasets <- sas_files
}

for (file in filtered_datasets) {
  dataset_name <- tools::file_path_sans_ext(basename(file))# Get the dataset name without extension
  dataset_name <- toupper(dataset_name)   # Convert to uppercase
  log_print(paste0("Attempting to load: ", dataset_name))
  assign(dataset_name, haven::read_sas(file), envir = .GlobalEnv) # Read the SAS dataset
}



#print the names of all loaded datasets
#print(names(datasets))
#print("All the required datasets are uploaded to work environment")

