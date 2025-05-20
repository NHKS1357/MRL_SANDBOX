
rm(list = ls())

# List of packages and their required versions
pkg_versions <- c(
  "data.table" = "1.16.2",
  "openxlsx"   = "4.2.7.1",
  "writexl"    = "1.5.1",
  "logr"       = "1.3.8",
  "common"     = "1.1.3",
  "glue"       = "1.8.0",
  "rlang"      = "1.1.4",
  "haven"      = "2.5.4",
  "stringdist" = "0.9.12",
  "readxl"     = "1.4.3",
  "tidyr"      = "1.3.1",
  "purrr"      = "1.0.2",
  "lubridate"  = "1.9.3",
  "stringr"    = "1.5.1",
  "dplyr"      = "1.1.4",
  "phonics"    = "1.3.10"
)
library(tools)
# Install each package at the specified version if not already installed at that version
for(pkg in names(pkg_versions)) {
  required_version <- pkg_versions[[pkg]]
  is_installed <- pkg %in% rownames(installed.packages())
  current_version <- if (is_installed) as.character(packageVersion(pkg)) else NA
  if (!is_installed || current_version != required_version) {
    message(sprintf("Installing %s version %s...", pkg, required_version))
    remotes::install_version(pkg, version = required_version, upgrade = "never", quiet = TRUE)
  }
}

# Load all packages
invisible(lapply(names(pkg_versions), library, character.only = TRUE))

f_exc <- "N"
run_from_qa <- "Y"
use_links_macro <- "Y"

dir_name <- "j3l_mc_ezef"
trial <- toupper(sub(".*_", "", dir_name))    
comp <- "ly3819469"
run_date <- Sys.Date()
mrl_spec <- "MRL_SPEC"

common_path <- glue("/lillyce/qa/{comp}/{dir_name}/common")
prelock_path <- glue("/lillyce/qa/{comp}/{dir_name}/prelock")
dir.create(file.path(prelock_path, "programs/oversight/MRL_R/log"), showWarnings = FALSE, recursive = TRUE)
logfile_path <- file.path(prelock_path,"programs/oversight/MRL_R/log")
file_main <- glue("MAIN_MRL_{trial}_{run_date}.log") #06MAY2025 - CHANGE1#

log_file <- file.path(logfile_path,file_main)
lf <- log_open(log_file)

log_print("Starting SAS dataset import process.") #Give more meaningful Text - 06MAY2025#

dir.create(file.path(prelock_path, "programs/oversight/MRL_R"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(prelock_path, "programs/oversight/MRL_R/spec"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(prelock_path, "programs/oversight/MRL_R/data"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(prelock_path, "programs/oversight/MRL_R/programs"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(prelock_path, "programs/oversight/MRL_R/references"), showWarnings = FALSE, recursive = TRUE)

dir.create(file.path(common_path, "mDVP_R_Reports"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(common_path, "mDVP_R_Reports/MRL_R"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(common_path, "mDVP_R_Reports/MRL_R/input_files"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(common_path, "mDVP_R_Reports/MRL_R/output"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(common_path, "mDVP_R_Reports/MRL_R/reviewed_files"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(common_path, "mDVP_R_Reports/MRL_R/duplicates_export"), showWarnings = FALSE, recursive = TRUE)

out_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/output")
ip_file_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/input_files")
rev_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/reviewed_files")
dup_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/duplicates_export")
ds_lib <- file.path(prelock_path, "programs/oversight/MRL_R/data")
prgm_path <- file.path(prelock_path, "programs/oversight/MRL_R/programs")
spec_path <- file.path(prelock_path, "programs/oversight/MRL_R/spec")
gbl_qa_prgm_path <- "/lillyce/qa/general/other/diva/programs/MRL_Standard_Listings"

#Step 1: Getting data into work environment
source(file.path(prgm_path, "alsc.r"))
source(file.path(prgm_path,"Utility_macro.r"))

spec_mrl <- file.path(spec_path, paste0(trial,"_",mrl_spec,".xlsx"))
sheets <- excel_sheets(spec_mrl)

# Read all sheets into a list
all_sheets <- lapply(sheets, function(X) read_excel(spec_mrl, sheet = X))

listings_to_include <-readxl::read_excel(file.path(spec_path, 
                    base::paste0(toupper(trial), "_",mrl_spec,".xlsx")), 
                    sheet = 'Spec')

listings_to_include <- listings_to_include %>%
  rename(des = Description) %>%
  mutate(Description = gsub("0A", "", des)) %>%
  filter(Include=="X") %>% 
  select(-des) 

# Ensure column names are in uppercase for comparison
listings_to_include <- listings_to_include %>% 
  mutate(Listing = toupper(trimws(Listing)))  

# Create two separate datasets
listings_to_include_feedback <- listings_to_include %>% 
  filter(toupper(Listing) == "D_FEEDBACK")

# Extract listings into a vector
list1 <- listings_to_include$Listing

sources <- listings_to_include$Source
# Concatenate listing and source with "-"
src1 <- paste0(listings_to_include$Listing, "-", listings_to_include$Source)

# Replace commas with spaces in com_col_list
ccl1 <- str_replace_all(listings_to_include$com_col_list, ",", " ")

# Remove "STATUS" from com_col_list after replacing commas with spaces
ccl_new1 <- str_replace_all(toupper(ccl1), "STATUS", "")

# Construct key with "catx('|', key)"
key1 <- paste0("catx('|',", listings_to_include$Key, ")")

# Count distinct listings
tot_list <- n_distinct(listings_to_include$Listing)

# Format skip_comparison values into a quoted string
skip_comp1 <- paste0("'", str_replace_all(listings_to_include$skip_comparison, ",", "','"), "'")

input_files_md <- readxl::read_excel(file.path(spec_path, 
                base::paste0(toupper(trial), "_",mrl_spec,".xlsx")), 
                sheet = 'input_files')
input_files_md <- input_files_md %>% filter(read == "X")
#Step2: Preprocessing the data and finding dups 
data_preprocessing <- function(){
  
  source(file.path(prgm_path, "read_input_files.r"))
  source(file.path(prgm_path,"mrl_preprocessing.r"))
  n_to_c()
}

data_preprocessing()


#Step3: Process to generate report

source(file.path(prgm_path, "spec_to_listing.r"))

source(file.path(prgm_path, "export_dups_in_excel.r"))

source(file.path(prgm_path, "get_pdate.r"))


prv_report_does_not_exist <- function() {
  if (f_exc == "N") {
    stop_exec <<- ifelse(file.exists(file.path(rev_path, 
                                               base::paste0(toupper(trial), 
                                                            "_MRL_", 
                toupper(prv_date), ".xlsx"))), "N", "Y")
    cat(stop_exec, "\n")
  } else {
    stop_exec <<- "Y" #1st execution and 2nd with no prv file
    cat(stop_exec, "\n")
  }
}
prv_report_does_not_exist()


# 
#   if (stop_exec == "N"){#2nd run onwards when prv file exists
#     source(file.path(prgm_path, "prv_report.r"))
#     source(file.path(prgm_path, "get_comments.r"))
#     source(file.path(prgm_path, "get_status.r"))
#     source(file.path(prgm_path, "gen_report.r"))
#   }else{
#     
#     if(f_exc=="Y"){
#       source(file.path(prgm_path, "gen_report.r")) #1st execution
#     }
#     if(f_exc=="N"){
#       message("PRV report does not exist. please check") 
#       # 2nd run on wards with no previous file
#     }
#     
#   }
  

if (stop_exec=="Y" ) {
  if (f_exc=="Y"){
    source(file.path(prgm_path, "gen_report.r")) #1st execution
  }
  if (f_exc=="N"){
    message("PRV report does not exist. please check") 
    # 2nd run on wards with no previous file
  }
}

if (stop_exec=="N"){
  source(file.path(prgm_path, "prv_report.r"))
}

if (stop_exec=="N"){
  source(file.path(prgm_path, "get_comments.r"))
}

if (stop_exec=="N"){
  source(file.path(prgm_path, "get_status.r"))
}

if (stop_exec=="N"){
  source(file.path(prgm_path, "gen_report.r"))
}

log_close()

