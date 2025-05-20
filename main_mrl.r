

#another block
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)
library(readxl)
library(stringdist)
library(haven)
library(phonics)
library(rlang)
library(glue)
library(writexl)
library(openxlsx)
library(data.table)



# Please pay extra attention to below logical statements as we had seen some logical discrepencies in SAS to R code conversions for WHERE statements. Please refer to the below path to see similar examples.
options(warn = -1)

f_exc <- "N"
run_from_qa <- "Y"
use_links_macro <- "Y"

common_path <- "/lillyce/qa/ly3819469/j3l_mc_ezef/common"
prelock_path <- "/lillyce/qa/ly3819469/j3l_mc_ezef/prelock"


# Extract "jahw" from the path

dir_name <- tail(strsplit(common_path, "/")[[1]], 2)[1]
trial <- toupper(sub(".*_", "", dir_name))    

study <- tools::toTitleCase(stringr::str_split(common_path, "/")[[1]][4])

out_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/output")
ip_file_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/input_files")
rev_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/reviewed_files")
dup_path <- file.path(common_path, "mDVP_R_Reports/MRL_R/duplicates_export")
ds_lib <- file.path(prelock_path, "programs/oversight/MRL_R/data")
prgm_path <- file.path(prelock_path, "programs/oversight/MRL_R/programs")
spec_path <- file.path(prelock_path, "programs/oversight/MRL_R/spec/test")
#spec_path <- file.path(prelock_path, "programs/oversight/MRL_R/spec")
log_path <- file.path(prelock_path, "programs/oversight/MRL_R/log")
gbl_prgm_path <- "/lillyce/prd/general/other/diva/programs/MRL_Standard_Listings"
gbl_qa_prgm_path <- "/lillyce/qa/general/other/diva/programs/MRL_Standard_Listings"


#source("~/SAS2R_MRL/MRL/MRLs_converted_R/alsc.r")
source(file.path(prgm_path, "alsc.r"))

source(file.path(prgm_path,"Utility_macro.r"))

#source("/lillyce/prd/general/other/diva/programs/utility_macros/Utility_macro.r")
#source("~/SAS2R_MRL/MRL/MRLs_converted_R/Utility_macro.r")
spec_mrl <- file.path(spec_path, paste0(trial, "_MRL_SPEC_usethis.xlsx"))

sheets <- excel_sheets(spec_mrl)

# Read all sheets into a list
all_sheets <- lapply(sheets, function(X) read_excel(spec_mrl, sheet = X))

# Name the list elements with sheet names
names(all_sheets) <- sheets


listings_to_include <-readxl::read_excel(file.path(spec_path, base::paste0(toupper(trial), "_MRL_SPEC_usethis.xlsx")), sheet = 'Spec')

listings_to_include <- listings_to_include %>%
  rename(des = Description) %>%
  mutate(Description = gsub("0A", "", des)) %>%
  filter(!is.na(Include)) %>%
  select(-des) 
colnames(listings_to_include)

# Ensure column names are in uppercase for comparison
listings_to_include <- listings_to_include %>% 
  mutate(Listing = toupper(trimws(Listing)))  

# Create two separate datasets
listings_to_include_feedback <- listings_to_include %>% 
  filter(Listing == "D_FEEDBACK")

listings_to_include <- listings_to_include %>% 
  filter(Listing != "D_FEEDBACK")

#START-CHUNK:5 UID:4876aaa6-6c29-4674-8d80-626b78f07ee8
#another block
listings_to_include <- dplyr::bind_rows(listings_to_include, listings_to_include_feedback)
#END-CHUNK:5 UID:4876aaa6-6c29-4674-8d80-626b78f07ee8

#START-CHUNK:6 UID:559b834f-5529-4754-9a16-6ba25fc9fd83
colnames(listings_to_include)


# Extract listings into a vector
list1 <- listings_to_include$Listing

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


#another block
input_files_md <- readxl::read_excel(file.path(spec_path, base::paste0(toupper(trial), "_MRL_SPEC.xlsx")), sheet = 'input_files')
input_files_md <- input_files_md %>% filter(!is.na(read))


#another block
if (stringr::str_trim(run_from_qa) == "Y") {
  nobs_ifm <- nrow(input_files_md)
  if (nobs_ifm != 0) {
    source(file.path(gbl_qa_prgm_path, "read_input_files.r"))
  }
  #source(file.path(gbl_qa_prgm_path, "read_input_files.r"))
} 


#source(file.path(prelock_path, "programs/oversight/mrl/programs/mrl_preprocessing.sas"))
source(file.path(prgm_path,"mrl_preprocessing.r"))

#source("~/SAS2R_MRL/MRL/MRLs_converted_R/mrl_preprocessing.r")
n_to_c()

if (stringr::str_trim(run_from_qa) == "Y") {
  #source("~/SAS2R_MRL/MRL/MRLs_converted_R/spec_to_listing.r")
  source(file.path(prgm_path, "spec_to_listing.r"))
} else {
  #source(file.path(gbl_prgm_path, "spec_to_listing.sas"))
}

#spec_to_listing()
if (stringr::str_trim(run_from_qa) == "Y") {
  #source("~/SAS2R_MRL/MRL/MRLs_converted_R/export_dups_in_excel.r")
  source(file.path(prgm_path, "export_dups_in_excel.r"))
} else {
  #source(file.path(gbl_prgm_path, "export_dups_in_excel.sas"))
}


#export_dups_in_excel()

# if (stringr::str_trim(run_from_qa) == "Y") {
#   source(file.path(gbl_qa_prgm_path, "send_mails_for_dups.sas"))
# } else {
#   source(file.path(gbl_qa_prgm_path, "send_mails_for_dups.sas"))
# }

#send_mails_for_dups()

#################### AFTER EXPORT DUPS IF F_EXC = "Y" THEN RUN GEN_REPORT FILE, IF F_EXC = "N" THEN CONTINUE WITH PRV_REPORT #####################

if (stringr::str_trim(run_from_qa) == "Y") {
  #source("~/SAS2R_MRL/MRL/MRLs_converted_R/get_pdate.r")
  source(file.path(prgm_path, "get_pdate.r"))
} else {
  #source(file.path(gbl_prgm_path, "get_pdate.sas"))
}

#get_pdate()

prv_report_does_not_exist <- function() {
  if (f_exc == "N") {
    stop_exec <<- ifelse(file.exists(file.path(rev_path, base::paste0(toupper(trial), "_MRL_", toupper(prv_date), ".xlsx"))), "N", "Y")
    cat(stop_exec, "\n")
  } else {
    stop_exec <<- "N"
    cat(stop_exec, "\n")
  }
}

prv_report_does_not_exist()
#END-CHUNK:8 UID:1a239727-a13c-471c-8d2a-630c5ff93cd5

#START-CHUNK:9 UID:d06273b7-7158-41f5-95f1-1e6baf498b12

#another block

# if (stop_exec == "Y") {
#   sendmailR::sendmail(
#     to = paste0(Sys.getenv("USER"), "@lilly.com"),
#     subject = paste(trial, ": MRL - MRL report with", prv_date, "does not exists."),
#     body = paste(
#       "Hi\n",
#       "MRL report with prv_date", prv_date, "does not exists. Aborting further steps.\n",
#       "Thank you\n"
#     ),
#     headers = c(
#       Importance = "HIGH",
#       Sensitivity = "CONFIDENTIAL"
#     )
#   )
# }
#END-CHUNK:9 UID:d06273b7-7158-41f5-95f1-1e6baf498b12

#START-CHUNK:10 UID:8deb280c-fb86-4f92-97ee-2fcae7dbde6b

#another block
if (stringr::str_trim(run_from_qa) == "Y") {
  source(file.path(prgm_path, "prv_report.r"))
} else {
  source(file.path(gbl_prgm_path, "prv_report.r"))
}


if (stringr::str_trim(run_from_qa) == "Y") {
  source(file.path(prgm_path, "get_comments.r"))
} else {
  source(file.path(gbl_prgm_path, "get_comments.r"))
}


if (stringr::str_trim(run_from_qa) == "Y") {
  source(file.path(prgm_path, "get_status.r"))
} else {
  source(file.path(gbl_prgm_path, "get_status.r"))
}


if (stringr::str_trim(run_from_qa) == "Y") {
  source(file.path(prgm_path, "gen_report.r"))
} else {
  source(file.path(gbl_prgm_path, "gen_report.r"))
}



#rm(list = ls(envir = .GlobalEnv, all.names = TRUE))
#test comment