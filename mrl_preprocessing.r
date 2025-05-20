#Total Time Taken for code generation : 0:08:41.685694
#GENAI LLM used : gpt-4 

#START-CHUNK:1 UID:1eb046b2-0289-44ee-b485-de6d7134b207


#another block
# Including fixleng.sas equivalent in R
#source("/lillyce/qa/general/other/diva/programs/utility_macros/fixleng.R")
#source("~/SAS2R_MRL/MRL/MRLs_converted_R/fixleng.r")
# Including sys_link.sas equivalent in R
source("/lillyce/qa/general/other/diva/programs/utility_macros/sys_link/sys_link.r")

# Setting up library path for dictionary
dict <- "/lillyce/prd/dictionaries/current"
dict_files <- list.files(dict, pattern = "\\.sas7bdat$", full.names = TRUE, ignore.case = TRUE)
#print(dict_files)

for (file in dict_files) {
  dataset_name <- tools::file_path_sans_ext(basename(file))
  dataset_name <- toupper(dataset_name)# Get the dataset name without extension
  assign(dataset_name, haven::read_sas(file), envir = .GlobalEnv) # Read the SAS dataset
}


#another block
all_datasets <- ls(envir = .GlobalEnv)
dataset_names <- all_datasets[all_datasets != "SUBJECT"]
name_list <- paste(dataset_names, collapse = " ")


if(base::nzchar(base::trimws(use_links_macro)) && base::toupper(base::trimws(use_links_macro)) == "Y") {
  links()
}

if (base::nzchar(base::trimws(run_from_qa)) && 
    base::toupper(base::trimws(run_from_qa)) == "Y") {
  file_path <- base::paste0(gbl_qa_prgm_path, "/ATCRef_final.RDS")
} else {
  file_path <- base::paste0(gbl_prgm_path, "/ATCRef_final.RDS")
}

# Check if file exists before loading
if (file.exists(file_path)) {
  atc_ref_data <- readRDS(file_path)  # Use readRDS instead of source
  print("RDS file loaded successfully!")
} else {
  stop(paste("Error: RDS file not found -", file_path))
}

convert_dataset_names_to_uppercase <- function() {
  objs <- ls(envir = .GlobalEnv)  # List all objects in the global environment
  
  for (obj in objs) {
    # Check if the object is a data frame and its name is in lowercase
    if (is.data.frame(get(obj, envir = .GlobalEnv)) && obj != toupper(obj)) {  
      assign(toupper(obj), get(obj, envir = .GlobalEnv), envir = .GlobalEnv)  # Rename dataset
      rm(list = obj, envir = .GlobalEnv)  # Remove the old dataset
      cat("✅ Renamed:", obj, "→", toupper(obj), "\n")
    }
  }
}
convert_dataset_names_to_uppercase()

dict_ver <- function(dict, domain) {
  dict_data <- get(dict, envir = .GlobalEnv)
  dv_domain <- paste0("dv_", domain)
  dvn_domain <- paste0("dvn_", domain)
  
  
  # Check if 'dictver' column exists
  if (!"DICTVER" %in% colnames(dict_data)) {
    domain_dict <- dict_data %>%
      mutate(!!dvn_domain := as.numeric(dictver),  # Convert dictver to numeric
             !!dv_domain := dictver) %>%
      select(all_of(c(dvn_domain, dv_domain)))
  }else{
    domain_dict <- dict_data %>%
      mutate(!!dvn_domain := as.numeric(DICTVER),  # Convert dictver to numeric
             !!dv_domain := DICTVER) %>%
      select(all_of(c(dvn_domain, dv_domain))) 
  }
  
  # Sort by dictionary version (descending) and remove duplicates
  domain_dict_1 <- domain_dict %>%
    arrange(desc(!!sym(dvn_domain))) %>%
    distinct(!!sym(dvn_domain), .keep_all = TRUE)
  
  # Select the latest dictionary version (first row)
  domain_dict_2 <- domain_dict_1 %>%
    slice(1)
  
  # Assign the latest dictionary version to a global variable
  latest_dict_version <- domain_dict_2[[dv_domain]][1]
  assign(dv_domain, latest_dict_version, envir = .GlobalEnv)
  
  # Print the latest dictionary version
  cat("\n")
  cat(paste("Latest Dictionary Version for", domain, ":", latest_dict_version, "\n"))
  cat("\n")
}

#END-CHUNK:8 UID:21ca97a5-b61e-4ec9-8f21-92e3fac6b9ab
dict_ver("DICT_MEDDRA", "AE")
dict_ver("DICT_MEDDRA_SMQ", "AE_SMQ")
#START-CHUNK:9 UID:d676cdad-e460-4175-aa20-f2631bfeb393

#another block
# Closing the dict_ver function as the corresponding %mend statement appeared


#INSTRUCTION-FOR-CLOSING-R-FUNCTION: dict_ver Corresponding %mend statement appeared in SAS code.

# Calculate site and country
# site <- base::subset(dm1001, select = c("SITE", "SUBJID"), !base::is.na(SUBJID))
# site <- site[base::order(site$SITE, site$SUBJID), ]
# site <- dplyr::distinct(site, .keep_all = TRUE)
# #END-CHUNK:10 UID:a51e9613-04b8-48bc-8fbf-6f38628628b4
# library(dplyr)
# 
# # Filter dataset and remove missing values in `subjid`
SITE <- DM1001 %>%
  filter(!is.na(SUBJID)) %>%
  select(SITE, SUBJID) %>%
  distinct()  # Equivalent to PROC SORT NODUPKEY (removes duplicates)
# 
# #START-CHUNK:11 UID:fae83987-fa57-4799-8da7-7564cc544115
# 
# #another block
COUNTRY <- SYS_SITE[c("SITEID", "COUNTRY")]
# 
# #INSTRUCTION-FOR-CLOSING-R-FUNCTION: dict_ver Function definition is now closed.
# #END-CHUNK:11 UID:fae83987-fa57-4799-8da7-7564cc544115
# library(dplyr)
# 
# # Ensure both columns are characters and trim spaces
SITE <- SITE %>%
  mutate(SITE = as.character(trimws(SITE)))
# 
COUNTRY <- COUNTRY %>%
  mutate(SITEID = as.character(trimws(SITEID)))
# 
# # Perform the LEFT JOIN correctly
SITE_CNT_STAT <- SITE %>%
  left_join(COUNTRY, by = c("SITE" = "SITEID")) %>%
  mutate(SITECOUNTRY = COUNTRY) %>%  # Ensure sitecountry is the same as country
  distinct()
# 
# # Print final structure
print(head(SITE_CNT_STAT))
# 
# #START-CHUNK:12 UID:c56543e1-6d93-4501-8531-a3ad61d56ff4
# site_cnt_stat <- site %>%
#   left_join(country, by = c("SITE" = "SITEID")) %>%
#   mutate(sitecountry = country) %>%
#   distinct()
# #another block
# # site_cnt_stat <- dplyr::left_join(
# #   dplyr::select(site, SUBJID, SITE),
# #   dplyr::select(country, SITEID, COUNTRY),
# #   by = c("SITE" = "SITEID")
# # ) %>%
# #   dplyr::distinct(SUBJID, SITE, COUNTRY, .keep_all = TRUE) %>%
# #   dplyr::mutate(SITECOUNTRY = COUNTRY)
# # #END-CHUNK:12 UID:c56543e1-6d93-4501-8531-a3ad61d56ff4
# library(dplyr)

# Get all datasets (data frames) present in the global environment
dataset_names <- ls(envir = .GlobalEnv) 

# Filter only data frames
dataset_names <- dataset_names[sapply(dataset_names, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]

# Initialize an empty list to store dataset metadata
ds_list <- data.frame(memname = character(), name = character(), type = integer())

for (dataset_name in dataset_names) {
  dataset <- get(dataset_name, envir = .GlobalEnv)  # Load dataset
  
  if (is.data.frame(dataset)) {  # Ensure it's a data frame
    dataset_info <- data.frame(
      memname = rep(dataset_name, length(colnames(dataset))),  # Repeat dataset name for each column
      name = colnames(dataset),  # Column names
      type = sapply(dataset, function(x) if (is.numeric(x)) 1 else 2)  # Assign type: 1 for numeric, 2 for character
    )
    
    ds_list <- bind_rows(ds_list, dataset_info)  # Append to ds_list
  }
}
ds_list <- ds_list %>%
  mutate(across(where(is.character), toupper))

#START-CHUNK:14 UID:2843b1c2-7a88-4462-a9c7-e91df8e29d8b
ds_list1 <- ds_list %>%
  filter(
    type == 2 &  # Only numeric columns
      (str_detect(toupper(str_trim(name)), "DAT") | toupper(str_trim(name)) == "EVENTDT") &  # Name contains "DAT" or is "EVENTDT"
      !str_detect(toupper(str_trim(name)), "DATMO") &  # Exclude "DATMO"
      !str_detect(toupper(str_trim(name)), "DATYY") &  # Exclude "DATYY"
      !str_detect(toupper(str_trim(memname)), "SYS") &  # Exclude dataset names containing "SYS"
      !str_detect(toupper(str_trim(name)), "ADJUDICATION")  # Exclude "ADJUDICATION"
  ) %>%
  distinct()  

#START-CHUNK:15 UID:c695e696-012e-4e57-8119-8b3f6f8c4bb2
ds_list2 <- ds_list %>%
  filter(
    type == 2 &
      toupper(str_trim(name)) == "UPDATEDDT" &
      !str_detect(toupper(str_trim(memname)), "SYS")
  ) %>%
  distinct()

#another block
ds_list3 <- dplyr::filter(ds_list, type == 2 & 
                            stringr::str_to_upper(stringr::str_trim(name)) == "FORMEID" & 
                            stringr::str_detect(stringr::str_to_upper(stringr::str_trim(memname)), "SYS") == FALSE)

ds_list3 <- ds_list3[base::order(ds_list3$memname, ds_list3$name), ]
ds_list3 <- dplyr::distinct(ds_list3, .keep_all = TRUE)

u_getcomponents <- function(df, invar) {
  tryCatch({
    cat("\n🔍 Debugging: Checking if column exists:", invar, "\n")
    
    # Check if the column exists
    if (!(invar %in% colnames(df))) {
      stop(paste("🚨 Error: Column", invar, "not found in the dataset. Available columns:", paste(colnames(df), collapse=", ")))
    }
    
    # Print column values before transformation
    cat("\n📌 Original column values:\n")
    
    # Normalize Dates: Handle Both Formats
    df <- df %>%
      mutate(
        clean_date = case_when(
          str_detect(.data[[invar]], "^[0-9]{4}-[0-9]{2}-[0-9]{2}") ~ str_extract(.data[[invar]], "^[0-9]{4}-[0-9]{2}-[0-9]{2}"),  # YYYY-MM-DD
          str_detect(.data[[invar]], "^[0-9]{2}-[A-Za-z]{3}-[0-9]{4}") ~ format(as.Date(.data[[invar]], format="%d-%b-%Y"), "%Y-%m-%d"),  # DD-MMM-YYYY
          TRUE ~ NA_character_  # If not matching any format
        )
      )
    
    cat("\n🛠️ Normalized Date Values (Converting to YYYY-MM-DD Format):\n")
    print(head(df$clean_date))
    
    # Split the cleaned date into Year, Month, and Day
    date_parts <- str_split_fixed(df$clean_date, "-", 3)
    
    # Apply transformations
    df <- df %>%
      mutate(
        !!sym(paste0(invar, "YY")) := {
          extracted_year <- date_parts[, 1]  # Extract Year
          cat("\n🟢 Extracted Year (YY) for", invar, ":\n")
          print(head(extracted_year))
          extracted_year
        },
        
        !!sym(paste0(invar, "MO")) := {
          extracted_month <- date_parts[, 2]  # Extract Month
          cat("\n🟠 Extracted Month (MO) for", invar, ":\n")
          print(head(extracted_month))
          extracted_month
        },
        
        !!sym(paste0(invar, "DD")) := {
          extracted_day <- date_parts[, 3]  # Extract Day
          cat("\n🔵 Extracted Day (DD) for", invar, ":\n")
          print(head(extracted_day))
          extracted_day
        }
      ) %>%
      select(-clean_date)  # Remove intermediate column
    
    cat("\n✅ Transformation Complete for", invar, "\n")
    return(df)
    
  }, error = function(e) {
    message("\n⚠️ Warning: ", e$message)
    return(df)  # Return the original dataset unmodified
  })
}
# Function to Apply `u_getcomponents` to All Datasets in `ds_list1`
call_ugc <- function() {
  for (i in 1:nrow(ds_list1)) {
    dataset_name <- ds_list1$memname[i]
    date_column <- ds_list1$name[i]
    
    # Load dataset dynamically
    if (exists(dataset_name, envir = .GlobalEnv)) {
      df <- get(dataset_name, envir = .GlobalEnv)
      
      # Apply transformation
      df <- u_getcomponents(df, date_column)
      
      # Save updated dataset
      assign(dataset_name, df, envir = .GlobalEnv)
      
      cat(paste0("Updated: ", dataset_name, " (", date_column, ")\n"))
    } else {
      cat(paste0("Warning: Dataset '", tolower(dataset_name), "' not found!\n"))
    }
  }
}

#START-CHUNK:21 UID:c386ca95-1077-4a2d-bf22-9576b29108f0
call_ugc()



# Function to Loop Over `ds_list1` and Apply `u_getdate`
call_ugdt <- function() {
  for (i in 1:nrow(ds_list1)) {
    dataset_name <- ds_list1$memname[i]
    base_col <- ds_list1$name[i]
    
    mon <- paste0(base_col, "MO")
    date <- paste0(base_col, "DD")
    year <- paste0(base_col, "YY")
    var <- paste0(base_col, "_DATE")
    
    # Load dataset dynamically
    if (exists(dataset_name, envir = .GlobalEnv)) {
      data <- get(dataset_name, envir = .GlobalEnv)
      
      # Apply transformation
      data <- u_getdate(data, mon, date, year, var)
      
      # Save updated dataset
      assign(dataset_name, data, envir = .GlobalEnv)
      
      cat(paste0("✅ Updated: ", tolower(dataset_name), " (", var, ")\n"))
    } else {
      cat(paste0("⚠️ Warning: Dataset '", dataset_name, "' not found!\n"))
    }
  }
}
call_ugdt()


# Closing the call_ugdt function as the corresponding %mend statement appeared
# call_last <- function() {
#   for (i in 1:nrow(ds_list2)) {
#     dataset_name <- ds_list2$memname[i]
#     updateddt_col <- ds_list2$name[i]  # Column with datetime values
#     
#     # Load dataset dynamically
#     if (exists(dataset_name, envir = .GlobalEnv)) {
#       data <- get(dataset_name, envir = .GlobalEnv)
#       
#       tryCatch({
#         cat("\n🔍 Debugging: Checking if column exists:", updateddt_col, "\n")
#         
#         # Ensure the required column exists
#         if (!(updateddt_col %in% colnames(data))) {
#           stop(paste("🚨 Error: Column", updateddt_col, "not found in the dataset. Available columns:", paste(colnames(data), collapse=", ")))
#         }
#         
#         # Convert UPDATEDDT to date format
#         data <- data %>%
#           mutate(LASTCHGDATE = as.Date(.data[[updateddt_col]]))
#         
#         cat("\n✅ Transformation Complete - Created Column: LASTCHGDATE\n")
#         assign(dataset_name, data, envir = .GlobalEnv)
# 
#       }, error = function(e) {
#         message("\n⚠️ Warning: ", e$message)
#         return(data)  # Return original dataset unmodified
#       })
#       # Apply transformation
#       #data <- convert_lastchgdate(data, updateddt_col)
#       
#       # Save updated dataset
#       assign(dataset_name, data, envir = .GlobalEnv)
#       
#       cat(paste0("✅ Updated: ", dataset_name, " (LASTCHGDATE added)\n"))
#     } else {
#       cat(paste0("⚠️ Warning: Dataset '", dataset_name, "' not found!\n"))
#     }
#   }
# }
# call_last()
# # colnames(AESTEX1001)
# 
# library(dplyr)
# library(lubridate)

# Function to Convert UPDATEDDT to LASTCHGDATE
convert_lastchgdate <- function(data, updateddt_col) {
  tryCatch({
    cat("\n🔍 Debugging: Checking if column exists:", updateddt_col, "\n")
    
    # Ensure the required column exists
    if (!(updateddt_col %in% colnames(data))) {
      stop(paste("🚨 Error: Column", updateddt_col, "not found in the dataset. Available columns:", paste(colnames(data), collapse=", ")))
    }
    
    # Convert UPDATEDDT to date format
    data <- data %>%
      mutate(LASTCHGDATE = as.Date(.data[[updateddt_col]]))
    
    cat("\n✅ Transformation Complete - Created Column: LASTCHGDATE\n")
    return(data)
    
  }, error = function(e) {
    message("\n⚠️ Warning: ", e$message)
    return(data)  # Return original dataset unmodified
  })
}

# Function to Loop Over `ds_list2` and Apply `convert_lastchgdate`
call_last <- function() {
  for (i in 1:nrow(ds_list2)) {
    dataset_name <- ds_list2$memname[i]
    updateddt_col <- ds_list2$name[i]  # Column with datetime values
    
    # Load dataset dynamically
    if (exists(dataset_name, envir = .GlobalEnv)) {
      data <- get(dataset_name, envir = .GlobalEnv)
      if (any(duplicated(colnames(data)))) {
        colnames(data) <- make.unique(colnames(data))
      }
      if (!(updateddt_col %in% colnames(data))) {
        stop(paste("🚨 Error: Column", updateddt_col, "not found in the dataset. Available columns:", paste(colnames(data), collapse=", ")))
      }
      
      # Convert UPDATEDDT to date format
      data <- data %>%
        mutate(LASTCHGDATE = as.Date(.data[[updateddt_col]]))
      
      cat("\n✅ Transformation Complete - Created Column: LASTCHGDATE\n")
      assign(dataset_name, data, envir = .GlobalEnv)
      
      # Apply transformation
      data <- convert_lastchgdate(data, updateddt_col)
      
      # Save updated dataset
      assign(dataset_name, data, envir = .GlobalEnv)
      
      cat(paste0("✅ Updated: ", dataset_name, " (LASTCHGDATE added)\n"))
    } else {
      cat(paste0("⚠️ Warning: Dataset '", dataset_name, "' not found!\n"))
    }
  }
}


# Apply Function to Process All Datasets
call_last()


call_form <- function() {
  # Check if ds_list3 exists
  if (!exists("ds_list3", envir = .GlobalEnv)) {
    stop("🚨 Error: ds_list3 not found in the global environment.")
  }
  
  # Load ds_list3 from the global environment
  ds_list3 <- get("ds_list3", envir = .GlobalEnv)
  
  # Loop Over All Datasets Listed in `ds_list3`
  for (i in 1:nrow(ds_list3)) {
    dataset_name <- ds_list3$memname[i]
    formeid_col <- ds_list3$name[i]  # Column containing FORMEID
    
    # Check if dataset exists
    tryCatch({
      # Check if dataset exists
      if (!exists(dataset_name, envir = .GlobalEnv)) {
        stop(paste("⚠️ Warning: Dataset '", dataset_name, "' not found!"))
      }
      
      data <- get(dataset_name, envir = .GlobalEnv)
      
      # Ensure the required column exists
      if (!(formeid_col %in% colnames(data))) {
        stop(paste("⚠️ Warning: Column", formeid_col, "not found in", dataset_name))
      }
      
      # Remove existing FORM column if it already exists
      if ("FORM" %in% colnames(data)) {
        data <- data %>% select(-FORM)
        cat("🔄 Existing FORM column removed from", dataset_name, "\n")
      }
      
      # Copy FORMEID to FORM
      data <- data %>%
        mutate(FORM = .data[[formeid_col]])
      
      # Save updated dataset
      assign(dataset_name, data, envir = .GlobalEnv)
      
      cat(paste0("✅ Updated: ", dataset_name, " (FORM copied from ", formeid_col, ")\n"))
      
    }, error = function(e) {
      message("\n⚠️ Skipping dataset ", dataset_name, " due to error: ", e$message)
    })
  }
}

#START-CHUNK:26 UID:c1a4fb90-1f7b-418b-9961-8e80f4255123
call_form()

exist_gen <- function() {
  # Check if the 'dict' environment exists
  if (!exists("dict", envir = .GlobalEnv)) {
    stop("🚨 Error: Source environment 'dict' not found in the global environment.")
  }
  
  # Check if 'DICT_MEDDRA_GENDSPEC' exists in 'dict' environment
  if (exists("DICT_MEDDRA_GENDSPEC", envir = .GlobalEnv)) {
    # Copy dataset to the global environment (work)
    assign("DICT_MEDDRA_GENDSPEC", get("DICT_MEDDRA_GENDSPEC", envir = .GlobalEnv), envir = .GlobalEnv)
    
    cat("✅ Copied dataset: DICT_MEDDRA_GENDSPEC to work environment.\n")
  } else {
    # Print missing dataset message
    cat("⚠️ DICT_MEDDRA_GENDSPEC doesn't exist, m_BIO_AE006_MH007 won't execute\n")
  }
}

exist_gen()

# Print Datasets in Work Environment

DICT_MEDDRA <- DICT_MEDDRA %>%
  filter(as.numeric(DICTVER) == as.numeric(dv_AE))

colnames(DICT_MEDDRA_SMQ) <- toupper(colnames(DICT_MEDDRA_SMQ))
DICT_MEDDRA_SMQ <- DICT_MEDDRA_SMQ%>%
  filter(as.numeric(DICTVER) == as.numeric(dv_AE_SMQ))

chk_var_create <- function(ds_name, var_name, vc_name) {
  tryCatch({
    # Check if dataset exists in the global environment
    if (!exists(ds_name, envir = .GlobalEnv)) {
      stop(paste("🚨 Error: Dataset", ds_name, "not found in the global environment."))
    }
    
    # Load dataset
    data <- get(ds_name, envir = .GlobalEnv)
    
    # Check if the variable exists
    if (var_name %in% colnames(data)) {
      cat("✅ Variable", var_name, "already exists in", ds_name, "- No changes made.\n")
    } else {
      # If variable does not exist, create it and assign the value of vc_name
      data <- data %>%
        mutate(!!sym(var_name) := vc_name)
      
      # Save updated dataset
      assign(ds_name, data, envir = .GlobalEnv)
      
      cat("✅ Variable", var_name, "created in", ds_name, "with value:", vc_name, "\n")
    }
  }, error = function(e) {
    message("\n⚠️ Warning: ", e$message)
  })
}


# Function to Check, Create, and Notify for 'trigger' Dataset
create_trigger <- function() {
  tryCatch({
    # Check if 'trigger' exists in the global environment
    if (exists("TRIGGER", envir = .GlobalEnv)) {
      # Copy dataset to 'trigger_org'
      TRIGGER <- get("TRIGGER", envir = .GlobalEnv)
      assign("TRIGGER", TRIGGER, envir = .GlobalEnv)
      
      cat("✅ Dataset 'trigger' exists. Copied to 'trigger_org'.\n")
    } else {
      # Create an empty 'trigger' dataset with predefined columns
      TRIGGER <- data.frame(
        ACRONYM = character(),
        ENDPOINT = character(),
        `MEDDRA PREFERRED TERM (PT)` = character(),
        `SMQ (IF APPLICABLE)` = character(),
        `SOC (HLGT; HLT)` = character(),
        `RECOMMENDATION FOR ADJUDICATION` = character(),
        COMMENTS = character(),
        stringsAsFactors = FALSE
      )
      
      assign("TRIGGER", TRIGGER, envir = .GlobalEnv)
      
      cat("⚠️ 'TRIGGER' dataset was missing. Created an empty dataset.\n")
      
      # Sending an email notification (optional)
      # if (requireNamespace("mailR", quietly = TRUE)) {
      #   library(mailR)
      #   
      #   send.mail(
      #     from = "your_email@company.com",
      #     to = paste0(Sys.getenv("USER"), "@lilly.com"),
      #     subject = paste(Sys.getenv("TRIAL"), ": MRL - AE090 not available for subject_ae_mh."),
      #     body = "Hi,\n\nAE090 is not available as of now. Kindly get the file from DM via ART team. A dummy metadata dataset has been created.\n\nThank you.",
      #     smtp = list(host.name = "smtp.yourcompany.com", port = 465, user.name = "your_email@company.com", passwd = "yourpassword", ssl = TRUE),
      #     authenticate = TRUE,
      #     send = TRUE
      #   )
      #   
      #   cat("📧 Email notification sent.\n")
      # } else {
      #   cat("⚠️ Email package 'mailR' not installed. No email sent.\n")
      # }
    }
  }, error = function(e) {
    message("\n🚨 Error in create_trigger(): ", e$message)
  })
}

# Run the Function
create_trigger()

# Initialize an empty dataframe
ZZ <- data.frame(MEMNAME = character(), NAME = character(), stringsAsFactors = FALSE)

# Loop through each dataset in the global environment
for (dataset_name in dataset_names) {
  if (exists(dataset_name, envir = .GlobalEnv)) {  # Check if dataset exists
    dataset <- get(dataset_name, envir = .GlobalEnv)  # Load dataset dynamically
    
    if (is.data.frame(dataset) && length(colnames(dataset)) > 0) {  # Ensure it's a non-empty dataframe
      dataset_info <- data.frame(
        MEMNAME = rep(dataset_name, length(colnames(dataset))),  # Repeat dataset name for each column
        NAME = colnames(dataset),  # Extract column names
        stringsAsFactors = FALSE
      )
      
      ZZ <- bind_rows(ZZ, dataset_info)  # Append new dataset info
    }
  }
}

ZZ1 <- ZZ %>%
  filter(NAME == "ROW_ILB")

#another block
M <- base::nrow(ZZ1)
DY <- ZZ1$MEMNAME[1:M]

# Extract MEMNAME column values into a named vector
if (M > 0) {
  DY <- setNames(ZZ1$MEMNAME, paste0("DY", seq_len(M)))
} else {
  DY <- NULL
}

call_cm_clascd <- function(indsn) {
  tryCatch({
    # Retrieve dataset
    dataset <- get(indsn, envir = .GlobalEnv)
    
    # Ensure dataset is a valid data frame
    if (!is.data.frame(dataset)) {
      stop("🚨 Error: The dataset retrieved is not a valid data frame.")
    }
    
    # Ensure column names are unique before proceeding
    if (any(duplicated(colnames(dataset)))) {
      warning("⚠️ Warning: Duplicate column names detected. Renaming them.")
      names(dataset) <- make.unique(names(dataset))
    }
    
    # Define expected column names
    ATC_cols <- c("CMATC4CD", "CMATC3CD", "CMATC2CD", "CMATC1CD")
    CMCLAS_cols <- c("CMCLAS4", "CMCLAS3", "CMCLAS2", "CMCLAS1")
    
    # Keep only columns that exist in the dataset
    ATC_cols <- ATC_cols[ATC_cols %in% colnames(dataset)]
    CMCLAS_cols <- CMCLAS_cols[CMCLAS_cols %in% colnames(dataset)]
    
    # Apply transformations only if valid columns exist
    dataset <- dataset %>%
      mutate(
        CMCLASCD = if (length(ATC_cols) > 0) coalesce(!!!syms(ATC_cols)) else NA_character_,
        CMCLAS   = if (length(CMCLAS_cols) > 0) coalesce(!!!syms(CMCLAS_cols)) else NA_character_
      )
    
    # Save updated dataset in the global environment
    assign(indsn, dataset, envir = .GlobalEnv)
    
    cat("✅ CMCLASCD and CMCLAS created in", indsn, "\n")
  }, error = function(e) {
    message("\n🚨 Error in call_cm_clascd(): ", e$message)
  })
}

dataset_list <- c("CM1001", "CM2001", "CM3001", "CM4001")

for (dataset in dataset_list) {
  try(call_cm_clascd(dataset), silent = TRUE)  # Uses 'try()' to continue on errors
}

# Function to Process Prohibited Terms (Exact SAS Macro Translation)
prohibited_xx <- function(indsn1, indsn2) {
  tryCatch({
    # Convert dataset references to string names
    indsn1_name <- deparse(substitute(indsn1))
    indsn2_name <- deparse(substitute(indsn2))
    
    # Check if datasets exist
    if (!exists(indsn1_name, envir = .GlobalEnv)) {
      stop(paste("🚨 Error: Dataset", indsn1_name, "not found in the global environment."))
    }
    if (!exists(indsn2_name, envir = .GlobalEnv)) {
      stop(paste("🚨 Error: Dataset", indsn2_name, "not found in the global environment."))
    }
    
    # Retrieve datasets
    dataset1 <- get(indsn1_name, envir = .GlobalEnv)
    dataset2 <- get(indsn2_name, envir = .GlobalEnv)
    
    # Check if HLGT and PT columns exist
    if (!("HLGT" %in% colnames(dataset1) & "PT" %in% colnames(dataset1))) {
      cat("\n⚠️ Warning: Variable HLGT or PT missing in", indsn1_name, "- Skipping processing.\n")
      #   email <- compose_email(
      #     body = md(glue::glue("
      #   **Hi,**
      #   
      #   The variable `HLGT` or `PT` is missing in `{dataset_name}` dataset.  
      #   Kindly check the source file.
      # 
      #   **Thank you!**
      # "))
      #   )
      #   
      #   # Replace with your SMTP server details
      #   smtp_send(
      #     email,
      #     from = "your_email@example.com",
      #     to = "your_recipient@example.com",
      #     subject = paste("MRL - Variable HLGT or PT missing in", indsn1_name),
      #     credentials = creds_key(id = "your_email_credentials") # Set up credentials using blastula
      #   )
      #send_email_alert(indsn1_name)  # Send Email Alert
      return()
    }
    
    # Step 1: Split Dataset
    dataset1_hlgt <- dataset1 %>% filter(!is.na(HLGT))  # HLGT present
    dataset1_pt <- dataset1 %>% filter(is.na(HLGT) & !is.na(PT))  # HLGT missing, PT present
    
    # Step 3: Join `_hlgt` dataset with `indsn2`
    dataset1_hlgt_pt <- dataset1_hlgt %>%
      left_join(dataset2 %>% select(HLGTCD, PFTERM), by = c("HLGT" = "HLGTCD")) %>%
      rename(PT = PFTERM)  # Rename PFTERM to PT
    
    # Step 4: Combine `_pt` and `_hlgt_pt` datasets
    final_dataset <- bind_rows(dataset1_pt, dataset1_hlgt_pt) %>%
      distinct(PT, .keep_all = TRUE)  # Remove duplicate PT values
    
    # Step 5: Save updated dataset back to the environment
    assign(indsn1_name, final_dataset, envir = .GlobalEnv)
    
    cat("✅ Processed prohibited terms for", indsn1_name, "\n")
    
  }, error = function(e) {
    message("\n⚠️ Warning: ", e$message)
  })
}

prohibited_xx(PROHIBITED_AE, DICT_MEDDRA)
prohibited_xx(PROHIBITED_MH,DICT_MEDDRA)


#Referenced Code Here.
procedures_end_date <- function() {
  dataset_name <- "PROCEDURES_ENDDATE"
  
  # Check if dataset exists
  if (!exists(dataset_name, envir = .GlobalEnv)) {
    # email <- compose_email(
    #   body = md(glue("
    #   **Hi,**
    #   
    #   The following dataset `{dataset_name}`   
    # 
    #   Kindly check the source file.
    # 
    #   **Thank you!**
    # "))
    # )
    # 
    # # Replace with your SMTP server details
    # smtp_send(
    #   email,
    #   from = "your_email@example.com",
    #   to = "your_recipient@example.com",
    #   subject = paste("MRL - Issue with", dataset_name),
    #   credentials = creds_key(id = "your_email_credentials") # Set up credentials using blastula
    # )
    #send_email_alert(dataset_name, "Dataset not found in the environment")
    cat("🚨 Error: Dataset is not found in the environment.")
    return()
  }
  
  # Retrieve dataset
  dataset <- get(dataset_name, envir = .GlobalEnv)
  
  # Check if SOURCE column exists
  if (!("SOURCE" %in% colnames(dataset))) {
    stop("🚨 Error: Column SOURCE not found in the dataset.")
  }
  
  # Split Dataset Based on `SOURCE`
  dataset_soc <- dataset %>% filter(toupper(trimws(SOURCE)) == "SOC")
  dataset_pt <- dataset %>% filter(toupper(trimws(SOURCE)) != "SOC")
  
  # Retrieve dictionary dataset
  dict_meddra <- get("DICT_MEDDRA", envir = .GlobalEnv)
  
  # Perform Left Join with `DICT_MEDDRA` on `SOCTERM`
  dataset_soc <- dataset_soc %>%
    left_join(dict_meddra %>% select(SOCTERM, PFTERM), by = c("LIST" = "SOCTERM")) %>%
    rename(LIST1 = PFTERM)  # Rename `PFTERM` to `LIST1`
  
  # Combine Datasets
  dataset_final <- bind_rows(dataset_pt, dataset_soc) %>%
    mutate(
      LIST = case_when(
        toupper(trimws(SOURCE)) == "SOC" & !is.na(LIST1) ~ LIST1,
        TRUE ~ LIST
      )
    ) %>%
    filter(!(toupper(trimws(SOURCE)) == "SOC" & is.na(LIST1) & is.na(LIST)))  # Remove missing SOC records
  
  # Sort & Remove Duplicates
  dataset_final <- dataset_final %>%
    arrange(LIST) %>%
    distinct(LIST, .keep_all = TRUE)
  
  # Save updated dataset in the global environment
  assign(dataset_name, dataset_final, envir = .GlobalEnv)
  
  cat("✅ Processed `procedures_enddate` dataset successfully.\n")
}

procedures_end_date()


#another block
handle_ds6001 <- function() {
  if(exists("DS6001", where = .GlobalEnv)) {
    dsid_x <- get("DS6001")
    chk_x <- if("DSSCAT" %in% names(dsid_x)) TRUE else FALSE
    chk_y <- if("DSSCAT_2" %in% names(dsid_x)) TRUE else FALSE
    
    if(!chk_x && chk_y) {
      dsid_x$DSSCAT <- dsid_x$DSSCAT_2
      assign("DS6001", dsid_x, envir = .GlobalEnv)
    }
  }
}
handle_ds6001()

CM_CLAS <- function(indsn1) {
  # Check if dataset exists
  if (!exists(indsn1, envir = .GlobalEnv)) {
    cat("\n⚠️ Warning: Dataset", indsn1, "does not exist. Skipping processing.\n")
    return()
  }
  
  # Retrieve dataset
  dataset <- get(indsn1, envir = .GlobalEnv)
  
  if (any(duplicated(colnames(dataset)))) {
    warning("⚠️ Warning: Duplicate column names detected. Renaming them.")
    names(dataset) <- make.unique(names(dataset))
  }
  
  # Check if CMCLAS column exists
  if ("CMCLAS" %in% colnames(dataset)) {
    cat("\n✅ CMCLAS column already exists in", dataset, "- No changes made.\n")
  } else {
    dataset <- dataset %>%
      mutate(CMCLAS = .data[["CMATC4"]])  # Assign CMATC4 column to CMCLAS
    cat("\n✅ CMCLAS column created using", "CMATC4", "in", indsn1, "\n")
  }
  
  # Save updated dataset in the global environment
  assign(indsn1, dataset, envir = .GlobalEnv)
}

dataset_list <- c("CM1001", "CM2001", "CM3001", "CM4001")

for (dataset in dataset_list) {
  try(CM_CLAS(dataset), silent = TRUE)  # Uses 'try()' to continue on errors
}


handle_ae3001 <- function() {
  if (exists("AE3001", where = .GlobalEnv)) {
    dsid_x <- get("AE3001", envir = .GlobalEnv)  # Retrieve dataset
    
    # Check if AEONGO column exists
    if (!"AEONGO" %in% names(dsid_x)) {
      dsid_x <- dsid_x %>%
        mutate(AEONGO = ifelse(!is.na(AESTDAT) & is.na(AEENDAT), "Y", NA))  # Create AEONGO
      
      assign("AE3001", dsid_x, envir = .GlobalEnv)  # Update global dataset
      cat("\n✅ AEONGO column created in AE3001 where AESTDAT is present, but AEENDAT is missing.\n")
    } else {
      cat("\n✅ AEONGO column already exists in AE3001 - No changes made.\n")
    }
  } else {
    cat("\n⚠️ Warning: Dataset AE3001 does not exist. Skipping processing.\n")
  }
}
#another block
handle_ae3001()

CM1001 <- CM1001 %>%  mutate(CMCLASCD = CMATC4CD, CMCLAS = CMATC4)
CM2001 <- CM2001 %>%  mutate(CMROUTE = "", CMCLASCD = CMATC4CD, CMCLAS = CMATC4)
CM3001 <- CM3001 %>%  mutate(CMCLASCD = CMATC4CD, CMCLAS = CMATC4)

