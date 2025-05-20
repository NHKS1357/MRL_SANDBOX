

indsn1 = strsplit(sources[4], ",")[[1]][1]
indsn2 = strsplit(sources[4], ",")[[1]][2]
indsn3 = strsplit(sources[4], ",")[[1]][3]
indsn4 = strsplit(sources[4], ",")[[1]][4]
indsn5 = strsplit(sources[4], ",")[[1]][5]
indsn6 = strsplit(sources[4], ",")[[1]][6]
indsn7 = strsplit(sources[4], ",")[[1]][7]
indsn8 = strsplit(sources[4], ",")[[1]][8]
spedis_value = as.integer(strsplit(sources[4], ",")[[1]][9])
indsn9 = strsplit(sources[4], ",")[[1]][10]
indsn10 = strsplit(sources[4], ",")[[1]][11]
indsn11 = strsplit(sources[4], ",")[[1]][12]
indsn12 = strsplit(sources[4], ",")[[1]][13]
indsn13 = strsplit(sources[4], ",")[[1]][14]
indsn14 = strsplit(sources[4], ",")[[1]][15]
indsn15 = strsplit(sources[4], ",")[[1]][16]
indsn16 = strsplit(sources[4], ",")[[1]][17]
include_mhpresp = strsplit(sources[4], ",")[[1]][18]
keep_cm1_only = strsplit(sources[4], ",")[[1]][19]


#d_Subject_CMTreatment
var1 <- "SUBJID"
var2 <- "CMDECOD"
var3 <- "CMROUTE"
var4 <- "CMINDC"
var5 <- "CMCLASCD"
var6 <- "CMCLAS"
var7 <- "CMTRT"

for (i in 1:7) {
  u_checkds_var(indsn1, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}

CM1_1 <- base::get(indsn1, envir = .GlobalEnv)


CM2_1 <- if (exists(indsn2, envir = .GlobalEnv)) get(indsn2, envir = .GlobalEnv) else CM1_1[0, ]

# **Ensure CMSPID and CMDOSFRQ Exist in All Datasets**
ensure_variables <- function(dataset, dataset_name) {
  required_vars <- c("SUBJID", "CMDECOD", "CMROUTE", "CMSPID", "CMCLASCD", "CMCLAS", "CMTRT")
  
  for (var in required_vars) {
    if (!(var %in% colnames(dataset))) {
      dataset[[var]] <- NA  # Add missing column with NA values
      cat("\n✅ Variable", var, "added to", dataset_name, "with NA values.\n")
    }
  }
  return(dataset)
}

CM2_1 <- ensure_variables(CM2_1, "CM2_1")

CM3_1 <- if (exists(indsn3, envir = .GlobalEnv)) get(indsn3, envir = .GlobalEnv) else CM1_1[0, ]
ensure_variables <- function(dataset, dataset_name) {
  required_vars <- c("SUBJID","CMDECOD","CMTRADNM","CMOCCUR","CMCLASCD","CMCLAS","CMTRT")
  for (var in required_vars) {
    if (!(var %in% colnames(dataset))) {
      dataset[[var]] <- NA  # Add missing column with NA values
      cat("\n✅ Variable", var, "added to", dataset_name, "with NA values.\n")
    }
  }
  return(dataset)
}

# Apply fixes to all datasets
CM3_1 <- ensure_variables(CM3_1, "CM3_1")

CM4_1 <- if (exists(indsn9, envir = .GlobalEnv)) get(indsn9, envir = .GlobalEnv) else CM1_1[0, ]


var1 <- "SUBJID"
var2 <- "DSSTDAT_ICMO"
var3 <- "DSSTDAT_ICDD"
var4 <- "DSSTDAT_ICYY"

for (i in 1:4) {
  u_checkds_var(indsn7, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}

var1 <- "SUBJID"

for (i in 1:1) {
  u_checkds_var(indsn8, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}

CM1_1 = u_getcomponents(CM1_1,"CMSTDAT")
CM1_1 <<- u_getdate("CM1_1", "CMSTDATMO", "CMSTDATDD", "CMSTDATYY", "CMSTDAT_")

CM1_1 = u_getcomponents(CM1_1,"CMENDAT")
CM1_1 <- u_getdate("CM1_1", "CMENDATMO", "CMENDATDD", "CMENDATYY", "CMENDAT_")

CM2_1 <- u_getdate("CM2_1", "CMSTDATMO", "CMSTDATDD", "CMSTDATYY", "CMSTDAT_")
CM2_1 <- u_getdate("CM2_1", "CMENDATMO", "CMENDATDD", "CMENDATYY", "CMENDAT_")

CM3_1 <- u_getdate("CM3_1", "CMSTDATMO", "CMSTDATDD", "CMSTDATYY", "CMSTDAT_")
CM3_1 <- u_getdate("CM3_1", "CMENDATMO", "CMENDATDD", "CMENDATYY", "CMENDAT_")

CM4_1 <- u_getdate("CM4_1", "CMSTDATMO", "CMSTDATDD", "CMSTDATYY", "CMSTDAT_")


chk_var <- function(dataset, var) {
  # Extract variable name and value (assuming "var=value" format)
  var_split <- strsplit(var, "=")[[1]]
  var_name <- var_split[1]
  var_value <- var_split[2]
  
  # Check if the variable exists in the dataset
  if (!(var_name %in% names(dataset))) {
    # Add variable with specified value
    if (var_value == ".") {
      dataset[[var_name]] <- NA
    } else {
      dataset[[var_name]] <- eval(parse(text = var_value))
    }
    print(paste("Variable", var_name, "added."))
  } else {
    print(paste("Variable", var_name, "already exists."))
  }
  
  return(dataset)
}

# Define a wrapper function to run chk_var for multiple variables
update_dataset <- function(dataset_name, variables) {
  # Retrieve the dataset from the global environment
  dataset <- get(dataset_name, envir = .GlobalEnv)
  
  # Apply chk_var for each variable
  for (var in variables) {
    dataset <- chk_var(dataset, var)
    assign(dataset_name, dataset, envir = .GlobalEnv)
    # Pass dataset to chk_var and capture any modifications
  }
  
  # Assign the updated dataset back to the global environment
  #assign(dataset_name, dataset, envir = .GlobalEnv)
  
  # Optionally return the updated dataset
  return(dataset)
}

vars_to_check_c3 <- c("CMDOSFRQ=''", "CMINDC=''", "CMAEGRPID4=.", "CMROUTE=''", 
                   "CMENREAS_PRIOR=''", "CMDOSE=''", "CMDOSEU=''", "CMLOC=''", 
                   "CMSCAT=''", "CMMHNO4=.")

# Call update_dataset to update CM3_1 in the global environment
update_dataset("CM3_1", vars_to_check_c3)

vars_to_check_c2 <- c("CMDOSFRQ=''", "CMMHNO4=.", "CMAEGRPID4=.", "CMROUTE=''", 
                   "CMDOSE=''", "CMDOSEU=''", "CMENREAS_PRIOR=''", "CMLOC=''")

# Call update_dataset to update CM3_1 in the global environment
update_dataset("CM2_1", vars_to_check_c2)

vars_to_check_c1 <- c("CMDOSE=''", "CMDOSEU=''", "CMLOC=''", "CMENREAS_PRIOR=''", "CMDOSFRQ=''", "CMMHNO4=.", "CMAEGRPID4=.", "CMROUTE=''")

# Call update_dataset to update CM3_1 in the global environment
update_dataset("CM1_1", vars_to_check_c1)

CM1 <- CM1_1 %>%
  filter(CMDECOD != "") %>% #, CMROUTE != ""
  # Add the Form column (replace "indsn1" with the actual variable or string)
  mutate(FORM = toupper(indsn1)) %>%
  # Ensure CMENREAS_PRIOR is a character column (no need for length restriction in R)
  mutate(CMENREAS_PRIOR = as.character(CMENREAS_PRIOR)) %>%
  # Concatenate CMCLASCD and CMCLAS into ATC with " : " separator
  mutate(ATC = ifelse(CMCLASCD != "" & CMCLAS != "", 
                      paste(toupper(CMCLASCD), toupper(CMCLAS), sep = " : "), 
                      ""))%>%
  select(
    SUBJID, FORM, CMAEGRPID4, CMMHNO4, CMCAT, CMSCAT, CMSPID, CMTRT,
    CMDECOD, CMTRADNM, CMLOC, CMROUTE, CMINDC, CMDOSE, CMDOSEU,
    CMDOSFRQ, CMCLASCD, CMCLAS, CMENREAS_PRIOR, CMONGO,
    CMSTDAT, CMENDAT, CMSTDAT_, CMENDAT_, LASTCHGDATE, ATC
  )

CM2 <- CM2_1 %>%
  mutate(
    FORM = tools::toTitleCase(indsn2),
    CMAEGRPID = as.numeric(CMAEGRPID4),
    CMSCAT = "",
    CMINDC = CMINDC_PRIOR,
    CMONGO = "N/A",
    ATC = ifelse(CMCLASCD != "" & CMCLAS != "", 
                       paste(toupper(CMCLASCD), toupper(CMCLAS), sep = " : "), 
                       "")
  ) %>%
  filter(CMDECOD != "")%>%
  select(
    SUBJID, FORM, CMAEGRPID4, CMMHNO4, CMCAT, CMSCAT, CMSPID, CMTRT, CMDECOD,
    CMTRADNM, CMLOC, CMROUTE, CMINDC, CMDOSE, CMDOSEU, CMDOSFRQ, CMCLASCD,
    CMCLAS, CMENREAS_PRIOR, CMONGO, CMSTDAT, CMENDAT, CMSTDAT_, CMENDAT_,
    LASTCHGDATE, ATC
  )

CM3 <- CM3_1 %>%
  mutate(
    FORM = tools::toTitleCase(indsn3),
    CMONGO = "N/A",
    CMDOSE = as.character(CMDOSTOT),
    CMDOSEU = CMDOSTOTU,
    ATC = ifelse(CMCLASCD != "" & CMCLAS != "", 
                 paste(toupper(CMCLASCD), toupper(CMCLAS), sep = " : "), 
                 "")
  ) %>%
  filter(CMDECOD != "" & CMOCCUR == "Y")%>%
  select(
    SUBJID, FORM, CMAEGRPID4, CMMHNO4, CMCAT, CMSCAT, CMSPID, CMTRT, CMDECOD,
    CMTRADNM, CMLOC, CMROUTE, CMINDC, CMDOSE, CMDOSEU, CMDOSFRQ, CMCLASCD,
    CMCLAS, CMENREAS_PRIOR, CMONGO, CMSTDAT, CMENDAT, CMSTDAT_, CMENDAT_,
    LASTCHGDATE, ATC
  )

CM1 <- CM1 %>% mutate(CMDOSE = as.character(CMDOSE))
CM2 <- CM2 %>% mutate(CMDOSE = as.character(CMDOSE))
CM3 <- CM3 %>% mutate(CMDOSE = as.character(CMDOSE))
#CM4 <- CM4 %>% mutate(CMDOSE = as.character(CMDOSE))
data_frames <- list(CM1, CM2, CM3)

combined_CM <- bind_rows(lapply(data_frames, function(df) mutate(df, CMSPID = as.character(CMSPID))))

assign("CM1", combined_CM, envir = .GlobalEnv)

# CM1 <- CM1 %>%
#   mutate(
#     CMSTDAT_ = if_else(is.na(CMSTDAT_), Sys.Date(), CMSTDAT_)
#   )

var1 <- "SUBJID"
var2 <- "MHSPID"
var3 <- "MHTERM"
var4 <- "MHDECOD"
for (i in 1:4) {
  u_checkds_var(indsn11, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}

indsn11 <- get(indsn11, envir = .GlobalEnv)

MH_R1 <- indsn11 %>%
  filter(!is.na(MHSPID) & MHSPID != "") %>%  # Filter rows where MHSPID is not missing or empty
  select(SUBJID, MHSPID, MHTERM, MHDECOD)   # Keep only specified columns
MH_R1 <- MH_R1%>%
  filter(MHDECOD!="")

if (exists(indsn12, envir = .GlobalEnv)) {
  dsidzz <- get(indsn12, envir = .GlobalEnv)
  
  # Check if dsidzz is a data frame and has rows
  if (inherits(dsidzz, "data.frame") && nrow(dsidzz) > 0) {
    
    # Define required variables
    required_vars <- c("SUBJID", "MHSPID", "MHTERM", "MHLLTCDPRESP")
    
    # Check if all required variables exist in the dataset
    missing_vars <- setdiff(required_vars, names(dsidzz))
    if (length(missing_vars) > 0) {
      stop(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
    }
    
    # Step 1: Create MHPRESP_R1
    MHPRESP_R1 <- dsidzz %>%
      rename(MHSPID_ = MHSPID) %>%                     # Rename MHSPID to MHSPID_
      # mutate(
      #   MHSPID = ifelse(is.character(MHSPID_), as.numeric(MHSPID_), MHSPID_)  # Convert to numeric if character
      # ) %>%
      filter(!is.na(MHSPID_)) %>%                       # Filter out rows where MHSPID is missing
      select(SUBJID, MHSPID_, MHTERM, MHLLTCDPRESP)     # Keep only specific columns
    print("one")
    # Step 2: Perform the SQL join
    if (exists(indsn16, envir = .GlobalEnv)) {
      dsid16 <- get(indsn16, envir = .GlobalEnv)
      
      MHPRESP <- MHPRESP_R1 %>%
        left_join(
          dsid16 %>% 
            select(LLTCD, PFTERM) %>% 
            mutate(LLTCD = as.character(LLTCD)),       # Ensure lltcd is character for joining
          by = c("MHLLTCDPRESP" = "LLTCD")
        ) %>%
        distinct()                                    # Remove duplicate rows
      
      # Drop the MHLLTCDPRESP column
      MHPRESP <- MHPRESP %>% select(-MHLLTCDPRESP)
      print("two")
    } else {
      stop("Error: Dataset 'indsn16' does not exist.")
    }
    
  } else {
    # If INDSN12 exists but has no rows, create an empty dataset with the same structure as MH_R1
    if (exists("MH_R1", envir = .GlobalEnv)) {
      print("three")
      MHPRESP <- MH_R1[0, ]  # Create an empty dataset with the same structure as MH_R1
    } else {
      stop("Error: Dataset 'MH_R1' does not exist.")
    }
  }
  
} else {
  # If INDSN12 does not exist, create an empty dataset with the same structure as MH_R1
  if (exists("MH_R1", envir = .GlobalEnv)) {
    MHPRESP <- MH_R1[0, ]  # Create an empty dataset with the same structure as MH_R1
  } else {
    stop("Error: Dataset 'MH_R1' does not exist.")
  }
}


MH_MD <- data.frame(
  MHDECOD = NA,
  MHTERM = NA
)


if (toupper(trim(include_mhpresp)) == "Y") {
  MH_COMB <- dplyr::bind_rows(MH_MD, MH_R1, MHPRESP)
}else {
  # Combine only MH_MD and MH_R1
  MH_COMB <- dplyr::bind_rows(MH_MD, MH_R1)
}
#END-CHUNK:42 UID:1f93930d-0ae6-4d2f-a088-6ca6526f52fe

#START-CHUNK:43 UID:ab1b63ed-9890-41cc-bb83-4533153b3bd0
MH_COMB <- MH_COMB %>%
  filter(!is.na(MHDECOD))

CM2 <- CM1 %>%
  mutate(
    CMENDAT_T = if_else(is.na(CMENDAT_), Sys.Date(), CMENDAT_) #& !is.na(CMSTDAT_)
  )

CM22 <- CM2 %>% 
  mutate(
    CMENDAT_f = case_when(
      !is.na(CMENDAT_) ~ CMENDAT_,
      is.na(CMENDAT_) & is.na(CMENDAT) ~ Sys.Date(),
      is.na(CMENDAT_) & CMENDAT == "" ~ Sys.Date(),
      TRUE ~ as.Date(NA)
    )
  )

CM3 <- CM22 %>%
  mutate(
    CMENDAT_f = as.Date(CMENDAT_f),
    CMSTDAT_ = as.Date(CMSTDAT_),
    CMDURATION_DAYS = as.integer(CMENDAT_f - CMSTDAT_)
  )

cm1a <- CM3 %>%
  mutate(
    key3 = str_c(
      toupper(CMTRT), toupper(CMROUTE), toupper(CMINDC), toupper(CMDOSFRQ),
      sep = "/"
    )
  )

cm1b <- cm1a %>%
  group_by(key3) %>%
  mutate(CM_PT_MAPPED_NUMBER = n_distinct(CMDECOD)) %>%
  ungroup()


cm1b_sorted <- cm1b %>%
  arrange(key3, CMDECOD) %>%
  distinct(key3, CMDECOD, .keep_all = TRUE)

cm1c <- cm1b_sorted %>%
  group_by(key3) %>%
  summarise(
    CM_PT_MAPPED_LIST = str_c(CMDECOD, collapse = " // "),
    CM_PT_MAPPED_NUMBER = first(CM_PT_MAPPED_NUMBER),
    CM_PT_MAPPING_CONSISTENCY = if_else(
      CM_PT_MAPPED_NUMBER == 1, "Ok", "Multiple Mapped"
    ),
    .groups = "drop"
  )


cm1d <- cm1a %>%
  group_by(CMDECOD) %>%
  mutate(ATC_MAPPED_NUMBER = n_distinct(toupper(CMCLASCD))) %>%
  ungroup() %>%
  select(key3, CMDECOD, CMCLASCD, ATC, ATC_MAPPED_NUMBER) %>%
  distinct()


cm1h <- cm1b %>%
  left_join(cm1c %>% select(key3, CM_PT_MAPPED_LIST, CM_PT_MAPPING_CONSISTENCY), by = "key3") %>%
  select(-key3)


if (exists(indsn4, envir = .GlobalEnv)) {
  dsidz <- get(indsn4, envir = .GlobalEnv)
  
  # Check if the dataset has rows
  if (inherits(dsidz, "data.frame") && nrow(dsidz) > 0) {
    
    # Create Prohibited_ATC dataset
    PROHIBITED_ATC <- dsidz %>%
      filter(ATC4_Code != "N/A") %>%  # Filter rows where ATC4_Code is not "N/A"
      mutate(
        DOMAIN = "CM",                # Add Domain column
        PROHIBITED_TERM = "Y"         # Add Prohibited_Term column
      )
    
    # Create Prohibited_CMDECOD dataset
    PROHIBITED_CMDECOD <- dsidz %>%
      filter(ATC4_Code == "N/A") %>%  # Filter rows where ATC4_Code is "N/A"
      mutate(
        DOMAIN = "CM",                # Add Domain column
        PROHIBITED_TERM = "Y",        # Add Prohibited_Term column
        PROHIB_MED = toupper(Preferred_base)  # Add Prohib_Med column (uppercase Preferred_base)
      ) %>%
      select(-Preferred_base)         # Drop Preferred_base column
    
  } else {
    # If INDSN4 exists but has no rows, create empty datasets
    PROHIBITED_ATC <- data.frame(
      ATC4_Code = character(),
      PROHIBITED_TERM = character(),
      stringsAsFactors = FALSE
    )
    
    PROHIBITED_CMDECOD <- data.frame(
      PROHIB_MED = character(),
      PROHIBITED_TERM = character(),
      stringsAsFactors = FALSE
    )
  }
  
} else {
  # If INDSN4 does not exist, create empty datasets
  PROHIBITED_ATC <- data.frame(
    ATC4_Code = character(),
    PROHIBITED_TERM = character(),
    stringsAsFactors = FALSE
  )
  
  PROHIBITED_CMDECOD <- data.frame(
    PROHIB_MED = character(),
    PROHIBITED_TERM = character(),
    stringsAsFactors = FALSE
  )
}

indsn5 <- get(indsn5, envir = .GlobalEnv)
SBJCT_STS_NOX_1 <- indsn5 %>%
  # Rename RNDDTTXT to RD
  rename(RD = RNDDTTXT) %>%
  # Convert RD to a date format and create new columns
  mutate(
    RANDOMZN_DATE = as.Date(RD, format = "%d%b%Y"),  # Convert RD to DATE9. format
    RNDDTTXT = as.Date(RD, format = "%d%b%Y")       # Convert RD to DATE9. format
  ) %>%
  # Filter rows where RD is not null
  filter(!is.na(RD)) %>%
  # Keep only specific columns
  select(SUBJID, RNDDTTXT)%>%
  distinct()

SBJCT_STS_NOX_1 <- SBJCT_STS_NOX_1%>%
  filter(!is.na(RNDDTTXT))%>%
  arrange(SUBJID)

SBJCT_DOB_STG_ <- indsn5 %>%
  dplyr::mutate(DATEOFBIRTH = DOB) %>%
  dplyr::select(SUBJID, DATEOFBIRTH, DOB)

DOB <- SBJCT_DOB_STG_ %>%
  dplyr::mutate(DATEOFBIRTH_ = as.Date(DATEOFBIRTH))


indsn8 <- get(indsn8, envir = .GlobalEnv)

ds <- indsn8 %>%
  select(SUBJID, VISITNUM, DSSCAT, DSDECOD, DSSTDAT)

ds_cat <- ds %>%
  filter(!is.na(DSSCAT)) %>%
  distinct(DSSCAT) %>%
  pull(DSSCAT)

ds_catv <- ds_cat %>%
  str_replace_all(" ", "_") %>%  # Replace spaces with underscores
  substr(1, 26)

tot_ds <- length(ds_cat)

DS_CAT_VARS <- data.frame(DSSCAT = ds_cat, CAT_VAR = ds_catv, stringsAsFactors = FALSE)

if (tot_ds >= 1) {
  
  # Loop over the number of unique DSSCAT categories
  for (ii in 1:tot_ds) {
    cat_name <- DS_CAT_VARS$DSSCAT[ii]  # Get category name
    cat_var  <- DS_CAT_VARS$CAT_VAR[ii]  # Get category variable name
    
    # Ensure valid column name and create new column
    if (!is.na(cat_var) && cat_var != "") {
      ds <- ds %>%
        mutate(!!sym(cat_var) := ifelse(DSSCAT == cat_name, 
                                        paste(DSDECOD, "// DSSTDAT:", DSSTDAT, "// VISITNUM:", VISITNUM), 
                                        NA))
    }
  }
  
  # Create a list to store filtered datasets
  DS_LIST <- list()
  
  for (ii in 1:tot_ds) {
    cat_var <- DS_CAT_VARS$CAT_VAR[ii]  # Get category variable
    
    if (!is.na(cat_var) && cat_var != "") {
      DS_FILTERED <- ds %>% filter(!is.na(!!sym(cat_var)))
      
      if (nrow(DS_FILTERED) > 0) {  # Ensure dataset is not empty
        DS_FILTERED <- DS_FILTERED %>%
          group_by(SUBJID) %>%
          summarise(!!paste0(cat_var, "_S") := paste(!!sym(cat_var), collapse = " , ")) %>%
          ungroup()
        
        DS_LIST[[length(DS_LIST) + 1]] <- DS_FILTERED  # Store dataset in list
      }
    }
  }
  
  # 🛠 Fix: Ensure `DS_LIST` is not empty before merging
  if (length(DS_LIST) > 0) {
    DS_FINAL <- Reduce(function(x, y) full_join(x, y, by = "SUBJID"), DS_LIST)
    
    if (!is.null(DS_FINAL) && ncol(DS_FINAL) > 1) {
      DS_FINAL_CON <- names(DS_FINAL)
      DISP_VARS <- DS_FINAL_CON[DS_FINAL_CON != "SUBJID"]
      DISP_VAR <- paste0("d.", DISP_VARS, collapse = ", ")
    } else {
      message("⚠️ DS_FINAL is empty. No valid DISP_VAR generated.")
    }
  } else {
    DS_FINAL <- data.frame(SUBJID = character())  # Create empty dataset
    message("⚠️ No valid datasets found in DS_LIST. Created empty DS_FINAL.")
  }
}

DS_FINAL_CON <- colnames(DS_FINAL)
DISP_VARS <- DS_FINAL_CON[DS_FINAL_CON != "SUBJID"]  # Remove "SUBJID"

# Create DISP_VAR with "d." prefix and comma-separated format
DISP_VAR <- paste0("d.", DISP_VARS, collapse = ",")

indsn10 <- get(indsn10,envir = .GlobalEnv)
indsn13 <- get(indsn13, envir = .GlobalEnv)
SUBJECT_CM1 <- cm1h %>%
  dplyr::left_join(SBJCT_STS_NOX_1, by = "SUBJID") %>%
  dplyr::left_join(
    indsn10 %>% dplyr::select(SUBJID, CMAEGRPID4 = AEGRPID, AEDECOD), 
    by = c("SUBJID", "CMAEGRPID4"))%>%
  dplyr::left_join(MH_COMB%>% dplyr::select(SUBJID, CMMHNO4 = MHSPID, MHDECOD), 
                   by = c("SUBJID", "CMMHNO4" ))%>%
  dplyr::left_join(indsn13%>% dplyr::select(SUBJID,SEX), 
                   by = c("SUBJID"))%>%
  dplyr::left_join(DOB, by = c("SUBJID")) %>%
  dplyr::mutate(
    SUBJAGE_RANDM = floor(lubridate::interval(DATEOFBIRTH_, RNDDTTXT) / lubridate::years(1)),
    RANDOMIZATION_DATE = RNDDTTXT,
    RANDMTOSTARTDATE_DAYS = as.integer(difftime(CMSTDAT_, RNDDTTXT, units = "days")),
    RANDMTOENDDATE_DAYS = as.integer(difftime(CMENDAT_ +1, RNDDTTXT, units = "days"))
  ) %>%
  dplyr::distinct()

SUBJECT_CM2 <- SUBJECT_CM1 %>%
  mutate(
    # Clean strings
    string1 = str_to_upper(str_replace_all(CMDECOD, "\\s", "")),
    string2 = str_to_upper(str_replace_all(CMTRT, "\\s", "")),
    
    # Calculate substring matches using vectorized logic
    score = map2_dbl(
      string1, string2,
      ~ {
        if (str_length(.x) > 1 && str_length(.y) > 1) {
          substrings1 <- str_sub(.x, 1:(str_length(.x) - 1), 2)
          substrings2 <- str_sub(.y, 1:(str_length(.y) - 1), 2)
          score1 <- sum(substrings1 %in% substrings2)
          score2 <- sum(substrings2 %in% substrings1)
          round((score1 + score2) / (2 * max(str_length(.x) - 1, str_length(.y) - 1)), 2)
        } else {
          NA_real_
        }
      }
    ),
    
    # Final Score_CM calculation
    SCORE_CM = score,
    
    # Calculate CM_PT_Mapping_Accuracy
    CM_PT_MAPPING_ACCURACY = case_when(
      # Perfect Match
      str_to_upper(str_trim(CMTRT)) == str_to_upper(str_trim(CMDECOD)) |
        str_to_upper(str_trim(CMTRT)) == str_to_upper(str_trim(CMTRADNM)) ~ "Perfect Match",
      
      # Good Match
      soundex(str_trim(CMTRT)) == soundex(str_trim(CMDECOD)) |
        soundex(str_trim(CMTRT)) == soundex(str_trim(CMTRADNM)) |
        stringdist::stringdist(str_trim(CMTRT), str_trim(CMDECOD), method = "osa") < spedis_value |
        stringdist::stringdist(str_trim(CMTRT), str_trim(CMTRADNM), method = "osa") < spedis_value |
        str_detect(str_trim(CMTRADNM), fixed(str_trim(CMTRT), ignore_case = TRUE)) |
        str_detect(str_trim(CMDECOD), fixed(str_trim(CMTRT), ignore_case = TRUE)) ~ "Good Match",
      
      # Need Review (default case)
      TRUE ~ "Need Review"
    )
  ) %>%
  filter(str_length(CMTRT) > 1) %>% # Filter rows where CMTRT length > 1
  select(-string1, -string2, -score)

SUBJECT_CM3 <- SUBJECT_CM2 %>%
  dplyr::left_join(PROHIBITED_ATC, by = c("CMCLASCD" = "ATC4_Code")) %>%
  dplyr::left_join(PROHIBITED_CMDECOD, by = c("CMDECOD" = "PROHIB_MED")) %>%
  dplyr::mutate(
    POTENTIALPROHIBITED_TERM = dplyr::coalesce(PROHIBITED_TERM.x, PROHIBITED_TERM.y, "N")
  ) %>%
  dplyr::filter(!is.na(SUBJID)) %>%
  dplyr::distinct()

indsn15 <- get(indsn15, envir = .GlobalEnv)
indsn14 <- get(indsn14,envir = .GlobalEnv)

SUBJECT_CMTREATMENT_1 <- SUBJECT_CM3 %>%
  left_join(indsn15 %>% select(SUBJID, SITE,COUNTRY = SITECOUNTRY), by = "SUBJID") %>%
  left_join(DS_FINAL ,by = "SUBJID")%>%
  left_join(indsn14 ,by = c("CMDECOD" = "ENGLISH_TEXT")) %>%
  select(
    CM_PT_MAPPING_CONSISTENCY,
    CM_PT_MAPPING_ACCURACY,
    POTENTIALPROHIBITED_TERM,
    FORM,
    SITE,
    COUNTRY,
    SUBJID,
    SEX,
    SUBJAGE_RANDM,
    CMSPID,
    CMCAT,
    CMSCAT,
    VERBATIMTERM = CMTRT,
    CM_PT = CMDECOD,
    CMTRADNM,
    CMLOC,
    CMROUTE,
    CMDOSE,
    CM_UNITS = CMDOSEU,
    CMDOSFRQ,
    CM_STARTDATE = CMSTDAT,
    CM_ENDDATE = CMENDAT,
    REASON_PRIORMED_DISCONT = CMENREAS_PRIOR,
    RANDOMIZATION_DATE,
    RANDMTOSTARTDATE_DAYS,
    RANDMTOENDDATE_DAYS,
    CMDURATION_DAYS,
    CMONGO,
    CMINDC,
    CMAEGRPID4,
    AE_PT = AEDECOD,
    CMMHNO4,
    MH_PT = MHDECOD,
    CM_PT_MAPPED_NUMBER,
    CM_PT_MAPPED_LIST,
    ATC,
    AVAILABLEATC_MAP = RefATC_Map,
    AVAILABLEATC_LIST = RefATC_List,
    any_of(DISP_VAR), # DYNAMICALLY INCLUDE DISP_VAR
    SCORE_CM,
    LASTCHGDATE
  )%>%
  distinct()

LDD_R1 <- get(indsn6, envir = .GlobalEnv)
LDD <- LDD_R1 %>%
  arrange(SUBJID, desc(ECSTDAT)) %>%
  group_by(SUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(SUBJID, LAST_DOSE_DATE = ECSTDAT)

IFD_R1 <- get(indsn7, envir = .GlobalEnv)
IFD_R1 <- u_getdate("IFD_R1", "DSSTDAT_ICMO", "DSSTDAT_ICDD", "DSSTDAT_ICYY", "IFC_DATE")
IFD <- IFD_R1 %>%
  filter(!is.na(IFC_DATE)) %>%
  select(SUBJID, IFC_DATE)%>%
  arrange(SUBJID, IFC_DATE)
IFD <- IFD %>%
  group_by(SUBJID) %>%
  slice_head(n = 1) %>%
  ungroup()

SUBJECT_CMTREATMENT_2 <- SUBJECT_CMTREATMENT_1 %>%
  dplyr::left_join(LDD, by = c("SUBJID" = "SUBJID")) %>%
  dplyr::distinct()

SUBJECT_CMTREATMENT <- SUBJECT_CMTREATMENT_2 %>%
  dplyr::left_join(IFD, by = c("SUBJID" = "SUBJID")) %>%
  dplyr::distinct()

SUBJECT_CMTREATMENT <- SUBJECT_CMTREATMENT %>%
  mutate(
    ERR_MSG = ifelse(toupper(CM_PT) == "METFORMIN" & CMDOSE <= 1500, 
                     "METFORMIN Less Than 1500", 
                     NA)
  )


SUBJECT_CMTREATMENT <- SUBJECT_CMTREATMENT %>%
  filter(!is.na(RANDOMIZATION_DATE)) %>%
  arrange(SUBJID)

SUBJECT_CMTREATMENT <- SUBJECT_CMTREATMENT %>%
  arrange(SUBJID, SITE, CMONGO, CMSPID)

# unique_count <- SUBJECT_CMTREATMENT %>%
#   count(CM_PT_MAPPING_CONSISTENCY) %>%
#   arrange(desc(n))



rm(CM1, ds, ds1, ds2, CM1_1, CM2_1, CM3_1, CM2, CM3, 
   cm1a, cm1b, cm1b_, cm1c, cm1d, cm1h, SBJCT_DOB_STG_, 
   SUBJECT_CM1, DOB, SUBJECT_CM2, SUBJECT_CM3, SBJCT_STS_NOX_1, 
   PROHIBITED_ATC, PROHIBITED_CMDECOD, LDD_R1, LDD, IFD_R1, IFD, 
   SUBJECT_CMTREATMENT_1, SUBJECT_CMTREATMENT_2, CHECK_DS_CON_1, CHECK_DS_CON_2, 
   CHECK_DS_CON_3, CHECK_MD, CHECK_DS_CON_4, CHECK_DS_CON_5, CHECK_DS_CON_6, 
   CHECK_DS_CON_7, CM4_1, CM4, MH_R1, MHPRESP, MHPRESP_R1, MH_MD, MH_COMB)



