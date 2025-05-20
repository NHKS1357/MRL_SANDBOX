
d_Subject_AE_MH <- function(indsn1,indsn2,indsn3,indsn4,indsn5,indsn6,indsn7,indsn8,indsn9,indsn11,indsn12,DS_LIST,VAR_LIST,FORM_LIST,indsn13,indsn14) {
  
  var1 <- "SUBJID"
  var2 <- "AETERM"
  var3 <- "AEDECOD"
  var4 <- "AEGRPID"
  var5 <- "AELLT"
  
  for(i in 1:5) {
    u_checkds_var(indsn1, base::get(base::paste0("var", i)))
    if (chk == 0) {
      base::return(NULL)
    }
  }
  
  
  var1 <- "SUBJID"
  var2 <- "MHTERM"
  var3 <- "MHDECOD"
  var4 <- "MHSPID"
  var5 <- "MHLLT"
  
  for(i in 1:5) {
    u_checkds_var(indsn2, base::get(base::paste0("var", i)))
    if (chk == 0) {
      base::return(NULL)
    }
  }
  
  # Define dataset name
  
  # Check if dataset exists
  if (exists(indsn4, envir = .GlobalEnv)) {
    
    dsidzz <- get(indsn4, envir = .GlobalEnv)
    
    # Check if dataset has observations
    nobszz <- ifelse(is.null(dsidzz), 0, nrow(dsidzz))
    
    if (nobszz != 0) {
      
      # Extract the second column name dynamically
      if (ncol(dsidzz) >= 2) {
        trial <- colnames(dsidzz)[4]
        cat("\n✅ Extracted 'trial' variable:", trial, "\n")
      } else {
        stop("\n🚨 Error: The dataset does not have at least two columns.")
      }
      
      var1 <- "PT"
      var2 <- as.character(colnames(dsidzz)[4])
      # Check all variables
      chk <- TRUE
      for (var in c(var1, var2)) {
        u_checkds_var(indsn4, var)
        if (chk == 0) {
          base::return(NULL)
        }
      }
      
    } else {
      # Create empty dataset if there are no observations
      PROHIBITED_MH <- data.frame(PT = character(), PROHIBITED_TERM = character(), stringsAsFactors = FALSE)
      assign("PROHIBITED_MH", PROHIBITED_MH, envir = .GlobalEnv)
    }
    
  } else {
    # Create empty dataset if indsn4 does not exist
    PROHIBITED_MH <- data.frame(PT = character(), PROHIBITED_TERM = character(), stringsAsFactors = FALSE)
    assign("PROHIBITED_MH", PROHIBITED_MH, envir = .GlobalEnv)
  }
  
  # Check if dataset exists
  if (exists(indsn3, envir = .GlobalEnv)) {
    
    dsidzz <- get(indsn3, envir = .GlobalEnv)
    
    # Check if dataset has observations
    nobszz <- ifelse(is.null(dsidzz), 0, nrow(dsidzz))
    
    if (nobszz != 0) {
      # Extract the second column name dynamically
      if (ncol(dsidzz) >= 2) {
        trial <- colnames(dsidzz)[2]
        cat("\n✅ Extracted 'trial' variable:", trial, "\n")
      } else {
        stop("\n🚨 Error: The dataset does not have at least two columns.")
      }
      
      var1 <- "PT"
      var2 <- as.character(colnames(dsidzz)[2])
      # Check all variables
      chk <- TRUE
      for (var in c(var1, var2)) {
        u_checkds_var(indsn3, var)
        if (chk == 0) {
          base::return(NULL)
        }
      }
      
    } else {
      # Create empty dataset if there are no observations
      PROHIBITED_AE <- data.frame(PT = character(), PROHIBITED_TERM = character(), stringsAsFactors = FALSE)
      assign("PROHIBITED_AE", PROHIBITED_AE, envir = .GlobalEnv)
    }
    
  } else {
    # Create empty dataset if indsn3 does not exist
    PROHIBITED_AE <- data.frame(PT = character(), PROHIBITED_TERM = character(), stringsAsFactors = FALSE)
    assign("PROHIBITED_AE", PROHIBITED_AE, envir = .GlobalEnv)
  }
  
  
  var1 <- "HLGTERM"
  for(i in 1:1) {
    u_checkds_var(indsn5, base::get(base::paste0("var", i)))
    if (chk == 0) {
      base::return(NULL)
    }
  }
  
  var1 <- "SUBJID"
  var2 <- "ECSTDATMO"
  var3 <- "ECSTDATDD"
  var4 <- "ECSTDATYY"
  for(i in 1:4) {
    u_checkds_var(indsn6, base::get(base::paste0("var", i)))
    if (chk == 0) {
      next      }
  }
  
  # Checking variables in the dataset
  var1 <- "HLGTERM"
  var2 <- "DSSTDAT_ICMO"
  var3 <- "DSSTDAT_ICDD"
  var4 <- "DSSTDAT_ICYY"
  
  # Run checks on all variables
  chk <- 1  # Initialize chk to a valid value
  for (i in 1:4) {
    u_checkds_var(indsn7, base::get(base::paste0("var", i)))  # Call function
    
    if (chk == 0) {  # If a variable is missing, exit
      next  
    }
  }
  
  var1 <- "SUBJID"
  var2 <- "DSSTDAT"
  
  chk <- 1  # Initialize chk to a valid value
  for (i in 1:2) {
    u_checkds_var(indsn8, base::get(base::paste0("var", i)))  # Call function
    
    if (chk == 0) {  # If a variable is missing, exit
      next  
    }
  }
  
  var1 <- "RNDDTTXT"
  var2 <- "DOB"
  
  chk <- 1  # Initialize chk to a valid value
  for (i in 1:2) {
    u_checkds_var(indsn9, base::get(base::paste0("var", i)))  # Call function
    
    if (chk == 0) {  # If a variable is missing, exit
      next  
    }
  }
  
  
  var1 <- "SUBJID"
  var2 <- "VISDAT"
  
  chk <- 1  # Initialize chk to a valid value
  for (i in 1:2) {
    u_checkds_var(indsn11, base::get(base::paste0("var", i)))  # Call function
    
    if (chk == 0) {  # If a variable is missing, exit
      next  
    }
  }
  
  var1 <- "SUBJID"
  var2 <- "AESTEXRES2"
  
  chk <- 1  # Initialize chk to a valid value
  for (i in 1:2) {
    u_checkds_var(indsn12, base::get(base::paste0("var", i)))  # Call function
    
    if (chk == 0) {  # If a variable is missing, exit
      next  
    }
  }
  
  
  ds_names <- strsplit(DS_LIST, "|", fixed = TRUE)[[1]]
  var_names <- strsplit(VAR_LIST, "|", fixed = TRUE)[[1]]
  
  m <- 0
  
  for (m in seq_along(ds_names)) {
    if (ds_names[m] == "") break
    
    dsn <- ds_names[m]
    dsn_var <- var_names[m]
    
    # Check if dataset exists
    if (exists(dsn)) {
      # Get number of observations
      nobszz <- nrow(get(dsn))
      
      # Process non-empty datasets
      if (nobszz != 0) {
        chk <- TRUE
        u_checkds_var(dsn, dsn_var)
        if (chk == 0) {
          next
        }
      }
    } else {
      # Create empty dataset if it doesn't exist
      assign(dsn, data.frame(
        subjid = character(),
        unique_var = numeric(),
        UNIQUE_ID = numeric(),
        src = character(),
        formeid = character()
      ))
    }
  }
  
  DSN_MD <- data.frame(
    SUBJID = character(),
    UNIQUE_ID = numeric(),
    SRC = character(),
    FORMEID = character(),
    stringsAsFactors = FALSE
  )
  
  DS_LIST <- unlist(strsplit(DS_LIST, "|", fixed = TRUE))
  VAR_LIST <- unlist(strsplit(VAR_LIST, "|", fixed = TRUE))
  FORM_LIST <- unlist(strsplit(FORM_LIST, "|", fixed = TRUE))
  
  # Loop through the datasets
  for (mm in seq_along(DS_LIST)) {
    # Get current dataset name, variable, and form
    dsn <- toupper(DS_LIST[mm])
    dsn_var <- toupper(VAR_LIST[mm])
    dsn_form <- toupper(FORM_LIST[mm])
    
    # Create R1 dataframe
    dsn_r1 <- get(dsn)[, c("SUBJID", dsn_var, "FORMEID")]
    dsn_r1$SRC <- dsn
    
    # Create R2 dataframe
    dsn_r2 <- dsn_r1[!is.na(dsn_r1[[dsn_var]]) & 
                       tolower(dsn_r1$FORMEID) == tolower(dsn_form), 
                     c("SUBJID", dsn_var, "SRC", "FORMEID")]
    
    names(dsn_r2)[names(dsn_r2) == dsn_var] <- "UNIQUE_ID"
    
    # Append to final dataframe
    DSN_MD <- rbind(DSN_MD, dsn_r2)
  }
  
  DSN_FINAL <- DSN_MD
  
  # Sort dsn_final by SUBJID and UNIQUE_ID
  DSN_FINAL <- DSN_FINAL %>%
    arrange(SUBJID, UNIQUE_ID)
  assign("DSN_FINAL",DSN_FINAL, envir = .GlobalEnv)
  
  
  DSN_REL <- DSN_FINAL %>%
    arrange(SUBJID, UNIQUE_ID) %>%  # Ensure sorted by subjid and unique_id
    group_by(SUBJID, UNIQUE_ID) %>%
    mutate(
      DOMAIN_TO_LINK_TO = "AE",
      SOURCES = paste(unique(SRC), collapse = " , ")  # Concatenate unique 'SRC' values
    ) %>%
    slice_tail(n = 1) %>%  # Keep only the last occurrence within the group
    select(-SRC) %>%  # Drop SRC column
    ungroup()
  assign("DSN_REL",DSN_REL, envir = .GlobalEnv)
  
  indsn11 <- get(indsn11)
  
  SV_V1 <- indsn11 %>%
    filter(VISITNUM == "1") %>%
    select(SUBJID, VISITNUM, VISDATMO, VISDATDD, VISDATYY) %>%
    
    # Step 2: Combine columns into a single date column
    mutate(
      VISIT_DATE = make_date(year = as.numeric(VISDATYY), 
                             month = as.numeric(VISDATMO), 
                             day = as.numeric(VISDATDD))
    )
  
  SV_V1 <- u_getdate(SV_V1,VISDATMO,VISDATDD,VISDATYY,VISIT_DATE)
  assign("SV_V1",SV_V1, envir = .GlobalEnv)
  
  if (is.character(indsn1)) {
    indsn1 <- get(indsn1, envir = .GlobalEnv)
  }
  AE3001_ <- indsn1 %>%
    mutate(
      AELLTCD_n = as.character(AELLTCD),  # Convert AELLTCD to character
      
      # Conditional transformation for AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE
      AESDTH_ = ifelse(AESDTH == "Y", "Death", ""),
      AESLIFE_ = ifelse(AESLIFE == "Y", "Life threatening", ""),
      AESHOSP_ = ifelse(AESHOSP == "Y", "Hospitalization (initial or prolonged)", ""),
      AESDISAB_ = ifelse(AESDISAB == "Y", "Disability or Permanent Damage", ""),
      AESCONG_ = ifelse(AESCONG == "Y", "Congenital anomaly or birth defect", ""),
      AESMIE_ = ifelse(AESMIE == "Y", "medically important event not covered by other serious criteria", "")
    ) %>%
    select(-AESDTH, -AESLIFE, -AESHOSP, -AESDISAB, -AESCONG, -AESMIE) %>%
    rename(
      AESDTH = AESDTH_,
      AESLIFE = AESLIFE_,
      AESHOSP = AESHOSP_,
      AESDISAB = AESDISAB_,
      AESCONG = AESCONG_,
      AESMIE = AESMIE_
    )
  
  assign("AE3001_",AE3001_, envir = .GlobalEnv)
  print("AE3001_ is done")
  #MH7001 <- u_getdate( indsn2,"MHSTDATMO","MHSTDATDD","MHSTDATYY","MHENDAT_")
  # Apply u_getdate and update the dataset
  MH7001_1 <- u_getdate(indsn2, "MHENDATMO", "MHENDATDD", "MHENDATYY", "MHENDAT_")
  assign("MH7001_1", MH7001_1, envir = .GlobalEnv)  # Ensure it's saved globally

  MH7001_2 <- u_getdate("MH7001_1", "MHSTDATMO", "MHSTDATDD", "MHSTDATYY", "MHSTDAT_")
  AE3001_1 <- u_getdate("AE3001_", "AEENDATMO", "AEENDATDD", "AEENDATYY", "AEENDAT_")
  assign("AE3001_1", AE3001_1, envir = .GlobalEnv)  # Ensure it's saved globally
  
  AE3001_2 <- u_getdate("AE3001_1", "AESTDATMO", "AESTDATDD", "AESTDATYY", "AESTDAT_")
  
  MH1 <- MH7001_2 %>%
    filter(MHTERM != "") %>%  # Equivalent to WHERE MHTERM ne " ";
    mutate(
      DOMAIN = "MH",
      SER_REASON = "N/A",
      EVENT_SER = "N/A",
      TREATMENT_ACTION = "N/A",
      EVENT_OUTCOME = "N/A",
      REL_STUDYTREATMENT = "N/A",
      REL_NONSTUDYTREATMENT = "N/A",
      AESPID = NA  # Assign missing value
    ) %>%
    rename(
      EVENT_TERM = MHTERM,
      PT = MHDECOD,
      EVENT_ONGO = MHONGO,
      EVENT_SEV = MHSEV,
      EVENT_ID = MHSPID,
      LLT = MHLLT,
      LLTCD = MHLLTCD,
      EVENT_ENDAT = MHENDAT,
      EVENT_ENDAT_ = MHENDAT_,
      EVENT_STARTDATE = MHSTDAT,
      EVENT_STARTDATE_ = MHSTDAT_
    ) %>%
    select(
      SUBJID, DOMAIN,SITE, EVENT_ID, AESPID, LLT, LLTCD, EVENT_ONGO, EVENT_SER,
      SER_REASON, EVENT_SEV, TREATMENT_ACTION, EVENT_OUTCOME, REL_STUDYTREATMENT,
      REL_NONSTUDYTREATMENT, EVENT_STARTDATE, EVENT_STARTDATE_, EVENT_ENDAT,
      EVENT_ENDAT_, EVENT_TERM, PT, LASTCHGDATE
    )
  assign("MH1", MH1, envir = .GlobalEnv)
  
  if (base::exists(indsn14)) {
    dsidz <- base::get(indsn14)
    nobsz <- base::nrow(dsidz)
    
    if (nobsz != 0) {
      var1 <- "SUBJID"
      var2 <- "MHSPID"
      var3 <- "MHLLTCDPRESP"
      var4 <- "MHTERM"
      
      for (i in 1:4) {
        u_checkds_var(indsn14, base::get(base::paste0("var", i)))
        if (chk == 0) {
          base::return(NULL)
          next
        }
      }
      MHPRESP1 <- dsidz
    }
    
  }
  assign("MHPRESP1", MHPRESP1, envir = .GlobalEnv)

  MHPRESP1_1 <- u_getdate("MHPRESP1", "MHSTDATMO", "MHSTDATDD", "MHSTDATYY", "MHSTDAT_")
  assign("MHPRESP1_1", MHPRESP1_1, envir = .GlobalEnv)  # Ensure it's saved globally
  # Verify the column exists before renaming
  
  MHPRESP1_2 <- u_getdate("MHPRESP1_1","MHENDATMO","MHENDATDD","MHENDATYY","MHENDAT_")
  
  MHPRESP_R1 <- MHPRESP1_2 %>%
    # Rename MHSPID to MHSPID_
    rename(MHSPID_ = MHSPID) %>%
    # Replace empty strings with NA and then convert to numeric
    mutate(
      MHSPID_ = ifelse(MHSPID_ == "", NA, MHSPID_), # Replace "" with NA
      MHSPID = as.numeric(MHSPID_) # Convert to numeric
    ) %>%
    # Filter rows where MHSPID is not missing
    filter(!is.na(MHSPID)) %>%
    # Select specific columns for the output dataset
    select(SUBJID,SITE,MHSPID, MHOCCUR, MHONGO, MHTERM, MHLLTCDPRESP, 
           LASTCHGDATE, MHSTDAT, MHSTDAT_, MHENDAT, MHENDAT_)

  assign("MHPRESP_R1", MHPRESP_R1, envir = .GlobalEnv)

  
  indsn5 <- base::get(indsn5) %>%
    mutate(LLTCD = as.character(LLTCD))
  
  MHPRESP <- MHPRESP_R1 %>%
    left_join(indsn5 %>% select(LLTCD, PFTERM, LLTERM), 
              by = c("MHLLTCDPRESP" = "LLTCD")) %>%
    mutate(MHDECOD = PFTERM, MHLLT = LLTERM) %>%
    select(everything(), -PFTERM, -LLTERM) %>%  # Remove redundant columns
    distinct() 
  assign("MHPRESP", MHPRESP, envir = .GlobalEnv)
  
  if (exists("MHPRESP", envir = .GlobalEnv)) {
    
    # Process MHPRESP dataset
    MHPRESP_1 <- MHPRESP %>%
      mutate(
        EVENT_TERM = ifelse(!is.na(MHTERM) & toupper(trimws(MHOCCUR)) == "Y", MHTERM, NA_character_),
        PT = ifelse(!is.na(MHTERM) & toupper(trimws(MHOCCUR)) == "Y", MHDECOD, NA_character_),
        LLT = ifelse(!is.na(MHTERM) & toupper(trimws(MHOCCUR)) == "Y", MHLLT, NA_character_),
        DOMAIN = "MHPRESP",
        SER_REASON = "N/A",
        EVENT_SER = "N/A",
        TREATMENT_ACTION = "N/A",
        EVENT_OUTCOME = "N/A",
        REL_STUDYTREATMENT = "N/A",
        REL_NONSTUDYTREATMENT = "N/A",
        EVENT_SEV = "N/A"
      ) %>%
      rename(
        EVENT_ENDAT = MHENDAT,
        EVENT_ENDAT_ = MHENDAT_,
        EVENT_STARTDATE = MHSTDAT,
        EVENT_STARTDATE_ = MHSTDAT_,
        EVENT_ONGO = MHONGO,
        EVENT_ID = MHSPID
      ) %>%
      filter(!is.na(EVENT_TERM))  # Keep only rows where EVENT_TERM is not missing
    
  } else {
    # Create empty dataset with the same structure as MH1
    MHPRESP_1 <- MH1[0, ]
  }
  
  # Assign back to global environment
  assign("MHPRESP_1", MHPRESP_1, envir = .GlobalEnv)
  
  AEX <- AE3001_2 %>%
    select(starts_with("AERELNST"))
  assign("AEX", AEX, envir = .GlobalEnv)
  
  AEY <- tibble(NAME = colnames(AEX)) %>%
    distinct() %>%
    arrange(NAME)
  
  AERELNST_VAR <- paste(AEY$NAME, collapse = ",")
  
  AE1 <- AE3001_2 %>%
    filter(AETERM != "") %>% # Equivalent to 'where AETERM ne " "'
    mutate(
      SER_REASON = paste(AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE, sep = " / "),
      REL_NONSTUDYTREATMENT = paste(!!!syms(strsplit(AERELNST_VAR, ",")[[1]]), sep = " // "), # Equivalent to SAS's CATX
      DOMAIN = "AE",
      EVENT_TERM = AETERM,
      PT = AEDECOD,
      EVENT_ONGO = AEONGO,
      EVENT_ID = AEGRPID,
      LLT = AELLT,
      LLTCD = AELLTCD,
      TREATMENT_ACTION = AEACN,
      EVENT_OUTCOME = AEOUT,
      EVENT_SEV = AESEV,
      EVENT_ENDAT = AEENDAT,
      EVENT_STARTDATE = AESTDAT,
      EVENT_ENDAT_ = AEENDAT_,
      EVENT_STARTDATE_ = AESTDAT_,
      REL_STUDYTREATMENT = trimws(AEREL),
      EVENT_SER = trimws(AESER)
    ) %>%
    select(
      SUBJID, DOMAIN, EVENT_ID, AESPID, LLT, LLTCD, EVENT_ONGO, EVENT_SER, SER_REASON, 
      EVENT_SEV, TREATMENT_ACTION, EVENT_OUTCOME, REL_STUDYTREATMENT, REL_NONSTUDYTREATMENT, 
      EVENT_STARTDATE, EVENT_STARTDATE_, EVENT_ENDAT, EVENT_ENDAT_, EVENT_TERM, PT, LASTCHGDATE
    )
  
  # AE_MH_MD <- data.frame(
  #   EVENT_TERM = character(),
  #   DOMAIN = character(),
  #   stringsAsFactors = FALSE
  # )
  
  AE1 <- AE1 %>% mutate(EVENT_ONGO = as.character(EVENT_ONGO))
  MH1 <- MH1 %>% mutate(EVENT_ONGO = as.character(EVENT_ONGO))
  MHPRESP_1 <- MHPRESP_1 %>% mutate(EVENT_ONGO = as.character(EVENT_ONGO))
  MH1_1 <- bind_rows(AE1[0, ], MH1, MHPRESP_1)
  
  AE1_2 <- bind_rows(AE1, MH1_1)
  # Filter DICT_MEDDRA_ dataset where PRISOCFLG is "Y"
  DICT_MEDDRA_ <- indsn5 %>%
    filter(trimws(toupper(PRISOCFLG)) == "Y")
  
  # Convert LLTCD to numeric in AE1
  AE1A <- AE1_2 %>%
    mutate(LLTCD_n = as.numeric(LLTCD))
  
  AE1B <- AE1A %>%
    mutate(LLTCD_n = as.character(LLTCD_n)) %>% # Convert LLTCD_n to character
    left_join(DICT_MEDDRA_ %>% mutate(LLTCD = as.character(LLTCD)), # Ensure LLTCD is character
              by = c("LLTCD_n" = "LLTCD")) %>%
    select(
      everything(),               # Select all columns from AE1A
      HLGT = HLGTERM,             # Add HLGT column (alias for HLGTERM)
      SOC = SOCTERM               # Add SOC column (alias for SOCTERM)
    )

  # AE2 <- AE1B %>%
  #   rename(EE = EVENT_ENDAT_) %>%
  #   mutate(
  #     EVENT_ENDAT_ = case_when(
  #       !is.na(EE) ~ EE,
  #       is.na(EE) & !is.na(EVENT_STARTDATE_) & toupper(trimws(EVENT_ONGO)) != 'N' ~ Sys.Date(),
  #       TRUE ~ as.Date(NA)
  #     
  #     )
  #   ) %>%
  #   select(-EE)  # Remove intermediate column
  
  AEend <- AE1B %>% 
    mutate(EVENT_ENDAT_D = if_else(
      is.na(EVENT_ENDAT_),
      Sys.Date(),   # Today's date
      EVENT_ENDAT_
    ),
    EVENT_ENDAT_D = as.Date(EVENT_ENDAT_D))
  
  # AE3 <- AE2 %>%
  #   mutate(
  #     EVENTDURATION_DAYS = as.integer(difftime(EVENT_ENDAT_ + 1, EVENT_STARTDATE_, units = "days")),
  #     VERBATIMTERM = toupper(trimws(EVENT_TERM)),
  #     COMP_VERBTM = toupper(gsub("\\s+", "", EVENT_TERM))  # Remove spaces from EVENT_TERM
  #   )
  
  AEend_1 <- AEend %>% 
    mutate(
      EVENT_ENDAT_f = case_when(
        !is.na(EVENT_ENDAT_) ~ EVENT_ENDAT_,
        is.na(EVENT_ENDAT_) & is.na(EVENT_ENDAT) ~ Sys.Date(),
        is.na(EVENT_ENDAT_) & EVENT_ENDAT == "" ~ Sys.Date(),
        TRUE ~ as.Date(NA)
      )
    )
  
  AE3 <- AEend_1 %>%
    mutate(
      EVENT_ENDAT_D = as.Date(EVENT_ENDAT_D),           # Use correct date format if needed!
      EVENT_STARTDATE_ = as.Date(EVENT_STARTDATE_),     # Use correct date format if needed!
      EVENTDURATION_DAYS = as.integer(EVENT_ENDAT_f - EVENT_STARTDATE_),
      #EVENTDURATION_DAYS = as.integer(EVENT_ENDAT_D - EVENT_STARTDATE_),
      VERBATIMTERM = toupper(trimws(EVENT_TERM)),
      COMP_VERBTM  = toupper(gsub("\\s+", "", EVENT_TERM))
    )
  
  
  AE4 <- AE3 %>%
    group_by(DOMAIN, COMP_VERBTM) %>%
    summarise(
      PT = unique(toupper(trimws(PT))),
      PT_MAPPED_NUMBER = n_distinct(toupper(trimws(PT)))
    ) %>%
    ungroup()
  
  AE4 <- AE4 %>% arrange(DOMAIN, COMP_VERBTM)
  
  AE5 <- AE4 %>%
    group_by(DOMAIN, COMP_VERBTM) %>%
    summarise(
      PT_MAPPED_LIST = paste(unique(PT), collapse = " // "),
      PT_MAPPED_NUMBER = first(PT_MAPPED_NUMBER)
    ) %>%
    ungroup()
  
  AE6 <- AE3 %>%
    left_join(AE5, by = c("COMP_VERBTM","DOMAIN"))
  
  PROHIBITED_AE <- get(indsn3) %>%
    mutate(DOMAIN = "AE") %>% 
    mutate(PROHIBITED_TERM = ifelse(!is.na(EZEF),"Y","N"))
  
  PROHIBITED_MH <- get(indsn4) %>%
    mutate(DOMAIN = "MH") %>% 
    mutate(PROHIBITED_TERM = ifelse(!is.na(EZEF),"Y","N"))
  
  PROHIBITED_TERMS <- bind_rows(PROHIBITED_AE, PROHIBITED_MH) %>%
    filter(PROHIBITED_TERM == "Y")%>%
    mutate(PT_TERMS = toupper(PT)) %>%
    select(-PT)  # Drop the PT column
  
  SBJCT_STS_NOX_1 <- get(indsn9) %>%
    filter(!is.na(RNDDTTXT)) %>%  # Equivalent to WHERE RNDDTTXT IS NOT NULL
    mutate(RANDOMZN_DATE = as.Date(RNDDTTXT, format="%d%b%Y")) %>%  # Convert RNDDTTXT to Date format
    select(SUBJID, RANDOMZN_DATE)%>%
    distinct()  # Keep only SUBJID and RANDOMZN_DATE
  
  
  SBJCT_STS_NOX_2 <- SBJCT_STS_NOX_1 %>%
    group_by(SUBJID) %>%
    slice(1) %>%
    ungroup()
  
  SBJCT_DOB_STG_ <- get(indsn9) %>%
    mutate(DATEOFBIRTH = DOB) %>%  # Assign DOB to dateofbirth
    select(SUBJID, DATEOFBIRTH, DOB)  # Keep only necessary columns
  
  DOB <- SBJCT_DOB_STG_ %>%
    mutate(DATEOFBIRTH_ = as.Date(DATEOFBIRTH, origin = "1970-01-01")) %>%  # Convert date to DATE9. equivalent
    mutate(DATEOFBIRTH_ = format(DATEOFBIRTH_, "%d%b%Y"))  # 
  
  DS <- get(indsn13) %>%
    mutate(
      # Populate STUDY_DISPOSITION if DSSCAT == "STUDY DISPOSITION"
      STUDY_DISPOSITION = ifelse(DSSCAT == "STUDY DISPOSITION", 
                                 paste(DSDECOD, "// DSSTDAT:", DSSTDAT), 
                                 NA_character_),
      # Populate TREAT_DISPOSITION if DSSCAT == "TREATMENT DISPOSITION"
      TREAT_DISPOSITION = ifelse(DSSCAT == "TREATMENT DISPOSITION", 
                                 paste(DSDECOD, "// DSSTDAT:", DSSTDAT), 
                                 NA_character_),
      # Populate STUDY_DRUG_TREAT if DSSCAT == "STUDY DRUG TREATMENT"
      STUDY_DRUG_TREAT = ifelse(DSSCAT == "STUDY DRUG TREATMENT", 
                                paste(DSDECOD, "// DSSTDAT:", DSSTDAT), 
                                NA_character_)
    )
  
  SV_sorted <- SV1001 %>%
    filter(VISITOCCUR == "Y") %>%
    arrange(SUBJID, VISDAT)
  
  first_vis <- SV_sorted %>%
    group_by(SUBJID) %>%
    slice(1) %>% # Select the first row for each subjid
    ungroup()
  
  first_vis <- first_vis %>%
    mutate(VISDAT_FRT = as.Date(VISDAT, format = "%Y-%m-%d")) %>%
    select(SUBJID, VISITNUM, VISDAT_FRT)
  
  # Format VISDAT_frt as date9 (R default format is YYYY-MM-DD)
  first_vis$VISDAT_FRT <- format(first_vis$VISDAT_FRT, "%Y-%m-%d")
  
  
  UNIQUE_CATS <- DS %>% filter(!is.na(DSSCAT) & DSSCAT!="") %>% distinct(DSSCAT)
  
  DS_CAT_VARS <- UNIQUE_CATS %>% 
    mutate(CAT_VAR = substr(gsub(" ", "_", DSSCAT), 1, 26))
  
  tot_ds <- nrow(UNIQUE_CATS)
  
  if (tot_ds >= 1) {
    
    for (ii in 1:tot_ds) {
      CAT_NAME <- DS_CAT_VARS$DSSCAT[ii]
      CAT_VAR <- DS_CAT_VARS$CAT_VAR[ii]
      
      if (!is.na(CAT_VAR) && CAT_VAR != "") {  # Ensure valid variable name
        DS <- DS %>%
          mutate(!!sym(CAT_VAR) := ifelse(DSSCAT == CAT_NAME, 
                                          paste(DSDECOD, "// DSSTDAT:", DSSTDAT, "// VISITNUM:", VISITNUM), 
                                          NA))
      }
    }
    
    DS_LIST <- list()
    
    for (ii in 1:tot_ds) {
      CAT_VAR <- DS_CAT_VARS$CAT_VAR[ii]
      
      if (!is.na(CAT_VAR) && CAT_VAR != "") {  # Ensure valid variable name
        DS_FILTERED <- DS %>% filter(!is.na(!!sym(CAT_VAR)))
        
        if (nrow(DS_FILTERED) > 0) {  # Avoid empty summarization
          DS_FILTERED <- DS_FILTERED %>%
            group_by(SUBJID) %>%
            summarise(!!paste0(CAT_VAR, "_S") := paste(!!sym(CAT_VAR), collapse = " , ")) %>%
            ungroup()
          
          DS_LIST[[length(DS_LIST) + 1]] <- DS_FILTERED
        }
      }
    }
    
    if (length(DS_LIST) > 0) {  # Ensure DS_LIST has valid data before joining
      DS_FINAL <- Reduce(function(x, y) full_join(x, y, by = "SUBJID"), DS_LIST)
      
      if (!is.null(DS_FINAL) && ncol(DS_FINAL) > 1) {  # Ensure DS_FINAL has columns
        DS_FINAL_CON <- names(DS_FINAL)
        DISP_VARS <- DS_FINAL_CON[DS_FINAL_CON != "SUBJID"]
        DISP_VAR <- paste0("d.", DISP_VARS, collapse = ", ")
      } else {
        message("⚠️ DS_FINAL is empty. No valid DISP_VAR generated.")
      }
    } else {
      DS_FINAL <- data.frame(SUBJID = character())  # Create an empty placeholder dataset
      message("⚠️ No valid datasets found in DS_LIST. Created empty DS_FINAL.")
    }
  }
  
  domain_cols <- colnames(AE6)[grepl("^DOMAIN", colnames(AE6))]
  
  DOB_1 <- DOB %>%
    mutate(DATEOFBIRTH_ = dmy(DATEOFBIRTH_))
  
  DM1001_1 <- DM1001 %>% 
    filter(!is.na(SEX)&SEX!="") %>% 
    select(SUBJID,SEX) %>% 
    distinct()
  
  SUBJECT_MEDDRA1 <- AE6 %>%
    left_join(DM1001_1, by = "SUBJID") %>%
    left_join(SITE_CNT_STAT, by = "SUBJID") %>%
    left_join(SBJCT_STS_NOX_2, by = "SUBJID") %>%
    left_join(DS_FINAL, by = "SUBJID") %>%
    left_join(CM1001, by = c("SUBJID", "LASTCHGDATE", "EVENT_ID" = "CMAEGRPID4")) %>%
    #left_join(DM1001, by = c("SUBJID", "LASTCHGDATE")) %>%
    left_join(DOB_1, by = "SUBJID") %>%
    left_join(first_vis, by = "SUBJID") %>%  # Add first_vis join
    mutate(
      LASTCHGDATE = as.Date(LASTCHGDATE, format = "%Y-%m-%d"),
      RANDOMZN_DATE = as.Date(RANDOMZN_DATE, format = "%Y-%m-%d"),
      DATEOFBIRTH_ = as.Date(DATEOFBIRTH_, format = "%Y-%m-%d"),
      EVENT_STARTDATE_ = as.Date(EVENT_STARTDATE_, format = "%Y-%m-%d"),
      EVENT_ENDAT_ = as.Date(EVENT_ENDAT_, format = "%Y-%m-%d"),
      VISDAT_FRT = as.Date(VISDAT_FRT, format = "%Y-%m-%d"),  # Convert first_vis date
      SITE = coalesce(SITE.x,SITE.y),
      # # New column: Visit_601or1_ToStartDate_days
      # Visit_601or1_ToStrtDate = case_when(
      #   DOMAIN == "MH" & !is.na(VISDAT_FRT) & !is.na(EVENT_STARTDATE_) ~ 
      #     as.integer(difftime(EVENT_STARTDATE_, VISDAT_FRT, units = "days")),
      #   TRUE ~ NA_integer_
      # ),
      
      Visit_601or1_ToStrtDate = case_when(
        DOMAIN %in% c("MH", "MHPRESP") & !is.na(VISDAT_FRT) & !is.na(EVENT_STARTDATE_) ~
          as.integer(VISDAT_FRT - EVENT_STARTDATE_),
        TRUE ~ NA_integer_
      ),
      
      SUBJAGE_RANDM = interval(DATEOFBIRTH_, RANDOMZN_DATE) %/% years(1),
      RANDMTOSTARTDATE_DAYS = as.integer(difftime(EVENT_STARTDATE_, RANDOMZN_DATE, units = "days")),
      RANDMTOENDDATE_DAYS = as.integer(difftime(EVENT_ENDAT_, RANDOMZN_DATE, units = "days")),
      VERBATIMTERM = toupper(EVENT_TERM),
      LLT = toupper(LLT),
      PT = toupper(PT),
      HLGT = toupper(HLGT)
    ) %>%
    select(
      LASTCHGDATE, DOMAIN, SITE, COUNTRY = SITECOUNTRY,
      SUBJID, SEX, SUBJAGE_RANDM, EVENT_ID, AESPID,
      EVENT_STARTDATE_, RANDOMZN_DATE, EVENT_ENDAT,EVENT_STARTDATE,
      RANDMTOSTARTDATE_DAYS, RANDMTOENDDATE_DAYS,
      EVENTDURATION_DAYS, EVENT_ONGO, EVENT_SER,
      SER_REASON, SOC, EVENT_SEV, TREATMENT_ACTION,
      EVENT_OUTCOME, REL_STUDYTREATMENT, REL_NONSTUDYTREATMENT,
      VERBATIMTERM, LLT, PT, HLGT, PT_MAPPED_NUMBER,
      PT_MAPPED_LIST, DISP_VARS, Visit_601or1_ToStrtDate,VISDAT_FRT
    ) %>% 
    distinct()
  

  SUBJECT_MEDDRA2 <- SUBJECT_MEDDRA1 %>%
    filter(nchar(VERBATIMTERM) > 1) %>%
    mutate(
      PT_MAPPING_ACCURACY = case_when(
        VERBATIMTERM == PT | VERBATIMTERM == LLT ~ "Perfect Match",
        phonetic(VERBATIMTERM) == phonetic(PT) | phonetic(VERBATIMTERM) == phonetic(LLT) ~ "Good Match",
        TRUE ~ "Need Review"
      )
    )
  # Renaming column in trigger dataset
  trigger <- TRIGGER %>% rename(MEDDRA_PT = `MedDRA Preferred Term (PT)`) 
  trigger <- trigger %>%
    mutate(across(where(is.character), toupper))
  
  
  SUBJECT_MEDDRA3 <- SUBJECT_MEDDRA2 %>%
    left_join(PROHIBITED_TERMS, by = c("DOMAIN" = "DOMAIN", "PT" = "PT_TERMS")) %>%
    #left_join(PROCEDURES_ENDDATE, by = c("PT" = "LIST")) %>%
    left_join(trigger, by = c("PT" = "MEDDRA_PT")) %>%
    mutate(
      POTENTIALPD_TERM = coalesce(PROHIBITED_TERM, "N"),
      PT_MAPPING_CONSISTENCY = ifelse(PT_MAPPED_NUMBER > 1, "Multiple MedDRAs", "OK"),
      PROC_EVENT_MISSINGENDDATE = ifelse(is.na(EVENT_ENDAT), "Missing EndDate", "OK"),
      ADJ_TRIGGER_TEMP = ifelse(!is.na(PT), Acronym, " ")
      )
  
  
  SUBJECT_MEDDRA3_A <- SUBJECT_MEDDRA3 %>%
    select(PT, ADJ_TRIGGER_TEMP) %>%
    filter(!is.na(ADJ_TRIGGER_TEMP)) %>%
    distinct() %>%
    arrange(PT, ADJ_TRIGGER_TEMP)
  
  # Creating SUBJECT_MEDDRA_TRIGGER equivalent in R
  SUBJECT_MEDDRA_TRIGGER <- SUBJECT_MEDDRA3_A %>%
    group_by(PT) %>%
    summarise(ADJ_TRIGGER = paste(unique(ADJ_TRIGGER_TEMP), collapse = " & "), .groups = "drop")
  
  # Creating Subject_MedDRA3A equivalent in R
  SUBJECT_MEDDRA3_A <- SUBJECT_MEDDRA3 %>%
    left_join(SUBJECT_MEDDRA_TRIGGER, by = "PT") %>%
    select(-ADJ_TRIGGER_TEMP)
  
  SUBJECT_MEDDRA4 <- SUBJECT_MEDDRA3_A %>%
    mutate(
      LLT = coalesce(LLT.x, LLT.y),
      string1 = gsub(" ", "", VERBATIMTERM),
      string2 = gsub(" ", "", PT),
      string3 = gsub(" ", "", VERBATIMTERM),
      string4 = gsub(" ", "", LLT),
      
      SCORE_PT = round(1 - stringdist::stringdist(string1, string2, method = "jw"),digits = 2),  # Jaro-Winkler similarity
      SCORE_LLT = round(1 - stringdist::stringdist(string3, string4, method = "jw"),digits = 2)   # Jaro-Winkler similarity
    ) %>%
    select(-string1, -string2, -string3, -string4)
  
  SUBJECT_AE_MH_1 <- SUBJECT_MEDDRA4 %>%
    select(
      PT_MAPPING_CONSISTENCY, PT_MAPPING_ACCURACY, POTENTIALPD_TERM, ADJ_TRIGGER,
      PROC_EVENT_MISSINGENDDATE, DOMAIN, SITE = SITE, COUNTRY, SUBJID, SEX,
      SUBJAGE_RANDM, EVENT_ID, AESPID, VERBATIMTERM, PT, LLT, PT_MAPPED_NUMBER,
      PT_MAPPED_LIST, SOC, RANDOMIZATION_DATE = RANDOMZN_DATE,
      EVENT_STARTDATE, EVENT_STARTDATE_, EVENT_ENDDATE = EVENT_ENDAT,
      RANDMTOSTARTDATE_DAYS, RANDMTOENDDATE_DAYS, EVENTDURATION_DAYS,
      EVENT_ONGO, EVENT_SER, SER_REASON, EVENT_SEV, TREATMENT_ACTION,
      EVENT_OUTCOME, REL_STUDYTREATMENT, REL_NONSTUDYTREATMENT,
      all_of(DISP_VARS), SCORE_PT, SCORE_LLT, LASTCHGDATE,Visit_601or1_ToStrtDate
    ) %>%
    arrange(SUBJID, EVENT_ID)
  
  LDD_R1 <- get(indsn6, envir = .GlobalEnv)
  LDD <- LDD_R1 %>%
    arrange(SUBJID, desc(ECSTDAT)) %>%
    group_by(SUBJID) %>%
    slice(1) %>%
    ungroup() %>%
    select(SUBJID, LAST_DOSE_DATE = ECSTDAT)
  
  
  IFD_R1 <- u_getdate(indsn7,"DSSTDAT_ICMO","DSSTDAT_ICDD","DSSTDAT_ICYY","IFC_DATE")
  
  IFD <- IFD_R1 %>%
    filter(!is.na(IFC_DATE)) %>%  # Remove missing ifc_date
    arrange(SUBJID, IFC_DATE) %>%  # Sort by subjid and ifc_date
    group_by(SUBJID) %>%  # Group by subjid
    slice(1) %>%  # Keep only the first occurrence
    ungroup() %>%  # Ungroup the data
    select(SUBJID, IFC_DATE) 
  
  SUBJECT_AE_MH_2 <- SUBJECT_AE_MH_1 %>%
    left_join(LDD, by = "SUBJID")
  
  SUBJECT_AE_MH_3 <- SUBJECT_AE_MH_2 %>%
    left_join(IFD, by = "SUBJID")
  
  SUBJECT_AE_MH_4 <- SUBJECT_AE_MH_3 %>%
    rename(PMN = PT_MAPPED_NUMBER, PML = PT_MAPPED_LIST) %>%
    mutate(
      PT_MAPPED_NUMBER = ifelse(is.na(PT), NA, PMN),
      PT_MAPPED_LIST = ifelse(is.na(PT), "", PML)
    ) %>%
    select(-PMN, -PML)
  
  SUBJECT_AE_MH_5 <- SUBJECT_AE_MH_4 %>%
    left_join(DSN_REL, by = c("SUBJID" = "SUBJID", "EVENT_ID" = "UNIQUE_ID","DOMAIN" = "DOMAIN_TO_LINK_TO")) %>%
    #filter(DOMAIN == DOMAIN_TO_LINK_TO) %>%
    rename(FORMS = SOURCES)
  
  
  SUBJECT_AE_MH_6 <- SUBJECT_AE_MH_5 %>%
    mutate(
      AESTEXRES2 = case_when(
        DOMAIN == "AE" & !is.na(EVENT_STARTDATE_) & !is.na(RANDOMIZATION_DATE) & EVENT_STARTDATE_ < RANDOMIZATION_DATE ~ "Y",
        DOMAIN == "AE" & !is.na(EVENT_STARTDATE_) & !is.na(RANDOMIZATION_DATE) & EVENT_STARTDATE_ >= RANDOMIZATION_DATE ~ "N",
        DOMAIN %in% c("MH", "MHPRESP") ~ "",  # Blank for these domains
        #DOMAIN == "AE" ~ "",  # Blank for AE if dates missing
        TRUE ~ NA_character_  # NA for all other domains
      ),
      AESTEXRES2 = if_else(DOMAIN == "AE", AESTEXRES2, "")
    )%>% 
    distinct()
  
  # Adjusting adj_trigger based on event_startdate_
  SUBJECT_AE_MH_7 <- SUBJECT_AE_MH_6 %>%
    mutate(ADJ_TRIGGER = ifelse(EVENT_STARTDATE_ > RANDOMIZATION_DATE, ADJ_TRIGGER, "")) %>%
    select(-EVENT_STARTDATE_)%>% 
    distinct()
  
  DS_SF <- get(indsn13) %>%
    filter(toupper(trimws(DSDECOD)) == "SCREEN FAILURE") %>%
    select(SUBJID, DSDECOD)
  
  INJSR1001_1 <- INJSR1001 %>% 
    filter(!is.na(AEGRPID_RELREC)) %>% 
    select(SUBJID,AEGRPID_RELREC) %>% 
    mutate(AEGRPID_RELREC = as.character(AEGRPID_RELREC)) %>% 
    distinct()
  
  # subjid_present <- any(SUBJECT_AE_MH_7$SUBJID %in% INJSR1001_1$SUBJID)
  # 
  # if (subjid_present) {
  #   # Create a lookup table for fast matching
  #   aegrpid_map <- INJSR1001_1 %>% select(SUBJID, AEGRPID_RELREC)
  #   
  #   SUBJECT_AE_MH_8 <- SUBJECT_AE_MH_7 %>%
  #     mutate(EVENT_ID = as.character(EVENT_ID)) %>% 
  #     left_join(aegrpid_map, by = "SUBJID") %>%
  #     mutate(
  #       AE_ISR_Linked = if_else(
  #         DOMAIN == "AE" & EVENT_ID == AEGRPID_RELREC,
  #         "Y",
  #         NA_character_
  #       )
  #     ) %>%
  #     select(-AEGRPID_RELREC) %>% 
  #     distinct()      # Remove join helper if you want
  # } else {
  #   SUBJECT_AE_MH_8 <- SUBJECT_AE_MH_7 %>%
  #     mutate(AE_ISR_Linked = NA_character_) %>% 
  #     distinct()
  # }
  #   
  subjid_present <- any(SUBJECT_AE_MH_7$SUBJID %in% INJSR1001_1$SUBJID)
  
  if (subjid_present) {
    # Create a lookup set for fast matching
    match_set <- INJSR1001_1 %>%
      mutate(AEGRPID_RELREC = as.character(AEGRPID_RELREC)) %>%
      transmute(key = paste(SUBJID, AEGRPID_RELREC, sep = "_")) %>%
      pull(key)
    
    SUBJECT_AE_MH_8 <- SUBJECT_AE_MH_7 %>%
      mutate(
        EVENT_ID = as.character(EVENT_ID),
        key = paste(SUBJID, EVENT_ID, sep = "_"),
        AE_ISR_Linked = if_else(DOMAIN == "AE" & key %in% match_set, "Y", NA_character_)
      ) %>%
      select(-key)
  } else {
    SUBJECT_AE_MH_8 <- SUBJECT_AE_MH_7 %>%
      mutate(AE_ISR_Linked = NA_character_)
  }
  
  
  
  # SUBJECT_AE_MH_8 <- SUBJECT_AE_MH_7 %>%
  #   left_join(INJSR1001_1, by = c("SUBJID" = "SUBJID", "EVENT_ID" = "AEGRPID_RELREC")) %>%
  #   distinct() %>% 
  #   mutate(
  #     AE_ISR_Linked = case_when(
  #       DOMAIN == "AE" & EVENT_ID == INJSR1001_1$AEGRPID_RELREC ~ "Y",
  #       TRUE ~ NA_character_
  #     )
  #   ) 
  
  SUBJECT_AE_MH_9 <- SUBJECT_AE_MH_8 %>%
    filter(!SUBJID %in% DS_SF$SUBJID) %>%
    distinct()
  
  SUBJECT_AE_MH <- SUBJECT_AE_MH_9 %>%
    filter(!is.na(RANDOMIZATION_DATE)) %>%
    arrange(SUBJID)
  
  assign("SUBJECT_AE_MH", SUBJECT_AE_MH, envir = .GlobalEnv)
  
  
}

d_Subject_AE_MH(strsplit(sources[2], ",")[[1]][1],strsplit(sources[2], ",")[[1]][2],strsplit(sources[2], ",")[[1]][3],strsplit(sources[2], ",")[[1]][4],strsplit(sources[2], ",")[[1]][5],
                strsplit(sources[2], ",")[[1]][6],strsplit(sources[2], ",")[[1]][7],strsplit(sources[2], ",")[[1]][8],strsplit(sources[2], ",")[[1]][9],strsplit(sources[2], ",")[[1]][10],
                strsplit(sources[2], ",")[[1]][11],strsplit(sources[2], ",")[[1]][12],strsplit(sources[2], ",")[[1]][13],strsplit(sources[2], ",")[[1]][14],strsplit(sources[2], ",")[[1]][15],strsplit(sources[2], ",")[[1]][16])


rm(AE3001_, AE1A, MH1, AE1B,  AE2, AE3, AE4, AE5, AE6, SUBJECT_MEDDRA1,
   SUBJECT_MEDDRA3A,
   SBJCT_DOB_STG_, DOB,  SUBJECT_MEDDRA2, SBJCT_STS_NOX_1, SUBJECT_MEDDRA_TRIGGER,
   SUBJECT_MEDDRA3, SUBJECT_MEDDRA4,   SUBJECT_MEDDRA3_A, DICT_MEDDRA_,
   DS, DS1, DS2,  SUBJECT_AE_MH_1, SUBJECT_AE_MH_2, SUBJECT_AE_MH_3,SUBJECT_AE_MH_5,SUBJECT_AE_MH_4,SUBJECT_AE_MH_5,SUBJECT_AE_MH_6,SUBJECT_AE_MH_7,SUBJECT_AE_MH_8,SUBJECT_AE_MH_9, LDD_R1, LDD, IFD_R1, IFD, AEX, AEY)

