
#Referenced Code Here.

d_Study_AE_MH <- function(indsn1, indsn2, indsn3, indsn4, indsn5, indsn6, include_mhpresp) {
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
  
  if (exists(indsn2, envir = .GlobalEnv)) {  
    indsn2 <- get(indsn2, envir = .GlobalEnv)
    #MH1 <- base::subset(base::get("mh701"), select = c("SUBJID", "GROUPID", "LLT", "EVENT_TERM", "PT", "LASTCHGDATE"))
    MH1 <- dplyr::mutate(indsn2,
                         EVENT_TERM = base::toupper(MHTERM),
                         PT = base::toupper(MHDECOD),
                         GROUPID = MHSPID,
                         LLT = base::toupper(MHLLT))
    MH1 <- base::subset(base::get("MH1"), select = c("SUBJID", "GROUPID", "LLT", "EVENT_TERM", "PT", "LASTCHGDATE"))
    MH1 <- dplyr::filter(MH1, EVENT_TERM != " ")
  }
  #INSTRUCTION-FOR-KEEPING-R-FUNCTION-OPEN: d_Study_AE_MH %mend is not present in this SAS chunk
  
  #END-CHUNK:1 UID:90fa6e47-3d19-4c77-8b9a-8d8851479467
  
  #START-CHUNK:2 UID:2e1b86f4-525e-421d-a3be-629f1d818abf
  
  #another block
  
  #Referenced Code Here.
  
  if (base::exists(indsn5)) {
    dsidz <- base::get(indsn5)
    nobsz <- base::nrow(dsidz)
    
    if (nobsz != 0) {
      var1 <- "SUBJID"
      var2 <- "MHSPID"
      var3 <- "MHLLTCDPRESP"
      var4 <- "MHTERM"
      
      for (i in 1:4) {
        u_checkds_var(indsn5, base::get(base::paste0("var", i)))
        if (chk == 0) {
          base::return(NULL)
        }
      }
      MHPRESP1 <- dsidz
    }
    
  }
  
  #MHPRESP1 <- dsidz
  colnames(MHPRESP1)
  #INSTRUCTION-FOR-KEEPING-R-FUNCTION-OPEN: d_Study_AE_MH %mend is not present in this SAS chunk
  #END-CHUNK:2 UID:2e1b86f4-525e-421d-a3be-629f1d818abf
  MHPRESP_R1 <- MHPRESP1 %>%
    dplyr::rename(MHSPID_ = MHSPID) %>%  # Rename MHSPID to MHSPID_
    dplyr::mutate(
      MHSPID = ifelse(is.character(MHSPID_), as.numeric(MHSPID_), MHSPID_)  # Convert if character
    ) %>%
    dplyr::filter(!is.na(MHSPID)) %>%  # Keep only non-missing MHSPID values
    dplyr::select(SUBJID, MHSPID, MHTERM, MHLLTCDPRESP, LASTCHGDATE)  # Keep only required columns
  
  assign("MHPRESP_R1", MHPRESP_R1, envir = .GlobalEnv) 
  
  
  #START-CHUNK:4 UID:25068bfe-c360-43d8-8b0b-e095c38942df
  
  if (base::exists(indsn6)) {
    indsn6 <- base::get(indsn6)
    MHPRESP <- MHPRESP_R1 %>%
      left_join(
        indsn6 %>%
          select(LLTCD, PFTERM, LLTERM) %>%
          mutate(LLTCD = as.character(LLTCD)),  # Convert LLTCD to character for proper matching
        by = c("MHLLTCDPRESP" = "LLTCD")
      ) %>%
      select(SUBJID, MHSPID, MHTERM, MHLLTCDPRESP, LASTCHGDATE, PFTERM, LLTERM)  %>%
      rename(MHDECOD = PFTERM, MHLLT = LLTERM) %>%  # Rename columns
      distinct()
  }
  # #Referenced Code Here.
  #   
  #   MHPRESP <- MHPRESP_R1 %>%
  #     dplyr::left_join(indsn6, by = c("MHLLTCDPRESP" = "LLTCD")) %>%
  #     dplyr::mutate(MHLLTCDPRESP = base::trimws(MHLLTCDPRESP),
  #                    LLTCD = base::as.character(base::format(LLTCD, scientific = FALSE))) %>%
  #     dplyr::mutate(LLTCD = base::trimws(LLTCD)) %>%
  #     dplyr::filter(MHLLTCDPRESP == LLTCD) %>%
  #     dplyr::select(SUBJID, MHSPID, MHTERM, MHLLTCDPRESP, LASTCHGDATE, PFTERM, LLTERM) %>%
  #     dplyr::distinct()
  #END-CHUNK:4 UID:25068bfe-c360-43d8-8b0b-e095c38942df
  
  #START-CHUNK:5 UID:4c77726c-8f0f-41cb-90fc-e0cc9d10697a
  
  #another block
  
  # Please pay extra attention to below logical statements as we had seen some logical discrepencies in SAS to R code conversions for WHERE statements. Please refer to the below path to see similar examples.
  MHPRESP <- MHPRESP %>%
    dplyr::select(-MHLLTCDPRESP) %>%
    dplyr::mutate(EVENT_TERM = base::toupper(MHTERM),
                  PT = base::toupper(MHDECOD),
                  GROUPID = MHSPID,
                  LLT = base::toupper(MHLLT)) %>%
    dplyr::filter(MHTERM != " ")
  #END-CHUNK:5 UID:4c77726c-8f0f-41cb-90fc-e0cc9d10697a
  
  #START-CHUNK:6 UID:45fbc6c2-e21b-49e6-a88a-da18f6f690a3
  
  #another block
  # Since the %ELSE part of the SAS macro corresponds to a situation where a condition is not met,
  # and the SAS code inside this block creates an empty dataset based on MH1 with zero observations,
  # the equivalent R code would create an empty dataframe with the same structure as MH1.
  
  #MHPRESP <- MH1[0, ]
  #END-CHUNK:6 UID:45fbc6c2-e21b-49e6-a88a-da18f6f690a3
  
  #START-CHUNK:7 UID:c7019f48-bc2f-4ee1-9949-0bf7bbd90abd
  
  #another block
  # Since the %ELSE part of the SAS macro corresponds to a situation where a condition is not met,
  # and the SAS code inside this block creates an empty dataset based on MH1 with zero observations,
  # the equivalent R code would create an empty dataframe with the same structure as MH1.
  
  #MHPRESP <- MH1[0, ]
  #END-CHUNK:7 UID:c7019f48-bc2f-4ee1-9949-0bf7bbd90abd
  
  #START-CHUNK:8 UID:91efc912-a0d4-4506-86ba-9615da0af0ff
  
  #another block
  # Please pay extra attention to below logical statements as we had seen some logical discrepencies in SAS to R code conversions for WHERE statements. Please refer to the below path to see similar examples.
  #AE1 <- base::subset(base::get("indsn1"), select = c("SUBJID", "GROUPID", "EVENT_TERM", "LLT", "PT", "LASTCHGDATE"))
  if (base::exists(indsn1)) {
    indsn1 <- base::get(indsn1)
    AE1 <- dplyr::mutate(indsn1,
                         EVENT_TERM = base::toupper(AETERM),
                         PT = base::toupper(AEDECOD),
                         GROUPID = AESPID,
                         LLT = base::toupper(AELLT))
    AE1 <- base::subset(base::get("AE1"), select = c("SUBJID", "GROUPID", "EVENT_TERM", "LLT", "PT", "LASTCHGDATE"))
    AE1 <- dplyr::filter(AE1, EVENT_TERM != " ")
    #END-CHUNK:8 UID:91efc912-a0d4-4506-86ba-9615da0af0ff
    
    #START-CHUNK:9 UID:1776ecee-9a1b-4b1b-b508-e7903b3ea7aa
    
    #another block
    AE1 <- dplyr::bind_rows(AE1, MH1)
    #END-CHUNK:9 UID:1776ecee-9a1b-4b1b-b508-e7903b3ea7aa
    
    #START-CHUNK:10 UID:2f6655ac-0480-49cc-9870-e3d12a5caa9f
    
    #another block
    if (base::toupper(base::trimws(include_mhpresp)) == "Y") {
      AE1 <- dplyr::bind_rows(AE1, MHPRESP)
    }
  }
  #END-CHUNK:10 UID:2f6655ac-0480-49cc-9870-e3d12a5caa9f
  
  #START-CHUNK:11 UID:ef3d6b5b-09f2-4c70-821a-b62597a7dff2
  
  # #another block
  # if (base::exists(indsn3)) {
  #   dsidz <- base::get(indsn3)
  #   nobsz <- base::nrow(dsidz)
  #   
  #   if (nobsz != 0) {
  #     Prohibited_AE <- dsidz
  #     Prohibited_AE$Prohibited_Term <- "Y"
  #   }
  # }
  # #END-CHUNK:11 UID:ef3d6b5b-09f2-4c70-821a-b62597a7dff2
  # 
  # #START-CHUNK:12 UID:9af73212-2ed9-4f7a-88a7-bb8a5ddb67f8
  # 
  # #another block
  # Prohibited_AE <- data.frame(PT = character(0), Prohibited_Term = character(0))
  # base::stop("Execution stopped")
  # #END-CHUNK:12 UID:9af73212-2ed9-4f7a-88a7-bb8a5ddb67f8
  # 
  # #START-CHUNK:13 UID:521cb5e9-efba-48e5-8ff7-012298db6c51
  # 
  # #another block
  # Prohibited_AE <- data.frame(PT = character(0), Prohibited_Term = character(0))
  # base::stop("Execution stopped")
  
  
  #END-CHUNK:13 UID:521cb5e9-efba-48e5-8ff7-012298db6c51
  if (exists(indsn3, envir = .GlobalEnv)) {
    
    # Retrieve dataset
    dataset <- get(indsn3, envir = .GlobalEnv)
    
    # Check if dataset has observations
    if (nrow(dataset) != 0) {
      
      # Create Prohibited_AE dataset
      PROHIBITED_AE <- dataset %>%
        mutate(PROHIBITED_TERM = "Y")  # Add new column
      
    } else {
      # If dataset is empty, create an empty Prohibited_AE dataset with required columns
      PROHIBITED_AE <- tibble(PT = character(), PROHIBITED_TERM = character())
    }
    
  } else {
    # If dataset does not exist, create an empty Prohibited_AE dataset
    PROHIBITED_AE <- tibble(PT = character(), PROHIBITED_TERM = character())
  }
  
  # Save Prohibited_AE in the global environment
  #START-CHUNK:14 UID:76c7e4c5-c321-4070-b652-6ad371b822db
  if (exists(indsn4, envir = .GlobalEnv)) {
    
    # Retrieve dataset
    dataset <- get(indsn4, envir = .GlobalEnv)
    
    # Check if dataset has observations
    if (nrow(dataset) != 0) {
      
      # Create Prohibited_AE dataset
      PROHIBITED_MH <- dataset %>%
        mutate(PROHIBITED_TERM = "Y")  # Add new column
      
    } else {
      # If dataset is empty, create an empty Prohibited_AE dataset with required columns
      PROHIBITED_MH <- tibble(PT = character(), PROHIBITED_TERM = character())
    }
    
  } else {
    # If dataset does not exist, create an empty Prohibited_AE dataset
    PROHIBITED_MH <- tibble(PT = character(), PROHIBITED_TERM = character())
  }
  
  #another block
  # if (base::exists(indsn4)) {
  #   dsidz <- base::get(indsn4)
  #   nobsz <- base::nrow(dsidz)
  #   
  #   if (nobsz != 0) {
  #     Prohibited_MH <- dsidz
  #     Prohibited_MH$Prohibited_Term <- "Y"
  #   }
  # }
  # #END-CHUNK:14 UID:76c7e4c5-c321-4070-b652-6ad371b822db
  # 
  # #START-CHUNK:15 UID:0ce4e4cf-5ea6-4820-a129-eca3becfa618
  # 
  # #another block
  # Prohibited_MH <- data.frame(PT = character(0), Prohibited_Term = character(0))
  # base::stop("Execution stopped")
  # #END-CHUNK:15 UID:0ce4e4cf-5ea6-4820-a129-eca3becfa618
  # 
  # #START-CHUNK:16 UID:1594b6a8-37af-4668-b1fa-6f47d76d95ca
  # 
  # #another block
  # Prohibited_MH <- data.frame(PT = character(0), Prohibited_Term = character(0))
  # base::stop("Execution stopped")
  # #END-CHUNK:16 UID:1594b6a8-37af-4668-b1fa-6f47d76d95ca
  
  #START-CHUNK:17 UID:75b1c351-b45a-4898-ac52-59a38528bec0
  
  #another block
  PROHIBITED_TERMS <- dplyr::bind_rows(PROHIBITED_AE, PROHIBITED_MH) %>%
    dplyr::mutate(PT_TERMS = base::toupper(PT)) %>%
    dplyr::select(-PT)
  #END-CHUNK:17 UID:75b1c351-b45a-4898-ac52-59a38528bec0
  
  #START-CHUNK:18 UID:a9c2b1f1-71c9-4286-aea0-00449f42aae2
  
  #another block
  AE1A <- AE1 %>%
    dplyr::mutate(key_local = base::paste(base::toupper(PT), base::toupper(LLT), sep = "/")) %>%
    dplyr::mutate(key_local = as.character(key_local))
  #END-CHUNK:18 UID:a9c2b1f1-71c9-4286-aea0-00449f42aae2
  
  #START-CHUNK:19 UID:d66af9ef-f5a4-4492-a6d3-ae2a88a8328d
  
  #another block
  STUDY_MEDDRA1 <- AE1A %>%
    dplyr::group_by(key_local) %>%
    dplyr::summarise(
      LASTCHGDATE = max(LASTCHGDATE),
      EVENT_TERM = base::toupper(EVENT_TERM),
      LLT = base::toupper(LLT),
      PT = base::toupper(PT),
      UNIQUE_SUBJECT = dplyr::n_distinct(SUBJID),
      UNIQUE_VERBATIMTERM = dplyr::n_distinct(base::trimws(base::toupper(EVENT_TERM)))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  #END-CHUNK:19 UID:d66af9ef-f5a4-4492-a6d3-ae2a88a8328d
  
  #START-CHUNK:20 UID:e95d2aaf-09a1-4468-8953-3e2774e2bd40
  
  #another block
  STUDY_MEDDRA1A <- STUDY_MEDDRA1 %>%
    dplyr::arrange(key_local, EVENT_TERM) %>%
    dplyr::distinct(key_local, EVENT_TERM, .keep_all = TRUE)
  #END-CHUNK:20 UID:e95d2aaf-09a1-4468-8953-3e2774e2bd40
  
  
  STUDY_MEDDRA2 <- STUDY_MEDDRA1A %>%
    arrange(key_local, EVENT_TERM) %>%  # Sort by key_local and EVENT_TERM
    group_by(key_local) %>%
    mutate(
      VERBATIMTERM_LIST = accumulate(EVENT_TERM, ~paste(.x, .y, sep = " // "))  # Accumulate values progressively
    ) %>%
    slice_tail(n = 1) %>%  # Keep only the last occurrence (like "if last.key_local then output;")
    select(-EVENT_TERM) %>%  # Drop EVENT_TERM column
    ungroup()
  
  
  AE1A_SUBJIDLIST <- AE1A %>%
    dplyr::arrange(key_local, SUBJID) %>%
    dplyr::distinct(key_local, SUBJID, .keep_all = TRUE)
  #END-CHUNK:22 UID:d70dbe70-2576-49c6-83df-80a4aef54650
  
  #START-CHUNK:23 UID:01dc9503-6bb5-4242-ae61-57ae2bdb9af1
  
  #another block
  
  # Please pay extra attention to below logical statements as we had seen some logical discrepencies in SAS to R code conversions for WHERE statements. Please refer to the below path to see similar examples.
  STUDY_MEDDRA3 <- AE1A_SUBJIDLIST %>%
    dplyr::select(SUBJID, PT, LLT, key_local) %>%
    dplyr::filter(!is.na(PT))
  #END-CHUNK:23 UID:01dc9503-6bb5-4242-ae61-57ae2bdb9af1
  
  #START-CHUNK:24 UID:5f03c8b0-46f6-4899-8501-1420eacfee18
  
  #another block
  # STUDY_MEDDRA4 <- STUDY_MEDDRA3 %>%
  #   dplyr::group_by(key_local) %>%
  #   dplyr::mutate(Subject_List = ifelse(dplyr::row_number() == 1, SUBJID, base::paste(Subject_List, SUBJID, sep = " // "))) %>%
  #   dplyr::slice_tail(n = 1) %>%
  #   dplyr::select(-SUBJID) %>%
  #   dplyr::ungroup()
  #END-CHUNK:24 UID:5f03c8b0-46f6-4899-8501-1420eacfee18
  STUDY_MEDDRA4 <- STUDY_MEDDRA3 %>%
    arrange(key_local, SUBJID) %>%  # Sort by key_local and SUBJID
    group_by(key_local) %>%
    mutate(
      SUBJECT_LIST = accumulate(SUBJID, ~paste(.x, .y, sep = " // "))  # Accumulate Subject IDs progressively
    ) %>%
    slice_tail(n = 1) %>%  # Keep only the last occurrence per group
    select(-SUBJID) # Drop original SUBJID column
  
  
  # #another block
  STUDY_AE_MH <- STUDY_MEDDRA2 %>%
    dplyr::left_join(PROHIBITED_TERMS, by = c("PT" = "PT_TERMS")) %>%
    dplyr::left_join(STUDY_MEDDRA4, by = "key_local") %>%
    dplyr::mutate(POTENTIALPD_TERM = dplyr::coalesce(PROHIBITED_TERM, "N")) %>%
    dplyr::select(PT = PT.x, LLT, UNIQUE_SUBJECT, UNIQUE_VERBATIMTERM, SUBJECT_LIST, VERBATIMTERM_LIST, LASTCHGDATE, POTENTIALPD_TERM) %>%
    dplyr::distinct()
  
  assign("STUDY_AE_MH",STUDY_AE_MH, envir = .GlobalEnv)
  
  rm(AE1A, STUDY_MEDDRA1, STUDY_MEDDRA2, STUDY_MEDDRA4, STUDY_MEDDRA3, STUDY_MEDDRA1A, AE1A_SUBJIDLIST, AE1, MH1)
  
} #INSTRUCTION-FOR-CLOSING-R-FUNCTION: d_Study_AE_MH The %mend statement appeared, closing the open R function.
#END-CHUNK:28 UID:8b9adcf9-a7f5-4d15-8c4b-20e906ea4334

#START-CHUNK:29 UID:3bd81182-667d-435f-8476-5b22d5b4bd76
d_Study_AE_MH(strsplit(sources[1], ",")[[1]][1], strsplit(sources[1], ",")[[1]][2], strsplit(sources[1], ",")[[1]][3], strsplit(sources[1], ",")[[1]][4], strsplit(sources[1], ",")[[1]][5], strsplit(sources[1], ",")[[1]][6], strsplit(sources[1], ",")[[1]][7])
#another block



#"C:\SAS2R\MRL\d_listings.xlsx"
# The SAS macro call is commented out, so the equivalent R code would also be commented out.
# d_Study_AE_MH("AE3001", "MH7001", "PROHIBITED_AE", "PROHIBITED_MH", "MHPRESP1001", "DICT_MEDDRA", "Y")
#END-CHUNK:29 UID:3bd81182-667d-435f-8476-5b22d5b4bd76