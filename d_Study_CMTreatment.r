
d_study_CMTreatment <- function(indsn1, indsn2, indsn3, indsn4, indsn5) {
  
  # Process CM1 dataset
  CM1_1 <- get(indsn1) %>%
    filter(CMDECOD != "", CMROUTE != "") %>%
    mutate(ATC = str_c(toupper(CMCLASCD), toupper(CMCLAS), sep = " : "))
  
  # Process other datasets with existence checks
  process_cm_dataset <- function(ds_name, filter_expr = expr(CMDECOD != "")) {
    if(!is.null(get0(ds_name))) {
      get(ds_name) %>%
        filter(!!filter_expr) %>%
        mutate(ATC = str_c(toupper(CMCLASCD), toupper(CMCLAS), sep = " : "))
    } else {
      CM1_1[0,] # Empty tibble with same structure
    }
  }
  
  # Process remaining datasets
  CM2_1 <- process_cm_dataset(indsn2)
  CM3_1 <- process_cm_dataset(indsn3)
  CM4_1 <- process_cm_dataset(indsn5)
  
  CM1_1 <- CM1_1 %>% mutate(CMSPID = as.character(CMSPID))
  CM2_1 <- CM2_1 %>% mutate(CMSPID = as.character(CMSPID))
  CM3_1 <- CM3_1 %>% mutate(CMSPID = as.character(CMSPID))
  CM4_1 <- CM4_1 %>% mutate(CMSPID = as.character(CMSPID))
  
  # Combine all CM data
  combined_cm <- bind_rows(
    CM1_1, CM2_1, CM3_1, CM4_1
  ) %>%
    distinct() %>%
    mutate(
      KEY1 = str_c(FORM, CMDECOD, CMCLASCD, CMROUTE, sep = "/"),
      KEY2 = str_c(FORM, CMDECOD, CMROUTE, sep = "/")
    )
  
  # Create study_CM1
  study_CM1 <- combined_cm %>%
    # Clean text fields BEFORE grouping
    mutate(
      CMTRT_CLEAN = toupper(str_trim(CMTRT)),
      CMDECOD_CLEAN = toupper(str_trim(CMDECOD)),
      CMINDC_CLEAN = toupper(str_trim(CMINDC)),
      CMROUTE_CLEAN = toupper(str_trim(CMROUTE))
    ) %>%
    group_by(KEY1) %>%
    summarise(
      KEY2 = first(KEY2),
      LASTCHGDATE = max(LASTCHGDATE, na.rm = TRUE),
      ATC = first(ATC),
      CMCLAS = first(CMCLAS),
      CMCLASCD = first(CMCLASCD),
      CMROUTE = first(CMROUTE_CLEAN),
      CMTRT = first(CMTRT_CLEAN),  # Use cleaned value for display
      CMDECOD = first(CMDECOD_CLEAN),
      CMINDC = first(CMINDC_CLEAN),
      UNIQUE_SUBJECT = n_distinct(SUBJID),
      UNIQUE_VERBATIMTERM = n_distinct(CMTRT_CLEAN),  # Count cleaned values
      .groups = "drop"
    ) %>%
    # Handle cases where all CMTRT values were NA/blank
    mutate(UNIQUE_VERBATIMTERM = ifelse(UNIQUE_VERBATIMTERM == 0, 1, UNIQUE_VERBATIMTERM))
  
  
  # Create term lists
  study_CM3 <- combined_cm %>%
    group_by(KEY1) %>%
    summarise(VERBATIMTERM_LIST = toupper(str_c(unique(CMTRT), collapse = " // ")))
  
  # Create subject lists
  study_CM4 <- combined_cm %>%
    distinct(KEY1, SUBJID) %>%
    group_by(KEY1) %>%
    summarise(SUBJECT_LIST = str_c(unique(SUBJID), collapse = " // "))
  
  # Create ATC mappings
  study_CM9 <- combined_cm %>%
    group_by(KEY2) %>%
    summarise(
      UNIQUE_MAPPEDATC = n_distinct(CMCLASCD),
      MAPPEDATC_LIST = str_c(unique(ATC), collapse = " // "),
      .groups = "drop"
    )
  
  # Prohibited terms processing
  process_prohibited <- function(ds) {
    if(!is.null(get0(ds))) {
      get(ds) %>%
        mutate(
          DOMAIN = "CM",
          PROHIBITED_TERM = "Y",
          across(where(is.character), toupper)
        )
    } else {
      tibble(
        ATC4_CODE = character(),
        PROHIBITED_TERM = character(),
        PROHIB_MED = character()
      )
    }
  }
  
  PROHIBITED_ATC <- process_prohibited(indsn4)
  PROHIBITED_CMDECOD <- process_prohibited(indsn4)
  
  
  # Final assembly
  final_output <- study_CM1 %>%
    left_join(study_CM3, by = "KEY1") %>%
    left_join(study_CM4, by = "KEY1") %>%
    left_join(study_CM9, by = "KEY2") %>%
    left_join(ATC_REF_DATA, by = c("CMDECOD" = "DRUG_PREF_TERM")) %>%  # Fix this line
    mutate(
      POTENTIALPROHIBITED_TERM = coalesce(
        if_else(CMCLASCD %in% PROHIBITED_ATC$ATC4_CODE, "Y", NA_character_),
        if_else(CMDECOD %in% PROHIBITED_CMDECOD$PROHIB_MED, "Y", NA_character_),
        "N"
      )
    ) %>%
    arrange(
      desc(POTENTIALPROHIBITED_TERM), CMDECOD, CMCLASCD, ATC, CMROUTE,
      desc(LASTCHGDATE)
    ) %>%
    distinct(
      POTENTIALPROHIBITED_TERM, CM_PT = CMDECOD, CMCLASCD, ATC, CMROUTE,
      UNIQUE_SUBJECT, UNIQUE_VERBATIMTERM, SUBJECT_LIST, VERBATIMTERM_LIST,
      UNIQUE_MAPPEDATC, MAPPEDATC_LIST, AVAILABLEATC_MAP = RefATC_Map,
      AVAILABLEATC_LIST = RefATC_List, LASTCHGDATE
    )
  
  final_output <- final_output %>%
    mutate(
      ATC = if_else(str_trim(ATC) == ":", " ", ATC)
    )

  assign("STUDY_CMTREATMENT", final_output, envir = .GlobalEnv)
  
  cat("\n✅ Final STUDY_CMTREATMENT dataset updated with", nrow(STUDY_CMTREATMENT), "rows and", ncol(STUDY_CMTREATMENT), "columns.\n") 
  
}


d_study_CMTreatment(strsplit(sources[3], ",")[[1]][1], strsplit(sources[3], ",")[[1]][2], strsplit(sources[3], ",")[[1]][3], strsplit(sources[3], ",")[[1]][4], strsplit(sources[3], ",")[[1]][5])


rm(CM1, CM2, CM3, PROHIBITED_ATC, PROHIBITED_CMDECOD, 
   STUDY_CM1, STUDY_CM2,  STUDY_CM3, STUDY_CM6, STUDY_CM7, 
   STUDY_CM9, CM1A,  STUDY_CM4, STUDY_CM1_ATC, STUDY_CM1_ATC_,
   STUDY_CMTreatment_, STUDY_CM1_SUBJIDLIST, CM3_1, CM2_1, CM1_1, check_ds_con_1,
   check_ds_con_2, check_ds_con_3, check_ds_con_4, check_ds_con_5, CM4_1, CM4,final_output)
