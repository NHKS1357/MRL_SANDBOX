
d_INJSR <- function(INJSR1001, AE3001, RTSM_SBJCT_DATA, DS2001, formeid_list) {
  # Ensure all inputs are data.tables
  for (dt in list(INJSR1001, AE3001, RTSM_SBJCT_DATA, DS2001)) setDT(dt)
  formeid_list <- 'INJSR1001_LV1'
  # Fill missing required variables with NA if needed
  required_vars <- c("ROW_ILB", "FORMEID", "IISRYN")
  for (v in required_vars) if (!v %in% names(INJSR1001)) INJSR1001[, (v) := NA_character_]
  
  # Remove duplicate columns before merge to avoid .x/.y suffixes
  duplicate_cols <- intersect(names(INJSR1001), names(AE3001))
  duplicate_cols <- setdiff(duplicate_cols, "SUBJID")
  keep_cols <- setdiff(names(AE3001), duplicate_cols)
  AE3001_stripped <- AE3001[, ..keep_cols]
  
  # Step 1: Filter INJSR forms with IISRYN == "Y"
  # INJSR_1 <- merge(INJSR1001, AE3001_stripped, by = c("SUBJID", "AEGRPID_RELREC" = "AEGRPID"), all.x = TRUE)
  INJSR_1 <- left_join(INJSR1001, AE3001_stripped, by = c("SUBJID", "AEGRPID_RELREC" = "AEGRPID"))
  # injsr %>% left_join(ae,by  = c("aegrpid_relrec" = "aegrpid", "subjid")
  INJSR_1 <- INJSR_1[ROW_ILB != "Y" & grepl(paste(formeid_list, collapse = "|"), FORMEID, ignore.case = TRUE) & IISRYN == "Y"]
  INJSR_1 <- unique(INJSR_1)
  
  # Step 2: Extract Randomization Dates
  RTSM_SBJCT_DATA[, RANDOMZN_DATE := suppressWarnings(dmy(RNDDTTXT))]
  SBJCT_STS_NOX_1 <- RTSM_SBJCT_DATA[!is.na(RANDOMZN_DATE), .(SUBJID, RANDOMZN_DATE)]
  
  # Step 3: Create IFC date if needed from DS2001
  if (!"ifc_date" %in% names(DS2001)) {
    DS2001[, ifc_date := suppressWarnings(as.IDate(paste(DSSTDAT_ICYY, DSSTDAT_ICMO, DSSTDAT_ICDD, sep = "-"), "%Y-%m-%d"))]
  }
  
  
  ifd <- DS2001[!is.na(ifc_date), .(SUBJID, ifc_date)]
  setorder(ifd, SUBJID, ifc_date)
  ifd <- ifd[!duplicated(SUBJID)]
  
  # Step 4: Merge with Randomization Dates
  # INJSR_2 <- merge(INJSR_1, SBJCT_STS_NOX_1, by = "SUBJID")
  INJSR_2 <- inner_join(INJSR_1, SBJCT_STS_NOX_1, by = "SUBJID")
  INJSR_2 <- unique(INJSR_2)
  
  # Step 5: Merge with IFC dates
  INJSR <- left_join(INJSR_2, ifd, by = c("SUBJID"))
  INJSR <- unique(INJSR)
  INJSR$AEGRPID <- INJSR[, AEGRPID_RELREC] 
  
  # Step 6: Final sort
  setorder(INJSR, SUBJID)
  INJSR <- rename(INJSR,RANDOMIZATION_DATE = RANDOMZN_DATE) 
  INJSR <- rename_with(INJSR, str_to_upper)
  return(INJSR)
}

# Example usage:
INJSR <- d_INJSR(INJSR1001, AE3001, RTSM_SBJCT_DATA, DS2001, formeid_list = c("INJSR1001_LV1"))
