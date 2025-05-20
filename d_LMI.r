d_LMI <- function(LMI1001, RTSM_SBJCT_DATA, DS2001, formeid_list = "LMI1001_LV1") {
  # Ensure data.tables
  setDT(LMI1001)
  setDT(RTSM_SBJCT_DATA)
  setDT(DS2001)
  
  # CHUNK 1: Filter for valid LMI rows (not interim, matching formeid, and LMIPERF not missing)
  if (!"LMIPERF" %in% names(LMI1001)) LMI1001[, LMIPERF := NA_character_]
  if (!"FORMEID" %in% names(LMI1001)) LMI1001[, FORMEID := NA_character_]
  if (!"ROW_ILB" %in% names(LMI1001)) LMI1001[, ROW_ILB := NA_character_]
  
  LMI_1 <- LMI1001[ROW_ILB != "Y" & grepl(formeid_list, FORMEID, ignore.case = TRUE) & !is.na(LMIPERF)]
  
  # CHUNK 2: Get randomization date from RTSM_SBJCT_DATA
  RTSM_SBJCT_DATA[, RANDOMIZATION_DATE := suppressWarnings(as.Date(RNDDTTXT, format = "%d%b%Y"))]
  SBJCT_STS_NOX_1 <- RTSM_SBJCT_DATA[!is.na(RANDOMIZATION_DATE), .(SUBJID, RANDOMIZATION_DATE)]
  SBJCT_STS_NOX_1 <- unique(SBJCT_STS_NOX_1, by = "SUBJID")
  
  # CHUNK 3: Create IFC date from DS2001 (construct from MO/DD/YYYY parts if needed)
  DS2001 <- copy(DS2001)
  for (col in c("DSSTDAT_ICMO", "DSSTDAT_ICDD", "DSSTDAT_ICYY")) {
    if (!col %in% names(DS2001)) DS2001[, (col) := NA_character_]
  }
  DS2001[, IFC_DATE := suppressWarnings(as.Date(paste(DSSTDAT_ICYY, DSSTDAT_ICMO, DSSTDAT_ICDD, sep = "-"), "%Y-%m-%d"))]
  ifd <- DS2001[!is.na(IFC_DATE), .(SUBJID, IFC_DATE)]
  setorder(ifd, SUBJID, IFC_DATE)
  ifd <- ifd[!duplicated(SUBJID)]
  
  # CHUNK 4: Join LMI_1 with randomization date
  LMI_2 <- merge(LMI_1, SBJCT_STS_NOX_1, by = "SUBJID", allow.cartesian = TRUE)
  
  # CHUNK 5: Add IFC date (left join)
  LMI <- merge(LMI_2, ifd, by = "SUBJID", all.x = TRUE)
  
  return(LMI)
}

# Example usage:
LMI <- d_LMI(LMI1001, RTSM_SBJCT_DATA, DS2001, formeid_list = "LMI1001_LV1")