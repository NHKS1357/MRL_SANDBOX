

indsn1 <- strsplit(sources[5], ",")[[1]][1]
indsn2 <- strsplit(sources[5], ",")[[1]][2]
indsn3 <- strsplit(sources[5], ",")[[1]][3]
indsn4 <- strsplit(sources[5], ",")[[1]][4]
indsn5 <- strsplit(sources[5], ",")[[1]][5]
indsn6 <- strsplit(sources[5], ",")[[1]][6]
visit_number <- as.integer(strsplit(sources[5], ",")[[1]][7])
formeid_list <- strsplit(sources[5], ",")[[1]][8]

var1 <- "DSDECOD"
for(i in 1:1) {
  u_checkds_var(indsn1, base::get(base::paste0("var", i)))
  if (chk == 0) {
    stop("Variable is missing!")
  }
}


var1 <- "SUBJID"
var2 <- "AETERM"
var3 <- "AEDECOD"
var4 <- "AEGRPID"
var5 <- "AELLT"

for(i in 1:5) {
  u_checkds_var(indsn2, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}

var1 <- "DSSTDAT_IC"

for(i in 1:1) {
  u_checkds_var(indsn3, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}


var1 <- "RNDDTTXT"
for(i in 1:1) {
  u_checkds_var(indsn4, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}

var1 <- "VISDAT"

for(i in 1:1) {
  u_checkds_var(indsn6, base::get(base::paste0("var", i)))
  if (chk == 0) {
    base::return(NULL)
  }
}

var1 <- "ECSTDAT"
chk <- 1  # Initialize chk to a valid value
for (i in 1:1) {
  u_checkds_var(indsn5, base::get(base::paste0("var", i)))  # Call function
  
  if (chk == 0) {  # If a variable is missing, exit
    next  
  }
}

EC1001 <- u_getcomponents(EC1001, "ECSTDAT")

LD_R1 <- get(indsn5) %>%
  select(SUBJID, VISITNUM, ECSTDAT, ECSTDATMO, ECSTDATDD, ECSTDATYY) %>%
  filter(!is.na(ECSTDAT)) %>%
  distinct()


LD_R1 <- u_getdate("LD_R1","ECSTDATMO","ECSTDATDD","ECSTDATYY","LAST_DOSE_DATE")

LD_FINAL <- LD_R1 %>%
  arrange(SUBJID, VISITNUM, LAST_DOSE_DATE) %>%
  group_by(SUBJID,VISITNUM, LAST_DOSE_DATE) %>%
  filter(!is.na(LAST_DOSE_DATE)) %>% 
  slice_tail(n = 1) %>%
  ungroup()


if (indsn1 == "DS1001") {
  DS_R1 <- DS1001 %>%
    mutate(
      DISP_SCR      = if_else(str_trim(toupper(FORMEID)) == "DS1001_LF4", DSDECOD, NA_character_),
      DISP_TRT      = if_else(str_trim(toupper(FORMEID)) == "DS1001_LF5", DSDECOD, NA_character_),
      DISP_TRT_PHASE= if_else(str_trim(toupper(FORMEID)) == "DS1001_LF6", DSDECOD, NA_character_),
      DISP_FU_PHASE = if_else(str_trim(toupper(FORMEID)) == "DS1001_LF7", DSDECOD, NA_character_)
    )
}

if (indsn1 == "DS6001") {
  DS_R1 <- DS6001 %>%
    mutate(
      DS_SCR      = if_else(str_trim(toupper(FORMEID)) == "DS6001_LV4", DSDECOD, NA_character_),
      DS_TRT      = if_else(str_trim(toupper(FORMEID)) == "DS6001_LV5", DSDECOD, NA_character_),
      DS_TRT_PHASE= if_else(str_trim(toupper(FORMEID)) == "DS6001_LV6", DSDECOD, NA_character_),
      DS_FU_PHASE = if_else(str_trim(toupper(FORMEID)) == "DS6001_LV7", DSDECOD, NA_character_)
    )
}

DS_R1 <- u_getcomponents(DS_R1, "DSSTDAT")
DS_R1 <- u_getdate("DS_R1" , "DSSTDATMO","DSSTDATDD","DSSTDATYY","DISPOSITION_DATE")

DS_FINAL <- DS_R1 %>%
  filter(
    str_detect(FORMEID, formeid_list) & 
      (!is.na(DSDECOD) | !is.na(DSSTDAT))
  ) %>%
  select(
    SUBJID, FORMEID, VISITNUM, DSTERM, DSDCDSCAT_SUBJECT, DSDECOD, SITE, AEGRPID_RELREC,
    DTHDAT, LASTCHGDATE, DS_SCR, DS_TRT_PHASE, DS_FU_PHASE, DS_TRT, DISPOSITION_DATE
  )

# 1. Copy indsn2 to AE_R1
AE_R1 <- get(indsn2)

# 2. Filter indsn3 to create IC_R1 (where DSSTDAT_IC is not missing)
IC_R1_1 <- get(indsn3) %>%
  filter(!is.na(DSSTDAT_IC)& DSSTDAT_IC != "" & FORMEID == "DS2001_C1LV1") 

IC_R1_2 <- u_getdate("IC_R1_1" , "DSSTDAT_ICMO","DSSTDAT_ICDD","DSSTDAT_ICYY","ICF_DATE")

IC_R1 <- IC_R1_2 %>%
  arrange(SUBJID, ICF_DATE) %>%
  group_by(SUBJID, ICF_DATE) %>%
  slice_head(n = 1) %>%
  ungroup()

# 3. Keep only specific columns from &indsn6.
SV_R1 <- get(indsn6) %>%
  select(SUBJID, VISITNUM, VISDAT)

RND_DATE <- get(indsn4) %>%
  filter(!is.na(RNDDTTXT)& RNDDTTXT!="") %>%  # Only non-missing RNDDTTXT
  mutate(
    RANDOMZN_DATE = as.Date(RNDDTTXT, format = "%d%b%Y") # SAS DATE9. corresponds to e.g. "22APR2024"
  ) %>%
  select(SUBJID, RANDOMZN_DATE) %>% 
  distinct()

DS_IC <- DS_FINAL %>%
  left_join(IC_R1 %>% select(SUBJID, DSSTDAT_IC), by = "SUBJID") %>%
  distinct()

DS_IC_RND <- DS_IC %>%
  left_join(RND_DATE %>% select(SUBJID, RANDOMZN_DATE), by = "SUBJID") %>%
  distinct()

DS_IC_RND_LD <- DS_IC_RND %>%
  left_join(LD_FINAL %>% select(SUBJID, LAST_DOSE_DATE), by = "SUBJID") %>%
  distinct()%>%
  mutate(across(where(is.Date), as.character))

# 5. DS_IC_RND_LD_AE: join DS_IC_RND_LD + columns from AE_R1 on subjid, AEGRPID_RELREC/aegrpid

DS_IC_RND_LD_AE <- DS_IC_RND_LD %>%
  left_join(
    AE_R1 %>%
      select(SUBJID, AEGRPID, AETERM, AESTDAT, AEENDAT, AESER, AEACN),
    by = c("SUBJID" = "SUBJID", "AEGRPID_RELREC" = "AEGRPID")
  ) %>%
  distinct()

# 6. DS_IC_RND_LD_AE_SV: join DS_IC_RND_LD_AE + VISDAT from SV_R1 on subjid, visitnum
DS_IC_RND_LD_AE_SV <- DS_IC_RND_LD_AE %>%
  left_join(
    SV_R1 %>% select(SUBJID, VISITNUM, VISDAT),
    by = c("SUBJID" = "SUBJID", "VISITNUM" = "VISITNUM")
  ) %>%
  distinct()

DISPOSITION_R1 <- DS_IC_RND_LD_AE_SV %>%
  mutate(
    # Apply each condition; later condition will overwrite previous if more than one is true
    QUERY_TEXT = case_when(
      toupper(str_trim(DSDECOD)) == "SCREEN FAILURE" & !is.na(RANDOMZN_DATE) ~
        "SUBJECT IS SF BUT RANDOMISATION DATE ENTERED",
      toupper(str_trim(DSDECOD)) == "WITHDRAWAL BY SUBJECT" & is.na(DSDCDSCAT_SUBJECT)~
        "WITHDRAWAL BY SUBJECT BUT SPECIFY MISSING",
      toupper(str_trim(DSDECOD)) == "WITHDRAWAL BY SUBJECT" & DSDCDSCAT_SUBJECT == ""~
        "WITHDRAWAL BY SUBJECT BUT SPECIFY MISSING",
      toupper(str_trim(DSDECOD)) == "COMPLETED" &
        (VISITNUM != visit_number & toupper(str_trim(VISITNUM)) != "DS") ~
        sprintf("STATUS IS COMPLETED BUT VISITNUM IS NOT %s", visit_number),
      TRUE ~ NA_character_
    )
  ) %>%
  select(-DSDECOD)

# Define the mapping: For each FORMEID, what goes where
form_map <- list(
  DS6001_LV4 = list(
    DS_SCR_VISIT      = "VISITNUM",
    DS_SCR_VISIT_DOV  = "VISDAT",
    DS_SCR_DISP_DATE  = "DISPOSITION_DATE",
    LAST_DOSE         = "LAST_DOSE_DATE",
    DS_SCR_DISCON_REAS= "DS_SCR",
    DS_SCR_DSTERM     = "DSTERM",
    DS_SCR_AEGRPID    = "AEGRPID_RELREC",
    DS_SCR_AEACN      = "AEACN",
    DS_SCR_AETERM     = "AETERM",
    DS_SCR_AESTDAT    = "AESTDAT",
    DS_SCR_AEENDAT    = "AEENDAT",
    DS_SCR_AESER      = "AESER"
  ),
  DS6001_LV5 = list(
    DS_TRT_VISIT      = "VISITNUM",
    DS_TRT_VISIT_DOV  = "VISDAT",
    DS_TRT_DISP_DATE  = "DISPOSITION_DATE",
    LAST_DOSE         = "LAST_DOSE_DATE",
    DS_TRT_DISCON_REAS= "DS_TRT",
    DS_TRT_DSTERM     = "DSTERM",
    DS_TRT_AEGRPID    = "AEGRPID_RELREC",
    DS_TRT_AEACN      = "AEACN",
    DS_TRT_AETERM     = "AETERM",
    DS_TRT_AESTDAT    = "AESTDAT",
    DS_TRT_AEENDAT    = "AEENDAT",
    DS_TRT_DISP_AESER = "AESER"
  ),
  DS6001_LV6 = list(
    TRT_PH_DISP_VISIT      = "VISITNUM",
    TRT_PH_DISP_VISIT_DOV  = "VISDAT",
    TRT_PH_DISP_DATE       = "DISPOSITION_DATE",
    LAST_DOSE              = "LAST_DOSE_DATE",
    TRT_PH_DISCON_REAS     = "DS_TRT_PHASE",
    TRT_PH_DISP_DSTERM     = "DSTERM",
    TRT_PH_DISP_AEGRPID    = "AEGRPID_RELREC",
    TRT_PH_DISP_AEACN      = "AEACN",
    TRT_PH_DISP_AETERM     = "AETERM",
    TRT_PH_DISP_AESTDAT    = "AESTDAT",
    TRT_PH_DISP_AEENDAT    = "AEENDAT",
    TRT_PH_DISP_AESER      = "AESER"
  ),
  DS6001_LV7 = list(
    FU_PH_DISP_VISIT      = "VISITNUM",
    FU_PH_DISP_VISIT_DOV  = "VISDAT",
    FU_PH_DISP_DATE       = "DISPOSITION_DATE",
    LAST_DOSE             = "LAST_DOSE_DATE",
    FU_PH_DISCON_REAS     = "DS_TRT_PHASE",
    FU_PH_DISP_DSTERM     = "DSTERM",
    FU_PH_DISP_AEGRPID    = "AEGRPID_RELREC",
    FU_PH_DISP_AEACN      = "AEACN",
    FU_PH_DISP_AETERM     = "AETERM",
    FU_PH_DISP_AESTDAT    = "AESTDAT",
    FU_PH_DISP_AEENDAT    = "AEENDAT",
    FU_PH_DISP_AESER      = "AESER"
  )
)

DISPOSITION_R2 <- DISPOSITION_R1

for (fid in names(form_map)) {
  for (target in names(form_map[[fid]])) {
    source <- form_map[[fid]][[target]]
    # If target column doesn't exist, create as NA of the correct type (guess character, change if you need Date)
    if (!target %in% names(DISPOSITION_R2)) {
      DISPOSITION_R2[[target]] <- NA
    }
    DISPOSITION_R2[[target]] <- ifelse(
      DISPOSITION_R1$FORMEID == fid,
      DISPOSITION_R1[[source]],
      DISPOSITION_R2[[target]]
    )
  }
}


DISPOSITION_R2 <- DISPOSITION_R2 %>%
  mutate(DEATH = if_else(!is.na(DTHDAT), "Y", NA_character_))

# For DS_SCR
DS_SCR <- DISPOSITION_R2 %>%
  filter(FORMEID == "DS6001_LV4") %>%
  mutate(DEATH_SCR = DEATH) %>%
  mutate(DTHDAT_SCR = DTHDAT) %>% 
  select(-starts_with("DS_TRT"), -starts_with("TRT_PH"), -starts_with("FU_PH"),-DS_FU_PHASE,-DS_SCR) %>% 
  distinct()

# For DS_TRT
DS_TRT <- DISPOSITION_R2 %>%
  filter(FORMEID == "DS6001_LV5") %>%
  mutate(DEATH_TRT = DEATH) %>%
  mutate(DTHDAT_TRT = DTHDAT) %>% 
  select(-starts_with("DS_SCR"), -starts_with("TRT_PH"), -starts_with("FU_PH"),-DS_FU_PHASE,-DS_TRT_PHASE) %>% 
  distinct()

# For DS_TRT_PH
DS_TRT_PH <- DISPOSITION_R2 %>%
  filter(FORMEID == "DS6001_LV6") %>%
  mutate(DEATH_TRT_PH = DEATH) %>%
  mutate(DTHDAT_TRT_PH = DTHDAT) %>% 
  select(-starts_with("DS_SCR"), -starts_with("DS_TRT"), -starts_with("FU_PH"),-DS_FU_PHASE,-DS_TRT_PHASE) %>% 
  distinct()

# For DS_FU_PH
DS_FU_PH <- DISPOSITION_R2 %>%
  filter(FORMEID == "DS6001_LV7") %>%
  mutate(DTHDAT_FU_PH = DTHDAT) %>% 
  mutate(DEATH_FU_PH = DEATH) %>%
  select(-starts_with("DS_SCR"), -starts_with("DS_TRT"), -starts_with("TRT_PH"),-DS_TRT_PHASE,-DS_FU_PHASE) %>% 
  distinct()


remove_cols <- c("FORMEID","VISITNUM","DSTERM","DSDCDSCAT_SUBJECT","AEGRPID_RELREC", "LASTCHGDATE",
                 "DISPOSITION_DATE", "AETERM", "AESTDAT","AEENDAT" ,  "AESER", "AEACN","DEATH","DTHDAT" )

DS_SCR     <- DS_SCR    %>% arrange(SUBJID)
DS_TRT     <- DS_TRT    %>% arrange(SUBJID)
DS_TRT_PH  <- DS_TRT_PH %>% arrange(SUBJID)
DS_FU_PH   <- DS_FU_PH  %>% arrange(SUBJID)

common_cols <- Reduce(intersect, list(
  colnames(DS_SCR),
  colnames(DS_TRT),
  colnames(DS_TRT_PH),
  colnames(DS_FU_PH)
))

common_cols <- setdiff(common_cols,remove_cols)


DS_SCR2 <- DS_SCR %>%
  select(all_of(common_cols), starts_with("DS_SCR"), DEATH_SCR,DTHDAT_SCR)

DS_TRT2 <- DS_TRT %>%
  select(all_of(common_cols), starts_with("DS_TRT"), DEATH_TRT,DTHDAT_TRT)

DS_TRT_PH2 <- DS_TRT_PH %>%
  select(all_of(common_cols), starts_with("TRT_PH"), DEATH_TRT_PH,DTHDAT_TRT_PH)

DS_FU_PH2 <- DS_FU_PH %>%
  select(all_of(common_cols), starts_with("FU_PH"), DEATH_FU_PH,DTHDAT_FU_PH)

DISPOSITION_R15 <- DS_SCR2 %>%
  full_join(DS_TRT2,     by = common_cols) %>%
  full_join(DS_TRT_PH2,  by = common_cols) %>%
  full_join(DS_FU_PH2,   by = common_cols)

DISPOSITION_R15 <- DISPOSITION_R15 %>%
  mutate(
    DEATH = coalesce(DEATH_SCR, DEATH_TRT, DEATH_TRT_PH, DEATH_FU_PH),
    DTHDAT = coalesce(DTHDAT_SCR,DTHDAT_TRT,DTHDAT_TRT,DTHDAT_FU_PH)
  )

final_cols <- c(
  "SITE", "SUBJID", "DSSTDAT_IC", "RANDOMZN_DATE",
  "DS_SCR_VISIT", "DS_SCR_VISIT_DOV", "DS_SCR_DISP_DATE", "LAST_DOSE", "DS_SCR_DISCON_REAS", "DS_SCR_DSTERM",
  "DS_SCR_AEGRPID", "DS_SCR_AEACN", "DS_SCR_AETERM", "DS_SCR_AESTDAT", "DS_SCR_AEENDAT", "DS_SCR_AESER",
  "DS_TRT_VISIT", "DS_TRT_VISIT_DOV", "DS_TRT_DISP_DATE", "DS_TRT_DISCON_REAS", "DS_TRT_DSTERM",
  "DS_TRT_AEGRPID", "DS_TRT_AEACN", "DS_TRT_AETERM", "DS_TRT_AESTDAT", "DS_TRT_AEENDAT", "DS_TRT_DISP_AESER",
  "TRT_PH_DISP_VISIT", "TRT_PH_DISP_VISIT_DOV", "TRT_PH_DISP_DATE", "TRT_PH_DISCON_REAS", "TRT_PH_DISP_DSTERM",
  "TRT_PH_DISP_AEGRPID", "TRT_PH_DISP_AEACN", "TRT_PH_DISP_AETERM", "TRT_PH_DISP_AESTDAT", "TRT_PH_DISP_AEENDAT", "TRT_PH_DISP_AESER",
  "FU_PH_DISP_VISIT", "FU_PH_DISP_VISIT_DOV", "FU_PH_DISP_DATE", "FU_PH_DISCON_REAS", "FU_PH_DISP_DSTERM",
  "FU_PH_DISP_AEGRPID", "FU_PH_DISP_AEACN", "FU_PH_DISP_AETERM", "FU_PH_DISP_AESTDAT", "FU_PH_DISP_AEENDAT", "FU_PH_DISP_AESER", "QUERY_TEXT","DTHDAT"
)
final_cols <- unique(final_cols)

DISPOSITION_R3 <- DISPOSITION_R15 %>% select(all_of(final_cols))

DISPOSITION <- DISPOSITION_R3 %>%
  rename(
    INFORMED_CONSENT_DATE = DSSTDAT_IC,
    RANDOMISATION_DATE   = RANDOMZN_DATE,
    DEATH_DATE           = DTHDAT,
    MISSING_DATA         = QUERY_TEXT
  ) %>%
  distinct() %>%
  arrange(SUBJID)


#d_Disposition(indsn1,indsn2,indsn3,indsn4,indsn5,indsn6,visit_number,formeid_list)
