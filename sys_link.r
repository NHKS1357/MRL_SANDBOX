
links <- function() {
  
  # Get list of datasets in environment
  name_list <- ls(envir = .GlobalEnv)
  
  # Importing the links metadata file
  LINKS_Meta <- read_excel("/lillyce/qa/general/other/diva/programs/utility_macros/sys_link/Link Details.xlsx")
  
  LINKS_Meta_1 <- LINKS_Meta %>%
    mutate(
      formlink = paste0("V1000", row_number()),
      base_lower = Base,
      comparator_lower = Comparator # Convert to lowercase
    ) %>%
    filter(
      base_lower %in% name_list | comparator_lower %in% name_list
    ) %>%
    select(-base_lower, -comparator_lower)
  
  if (nrow(LINKS_Meta_1) == 0) {
    stop("Error: links_final is empty after processing. Check dataset availability.")
  }
  
  LINKS_Meta_1 <- LINKS_Meta_1 %>%
    arrange(formlink)
  
  if (nrow(LINKS_Meta_1) > 0) {
    domain_list <- LINKS_Meta_1$Base
    var_list <- LINKS_Meta_1$Field
    comp_list <- LINKS_Meta_1$Comparator
    baseid_list <- LINKS_Meta_1$Base_id
    tot <- nrow(LINKS_Meta_1)  # Store the total count
    
    #print(paste("Total Count:", tot))
  } else {
    print("LINKS_Meta_1 is empty.")
  }
  syslinking_all <- function(bas) {
    tryCatch({
      # Extract relevant rows from LINKS_Meta for the given base
      links_filtered <- LINKS_Meta %>% filter(Base == bas)
      
      # Filter sys_links based on 'bas' value
      sys1 <- SYS_LINKS %>%
        filter(str_detect(FORMNAME, paste0("^", bas))) %>%
        mutate(formseqnbr1 = ifelse(is.na(ITEMGROUPSEQNBR), FORMSEQNBR, ITEMGROUPSEQNBR)) %>%
        select(formseqnbr1, SUBJECTNAME, EVENTNAME, FORMNAME, FORMLINKID)
      
      # Handle case when no data is found
      if (nrow(sys1) == 0) {
        base_data <- get(tolower(bas), envir = globalenv())
        base_data <- base_data %>% mutate(across(everything(), as.character))  # Convert all to character
        assign(tolower(bas), base_data, envir = .GlobalEnv)  # Ensure update in global env
        return()
      }
      
      # Load base dataset from the global environment
      base_data <- get(bas, envir = globalenv())
      
      # Initialize merged dataset as base_data
      linkeddata <- base_data
      
      # Iterate through each row in links_filtered
      for (i in 1:nrow(links_filtered)) {
        comp_val <- links_filtered$Comparator[i]
        base_id <- links_filtered$Base_id[i]
        field <- links_filtered$Field[i]
        
        # Filter sys_links based on current 'comp' value
        sys_comp <- SYS_LINKS %>%
          filter(str_detect(FORMNAME, paste0("^", comp_val))) %>%
          mutate(formseqnbr1 = ifelse(is.na(ITEMGROUPSEQNBR), FORMSEQNBR, ITEMGROUPSEQNBR)) %>%
          select(formseqnbr1, SUBJECTNAME, EVENTNAME, FORMNAME, FORMLINKID)
        
        if (nrow(sys_comp) == 0) {
          next  # Skip iteration if no matching rows found in sys_links
        }
        
        # Perform merging between sys1 and sys_comp
        sys_merge <- left_join(sys1, sys_comp, by = c("FORMLINKID", "SUBJECTNAME")) %>%
          select(-c("EVENTNAME.y", "FORMNAME.y", "FORMLINKID")) %>%
          rename(SUBJID = SUBJECTNAME, VISIT = EVENTNAME.x, FORMEID = FORMNAME.x)
        
        # Rename the key columns
        colnames(sys_merge)[colnames(sys_merge) == "formseqnbr1.x"] <- base_id
        
        # Merge with linkeddata
        linkeddata <- linkeddata %>%
          left_join(sys_merge, by = c("SUBJID", base_id, "VISIT", "FORMEID")) %>%
          distinct()
        
        colnames(linkeddata)[colnames(linkeddata) == "formseqnbr1.y"] <- field
      }
      
      # Ensure the updated dataset is stored in the global environment
      assign(bas, linkeddata, envir = .GlobalEnv)
      
      return()
      
    }, error = function(e) {
      
      return(NULL)  # Return NULL so the iteration continues
    })
  }
  
  for (bas in domain_list) {
    if (!(bas %in% name_list)) {
      next
      
    }
    print(paste("Processing:", bas))
    syslinking_all(bas)
  }
}  



