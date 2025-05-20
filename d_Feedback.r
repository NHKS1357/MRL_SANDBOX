create_feedback <- function() {
  column_names <- c("study_name", "tab_name", "comment_suggestions", "study_specific_general",
                    "follow_up_data_analyst", "follow_up_crs_team", "outcome", "version", "feedback_status",
                    "action_date")
  feedback <- tibble::as_tibble(setNames(rep(list(character()), length(column_names)), column_names))
  feedback$version <- "First Draft"
  
  colnames(feedback) <- toupper(colnames(feedback))
  
  return(feedback)
}

# usage
FEEDBACK <- create_feedback()