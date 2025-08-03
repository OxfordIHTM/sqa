#'
#' Get SQA strategic priorities
#' 

get_sqa_strat_priority <- function(sqa_strat_plan) {
  sqa_strat_plan |> 
    dplyr::select(strategic_priority_code, strategic_priority) |> 
    summarise(
      strategic_priority = unique(strategic_priority), 
      .by = strategic_priority_code
    )
}