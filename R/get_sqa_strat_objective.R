#'
#' Get SQA strategic objectives
#' 

get_sqa_strat_objective <- function(sqa_strat_plan) {
  sqa_strat_plan |> 
    dplyr::select(
      strategic_priority_code, strategic_priority,
      strategic_objective_code, strategic_objective
    ) |> 
    summarise(
      strategic_objective = unique(strategic_objective), 
      .by = c(
        strategic_priority_code, strategic_priority, strategic_objective_code
      )
    )
}