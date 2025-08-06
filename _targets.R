# Seychelles SQA database workflow ---------------------------------------------


## Load libraries and custom functions ----
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

## Data targets ----
data_targets <- tar_plan(
  sqa_strat_plan_pdf = "data-raw/pdf/SQA Strategic Plan 2022 to 2026.pdf"
)


## Processing targets ----
processing_targets <- tar_plan(
  tar_target(
    name = sqa_strat_plan,
    command = read_sqa_strategy(pdf = sqa_strat_plan_pdf)
  ),
  tar_target(
    name = sqa_strat_priority, 
    command = get_sqa_strat_priority(sqa_strat_plan)
  ),
  tar_target(
    name = sqa_strat_objective,
    command = get_sqa_strat_objective(sqa_strat_plan)
  ),
  tar_target(
    name = sqa_strat_results,
    command = read_sqa_strategy_results(
      pdf = sqa_strat_plan_pdf, sqa_strat_objective
    )
  )
)


## Analysis targets ----
analysis_targets <- tar_plan(
  
)


## Output targets ----
output_targets <- tar_plan(
  tar_target(
    name = sqa_strat_plan_csv,
    command = write_as_csv(
      df = sqa_strat_plan, path = "data/sqa_strat_plan.csv"
    ),
    format = "file"
  ),
  tar_target(
    name = sqa_strat_priority_csv,
    command = write_as_csv(
      df = sqa_strat_priority, path = "data/sqa_strat_priority.csv"
    ),
    format = "file"
  ),
  tar_target(
    name = sqa_strat_objective_csv,
    command = write_as_csv(
      df = sqa_strat_objective, path = "data/sqa_strat_objective.csv"
    ),
    format = "file"
  ),
  tar_target(
    name = sqa_strat_results_csv,
    command = write_as_csv(
      df = sqa_strat_results, path = "data/sqa_strat_results.csv"
    ),
    format = "file"
  )
)


## Reporting targets ----
report_targets <- tar_plan(
  
)


## Deploy targets ----
deploy_targets <- tar_plan(
  
)


## List targets ----
all_targets()
