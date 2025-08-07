#'
#' Read SQA strategic plan implementation page 18
#' 

read_sqa_strategy_implementation_p18 <- function(df_text) {
  ptext <- df_text[[18]][5:6] |>
    (\(x) x[x != ""])() |>
    (\(x) paste0(x, ";;;;;;;;;"))() |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws()

  tibble::tibble(
    outcome, 
    strategy_code = "", 
    strategy = "", 
    performance_indicator = "", 
    `2022` = "",
    `2023` = "",
    `2024` = "",
    `2025` = "",
    `2026` = "",
    responsible = "", 
    data_source = ""
  )
}