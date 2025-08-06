pdf <- sqa_strat_plan_pdf

#'
#' Read SQA strategic plan implementation page 7
#' 

read_sqa_strategy_implementation_p7 <- function(df_text) {
  ptext <- df_text[[7]][c(5:31)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{54,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{30,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{98,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{64,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{42,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "^;requirements of the$", 
      replacement = ";requirements of the;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^;SQA Act 2021$", replacement = ";requirements of the;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";;Board$", replacement = ";;Board;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "Consultant$", replacement = "Consultant;"
    ) |>
    stringr::str_split(";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c("", x))()

  strategy_code <- stringr::str_extract(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\."
  ) |>
    (\(x) x[!is.na(x)])() |>
    stringr::str_split(pattern = "\\.") |>
    lapply(
      FUN = function(x) {
        x[x != ""] |>
          stringr::str_pad(width = 2, pad = "0") |>
          (\(x) paste0(c("SP", "SO", "ST"), x))() |>
          paste(collapse = "-")
      }
    ) |>
    unlist() |>
    (\(x) c("", x))()

  strategy <- stringr::str_remove_all(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
  ) |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{1}Develop", replacement = ";Develop"
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{1}Review", replacement = ";Review"
    ) |>
    stringr::str_split(pattern = "\\s{2,}|;") |>
    unlist()

  performance_indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "Board School", replacement = "Board;School"
    ) |>
    stringr::str_replace_all(
      pattern = "Board Policy", replacement = "Board;Policy"
    ) |>
    stringr::str_replace_all(
      pattern = "Board National", replacement = "Board;National"
    ) |>
    stringr::str_split(pattern = "\\s{2,}|;") |>
    unlist() |>
    (\(x) c("", x))()

  target <- df[ , c("X4", "X5", "X6", "X7", "X8")] |>
    lapply(FUN = get_indicator_targets) |>
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(nm = 2022:2026) |>
    (\(x) rbind("", x))()

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{1,}Consultant", replacement = "; Consultant"
    ) |>
    stringr::str_replace_all(
      pattern = "Consultant Principal", replacement = "Consultant;;Principal"
    ) |>
    stringr::str_replace_all(
      pattern = "Inspection Principal", replacement = "Inspection;;Principal"
    ) |>
    stringr::str_split("\\s{2,}|;;") |>
    unlist() |>
    (\(x) c("", x))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\sPublished", replacement = ";Published"
    ) |>
    stringr::str_split(pattern = ";") |>
    unlist() |>
    (\(x) c("", x))()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}