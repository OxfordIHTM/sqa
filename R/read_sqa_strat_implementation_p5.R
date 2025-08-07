#'
#' Read SQA strategic plan implementation page 5
#' 

read_sqa_strategy_implementation_p5 <- function(df_text) {
  ptext <- df_text[[5]][c(5:31)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{29,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{88,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{59,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{37,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "^employment or$", replacement = "employment or;;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^profession$", replacement = "profession;;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "or a profession$", replacement = "or a profession;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "tertiary education$", 
      replacement = "tertiary education;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "and training$", 
      replacement = "and training;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^;providers$", 
      replacement = ";providers;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";Board$", replacement = ";Board;;;;;;;"
    ) |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "profession Policy", replacement = "profession;Policy"
    ) |>
    stringr::str_split(pattern = "\\s{2,}|;") |>
    unlist()

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
      pattern = "\\s{1,}Review", replacement = ";Review"
    ) |>
    stringr::str_split(pattern = "\\s{2,}|;") |>
    unlist()

  performance_indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  target <- df[ , c("X4", "X5", "X6", "X7", "X8")] |>
    lapply(FUN = get_indicator_targets) |>
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(nm = 2022:2026) |>
    (\(x) rbind("", x))()

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split("\\s{2,}") |>
    unlist()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}