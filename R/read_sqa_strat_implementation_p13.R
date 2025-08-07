#'
#' Read SQA strategic plan implementation page 13
#' 

read_sqa_strategy_implementation_p13 <- function(df_text) {
  ptext <- df_text[[13]][c(5:6, 19:30)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{136,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{72,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{35,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    (\(x)
      {
        x[c(7, 13)] <- paste0(x[c(7, 13)], ";;;;;;;;;")
        x
      }
    )() |>    
    (\(x)
      {
        x[c(6, 11:12)] <- paste0(x[c(6, 11:12)], ";;;;;;;;")
        x
      }
    )() |>
    stringr::str_replace_all(
      pattern = "Accreditation$", replacement = "Accreditation;"
    ) |>    
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{1}Providers", replacement = ";Providers"
    ) |>
    stringr::str_split(pattern = ";") |>
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

  strategy <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(
      pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. |\\s{1,}[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
    ) |>
    unlist()

  performance_indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
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
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c("", x))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = " Quarterly", replacement = ";;Quarterly"
    ) |>
    stringr::str_split(pattern = ";;")
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}