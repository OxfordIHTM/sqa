#'
#' Read SQA strategic plan implementation page 17
#' 

read_sqa_strategy_implementation_p17 <- function(df_text) {
  ptext <- df_text[[17]][9:30] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{29,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{88,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{60,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{40,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{23,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "survey;;", replacement = "survey;-;") |>
    (\(x)
      {
        x[c(3, 5, 9:11, 18:19)] <- paste0(x[c(3, 5, 9:11, 18:19)], ";;;;;;;;")
        x
      }
    )() |>
    (\(x)
      {
        x[c(14, 16)] <- paste0(x[c(14, 16)], ";;;;;;;")
        x
      }
    )() |>
    (\(x)
      {
        x[c(7, 21)] <- paste0(x[c(7, 21)], ";")
        x
      }
    )() |>
    stringr::str_replace_all(
      pattern = "have 4\\.1\\.1\\.", replacement = "have;4.1.1."
    ) |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
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
    unlist()

  strategy <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(
      pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. |\\s{1,}[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
    ) |>
    unlist() |>
    (\(x) x[x != ""])()

  performance_indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  target <- df[ , c("X4", "X5", "X6", "X7", "X8")] |>
    lapply(FUN = get_indicator_targets) |>
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(nm = 2022:2026)

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "records  Survey", replacement = "records;;Survey"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}