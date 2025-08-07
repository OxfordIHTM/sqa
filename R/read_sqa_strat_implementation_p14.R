#'
#' Read SQA strategic plan implementation page 14
#' 

read_sqa_strategy_implementation_p14 <- function(df_text) {
  ptext <- df_text[[14]][5:30] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{52,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{29,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{104,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{71,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{36,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    (\(x)
      {
        x[6:8] <- paste0(x[6:8], ";;;;;;;;;")
        x
      }
    )() |>
    (\(x)
      {
        x[c(4:5, 14:17, 20)] <- paste0(x[c(4:5, 14:17, 20)], ";;;;;;;;")
        x
      }
    )() |>
    (\(x)
      {
        x[c(3, 12:13, 25)] <- paste0(x[c(3, 12:13, 25)], ";;;;;;;")
        x
      }
    )() |>
    (\(x)
      {
        x[24] <- paste0(x[24], ";;")
        x
      }
    )() |>
    (\(x)
      {
        x[c(11, 21:23)] <- paste0(x[c(11, 21:23)], ";")
        x
      }
    )() |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{1}Schools", replacement = ";Schools"
    ) |>
    stringr::str_split(pattern = "\\s{2,}|;") |>
    unlist() |>
    (\(x) c(x, x[3]))()

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
    (\(x) c(x, x[3]))()

  strategy <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(
      pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. |\\s{1,}[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
    ) |>
    unlist() |>
    (\(x) x[x != ""])() |>
    (\(x) c(x, x[3]))()

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
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x, ""))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c(x, ""))()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}