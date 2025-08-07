#'
#' Read SQA strategic plan implementation page 11
#' 

read_sqa_strategy_implementation_p11 <- function(df_text) {
  ptext <- df_text[[11]][c(5:6, 9:26)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{29,}", replacement = ";") |>  
    stringr::str_replace_all(pattern = "\\s{99,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{65,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{30,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "197 226 246 272 297 Principal Officers",
      replacement = "197;226;246;272;297;Principal Officers"
    ) |>
    stringr::str_replace_all(pattern = "YES -", replacement = "YES;-") |>
    (\(x)
      {
        x[c(4, 7:9, 12:14, 17:20)] <- paste0(x[c(4, 7:9, 12:14, 17:20)], ";;;;;;;;")
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
      pattern = "institutionalised Reviewed", 
      replacement = "institutionalised  Reviewed"
    ) |>
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
    unlist() |>
    (\(x) c("", x))()

  strategy <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. ") |>
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
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x, ""))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "Reports Cabinet", replacement = "Reports;;Cabinet"
    ) |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}