#'
#' Read SQA strategic plan implementation page 12
#' 

read_sqa_strategy_implementation_p12 <- function(df_text) {
  ptext <- df_text[[12]][5:30] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{139,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{30,}", replacement = ";") |>  
    stringr::str_replace_all(pattern = "\\s{91,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{60,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{41,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "AO$", replacement = "AO;") |>
    stringr::str_replace_all(
      pattern = "Completed registration;YES", 
      replacement = "Completed registration;-;YES"
    ) |>
    (\(x)
      {
        x[c(4:5, 10:13)] <- paste0(x[c(4:5, 10:13)], ";;;;;;;;")
        x
      }
    )() |>
    (\(x)
      {
        x[c(8, 23, 26)] <- paste0(x[c(8, 23, 26)], ";;;;;;;")
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
    stringr::str_replace_all(pattern = "Database", replacement = " Database") |>
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
    stringr::str_split(pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. ") |>
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
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "Officer\\s{2}Registration", 
      replacement = "Officer;;Registration"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "Website Sensitisation", replacement = "Website;;Sensitisation"
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{2}Registration", replacement = ";;Registration"
    ) |>
    stringr::str_replace_all(
      pattern = " Attendance", replacement = "; Attendance"
    ) |>
    stringr::str_replace_all(
      pattern = "registration Website", replacement = "registration; Website"
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