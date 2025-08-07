#'
#' Read SQA strategic plan implementation page 15
#' 

read_sqa_strategy_implementation_p15 <- function(df_text) {
  ptext <- df_text[[15]][c(6:23, 27:31)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{30,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{61,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(
      pattern = "programmes\\s{42}by", replacement = "programmes;;by"
    ) |>
    stringr::str_replace_all(pattern = "\\s{36,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "YES -", replacement = "Yes;-") |>
    stringr::str_replace_all(pattern = " RPL -", replacement = " RPL;-") |>
    stringr::str_replace_all(
      pattern = "Tools for monitoring", replacement = "Tools for monitoring;-"
    ) |>
    stringr::str_replace_all(
      pattern = "implement tools for implementation of", 
      replacement = "implement tools for;implementation of"
    ) |>
    (\(x)
      {
        x[c(5, 10)] <- paste0(x[c(5, 10)], ";;;;;;;;")
        x
      }
    )() |>
    (\(x)
      {
        x[21] <- paste0(x[21], ";")
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
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "\\s{1}Reviewed", replacement = ";Reviewed"
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{1}Tools", replacement = ";Tools"
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
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = " Guidelines", replacement = ";Guidelines"
    ) |>
    stringr::str_replace_all(
      pattern = " Tools", replacement = ";Tools"
    ) |>
    stringr::str_split(pattern = ";") |>
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
    stringr::str_replace_all(
      pattern = "\\s{1,}Published", replacement = ";;Published"
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{2,}Monitoring", replacement = "; Monitoring"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}