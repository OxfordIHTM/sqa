
pdf <- sqa_strat_plan_pdf

#'
#' Read SQA strategic plan implementation page 3
#' 

read_sqa_strategy_implementation_p3 <- function(df_text) {
  ptext <- df_text[[3]][5:30] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{138,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{115,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{52,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{21,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{129,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{102,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{85,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{39,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{33,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "^;by the workforce are", 
      replacement = ";by the workforce are;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^;recognised", 
      replacement = ";recognised;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "Accreditation$", replacement = "Accreditation;"
    ) |>
    stringr::str_replace_all(
      pattern = "Inspection$", replacement = "Inspection;"
    ) |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c("", x[1], x))()

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
    (\(x) c("", x[1], x))()

  strategy <- stringr::str_remove_all(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
  ) |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "Accredit", replacement = "  Accredit") |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c(x[1], x[2], x[2], x[3], x[4]))()

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
    stringr::str_split("\\s{2,}") |>
    unlist() |>
    (\(x) c("", x))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{2,}Database", replacement = "; Database"
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{1,}Quarterly", replacement = ";;Quarterly"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}