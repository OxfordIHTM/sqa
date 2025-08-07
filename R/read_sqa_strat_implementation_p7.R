#'
#' Read SQA strategic plan implementation page 7
#' 

read_sqa_strategy_implementation_p7 <- function(df_text) {
  ptext <- df_text[[7]][c(5:31)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{49,}", replacement = ";;") |>
    #stringr::str_replace_all(pattern = "^\\s{30,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{106,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{64,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(
      pattern = "^Reviewed\\s{41}", replacement = "Reviewed;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^Programme\\s{40}", replacement = "Programme;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^Providers\\s{40}", replacement = "Providers;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^- Programme\\s{38}", replacement = "- Programme;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^The reviewed\\s{37}", replacement = "The reviewed;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^Accreditation\\s{36}", replacement = "Accreditation;;"
    ) |>
    stringr::str_replace_all(pattern = "\\s{36,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{33,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "^another$", replacement = "another;;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^Guide for$", replacement = "Guide for;;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^Accreditation$", replacement = "Accreditation;;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "documents for;views and input on the", 
      replacement = ";views and input on the;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";stakeholders to gauge their$", 
      replacement = ";stakeholders to gauge their;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";The QA Manual reviewed$", 
      replacement = ";The QA Manual reviewed;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";;and approved by SQA$", 
      replacement = ";;and approved by SQA;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";- Institutional$", 
      replacement = ";- Institutional;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";Accreditation and$", 
      replacement = ";Accreditation and;"
    ) |>
    stringr::str_replace_all(
      pattern = ";;Accreditation Manual$", 
      replacement = ";;Accreditation Manual;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";;- Programme Accreditation$", 
      replacement = ";;- Programme Accreditation;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";;Manual$", 
      replacement = ";;Manual;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";;Board as:$", 
      replacement = ";;Board as:;;;;;;;"
    ) |>
    stringr::str_split(";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{1,}Reviewed", replacement = ";Reviewed" 
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{1,}The reviewed", replacement = ";The reviewed" 
    ) |>    
    stringr::str_split(pattern = ";") |>
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
    (\(x) c("", rep(x, 3)))()

  strategy <- stringr::str_remove_all(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
  ) |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{2,}Review", replacement = ";Review"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = " ") |>
    stringr::str_split(pattern = ";") |>
    unlist() |>
    (\(x) c(x, x[2], x[2]))()

  performance_indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}|;") |>
    unlist()

  target <- df[ , c("X4", "X5", "X6", "X7", "X8")] |>
    lapply(FUN = get_indicator_targets) |>
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(nm = 2022:2026) |>
    (\(x) rbind("", x, x, x))()

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = "; ") |>
    (\(x) c("", x, x, x))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace(
      pattern = "\\sPublished", replacement = ";;Published"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x, x[2], x[2]))()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}