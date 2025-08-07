#'
#' Read SQA strategic plan implementation page 4
#' 

read_sqa_strategy_implementation_p4 <- function(df_text) {
  ptext <- df_text[[4]][c(5:10, 13:31)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{29,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{63,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(
      pattern = "National\\s{42}", replacement = "National;;"
    ) |>
    stringr::str_replace_all(
      pattern = "Guidelines for\\s{36}", replacement = "Guidelines for;;"
    ) |>
    stringr::str_replace_all(pattern = "\\s{36,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{33,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "^RPL$", 
      replacement = "RPL;;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^;for Implementation", 
      replacement = ";for Implementation;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";providers and$", replacement = ";providers and;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";programmes, and$", replacement = ";programmes, and;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";performance status of$", 
      replacement = ";performance status of;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";schools$", replacement = ";schools;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "Qualifications;Qualifications$", 
      replacement = "Qualifications;Qualifications;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "National National", replacement = "National;National"
    ) |>
    stringr::str_replace_all(pattern = "YES YES", replacement = "YES;YES") |>
    stringr::str_replace_all(pattern = "YES -", replacement = "YES;-") |>
    stringr::str_replace_all(
      pattern = "Guidelines SQA", replacement = "Guidelines;SQA"
    ) |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "schools The", replacement = "schools;The"
    ) |>
    stringr::str_replace_all(pattern = "RPL The", replacement = "RPL;The") |>
    stringr::str_replace_all(
      pattern = "Qualifications Policy", replacement = "Qualifications;Policy"
    ) |>
    stringr::str_split(pattern = ";|\\s{2,}") |>
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
    (\(x) c(x[1], x[2], x[2], x[3:4]))()

  strategy <- stringr::str_remove_all(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
  ) |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{1,}Review", replacement = ";Review"
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{1,}Develop", replacement = ";Develop"
    ) |>
    stringr::str_split(pattern = ";") |>
    unlist() |>
    (\(x) c(x[1], x[2], x[2], x[3:4]))()

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
    (\(x) rbind(x[1, ], x[2, ], x[2:4, ]))()

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split("\\s{2,}") |>
    unlist() |>
    (\(x) c(x[1], x[2], x[2:4]))()

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