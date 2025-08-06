
pdf <- sqa_strat_plan_pdf
df_text <- pdftools::pdf_text(pdf) |>
    (\(x) x[32:49])() |>
    stringr::str_split(pattern = "\n")

get_start_line <- function(x) {
  grep(pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\.", x = x) |>
    (\(x) x[1])()
}

get_end_line <- function(x) {
  grep(
    pattern = "The Seychelles Qualifications Authority Strategic Plan (2022-2026)", 
    x = x
  )
}

#'
#' Read SQA strategic plan implementation page 1
#' 

read_sqa_strategy_implementation_p1 <- function(df_text) {
  ptext <- df_text[[1]][14:30] |>
    (\(x) x[x != ""])() |>
    # stringr::str_replace_all(pattern = "^\\s{146,}", replacement = ";;;;;;;;;;;") |>
    # stringr::str_replace_all(pattern = "^\\s{40,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{21,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{122,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{86,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{60,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{59}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{58,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{40,}", replacement = ";;;;;;") |>
    # stringr::str_replace_all(pattern = "\\s{30,33}", replacement = ";;") |>
    # stringr::str_replace_all(pattern = "\\s{28}", replacement = ";;;;") |>
    # stringr::str_replace_all(pattern = "\\s{23,26}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    # stringr::str_replace_all(pattern = "^;education, quality", replacement = ";education, quality;;;;;;;;;;") |>
    # stringr::str_replace_all(pattern = "^;assurance and", replacement = ";assurance and;;;;;;;;;;") |>
    # stringr::str_replace_all(pattern = "^;qualifications", replacement = ";qualifications;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "YES YES", replacement = "YES;YES") |>
    stringr::str_replace_all(pattern = "\\. Board", replacement = ".;Board") |>
    stringr::str_replace_all(
      pattern = "National and", replacement = "National;and"
    ) |>
    stringr::str_replace_all(
      pattern = "^;internationally and", 
      replacement = ";internationally and;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "^;developments in", 
      replacement = ";developments in;;;;;;;;"
    ) |>
        stringr::str_replace_all(
      pattern = "^;qualifications frameworks", 
      replacement = ";qualifications frameworks;;;;;;;;"
    ) |>
        stringr::str_replace_all(
      pattern = "^;and quality assurance.", 
      replacement = ";and quality assurance.;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = "account;Qualifications Map$", 
      replacement = "account;Qualifications Map;;;;;;;"
    ) |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_replace_all(
      pattern = "Regulations The", replacement = "Regulations;;The"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x, x[2]))()

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
    (\(x) c(x, x[2]))()

  strategy <- stringr::str_remove_all(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
  ) |>
    paste(collapse = " ") |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c(x, x[2]))()

  performance_indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  target <- df[ , c("X4", "X5", "X6", "X7", "X8")] |>
    lapply(FUN = get_indicator_targets) |>
    do.call(cbind, args = _) |>
    data.frame() |>
    (\(x) rbind(x, x[2, ]))() |>
    setNames(nm = 2022:2026)

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x, x[2]))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(
      pattern = "\\s{1,}Published", replacement = ";;Published"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}


#'
#' Read SQA strategic plan implementation page 2
#' 

read_sqa_strategy_implementation_p2 <- function(df_text) {
  ptext <- df_text[[2]][6:31] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{142,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{52,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{22,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{139,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{96,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{65,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "^and part\\s{44}", replacement = "and part;;") |>
    stringr::str_replace_all(pattern = "\\s{41,}", replacement = ";;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{33,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(
      pattern = "80 Principal", replacement = "80;Principal"
    ) |>
    stringr::str_replace_all(
      pattern = "standards for qualifications and", 
      replacement = "standards for qualifications;and"
    ) |>
    stringr::str_replace_all(
      pattern = "^;formal education system", 
      replacement = ";formal education system;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";qualifications reviewed", 
      replacement = ";qualifications reviewed;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";developed using learning", 
      replacement = ";developed using learning;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";process", replacement = ";process;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";acquired outside the", 
      replacement = ";acquired outside the;;;;;;;;"
    ) |>
    stringr::str_replace_all(
      pattern = ";developed using unit", 
      replacement = ";developed using unit;;;;;;;"
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

  strategy <- stringr::str_remove_all(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
  ) |>
    paste(collapse = " ") |>
    stringr::str_split(pattern = "\\s{2,}") |>
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
    setNames(nm = 2022:2026)

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split("\\s{2,}") |>
    unlist()

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
    unlist()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}

