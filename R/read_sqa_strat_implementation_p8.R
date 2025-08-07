#'
#' Read SQA strategic plan implementation page 8
#' 

read_sqa_strategy_implementation_p8 <- function(df_text) {
  ptext <- df_text[[8]][c(5:20, 26:30)] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace_all(pattern = "^\\s{51,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{42}Committee", replacement = ";;;;;;Committee;") |>
    stringr::str_replace_all(pattern = "\\s{38,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    (\(x)
      {
        x[c(1:6, 8:15, 18:20)] <- paste0(x[c(1:6, 8:15, 18:20)], ";;;;;;;")
        x
      }
    )() |>
    stringr::str_replace_all(
      pattern = "^Assessors", replacement = "Assessors;;;;;;;;;"
    ) |>
    (\(x)
      {
        x[16] <- "Strengthened links;1.4.1. Strengthen linkages;Records of meetings and;YES;YES;YES;YES;YES;SQA Technical;SQA Records"
        x
      }
    )() |>
    stringr::str_replace_all(
      pattern = "with local providers, agreements with local",
      replacement = "with local providers,;agreements with local"
    ) |>
    stringr::str_split(";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  outcome <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>    
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x[1], x[1], x[1], x[2]))()

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
    (\(x) c("", "", "", x))()

  strategy <- stringr::str_remove_all(
    string = df$X2, pattern = "[0-9]{1}\\.[0-9]{1,2}\\.[0-9]{1,2}\\. "
  ) |>
    paste(collapse = " ") |>
    trimws() |>
    (\(x) c("", "", "", x))()

  performance_indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\. Guide", replacement = ".;Guide") |>
    stringr::str_replace_all(
      pattern = "Board Records", replacement = "Board;Records"
    ) |>
    stringr::str_split(pattern = "\\s{2,}|;") |>
    unlist()

  target <- df[ , c("X4", "X5", "X6", "X7", "X8")] |>
    lapply(FUN = get_indicator_targets) |>
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(nm = 2022:2026) |>
    (\(x) rbind("", "", "", x))()

  responsible <- df$X9 |>
    paste(collapse = " ") |>
    trimws() |>
    (\(x) c("", "", "", x))()

  data_source <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    (\(x) c("", "", "", x))()
  
  tibble::tibble(
    outcome, strategy_code, strategy, performance_indicator, target,
    responsible, data_source
  )
}