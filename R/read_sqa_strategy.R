#'
#' Read SQA strategy document 2022-2026
#' 
#' @rdname read_sqa_strategy
#' 

read_sqa_strategy <- function(pdf) {
  df_text <- pdftools::pdf_text(pdf) |>
    (\(x) x[14:19])() |>
    stringr::str_split(pattern = "\n")

  p1 <- df_text[[1]]

  p1_df <- read_sqa_strategy_p1(p1)

  p_df <- read_sqa_strategy_p(df_text)

  rbind(p1_df, p_df)
}


#' 
#' @rdname read_sqa_strategy
#'

read_sqa_strategy_p1 <- function(p1) {
  start_line <- grep(pattern = "^\\s{2,}Strategic Objectives", x = p1) + 1
  end_line <- grep(
    pattern = "^\\sThe Seychelles Qualifications Authority Strategic Plan",
    x = p1
  ) - 1

  sp_text <- p1[25:26] |>
    trimws() |>
    paste(collapse = " ")
  
  sp_code <- sp_text |>
    stringr::str_extract(pattern = "[0-9]{1}") |>
    stringr::str_pad(width = 2, pad = "0") |>
    (\(x) paste0("SP", x))()

  sp_text <- sp_text |>
    stringr::str_remove(pattern = "^Strategic Priority [0-9]{1}\\: ")

  df <- p1[start_line:end_line] |>
    (\(x) x[x != ""])() |>
    stringr::str_replace(pattern = "\\s{50,}", replacement = ";;") |>
    stringr::str_replace(pattern = "\\s{5,}", replacement = ";") |>
    trimws() |>
    stringr::str_replace(pattern = "\\s{5,}", replacement = ";") |>
    stringr::str_replace(pattern = "\\s{3,}", replacement = ";") |>
    stringr::str_split(pattern = ";", simplify = TRUE) |>
    data.frame()

  so_text <- df$X2 |>
    paste(collapse = " ") |>
    trimws()

  so_code <- df$X1 |>
    stringr::str_split(pattern = "\\.") |>
    unlist() |>
    (\(x) x[x != ""])() |>
    stringr::str_pad(width = 2, pad = "0") |>
    (\(x) paste0(c("SP", "SO"), x))() |>
    paste(collapse = "-")

  st_text <- df$X3 |>
    stringr::str_remove_all(
      pattern = "^[0-9]{1,}\\.[0-9]{1,}\\.[0-9]{1,}\\. |^[0-9]{1,}\\.[0-9]{1,}\\.[0-9]{1,} "
    ) |>
    paste(collapse = " ") |>
    stringr::str_replace(pattern = "\\. ", replacement = ".; ")

  st_code <- df$X3 |>
    stringr::str_extract_all(pattern = "[0-9]{1,}\\.[0-9]{1,}\\.[0-9]{1,}") |>
    unlist() |>
    stringr::str_split(pattern = "\\.") |>
    lapply(FUN = stringr::str_pad, width = 2, pad = "0") |>
    (\(x) lapply(X = x, FUN = function(x) paste0(c("SP", "SO", "ST"), x)))() |>
    lapply(FUN = paste, collapse = "-") |>
    paste(collapse = "; ")

  tibble::tibble(
    strategic_priority_code = sp_code,
    strategic_priority = sp_text,
    strategic_objective_code = so_code,
    strategic_objective = so_text,
    strategy_code = st_code,
    strategy = st_text
  ) |>
    dplyr::mutate(
      strategy_code = stringr::str_split(string = strategy_code, pattern = "; "),
      strategy = stringr::str_split(string = strategy, pattern = "; ")
    ) |>
    tidyr::unnest(cols = c(strategy_code, strategy))
}


#' 
#' @rdname read_sqa_strategy
#'

read_sqa_strategy_p <- function(df_text) {
  df <- df_text[2:6] |>
    unlist() |>
    (\(x) x[!grepl(pattern = "\\([0-9]{4}-[0-9]{4}\\)", x = x)])() |>
    (\(x) x[x != ""])() |>
    (\(x) x[1:(grep(pattern = "6\\.0", x = x) - 1)])()

  sp_line <- grep(pattern = "^\\s{2,}Strategic Priority [0-9]{1}", x = df)

  st_raw_df <- Map(f = seq, from = sp_line, to = sp_line + 2, by = 1) |>
    unlist() |>
    (\(x) df[!seq_len(length(df)) %in% x])() |>
    stringr::str_replace(pattern = "\\s{45,}", replacement = ";;") |>
    stringr::str_replace(pattern = "\\s{5,}", replacement = ";") |>
    trimws() |>
    stringr::str_replace(pattern = "\\s{5,}", replacement = ";") |>
    stringr::str_replace(pattern = "\\s{3,}", replacement = ";") |>
    stringr::str_split(pattern = ";", simplify = TRUE) |>
    data.frame()

  st_code <- st_raw_df$X3 |>
    stringr::str_extract_all(pattern = "[0-9]{1,}\\.[0-9]{1,}\\.[0-9]{1,}") |>
    unlist() |>
    stringr::str_split(pattern = "\\.") |>
    lapply(FUN = stringr::str_pad, width = 2, pad = "0") |>
    (\(x) lapply(X = x, FUN = function(x) paste0(c("SP", "SO", "ST"), x)))() |>
    do.call(rbind, args = _) |>
    data.frame() |>
    setNames(
      nm = c(
        "strategic_priority_code", "strategic_objective_code", "strategy_code"
      )
    ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      strategic_objective_code = paste(
        strategic_priority_code, strategic_objective_code, sep = "-"
      ),
      strategy_code = paste(
        strategic_objective_code, strategy_code, sep = "-"
      )
    ) |>
    dplyr::select(-strategic_priority_code)

  st_text <- st_raw_df$X3 |>
    stringr::str_remove_all(
      pattern = "^[0-9]{1,}\\.[0-9]{1,}\\.[0-9]{1,}\\. |^[0-9]{1,}\\.[0-9]{1,}\\.[0-9]{1,} "
    ) |>
    (\(x) x[x != ""])() |>
    stringr::str_replace(
      pattern = "Accredit national programmes of study",
      replacement = "Accredit national programmes of study."
    ) |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\. ", replacement = ".;") |>
    stringr::str_split(pattern = ";") |>
    unlist()

  st_df <- tibble::tibble(st_code, strategy = st_text)

  sp_code <- lapply(
    X = sp_line, 
    FUN = function(x) {
      df[x:(x+1)] |>
        trimws() |>
        paste(collapse = " ") |>
        stringr::str_extract(pattern = "[0-9]{1}") |>
        stringr::str_pad(width = 2, pad = "0") |>
        (\(x) paste0("SP", x))()
    } 
  ) |>
    unlist() |>
    (\(x) x[!duplicated(x)])()

  sp_text <- lapply(
    X = sp_line, 
    FUN = function(x) {
      df[x:(x+1)] |>
        stringr::str_remove(
          pattern = "^\\s{2,}Strategic Priority [0-9]{1}\\: "
        ) |>
        trimws() |>
        paste(collapse = " ")
    } 
  ) |>
    unlist() |>
    (\(x) x[!duplicated(x)])()

  sp_df <- tibble::tibble(
    strategic_priority_code = sp_code, strategic_priority = sp_text
  )

  so_code <- st_raw_df$X1 |>
    (\(x) x[x != ""])() |>
    stringr::str_split(pattern = "\\.") |>
    lapply(
      FUN = function(x) {
        x[x != ""] |>
          stringr::str_pad(width = 2, pad = "0") |>
          (\(x) paste0(c("SP", "SO"), x))()
      }
    ) |>
    do.call(rbind, args = _) |>
    data.frame() |>
    setNames(nm = c("strategic_priority_code", "strategic_objective_code")) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      strategic_objective_code = paste(
        strategic_priority_code, strategic_objective_code, sep = "-"
      )
    )

  so_text <- st_raw_df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  so_df <- tibble::tibble(so_code, strategic_objective = so_text)

  full_df <- dplyr::right_join(sp_df, so_df, by = "strategic_priority_code") |>
    dplyr::right_join(st_df, by = "strategic_objective_code")

  full_df
}


