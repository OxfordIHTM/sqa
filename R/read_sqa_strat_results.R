#'
#' Read SQA strategic results
#' 

read_sqa_strategy_results <- function(pdf) {

}

pdf <- "data-raw/pdf/SQA Strategic Plan 2022 to 2026.pdf"

df_text <- pdftools::pdf_text(pdf) |>
  (\(x) x[20:27])() |>
  stringr::str_split(pattern = "\n")


#'
#' Read SQA strategic results page 1
#' 

read_sqa_strategy_results_p1 <- function(df_text) {
  ptext <- df_text[[1]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{146,}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{40,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{17,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{110,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{95,97}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{50,61}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{40}", replacement = ";;;") |>
    stringr::str_replace_all(pattern = "\\s{30,33}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{28}", replacement = ";;;;") |>
    stringr::str_replace_all(pattern = "\\s{23,26}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "^;education, quality", replacement = ";education, quality;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^;assurance and", replacement = ";assurance and;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^;qualifications", replacement = ";qualifications;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = ";frameworks", replacement = ";frameworks;;;;;;;;;;") |>
    (\(x) ifelse(stringr::str_detect(string = x, pattern = "SQA reports$"), paste0(x, ";;"), x))() |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c(x, x[2]))()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace(
      pattern = "frameworks Consolidate", replacement = "frameworks;Consolidate"
    ) |>
    stringr::str_split(pattern = "\\;") |>
    unlist() |>
    (\(x) c(x, x[2]))()

  indicator <- df$X3 |>
    paste(collapse = " ") |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  df_targets <- suppressWarnings(
    lapply(
      X = df[ , c("X4", "X5", "X6", "X7", "X8", "X9")], 
      FUN = get_indicator_targets
    ) |> 
      do.call(cbind, args = _) |>
      data.frame() |>
      setNames(
        nm = c(
          "baseline", "target_2022", "target_2023", "target_2024", 
          "target_2025", "target_2026")
      )
  )

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    stringr::str_split(pattern = "\\s{3,}") |>
    lapply(
      FUN = stringr::str_replace_all, pattern = "\\s{2}", replacement = "; "
    ) |>
    unlist()
  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{1}\\%", replacement = ";%") |>
    stringr::str_split(pattern = "\\;") |>
    unlist()    

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' Read SQA strategic results page 2
#' 

read_sqa_strategy_results_p2 <- function(df_text) {
  ptext <- df_text[[2]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{144,}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{125,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{107,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{36}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{100,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{73,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{52,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{23,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    trimws()

  indicator <- df$X3 |>
    paste(collapse = " ") |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  df_targets <- lapply(
    X = df[ , c("X4", "X5", "X6", "X7", "X8", "X9")], 
    FUN = get_indicator_targets
  ) |> 
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(
      nm = c(
        "baseline", "target_2022", "target_2023", "target_2024", 
        "target_2025", "target_2026")
    )

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    stringr::str_replace_all(
      pattern = "SQA reports\\s{2,}Database", 
      replacement = "SQA reports;;Database"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist()
  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{1}\\%", replacement = ";%") |>
    stringr::str_replace_all(pattern = "\\s{1}Increase", replacement = ";Increase") |>
    stringr::str_split(pattern = "\\;") |>
    unlist()    

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' Read SQA strategic results page 3
#' 

read_sqa_strategy_results_p3 <- function(df_text) {
  ptext <- df_text[[3]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{141,}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{122,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{107,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{38}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{110,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{86,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{64,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{50,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{23,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "Standards$", replacement = "Standards;") |>
    stringr::str_replace_all(pattern = "Officers$", replacement = "Officers;") |>
    (\(x) ifelse(grepl(pattern = "^to meet the|^functions|^requirements|^specified in|^the SQA Act|^2021", x = x), paste0(x, ";;;;;;;;;;;"), x))() |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    stringr::str_replace(pattern = "^\\s{2,}", replacement = ";;") |>
    stringr::str_split(pattern = ";") |>
    unlist()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    stringr::str_replace(pattern = "^\\s{2,}", replacement = ";;") |>
    stringr::str_split(pattern = ";") |>
    unlist()

  indicator <- df$X3 |>
    paste(collapse = " ") |>
    stringr::str_trim(side = "right") |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  df_targets <- lapply(
    X = df[ , c("X4", "X5", "X6", "X7", "X8", "X9")], 
    FUN = get_indicator_targets
  ) |> 
    do.call(cbind, args = _) |>
    (\(x) rbind(rep("", 6), x))() |>
    data.frame() |>
    setNames(
      nm = c(
        "baseline", "target_2022", "target_2023", "target_2024", 
        "target_2025", "target_2026")
    )

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    stringr::str_replace_all(
      pattern = "records\\s{2}Annual", replacement = "records;;Annual"
    ) |>
    stringr::str_replace_all(
      pattern = "Website\\s{2,}SQA", replacement = "Website;;SQA"
    ) |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist()
  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace(pattern = "\\s{2,}", replacement = ";;") |>
    stringr::str_replace(pattern = "\\s{2}", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{1}Level", replacement = ";Level") |>
    stringr::str_replace_all(pattern = "\\s{1}Updated", replacement = ";Updated") |>
    stringr::str_split(pattern = "\\;") |>
    unlist()    

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' Read SQA strategic results page 4
#' 

read_sqa_strategy_results_p4 <- function(df_text) {
  ptext <- df_text[[4]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{40}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{17}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{133,}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{109,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{83,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{72,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{54,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{21,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "assurance$", replacement = "assurance;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "other$", replacement = "other;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "with a$", replacement = "with a;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "quality$", replacement = "quality;;;;;;;;;") |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c(x[1], x, x[2]))()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    (\(x) c(x[1], x, x[2]))()


  indicator <- df$X3 |>
    paste(collapse = " ") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "assurance Number", replacement = "assurance;;Number"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()

  df_targets <- lapply(
    X = df[ , c("X4", "X5", "X6", "X7", "X8", "X9")], 
    FUN = get_indicator_targets
  ) |> 
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(
      nm = c(
        "baseline", "target_2022", "target_2023", "target_2024", 
        "target_2025", "target_2026")
    )

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = "; ") |>
    stringr::str_replace(
      pattern = "Administrativ e", replacement = "Administrative"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()

  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "stakeholders Involvement", 
      replacement = "stakeholders;;Involvement"
    ) |>
    stringr::str_replace_all(
      pattern = "activities Published", replacement = "activities;;Published"
    ) |>
    stringr::str_replace(
      pattern = "Administrativ e", replacement = "Administrative"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()    

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' Read SQA strategic results page 5
#' 

read_sqa_strategy_results_p5 <- function(df_text) {
  ptext <- df_text[[5]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{143}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{110}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{42}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{130,}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{86,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{67,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{49,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{22,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "deliver$", replacement = "deliver;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "mandate and$", replacement = "mandate and;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "Cabinet$", replacement = "Cabinet;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "requiring$", replacement = "requiring;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "expertise$", replacement = "expertise;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "teams$", replacement = "teams;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^;;;;;;;;;Contract$", replacement = ";;;;;;;;;Contract;;") |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    stringr::str_split(pattern = "\\s{3,}") |>
    unlist() |>
    (\(x) c(x[1], "", "", x[2], x[2], x[2], x[3]))()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") |>
    (\(x) c("", "", "", x, x, x, ""))()

  indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    
    
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "assurance Number", replacement = "assurance;;Number"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()

  df_targets <- lapply(
    X = df[ , c("X4", "X5", "X6", "X7", "X8", "X9")], 
    FUN = get_indicator_targets
  ) |> 
    do.call(cbind, args = _) |>
    data.frame() |>
    setNames(
      nm = c(
        "baseline", "target_2022", "target_2023", "target_2024", 
        "target_2025", "target_2026")
    )

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = "; ") |>
    stringr::str_replace(
      pattern = "Administrativ e", replacement = "Administrative"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()

  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "stakeholders Involvement", 
      replacement = "stakeholders;;Involvement"
    ) |>
    stringr::str_replace_all(
      pattern = "activities Published", replacement = "activities;;Published"
    ) |>
    stringr::str_replace(
      pattern = "Administrativ e", replacement = "Administrative"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist()    

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}

remove_extra_lines <- function(p) {
  p |>
    (\(x) x[!grepl(pattern = "^\\s{2,}Table", x = x)])() |>
    (\(x) x[!grepl(pattern = "^Key", x = x)])() |>
    (\(x) x[!grepl(pattern = "^Challenge", x = x)])() |>
    (\(x) x[!grepl(pattern = "^\\s{2,}\\‘[0-9]{2}", x = x)])() |>
    (\(x) x[!grepl(pattern = "^\\s{2,}The Seychelles Qualifications Authority Strategic Plan", x = x)])() |>
    (\(x) x[x != ""])()
}

get_indicator_targets <- function(x) {
  x |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    stringr::str_replace_all(pattern = "NIL|Nil", replacement = "0") |>
    stringr::str_replace_all(pattern = "N\\/A", replacement = NA_character_)
}