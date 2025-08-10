#'
#' Read SQA strategic results
#' 

read_sqa_strategy_results <- function(pdf, sqa_strat_objective) {
  df_text <- pdftools::pdf_text(pdf) |>
    (\(x) x[20:27])() |>
    stringr::str_split(pattern = "\n")

  p1 <- read_sqa_strategy_results_p1(df_text)
  p2 <- read_sqa_strategy_results_p2(df_text)
  p3 <- read_sqa_strategy_results_p3(df_text)
  p4 <- read_sqa_strategy_results_p4(df_text)
  p5 <- read_sqa_strategy_results_p5(df_text)
  p6 <- read_sqa_strategy_results_p6(df_text)
  p7 <- read_sqa_strategy_results_p7(df_text)
  p8 <- read_sqa_strategy_results_p8(df_text)

  p1p2 <- rbind(p1, p2)

  p1p2p3 <- rbind(p1p2, p3)

  row8 <- lapply(
    X = p1p2p3[8:9, ], 
    FUN = function(x) paste(x, collapse = " ") |> trimws()
  ) |>
    dplyr::bind_cols()

  p1p2p3 <- p1p2p3[1:7, ] |>
    rbind(row8) |>
    rbind(p1p2p3[10:nrow(p1p2p3), ])
    
  p1p2p3p4 <- rbind(p1p2p3, p4)

  row14 <- rbind(p1p2p3p4[14, ], p5[1, ]) |>
    lapply(FUN = function(x) paste(x, collapse = " ") |> trimws()) |>
    dplyr::bind_cols()

  p1p2p3p4p5 <- rbind(p1p2p3p4[1:13, ], row14) |>
    rbind(p5[2:nrow(p5), ])

  row20 <- rbind(p1p2p3p4p5[20, ], p6[1, ]) |>
    lapply(FUN = function(x) paste(x, collapse = " ") |> trimws()) |>
    dplyr::bind_cols()

  p1p2p3p4p5p6 <- rbind(p1p2p3p4p5[1:19, ], row20) |>
    rbind(p6[2:nrow(p6), ])

  row24 <- rbind(p1p2p3p4p5p6[24, ], p7[1, ]) |>
    lapply(FUN = function(x) paste(x, collapse = " ") |> trimws()) |>
    dplyr::bind_cols()

  p1p2p3p4p5p6p7 <- rbind(p1p2p3p4p5p6[1:23, ], row24) |>
    rbind(p7[2:nrow(p7), ])

  row27 <- rbind(p1p2p3p4p5p6p7[27, ], p8[1, ]) |>
    lapply(FUN = function(x) paste(x, collapse = " ") |> trimws()) |>
    dplyr::bind_cols()

  p1p2p3p4p5p6p7p8 <- rbind(p1p2p3p4p5p6p7[1:26, ], row27) |>
    rbind(p8[2:nrow(p8), ]) |>
    dplyr::mutate(
      key_sector_challenge = ifelse(
        key_sector_challenge == "", NA_character_, key_sector_challenge
      ),
      strategic_objective = ifelse(
        strategic_objective == "", NA_character_, strategic_objective
      )
    ) |>
    tidyr::fill(key_sector_challenge:strategic_objective)

  df <- p1p2p3p4p5p6p7p8 |>
    dplyr::mutate(
      strategic_objective = trimws(strategic_objective) |>
        stringr::str_replace_all(
          pattern = "andRecognition", replacement = "and Recognition"
        ),
      dplyr::across(
        .cols = baseline:target_2026,
        .fns = function(x) ifelse(x == "NA", NA_character_, x)
      )
    ) |>
    dplyr::left_join(
      sqa_strat_objective |>
        dplyr::select(strategic_objective_code:strategic_objective)
    ) |>
    dplyr::relocate(strategic_objective_code, .before = strategic_objective)

  df
}

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
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") |>
    stringr::str_replace_all(
      pattern = "\\sOperational", replacement = ";;Operational"
    ) |>
    stringr::str_replace_all(pattern = "\\sSQA", replacement = ";;SQA") |>
    stringr::str_replace_all(pattern = "\\sNumber", replacement = ";;Number") |>
    stringr::str_replace_all(
      pattern = "\\sReviewed", replacement = ";;Reviewed"
    ) |>
    stringr::str_replace_all(pattern = "\\sThe", replacement = ";;The") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()

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
    ) |>
    (\(x) rbind(rep("", 6), x))()

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\sCabinet", replacement = ";;Cabinet"
    ) |>
    stringr::str_replace_all(
      pattern = "\\sRecruitment", replacement = ";;Recruitment"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()
  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{3,}") |>
    lapply(
      FUN = stringr::str_replace_all, pattern = "\\s{2}", replacement = " "
    ) |>
    unlist() |>
    (\(x) c("", x))()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\sConducive", replacement = ";;Conducive"
    ) |>
    stringr::str_replace_all(pattern = "\\sAnnual", replacement = ";;Annual") |>
    stringr::str_replace_all(
      pattern = "\\s{2}Improved", replacement = "; Improved"
    ) |>
    stringr::str_replace_all(
      pattern = "\\sPublished", replacement = ";;Published"
    ) |>
    stringr::str_replace_all(
      pattern = "\\s{2}Registration", replacement = ";;Registration"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = "") |>
    stringr::str_replace_all(pattern = "a l", replacement = "al") |>
    stringr::str_split(pattern = ";;") |>
    unlist()    

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' Read SQA strategic results page 6
#' 

read_sqa_strategy_results_p6 <- function(df_text) {
  ptext <- df_text[[6]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{144}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{110}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{44}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{17}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{103,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{83,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{66,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{48,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{23,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "incorporated$", replacement = "incorporated;;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "education and$", replacement = "education and;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "providers and$", replacement = "providers and;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "processes of$", replacement = "processes of;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "inspection,$", replacement = "inspection,;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "institutional$", replacement = "institutional;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "accreditation and$", replacement = "accreditation and;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "year$", replacement = "year;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "records$", replacement = "records;;") |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = " ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x[1], "", "", "", x[2]))()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = " ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x[1], x[1], x[1], x[1], x[2]))()

  indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\s{2}Number", replacement = ";;Number"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = " ") |>
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
    ) |>
    (\(x) rbind(rep("", 6), x))()

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") |>
    stringr::str_replace_all(
      pattern = "\\sSensitisation", replacement = ";;Sensitisation"
    ) |>
    stringr::str_replace_all(
      pattern = "\\sRecruitment", replacement = ";;Recruitment"
    ) |>
    stringr::str_replace_all(
      pattern = "\\sRegistration", replacement = ";;Registration"
    ) |>
    stringr::str_replace_all(pattern = "\\sSQA", replacement = ";;SQA") |>
    stringr::str_replace_all(pattern = "\\sWebsite", replacement = "; Website") |>
    stringr::str_replace_all(pattern = ", ", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()
  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{3,}") |>
    lapply(
      FUN = stringr::str_replace_all, pattern = "\\s{2}", replacement = " "
    ) |>
    unlist() |>
    (\(x) c("", x))()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\sNumber", replacement = ";;Number"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2}\\%", replacement = ";;%") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") |>
    stringr::str_split(pattern = ";;") |>
    unlist()    

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' Read SQA strategic results page 7
#' 

read_sqa_strategy_results_p7 <- function(df_text) {
  ptext <- df_text[[7]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{143}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^\\s{41}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "^\\s{17}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{132,}", replacement = ";;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{118,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{84,}", replacement = ";;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{68,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{50,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{23,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "Prior$", replacement = "Prior;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "monitoring$", replacement = "monitoring;;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\)$", replacement = ");;;;;;;;;;") |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = " ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x[1], x[2], x[2]))()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = " ") |>
    stringr::str_replace_all(pattern = "\\sReview", replacement = ";;Review") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c(x, x[3]))()

  indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\s{2}Compliance", replacement = ";;Compliance"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2}", replacement = " ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()

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
    ) |>
    (\(x) rbind(rep("", 6), x))()

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") |>
    stringr::str_replace_all(pattern = "\\sSQA", replacement = "; SQA") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()
  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{3,}") |>
    lapply(
      FUN = stringr::str_replace_all, pattern = "\\s{2}", replacement = " "
    ) |>
    unlist() |>
    (\(x) c("", x))()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") |>
    stringr::str_replace_all(
      pattern = "\\sMonitoring", replacement = ";;Monitoring"
    ) |>
    stringr::str_replace_all(
      pattern = "\\sCompliance", replacement = ";;Compliance"
    ) |>
    stringr::str_replace_all(pattern = "\\s,", replacement = ",") |>
    stringr::str_replace_all(pattern = ", ", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' Read SQA strategic results page 8
#' 

read_sqa_strategy_results_p8 <- function(df_text) {
  ptext <- df_text[[8]] |>
    remove_extra_lines() |>
    stringr::str_replace_all(pattern = "^\\s{41}", replacement = ";;") |>
    #stringr::str_replace_all(pattern = "^\\s{17}", replacement = ";") |>
    stringr::str_replace_all(pattern = "\\s{132,}", replacement = ";;;;;;;;;;;") |>
    #stringr::str_replace_all(pattern = "\\s{118,}", replacement = ";;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{90,}", replacement = ";;;;;;;;;") |>
    #stringr::str_replace_all(pattern = "\\s{68,}", replacement = ";;;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{50,}", replacement = ";;;;;;;") |>
    stringr::str_replace_all(pattern = "\\s{24,}", replacement = ";;") |>
    stringr::str_replace_all(pattern = "\\s{2,}", replacement = ";") |>
    stringr::str_replace_all(pattern = "developed and$", replacement = "developed and;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "^;;implemented$", replacement = ";;implemented;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "media$", replacement = "media;;;;;;;;;") |>
    stringr::str_replace_all(pattern = ";;year$", replacement = ";;year;;;;;;;;;") |>
    stringr::str_replace_all(pattern = "records$", replacement = "records;;") |>
    stringr::str_replace_all(pattern = "page$", replacement = "page;;") |>
    stringr::str_split(pattern = ";")

  df <- ptext |>
    do.call(rbind, args = _) |>
    data.frame()

  key_sector_challenge <- df$X1 |>
    paste(collapse = " ") |>
    trimws() |>
    (\(x) c("", x, x, x))()

  strategic_objective <- df$X2 |>
    paste(collapse = " ") |>
    trimws() |>
    (\(x) c("", x, x, x))()

  indicator <- df$X3 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\sNumber", replacement = ";;Number"
    ) |>
    # stringr::str_replace_all(pattern = "\\sSQA", replacement = ";;SQA") |>
    # stringr::str_replace_all(pattern = "\\sNumber", replacement = ";;Number") |>
    # stringr::str_replace_all(
    #   pattern = "\\sReviewed", replacement = ";;Reviewed"
    # ) |>
    # stringr::str_replace_all(pattern = "\\sThe", replacement = ";;The") |>
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
    ) |>
    (\(x) rbind(rep("", 6), x))()

  data_sources <- df$X10 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\s{2}Survey instruments", replacement = ";;Survey instruments"
    ) |>
    stringr::str_replace_all(pattern = "\\s{2}|, ", replacement = "; ") |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()
  
  responsible <- df$X11 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{3,}") |>
    lapply(
      FUN = stringr::str_replace_all, pattern = "\\s{2}", replacement = " "
    ) |>
    unlist() |>
    (\(x) c("", x))()

  indicator_protocol <- df$X12 |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_replace_all(pattern = "\\s{3,}", replacement = ";;") |>
    stringr::str_replace_all(
      pattern = "\\sImproved", replacement = ";;Improved"
    ) |>
    stringr::str_replace_all(pattern = "\\sAnnual", replacement = "; Annual") |>
    stringr::str_replace_all(
      pattern = "\\s{2}Improved", replacement = "; Improved"
    ) |>
    stringr::str_split(pattern = ";;") |>
    unlist() |>
    (\(x) c("", x))()

  tibble::tibble(
    key_sector_challenge, strategic_objective, indicator,
    df_targets, data_sources, responsible, indicator_protocol
  )
}


#'
#' 
#' 

remove_extra_lines <- function(p) {
  p |>
    (\(x) x[!grepl(pattern = "^\\s{2,}Table", x = x)])() |>
    (\(x) x[!grepl(pattern = "^Key", x = x)])() |>
    (\(x) x[!grepl(pattern = "^Challenge", x = x)])() |>
    (\(x) x[!grepl(pattern = "^\\s{2,}\\‘[0-9]{2}", x = x)])() |>
    (\(x) x[!grepl(pattern = "^\\s{2,}The Seychelles Qualifications Authority Strategic Plan", x = x)])() |>
    (\(x) x[x != ""])()
}


#'
#' 
#' 

get_indicator_targets <- function(x) {
  x |>
    paste(collapse = " ") |>
    trimws() |>
    stringr::str_split(pattern = "\\s{2,}") |>
    unlist() |>
    stringr::str_replace_all(pattern = "NIL|Nil", replacement = "0") |>
    stringr::str_replace_all(pattern = "N\\/A", replacement = NA_character_)
}