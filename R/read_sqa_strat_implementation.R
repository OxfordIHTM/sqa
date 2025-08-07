#'
#' Read SQA strategic implementation plan
#' 

read_sqa_strategy_implementation <- function(pdf) {
  df_text <- pdftools::pdf_text(pdf) |>
    (\(x) x[32:49])() |>
    stringr::str_split(pattern = "\n")

  p1 <- read_sqa_strategy_implementation_p1(df_text)
  p2 <- read_sqa_strategy_implementation_p2(df_text)
  p3 <- read_sqa_strategy_implementation_p3(df_text)
  p4 <- read_sqa_strategy_implementation_p4(df_text)
  p5 <- read_sqa_strategy_implementation_p5(df_text)
  p6 <- read_sqa_strategy_implementation_p6(df_text)
  p7 <- read_sqa_strategy_implementation_p7(df_text)
  p8 <- read_sqa_strategy_implementation_p8(df_text)
  p9 <- read_sqa_strategy_implementation_p9(df_text)
  p10 <- read_sqa_strategy_implementation_p10(df_text)
  p11 <- read_sqa_strategy_implementation_p11(df_text)
  p12 <- read_sqa_strategy_implementation_p12(df_text)
  p13 <- read_sqa_strategy_implementation_p13(df_text)
  p14 <- read_sqa_strategy_implementation_p14(df_text)
  p15 <- read_sqa_strategy_implementation_p15(df_text)
  p16 <- read_sqa_strategy_implementation_p16(df_text)
  p17 <- read_sqa_strategy_implementation_p17(df_text)
  p18 <- read_sqa_strategy_implementation_p18(df_text)

  p1p2 <- rbind(p1, p2)

  rox <- rbind(p1p2[7, ], p3[1, ]) |>
    lapply(FUN = paste, collapse = " ") |>
    lapply(FUN = trimws) |>
    dplyr::bind_cols()

  p1p2p3 <- rbind(p1p2, rox, p3[2:nrow(p3), ])

  p1p2p3p4 <- rbind(p1p2p3, p4)

  rox <- rbind(p1p2p3p4[17, ], p5[1, ]) |>
    lapply(FUN = paste, collapse = " ") |>
    lapply(FUN = trimws) |>
    dplyr::bind_cols()

  p1p2p3p4p5 <- rbind(p1p2p3p4[1:16, ], rox, p5[2:nrow(p5), ])

  rox <- rbind(p1p2p3p4p5[19, ], p6[1, ]) |>
    lapply(FUN = paste, collapse = " ") |>
    lapply(FUN = trimws) |>
    dplyr::bind_cols()

  p1p2p3p4p5p6 <- rbind(p1p2p3p4p5[1:18, ], rox, p6[2:nrow(p6), ])

  rox <- rbind(p1p2p3p4p5p6[24, ], p7[1, ]) |>
    lapply(FUN = paste, collapse = " ") |>
    lapply(FUN = trimws) |>
    dplyr::bind_cols()

  p1p2p3p4p5p6p7 <- rbind(p1p2p3p4p5p6[1:23, ], rox, p7[2:nrow(p7), ])

  df <- rbind(p1p2p3p4p5p6p7, p8, p9, p10, p11, p12, p13, p14, p15)

  rox <- tibble::tibble(
    outcome = paste(df$outcome[59], p16$outcome[1], collapse = "; "),
    strategy_code = paste(df$strategy_code[59], p16$strategy_code[1], collapse = ""),
    strategy = paste(df$strategy_code[59], p16$strategy[1], collapse = " "),
    performance_indicator = paste(
      df$performance_indicator[59], p16$performance_indicator, collapse = ""
    ),
    `2022` = paste(df$`2022`[59], p16$`2022`[1], collapse = ""),
    `2023` = paste(df$`2023`[59], p16$`2023`[1], collapse = ""),
    `2024` = paste(df$`2024`[59], p16$`2024`[1], collapse = ""),
    `2025` = paste(df$`2025`[59], p16$`2025`[1], collapse = ""),
    `2026` = paste(df$`2026`[59], p16$`2026`[1], collapse = ""),
    responsible = paste(df$responsible[59], p16$responsible[1], collapse = ""),
    data_source = paste(df$data_source[59], p16$data_source[1], collapse = "")
  )

  df <- rbind(df[1:58, ], rox, p16[2:nrow(p16), ])

  rox <- rbind(p17[4, ], p18) |>
    lapply(FUN = paste, collapse = " ") |>
    lapply(FUN = trimws) |>
    dplyr::bind_cols() 

  df <- rbind(df, p17[1:3, ], rox)

  df
}