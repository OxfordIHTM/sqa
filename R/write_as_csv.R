#'
#' Write as CSV
#' 

write_as_csv <- function(df, path) {
  write.csv(x = df, file = path, row.names = FALSE)

  path
}