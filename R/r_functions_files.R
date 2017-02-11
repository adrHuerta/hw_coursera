#' Read csv files
#' @param filename name (character) of the csv file
#' @details if file does not exist, a warning message appears
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @return an object of tbl_df class
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
