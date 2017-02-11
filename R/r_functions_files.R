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

#' Create a file name containing a specific year
#' @param year an character or integer value that represent a year
#' @return an object of class character
#' @examples
#' make_filename("2015")
#' make_filename(2015)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read information of specific year
#' @param years an character or integer value that represent a year
#' @importFrom dplyr mutate %>% select
#' @details if the value of year is not in the data, a warning message appears
#' @return an object of tbl_df class with two variables: month and year
#' @examples
#' fars_read_years("2015")
#' fars_read_years(2015)
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Read information of specific year giving summarize information
#' @param years an character or integer value that represent a year
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @return an object of tbl_df class with two variables: year and n (count)
#' @examples
#' fars_summarize_years("2015")
#' fars_summarize_years(2015)
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Create a map of the count of accidents by giving information of year and state number
#' @param state.num an character or integer value that represent a state
#' @param year an character or integer value that represent a year
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @details if state number does not exist, a warning message appears. In addition, function does return nothing if there is not information of accidents
#' @return a plot of a map where dots represent accidents
#' @examples
#' fars_map_state(state.num = "1", years = "2015")
#' fars_map_state(state.num = 1, years = 2015)
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
