globalVariables(c("STATE","MONTH","year"), package = "FSH")

#' Read a fars data set
#'
#' Read the named fars data set and return a tibble.
#' Returns an error if the specified fars data set is not found.
#'
#' @param filename A character string with the name of the fars
#'    data set file. May include path information.
#'
#' @return A tibble
#'
#' @importFrom readr read_csv
#'
#' @seealso Documentation for tibble package
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make a fars data file name from a year
#'
#' Given a year, returns a string in the format of a fars data file
#' name for that year
#'
#' @param  year The four-digit year for which data is desired
#'
#' @return character string of fars file name for given year
#'
#' @note Does not check for valid year or existence of data file
#'
#' @examples
#'  \dontrun{make_filename(2013)}
#'  \dontrun{make_filename("2013")}
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Generate tibbles to count fatal accidents by month and year
#'
#' For each year in the input list, reads the fars data file for that year, and returns a
#' tibble with one row per fatal accident. The only data in the row is the year and month of the
#' accident.  The tibbles are only useful for later counting by month and year.
#' Returns a NULL list element if a year in the list is not found in the local data,
#' and issues a warning.
#'
#' @param years A list of four-digit years
#'
#' @return A list of tibbles, one list element for each year in years. Each tibble has one row per fatal accident,
#'          containing the month and year of the accident
#'
#' @import dplyr
#'
#' @examples
#'    \dontrun{fars_read_years(list(2012))}
#'    \dontrun{fars_read_years(list("2012"))}
#'    \dontrun{fars_read_years(list(2012:2014))}
#'
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


#' Create a tibble with number of fatal accidents per month and year
#'
#' The input is a list of four digit years. The output is a tibble with one row per month,
#' and one column per year of the number of fatal accidents. If any of the input values
#' for year cannot be interpreted by \code{as.integer}, or if there is not a data
#' file for a year, a message is printed warning of an invalid year. Valid years are
#' processed
#'
#' @param years A list of four-digit years
#'
#' @return tibble with number of fatal accidents in a tabular format
#'
#' @examples
#'  \dontrun{fars_summarize_years(list(2012))}
#'  \dontrun{fars_summarize_years(list("2012","2014"))}
#'  \dontrun{fars_summarize_years(list(2012:2014))}

#' @import dplyr
#' @importFrom tidyr spread
#'
#' @export
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Show the location of fatal accidents
#'
#' Given a state number and a four digit year, a map is generated with
#' one data marker at the location of each fatal accident. If the state number is
#' invalid, or if no records are found for that state, an error is returned
#'
#' @param state.num A state number
#' @param year A four digit year
#'
#' @note If there are multiple accidents at a location, there will be only
#'     one data point at that location on the map
#'
#' @examples
#'   \dontrun{fars_map_state(1,2014)}
#'   \dontrun{fars_map_state("3",2012)}
#'   \dontrun{fars_map_state(3, "2012")}
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
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
