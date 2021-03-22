#' year_month
#'
#' Return a vector of possible years, or possible months in a year.
#'
#' This function allows you to return a vector of observations from a year (or the previous one) OR observations of a month of a year (or the previous month)
#'
#' @param df dataset to obtain the dates
#' @param select_year year that we want to obtain the vector
#' @param select_month name of the month of the year that we want to obtain the vector
#' @param previous If we want to obtain the previous year or month
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM lubridate year month
#' @importFROM purrr set_names map pluck safely
#' @importFROM stringr str_to_upper str_glue
#'
#' @return
#' "This function returns *different results* based on the arguments \code{select_year}, \code{select_month} & \code{previous}".
#' \itemize{
#'   \item If \code{previous = FALSE} and  \code{select_year} return a vector of observations of that year in the date variable.
#'   \item If \code{previous = TRUE} and  \code{select_year} return a vector of observations of the previous year in the date variable.
#'   \item If \code{previous = FALSE} and  \code{select_month} return a vector of observations of that month of the year \code{select_year} in the date variable.
#'   \item If \code{previous = TRUE} and  \code{select_month} return a vector of observations of the previous month year \code{select_year} in the date variable.
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item If the \code{df} does not have a date variable, then return a string message.
#'   \item If the selected \code{select_year} is not included in the observations of the date variable from \code{df} then return a string message.
#'   \item If \code{previous = TRUE} & \code{select_month = "JANUARY"} then will return a string message, since it returns the observations of the previous of the exact year \code{select_year} ( _not the previous one_ ).
#'   \item If \code{previous = TRUE} and there is no previous \code{select_year} or \code{select_month}, then will return a string message.
#'  }
#'
#' @example
#' \dontrun{
#' *select actual year*
#' year_month(df = df, select_year = 2018)
#' *select previous year (2017)*
#' year_month(df = df, select_year = 2018, previous = TRUE)
#'
#' *select actual month of a specif year*
#' year_month(df = df, select_year = 2018, select_month = "NOVEMBER")
#' *select previous month of a specif year (OCTOBER from 2018)*
#' year_month(df = df, select_year = 2018, select_month = "NOVEMBER", previous = TRUE)
#'
#' *List of all months in a year*
#' map(format(x = ISOdate(year = Sys.Date() %>% year(),
#'                        month = 1:12,
#'                        day = 1),
#'            format = "%B") %>% str_to_upper(), ~ year_month(df = df,
#'                                                            select_year =2020,
#'                                                            select_month = .x,previous = FALSE))
#'
#' }
#'
year_month <- function(df, select_year, select_month = NULL, previous = FALSE){

  date_variable <- classes_vector(data_type = "Date", df = df)
  tryCatch({
    if(length(date_variable) != 0){
      date_variable <- df[,eval(parse(text = date_variable))]
      result <- tryCatch({
        if(select_year %in% year(date_variable)){

          months <- 1:12 %>% set_names(format(x = ISOdate(year = Sys.Date() %>% year(),
                                                          month = 1:12,
                                                          day = 1),
                                              format = "%B") %>% str_to_upper())
          if(is.null(select_month)){
            select_year <- map(list((as.integer(select_year)-1),as.integer(select_year)),
                               ~ year(date_variable)[year(date_variable) == .x]) %>%
              set_names(TRUE, FALSE) %>% pluck(shQuote(previous, type = "cmd2"))
            if(is.null(select_year)){select_year <- "There is no information on the previous year."}
            result <- select_year
          }else{
            filter_year <- date_variable[year(date_variable) == as.integer(select_year)]

            filter_month <- map(list(months[months[select_month]-1], months[select_month]),
                                safely(~month(filter_year)[month(filter_year) == .x])) %>%
              map("result") %>%
              set_names(TRUE, FALSE)

            if(is.null(filter_month[[1]])){
              if(select_month == "JANUARY"){filter_month[[1]] <- "There is no month before January"
              }else{filter_month[[1]] <- "There is no information on the previous month"}
            }
            if(is.null(filter_month[[2]])){filter_month[[2]] <- str_glue("There is no information on {select_month}")}

            result <- filter_month %>% pluck(shQuote(previous, type = "cmd2"))
          }
        }
        result
      }, error = function(e) "Define a correct year")
    }
    result
  }, error = function(e) "You do not have any Date variable in your dataset")
}
