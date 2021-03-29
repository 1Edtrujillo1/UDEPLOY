
# REPORT CREATION ---------------------------------------------------------


#' report_NumInVar
#'
#' Create a summary and a grouped dataset from the orignal dataframe.
#'
#' This function allows you to obtain a summary and a grouped dataset from the orignal dataframe
#'
#' @param day vector of filter dates on the \code{date_variable}
#' @param df dataset to obtain the report
#' @param date_variable string that represent variable´s name of data type date
#' @param num_int_var string that represent variable´s name of data type numeric or integer
#' @param factor_variable string that represent variable´s name of data type factor
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map set_names
#' @importFROM stringr str_subset str_c
#'
#' @return This function return a list with two datasets selecting only specific \code{day}´s from the \code{date_variable}, where is going to be a summary of all the numeric and integer variables and other containing a grouped dataset
#'
#' @note
#'\itemize{
#'   \item If the \code{day} is all the observations from \code{date_variable}, this means that we are creating historical summary and historical grouped dataset.
#'   \item The variables created on the summary are N:= number of observations per group, SUM:= total of observations per group, PERCENT:= participation percentage of each group from each numeric and integer variable
#'  }
#'
#' @example
#' \dontrun{
#' report_NumInVar(day = date_info,
#'                 df = df,
#'                 date_variable = date_variable,
#'                 num_int_var = num_int_var,
#'                 factor_variable = factor_variable)
#'
#' }
#'
report_NumInVar <- function(day, df, date_variable, num_int_var, factor_variable){
  each_numeric <- map(num_int_var, function(i){
    each <- copy(df)[eval(parse(text = date_variable)) %in% day,
                     c(factor_variable, i), with = FALSE]
    list(NOT_SUMMARY = each,
         SUMMARY = each[,.(.N,
                           sum(eval(parse(text = i)), na.rm = TRUE),
                           sum(eval(parse(text = i)), na.rm = TRUE)/
                             sum(each[,eval(parse(text = i))], na.rm = TRUE)),
                        by = factor_variable] %>%
           setnames(
             old = str_subset(string = names(.),
                              pattern = obtain_regex(pattern = factor_variable,
                                                     return_regex = "not_contains_pattern")),
             new = str_c(i, c('N','SUM','PERCENT'), sep = ".")
           )
    )
  }) %>% set_names(num_int_var)

  list(
    NOT_SUMMARY = map(each_numeric, "NOT_SUMMARY") %>% unname() %>%
      cbind.data.frame() %>% data.table() %>%
      .[,unique(names(.)), with = FALSE],
    SUMMARY = map(each_numeric, "SUMMARY") %>% iterative_merge(key = factor_variable)
  )
}

# DATES INFORMATION -------------------------------------------------------

#' year_month
#'
#' Return a vector of possible dates of a years, or dates of a month in a year.
#'
#' This function allows you to return a vector of dates from a year (or the previous one) OR dates of a month of a year (or the previous month)
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
#'   \item If \code{previous = FALSE} and  \code{select_year} return a vector of dates of that year in the date variable.
#'   \item If \code{previous = TRUE} and  \code{select_year} return a vector of dates of the previous year in the date variable.
#'   \item If \code{previous = FALSE} and  \code{select_month} return a vector of dates of that month of the year \code{select_year} in the date variable.
#'   \item If \code{previous = TRUE} and  \code{select_month} return a vector of dates of the previous month year \code{select_year} in the date variable.
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
                               ~ date_variable[year(date_variable) == .x]) %>%
              set_names(TRUE, FALSE) %>% pluck(shQuote(previous, type = "cmd2"))
            if(is.null(select_year)){select_year <- "There is no information on the previous year."}
            result <- select_year
          }else{
            filter_year <- date_variable[year(date_variable) == as.integer(select_year)]

            filter_month <- map(list(months[months[select_month]-1], months[select_month]),
                                safely(~filter_year[month(filter_year) == .x])) %>%
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

#' semester_quarter
#'
#' Return a vector of possible dates in a semester or quarter in a year.
#'
#' This function allows you to return a vector of dates from a semester or quarter of a year (or the previous year)
#'
#' @param df dataset to obtain the dates
#' @param year year that we want to obtain the vector
#' @param previous If we want to obtain the semester or quarter from the previous year
#' @param semester Number of the semester that we want to obtain the vector 1:2
#' @param quarter Number of the quarter that we want to obtain the vector 1:4
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM lubridate year
#' @importFROM purrr map set_names pluck map2 detect_index keep
#' @importFROM stringr str_to_upper str_glue
#'
#' @return
#' "This function returns *different results* based on the arguments \code{select_year}, \code{select_month} & \code{previous}".
#' \itemize{
#'   \item If \code{previous = FALSE} and \code{semester} return a vector of dates of that semester of the year \code{select_year} in the date variable.
#'   \item If \code{previous = TRUE} and  \code{semester} return a vector of dates of that semester of the previous year \code{select_year} in the date variable.
#'   \item If \code{previous = FALSE} and \code{quarter} return a vector of dates of that quarter of the year \code{select_year} in the date variable.
#'   \item If \code{previous = TRUE} and  \code{quarter} return a vector of dates of that quarter of the previous year \code{select_year} in the date variable.
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item If \code{semester} and \code{quarter} are selected both, then return a string message.
#'   \item If there is something wrong with the \code{year} and/or \code{previous} option defined in the firm, then return a string message.
#'   \item If there is no dates in the \code{semester} or \code{quarter} defined, then return a string message.
#'  }
#'
#' @example
#' \dontrun{
#' *select semester 2 actual year*
#' semester_quarter(df = df, year = 2018, semester = 2)
#' *select semester 2 of the previous year (2017)*
#' semester_quarter(df = df, year = 2018, semester = 2, previous = TRUE)
#'
#' *select quarter 4 actual year*
#' semester_quarter(df = df, year = 2018, quarter = 4)
#' *select quarter 4 of the previous year (2017)*
#' semester_quarter(df = df, year = 2018, quarter = 4, previous = TRUE)
#'
#' *List of the 4th. quarter in many years year*
#' map(c(2018,2019,2020), ~semester_quarter(df = df, year = .x, quarter = 4))
#' }
#'
semester_quarter <- function(df, year, previous = FALSE,
                             semester = NULL, quarter = NULL){
  tryCatch({
    if(isFALSE(!is.null(semester) & !is.null(quarter))){

      date_variable <- map(c(TRUE, FALSE),
                           ~ year_month(df = df, select_year = year, previous = .x)) %>%
        set_names(TRUE, FALSE) %>% pluck(shQuote(previous, type = "cmd2"))

      result <- tryCatch({
        if(!is.character(date_variable)){

          date_variable <- date_variable %>% year() %>% unique()

          months <- 1:12 %>% set_names(format(x = ISOdate(year = Sys.Date() %>% year(),
                                                          month = 1:12,
                                                          day = 1),
                                              format = "%B") %>% str_to_upper())
          result <- map2(list(list(1:6, 7:12),
                              list(1:3, 4:6, 7:9, 10:12)),
                         list(1:2, 1:4),
                         ~map(.x, function(i){
                           map(months[i], ~ year_month(df = df,
                                                       select_year = date_variable,
                                                       select_month = ..1,
                                                       previous = FALSE))
                         }) %>% set_names(.y)
          ) %>% set_names("semester", "quarter")

          filter_parameter <- list(semester,quarter) %>%
            set_names("semester","quarter")
          filter_parameter <- filter_parameter[detect_index(filter_parameter,
                                                            ~!is.null(.x))]

          result <- result %>% pluck(names(filter_parameter)) %>%
            pluck(unlist(filter_parameter)) %>% keep(~!is.character(.x)) %>%
            do.call(what = "c", args = .) %>% unname()

          if(is.null(result)){result <- str_glue("No dates in the year {date_variable}")}
        }
        result
      }, error = function(e) date_variable)
    }
    result
  }, error = function(e) "Select only a semester or quarter")
}
