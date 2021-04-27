
# DESIGN REPORT CREATEOR --------------------------------------------------

#' design_report_creator
#'
#' Create a periodical or historical designed-summary or designed-grouped dataset or sparklines from a grouped dataset.
#'
#' This function allows you to return a periodical or historical designed-summary or designed-grouped dataset or sparklines for a grouped dataset from the orignal dataframe.
#'
#' @param df dataset to obtain the report
#' @param year year that we want to obtain the report
#' @param select_month name of the month of the year that we want to obtain the report
#' @param select_semester Number of the semester that we want to obtain the report 1:2
#' @param select_quarter Number of the quarter that we want to obtain the report 1:4
#' @param summary Type of report we want to obtain.
#' @param sparklines Type of design we want for our report
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map pluck set_names walk2 keep discard flatten_chr walk map2 flatten
#' @importFROM stringr str_c str_subset
#' @importFROM htmltools as.tags
#' @importFROM sparkline sparkline spk_add_deps
#' @importFROM htmlwidgets JS
#' @importFROM DT formatStyle
#' @importFROM formattable percent accounting formattable formatter color_tile color_bar style icontext
#'
#' @return
#' "This function returns *different results* based on the arguments \code{sparklines} argument".
#' \itemize{
#'   \item If \code{sparklines = TRUE}, then return sparklines of a grouped dataset.
#'   \item If \code{sparklines = FALSE}, then return the output of the function \code{report_creator} with a design for each variable
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item Since we are using the defined function \code{report_creator} the output is going to be the same, but now the report created will have design
#'   \item If \code{sparklines = TRUE}, then you need to select the option code{summary = "NOT_SUMMARY"}, since it is going to create the sparklines of the distribution of a grouped dataset obtained from the function \code{report_creator}
#'  }
#'
#' @example
#' \dontrun{
#' rm(date_info)
#'
#' *ERROR STRING MESSAGE:*
#' - Selecting multiples parameters
#' date_info <- design_report_creator(df = df, year = 2019, select_month = "OCTOBER", select_semester = 2, summary = "SUMMARY")
#' - not having date variable
#' date_info <- design_report_creator(df = df[,-"DATE"], year = 2019, select_month = "OCTOBER")
#' - Selecting a summary for sparklines
#' date_info <- design_report_creator(df = df, year = 2019, select_quarter = 4, summary = "SUMMARY", sparklines = TRUE)
#'
#' *HISTORICAL:*
#'
#' _SUMMARY_ (design dataset)
#' date_info <- design_report_creator(df = df, summary = "SUMMARY")
#'
#' _NOT SUMMARY_
#' - design dataset
#' date_info <- design_report_creator(df = df, summary = "NOT_SUMMARY")
#' - sparklines
#' date_info <- design_report_creator(df = df, summary = "NOT_SUMMARY", sparklines = TRUE)
#'
#' *PERIODICAL:*
#'
#' _SUMMARY_ (design dataset)
#' - PRESENT AND PAST INFORMATION
#' date_info <- design_report_creator(df = df, year = 2019, select_quarter = 4, summary = "SUMMARY")
#' - PRESENT INFORMATION
#' date_info <- design_report_creator(df = df, year = 2018, select_quarter = 4, summary = "SUMMARY")
#' - PAST INFORMATION
#' date_info <- design_report_creator(df = df, year = 2020, select_quarter = 4, summary = "SUMMARY")
#'
#' _NOT SUMMARY_
#' - PRESENT AND PAST INFORMATION
#' -- design dataset
#' date_info <- design_report_creator(df = df, year = 2019, select_quarter = 4, summary = "NOT_SUMMARY")
#' -- sparklines
#' date_info <- design_report_creator(df = df, year = 2019, select_quarter = 4, summary = "NOT_SUMMARY", sparklines = TRUE)
#' - PRESENT INFORMATION
#' -- design dataset
#' date_info <- design_report_creator(df = df, year = 2018, select_quarter = 4, summary = "NOT_SUMMARY")
#' -- sparklines
#' date_info <- design_report_creator(df = df, year = 2018, select_quarter = 4, summary = "NOT_SUMMARY", sparklines = TRUE)
#' - PAST INFORMATION
#' -- design dataset
#' date_info <- design_report_creator(df = df, year = 2020, select_quarter = 4, summary = "NOT_SUMMARY")
#' -- sparklines
#' date_info <- design_report_creator(df = df, year = 2020, select_quarter = 4, summary = "NOT_SUMMARY", sparklines = TRUE)
#' }
#'
design_report_creator <- function(df, year = NULL, select_month = NULL,
                                  select_semester = NULL, select_quarter = NULL,
                                  summary = c("SUMMARY", "NOT_SUMMARY"),
                                  sparklines = FALSE){
  df <- report_creator(df = copy(df),
                       year = year,
                       select_month = select_month,
                       select_semester = select_semester,
                       select_quarter = select_quarter,
                       summary = summary)
  design_df <- tryCatch({
    if(!is.character(df)){
      factor_variable <- classes_vector(data_type = "factor", df = df)
      num_int_var <- classes_vector(data_type = c("integer", "numeric"), df = df)

      if(sparklines){
        design_df <- tryCatch({
          if(summary == "NOT_SUMMARY"){
            design_df <- map(num_int_var, function(i){

              each <- df[,c(factor_variable, i), with = FALSE]

              possibilities_each <- each[, .SD[1], by = factor_variable] %>%
                .[ ,factor_variable, with = FALSE] %>%
                split(x = ., f = seq(.[, .N]))

              each <- map(possibilities_each,
                          ~ iterative_merge(dfs_list = list(each, .x),
                                            key = factor_variable)) %>%
                map(~.x[,eval(parse(text = num_int_var))]) %>%
                set_names(map(possibilities_each, ~ str_c(.x %>% unique() %>% unlist(),
                                                          collapse = ",")))
              walk2(possibilities_each, each,
                    ~ .x[,(i):=as.character(htmltools::as.tags(sparkline(.y, type = "line")))]
              )
              possibilities_each <- possibilities_each %>% rbindlist()
              possibilities_each
            }) %>% iterative_merge(key = factor_variable) %>%
              design_DT(escape = FALSE,
                        extra_options = list(paging = FALSE,
                                             fnDrawCallback = htmlwidgets::JS(
                                               'function(){HTMLWidgets.staticRender();}'), #JS code for the sparklines to allow datatable format
                                             initComplete = JS("function(settings, json) {",
                                                               "$(this.api().table().header()).css({'color': '#D3D3D3'});", #design of the names variables changing the color
                                                               "}")
                        )) %>% spk_add_deps() %>%
              formatStyle(names(df), backgroundColor = 'white')
          }
          design_df
        }, error = function(e) "Can not generate sparklines from a summary")
      }else{
        percentage_variables <- map(num_int_var, function(i){
          keep(i,
               ~ length(keep(df[,eval(parse(text = i))], ~ -1<=.x & .x<=1)) ==
                 df[,length(eval(parse(text = i)))]
          )
        }) %>% purrr::discard(~length(.x) == 0) %>% flatten_chr()

        not_percentage_variables <-  tryCatch({
          if(length(percentage_variables) != 0){
            not_percentage_variables <-
              str_subset(string = num_int_var,
                         pattern = obtain_regex(pattern = obtain_regex(pattern = percentage_variables,
                                                                       return_regex = "or"),
                                                return_regex = "not_contains_pattern"))
          }
          not_percentage_variables
        }, error = function(e) num_int_var)

        walk2(list(percentage_variables, not_percentage_variables),
              list(percent, accounting), function(i, fun){
                walk(i, ~ df[,(.x):= fun(x = eval(parse(text = .x)),
                                         format = "f")])
              })
        partition <- map(list(percentage_variables, not_percentage_variables),
                         function(i){
                           map(c( "not_contains_pattern","contains_pattern"),
                               ~ str_subset(string = i,
                                            pattern = obtain_regex(pattern = "dif",
                                                                   return_regex = .x))
                           ) %>% set_names("PRINCIPAL", "RESULT")
                         }) %>% set_names("PERCENT", "NOT_PERCENT")
        design_df <-
          design_DT(
            df =
              formattable(df,
                          list_of_lists(no_sublists = length(factor_variable),
                                        element_sublists =
                                          formatter("span",
                                                    style = ~
                                                      formattable::style(color = "grey",
                                                                         font.weight = "bold"))) %>%
                            set_names(factor_variable) %>%
                            append(
                              map2(map(c("PRINCIPAL", "RESULT"), ~ map(partition, .x)),
                                   list(map2(c("white", "lightpink"),
                                             c("orange", "lightblue"),
                                             ~ color_tile(.x, .y)),
                                        list(formatter("span",
                                                       style = x ~
                                                         formattable::style(font.weight = "bold",
                                                                            color = ifelse(x > 0,
                                                                                           "#71CA97",
                                                                                           ifelse(x < 0,
                                                                                                  "#ff7f7f", "black"))),
                                                       x ~ icontext(ifelse(x > 0 ,
                                                                           "arrow-up",
                                                                           "arrow-down"), x)),
                                             color_bar(color = "lightblue",
                                                       fun = function(x) (x-min(x))/(max(x)-min(x)))
                                        )
                                   ), function(i,j){
                                     map2(i, j,
                                          ~ list_of_lists(no_sublists = length(..1),
                                                          element_sublists = ..2) %>%
                                            set_names(..1)
                                     )
                                   }) %>% flatten() %>% flatten()
                            )
              ),
            convert = TRUE) %>%
          formatStyle(c(percentage_variables,not_percentage_variables), color = 'black',
                      fontWeight = 'bold', `text-align` = 'center')
      }
    }
    design_df
  }, error = function(e) df)
  design_df
}

# REPORT CREATION ---------------------------------------------------------

#' report_creator
#'
#' Create a periodical or historical summary or grouped dataset.
#'
#' This function allows you to return a periodical or historical summary or grouped dataset from the orignal dataframe
#'
#' @param df dataset to obtain the report
#' @param year year that we want to obtain the report
#' @param select_month name of the month of the year that we want to obtain the report
#' @param select_semester Number of the semester that we want to obtain the report 1:2
#' @param select_quarter Number of the quarter that we want to obtain the report 1:4
#' @param summary Type of report we want to obtain.
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map map_int pluck set_names keep map2 walk2
#' @importFROM stringr str_subset str_glue
#'
#' @return
#' "This function returns *different results* based on the arguments \code{summary} argument".
#' \itemize{
#'   \item If \code{summary = "SUMMARY"}, then return a historical or periodical summary.
#'   \item If \code{summary = "NOT_SUMMARY"}, then return a historical or periodical grouped dataset.
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item If you do not select periodicity (any argument: \code{year, select_month, select_semester, select_quarter} ) then you are bringing a historical information.
#'   \item If you select more than one periodical arguments (\code{select_month, select_semester, select_quarter} ), it will return a string message, since the report is according to a specific period of time.
#'   \item If you do not have a date variable in your dataset, then will return a string message.
#'   \item A summary means the comparison between a selected period and the previous one, and if there is not previous period or actual period then return the summary of the period that has information.
#'   \item If there is previous and actual periocity and we select to return a grouped dataset, then will return the present grouped dataset, and if there is not previous period or actual period then return the grouped dataset of the period that has information.
#'  }
#'
#' @example
#' \dontrun{
#' rm(date_info)
#'
#' *ERROR STRING MESSAGE:*
#' - Selecting multiples parameters
#' date_info <- report_creator(df = df, year = 2019, select_month = "OCTOBER", select_semester = 2, summary = "SUMMARY")
#' - not having date variable
#' date_info <- report_creator(df = df[,-"DATE"], year = 2019, select_month = "OCTOBER")
#'
#' *HISTORICAL:*
#'
#' _SUMMARY_
#' date_info <- report_creator(df = df, summary = "SUMMARY")
#'
#' _NOT SUMMARY_
#' date_info <- report_creator(df = df, summary = "NOT_SUMMARY")
#'
#' *PERIODICAL:*
#'
#' _SUMMARY_
#' - PRESENT AND PAST INFORMATION
#' date_info <- report_creator(df = df, year = 2019, select_quarter = 4, summary = "SUMMARY")
#' - PRESENT INFORMATION
#' date_info <- report_creator(df = df, year = 2018, select_quarter = 4, summary = "SUMMARY")
#' - PAST INFORMATION
#' date_info <- report_creator(df = df, year = 2020, select_quarter = 4, summary = "SUMMARY")
#'
#' _NOT SUMMARY_
#' - PRESENT AND PAST INFORMATION
#' date_info <- report_creator(df = df, year = 2019, select_quarter = 4, summary = "NOT_SUMMARY")
#' - PRESENT INFORMATION
#' date_info <- report_creator(df = df, year = 2018, select_quarter = 4, summary = "NOT_SUMMARY")
#' - PAST INFORMATION
#' date_info <- report_creator(df = df, year = 2020, select_quarter = 4, summary = "NOT_SUMMARY")
#' }
#'
report_creator <- function(df, year = NULL, select_month = NULL,
                           select_semester = NULL, select_quarter = NULL,
                           summary = c("SUMMARY", "NOT_SUMMARY")){

  date_variable <- classes_vector(data_type = "Date", df = df)
  date_info <- tryCatch({
    if(length(date_variable) != 0){
      if(!is.null(year)){
        enter_condition <- map(list(str_subset(string = formalArgs(report_creator),
                                               pattern = obtain_regex(pattern = c("year", "select"),
                                                                      return_regex = "or")),
                                    str_subset(string = formalArgs(report_creator),
                                               pattern = "select")),
                               function(i){
                                 map_int(i, ~ !is.null(eval(parse(text = .x))))
                               })
        # date information
        date_info <- tryCatch({
          if(pluck(enter_condition, 1) %>% sum() <= 2){
            select_arguments <- str_subset(string = formalArgs(report_creator),
                                           pattern = "select")
            select_argument <- tryCatch({
              if(pluck(enter_condition, 2) %>% sum() == 1){
                select_argument <- map(select_arguments ,~ eval(parse(text = .x))) %>%
                  set_names(select_arguments) %>% keep(~!is.null(.x)) %>% names()
              }
              select_argument
            }, error = function(e) "year")

            date_info <- map(list(NULL, select_month), function(i){
              map(c(FALSE, TRUE),
                  ~ year_month(df = df,
                               select_year = year,
                               select_month = i,
                               previous = .x)) %>%
                set_names("PRESENT", "PAST")
            }) %>%
              append(
                map2(list(select_semester, NULL),
                     list(NULL, select_quarter), function(semester, quarter){
                       map(c(FALSE, TRUE),
                           ~ semester_quarter(df = df,
                                              year = year,
                                              semester = semester,
                                              quarter = quarter,
                                              previous = .x)) %>%
                         set_names("PRESENT", "PAST")
                     })
              ) %>% set_names("year", select_arguments) %>%
              pluck(select_argument)
          }
          date_info
        }, error = function(e) "Select only a Month or Semester or Quarter")
      }else{
        date_info <- df[,eval(parse(text = date_variable))]
      }
      date_info
    }
    date_info
  }, error = function(e) "You do not have any Date variable in your dataset")

  factor_variable <- classes_vector(data_type = "factor", df = df)
  num_int_var <- classes_vector(data_type = c("integer", "numeric"),  df = df)

  final_report <- tryCatch({
    if(!is.character(date_info)){
      report_info <- function(date_info){
        args <- str_subset(string = formalArgs(report_NumInVar),
                           pattern = obtain_regex(pattern = "day",
                                                  return_regex = "not_contains_pattern")
        )
        do.call(what = report_NumInVar,
                args = map(args, ~eval(parse(text = .x))) %>%
                  set_names(args) %>%
                  append(list(day = date_info)))
      }
      report_info <- map(list(function(date_info){map(date_info, ~ report_info(.x))},
                              report_info),
                         ~ do.call(what = ..1, args = list(date_info))
      ) %>% set_names("PERIODICAL", "HISTORICAL")

      final_report <- tryCatch({
        if(length(date_info) != df[,.N]){
          present_past_report <- pluck(report_info, "PERIODICAL") %>% map(summary)

          final_report <- tryCatch({
            if(any(map(present_past_report, ~.x[,.N] !=0))){
              if(summary == "SUMMARY"){
                identify_vars <- str_subset(string = names(present_past_report %>%
                                                             keep(~.x[,.N]!=0) %>% pluck(1)),
                                            pattern = obtain_regex(pattern = factor_variable,
                                                                   return_regex = "not_contains_pattern"))

                present_past_report_merged <- present_past_report %>%
                  iterative_merge(key = factor_variable, suffixes = c(".present", ".past"))

                final_report <- tryCatch({
                  if(present_past_report_merged[,.N] != 0){

                    walk2(str_glue("{identify_vars}.dif"),
                          map(identify_vars, ~ str_subset(string = names(present_past_report_merged),
                                                          pattern = .x) %>% set_names("present", "past")),
                          ~ present_past_report_merged[,(.x):=(eval(parse(text = .y["present"]))-
                                                                 eval(parse(text = .y["past"])))]
                    )
                    present_past_report_merged <-
                      present_past_report_merged[,
                                                 str_subset(string = names(present_past_report_merged),
                                                            pattern = obtain_regex(pattern = ".past",
                                                                                   return_regex = "not_contains_pattern"))
                                                 , with = FALSE] %>%
                      setnames(old = str_subset(string = names(.),
                                                pattern = obtain_regex(pattern = ".present",
                                                                       return_regex = "contains_pattern")),
                               new = identify_vars)
                    final_report <- present_past_report_merged
                  }
                  final_report
                }, error = function(e){
                  result_correct <- present_past_report %>% keep(~ copy(.x)[,.N]!=0)
                  final_report <- result_correct %>% pluck(1) %>%
                    setnames(old = identify_vars, new =
                               str_glue("{identify_vars}.{names(result_correct)}"))
                  final_report
                })
              }else if(summary == "NOT_SUMMARY"){
                final_report <- tryCatch({
                  if(all(map(present_past_report, ~.x[,.N] !=0))){
                    final_report <- pluck(present_past_report, "PRESENT") %>%
                      setnames(old = names(.), new = str_glue("{names(.)}.PRESENT"))
                  }
                  final_report
                }, error = function(e){
                  result_correct <- present_past_report %>% keep(~ copy(.x)[,.N]!=0)
                  final_report <- result_correct %>% pluck(1) %>%
                    setnames(old = names(.), new =
                               str_glue("{names(.)}.{names(result_correct)}"))
                  final_report
                })
              }
            }
            final_report
          }, error = function(e) "You do not have any information in that period")
        }
        final_report
      }, error = function(e) pluck(report_info, "HISTORICAL", summary)%>%
        setnames(old = names(.), new = str_glue("{names(.)}.{summary}"))
      )
    }
    final_report
  }, error = function(e) date_info)
  final_report
}


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
             old = classes_vector(data_type = c("integer", "numeric"),  df = .),
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
