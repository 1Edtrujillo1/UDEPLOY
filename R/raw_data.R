
# REGEX -------------------------------------------------------------------

#' obtain_regex
#'
#' Obtain a specific regex to apply in a vector or column
#'
#' This function allows you to obtain a regex based on your necesities with the intention that you do not focus on the regex part.
#'
#' @param pattern based on this string we obtain the regex.
#' @param return_regex choice of regex you want to obtain.
#'
#' @author Eduardo Trujillo
#'
#' @importFROM purrr map set_names pluck map_chr
#' @importFROM stringr str_glue
#'
#' @return This function return *different results* based on \code{return_regex} argument.
#' @export
#'
#' @note
#'\itemize{
#'   \item If \code{return_regex = "or"}, then \code{pattern} could be a vector of length greater or equal to 1.
#'   \item If \code{return_regex = "between_pattern"}, then \code{pattern} needs to be a vector of length 2.
#'   \item If \code{return_regex = "special_chrs"} or \code{return_regex = "regex_dates"}, then we do not need to specify \code{pattern}, since we are going to return a vector of specific regexes.
#'   \item Other case the \code{pattern} needs to have length greater or equal to 1.
#'   \item If you want to get the regex inside another regex you need to apply more than once this function, since the output is only based on \code{pattern}
#' }
#'
#' @example
#' string <- c("eds[ad", "gffs.df", "54tf", "egfgdfg.", ".sdffd", "sfsdswerwedndfv")
#' pattern1 <- "[.]"
#' pattern2 <- "\\["
#' pattern3 <- c("s", "d")
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "starts_with_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "not_starts_with_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "ends_with_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "not_ends_with_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "before_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "after_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern3,return_regex = "between_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "contains_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern2,return_regex = "contains_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "not_contains_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern1,return_regex = "extract_pattern"))
#' str_view(string = string, pattern = obtain_regex(pattern = pattern3,return_regex = "or"))
#' obtain_regex(return_regex = "special_chrs")
#' obtain_regex(return_regex = "regex_dates")
#'
obtain_regex <- function(pattern = NULL,
                         return_regex = c("starts_with_pattern", "not_starts_with_pattern",
                                          "ends_with_pattern", "not_ends_with_pattern",
                                          "before_pattern", "after_pattern", "between_pattern",
                                          "contains_pattern", "not_contains_pattern",
                                          "extract_pattern", "not_extract_pattern",
                                          "or", "special_chrs", "regex_dates")){
  list(
    starts_with_pattern = "^{pattern}.*",
    not_starts_with_pattern = "^(?!{pattern}).*",
    ends_with_pattern = ".*{pattern}$",
    not_ends_with_pattern = "^(?!.*{pattern}$).*",
    before_pattern = ".*(?={pattern})",
    after_pattern = "(?<={pattern}).*",
    between_pattern = "(?<={pattern[1]})(.*)(?={pattern[2]})",
    contains_pattern = ".*{pattern}.*",
    not_contains_pattern = "^(?!.*{pattern}.*).*",
    extract_pattern = "[{pattern}.*]+",
    not_extract_pattern = "(?!{pattern})",
    or = "(?:{str_c(pattern, collapse = '|')})"
  ) %>% map(str_glue, .envir = environment()) %>%
    append(
      list(
        special_chrs = c('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A',
                         'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                         'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N',
                         'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                         'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a',
                         'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                         'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i',
                         'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                         'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y',
                         'þ'='b', 'ÿ'='y', 'ã'='a'),

        regex_dates = c("^(?:[1-9]|0[1-9]|1[0-2])(?:\\/|\\-).{1,2}(?:\\/|\\-).{1,4}",
                        "^(?:[1-9]|0[1-9]|1[0-2])(?:\\/|\\-).{1,4}(?:\\/|\\-).{1,2}",
                        "^.{1,2}(?:\\/|\\-)(?:[1-9]|0[1-9]|1[0-2])(?:\\/|\\-).{1,4}",
                        "^.{1,4}(?:\\/|\\-)(?:[1-9]|0[1-9]|1[0-2])(?:\\/|\\-).{1,2}",
                        "^.{1,2}(?:\\/|\\-).{1,4}(?:\\/|\\-)(?:[1-9]|0[1-9]|1[0-2])",
                        "^.{1,4}(?:\\/|\\-).{1,2}(?:\\/|\\-)(?:[1-9]|0[1-9]|1[0-2])") %>%
          set_names("mdy", "myd", "dmy", "ymd", "dym", "ydm")
      )) %>%
    pluck(return_regex) %>% return()
}


# READ DATA ---------------------------------------------------------------

#' read_data
#'
#' Pull a dataset from your own directory files.
#'
#' This function allows you to pull a dataset with many types of formats.
#'
#' @param path path where is your dataset in your local directory
#' @param encoding type of code form
#' @param sheet page you want to import if you are pulling a \code{.xls, .xlsx} file
#' @param col_select vector of specific variables from your dataset that you want to use (rather than the all dataset).
#' @param ... additional arguments to use to import the dataset.
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM haven read_sas
#' @importFROM readxl read_excel
#' @importFROM purrr map2 set_names flatten map safely pluck keep
#' @importFROM stringr str_which
#'
#' @return
#' "This function allows us to _pull a clean dataset_ with great characteristics:"
#' \itemize{
#'   \item Use any of this format: \code{.sas7bdat, .txt, .csv, .rds, .xls, .xlsx}
#'   \item Use the parameter \code{col_select}, to select variables that we want from the dataset
#'   \item \code{Delete special characters}
#'   \item \code{Toupper Variables}
#'   \item \code{Toupper character and factor observations}
#'   \item \code{Assign the correct data type to each variable}
#'   \item \code{Change "" strings to N/A}
#'   \item \code{Remove whitespace from start and end of each observation}
#' }
#'
#' @export
#'
#' @example
#' \dontrun{
#' read_data(path = "path/file.sas7bdat")
#' read_data(path = "path/file.sas7bdat", col_select = c("Survived","Pclass","Name","Pclass"))
#' }
#'
read_data <- function(path, encoding = 'UTF-8',
                      sheet = 1, col_select = NULL,
                      ...){

  options(encoding = encoding, warn = -1) #warn = -1 hide warnings

  pattern = obtain_regex(pattern = c("\\.sas7bdat", "\\.txt",  "\\.csv",
                                     "\\.rds", "\\.xls", "\\.xlsx"),
                         return_regex = "ends_with_pattern")

  functs <- list(read_sas,
                 list_of_lists(no_sublists = 2, element_sublists = fread),
                 readRDS,
                 list_of_lists(no_sublists = 2, element_sublists = read_excel)) %>%
    unlist() #for the list_of_lists

  arguments_import <- list(

    map2(list_of_lists(no_sublists = 6, element_sublists = path),
         c("data_file", c(rep("file", 3), rep("path",2))),
         ~.x %>% set_names(.y)),

    list(
      list_of_lists(no_sublists = 3, element_sublists = list("encoding" = encoding)) ,
      list_of_lists(no_sublists = 3, element_sublists = NA)
    ) %>% flatten(),

    list(NA,
         list_of_lists(no_sublists = 2,
                       element_sublists = list(header = TRUE, na.strings = "NA")),
         list_of_lists(no_sublists = 3, element_sublists = NA)) %>% flatten(),


    list(list_of_lists(no_sublists = 4, element_sublists = NA),
         list_of_lists(no_sublists = 2,
                       element_sublists = list(sheet = sheet, col_names = TRUE,
                                               na = ""))) %>% flatten()
  )

  map(1:length(functs), safely(function(i){
    do.call(what = pluck(functs, i),
            args = map(arguments_import, i) %>% append(list(...)) %>%
              flatten() %>% keep(~!is.na(.x)))
  })) %>% map("result") %>% set_names(pattern) %>%
    pluck(pattern[str_which(string = path, pattern = pattern)]) %>%
    clean_df(col_select = col_select) %>% return()
}

# CLEAN DATA --------------------------------------------------------------

#' clean_df
#'
#' Clean a particular dataset
#'
#' This function allows you to clean a particular dataset and if you want, only select particular variables from it.
#'
#' @param df dataset to use for cleaning
#' @param col_select vector of specific variables from your dataset that you want to use (rather than the all dataset).
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM stringr str_replace_all str_trim
#'
#' @return
#' "This function allows us to _clean dataset_ with great characteristics:"
#' \itemize{
#'   \item Dataset is in \code{data.table} format
#'   \item Use the parameter \code{col_select}, to select variables that we want from the dataset
#'   \item \code{Delete special characters}
#'   \item \code{Toupper Variables}
#'   \item \code{Toupper character and factor observations}
#'   \item \code{Assign the correct data type to each variable}
#'   \item \code{Change "" strings to N/A}
#'   \item \code{Remove whitespace from start and end of each observation}
#' }
#'
#' @export
#'
#' @example
#' \dontrun{
#' clean_df(df = df)
#' clean_df(df = df, col_select = c("SURVIVED","PCLASS","NAME","PCLASS"))
#' }
#'
clean_df <- function(df, col_select = NULL){

  df <- df %>% as.data.table() %>%
    .[,(unique(names(df))), with = FALSE]

  if(!is.null(col_select)){
    df <- df[,unique(col_select), with = FALSE]
  }

  df <- df[,names(df):=lapply(.SD,
                              str_replace_all,
                              obtain_regex(return_regex = "special_chrs"))] %>%
    setnames(toupper(names(df))) %>%
    .[,lapply(.SD, toupper), .SDcols = names(df)] %>%
    .[,lapply(.SD, assign_data_type), .SDcols = names(df)]

  character_na <- c('\\A\\z'='N/A') #change "" strings to N/A string
  character_variables <- classes_vector(data_type = c("character", "factor"),
                                        df = df)

  df <- df[,(character_variables):= lapply(.SD, str_replace_all, character_na),
           .SDcols = character_variables] %>%
    .[,(character_variables):=lapply(.SD, str_trim),
      .SDcols = character_variables] %>%
    .[,lapply(.SD, assign_data_type), .SDcols = names(df)]
  df
}

#' assign_data_type
#'
#' Assign correct datatype to a variable
#'
#' This function allows you to assign correctly the datatype of a variable
#'
#' @param variable variable that we want to assign a correct datatype
#'
#' @author Eduardo Trujillo
#'
#' @importFROM purrr map2 map possibly set_names keep pluck flatten_chr
#' @importFROM stringr str_glue str_detect str_subset
#' @importFROM lubridate is.Date
#'
#' @return This function allows us to assign the correct data type to a variable
#' @export
#'
#' @note Useful when we want to assign the correct datatype to each variable of a dataset.
#'
#' @example
#' library(datasets)
#' library(data.table)
#'
#' assign_data_type(variable = iris$Sepal.Length) %>% class()
#'
#' df <- iris %>% as.data.table()
#'map(map(names(df),~df[,...x] %>% flatten_chr()),
#'assign_data_type) %>% set_names(names(df)) %>% map(class)
#'
assign_data_type <- function(variable){

  pattern = map2(list(c("[A-Z]+",
                        "[a-z]+",
                        str_glue("{obtain_regex(pattern = '[.]|[-]', return_regex = 'not_extract_pattern')}[:punct:]+")), #every punctuation (including ") except . or -
                      "[0-9]+[.][0-9]+",
                      c(".+\\/.+\\/.+",
                        obtain_regex(return_regex = "regex_dates")),
                      c("TRUE", "FALSE")),
                 c("or",NA, rep("or", 2)),
                 ~ ifelse(is.null(obtain_regex(pattern = ..1, return_regex = ..2)), ..1,
                          obtain_regex(pattern = ..1, return_regex = ..2))
  )
  general_values_type = c("character", "numeric", "date", "logical")

  values_type = map(variable, possibly(function(i){
    result <- map2(pattern, general_values_type,  ~ list(str_detect(string = i,
                                                                    pattern = ..1),
                                                         ..2)) %>%
      map(~ .x %>% set_names("condition", "value")) %>%
      keep(~isTRUE(pluck(.x, 1, 1))) %>% map("value") %>%
      unlist(recursive = FALSE)

    if(is.null(result)){
      if(is.Date(i)) result <- general_values_type[3]
      else if(is.logical(i)) result <- general_values_type[4]
      else result <- "integer"
    }
    result
  }, otherwise = NA)) %>% flatten_chr() %>% # ignore NA on the count
    table() %>% sort(decreasing = TRUE) #sort based on type most repeated

  check <- map(general_values_type[1:2], ~ any(names(values_type) == .x)) %>% #integer, numeric -> numeric
    set_names(general_values_type[1:2]) %>% keep(~isTRUE(.x)) %>% names() %>%
    ifelse(any(. == general_values_type[1]), general_values_type[1], .)

  if(!is.na(check)){values_type <- check}
  else{
    if(any(duplicated(values_type))){values_type <- general_values_type[1]} #if any count of the table is equal with other datatype
    else{values_type <- values_type %>% head(1) %>% names()} #datatype most repeated for all its observations
  }

  if(values_type %in% c(general_values_type[1], "integer")){
    variable_notNULL <- str_subset(string = variable, pattern = "[^N/A]+") %>%  #everything except "" or NA in the variable for the count
      table()
    if(length(variable_notNULL) == 0){values_type <- general_values_type[1]}
    else if(length(variable_notNULL) == sum(variable_notNULL >= 3)){values_type <- "factor"}
  }

  list(as.character, as.numeric, assign_date_type,
       as.logical, as.integer, as.factor) %>%
    set_names(general_values_type %>% append(c("integer", "factor"))) %>%
    pluck(values_type) %>% do.call(what = ., args = list(variable)) %>%
    return()
}

#' assign_date_type
#'
#' Assign correct datatype date to a variable
#'
#' This function allows you to assign date datatype to a variable
#'
#' @param variable variable that we want to assign date datatype
#'
#' @author Eduardo Trujillo
#'
#' @importFROM tidyr gather
#' @importFROM lubridate is.Date mdy myd dmy ymd dym ydm
#' @importFROM purrr map_dfc map pluck
#' @importFROM stringr str_detect
#'
#'
#' @return This function allows us to assign the correct data type to a variable
#' "This function allows us to assign date data type based on:"
#' \itemize{
#'   \item if a variable is date then return the variable
#'   \item In other case return a date type date checking the possibilities (Permutation 3 to 3 of date functions)to return the date with the format ISO 8601 YYYY-MM-DD
#' }
#'
#' @example
#' \dontrun{
#' assign_date_type(variable = df$date_variable) %>% class()
#' }
#'
assign_date_type <- function(variable){

  if(is.Date(variable)) variable <- variable

  else{
    variable <- as.character(variable)

    variable <- suppressMessages( #hide the message of creation of new variables ...1 ...2 ...3
      map_dfc(variable, function(i){

        condition <- map(obtain_regex(return_regex = "regex_dates"),
                         ~str_detect(string = i, pattern = .x))

        calculation <- map(list(mdy, myd, dmy, ymd, dym, ydm),
                           ~do.call(what = .x, args = list(i)))

        calculation[condition == TRUE & !is.na(calculation)] %>%
          pluck(1) %>% return()
      })
    ) %>% gather(., names(.),
                 key = "name_vars" , value = "date") %>% .[,"date"] %>%
      pluck(1)
  }
  variable
}

#' classes_vector
#'
#' Bring  dataset-variable´s names from a specific datatype
#'
#' This function allows you to brings all the variables´ names of a dataset that are in a certain data type
#'
#' @param data_type string datatype that we are interested
#' @param df dataset to use
#'
#' @author Eduardo Trujillo
#'
#' @importFROM purrr map set_names
#'
#' @return This function allows us to return vector of strings of variables that are in a certain data type
#' @export
#'
#' @example
#' library(datasets)
#' library(data.table)
#' library(purrr)
#' df <- iris %>% as.data.table()
#' classes_vector(data_type = "numeric", df = df)
#' map(c("numeric", "integer", "date", "logic",  "character", "factor"), classes_vector, df = df)
#'
classes_vector <- function(data_type, df){

  vars <- map(names(df), ~df[,class(eval(parse(text = .x)))]) %>%
    set_names(names(df))

  vars[vars == data_type] %>% names() %>% return()
}
