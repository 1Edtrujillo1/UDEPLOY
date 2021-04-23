
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

  check <- map(general_values_type[1:3], ~ any(names(values_type) == .x)) %>% #integer, numeric -> numeric
    set_names(general_values_type[1:3]) %>% keep(~isTRUE(.x)) %>% names()

  check <- ifelse(any(check == general_values_type[3]),
                  general_values_type[3],
                  ifelse(any(check == general_values_type[1]),
                         general_values_type[1],
                         check))

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
#' @return "This function allows us to assign date data type to a variable based on:"
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

  vars[vars %in% data_type] %>% names() %>% return()
}

#' firstvariables_df_datatype
#'
#' Check if the first variables have an specific data type and return the names of that variables
#'
#' This function allows you to check if the first or the first two or the first three variables have the same data type and return the names of that variables.
#'
#' @param df dataset to check.
#' @param data_type string datatype that we are interested.
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM lubridate is.Date
#' @importFROM purrr set_names pluck map every flatten_lgl keep map_int
#' @importFROM stringr str_which str_glue
#'
#' @return This function allows us to return:"
#' \itemize{
#'   \item Names of the first or first, second or first, second and third variables that have the same data type.
#'   \item In other case return a message indicate that the first variables do not have that data type.
#' }
#'
#' @export
#'
#' @example
#' \dontrun{
#' firstvariables_df_datatype(df = df, data_type = "factor")
#' firstvariables_df_datatype(df = df, data_type = "integer")
#' }
#'
firstvariables_df_datatype <- function(df,
                                       data_type = c("character", "numeric", "integer",
                                                     "date", "numeric", "factor")){

  datatype = list(is.character, is.numeric, is.integer,
                  is.Date, is.logical, is.factor) %>%
    set_names("character", "numeric", "integer", "date", "numeric", "factor") %>%
    pluck(data_type)

  indexes_variables <- list(three_variables = 1:3,
                            two_variables = 1:2,
                            first_variable = 1)

  vars <- map(indexes_variables, function(i){
    each <- df[,lapply(.SD, datatype), .SDcols = i]
    list(each %>% every(~isTRUE(.x)), #check
         each %>% names()) %>% return() #variables
  }) %>%
    .[str_which(string = map(., 1) %>% flatten_lgl(),
                pattern = "TRUE")] %>% map(2)

  vars <- keep(vars, ~length(.x) == map_int(vars, length) %>% max()) %>%
    pluck(1)

  if(is.null(vars)) vars <- str_glue("First variables do not have {data_type} data type")
  else vars

  vars
}

# OUTLIERS ----------------------------------------------------------------

#' delete_outliers
#'
#' Delete outliers from numeric or integer variables of a dataset
#'
#' This function allows you to delete outliers from numeric or integer variables of a dataset based on the z-score in function of the factor variables (or not)
#'
#' @param df dataset to clean outliers
#' @param factor_vars factor variables to delete outliers
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr walk2
#' @importFROM stringr str_c
#'
#'
#' @return "This function allows us to return original dataset without the outliers based on:"
#' \itemize{
#'   \item if \code{factor_vars = NULL} we delete every outlier of the numeric and/or integer variable without carrying about the factor variables
#'   \item if \code{factor_vars} is not NULL then the delete of the outliers is by every level of each factor variable selected.
#' }
#'
#' @export
#'
#' @note The z-score indicates that when a observation has a z-score +-3 then is going to be a NA (to not affect the distribution of the variable)
#'
#' @example
#' \dontrun{
#' df2 <- delete_outliers(df = copy(df), factor_vars = c("PCLASS","SEX", "PCLASS"))
#' df5 <- delete_outliers(df = copy(df))
#' }
#'
delete_outliers <- function(df, factor_vars = NULL){

  factor_variables <- classes_vector(data_type = "factor", df = df)
  factor_vars <- unique(factor_vars)
  tryCatch({
    if((all(factor_vars %in% factor_variables)) | (is.null(factor_vars))){

      num_variables <- classes_vector(data_type = c("integer", "numeric"), df = df)
      no_sd <- str_c("no_sd", num_variables, sep = "_")

      df <- df[,.SD, by = factor_vars]

      walk2(no_sd, num_variables,
            ~df[,(.x):=(eval(parse(text = .y))-mean(eval(parse(text = .y)),
                                                    na.rm = TRUE))/sd(eval(parse(text = .y)),
                                                                      na.rm = TRUE),
                by = factor_vars])
      df <- df

      for(i in 1:length(num_variables)){
        df[,(num_variables[i]):=ifelse(eval(parse(text = no_sd[i])) > -3 & eval(parse(text = no_sd[i])) < 3,
                                       eval(parse(text = num_variables[i])),
                                       NA),
           by = factor_vars]
      }
      df[,(no_sd):=NULL]

      result <- df
    }
    result
  }, error = function(e) message("At least one of the selected variables don´t have a factor datatype"))
}

#' nas_before_after_outliers
#'
#' Count the number of NAs before and after applying the function *delete_outliers*
#'
#' This function allows you to have a list where each sublist is the difference of NAs for each integer or numeric variable.
#'
#' @param df dataset that we want to clean outliers
#' @param factor_vars factor variables to delete outliers
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map set_names pluck map_dfc discard
#' @importFROM stringr str_subset
#'
#'
#' @return This function allows us to return a list which each sublist is for each of the integer and numeric variables where is going to be a dataset that counts the number of NAs before and after applying the function *delete_outliers* based on the factor variables (if there is one).
#'
#' @export
#'
#' @example
#' \dontrun{
#' nas_before_after_outliers(df = df)
#' nas_before_after_outliers(df = df, factor_vars = classes_vector(data_type = "factor", df = df)[c(2,4)])
#' }
#'
nas_before_after_outliers <- function(df, factor_vars = NULL){

  factor_vars <- unique(factor_vars)
  num_variables <- classes_vector(data_type = c("integer", "numeric"), df = df)
  df_delete_outliers <- delete_outliers(df = copy(df), factor_vars = factor_vars)

  NAS_count <- map(list(df, df_delete_outliers), function(i){
    map(num_variables, ~
          i[is.na(eval(parse(text = .x))), .N, by = factor_vars]) %>%
      set_names(num_variables)
  }) %>% set_names("NA_before_delete_outliers", "NA_after_delete_outliers")


  NAS_count <- map(1:length(pluck(NAS_count, 1)), function(i){

    each <- map(names(NAS_count), ~pluck(NAS_count, .x, i)) %>%
      set_names(names(NAS_count))

    if((dim(pluck(each, 1))[1] == 0) & (dim(pluck(each, 2))[1] != 0)){
      result <- pluck(each, "NA_after_delete_outliers") %>% copy() %>%
        .[,NAS_BEFORE:= 0] %>% setnames(old = "N", new = "NAS_AFTER") %>%
        .[,CHANGE:=NAS_AFTER]

    }else if((dim(pluck(each, 1))[1] != 0) & (dim(pluck(each, 2))[1] == 0)){
      result <- pluck(each, "NA_before_delete_outliers") %>% copy() %>%
        .[,NAS_AFTER:= 0] %>% setnames(old = "N", new = "NAS_BEFORE") %>%
        .[,CHANGE:=0]

    }else{
      if(is.null(factor_vars)){
        result <- suppressMessages(map_dfc(c("NA_before_delete_outliers",
                                             "NA_after_delete_outliers"),
                                           ~pluck(each, .x) %>% unlist() %>% unname) %>%
                                     as.data.table() %>% setnames(old = names(.),
                                                                  new = c("NAS_BEFORE",
                                                                          "NAS_AFTER")))
      }else{
        result <- iterative_merge(dfs_list = each,
                                  key = factor_vars,
                                  unify_id = TRUE,
                                  all.x = TRUE) %>%
          setnames(old = str_subset(string = names(.),
                                    pattern = obtain_regex(pattern =
                                                             c(obtain_regex(pattern = ".x",
                                                                            return_regex = "ends_with_pattern"),
                                                               obtain_regex(pattern = ".y",
                                                                            return_regex = "ends_with_pattern")),
                                                           return_regex = "or")),
                   new = c("NAS_BEFORE", "NAS_AFTER"))
      }
      result <- result[,CHANGE:=(NAS_AFTER-NAS_BEFORE)]
    }
    result %>% setcolorder(c(factor_vars, "NAS_BEFORE", "NAS_AFTER", "CHANGE")) %>%
      return()
  }) %>% set_names(num_variables) %>% purrr::discard(~dim(.x)[1] == 0)

  NAS_count
}

# KEY CREATION ------------------------------------------------------------

#' key_creation
#'
#' Create a unique key ID variable
#'
#' This function allows you to add a unique key ID to the dataset based on the factor variables (or not)
#'
#' @param df dataset to add a unique ID variable
#' @param factor_vars factor variables reference
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM lubridate today days day month year
#' @importFROM purrr map pluck
#' @importFROM stringr str_c
#'
#'
#' @return "This function allows us to return original dataset with a new KEY variable based on:"
#' \itemize{
#'   \item If \code{factor_vars = NULL} bring a KEY variable as day_month_year_*sequence from 1 to the length of the dataset*
#'   \item If \code{factor_vars} is not NULL bring a KEY variable as day_month_year_*concatenate the unique id for each level for each factor variable selected*
#' }
#'
#' @export
#'
#' @note
#' \itemize{
#'   \item The intention of this function is that in the future we use this new variable as an ID for merges.
#'   \item If for some reason the key variable is not unique then the program re-ajust it, so filter those duplicated observations and concatenate them with the corresponding index number (from 1 to the length of the duplicated values).
#'   \item If the dataset has a *date variable* then use this variable as reference but if not is going to create a new date variable reference
#'   \item today()+(1:length(vector)) is: sequence of days from today to the day that represent the last element of the vector
#'   \item today()+(1:length(vector))*days(-1) is: days(-1) invert the order, sequence this is from the day that represent the first element of the vector to yesterday "today()-1"
#'   \item sort(today()+(1:length(df1$ID_REGION))*days(-1)) is: sorted day from the lowest to the gratest
#' }
#'
#' @example
#' \dontrun{
#' r <- key_creation(df = df, factor_vars = c("PCLASS","SEX","SIBSP"))
#' r <- key_creation(df = df)
#' }
#'
key_creation <- function(df, factor_vars = NULL){

  factor_variables <- classes_vector(data_type = "factor", df = df)
  factor_vars <- unique(factor_vars)

  tryCatch({
    if((all(factor_vars %in% factor_variables)) | (is.null(factor_vars))){

      date_variable <- classes_vector(data_type = "Date", df = df)

      if(length(date_variable) == 0){date_variable <- sort(today()+(1:df[,.N])*days(-1))}
      else{date_variable <- df[,eval(parse(text = date_variable))]}

      date_variable <- map(list(day, month, year), ~
                             do.call(what = .x,
                                     args = list(date_variable)
                             )) %>%
        append(list(sep = "_")) %>% do.call(what = str_c)

      if(is.null(factor_vars)){
        result <- list(date_variable, 1:df[,.N]) %>% append(list(sep = "_")) %>%
          do.call(what = str_c)

      }else{
        result <- list(date_variable,

                       map(factor_vars, ~match(pluck(df[,..factor_vars], .x),
                                               unique(pluck(df[,..factor_vars], .x)))) %>%
                         append(list(sep = "_")) %>% do.call(what = str_c)

        ) %>% append(list(sep = "_")) %>% do.call(what = str_c)
      }

      if(length(result) == length(unique(result))){ result
      }else{
        result[duplicated(result)] <- str_glue("{result[duplicated(result)]}_{1:length(result[duplicated(result)])}")
      }

      result <- copy(df)[,KEY:=result]
    }
    result
  }, error = function(e) message("At least one of the selected variables don´t have a factor datatype"))
}
