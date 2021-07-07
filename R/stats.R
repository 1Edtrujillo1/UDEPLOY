
# DESCRIPTIVE STATISTICS --------------------------------------------------

#' general_descript_stats
#'
#' Create a descriptive analysis from a numeric variable based on the levels of factor variables from a dataset.
#'
#' This function allows you to return different types descriptive statistic analysis from a numeric variable based on the levels of factor variables from a dataset.
#'
#' @param df dataset to obtain the descriptive statistical analysis
#' @param num_int_var *ONE* NUMERICAL VARIABLE from the dataset
#' @param statistical_choice Type of descriptive statistical analysis
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map set_names pluck keep
#' @importFROM stringr str_split
#'
#' @return
#' "This function returns *different results* based on the arguments \code{statistical_choice} argument".
#' \itemize{
#'   \item If \code{statistical_choice = "TABLES.FREQUENCY.BOUNDARIES"}, list where each element is going to be a _list containing the bound, bins number, bin size and the interval values_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.FREQUENCY.DESCRIPTIVE_FREQUENCY"}, list where each element is going to be the _frequency table_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.MODE.MODE_VALUE"}, list where each element is going to be an _the mode(s)_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.MODE.MODE_DESCRIPTION"}, list where each element is going to be the _interpretation of the mode(s)_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.TENDENCY.CENTRAL_TENDENCY"},list where each element is going to be a _dataset with the mode, mean min, q25, median, q75,p90,p95,p99,max, midrange_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.TENDENCY.DISPERSION_TENDENCY"},list where each element is going to be a _dataset with the variance, standard deviation, interquantil range and range_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.STATISTICAL_COEFFICIENTS.COEFFICIENTS"}, list where each element is going to be a _list with the coefficient of variation, standard error, skeweness and kurtosis_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.STATISTICAL_COEFFICIENTS.COEFFICIENTS_DESCRIPTION"}, list where each element is going to be a _list with the interpretation of the coefficient of variation, standard error, skeweness and kurtosis_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.CONDITIONAL.CONDITIONAL_FREQUENCY"}, list where each element is going to be a _dataset with the conditional probability on that specific level-combination given the join observations on other specific level-combination_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "TABLES.CONDITIONAL.CONDITIONAL_EXPENCTATION"}, list where each element is going to be a _dataset with the conditional expectancy, conditional variance & conditional standard deviation on that specific level-combination given the join observations on other specific level-combination_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "PLOTS.DESCRIPTIVE_FREQUENCY.OGIVE"}, list where each element is going to be the _ogive plot_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "PLOTS.DESCRIPTIVE_FREQUENCY.CUMMULATIVE"}, list where each element is going to be the _accumulative distribution function plot_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "PLOTS.DESCRIPTIVE_FREQUENCY.HISTOGRAM"}, list where each element is going to be the _histogram plot_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "PLOTS.CENTRAL_TENDENCY.GENERAL_BOXPLOT"}, list where each element is going to be the _box plot_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "PLOTS.CENTRAL_TENDENCY.BINS_BOXPLOT"}, list where each element is going to be the _box plot on each bin from the frequency table_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item If \code{statistical_choice = "PLOTS.ANALYZING_MEANS.STANDARD_ERROR"}, _bar plot_ comparing the *standard error* of each combination level of the factor variables for the distribution of the \code{num_int_var}.
#'   \item If \code{statistical_choice = "PLOTS.ANALYZING_MEANS.STANDARD_ERROR"}, _Polar area plot_ comparing the *mean* of each combination level of the factor variables for the distribution of the \code{num_int_var}.
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item Each output is going to be a list, where each element is the result from each combination levels of the factor variables and the whole distribution of the numeric variable.
#'   \item If there is no factor variable(s) then it is going to bring the calculation of the whole distribution of  \code{num_int_var}
#'  }
#'
#' @example
#' \dontrun{
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice = "TABLES.FREQUENCY.BOUNDARIES")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice = "TABLES.FREQUENCY.DESCRIPTIVE_FREQUENCY")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.MODE.MODE_VALUE")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.MODE.MODE_DESCRIPTION")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.TENDENCY.CENTRAL_TENDENCY")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.TENDENCY.DISPERSION_TENDENCY")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.STATISTICAL_COEFFICIENTS.COEFFICIENTS")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.STATISTICAL_COEFFICIENTS.COEFFICIENTS_DESCRIPTION")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.CONDITIONAL.CONDITIONAL_FREQUENCY")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="TABLES.CONDITIONAL.CONDITIONAL_EXPENCTATION")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="PLOTS.DESCRIPTIVE_FREQUENCY.OGIVE")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="PLOTS.DESCRIPTIVE_FREQUENCY.CUMMULATIVE")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="PLOTS.DESCRIPTIVE_FREQUENCY.HISTOGRAM")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="PLOTS.CENTRAL_TENDENCY.GENERAL_BOXPLOT")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="PLOTS.CENTRAL_TENDENCY.BINS_BOXPLOT")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="PLOTS.ANALYZING_MEANS.STANDARD_ERROR")
#' r <- general_descript_stats(df = df, num_int_var = "PASSENGERID",
#'                             statistical_choice ="PLOTS.ANALYZING_MEANS.COMPARING_MEANS")
#' }
#'
general_descript_stats <- function(df, num_int_var,
                                   statistical_choice = c("TABLES.FREQUENCY.BOUNDARIES",
                                                          "TABLES.FREQUENCY.DESCRIPTIVE_FREQUENCY",
                                                          "TABLES.MODE.MODE_VALUE",
                                                          "TABLES.MODE.MODE_DESCRIPTION",
                                                          "TABLES.TENDENCY.CENTRAL_TENDENCY",
                                                          "TABLES.TENDENCY.DISPERSION_TENDENCY",
                                                          "TABLES.STATISTICAL_COEFFICIENTS.COEFFICIENTS",
                                                          "TABLES.STATISTICAL_COEFFICIENTS.COEFFICIENTS_DESCRIPTION",
                                                          "TABLES.CONDITIONAL.CONDITIONAL_FREQUENCY",
                                                          "TABLES.CONDITIONAL.CONDITIONAL_EXPENCTATION",
                                                          "PLOTS.DESCRIPTIVE_FREQUENCY.OGIVE",
                                                          "PLOTS.DESCRIPTIVE_FREQUENCY.CUMMULATIVE",
                                                          "PLOTS.DESCRIPTIVE_FREQUENCY.HISTOGRAM",
                                                          "PLOTS.CENTRAL_TENDENCY.GENERAL_BOXPLOT",
                                                          "PLOTS.CENTRAL_TENDENCY.BINS_BOXPLOT",
                                                          "PLOTS.ANALYZING_MEANS.STANDARD_ERROR",
                                                          "PLOTS.ANALYZING_MEANS.COMPARING_MEANS")){

  # Distribution in each combinations levels of the factor variables
  dist_each_fctr_cmbn <- marg_dist(df = df, num_int_var = num_int_var)

  # FREQUENCY, CENTRAL TENDENCY, MODE, DISPERSION FREQUENCY, STATISTICAL COEFFICIENTS
  descriptive_analytics <- map(list(descr_stats, descr_stats_plots), function(i){
    map(dist_each_fctr_cmbn, ~ i(num_int_var = num_int_var, marg_dist_df = .x))
  }) %>% set_names("TABLES", "PLOTS")

  # Conditional Stats
  conditional_results <- conditional_stats(df = df, num_int_var = num_int_var)
  #append only where is possible: probability conditional to descriptive analytics
  for(each in names(conditional_results)){
    pluck(pluck(descriptive_analytics, "TABLES"), each) <-
      pluck(pluck(descriptive_analytics, "TABLES"), each) %>% append(
        pluck(conditional_results, each))
  }

  # Analyzing the mean of all levels and append to the analysis.
  pluck(pluck(descriptive_analytics, "PLOTS"), "ALL")  <-
    pluck(pluck(descriptive_analytics, "PLOTS"), "ALL") %>% append(
      analyzing_means_plots(df = df, num_int_var = num_int_var)
    )

  # Return Final Result
  string_choice <- str_split(string = statistical_choice, pattern = "[.]") %>%
    unlist()
  general_descript_stats <- pluck(descriptive_analytics, string_choice[1])

  for(i in 1:(length(string_choice)-1)){
    general_descript_stats <- map(general_descript_stats, string_choice[i+1])
  }
  general_descript_stats <- general_descript_stats %>% keep(~!is.null(.x))

  general_descript_stats
}

#' marg_dist
#'
#' Create the marginal distribution of each combination of levels from the factor variables.
#'
#' This function allows you to create the marginal distribution of a numeric variable for each combination of levels from the factor variables.
#'
#' @param df dataset to obtain the descriptive statistical analysis
#' @param num_int_var *ONE* NUMERICAL VARIABLE from the dataset
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map set_names
#' @importFROM stringr str_c
#'
#' @return List where each element is the marginal distribution of \code{num_int_var} from each combination levels of the factor variables.
#' @export
#'
#' @note
#'\itemize{
#'   \item If there is no factor variable(s) then it is going to bring the whole distribution of  \code{num_int_var}
#'   \item This function is applied to one numeric variable \code{num_int_var}
#'  }
#'
#' @example
#' \dontrun{
#' marg_dist(df = df, num_int_var = "PASSENGERID")
#' }
#'
marg_dist <- function(df, num_int_var){

  factor_variable <- classes_vector(data_type = "factor", df = df)

  dist_each_fctr_cmbn <- tryCatch({
    if(length(factor_variable) != 0){

      each <- df[,c(factor_variable, num_int_var), with = FALSE]
      possibilities_each <- each[,.SD[1], by = factor_variable] %>%
        .[,factor_variable, with = FALSE] %>%
        split(x = ., f = seq(.[, .N]))

      dist_each_fctr_cmbn <- map(possibilities_each,
                                 ~ iterative_merge(dfs_list = list(each, .x),
                                                   key = factor_variable)) %>%
        map(~.x[,eval(parse(text = num_int_var))]) %>%
        set_names(map(possibilities_each, ~ str_c(.x %>% unique() %>% unlist(),
                                                  collapse = ","))) %>%
        map(~ .x[!is.na(.x)]) %>%
        append(list(ALL = df[,eval(parse(text = num_int_var))]))
    }
    dist_each_fctr_cmbn
  }, error = function(e) list(ALL = df[,eval(parse(text = num_int_var))]))

  dist_each_fctr_cmbn
}

#' descr_stats
#'
#' Create a descriptive analysis of the marginal distribution of a numeric variable on a specific levels of combination from factor variables.
#'
#' This function allows you to return different types of descriptive statistic analysis from the marginal distribution of a numeric variable on a specific levels of combination from factor variables.
#'
#' @param num_int_var *ONE* NUMERICAL VARIABLE that represents the marginal distribution
#' @param marg_dist_df numeric or integer vector that represent a marginal distribution
#' @param marg_dist_choice Type of descriptive statistical analysis
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map_dbl map2 set_names pluck map map_int walk2 pmap pwalk discard flatten keep compact
#' @importFROM scales percent
#' @importFROM stringr str_c str_glue
#' @importFROM moments skewness kurtosis
#'
#' @return
#' "This function returns *different results* based on the arguments \code{marg_dist_choice} argument".
#' \itemize{
#'   \item If \code{marg_dist_choice = "BOUNDARIES"}, _list containing the bound, bins number, bin size and the interval values_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "DESCRIPTIVE_FREQUENCY"}, the _frequency table_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "MODE_VALUE"}, the _mode(s)_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "MODE_DESCRIPTION"}, the _interpretation of the mode(s)_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "CENTRAL_TENDENCY"}, _dataset with the mode, mean min, q25, median, q75,p90,p95,p99,max, midrange_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "DISPERSION_TENDENCY"}, _dataset with the variance, standard deviation, interquantil range and range_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "COEFFICIENTS"}, _list with the coefficient of variation, standard error, skeweness and kurtosis_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "COEFFICIENTS_DESCRIPTION"}, _list with the interpretation of the coefficient of variation, standard error, skeweness and kurtosis_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = NULL}, list where each sublist is one of the previous mentioned.
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item \code{marg_dist_df} needs to be a filter sublist from \code{marg_dist} function.
#'   \item This function is applied to one numeric variable \code{num_int_var}
#'   \item If you are not using a *shiny app*, is better to use \code{general_descript_stats} function.
#'  }
#'
#' @example
#' \dontrun{
#' distr_cmbn <- marg_dist(df = df, num_int_var = "PASSENGERID")
#' marg_dist_df <- pluck(distr_cmbn, "MALE,2")
#'
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df)
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "BOUNDARIES")
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "DESCRIPTIVE_FREQUENCY")
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "MODE_VALUE")
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "MODE_DESCRIPTION")
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "CENTRAL_TENDENCY")
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "DISPERSION_TENDENCY")
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "COEFFICIENTS")
#' descr_stats(num_int_var = "PASSENGERID",
#'             marg_dist_df = marg_dist_df,
#'             marg_dist_choice = "COEFFICIENTS_DESCRIPTION")
#' }
#'
descr_stats <- function(num_int_var, marg_dist_df, marg_dist_choice = NULL){

  # Boundaries
  bounds <- map_dbl(list(min, max), ~ .x(marg_dist_df, na.rm = TRUE))
  bins_number <- marg_dist_df %>% length() %>% sqrt() %>% round()
  bin_size <- diff(bounds)/bins_number

  First <- bounds[1]
  for(i in seq_len(bins_number)){ #seq_len modify First vector
    interval_values <- First[i] + bin_size
    First <- c(First, interval_values)
  }
  interval_values <- First
  boundaries <- list(
    bounds =
      map2(1:2, 1:0, ~ interval_values[.x:(length(interval_values)-.y)]) %>%
      set_names("LI", "LS") %>% as.data.table()) %>%
    append(list(
      bins_number = bins_number,
      bin_size = bin_size,
      interval_values = interval_values
    ))
  # Descriptive Frequency
  frequency_df <- pluck(boundaries, "bounds") %>%
    split(x = ., f = seq(.[, .N])) %>%
    map(~ marg_dist_df[interval_between(x = marg_dist_df, rng = .x)])
  frequency_df <- pluck(boundaries, "bounds") %>% copy() %>%
    .[,FREQUENCY :=
        map(1:length(frequency_df), function(i){
          check <- pluck(frequency_df,(i-1))[pluck(frequency_df,(i-1)) %in% pluck(frequency_df,i)]
          pluck(frequency_df, i) <- tryCatch({
            if(length(check) != 0){
              pluck(frequency_df, i) <- pluck(frequency_df, i)[! pluck(frequency_df, i) %in% check]
            }
            pluck(frequency_df, i)
          }, error = function(e) pluck(frequency_df, i))
        }) %>% map_int(~length(.x))
    ]
  descriptive_frequency <- tryCatch({
    if(frequency_df[,sum(FREQUENCY)] == length(marg_dist_df)){

      frequency_df[,CUM_FREQUENCY:=cumsum(FREQUENCY)]
      walk2(c("PERCENT", "CUM_PERCENT"), c("FREQUENCY", "CUM_FREQUENCY"),
            ~ frequency_df[,(.x):=scales::percent(x = (eval(parse(text = .y))/length(marg_dist_df)),
                                                  scales = 1)])
      descriptive_frequency <- frequency_df
    }
    descriptive_frequency
  }, error = function(e) str_glue("It is not possible to define a correct frequency of {num_int_var}"))

  # Descriptive Mode
  final_mode <- table(marg_dist_df)
  final_mode <- which(final_mode == max(final_mode)) %>% unique() %>% sort()
  mode_text <- str_c(final_mode, collapse = ",")

  final_modes <- tryCatch({
    if(length(final_mode) != 0){
      final_modes <- pmap(list(list(1,2,3, 4:5),
                               list("UNIMODAL", "BIMODAL", "TRIMODAL", "MULTI-MODAL"),
                               list(list_of_lists(3, function(x1, x2){x1 == x2}),
                                    function(x1, x2){x1 %in% x2}) %>% unlist(recursive = F)),
                          function(i, j ,k){
                            if(k(length(final_mode), i)) str_glue("{j}:{mode_text}")
                          }) %>% purrr::discard(~ is.null(.x)) %>%
        ifelse(length(.) != 0, ., "MULTI-MODAL: more than 5 modes") %>% pluck(1)
    }
    final_modes
  }, error = function(e) str_glue("No mode in the distributions of {num_int_var}"))

  # Central Tendency
  probs_percentile <- list(
    Q25 = 0.25,
    Q75 = 0.75,
    P90 = 0.90,
    P95 = 0.95,
    P99 = 0.99)
  func <- list(max, min, mean, median, list_of_lists(5, quantile)) %>%
    unlist(recursive = FALSE)

  func_args <- list_of_lists(no_sublists = 4,
                             element_sublists = list(na.rm = TRUE)) %>%
    append(probs_percentile %>% set_names(rep("probs", 5))) %>% flatten()
  func_args <- map(1:length(func_args), function(i) func_args[i] %>% list()) %>%
    flatten()

  df_central_tendency <- map2(func, func_args,
                              ~ do.call(what = .x,
                                        args = list(x = marg_dist_df) %>% append(.y))) %>%
    unname() %>%
    set_names("MAX", "MIN", "MEAN", "MEDIAN", "Q25", "Q75", "P90", "P95", "P99") %>%
    as.data.table() %>% .[,':='(MIDRANGE = (MAX + MIN)/2, MODE = final_modes)]

  # Measures of Dispersion
  df_dispersion_tendency <- map(list(var, sd), ~.x(marg_dist_df, na.rm = TRUE)) %>%
    set_names("VAR", "SD") %>% as.data.table()
  pwalk(list(c("Q75", "MAX"),
             c("Q25", "MIN"),
             c("IQR", "RANGE")), function(i, j, k){
               df_dispersion_tendency[,(k):=
                                        df_central_tendency[,eval(parse(text = i))-
                                                              eval(parse(text = j))]
               ]
             })

  # Statistical Coefficients
  statistical_coefficients <-
    map(list(ifelse(df_central_tendency[,MEAN] != 0,
                    (df_dispersion_tendency[,SD]/df_central_tendency[,MEAN]),
                    "The sample mean need to be different from 0."),
             (df_dispersion_tendency[,SD]/pluck(boundaries, "bins_number")),
             moments::skewness(marg_dist_df),
             moments::kurtosis(marg_dist_df)),
        ~ ifelse(is.character(.x), .x, round(x = .x, digits = 2))) %>%
    set_names("COEF_VAR", "STANDARD_ERROR", "SKEWENESS", "KURTOSIS")

  statistical_coefficients_descriptions <-
    list(COEF_VAR =
           tryCatch({
             if(isFALSE(is.character(pluck(statistical_coefficients, "COEF_VAR")))){
               coef_var <- map(list(function(x1, x2){x1<x2}, function(x1, x2){x1 >= x2}),
                               ~ .x(pluck(statistical_coefficients, "COEF_VAR"), 1)) %>%
                 set_names("Indicates that you have a low variance around the sample mean.",
                           "Indicates that you have a high variance around the sample mean.") %>%
                 keep(~isTRUE(.x)) %>% names()
             }
             coef_var
           }, error = function(e) pluck(statistical_coefficients, "COEF_VAR"))
    ) %>%
    append(list(
      STANDARD_ERROR = str_c("STANDARD_ERROR of " ,
                             pluck(statistical_coefficients, 'STANDARD_ERROR'),
                             ": Is the dispersion from the mean of each bin around the sample mean (",
                             pluck(df_central_tendency[,MEAN]),
                             ")")
    )) %>% append(
      map2(c("SKEWENESS", "KURTOSIS"),
           list(list("The tail of the sample distribution goes to the left from the mean.",
                     "Sample distribution goes around the mean is simetric.",
                     "The tail of the sample distribution goes to the right from the mean."),
                list("PLATICURTIC, this is, the sample distribution has a slow decay and wide tails.",
                     "MESOCURTIC, this is, the sample distribution has normal curve.",
                     "LEPTOCURTIC, this is, the sample distribution has a fast decay and light tails.")),
           function(i, j){
             map(list(function(x1, x2){x1<x2}, function(x1, x2){x1==x2}, function(x1, x2){x1>x2}),
                 ~ .x(pluck(statistical_coefficients, i), 0)) %>% set_names(j) %>%
               keep(~isTRUE(.x)) %>% names()
           }) %>% set_names("SKEWENESS", "KURTOSIS")
    )

  tables = list(
    FREQUENCY = list(BOUNDARIES = boundaries,
                     DESCRIPTIVE_FREQUENCY = descriptive_frequency),
    MODE =list(MODE_VALUE = final_mode,
               MODE_DESCRIPTION = final_modes),
    TENDENCY = list(CENTRAL_TENDENCY = df_central_tendency,
                    DISPERSION_TENDENCY = df_dispersion_tendency),
    STATISTICAL_COEFFICIENTS = list(COEFFICIENTS = statistical_coefficients,
                                    COEFFICIENTS_DESCRIPTION = statistical_coefficients_descriptions)
  )
  if(isFALSE(is.null(marg_dist_choice))){
    tables <- map(tables, marg_dist_choice) %>% compact() %>% pluck(1)
  }
  tables
}

#' descr_stats_plots
#'
#' Create specific descriptive plots of the marginal distribution of a numeric variable on a specific levels of combination from factor variables.
#'
#' This function allows you to return different types of descriptive statistical plots from the marginal distribution of a numeric variable on a specific levels of combination from factor variables.
#'
#' @param num_int_var *ONE* NUMERICAL VARIABLE that represents the marginal distribution
#' @param marg_dist_df numeric or integer vector that represent a marginal distribution
#' @param marg_dist_choice Type of descriptive statistical analysis
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map pmap pluck keep flatten compact
#' @importFROM ggplot2 ggplot aes geom_line geom_point labs geom_hline geom_segment geom_col geom_text geom_vline geom_boxplot geom_jitter
#' @importFROM stringr str_glue str_c
#' @importFROM Hmisc cut2
#'
#' @return
#' "This function returns *different results* based on the arguments \code{marg_dist_choice} argument".
#' \itemize{
#'   \item If \code{marg_dist_choice = "OGIVE"}, _ogive plot_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "CUMMULATIVE"}, the _accumulative distribution function plot_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "HISTOGRAM"}, the _histogram plot_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "GENERAL_BOXPLOT"}, the _box plot_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = "BINS_BOXPLOT"}, _box plot on each bin from the frequency table_ of the marginal distribution \code{marg_dist_df} of  \code{num_int_var} on a specific combination level.
#'   \item If \code{marg_dist_choice = NULL}, list where each sublist is one of the previous mentioned.
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item \code{marg_dist_df} needs to be a filter sublist from \code{marg_dist} function.
#'   \item This function is applied to one numeric variable \code{num_int_var}
#'   \item If you are not using a *shiny app*, is better to use \code{general_descript_stats} function.
#'  }
#'
#' @example
#' \dontrun{
#' distr_cmbn <- marg_dist(df = df, num_int_var = "PASSENGERID")
#' marg_dist_df <- pluck(distr_cmbn, "MALE,2")
#'
#' descr_stats_plots(num_int_var = "PASSENGERID",
#'                   marg_dist_df = marg_dist_df)
#' descr_stats_plots(num_int_var = "PASSENGERID",
#'                   marg_dist_df = marg_dist_df,
#'                   marg_dist_choice = "OGIVE")
#' descr_stats_plots(num_int_var = "PASSENGERID",
#'                   marg_dist_df = marg_dist_df,
#'                   marg_dist_choice = "CUMMULATIVE")
#' descr_stats_plots(num_int_var = "PASSENGERID",
#'                   marg_dist_df = marg_dist_df,
#'                   marg_dist_choice = "HISTOGRAM")
#' descr_stats_plots(num_int_var = "PASSENGERID",
#'                   marg_dist_df = marg_dist_df,
#'                   marg_dist_choice = "GENERAL_BOXPLOT")
#' descr_stats_plots(num_int_var = "PASSENGERID",
#'                   marg_dist_df = marg_dist_df,
#'                   marg_dist_choice = "BINS_BOXPLOT")
#' }
#'
descr_stats_plots <- function(num_int_var, marg_dist_df, marg_dist_choice = NULL){

  descriptive_frequency <- descr_stats(marg_dist_df = marg_dist_df,
                                       marg_dist_choice = "DESCRIPTIVE_FREQUENCY",
                                       num_int_var = num_int_var)
  boundaries <- descr_stats(marg_dist_df = marg_dist_df,
                            marg_dist_choice = "BOUNDARIES",
                            num_int_var = num_int_var)
  final_mode <- descr_stats(marg_dist_df = marg_dist_df,
                            marg_dist_choice = "MODE_VALUE",
                            num_int_var = num_int_var)
  df_central_tendency <- descr_stats(marg_dist_df = marg_dist_df,
                                     marg_dist_choice = "CENTRAL_TENDENCY",
                                     num_int_var = num_int_var)
  dispersion_values <- descr_stats(marg_dist_df = marg_dist_df,
                                   marg_dist_choice = "DISPERSION_TENDENCY",
                                   num_int_var = num_int_var)
  # Ogive Plot
  ogive_plot <- ggplot(descriptive_frequency, aes(x = LS, y = (CUM_FREQUENCY/length(marg_dist_df)))) +
    geom_line(color = "red") +
    geom_point(color = "blue", shape = 15, size = 2) +
    labs(title = str_glue("OGIVE of {num_int_var}"),
         x = num_int_var,
         y = "CUMULATIVE")
  ogive_plot <- design_PLOT(plot = ogive_plot)
  # Cumulative
  cumulative_plot <- ggplot(descriptive_frequency, aes(x = LS, y = CUM_FREQUENCY)) +
    geom_point(color = "blue", shape = 21,
               size = 4, fill = "#2fa4e7", alpha = 0.5) +
    map(c(0, descriptive_frequency[,max(CUM_FREQUENCY)]),
        ~ geom_hline(aes(yintercept = .x), linetype = 4, col = "red")) +
    pmap(list(descriptive_frequency[,LS],
              descriptive_frequency[2:.N, LS] %>% append(
                descriptive_frequency[.N,LS] + pluck(boundaries, "bin_size"))) %>%
           append(list_of_lists(2, descriptive_frequency[,CUM_FREQUENCY])),
         ~ geom_segment(aes(x = ..1,
                            xend = ..2,
                            y = ..3,
                            yend = ..4),
                        color = "blue")) +
    labs(title = "CUMULATIVE DISTRIBUTION FUNCTION",
         x = num_int_var,
         y = "F(x)")
  cumulative_plot <- design_PLOT(plot = cumulative_plot)
  # Histogram
  histogram <- copy(descriptive_frequency)[,XI:=LI + (pluck(boundaries, "bin_size")/2)]
  h <- ggplot(histogram, aes(x = XI, y = FREQUENCY),
              width = pluck(boundaries, "bin_size")) +
    geom_col(alpha = 0.4, color = "black",
             fill = ifelse(histogram[,XI] %in%
                             histogram[pluck(boundaries, "bounds") %>%
                                         split(x = ., f = seq(.[, .N])) %>%
                                         map(~ final_mode[interval_between(x = final_mode,
                                                                           rng = .x)]) %>%
                                         keep(~length(.x) != 0) %>%
                                         names() %>% as.integer(),
                                       XI], "pink", "red")) +

    map(final_mode, ~ geom_point(aes(x = .x, y = 0), color = "#d24cff")) +

    map(list(geom_line, geom_point), ~ .x(color = "blue")) +
    geom_text(aes(label = PERCENT), size = 3, vjust = -1) +

    pmap(list(c("MEAN", "Q25", "MEDIAN", "Q75"), c("red", rep("green", 3)), c(1, rep(6, 3))),
         ~ geom_vline(aes(xintercept = df_central_tendency[,eval(parse(text = ..1))]),
                      col = ..2, linetype = ..3)) +
    pmap(list(
      list(df_central_tendency[,Q25],
           df_central_tendency[,MEAN]-dispersion_values[,SD]) %>% list(),
      list(df_central_tendency[,Q75],
           df_central_tendency[,MEAN]+dispersion_values[,SD]) %>% list(),
      list_of_lists(2, list(0, histogram[,max(FREQUENCY)] + 1)),
      list("green", "orange") %>% list()) %>% flatten(),
      ~ geom_segment(aes(x = ..1, xend = ..2, y = ..3, yend = ..4), color = ..5)) +

    map(c("Q25", "MEDIAN", "Q75"),
        ~ geom_point(aes(x = df_central_tendency[,eval(parse(text = .x))],
                         y = 0),
                     color = "green")) +

    map(list(df_central_tendency[,MEAN]-dispersion_values[,SD],
             df_central_tendency[,MEAN],
             df_central_tendency[,MEAN]+dispersion_values[,SD]),
        ~ geom_point(aes(x = .x, y = max(FREQUENCY) + 1),
                     color = "orange")) +

    labs(title = str_glue("Histogram of {num_int_var}"),
         x = num_int_var)
  h <- design_PLOT(plot = h)
  # General Box Plot
  general_boxplot <- ggplot(data.table(marg_dist_df), aes(x = 1, y = marg_dist_df)) +
    geom_boxplot(fill = "red", col = "blue", alpha = 0.4) +
    geom_jitter(alpha = 0.2) +
    labs(title = str_glue("Boxplot of the distribution of the {num_int_var}"),
         y = num_int_var)
  general_boxplot <- design_PLOT(plot = general_boxplot)
  # Bins Box Plot
  args_cut <- list(x = marg_dist_df,
                   breaks = pluck(boundaries, "bins_number"),
                   labels = str_c(
                     pluck(boundaries, "bounds")[,LI] %>%
                       round(digits = 2),
                     pluck(boundaries, "bounds")[,LS] %>%
                       round(digits = 2),
                     sep = "-"))
  bins_boxplot <- ggplot(data.table(marg_dist_df),
                         aes(x = tryCatch({do.call(what = cut,
                                                   args = args_cut)},
                                          error = function(e)
                                            do.call(what = Hmisc::cut2,
                                                    args = args_cut)),
                             y = marg_dist_df)) +
    geom_boxplot(fill = "red", col = "blue", alpha = 0.4) +
    labs(title = str_glue("Boxplot in each Bin of {num_int_var}"),
         x = "Bins",
         y = num_int_var)
  bins_boxplot <- design_PLOT(plot = bins_boxplot)

  plots = list(
    DESCRIPTIVE_FREQUENCY = list(OGIVE = ogive_plot,
                                 CUMMULATIVE = cumulative_plot,
                                 HISTOGRAM = h),
    CENTRAL_TENDENCY = list(GENERAL_BOXPLOT = general_boxplot,
                            BINS_BOXPLOT = bins_boxplot)
  )
  if(isFALSE(is.null(marg_dist_choice))){
    plots <- map(plots, marg_dist_choice) %>% compact() %>% pluck(1)
  }
  plots
}

#' conditional_stats
#'
#' Create a list with conditional information from a numeric variable based on the levels of factor variables from a dataset.
#'
#' This function allows you to return conditional information from a numeric variable based on the levels of factor variables from a dataset.
#'
#' @param df dataset to obtain the conditional information
#' @param num_int_var *ONE* NUMERICAL VARIABLE from the dataset
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map2 map set_names pluck compact flatten keep map_chr walk walk2
#' @importFROM dplyr intersect
#' @importFROM stringr str_glue str_subset
#' @importFROM scales percent
#'
#' @return
#' "This function returns a list where each element is going to be a sublist containing".
#' \itemize{
#'   \item _dataset with the conditional probability on that specific level-combination given the join observations on other specific level-combination_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#'   \item _dataset with the conditional expectancy, conditional variance & conditional standard deviation on that specific level-combination given the join observations on other specific level-combination_ for the distribution of the \code{num_int_var} on a specific combination level of the factor variables
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item Each output is going to be a list, where each element is the result from each combination levels of the factor variables and the whole distribution of the numeric variable.
#'   \item If there is no factor variable(s) then it is going to bring an empty list, because this function needs factor variables to obtain the intersection of the observations of the \code{num_int_var} for the levels combinations
#'   \item This function is applied to one numeric variable \code{num_int_var}
#'   \item If you are not using a *shiny app*, is better to use \code{general_descript_stats} function.
#'  }
#'
#' @example
#' \dontrun{
#' conditional_stats(df = df, num_int_var = "PASSENGERID")
#' }
#'
conditional_stats <- function(df, num_int_var){

  dist_each_fctr_cmbn <- marg_dist(df = df, num_int_var = num_int_var)

  x_var_df <- map2(dist_each_fctr_cmbn,
                   map(dist_each_fctr_cmbn, ~ .x/sum(.x, na.rm = TRUE)),
                   ~ data.table(.x, .y) %>% set_names(num_int_var, "PROB"))

  possibilities_intersect <-
    map(1:length(dist_each_fctr_cmbn), function(i){
      map(1:length(dist_each_fctr_cmbn), function(j){
        if(i == j) NULL
        else{
          dplyr::intersect(pluck(dist_each_fctr_cmbn, i),
                           pluck(dist_each_fctr_cmbn, j)) %>% list() %>%
            set_names(
              str_glue("{names(dist_each_fctr_cmbn)[i]}|{names(dist_each_fctr_cmbn)[j]}"))
        }
      })
    }) %>% set_names(names(dist_each_fctr_cmbn)) %>%
    map(~ .x %>% compact() %>% flatten() %>% keep(~length(.x) != 0)) %>%
    keep(~length(.x) != 0)

  conditional_results <- tryCatch({
    if(isFALSE(is.null(possibilities_intersect))){
      conditional_results <- map(names(possibilities_intersect), function(i){

        cond_expc_vars <- map_chr(list("N", "N_CUAD"), ~str_glue("COND_MEA{.x}"))

        each <- map(pluck(possibilities_intersect, i),
                    ~ pluck(x_var_df, i)[eval(parse(text = num_int_var)) %in% .x]) %>%
          walk(~.x[,PROB_COND := PROB/sum(PROB, na.rm = TRUE)]) %>%
          walk2(names(pluck(possibilities_intersect, i)), ~.x[,COND:=.y]) %>%
          walk(function(j){
            walk2(cond_expc_vars, 1:2,
                  ~ j[,(.x) := (eval(parse(text = num_int_var))^(.y))*PROB_COND])
          })

        conditional_frequency <- each %>% rbindlist() %>%
          .[,str_subset(string = names(.),
                        pattern = obtain_regex(pattern = cond_expc_vars,
                                               return_regex = "not_contains_pattern"))
            , with = FALSE]
        walk(c("PROB", "PROB_COND"), ~
               conditional_frequency[,(.x):= scales::percent(x = eval(parse(text = .x)))])
        list(CONDITIONAL = list(
          CONDITIONAL_FREQUENCY = conditional_frequency,

          CONDITIONAL_EXPENCTATION = map(each,
                                         ~ .x[,lapply(.SD, sum, na.rm = TRUE),
                                              by = COND, .SDcols = cond_expc_vars]) %>%
            walk(~ .x[,COND_VAR := eval(parse(text = cond_expc_vars[2]))-
                        (eval(parse(text = cond_expc_vars[1]))^2)] %>%
                   .[,':='(COND_SD = sqrt(COND_VAR), COND_MEAN_CUAD = NULL)]) %>%
            rbindlist()
        )) %>% return()
      }) %>% set_names(names(possibilities_intersect))
    }
    conditional_results
  }, error = function(e) "No intersection in any combination of levels")
  conditional_results
}

#' analyzing_means_plots
#'
#' Create a list with specific descriptive plots analizing the mean from the distribution of a numeric variable based on the levels of factor variables from a dataset.
#'
#' This function allows you to return descriptive plots analizing the mean from a numeric variable based on the levels of factor variables from a dataset.
#'
#' @param df dataset to obtain the plots
#' @param num_int_var *ONE* NUMERICAL VARIABLE from the dataset
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map_df map map_dbl
#' @importFROM ggplot2 ggplot aes geom_col geom_errorbar labs scale_x_discrete theme geom_bar coord_polar theme_minimal element_blank element_rect
#'
#' @return
#' "This function returns a list where each element is going to be a sublist containing".
#' \itemize{
#'   \item  _bar plot_ comparing the *standard error* of each combination level of the factor variables for the distribution of the \code{num_int_var}.
#'   \item _Polar area plot_ comparing the *mean* of each combination level of the factor variables for the distribution of the \code{num_int_var}.
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item Each output is going to be a list, where each element is the result from each combination levels of the factor variables and the whole distribution of the numeric variable.
#'   \item If there is no factor variable(s) then it is going to bring plots analyzing only the population mean, rather than the sample means, because this function needs factor variables to compare the means of the \code{num_int_var} for the levels combinations
#'   \item This function is applied to one numeric variable \code{num_int_var}
#'   \item If you are not using a *shiny app*, is better to use \code{general_descript_stats} function.
#'  }
#'
#' @example
#' \dontrun{
#' analyzing_means_plots(df = df, num_int_var = "PASSENGERID")
#' }
#'
analyzing_means_plots <- function(df, num_int_var){

  analyzing_stats <-
    map_df(marg_dist(df = df, num_int_var = num_int_var),
           ~ descr_stats(marg_dist_df = .x,
                         marg_dist_choice = "CENTRAL_TENDENCY",
                         num_int_var = num_int_var)) %>%
    as.data.table() %>%
    .[,ID:=names(marg_dist(df = df, num_int_var = num_int_var))] %>% .[,.(ID,MEAN)] %>%
    .[,':='(SE = map(marg_dist(df = df, num_int_var = num_int_var),
                     ~ descr_stats(marg_dist_df = .x,
                                   marg_dist_choice = "COEFFICIENTS",
                                   num_int_var = num_int_var)) %>%
              map_dbl("STANDARD_ERROR"),
            X = 1:.N)]

  # Standard Error
  plot_se <- analyzing_stats %>% ggplot(aes(x = X, y = MEAN,
                                            fill = as.factor(X))) +
    geom_col(alpha = 0.6, color = "black") +
    geom_errorbar(aes(ymin = MEAN-SE, ymax = MEAN+SE)) +
    labs(title = str_glue("Comparing means from each level based on {num_int_var}"),
         x = "LEVELS",
         fill = "BINS") +
    scale_x_discrete(limits = analyzing_stats[,ID]) +
    theme(legend.position = "none")
  plot_se <- design_PLOT(plot_se)
  # Comparing Means
  comparing_means <- ggplot(analyzing_stats, aes(ID, MEAN, fill = ID)) +
    geom_bar(width = 1, stat = "identity", color = "white", alpha = 0.6) +
    coord_polar() +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_rect(fill = "transparent",
                                         color = "transparent")) +
    labs(fill = num_int_var)

  list(ANALYZING_MEANS = list(STANDARD_ERROR = plot_se,
                              COMPARING_MEANS = comparing_means)) %>% return()
}

# UNSUPERVISED LEARNING ---------------------------------------------------

#' balanced_desagregated_kmean
#'
#' Create the k-means label, having in count a balanced datased based on
#' a unique id and a particular variable.
#'
#' 1. If the user select True on the \code{pre_cleaning} then the function will
#' clean the dataset as preparation to obtain the k-means labels as a new
#' dataset variable.
#' 2. Standarize the variable \code{var_model} according to the levels of
#' \code{ref_desgte}
#' 3. Desagregate the dataset for all the observations based on the variable
#' \code{ref_desgte} with frequency greater to 1, taking in mind that the
#' \code{id} variable needs to be unique.
#' 4. Creating the LABEL variable as the optimal k-mean labels for the dataset.
#' 5. Optimize generated k-labels for desagregated dataset based on the
#' \code{id} variable.
#' 6. Improve the dataset labels based on a balanced grouped sum of
#' \code{var_model} changing the labels that are upper the mean of the previous
#' to the lower labels.
#' 7. Improve the dataset labels based on a balanced grouped sum of
#' \code{var_model} changing the labels that are out of the range population
#' mean +- 1 standard deviation in the same way as 6.
#' 8. Again optmize generated k-labels for desagregated datased based on the
#' \code{id} variable to ensure that each \code{id} has different label.
#'
#' @param df dataset to create the labels
#' @param var_model reference variable of balance
#' @param ref_desgte reference desagregation variable
#' @param id id variable reference of balance
#' @param k number of desire clusters
#' @param pre_cleaning clean or not previous to apply the k-means model
#'
#' @author Eduardo Trujillo
#'
#' @seealso \href{https://uc-r.github.io/kmeans_clustering}{k-means algorithm}
#'
#' @import data.table
#' @importFrom tidyr drop_na
#' @importFrom purrr walk2
#' @importFrom stringr str_glue
#'
#' @return dataset \code{df} with a new created variable "LABEL" with levels up
#' to \code{k} that balance the whole dataset where:
#' \itemize{
#'   \item If \code{pre_cleaning == TRUE}, is going to clean the dataset before
#'         applying the k-means model
#'   \item If \code{pre_cleaning == FALSE}, is going to apply the k-means model
#'          to the provided dataset
#' }
#'
#' @export
#'
#' @example
#' \dontrun{
#' #Without cleaning at the begining:
#' k_means_df <- balanced_desagregated_kmean(
#'      df = df,
#'      var_model = "VOL_ENTREGA",
#'      ref_desgte = "FRECUENCIA",
#'      id = "ID_CLIENTE",
#'      k = 6,
#'      pre_cleaning = FALSE
#' )
#' k_means_df[, sum(VOL_ENTREGA), by = LABEL]
#' k_means_df[, .N, by = LABEL]
#' }
#'
balanced_desagregated_kmean <- function(df, var_model, ref_desgte,
                                        id, k, pre_cleaning = FALSE) {
  if (pre_cleaning) {
    df <- copy(df) %>%
      data.table() %>%
      delete_outliers() %>%
      tidyr::drop_na() %>%
      cardinality_adjustment(k = k)
  }

  walk2(
    str_glue("{var_model}_SCALE"), var_model,
    ~ df[, (.x) := scale(eval(parse(text = .y))), by = ref_desgte]
  )

  desgte_df <- desagregate_df(
    df = df,
    ref_desgte = ref_desgte,
    id = id
  ) %>%
    .[, LABEL := kmean_samesize(
      df = .[, c(ref_desgte, str_glue("{var_model}_SCALE")),
             with = FALSE
      ],
      k = k
    )] %>%
    improve_kmeans_labels(
      id = id,
      label = "LABEL",
      k = k
    )

  improve_kmeans_labels_variable(
    df = desgte_df,
    id = id,
    label = "LABEL",
    var_model = var_model,
    split_type = "mean_split"
  )
  improve_kmeans_labels_variable(
    df = desgte_df,
    id = id,
    label = "LABEL",
    var_model = var_model,
    split_type = "range_split"
  )

  desgte_df <- improve_kmeans_labels(
    df = desgte_df,
    id = id,
    label = "LABEL",
    k = k
  )
  desgte_df
}

# K-MEANS SUBFUNCTIONS ----

#' cardinality_adjustment
#'
#' Modify the cardinality of the dataset based on the divisibility with the
#' desire clusters.
#'
#' 1. If the cardinality is divisible by k then return the dataset, if not then.
#' obtain a number called "cardinality" where under the cardinality of df is
#' divisible by k
#' 2. From the elements that are not going to be in the dataset, make a
#' proportional sum (row cardinlaity + 1 to the last one) to add that proportion
#' to each numeric and integer variables on the rows of the dataset that are
#' going to be included.
#' 3. Define the dataset with rows until the cardinality "cardinality"
#' 4. Add 2. to each numeric and integer variable of the dataset to have a
#' balanced dataset.
#'
#' @param df dataset to adjust cardinality
#' @param k number of desire clusters
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFrom purrr walk
#'
#' @return the dataset with the cardinality adjusted based on the desire
#' clusters.
#'
#' @example
#' \dontrun{
#' adjusted_df <- cardinality_adjustment(df = df, k = 6)
#' }
#'
cardinality_adjustment <- function(df, k) {
  if (df[, .N] %% k == 0) {
    result <- df
  } else {
    cardinality <- df[, .N]

    while (cardinality %% k != 0) {
      if (cardinality %% k == 0) {
        break
      }
      cardinality <- cardinality - 1
    }

    num_int_vars <- classes_vector(
      data_type = c("integer", "numeric"),
      df = df
    )

    proportional_sum <- df[(cardinality + 1):.N,
                           lapply(.SD, sum),
                           .SDcols = num_int_vars
    ] /
      df[1:cardinality, .N]


    df <- df[1:cardinality]

    walk(
      num_int_vars,
      ~ df[, (.x) := (eval(parse(text = .x)) +
                        proportional_sum[, eval(parse(text = .x))])]
    )
    result <- df
  }
  result
}

#' kmean_samesize
#'
#' Create optimal k-mean labels for a dataset.
#'
#' 1. Calculate the k-means algorithm on numeric or integar variables from the
#' dataset \code{df}.
#' 2. Calculate the euclidean distance between the row df points and each
#' centroid.
#' - (x0,y0,z0,..),(x1,y1,z1,...),..,(xk,yk,zk,...) k centroids.
#' - We are going to have k df s. Therefore, we use _cbind.data.frame_ to create
#' a df where each df is a variable with length of the original df.
#' 3. 3.variables used:
#' - cardinality_sample: number of elements that are going to be for each
#' cluster (label).
#' - ctrl_clstr_no_elmnts: this variable helps to control the balance
#' cluster size.
#' - label: result balanced label that is going to be created.
#' 4. 4.For each iteration (each df row):
#' 5. 4.1. We obtain the row index where is the minimum euclidean distance in that
#' row of the df 2. defined as bestcluster and in that index we add 1 to the
#' observation of ctrl_clstr_no_elmnts variable representing adding 1 intended
#' top to k.
#' * example:
#' - iteration 1 - bestcluster = 2
#' - ctrl_clstr_no_elmnts  = 0 1 0 0 0 0 | row 1 of dataset *euclidean_distance*
#' - iteration 2 - bestcluster = 2
#' - ctrl_clstr_no_elmnts  = 0 2 0 0 0 0 | row 2 of dataset *euclidean_distance*
#' - iteration 3 - bestcluster = 3
#' - ctrl_clstr_no_elmnts  = 0 2 1 0 0 0 | row 3 of dataset *euclidean_distance*
#' - iteration 4 - bestcluster = 2
#' - ctrl_clstr_no_elmnts  = 0 3 1 0 0 0 | row 4 of dataset *euclidean_distance*
#' 6. 4.2.Add if statement to control and balance the size of each cluster based on
#' ctrl_clstr_no_elmnts variable. If in the index bestcluster of the observation
#' of ctrl_clstr_no_elmnts is greater than cardinality_sample then define the
#' variable on index bestcluster of df2. as NA to not affect the next iteration
#' and have only the columns in euclidean_distance where ctrl_clstr_no_elmnts is
#' less than cardinality_sample
#'
#' @param df dataset to generate k-means labels
#' @param  k number of desire clusters
#' @param nstart number of k-mean iterations for the centroids creation.
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFrom purrr map pluck walk
#' @importFrom stringr str_glue
#'
#' @return balanced k-means label for each observation of the dataset
#'
#' @note
#' \itemize{
#'    \item The kmeans algorithm assigns a label for each observation. However,
#'          it is not necessary to have balanced clusters in the amount of
#'          observations that is why we use this function.
#'    \item Kmeans algorithm assign randomly the centroids based on the
#'          iterations of \code{nstart}. This is why every time this
#'          function runs the output would have a different order in the labels.
#'    \item It is recommendable that the size of the population is divisible by
#'          the number of clusters \code{k} to have equal number of elements in
#'          each cluster.
#' }
#'
#' @example
#' label <- kmean_samesize(
#'      df = datasets::mtcars %>% as.data.table() %>% .[,.(mpg, wt)] %>% .[1:30],
#'      k = 6
#' )
#'
kmean_samesize <- function(df, k, nstart = 25) {
  optimal_samples <- kmeans(x = df, centers = k, nstart = nstart)

  euclidean_distance <- map(seq_len(k), function(i) {
    euclidean_distance_i <- t(
      t(df) - purrr::pluck(optimal_samples, "centers")[i, ]
    ) %>%
      data.table()

    walk(
      names(euclidean_distance_i),
      ~ euclidean_distance_i[, (.x) := eval(parse(text = .x))^2]
    )
    euclidean_distance_i[, .(sqrt(rowSums(.SD))),
                         .SDcols = names(euclidean_distance_i)
    ] %>%
      setnames(str_glue("EUCLIDEAN_DISTANCE_{i}"))
  }) %>%
    cbind.data.frame() %>%
    data.table()

  cardinality_sample <- df[, .N] / k
  ctrl_clstr_no_elmnts <- rep(0, k)
  label <- rep(NA, df[, .N])

  for (i in seq_len(euclidean_distance[, .N])) {
    bestcluster <- which.min(euclidean_distance[i, ])
    ctrl_clstr_no_elmnts[bestcluster] <-
      ctrl_clstr_no_elmnts[bestcluster] + 1
    label[i] <- bestcluster

    if (ctrl_clstr_no_elmnts[bestcluster] >= cardinality_sample) {
      euclidean_distance[, (bestcluster) := NA]
    }
  }
  label
}

#' improve_kmeans_labels
#'
#' Optimize generated K-labels for desagregated dataset
#'
#' 1. split the dataset by \code{id} testing unique elements on \code{label}.
#' Spliting on unique and duplicated sublist of the list improve_kmeans_labels
#' called \code{df_splited}.
#' 2. For the duplicated sublist, we split it by the \code{label} testing
#' unique element on  \code{label} in unique elements and duplicated elements.
#' - For the duplicated elements, then those are going to be the duplicated
#' sublist of 1.
#' - For the unique elements, then those are going to be appended to the unique
#' sublist 1.
#' on each same specific sublist of the unique sublist 1.
#' - Now we have a correct sublist of unique and duplicated elements.
#' 3. 3.From the duplicated sublist we take the first row of each sublist called
#' *to_modify* and from the unique sublist we take a random sample of the
#' same length from the duplicated one called *uniq_modify*. From that sublist
#' we create a sublist of the k-mean labels called *uniq_labels*.
#' 4. 4.We modify *to_modify* based on the list of labels *uniq_labels*
#' obtained from the sublist  *uniq_modify* where if the label of to_modify
#' is in *uniq_labels* then take a random number between 1 to k except that
#' labels of *uniq_labels*. In other case take any label from the sublist of
#' the sublist *uniq_labels*.
#' 5. 5.The modify sublist *to_modify* is going to be append in the list of
#' samples *uniq_modify* in each sublist
#' 6. 6.We modify the original created list \code{df_splited} modifying the
#' unique sublist elements with *uniq_modify* sublist and modify the duplicated
#' sublist deleting the first row of each sublist since was used on 3.
#' 7. 7.Create the original dataset with the modify labels.
#' 8. 8.If the duplicated sublist still have duplicate elements the apply
#' recursively the function to change the label of thoss repeated.
#'
#' @param df dataset to change labels.
#' @param id dataset id variable reference of balance
#' @param label k-means label variable
#' @param k number of desire clusters
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFrom purrr pluck map set_names flatten modify2 walk2 some
#'
#' @return desagregated dataset \code{df} with optimized K-labels
#'
#' @note This function is used to improve the k-means labels based on the id
#' variable.
#'
#' @example
#' \dontrun{
#' df <- improve_kmeans_labels(
#'     df = desgte_df,
#'     id = "ID_CLIENTE",
#'     label = "LABEL",
#'     k = 6
#' )
#' }
#'
improve_kmeans_labels <- function(df, id, label, k) {
  df_splited <- dfsplit_test_equality(
    df = df,
    split_reference = id,
    test_reference = label,
    names = c("UNIQUE_LABEL", "DUPLICATED_LABEL")
  )

  duplicated <- purrr::pluck(df_splited, "DUPLICATED_LABEL")
  uniq <- purrr::pluck(df_splited, "UNIQUE_LABEL")

  check_duplicated <- map(duplicated, function(i) {
    dfsplit_test_equality(
      df = i,
      split_reference = label,
      test_reference = label,
      names = c("DUPLICATED_UNIQUE", "DUPLICATED_DUPLICATED")
    )
  }) %>%
    set_names(names(duplicated))

  duplicated <- purrr::map(check_duplicated, "DUPLICATED_DUPLICATED") %>%
    map(1)

  uniq_replacement <- purrr::map(check_duplicated, "DUPLICATED_UNIQUE") %>%
    purrr::flatten()
  uniq <- replace(
    x = uniq,
    list = names(uniq_replacement),
    values = purrr::modify2(
      uniq[names(uniq_replacement)],
      uniq_replacement,
      ~ list(.x, .y) %>% rbindlist()
    ) %>%
      set_names(names(uniq_replacement))
  )

  to_modify <- map(
    seq_len(length(duplicated)),
    ~ pluck(duplicated, .x)[, .SD[1]]
  )

  uniq_modify <- sample(x = uniq, size = length(to_modify))
  uniq_labels <- purrr::map(uniq_modify, ~ .x[, eval(parse(text = label))])

  walk2(
    to_modify, uniq_labels,
    ~ .x[
      , (label) := ifelse(eval(parse(text = label)) %in% .y,
                          sample(x = c(1:k)[-.y], size = 1),
                          ifelse(length(.y) == 1, .y, sample(x = .y, size = 1))
      )
    ]
  )

  uniq_modify <- purrr::modify2(
    uniq_modify,
    to_modify,
    ~ list(.x, .y) %>% rbindlist()
  ) %>%
    set_names(names(uniq_labels))

  df_splited <- replace(
    x = df_splited,
    list = c("UNIQUE_LABEL", "DUPLICATED_LABEL"),
    values = list(
      replace(x = uniq, list = names(uniq_modify), values = uniq_modify),
      map(duplicated, ~ .x[-1, ])
    )
  )

  df_final <- df_splited %>%
    flatten() %>%
    rbindlist() %>%
    setorderv(id)

  result <- tryCatch(
    {
      if (some(pluck(df_splited, "DUPLICATED_LABEL"), ~ .x[, .N > 1])) {
        result <- improve_kmeans_labels(
          df = df_final,
          id = id,
          label = label,
          k = k
        )
      }
      result
    },
    error = function(e) df_final
  )
  result
}

#' improve_kmeans_labels_variable
#'
#' Improve the dataset labels based on a balanced grouped sum of a particular
#' variable.
#'
#' 1. Get the grouped sum of  \code{var_model} by the created k-mean
#' \code{label} variable.
#' 2. Calculate the population mean and standard deviation from **1.** as
#' parameters to modify the \code{label}
#' 3. create the *value_check* as the constant values as comparation reference
#' where the user will select based on the parameter \code{split_type}
#' 4. With help of the function *split_lower_upper_df* we obtain the labels
#' under and upper the constant values on **3.**
#' 5. Apply the subfunction *modify_labels_variable_df*
#'
#' @param df dataset to change labels.
#' @param id dataset id variable
#' @param label k-means label variable
#' @param var_model reference variable to balance
#' @param split_type type of label modification.
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFrom stringr str_glue
#' @importFrom purrr set_names pluck map
#'
#' @return This function modify the \code{label} variable of the \code{df} based
#' on \code{split_type}:
#' \itemize{
#'   \item If \code{split_type == "mean_split"}, change the labels where are
#'        upper the the mean of *1.* to the lower ones, to balance
#'        the grouped sum \code{var_model} by the \code{label}
#'   \item If \code{split_type == "range_split"}, change the labels out of the
#'         range population mean +- 1 standard deviation of *1.*. Channging
#'         equally to the previous option.
#' }
#'
#' @note This function is used to improve the k-means labels based on a
#' particular variable.
#'
#' @example
#' \dontrun{
#' improve_kmeans_labels_variable(
#'      df = desgte_df,
#'      id = "ID_CLIENTE",
#'      label = "LABEL",
#'      var_model = "VOL_ENTREGA",
#'      split_type = "mean_split"
#' )
#' }
#'
improve_kmeans_labels_variable <-
  function(df, id, label, var_model,
           split_type = c("mean_split", "range_split")) {
    sum_var_model <- str_glue("SUM_{var_model}")

    check <- df[, .(sum(eval(parse(text = var_model)))),
                by = label
    ] %>%
      setnames(old = "V1", new = sum_var_model)

    check_mean <- check[, mean(eval(parse(text = sum_var_model)))]
    check_sd <- check[, sd(eval(parse(text = sum_var_model)))]

    value_check <- list(
      list_of_lists(2, check_mean),
      list(
        (check_mean - check_sd),
        (check_mean + check_sd)
      )
    ) %>%
      set_names("mean_split", "range_split") %>%
      pluck(split_type)

    check_labels <- split_lower_upper_df(
      df = check,
      vars = list_of_lists(2, sum_var_model),
      value_check = value_check,
      names = c("UNDER_MEAN", "UPPER_MEAN")
    ) %>% map(~ .x[, eval(parse(text = label))])

    modify_labels_variable_df(
      df = df,
      id = id,
      label = label,
      var_model = var_model,
      check = check,
      check_mean = check_mean,
      check_labels = check_labels,
      sum_var_model = sum_var_model
    )
  }

#' modify_labels_variable_df
#'
#' Improve the dataset labels based on a balanced grouped sum of a particular
#' variable.
#'
#' 1. Based on the separeted labels (4. of improve_kmeans_labels_variable)
#' \code{check_labels} for the uppers get the difference between the
#' observations of \code{sum_var_model} in each level on \code{check}
#' and the mean (total sum of each level where is necessary to quit from
#' upper and pass to under).
#' 2. From the previous If we have only one total sum to quit from upper
#' then return that list in other case get the maximums according as the number
#' of unders as well as minimums. For the previous get a maximum and minimum for
#' each under (uppers that are going to pass to the uders)
#' 3. From the previous set the name of each sublist as the names of the unders
#' from the list \code{check_labels} to know which is going to pass from upper
#' to which from unders.
#' 4. Modify the dataset, based on the previous each under (max and min) change
#' the label of the uppers to unders to balance the dataset.
#'
#' @param df dataset to change labels.
#' @param id dataset id variable
#' @param label k-means label variable
#' @param var_model reference variable to balance
#' @param check grouped sum from improve_kmeans_labels_variable function
#' @param check_mean population mean from
#' improve_kmeans_labels_variable function
#' @param check_labels labels under and upper the constant values from
#' improve_kmeans_labels_variable function
#' @param sum_var_model variable from check that represent the grouped sum from
#' improve_kmeans_labels_variable function
#'
#' @author Eduardo Trujillo
#'
#' @seealso \href{https://stackoverflow.com/questions/45231735/match-values-to-nearest-value-in-another-array-in-r}{nearest values}
#'
#' @import data.table
#' @importFrom purrr map pluck set_names walk flatten
#'
#' @return The provided dataset \code{df} modifying the \code{label} variable.
#'
#' @note This function is a _subfunction_ of **improve_kmeans_labels_variable**
#'
#' @example
#' \dontrun{
#'    modify_labels_variable_df(
#'       df = desgte_df,
#'       id = "ID_CLIENTE",
#'       label = "LABEL",
#'       var_model = "VOL_ENTREGA",
#'       check = check,
#'       check_mean = check_mean,
#'       check_labels = check_labels,
#'       sum_var_model = sum_var_model
#' )
#' }
#'
modify_labels_variable_df <-
  function(df, id, label, var_model,
           check, check_mean, check_labels, sum_var_model) {
    difference_upper_to_mean <- map(
      pluck(check_labels, "UPPER_MEAN"),
      ~ round(check[
        eval(parse(text = label)) == .x,
        eval(parse(text = sum_var_model))
      ]
      - check_mean)
    ) %>% set_names(pluck(check_labels, "UPPER_MEAN"))

    difference_upper_to_mean <-
      tryCatch(
        {
          if (length(difference_upper_to_mean) != 1) {
            result <- map(
              c(TRUE, FALSE),
              ~ sort(x = unlist(difference_upper_to_mean), decreasing = .x)[
                seq_len(length(pluck(check_labels, "UNDER_MEAN")))
              ] %>%
                as.list()
            )
            result <- map(
              seq_len(length(pluck(check_labels, "UNDER_MEAN"))),
              ~ map(result, .x) %>%
                set_names(map(result, names) %>% map(.x))
            )
          }
          result
        },
        error = function(e) list(difference_upper_to_mean)
      )

    difference_upper_to_mean <- difference_upper_to_mean %>%
      set_names(pluck(check_labels, "UNDER_MEAN"))

    walk(seq_len(length(difference_upper_to_mean)), function(i) {
      each <- difference_upper_to_mean[i]
      each_sublevel <- flatten(each)

      walk(names(each_sublevel), function(x) {
        id_to_change <- df[eval(parse(text = label)) == x] %>%
          .[
            seq_len(which.min(abs(cumsum(eval(parse(text = var_model))) -
                                    pluck(each_sublevel, x)))),
            eval(parse(text = id))
          ]

        df[
          (eval(parse(text = label)) == x) &
            (eval(parse(text = id)) %in% id_to_change),
          (label) := names(each)
        ]
      })
    })
  }
