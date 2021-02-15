
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
                                          "contains_pattern", "not_contains_pattern", "extract_pattern",
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
