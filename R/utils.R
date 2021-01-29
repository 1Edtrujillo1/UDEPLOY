
# WORKING WITH LISTS ------------------------------------------------------

#' list_of_lists
#'
#' Create a list of sublists with the same elements each
#'
#' This function allows you to create a list where you can add in each sublist the same element
#'
#' @param no_sublists number of sublists
#' @param element_sublists element to instert in each sublist
#'
#' @author Eduardo Trujillo
#'
#' @importFROM purrr map pluck
#'
#' @return Using \code{no_sublists} and \code{element_sublists} list, which each sublist contains the same element
#' @export
#'
#' @note Add this function inside a UI in your shiny app.
#'
#' @example
#' list_of_lists(no_sublists = 2, element_sublists = list(c("a","b","c")))
#'
list_of_lists <- function(no_sublists, element_sublists){

  lists_creator <- map(1:no_sublists, ~ rep(list(), .x))

  map(1:length(lists_creator), ~(pluck(lists_creator, .x) <- element_sublists)) %>%
    return()
}

#' filter_column_list
#'
#' Filter a column of a list
#'
#' This function allows you to filter a column of a list bases on a element of a variable within.
#'
#' @param list list to filter column
#' @param variable variable reference
#' @param element of the variable reference of index
#'
#' @author Eduardo Trujillo
#'
#' @importFROM purrr pluck map
#' @importFROM stringr str_which
#'
#' @return This function returns all the column elements based on the position of \code{element} of the \code{variable}
#'
#' @export
#'
#' @example
#'filter_column_list(list = list(var1 = c("row1","row2","row3","row4"),
#'                               id = c("a", "b", "c", "d")),
#'                    variable = "id",
#'                    element = "b")
#'
filter_column_list <- function(list, variable, element){

  index_filter <- str_which(string = pluck(list, variable),
                            pattern = element)

  map(1:length(list), function(i)
    map(list[i], ~.x[index_filter])
  ) %>% unlist(recursive = FALSE) %>%
    return()
}
