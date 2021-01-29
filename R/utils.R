
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
