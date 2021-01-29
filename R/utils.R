
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

#' list_vs_df
#'
#' Create a dataset of elements of a list or list of elements of a dataset
#'
#' This function allows you to create a dataset of elements of a list **or** from a dataset create a list with sublists of the elements of the dataset
#'
#' @param list list or dataset (inside a list)
#' @param output type of output (list or dataset)
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map pluck keep
#'
#' @return
#' "This function returns \code{two possible results}:"
#' \itemize{
#'   \item If \code{output} is \code{"list_to_dfs"} then convert a list of lists to a a dataset inside a list
#'   \item If \code{output} is \code{"dfs_to_list"} then convert a dataset to a list where each column of the dataset is going to be a sublist and each observation is going to be another sublist of the sublist of the column.
#' }
#' @export
#'
#' @example
#'l <- list(ID1 = c("A", "B"),
#'          ID2 = c("a", "b"),
#'          ID3 = c(1, 2)
#'          )
#'l2 <- list_vs_df(list = l %>% list(), output = "list_to_dfs")
#'list_vs_df(list = l2, output = "dfs_to_list")
#'
list_vs_df <- function(list, output = c("list_to_dfs", "dfs_to_list")){

  if(output == "list_to_dfs"){
    result <- map(list, function(j){

      each <- map(1:length(j), function(i)
        data.table(j[i] %>% pluck(1)) %>%
          setnames(names(j)[i]) %>%
          .[,ID:=1:.N]
      )
      iterative_merge(each, key = "ID", all = TRUE) %>%
        .[,-"ID"] %>% return()
    })

  }else if(output == "dfs_to_list"){
    result <- map(list,
                  ~ ..1 %>% as.list() %>%
                    map(function(i){
                      quit_na <- map(i, ~ keep(..1, ~!is.na(.x)))
                      quit_na[map(quit_na, length)>0]
                    })
    )
  }
  result
}
