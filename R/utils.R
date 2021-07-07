
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
      iterative_merge(each, key = "ID", unify_id = TRUE, all = TRUE) %>%
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

# DYNAMIC DATA ------------------------------------------------------------

#' design_DT
#'
#' Visualize a dataset in a JS format.
#'
#' This function allows you to transform your dataset into JS format (beautiful design).
#'
#' @param df dataset to make the design
#' @param style theme of the datatable
#' @param extra_options *list* of additional **options** arguments
#' @param convert convert a dataset to datatable
#' @param ... extra arguments in \code{datatable}
#'
#' @author Eduardo Trujillo
#'
#' @importFROM formattable as.datatable
#' @importFROM DT datatable
#'
#' @return The dataset in DT format.
#' "This function returns two possible results based on \code{convert} parameter:"
#' \itemize{
#'   \item If \code{convert = TRUE} transform a formattable to datatable DT format
#'   \item If \code{convert = FALSE} transform a data.table or a data.fram to datatable (DT format)
#' }
#' @export
#'
#' @example
#' \dontrun{
#' desing_DT(df = dataset)
#' }
#'
design_DT <- function(df, style = 'bootstrap',
                      extra_options = NULL, convert = FALSE, ...){

  if(convert){
    func <- as.datatable
    data_parameter <- list(x = df)

  }else{
    func <- datatable
    data_parameter <- list(data = df)
  }

  do.call(what = func,
          args = list(
            style = style,
            filter = list(position = 'top', clear = FALSE),
            options = list(autoWidth = TRUE) %>% append(extra_options),
            ...
          ) %>% append(data_parameter)
  ) %>% return()
}

#' design_PLOT
#'
#' Visualize a plot in a plotly format.
#'
#' This function allows you to transform your plot into plotly format (beautiful design).
#'
#' @param plot defined ggplot plot
#' @param font_size letter type of axis and title
#' @param axis_color color of axis
#' @param axis_line_color color of line axis
#'
#' @author Eduardo Trujillo
#'
#' @importFROM ggplot2 element_text element_line theme_minimal theme
#' @importFROM purrr pmap set_names
#' @importFROM plotly ggplotly layout
#'
#' @return A plot in plotly format.
#' @export
#'
#' @example
#' \dontrun{
#' design_PLOT(plot = plot)
#' }
#'
design_PLOT <- function(plot, font_size = "italic",
                        axis_color = "white", axis_line_color = "red"){

  args <- pmap(list(list(font_size, font_size, NULL, NULL),
                    list(0.5, NULL, NULL, NULL),
                    list(NULL, NULL, axis_color, axis_color)),
               ~ element_text(face = ..1, hjust = ..2, colour = ..3)) %>%
    set_names("plot.title", "axis.title", "axis.text", "text") %>%
    append(
      list_of_lists(no_sublists = 2,
                    element_sublists = element_line(color = axis_line_color)) %>%
        set_names("axis.ticks", "axis.line")
    )

  final_plot <- plot + theme_minimal() +
    do.call(what = theme, args = args)

  final_plot <- plotly::ggplotly(final_plot) %>%
    plotly::layout(plot_bgcolor = 'transparent', paper_bgcolor = 'transparent')
  final_plot
}

# WORKING WITH DATASETS ---------------------------------------------------

#' interval_between
#'
#' Visualize a close interval from a vector.
#'
#' This function allows you to identify a close interval.
#'
#' @param x vector
#' @param rng range of dimension 2
#'
#' @author Eduardo Trujillo
#'
#' @return Vector of TRUE/FALSE values of where a vector \code{x} is in that particular \code{rng}
#' @export
#'
#' @example
#' c(4,6,7,9,10,13,15) %between% c(6,9)
#'
interval_between <- function(x,rng) x >= eval(parse(text = rng[[1]])) & x <= eval(parse(text = rng[[2]]))

#' test_equality
#'
#' Check if elements of a variable are unique
#'
#' This function has been design of a given vector
#'
#' @param var vector variable
#'
#' @author  Eduardo Trujillo
#'
#' @importFrom purrr map
#'
#' @return This function returns \code{two possible results}:
#' \itemize{
#'  \item _TRUE_ indicates that the vector has unique elements.
#'  \item _FALSE_ indicates that the vector has repeated elements.
#' }
#' @export
#'
#' @example
#' # unique elements in a variable
#' test_equality(var = 1:5)
#' # repeated elements in a variable
#' test_equality(var = c(1, 2, 3, 1))
#'
test_equality <- function(var) {
  map(
    list(
      function(x) {
        x %>%
          unique() %>%
          length()
      },
      length
    ),
    ~ do.call(what = .x, args = list(var))
  ) %>%
    unique() %>%
    length() == 1
}

#' dfsplit_test_equality
#'
#' Split a dataset into unique and duplicate elements by an _id_ reference
#'
#' This function has been design for a given dataset split it by the variable
#' \code{split_reference} and divide it in sublists. This is if the variable
#' \code{test_reference} has unique elements or not.
#'
#' @param df dataset to split
#' @param split_reference variable of reference to split dataset
#' @param test_reference variable of reference to test if elements of
#' that variable are unique
#' @param names names of the sublists unique and repeated.
#'
#' @author Eduardo Trujillo
#'
#' @import  data.table
#' @importFrom purrr map keep discard set_names
#'
#' @return List where each sublist is the unique and the duplicated
#' observations from \code{df}.
#'
#' @export
#'
#' @example
#' dfsplit_test_equality(
#'    df = data.table(
#'          ID_CLIENTE = c(1, 2, rep(3, 2), 4, rep(5, 2), rep(6, 3)),
#'          LABEL = c("a", "b", rep("d", 2), "e", "f", "g", rep("h", 2), "i")
#'    ),split_reference = "ID_CLIENTE",
#'    test_reference = "LABEL",
#'    names = c("UNIQUE_LABEL", "DUPLICATED_LABEL")
#' )
#'
dfsplit_test_equality <- function(df, split_reference, test_reference, names) {
  map(
    list(purrr::keep, purrr::discard), function(f) {
      f(
        df %>% split(
          x = .,
          f = df[, eval(parse(text = split_reference))]
        ),
        ~ test_equality(var = .x[, eval(parse(text = test_reference))])
      )
    }
  ) %>%
    set_names(names)
}

#' desagregate_df
#'
#' Desagregate a dateset for all the observations based on a specific variable
#' with frequency greater to 1, taking in mind that the id variable needs to
#' be unique.
#'
#' This function has been design to check if the variable \code{id} is unique
#' filtered by the variable \code{ref_desgte} and if it is then for the
#' observations in \code{ref_desgte} with frequency greater than one desagregate
#' in each \code{id}, in other case return the observations in the same way for
#' each filter \code{id}.
#'
#' @param df original dataset to desagregate
#' @param ref_desgte reference desagregation variable
#' @param id unique id variable
#'
#' @author  Eduardo Trujillo
#'
#' @import data.table
#' @importFrom purrr map map_df keep
#' @importFrom stringr str_glue
#'
#' @return the dataset \code{df} desagregated by the variable \code{ref_desgte}
#'
#' @export
#'
#' @example
#' \dontrun{
#' df <- desagregate_df(df = df, ref_desgte = "FRECUENCIA", id = "ID_CLIENTE")
#' }
#'
desagregate_df <- function(df, ref_desgte, id) {
  map(
    df[, eval(parse(text = ref_desgte))] %>% unique(),
    function(i) {
      each <- df[eval(parse(text = ref_desgte)) == i]
      id_var <- each[, eval(parse(text = id))]

      desgte_df <- tryCatch(
        {
          if (test_equality(var = id_var)) {
            desgte_df <- tryCatch(
              {
                if (i > 1) {
                  desgte_df <- map_df(id_var, function(j) {
                    each_deeper <-
                      each[eval(parse(text = id)) == j]

                    udeploy::list_of_lists(
                      no_sublists = each_deeper[
                        ,
                        eval(parse(text = ref_desgte))
                      ],
                      element_sublists = each_deeper
                    ) %>%
                      rbindlist() %>%
                      .[, (ref_desgte) := 1]
                  })
                }
                desgte_df
              },
              error = function(e) each
            )
          }
          desgte_df
        },
        error = function(e) str_glue("{id} needs to be unique")
      )
      desgte_df
    }
  ) %>%
    purrr::keep(~ isFALSE(is.character(.x))) %>%
    rbindlist() %>%
    setorderv(id)
}

#' split_lower_upper_df
#'
#' Dataset comparation split
#'
#' Split a dataset based on a comparison between values of a variable and a
#' constant value.
#'
#' @param df dataset to split
#' @param vars variable reference to split the dataset
#' @param value_check constant value as comparation reference
#' @param names of the sublists
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFrom purrr pmap set_names
#'
#' @return  list with two sublists:
#' \itemize{
#'   \item A sublist with the dataset filter for values of the
#'        variable \code{vars} under \code{value_check}
#'   \item A sublist with the dataset filter for values of the
#'        variable \code{vars} upper \code{value_check}
#' }
#'
#' @export
#'
#' @example
#' data <- data.table(iris)
#' split_lower_upper_df(
#'    df = data,
#'    vars = rep("Petal.Width", 2),
#'    value_check = rep(data[, mean(Petal.Width)], 2),
#'    names = c("under_mean", "upper_mean")
#' )
#'
split_lower_upper_df <- function(df, vars, value_check, names) {
  pmap(
    list(
      list(
        function(x1, x2) {
          x1 <= x2
        },
        function(x1, x2) {
          x1 > x2
        }
      ),
      vars,
      value_check
    ),
    ~ df[.x(eval(parse(text = ..2)), ..3)]
  ) %>% set_names(names)
}
