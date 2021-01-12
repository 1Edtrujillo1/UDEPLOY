#' title_bar
#'
#' Create a title bar for the header of the webpage
#'
#' This function allows you to put an image and a text in the title bar
#' of the web page
#'
#' @param image_href url-link of an image to show in the title bar
#' @param  title string of text to show in the title bar
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import dplyr
#' @importFROM stringr str_glue
#'
#' @return Using \code{image_href} and \code{title} return an HTML tag of the title bar
#'
#' @note Add this function inside a UI in your shiny app.
#'
#' @example
#' \dontrun{
#' title_bar(image_href = "www.image_reference.com", title = "example")
#' }
#' #' @export
title_bar <- function(image_href, title){

  image_href <- shQuote(image_href, type = "sh")

  list(
    tags$head(
      HTML(
        str_glue(
          '<link rel = "icon",
                    href = {image_href},
                    type = "image/png" />'
        )
      )
    ),div(
      titlePanel(
        title = "", windowTitle = title
      )
    )
  ) %>% do.call(what = tagList, args = .) %>%
    return()
}
