
# WEB TITLE ---------------------------------------------------------------

#' title_bar
#'
#' Create a title bar for the header of the webpage
#'
#' This function allows you to put an image and a text in the title bar
#' of the web page
#'
#' @param image_href url-link of an image to show in the title bar
#' @param title string of text to show in the title bar
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import stringr
#'
#' @return Using \code{image_href} and \code{title} return an HTML tag of the title bar
#' @export
#'
#' @note Add this function inside a UI in your shiny app.
#'
#' @example
#' \dontrun{
#' title_bar(image_href = "www.image_reference.com", title = "example")
#' }
#'
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

#' image_title
#'
#' Create a title for the web page with an image
#'
#' This function allows you to create a title with an image for your header
#'
#' @param src_image url-link of an image to show in the title header
#' @param width_image width of the image
#' @param height_image height of the image
#' @param style_image csss argument
#' @param TITLE string of text to show in the title header
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#'
#' @return return an HTML tag of the header bar
#' @export
#'
#' @example
#' \dontrun{
#' image_title(src_image = "www.image_reference.com", width_image = 30, height_image = 30,
#' style_image = "-webkit-filter: drop-shadow(3px 3px 3px #222);", TITLE = "example"),
#' }
#'
image_title <- function(src_image, width_image,
                        height_image, style_image, TITLE){
  div(
    tags$img(
      src = src_image,
      width = width_image,
      height = height_image,
      style = style_image
    ), TITLE
  )
}

#' UIelement
#'
#' Create a UI component
#'
#' This function allows you to create a UI component inside a shiny module
#'
#' @param id module id
#' @param outputID output for the renderUI
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#'
#' @return Using \code{id} and \code{outputID} return a UI component
#' @export
#'
#' @note Use this function with a RenderUI.
#'
#' @example
#' \dontrun{
#' UIelement(id = "module_id", outputID = "render_id")
#' }
#'
UIelement <- function(id, outputID){
  uiOutput(NS(id, outputID))
}

# NAVBAR ------------------------------------------------------------------

#' rightInputs_Navbar
#'
#' Create a navbar allowing inputs in the right side
#'
#' This function allows you to create a standard navarPage, with the possibility to
#' add inputs in the right side of the bar
#'
#' @param ... elements of the navbarPage
#' @param inputs inputs to use in your navbar
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import htmltools
#'
#' @return Create a navbarPage using \code{inputs} in the right side
#' @export
#'
#' @note Use this function in the same way as a Navar Page
#'
rightInputs_Navbar <- function(..., inputs){
  navbar <- navbarPage(...)

  form <- tags$form(class = "navbar-form navbar-right", inputs)

  navbar[[3]][[1]]$children[[1]]$children[[2]] <-

    htmltools::tagAppendChild(navbar[[3]][[1]]$children[[1]]$children[[2]],
                              form)
  navbar %>% return()
}

#' nav_tabPanel
#'
#' Create iterative elements inside a navbarMenu
#'
#' This function allows you to create more than one tabpanel inside a navbarMenu dynamically
#' for use inside a navbarpage
#'
#' @param id module id
#' @param title vector of the tabs inside the navbarMenu
#' @param outputID vector of outputs for the renderUI
#' @param TITLE title for the navbarMenu
#'
#' @author Eduardo Trujillo
#'
#' @import purrr
#' @import shiny
#'
#' @return return a navbarMenu with tabs where you can use for create UI elements
#' @export
#'
#' @note Add this function inside a navbar
#'
#' @example
#' \dontrun{
#' nav_tabPanel(id = id, title = c("Author", "Project"),outputID = c("author_id", "project_id"),TITLE = "About")
#' }
#'
nav_tabPanel <- function(id, title, outputID, TITLE){
  moduleServer(id, function(input, output, session){
    tryCatch({
      if(length(title) == length(outputID)){
        tabpanel <- map2(title, outputID,
                         ~ tabPanel(
                           title = ..1,
                           UIelement(id = id, ..2)
                         )) %>%
          do.call(what = tagList, args = .)

        result <- navbarMenu(title = TITLE) %>%
          list_modify(tabs = tabpanel)
      }
      result
    }, error = function(e){"title & ID need to have same length"})
  })
}



