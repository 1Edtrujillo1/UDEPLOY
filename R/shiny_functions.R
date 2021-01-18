
# WEB ELEMENTS ---------------------------------------------------------------

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

#' refernce_icon
#'
#' Create a reference shadow image.
#'
#' This function allows you to create an image with gray format that links to a web page.
#'
#' @param image_href url-link of the image
#' @param href url-link of the reference web page of the icon
#' @param width_image width of the image
#' @param width_column width of the column in the fluidRow
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import stringr
#'
#' @return return an image that refers to an URL link.
#' @export
#'
#' @example
#' \dontrun{
#' refernce_icon(image_href = "www.image_reference.com",
#' href = "www.reference.com", width_image = "100px", width_column = 3)
#' }
#'
refernce_icon <- function(image_href, href, width_image, width_column){
  img(
    class = "thumbnail img-responsive",
    style = str_glue("border:none;
                      width:{width_image};
                      filter:opacity(30%) grayscale(1);
                      padding:0px;"),
    src = image_href
  ) %>% a(href = href, target = "_blank") %>% column(width = width_column)

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

# JUMBOTRON ---------------------------------------------------------------

#' personalize_jumbotron
#'
#' Create a personalized jumbotron
#'
#' This function allows you to make a particular example of jumbotron
#'
#' @param image_href url-link of an image to show in the jumbotron
#' @param width_text width of the text square
#' @param heigth_text width of the text square
#' @param header_text title of the text square
#' @param text the string of a phrase you want to share
#' @param cited who made the text quote
#' @param href link to open as a reference of the quote
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import stringr
#'
#' @return return a particular jumbotron with a quote inside of a text square
#' @export
#'
#' @example
#' \dontrun{
#' personalize_jumbotron(image_href = "www.image_reference.com",
#' width_text = "600px", heigth_text = "305px",
#' header_text = "TITLE EXAMPLE", text = p("Quote"), href = "#")
#' }
#'
personalize_jumbotron <- function(image_href, width_text, heigth_text,
                                  header_text, text, cited, href){

  image_href <- shQuote(image_href, type = "sh")

  div(
    class = "jumbotron",
    style = str_glue("background:url({image_href});
                   background-size:cover; padding-left:0px; padding-right:0px;
                   height:796px;"),
    div(
      class = "",
      fluidRow(
        column(
          width = 4,
          offset = 1,
          br(), br(), br(), br(),br(), br(), br(),br(),

          div(
            style = str_glue("background-color:#99999929;
                             color:white;
                             width:{width_text};
                             height:{heigth_text};"),
            h1(header_text),
            tags$blockquote(
              class = "blockquote",
              text,
              tags$footer(style = "color: white;",
                          "Quote  by",
                          tags$cite(title = cited, cited))
            ),
            a("Get Started", class = "btn btn-primary btn-lg", href = href)
          )
        )
      )
    )
  )
}

#' intermediate_bar
#'
#' Create an intermidiate personalized bar
#'
#' This function allows you to make an horizontal bar in your webpage
#'
#' @param image_href url-link of an image to show in the jumbotron
#' @param size_text size of the text inside the bar
#' @param text string text or tag text you want to share.
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import stringr
#'
#' @return return a personalized bar with text that you want to share
#' @export
#'
#' @example
#' \dontrun{
#' intermediate_bar(image_href = "www.image_reference.com",
#' size_text = "60px",
#' text = "info to share"
#' }
#'
intermediate_bar <- function(image_href, size_text, text){

  image_href <- shQuote(image_href, type = "sh")

  div(
    class = "jumbotron",
    style = str_glue("background:url({image_href});
             background-size:cover; height:400px;
             padding-left: 0px; padding-right: 0px;"),
    div(
      class = "",
      fluidRow(
        column(
          width = 6,
          offset = 1,
          br(),br(),br(),br(),
          div(
            style = str_glue("color:#ffffffcc;
                     font-family: spotify-circular, Helvetica, Arial, sans-serif;
                     font-size:{size_text};"),
            text
          )
        ) )
    )
  )
}

# PANEL -------------------------------------------------------------------

#' interactive_panel
#'
#' Create an personalized panel
#'
#' This function allows you to create an interactive personalized panel for
#' your web application
#'
#' @param id module id
#' @param image_href url-link of an image to show in the background of your panel
#' @param icon_title string name of the icon in the heading part
#' @param title string of text to show in the title header
#' @param outputID output for the action Button
#' @param ID_shinyJS id of the body to hide and show
#' @param text_info the string of a phrase you want to add in your panel
#' @param size_title pixels size of your title
#' @param size_text pixels size of your body text
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import stringr
#' @importFROM shinyjs hidden
#'
#' @return return a beauty panel where you can hide or show the particular body text.
#' @export
#'
#' @note Use this function at the same time with the function *interactive_panelOutput*.
#'
#' @example
#' \dontrun{
#' interactive_panel(id = "hello",
#' image_href = "www.image_reference.com",
#' icon_title = "code-branch",
#' title = "TITLE",
#' outputID = "down1",
#' ID_shinyJS = "hidden1",
#' text_info = "body example",
#' size_title = "35px",
#' size_text = "22px")
#' }
#'
interactive_panel <- function(id, image_href, icon_title,
                              title, outputID, ID_shinyJS,
                              text_info, size_title, size_text){

  image_href <- shQuote(image_href, type = "sh")

  div(
    class = "col-sm-4",
    div(
      class = "panel",
      style = str_glue("background:url({image_href});
                        background-size:cover;
                        color:white;
                        font-family:'Raleway';"),
      div(
        class = "panel-heading text-center",
        icon(name = icon_title,
             class = str_glue("fas fa-{icon_title} fa-4x"),
             lib = "font-awesome"),
        div(
          style = str_glue("font-size:{size_title};"),
          HTML(str_glue("<strong>{title}</strong>")),
          actionButton(inputId = NS(id, outputID),
                       label = NULL,
                       icon = icon(name = "angle-down",
                                   class = "fas fa-angle-down fa-1x",
                                   lib = "font-awesome"),
                       style = "background-color:transparent;
                                    border-color: transparent;
                                    color:white;")
        ),
        hr()
      ),
      div(
        id = NS(id, ID_shinyJS),
        class = "panel-body text-left text-capitalize",
        style = str_glue("padding:10px; font-size:{size_text};"),
        text_info
      ) %>% hidden()
    )
  )
}

#' interactive_panelOutput
#'
#' Server part of the interactive_panel function
#'
#' This function allows you to make interactively your interactive_panel function
#'
#' @param id module id
#' @param outputID output for the action Button
#' @param ID_shinyJS id of the body to hide and show
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @importFROM shinyjs onclick toggle
#'
#' @return return the server side of the interactive_panel function
#' @export
#'
#' @note Use this function at the same time with the function interactive_panel.
#'
#' @example
#' \dontrun{
#' interactive_panelOutput(id = "hello", outputID = "down1", ID_shinyJS = "hidden1")
#' }
#'
interactive_panelOutput <- function(id, outputID, ID_shinyJS){
  moduleServer(id, function(input, output, session){

    counter <- reactiveValues()

    counter[[outputID]] <- 0

    btn_click <- eventReactive(pluck(input, outputID), {
      TRUE

      counter[[outputID]] <- counter[[outputID]] + 1
    })

    observeEvent(btn_click(),{
      if((btn_click() %% 2) == 0){
        updateActionButton(session = session,
                           inputId = outputID,
                           icon = icon(name = "angle-down",
                                       class = "fas fa-angle-down fa-1x",
                                       lib = "font-awesome"))
      }else{
        updateActionButton(session = session,
                           inputId = outputID,
                           icon = icon(name = "angle-up",
                                       class = "fas fa-angle-up fa-1x",
                                       lib = "font-awesome"))
      }
    })

    onclick(id = outputID,{

      toggle(id = ID_shinyJS, anim = TRUE, animType = "slide")
    })
  })
}
