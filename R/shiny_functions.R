
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
#'
#' outputUI <- function(id){
#' moduleServer(id, function(input, output, session){
#' output$render_id <- renderUI({
#'      ...
#'    })
#'  })
#' }
#' }
#'
UIelement <- function(id, outputID){
  uiOutput(NS(id, outputID))
}

#' message_reactive
#'
#' Create an interactive box message
#'
#' This function allows you to create an interactive personalized success or fail message
#'
#' @param id module id
#' @param text string to show for success or fail
#' @param type type of box message
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import shinyWidgets
#'
#' @return
#' "This function returns \code{two possible box messages}:"
#' \itemize{
#'   \item If \code{type} is \code{success} then returns success box message
#'   \item If \code{type} is \code{fail} then returns fail box message
#' }
#'
#' @export
#'
#' @example
#' \dontrun{
#' message_reactive (id = id,
#'                   text = "We will check your information and notify you back.",
#'                   type = "success")
#' }
#'
message_reactive <- function(id, text, type = c("success", "fail")){
  moduleServer(id, function(input, output, session){

    if(type == "success"){
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = p(text, style = "font-family:'Raleway';"),
        type = "success")

    }else if(type == "fail"){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = p(text, style = "font-family:'Raleway';"),
        type = "error")
    }
  })
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
#' @importFROM purrr map2
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

#' intermediate_bar_icon
#'
#' Create an intermidiate personalized bar of icons
#'
#' This function allows you to make a jumbotron and a bar of icons
#'
#' @param title title of the information inside the jumbotron
#' @param title_text referenced text of the jumbotron
#' @param icon_name 3 names of icons.
#' @param icon_class 3 classes of icons.
#' @param icon_title 3 titles which refer to the information that describes the icon.
#' @param icon_text_reference 3 texts which refer to the information that describes the icon.
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import stringr
#' @importFROM purrr map
#'
#' @return return a personalized jumbotron with referenced text and a bar with 3 icons information.
#' @export
#'
#' @example
#' \dontrun{
#' intermediate_bar_icon(title = "Want to visualize interactive analysis?",
#' title_text = "Now you can, through AImagination.",
#' icon_name = c("award", "check-circle", "cloudversify"),
#' icon_class = c("fas", "far", "fab"),
#' icon_title = c("icon title 1", "icon title 2", "icon title 3"),
#' icon_text_reference = c("text icon 1", "text icon 2", "text icon 3))
#' }
#'
intermediate_bar_icon <- function(title, title_text,
                                  icon_name, icon_class,
                                  icon_title, icon_text_reference){
  tryCatch({
    if(length(icon_name) == 3){

      icon_information <- list(
        icon_name = icon_name,
        icon_class = icon_class,
        icon_title = icon_title,
        icon_text_reference = icon_text_reference)

      result <- div(
        div(
          class = "jumbotron text-center",
          style = "background-color:#F8F8F8; height:300px; font-family:-webkit-pictograph;",
          p(strong(title), style = "font-size:48px;"),
          p(title_text, style ="font-size:22px;")
        ),
        br(), br(), br(),
        div(
          class = "",
          style = "font-family:system-ui;",
          fluidRow(
            map(1:length(icon_information), ~ do.call(
              what =
                function(icon_name, icon_class, icon_title, icon_text_reference){
                  column(
                    width = 4,
                    div(
                      class = "text-center",
                      icon(name = icon_name,
                           class = str_glue("{icon_class} fa-{icon_name} fa-8x"),
                           lib = "font-awesome"),
                      h3(strong(icon_title)),
                      p(icon_text_reference,
                        style = "font-size:larger; filter:opacity(60%);")
                    )
                  )},
              args = map(icon_information, ..1))) %>%
              do.call(what = tagList, args = .)
          )
        )
      )
    }
    result
  }, error = function(e) "You need to have only 3 icons")
}

#' end_jumbotron
#'
#' Create the end of the webpage with a jumbotron
#'
#' This function allows you to make a particular design of end page
#'
#' @param icon_names name of the font-awesome icon.
#' @param icon_class \code{"fab"} OR \code{"fas"} depend on font-awesome
#' @param icon_href URL page or tab from the navbar page
#' @param icon_ref_target \code{"link"} if is an URL link OR \code{"tab"} if is a tab of the navbar Page
#' @param icon_target if is going to open in a new page \code{"_blank"} or in the same page \code{""}
#' @param image_compamy image url of your company
#' @param company_name name of your company
#' @param terms_cond_ref link to open your terms and conditions of the webpage
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import stringr
#' @importFROM purrr pmap pluck
#'
#' @return return the end of the web page as a jumbotron
#' @export
#'
#' @example
#' \dontrun{
#' end_jumbotron(icon_names = c("github", "envelope"),
#' icon_class = c("fab", "fas"),
#' icon_href = c("www.reference-page.com", "Tab from Nabar"),
#' icon_target = c("_blank", ""),
#' icon_ref_target = c("link", "tab"),
#' image_compamy = "www.image-url.com",
#' company_name = "Company",
#' terms_cond_ref = "www.terms-cond.com")
#' }
#'
end_jumbotron <- function(icon_names, icon_class, icon_href, icon_ref_target, icon_target,
                          image_compamy, company_name, terms_cond_ref){
  tryCatch({
    if(
      ((length(icon_names) == length(icon_class)) ==
       (length(icon_href) == length(icon_ref_target))) ==
      (length(icon_names) == length(icon_target))){

      unlist_elements <- list_of_lists(no_sublists = length(icon_names),
                                       element_sublists = tags$li())

      elements <- pmap(list(icon_names, icon_class, icon_href, icon_ref_target, icon_target),
                       function(name, class, href, ref_target, target){
                         each <- a(icon(name = name,
                                        class = str_glue("{class} fa-{name} fa-2x"),
                                        lib = "font-awesome"),
                                   target = target)

                         if(ref_target == "link"){
                           each$attribs <- each$attribs %>%
                             append(list(href = href))

                         }else if(ref_target == "tab"){
                           each$attribs <- each$attribs %>%
                             append(list(onclick =
                                           str_glue("fakeClick({shQuote(href, type = 'sh')})")))
                         }
                         each
                       })

      for(i in 1:length(icon_names)){
        pluck(unlist_elements, i)$children <- pluck(elements, i)
      }

      result <- div(
        class = "jumbotron",
        style = "background-color:#05192D; padding-left:0px; padding-right: 0px;
                 color:white; font-family:-webkit-pictograph;",
        div(
          class = "",
          fluidRow(
            column(
              width = 3,
              offset = 1,
              img(
                class = "thumbnail img-responsive",
                style = "background-color:transparent; border:none; width:150px;",
                src = image_compamy),
              p(icon(name = "copyright",
                     class = "far fa-copyright",
                     lib = "font-awesome"),
                strong(company_name), "|",
                a("Terms & Conditions", href = terms_cond_ref))
            ))
        ),
        div(
          class = "pull-right",
          tags$ul(
            class = "list-inline",
            unlist_elements %>% do.call(what = tagList, args = .)
          )
        )
      )
    }
    result
  }, error = function(e){
    "icon_names, icon_class, icon_href, icon_ref_target, icon_target need to have the same cardinality"})
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

# LOG-IN ------------------------------------------------------------------

#' loginUI
#'
#' Create an personalized log in to a web page
#'
#' This function allows you to create an interactive personalized log-in webpage
#'
#' @param id module id
#' @param image_href url-link of an image to show in the log-in pannel and the load part
#' @param title string of text to show in the title header
#' @param error_message message string to show when your password is incorrect
#' @param company_name name of your company to show in the footer sign up and in the load part
#' @param header header of the dashboardPagePlus
#' @param sidebar sidebar of the dashboardPagePlus
#' @param body body of the dashboardPagePlus
#' @param rightsidebar rightsidebar of the dashboardPagePlus
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @importFROM shinydashboardPlus dashboardPagePlus
#' @importFROM stringr str_glue
#' @importFROM waiter spin_plus use_waiter
#' @importFROM shinyjs hidden useShinyjs
#'
#' @return
#' "This function returns \code{two possible UI}:"
#' \itemize{
#'   \item If header & sidebar & body & rightsidebar are *NULL* then return a created UI
#'   \item If header & sidebar & body & rightsidebar are not *NULL* then return a *dashboardPagePlus*
#' }
#'
#' @export
#'
#' @note Use this function at the same time with the function *loginOutput*.
#'
#' @example
#' \dontrun{
#' ui <- loginUI(id = "hello",
#'               image_href = "www.image_reference.com",
#'               title = "Welcome Back!",
#'               error_message = "Wrong Password!",
#'               company_name = "Company")
#' }
loginUI <- function(id, image_href, title,
                    error_message, company_name,
                    header = NULL,  sidebar = NULL,
                    body = NULL, rightsidebar = NULL){

  # 0.0 Export the waiter
  waiting_screen <<- tagList(
    div(
      tags$img(
        src = image_href,
        height = 100),
      h4(strong(str_glue("{company_name} loading ...")),
         style = "color:black; font-family:-webkit-pictograph;"),
      br(),
      spin_plus(), #type of spinner
    )
  )

  # 1.0 LOG-IN PAGE
  login_page <- list(

    # 1.1 TITLE BAR
    div(
      title_bar(image_href = image_href,
                title = company_name)
    ) %>% hidden(),

    # 1.2 LOG-IN BOX
    fluidPage(
      use_waiter(),
      useShinyjs(),

      div(
        id = NS(id, "login-panel"),
        style = "width:500px; max-width:100%; padding:20px; margin:auto;",

        wellPanel(

          style = "background-color:#2e856e33;",

          # 1.2.1 PRESENTATION
          div(
            img(class = "img-circle img-responsive",
                src = image_href,
                style = "width:150px; margin:auto;"),

            h2(strong(title),
               class = "text-center",
               style = "font-family:-webkit-pictograph; color:black;")
          ),

          br(),

          # 1.2.2 USER AND PASSWORD
          div(
            tags$style(type = "text/css",
                       ".control-label {font-family:-webkit-pictograph;
                                      color:black;}"),
            tags$style(type = "text/css",
                       "#user_name, #password {font-family:-webkit-pictograph}"),

            textInput(inputId = NS(id, "user_name"),
                      label = tagList(icon("user"), "User Name"),
                      placeholder = "Enter user name."),

            passwordInput(inputId = NS(id, "password"),
                          label = tagList(icon("unlock-alt"), "Password"),
                          placeholder = "Enter password.")
          ),

          # 1.2.3 LOG-IN BUTTON
          div(
            class = "text-center",
            actionButton(inputId = NS(id, "login_button"),
                         label = strong("Log in"),
                         class = "btn btn-lg",
                         style = "background-color:#2e6da4;
                                font-family:-webkit-pictograph;")
          ),

          # 1.2.4 LOG-IN ERROR
          div(
            id = NS(id, "error"),
            style =  "font-family:-webkit-pictograph; color:red;",
            p(strong(error_message),
              class = "text-center")
          ) %>% hidden(),

          br(),

          #1.2.5 REFERENCE BUTTON
          div(
            class = "text-left",
            style =  "font-family:-webkit-pictograph; color:black;",
            str_glue("New to {company_name}?"),
            actionButton(inputId = NS(id, "new_user"),
                         label = strong("Sign up."),
                         style = "background-color:transparent;
                                border-color: transparent;
                                padding:initial;
                                color:black;
                                font-family:-webkit-pictograph;")
          )
        )))
  )

  # 2.0 WEB PAGE
  if(is.null(header) & is.null(sidebar) &
     is.null(body) & is.null(rightsidebar)){
    #2.1 UI RESULT
    result <- login_page %>% append(
      list(
        div(
          id = NS(id, "display_content"),
          UIelement(id = id, outputID = "display_content_result")
        ) %>% hidden()
      )) %>% do.call(what = tagList, args = .)

  }else{
    # 2.2 DASHBORDPLUS
    result <- login_page %>% append(
      list(
        div(
          id = NS(id, "display_content"),
          dashboardPagePlus(header = header,
                            sidebar = sidebar,
                            body = body,
                            rightsidebar = rightsidebar)
        ) %>% hidden()
      )) %>% do.call(what = tagList, args = .)
  }
  result
}

#' loginOutput
#'
#' Server part of the *loginUI* function
#'
#' This function allows you to make the server work of the *loginUI* function
#'
#' @param id module id
#' @param user_base dataset that contain the users and the passwords to log-in
#' @param username variable of the \code{user_base} dataset that represent the users
#' @param password variable of the \code{user_base} dataset that represent the passwords
#' @param waiter created waiter to use for the load part
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import data.table
#' @importFROM shinyjs toggleState toggle delay hide
#' @importFROM dplyr pull
#' @importFROM sodium password_store
#' @importFROM waiter waiter_show waiter_hide
#'
#' @return return the server side of the function *loginUI*
#'
#' @export
#'
#' @note Use this function at the same time with the function *loginUI*.
#'
#' @example
#' ui <- loginUI(id = "hello",
#'               image_href = "www.image_reference.com",
#'               title = "Welcome Back!",
#'               error_message = "Wrong Password!",
#'               company_name = "Company")
#'
#' exampleOutput <- function(id){
#'  moduleServer(id, function(input, output, session){
#'   output$display_content_result <- renderUI({
#'    div(
#'      class = "well",
#'      id = "success",
#'      h1(class = "page-header", "Be happy", tags$small("by Eduardo Trujillo")),
#'      p(class = "lead", "Page content...")
#'     )
#'   })
#'  })
#' }
#'
#' user_base_tbl <- data.table(user = c("user1", "user2"),
#'                             password = c("pass1", "pass2"))
#'
#' server <- function(input, output) {
#'   credentials <- loginOutput(id = "hello",
#'                              user_base = user_base_tbl,
#'                              username = "user",
#'                              password = "password")
#'
#'
#'   exampleOutput(id = "hello")
#' }
#'
#' shinyApp(ui = ui, server = server)
#'
loginOutput <- function(id, user_base, username, password, waiter = waiting_screen){
  moduleServer(id, function(input, output, session){

    # 1.0 onclick login button
    observe({
      toggleState(id = "login_button", condition = {
        (input$user_name != "") && (input$password != "")
      })
    })

    # 2.0 validate user & password
    credentials <- reactiveValues()

    validate_pwd <- eventReactive(input$login_button,{

      validate <- FALSE

      # 2.1 user base for the access to the application
      user_base <- user_base[get(username) == input$user_name]

      # 2.2 validate correct user and password
      # (pull allows to convert the datatable to character eg. user:user1 -> "user1")
      if(user_base[,.N] != 0){
        credentials$username <- pull(user_base[,..username],1)

        credentials$password <- sapply(pull(user_base[,..password], 1),
                                       password_store)

        if(input$user_name == credentials$username &&
           input$password == names(credentials$password)){
          validate <- TRUE
        }
      }

      # 2.3 message of incorrect user & password
      if(!validate){
        toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
        delay(2000,
              toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
      }

      # 2.4 Hide log-in page
      if(validate) hide(id = "login-panel")

      validate
    })

    # 3.0 display Web Page
    observeEvent(validate_pwd(),{

      req(validate_pwd())

      waiter_show(html = waiter, color = "#2e856e33")
      Sys.sleep(2)
      waiter_hide()

      toggle(id = "display_content", anim = TRUE, animType = "fade")
    })

    reactive({
      reactiveValuesToList(credentials)
    }) %>% return()
  })
}

# DYNAMIC SIDEBAR ---------------------------------------------------------

#' dynamic_menuItem
#'
#' Create interactively the sidebar of a dashboard
#'
#' This function allows you to create an interactive personalized tabNames and subtabnames of a dashboardPage
#'
#' @param menu_text Show text of of the tab
#' @param menu_tabName id of the tab
#' @param menu_icon icon of the tab
#' @param menu_icon_class class of the icon of the tab
#' @param submenu_text show text of the subtabs of a tab
#' @param submenu_tabName id of the subtabs of a tab
#' @param subitem \code{yes} if there is subtabs or \code{no} if there is no subtabs
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @importFROM purrr pmap map2
#' @importFROM shinydashboard menuItem menuSubItem
#' @importFROM stringr str_glue
#'
#' @return
#' "This function returns \code{two possible tabs}:"
#' \itemize{
#'   \item If \code{subitem} is \code{yes} then it means that we can create a tab with subtabs
#'   \item If \code{subitem} is \code{no} then it means that a tab doesn´t have subtabs
#' }
#'
#' @export
#'
#' @note The advantage of this function is that we can create multiple tabs (thanks to the iteratior) only using this function.
#'
#' @example
#' \dontrun{
#' *No subtabs*
#'dynamic_menuItem(menu_text = c("Presentation", "Raw Data"),
#'                 menu_tabName = c("presentation", "rawdata"),
#'                 menu_icon = c("vector-square", "table"),
#'                 menu_icon_class = rep("fas", 2),
#'                 subitem = "no")
#'*Subtabs*
#'#'dynamic_menuItem(menu_text = c("General Inforamation","Statistical Distributions"),
#'                   menu_icon = c("calculator","chart-bar"),
#'                   menu_icon_class = rep("fas", 2),
#'                   submenu_text = list(c("General reports", "Descriptive Statistics", "Add"),
#'                                       c("Discrete Random Variable", "Continous Random Variable")),
#'                   submenu_tabName = list(c("GR", "DS", "a"),
#'                                          c("SDiscrete", "SDcontinous")),
#'                   subitem = "yes")
#' }
#'
dynamic_menuItem <- function(menu_text = NULL,
                             menu_tabName = NULL,
                             menu_icon = NULL,
                             menu_icon_class = NULL,
                             submenu_text = NULL,
                             submenu_tabName = NULL,
                             subitem = c("yes", "no")){

  if(subitem == "no"){
    result <- pmap(list(menu_text, menu_tabName, menu_icon, menu_icon_class), ~
                     do.call(
                       what = function(menu_text, menu_tabName,
                                       menu_icon, menu_icon_class){
                         menuItem(
                           text = menu_text,
                           tabName = menu_tabName,
                           icon = icon(name = menu_icon,
                                       class = str_glue("{menu_icon_class} fa-{menu_icon}"),
                                       lib = "font-awesome")
                         )},
                       args = list(menu_text = ..1,
                                   menu_tabName = ..2,
                                   menu_icon = ..3,
                                   menu_icon_class = ..4)
                     ))

  }else if(subitem == "yes"){
    result <- pmap(list(menu_text, menu_icon, menu_icon_class,
                        submenu_text, submenu_tabName),
                   ~menuItem(
                     text = ..1,
                     icon = icon(name = ..2,
                                 class = str_glue("{..3} fa-{..2}"),
                                 lib = "font-awesome"),

                     map2(..4, ..5, ~

                            do.call(
                              what = function(submenu_text, submenu_tabName){
                                menuSubItem(
                                  text = submenu_text,
                                  tabName = submenu_tabName,
                                  icon = icon(name = "angle-right",
                                              class = "fas fa-angle-right",
                                              lib = "font-awesome")
                                )},
                              args = list(submenu_text = .x,
                                          submenu_tabName = .y)
                            ))
                   )
    ) %>% do.call(what = tagList, args = .)
  }
  result
}

# DYNAMIC HEADER ----------------------------------------------------------

#' NotificationmenuInput
#'
#' Create a UI-Message component
#'
#' This function allows you to create a UI-message component inside a shiny module
#'
#' @param id module id
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#'
#' @return UI-message component
#' @export
#'
#' @note Use this function with function *NotificationmenuOutput*.
#'
#' @example
#' \dontrun{
#' NotificationmenuInput(id = "notifications")
#' }
#'
NotificationmenuInput <- function(id){
  dropdownMenuOutput(outputId = NS(id, "notificationMenu"))
}


#' NotificationmenuInput
#'
#' Create server side of the UI-message
#'
#' This function allows you to make work the UI-message component inside a shiny module
#'
#' @param id module id
#' @param message list of message we want to share
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import data.table
#' @importFROM purrr pluck
#'
#' @return UI-message component
#' @export
#'
#' @note Use this function with function *NotificationmenuInput*.
#' @note
#' "This message list needs to have the variables:"
#' \itemize{
#'   \item \code{text} refers to the messages we want to share
#'   \item \code{status} color of the messages ("warning", "success", "info", etc.)
#'   \item \code{Href} references link
#'   \item \code{icon} icon of the message
#' }
#'
#' @example
#' \dontrun{
#' NotificationmenuOutput(id = "notifications",
#'                        message = list(text = c("Done", "kk"),
#'                        status = c("success","info"),
#'                        Href = c("https://reference1.com",
#'                                 "https://reference2.com"),
#'                        icon = rep("info", 2)))
#' }
#'
NotificationmenuOutput <- function(id, message){
  moduleServer(id, function(input, output, session){
    output$notificationMenu <- renderMenu({

      df_messages <- message() %>% pluck(1) %>% as.data.table() %>%
        set_names(toupper(names(.))) %>%
        .[,TEXT:=toupper(TEXT)]

      # Create a Dynamic notifications for the header
      ntfcation <- apply(df_messages, 1,
                         function(row){
                           notificationItem(text = row[["TEXT"]],
                                            href = row[["HREF"]],
                                            status = row[["STATUS"]],
                                            icon = icon(name = row[["ICON"]]))
                         })
      dropdownMenu(type = "notifications", .list = ntfcation)
    })
  })
}

# SIGN UP -----------------------------------------------------------------

# SING UP (PASSWORD) ------------------------------------------------------

#' passUI
#'
#' Create a dynamic password visualization
#'
#' This function allows you to visualize the string password and its confirmation based on a click
#'
#' @param id module id
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @importFROM purrr map2 pmap
#' @importFROM shinyjs hidden
#' @importFROM stringr str_c
#'
#' @return passwords and its confirmation password and a click button to show what we are writting on them.
#' @export
#'
#' @note Use this function at the same time with the function *passOutput*.
#'
#' @example
#' \dontrun{
#' passUI(id = "hello")
#' }
#'
passUI <- function(id){

  div(
    map2(list(list(c("hide_pass", "confirm_hide_pass"),
                   c("password", "confirm_password"),
                   c("Password", "Confirmation")),
              list(c("hide_text", "confirm_hide_text"),
                   c("password_text", "confirm_password_text"),
                   c("Password", "Confirmation"))),
         list(passwordInput, textInput),

         ~ div(
           pmap(.x,
                ~ div(id = NS(id, ..1),
                      style="display: inline-block;vertical-align:top; width: 250px;
                           font-family:'Raleway';",
                      .y(inputId = NS(id, ..2),
                         label = NULL,
                         placeholder = ..3)
                ) %>% hidden()
           )
         )
    ),

    p(style = "font-family:'Raleway';",
      "Use 8 or more characters with a combination of letters, numbers and symbols."),

    div(
      tags$style(type = "text/css",
                 str_c("#",id, "-check_password {transform:scale(1.5);}")),

      style = "font-family:'Raleway'; font-size:larger;",
      checkboxInput(inputId = NS(id, "check_password"),
                    label = "Show password",
                    value = FALSE)
    )
  )
}

#' passOutput
#'
#' Server part of the *passUI* function
#'
#' This function allows you to make the server work of the *passUI* function
#'
#' @param id module id
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @importFROM purrr map2 pmap map
#' @importFROM shinyjs toggle hide
#'
#' @return return the server side of the function *passUI*
#' @export
#'
#' @note
#' \itemize{
#'   \item Use this function at the same time with the function *passUI*.
#'   \item his function returns the password and the confirmation if we need it later (as a reactive list).
#' }
#'
#' @example
#' ui <- fluidPage(
#'  useShinyjs(),
#'
#'  passUI(id = "hello")
#' )
#' server <- function(input, output){
#'  passOutput(id = "hello")
#'  }
#'
#'shinyApp(ui = ui, server = server)
#'
passOutput <- function(id){
  moduleServer(id, function(input, output, session){

    string_password <- reactiveValues()

    observe({
      string_password$store <- input$password
      string_password$store_confirmation <- input$confirm_password

      map2(c("password_text", "confirm_password_text"),
           c(string_password$store, string_password$store_confirmation),
           ~ updateTextInput(session = session,
                             inputId = ..1,
                             value = ..2))
    })

    observe({
      if(!input$check_password){

        pmap(list(c("hide_pass", "confirm_hide_pass"),
                  rep(TRUE, 2),
                  rep("slide", 2)),
             ~ toggle(id = ..1, anim = ..2, animType = ..3))

        map(c("hide_text", "confirm_hide_text"), hide)

      }else if(input$check_password){

        map(c("hide_pass", "confirm_hide_pass"), hide)

        pmap(list(c("hide_text", "confirm_hide_text"),
                  rep(TRUE, 2),
                  rep("slide", 2)),
             ~ toggle(id = ..1, anim = ..2, animType = ..3))
      }
    })

    reactive({
      reactiveValuesToList(string_password)
    }) %>% return()
  })
}

# SIGN UP (PAGE) ----------------------------------------------------------

#' sign_upUI
#'
#' Create an personalized sign up web page
#'
#' This function allows you to create an interactive personalized sign up webpage
#'
#' @param id module id
#' @param image_href url-link of an image of your company
#' @param title your company name as string
#' @param services list of services your company offer.
#' @param login_href url-link reference to go to another page (the log-in web page)
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @importFROM shinyjs useShinyjs hidden
#' @importFROM shinythemes shinytheme
#' @importFROM stringr str_glue
#' @importFROM purrr map2
#'
#' @return UI webpage of the sign up.
#' @export
#'
#' @note Use this function at the same time with the function *sign_upOutput*.
#'
#' @example
#' \dontrun{
#' ui <- sign_upUI(id = "password",
#'                 image_href = "www.image_reference.com",
#'                 title = "example",
#'                 services = list("Service 1" = "id1",
#'                                 "Service 2" = "id2",
#'                                 "Service 3" = "id3"),
#'                 login_href = "#")
#' }
#'
sign_upUI <- function(id, image_href, title, services, login_href){

  fluidPage(

    useShinyjs(),

    theme = shinytheme("readable"),

    title_bar(image_href = image_href,
              title = title),

    div(
      class = "container",
      style = "width:1100px; padding:20px; margin:auto;",
      wellPanel(
        style = "background-color:transparent;",

        # 1.0 Presentation
        div(
          class = "text-left",
          img(class = "img-circle img-responsive pull-left",
              src = image_href,
              style = "width:100px; margin:auto;"),
          br(), br(),br(),br(),br(),
          h3(str_glue("Create your {title} account"))
        ),
        br(),
        fluidRow(
          # Left-Side
          column(
            width = 7,

            # 2.0 Name
            div(
              map2(c("first_name", "second_name"),
                   c("Name", "Last name"),
                   ~ div(style="display: inline-block;vertical-align:top; width: 250px;
                           font-family:'Raleway';",
                         textInput(inputId = NS(id, ..1),
                                   label = NULL,
                                   placeholder = ..2))
              )
            ),
            br(),
            # 3.0 Mail
            div(style="width: 500px;font-family:'Raleway';",
                textInput(inputId = NS(id, "mail"), label = NULL, placeholder = "Mail")),
            br(),
            # 4.0 Password
            passUI(id = id),
            br(),
            # 5.0 Services
            div(
              style = "font-family:'Raleway';",
              h4("Choose your desire services."),
              hr(),
              checkboxGroupInput(inputId = NS(id, "services"),
                                 label = NULL,
                                 choices = services)
            ),
            br(), br(),
            # 6.0 Buttons
            div(
              style = "font-family:'Raleway';",
              a(strong("Access your account instead"),
                href = login_href),
              div(class = "pull-right",
                  actionButton(inputId = NS(id, "sign_up"),
                               label = "Sign Up",
                               class = "btn btn-primary btn-lg")
              )
            ),
            br(), br(), br(),
            div(
              id = NS(id, "error"),
              style = "font-family:'Raleway'; color:red;",
              p(strong("Passwords need to match!"),
                class = "text-center")
            ) %>% hidden()
          ),

          # Right-Side
          column(
            width = 5,
            div(
              class = "text-center hidden-md hidden-sm hidden-xs",
              style = "font-family:'Raleway'; font-size:xx-large;",

              tags$style(type = "text/css",
                         ".fa-laptop-house:before {color:#2e856e;}"),

              icon(name = "laptop-house",
                   class = "fas fa-laptop-house fa-10x"),
              br(), br(),
              p("One account. Multiple services at your disposal.")
            ))
        )
      ))
  )
}

#' sign_upOutput
#'
#' Server part of the *sign_upUI* function
#'
#' This function allows you to make the server work of the *sign_upUI* function
#'
#' @param id module id
#' @param pass reactive password obtained from the function \code{passOutput}
#' @param confirm_pass reactive confirmation password obtained from the function \code{passOutput}
#' @param config \code{YAML} file indicate the mongodb database where we are going to push the new users
#'
#' @author Eduardo Trujillo
#'
#' @import shiny
#' @import data.table
#' @importFROM shinyjs toggleState toggle delay
#'
#' @return return the server side of the function *sign_upUI*
#' @export
#'
#' @note
#' \itemize{
#'   \item Use this function at the same time with the function *sign_upUI*.
#'   \item The output of this function is a data.table that is going to push the recollected information to a MongoDB. So you need to have a collection.
#' }
#'
#' @example
#' ui <- sign_upUI(id = "password",
#'                 image_href = "www.image_reference.com",
#'                 title = "example",
#'                 services = list("Service 1" = "id1",
#'                                 "Service 2" = "id2",
#'                                 "Service 3" = "id3"),
#'                 login_href = "#")
#'
#' server <- function(input, output){
#'
#'    string_password <- udeploy::passOutput(id = "password")
#'    sign_upOutput(id = "password",
#'                  pass = reactive(string_password()$store),
#'                  confirm_pass = reactive(string_password()$store_confirmation),
#'                  config = config)
#'
#' }
#'
#' shinyApp(ui = ui, server = server)
#'
sign_upOutput <- function(id, pass, confirm_pass, config){
  moduleServer(id, function(input, output, session){

    # 1.0 onclick sign up button
    observe({
      toggleState(id = "sign_up", condition = {
        (input$first_name != "") && (input$second_name != "") &&
          (input$mail != "")  && (pass() != "") &&
          (input$services != "")
      })
    })
    # 2.0 Validate Password and create new users
    validate_pwd <- eventReactive(pass(),{

      if(pass() == confirm_pass()){

        message_reactive(id = id,
                         text = "We will check your information and notify you back.",
                         type = "success")

        new_user <- data.table(
          name = paste(input$first_name, input$second_name),
          user = input$first_name,
          mail = input$mail,
          password = pass(),
          tabnames = list(input$services))

        mongo_manipulation(collection = config$collection,
                           database = config$database,
                           host = config$host,
                           username = config$username,
                           password = config$password,
                           mongo_choice = "push",
                           push_record = new_user)
      }else{
        toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
        delay(2000,
              toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))

      }
    })
    # 3.0 Create new user information
    observeEvent(input$sign_up,{
      validate_pwd()
    })
  })
}
