#' hello UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(shiny)
mod_hello_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput(inputId= ns("prenom"), #3lines added for hello server function
              label="Entrez votre prenom" ),
    textOutput(outputId = ns("coucou"))

  )
}

#' hello Server Functions
#'
#' @noRd
mod_hello_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$coucou = renderText( say_hello(input$prenom))# added for hello server function

  })
}

## To be copied in the UI
# mod_hello_ui("hello_1")

## To be copied in the server
# mod_hello_server("hello_1")

