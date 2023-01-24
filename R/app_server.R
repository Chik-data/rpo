#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_hello_server("hello_1")# Attention copier coller les fct server SANS VIRGULE !!
  mod_hello_server("hello_2")
  mod_tibble_server("tibble_1")
}
