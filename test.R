library(shiny)
library(shinygadgets)

ts <- shiny:::buildTabset(
  list(
  ),
  "tabstrip"
)

ui <- fillPage(tabstripLayout(
  tabPanel("Hello", "Hello", icon = icon("home")),
  tabPanel("Goodbye", "Goodbye", icon = icon("star")),
  tabPanel("Whatever", "Whatever", icon = icon("camera-retro"))
))

server <- function(input, output, session) {
}

shinyApp(ui, server)
#' @examples
#' options(shiny.autoreload=TRUE)
#' shiny::runApp("test.R", launch.browser = getOption("viewer", TRUE))
