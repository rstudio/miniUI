library(shiny)
library(shinygadgets)
library(leaflet)

ui <- gadgetPage(
  titlebar("Shiny gadget example",
    left = titlebarButton("cancel", "Cancel", FALSE)
  ),
  contentPanel(padding = 0,
    leafletOutput("map", height = "100%")
  ),
  buttonBlock(
    actionButton("reset", "Reset bounds")
  )
)

server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })

  output$map <- renderLeaflet({
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(label = ~as.character(mag), labelOptions = list(zIndex = 1000))
  })

  output$table <- DT::renderDataTable({
    diamonds
  })

  observeEvent(input$cancel, {
    stopApp(stop("Gadget cancelled"))
  })

  observeEvent(input$done, {
    stopApp(TRUE)
  })

  observeEvent(input$reset, {
    leafletProxy("map", data = quakes) %>% fitBounds(
      ~min(long), ~min(lat),
      ~max(long), ~max(lat)
    )
  })
}

shinyApp(ui, server)
#' @examples
#' options(shiny.autoreload=TRUE)
#' shiny::runApp("test2.R", launch.browser = getOption("viewer", TRUE))
