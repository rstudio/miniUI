library(shiny)
library(shinygadgets)
library(leaflet)

ui <- fillPage(
  titlebarLayout("Shiny gadget example",
    tabstripLayout(
      tabPanel("Parameters", icon = icon("sliders"),
        paddedPanel(
          sliderInput("year", "Year", 1978, 2010, c(2000, 2010),
            sep = "")
        )
      ),
      tabPanel("Visualize", icon = icon("area-chart"),
        plotOutput("cars", height = "100%")
      ),
      tabPanel("Map", icon = icon("map-o"),
        leafletOutput("map", height = "100%")
      ),
      tabPanel("Data", icon = icon("table"), scrollPanel(
        paddedPanel(
          DT::dataTableOutput("table")
      )))
    ),
    left = titlebarButton("cancel", "Cancel", FALSE)
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
}

shinyApp(ui, server)
#' @examples
#' options(shiny.autoreload=TRUE)
#' shiny::runApp("test.R", launch.browser = getOption("viewer", TRUE))
