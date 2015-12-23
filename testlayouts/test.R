library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

ui <- miniPage(
  miniTitleBar("Shiny gadget example",
    left = miniTitleBarButton("cancel", "Cancel", FALSE)
  ),
  miniTabstripPanel(
    tabPanel("Parameters", icon = icon("sliders"),
      miniContentPanel(
        sliderInput("year", "Year", 1978, 2010, c(2000, 2010),
          sep = "")
      )
    ),
    tabPanel("Visualize", icon = icon("area-chart"),
      miniContentPanel(padding = 80,
        plotOutput("cars", width = "100%", height = "100%")
      )
    ),
    tabPanel("Map", icon = icon("map-o"),
      miniContentPanel(padding = 0,
        leafletOutput("map", height = "100%")
      )
    ),
    tabPanel("Data", icon = icon("table"),
      miniContentPanel(
        DT::dataTableOutput("table")
      )
    )
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

runGadget(shinyApp(ui, server), viewer = paneViewer())
