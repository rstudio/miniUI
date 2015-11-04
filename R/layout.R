#' @export
fillPage <- function(..., title = NULL, bootstrap = TRUE, theme = NULL) {
  fillCSS <- tags$head(tags$style(type = "text/css",
    "html, body { width: 100%; height: 100%; overflow: hidden; }"
  ))

  if (isTRUE(bootstrap)) {
    shiny::bootstrapPage(title = title, theme = theme, fillCSS, ...)
  } else {
    tagList(
      fillCSS,
      if (!is.null(title)) tags$head(tags$title(title)),
      ...
    )
  }
}

#' @export
dialogPage <- function(..., buttons = "OK", title = NULL, theme = NULL) {
  ui <- list(...)
  fillPage(ui, title = title, theme = theme)
}

#' @export
runGadget <- function(ui, server, port = getOption("shiny.port"),
  launch.browser = getOption("viewer", TRUE)) {

  shiny::runApp(shinyApp(ui, server), port = port,
    launch.browser = launch.browser)
}
