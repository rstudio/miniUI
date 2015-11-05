#' @import shiny
NULL

#' @export
fillPage <- function(..., padding = 0, title = NULL, bootstrap = TRUE,
  theme = NULL) {

  fillCSS <- tags$head(tags$style(type = "text/css",
    "html, body { width: 100%; height: 100%; overflow: hidden; }",
    sprintf("body { padding: %s; }", collapseSizes(padding))
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

collapseSizes <- function(padding) {
  paste(
    sapply(padding, shiny::validateCssUnit, USE.NAMES = FALSE),
    collapse = " ")
}

#' @export
dialogPage <- function(..., padding = 0, buttons = "OK", title = NULL,
  theme = NULL) {

  ui <- list(...)

  fillPage(ui, title = title, theme = theme)
}

#' @export
runGadget <- function(ui, server, port = getOption("shiny.port"),
  launch.browser = getOption("viewer", TRUE)) {

  shiny::runApp(shinyApp(ui, server), port = port,
    launch.browser = launch.browser)
}

#' @export
tabstripLayout <- function(..., height = "100%") {
  ts <- shiny:::buildTabset(list(...), "tabstrip")

  htmltools::attachDependencies(
    tagList(
      div(class = "tabstrip-container", ts$navList),
      div(class = "tabstrip-content-container", ts$content)
    ),
    gadgetDependencies()
  )
}

gadgetDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "shinygadgets",
      packageVersion("shinygadgets"),
      src = system.file("www", package = "shinygadgets"),
      stylesheet = "shinygadgets.css"
    )
  )
}

#' @export
titlebarButton <- function(inputId, label, primary = FALSE) {
  buttonStyle <- if (isTRUE(primary)) {
    "primary"
  } else if (identical(primary, FALSE)) {
    "default"
  } else {
    primary
  }

  tags$button(
    id = inputId,
    type = "button",
    class = sprintf("btn btn-%s btn-sm action-button", buttonStyle),
    label
  )
}

#' @export
titlebarLayout <- function(title, ..., left = NULL,
  right = titlebarButton("done", "Done", primary = TRUE)) {

  tags$div(class = "greedy",
    tags$div(class = "titlestrip",
      tags$h1(title),
      if (!is.null(left)) {
        tagAppendAttributes(left, class = "pull-left")
      },
      if (!is.null(right)) {
        tagAppendAttributes(right, class = "pull-right")
      }
    ),
    tags$div(class = "titlestrip-body",
      ...
    )
  )
}

#' @export
paddingPanel <- function(...) {
  tags$div(class = "greedy padded", ...)
}
