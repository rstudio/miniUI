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

  tagList(
    tags$head(
      tags$style(type = "text/css", HTML("
        .tabstrip-content-container {
        position: absolute;
        left: 0;
        right: 0;
        bottom: 0;
        top: 0;
        bottom: 51px;
        }
        .tabstrip-container {
        position: absolute;
        left: 0;
        right: 0;
        bottom: 0;
        height: 51px;
        border-top: 1px solid #dddddd;
        }
        ul.tabstrip {
        display: table;
        width: 100%;
        margin: 0;
        padding: 0 12px;
        }
        ul.tabstrip>li {
        display: table-cell;
        width: 1%;
        height: 50px;
        vertical-align: middle;
        }
        ul.tabstrip>li.active>a {
        color: #428bca;
        }
        ul.tabstrip>li>a {
        display: block;
        text-align: center;
        color: #929292;
        text-decoration: none;
        font-size: 12px;
        }
        ul.tabstrip>li>a>i {
        display: block;
        margin-left: auto;
        margin-right: auto;
        font-size: 24px;
        }
        "))
    ),
    div(class = "tabstrip-container", ts$navList),
    div(class = "tabstrip-content-container", ts$content)
  )
}
