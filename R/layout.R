#' @import shiny
NULL

#' Create a page that fills the window
#'
#' \code{fillPage} creates a page whose height and width always fill the
#' available area of the browser window.
#'
#' The \code{\link[shiny]{fluidPage}} and \code{\link[shiny]{fixedPage}}
#' functions are used for creating web pages that are laid out from the top
#' down, leaving whitespace at the bottom if the page content's height is
#' smaller than the browser window, and scrolling if the content is larger than
#' the window.
#'
#' \code{fillPage} is designed to latch the document body's size to the size of
#' the window. This makes it possible to fill it with content that also scales
#' to the size of the window.
#'
#' For example, \code{fluidPage(plotOutput("plot", height = "100\%"))} will not
#' work as expected; the plot element's effective height will be \code{0},
#' because the plot's containing elements (\code{<div>} and \code{<body>}) have
#' \emph{automatic} height; that is, they determine their own height based on
#' the height of their contained elements. However,
#' \code{fillPage(plotOutput("plot", height = "100\%"))} will work because
#' \code{fillPage} fixes the \code{<body>} height at 100\% of the window height.
#'
#' Note that \code{fillPage(plotOutput("plot"))} will not cause the plot to fill
#' the page. Like most Shiny output widgets, \code{plotOutput}'s default height
#' is a fixed number of pixels. You must explicitly set \code{height = "100\%"}
#' if you want a plot (or htmlwidget, say) to fill its container.
#'
#' One must be careful what layouts/panels/elements come between the
#' \code{fillPage} and the plots/widgets. Any container that has an automatic
#' height will cause children with \code{height = "100\%"} to misbehave. Stick
#' to functions that are designed for fill layouts, such as the ones in this
#' package.
#'
#' @param ... Elements to include within the page.
#' @param padding Padding to use for the body. This can be a numeric vector
#'   (which will be interpreted as pixels) or a character vector with valid CSS
#'   lengths. The length can be between one and four. If one, then that value
#'   will be used for all four sides. If two, then the first value will be used
#'   for the top and bottom, while the second value will be used for left and
#'   right. If three, then the first will be used for top, the second will be
#'   left and right, and the third will be bottom. If four, then the values will
#'   be interpreted as top, right, bottom, and left respectively.
#' @param title The title to use for the browser window/tab (it will not be
#'   shown in the document).
#' @param bootstrap If \code{TRUE}, load the Bootstrap CSS library.
#' @param theme URL to alternative Bootstrap stylesheet.
#'
#' @examples
#' library(shiny)
#'
#' fillPage(
#'   tags$style(type = "text/css",
#'     ".half-fill { width: 50%; height: 100%; }",
#'     "#one { float: left; background-color: #ddddff; }",
#'     "#two { float: right; background-color: #ccffcc; }"
#'   ),
#'   div(id = "one", class = "half-fill",
#'     "Left half"
#'   ),
#'   div(id = "two", class = "half-fill",
#'     "Right half"
#'   ),
#'   padding = 10
#' )
#'
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

#' @export
gadgetPage <- function(..., theme = NULL) {
  fillPage(
    tags$div(class = "gadget-container", ...),
    theme = theme
  )
}

collapseSizes <- function(padding) {
  paste(
    sapply(padding, shiny::validateCssUnit, USE.NAMES = FALSE),
    collapse = " ")
}

#' @export
runGadget <- function(ui, server, port = getOption("shiny.port"),
  launch.browser = getOption("viewer", TRUE)) {

  shiny::runApp(shinyApp(ui, server), port = port,
    launch.browser = launch.browser)
}

#' @export
tabstripPanel <- function(..., between = NULL) {
  ts <- shiny:::buildTabset(list(...), "gadget-tabs")

  htmltools::attachDependencies(
    tagList(
      div(class = "gadget-tabs-content-container", ts$content),
      between,
      div(class = "gadget-tabs-container", ts$navList)
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
titlebar <- function(title, left = NULL,
  right = titlebarButton("done", "Done", primary = TRUE)) {

  htmltools::attachDependencies(
    tags$div(class = "gadget-title",
      tags$h1(title),
      if (!is.null(left)) {
        tagAppendAttributes(left, class = "pull-left")
      },
      if (!is.null(right)) {
        tagAppendAttributes(right, class = "pull-right")
      }
    ),
    gadgetDependencies()
  )
}

#' @export
paddedPanel <- function(...) {
  htmltools::attachDependencies(
    tags$div(class = "gadget-padded", ...),
    gadgetDependencies()
  )
}

#' @export
scrollPanel <- function(...) {
  htmltools::attachDependencies(
    tags$div(class = "gadget-scroll", ...),
    gadgetDependencies()
  )
}

#' @export
contentPanel <- function(..., padding = 10) {
  htmltools::attachDependencies(
    tags$div(class = "gadget-content",
      tags$div(class = "gadget-absfill",
        style = sprintf("position: absolute; %s;", paddingToPos(padding)),
        ...
      )
    ),
    gadgetDependencies()
  )
}

paddingToPos <- function(padding) {
  padding <- sapply(padding, shiny::validateCssUnit, USE.NAMES = FALSE)
  sizes <- if (length(padding) == 0) {
    rep_len("0", 4)
  } else if (length(padding) == 1) {
    rep_len(padding, 4)
  } else if (length(padding) == 2) {
    padding[c(1,2,1,2)]
  } else if (length(padding) == 3) {
    padding[c(1,2,3,2)]
  } else {
    padding[1:4]
  }

  props <- c("top", "right", "bottom", "left")
  paste0(props, ":", sizes, ";", collapse = "")
}

#' @export
buttonBlock <- function(..., border = "top") {
  tags$div(class = "gadget-block-button", ...)
}
