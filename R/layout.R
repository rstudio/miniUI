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
    sprintf("body { padding: %s; margin: 0; }", collapseSizes(padding))
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


#' Page function for Shiny Gadgets
#'
#' Designed to serve as the outermost function call for your gadget UI. Similar
#' to \code{\link{fillPage}}, but always includes the Bootstrap CSS library, and
#' is designed to contain \code{\link{titlebar}}, \code{\link{tabstripPanel}},
#' \code{\link{contentPanel}}, etc.
#'
#' @param ... Elements to include within the page.
#'
#' @export
gadgetPage <- function(..., theme = NULL) {
  htmltools::attachDependencies(
    tagList(
      fillPage(
        tags$div(class = "gadget-container", ...),
        theme = theme
      )
    ),
    gadgetDependencies()
  )
}

collapseSizes <- function(padding) {
  paste(
    sapply(padding, shiny::validateCssUnit, USE.NAMES = FALSE),
    collapse = " ")
}

#' Run a gadget
#'
#' Similar to \code{runApp}, but if running in RStudio, defaults to viewing the
#' app in the Viewer pane.
#'
#' @param app Either a Shiny app object as created by
#'   \code{\link[=shiny]{shinyApp}} et al, or, a UI object.
#' @param server Ignored if \code{app} is a Shiny app object; otherwise, passed
#'   along to \code{shinyApp} (i.e. \code{shinyApp(ui = app, server = server)}).
#' @param port See \code{\link[=shiny]{runApp}}.
#' @param preferViewer Pass \code{TRUE} if the gadget is designed to work best
#'   in the RStudio Viewer pane. The advantage of the Viewer pane is that it
#'   sits conveniently inside the RStudio IDE. The disadvantage is its small
#'   size, relative to standalone browser windows.
#' @return The value returned by the gadget.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- gadgetPage(...)
#'
#' server <- function(input, output, session) {
#'   ...
#' }
#'
#' # Either pass ui/server as separate arguments...
#' runGadget(ui, server)
#'
#' # ...or as a single app object
#' runGadget(shinyApp(ui, server))
#' }
#'
#' @export
runGadget <- function(app, server = NULL, port = getOption("shiny.port"),
  preferViewer = TRUE) {

  if (!is.shiny.appobj(app)) {
    app <- shinyApp(app, server)
  }

  launch.browser <- if (identical(preferViewer, TRUE)) {
    getOption("viewer", TRUE)
  } else if (identical(preferViewer, FALSE)) {
    getOption("shiny.launch.browser", TRUE)
  } else {
    preferViewer
  }

  retVal <- withVisible(shiny::runApp(app, port = port, launch.browser = launch.browser))
  if (retVal$visible)
    retVal$value
  else
    invisible(retVal$value)
}


#' Create a tabstrip panel
#'
#' Create a tabstrip panel that contains \code{\link[=shiny]{tabPanel}}
#' elements. Similar to \code{\link[=shiny]{tabsetPanel}}, but optimized for
#' small page sizes like mobile devices or the RStudio Viewer pane.
#'
#' @param ... \code{\link[=shiny]{tabPanel}} elements to include in the tabset.
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the tab that should be selected by default. If \code{NULL}, the first
#'   tab will be selected.
#' @param between A tag or list of tags that should be inserted between the
#'   content (above) and tabstrip (below).
#'
#' @examples
#' library(shiny)
#'
#' tabstripPanel(
#'   tabPanel("Data",
#'     selectInput("dataset", "Data set", ls("package:datasets"))),
#'   tabPanel("Subset",
#'     uiOutput("subset_ui")
#'   )
#' )
#'
#' @export
tabstripPanel <- function(..., id = NULL, selected = NULL, between = NULL) {
  ts <- shiny:::buildTabset(list(...), "gadget-tabs", id = id,
    selected = selected
  )

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

#' Create a title bar
#'
#' Creates a title bar for a Shiny Gadget. Intended to be used with
#' \code{\link{gadgetPage}}. Title bars contain a title, and optionally, a
#' \code{titlebarButton} on the left and/or right sides.
#'
#' @param title The title of the gadget. If this needs to be dynamic, pass
#'   \code{\link[=shiny]{textOutput}} with \code{inline = TRUE}.
#' @param left The \code{titlebarButton} to put on the left, or \code{NULL} for
#'   none.
#' @param right The \code{titlebarButton} to put on the right, or \code{NULL}
#'   for none. Defaults to a primary "Done" button that can be handled using
#'   \code{observeEvent(input$done, \{...\})}.
#'
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

#' @param inputId The \code{input} slot that will be used to access the button.
#' @param label The text label to display on the button.
#' @param primary If \code{TRUE}, render the button in a bold color to indicate
#'   that it is the primary action of the gadget.
#' @rdname titlebar
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

#' Create a block-level button container
#'
#' Creates a full-width container for one or more buttons. The horizontal space
#' will be evenly divided among any buttons that are added.
#'
#' When using \code{buttonBlock} with a \code{tabstripPanel}, consider passing
#' the \code{buttonBlock} to \code{tabstripPanel} as the \code{between}
#' argument.
#'
#' @param ... One or more \code{\link[=shiny]{actionButton}} or
#'   \code{\link[=shiny]{downloadButton}} objects.
#' @param border Zero or more of \code{c("top", "bottom")}, indicating which
#'   sides should have borders, if any.
#'
#' @examples
#' library(shiny)
#'
#' buttonBlock(
#'   actionButton("reset", "Reset to defaults"),
#'   actionButton("clear", "Clear all")
#' )
#'
#' @export
buttonBlock <- function(..., border = "top") {
  cssClass <- "gadget-block-button"
  if (length(border) > 0) {
    cssClass <- paste(collapse = " ", c(cssClass, paste0("gadget-block-button-", border)))
  }

  tags$div(
    class = cssClass,
    ...
  )
}


cssList <- function(props) {
  names(props) <- gsub("[._]", "-", tolower(gsub("([A-Z])", "-\\1", names(props))))
  props <- props[names(props)[!sapply(props, is.null)]]
  if (length(props) == 0)
    ""
  else
    paste0(names(props), ":", props, ";", collapse = "")
}

flexboxContainer <- function(...,
  flex_direction = c("row", "row-reverse", "column", "column-reverse"),
  flex_wrap = c("nowrap", "wrap", "wrap-reverse"),
  justify_content = c("flex-start", "flex-end", "center", "space-between", "space-around"),
  align_items = c("stretch", "flex-start", "flex-end", "center", "baseline"),
  align_content = c("stretch", "flex-start", "flex-end", "center", "space-between", "space-around")
) {

  style <- cssList(list(
    display = "flex",
    flex_direction = if (!missing(flex_direction)) flex_direction,
    flex_wrap = if (!missing(flex_wrap)) flex_wrap,
    justify_content = if (!missing(justify_content)) justify_content,
    align_items = if (!missing(align_items)) align_items
  ))

  tags$div(style = style, ...)
}

flexboxItem <- function(...,
  order = integer(1),
  flex = "0 1 auto",
  align_self = c("auto", "flex-start", "flex-end", "center", "baseline", "stretch")
) {

  style <- cssList(list(
    order = if (!missing(order)) order,
    flex = if (!missing(flex)) flex,
    align_self = if (!missing(align_self)) align_self
  ))

  tags$div(style = style, ...)
}
