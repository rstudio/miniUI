#' @import shiny
#' @importFrom htmltools css
NULL

#' Page function for Shiny Gadgets
#'
#' Designed to serve as the outermost function call for your gadget UI. Similar
#' to \code{\link[shiny]{fillPage}}, but always includes the Bootstrap CSS
#' library, and is designed to contain \code{\link{titlebar}},
#' \code{\link{tabstripPanel}}, \code{\link{contentPanel}}, etc.
#'
#' @param ... Elements to include within the page.
#' @param title The title to use for the browser window/tab (it will not be
#'   shown in the document).
#' @param theme URL to alternative Bootstrap stylesheet.
#'
#' @export
gadgetPage <- function(..., title = NULL, theme = NULL) {
  htmltools::attachDependencies(
    tagList(
      fillPage(
        tags$div(class = "gadget-container", ...),
        title = title,
        theme = theme
      )
    ),
    gadgetDependencies()
  )
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
  ts <- buildTabset(list(...), "gadget-tabs", id = id,
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
      "miniUI",
      packageVersion("miniUI"),
      src = system.file("www", package = "miniUI"),
      stylesheet = "miniUI.css"
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

scrollPanel <- function(...) {
  htmltools::attachDependencies(
    tags$div(class = "gadget-scroll", ...),
    gadgetDependencies()
  )
}

#' Create a content panel
#'
#' Creates a panel for containing arbitrary content within a flex box container.
#' This is mainly useful within \code{\link{gadgetPage}} or a
#' \code{\link{tabstripPanel}}'s \code{\link[shiny]{tabPanel}}. You can use
#' \code{contentPanel} to introduce padding and/or scrolling, but even if
#' padding/scrolling aren't needed, it's a good idea to wrap your custom content
#' into \code{contentPanel} as it fixes some odd behavior with percentage-based
#' heights.
#'
#' @param ... UI objects to be contained in the \code{contentPanel}. A single
#'   htmlwidget or \code{\link[shiny]{plotOutput}} with \code{height="100\%"}
#'   works well, as do
#'   \code{\link[shiny]{fillRow}}/\code{\link[shiny]{fillCol}}.
#' @param padding Amount of padding to apply. Can be numeric (in pixels) or
#'   character (e.g. \code{"3em"}).
#' @param scrollable If \code{TRUE}, then content large enough to overflow the
#'   \code{contentPanel} will make scrollbars appear.
#'
#' @export
contentPanel <- function(..., padding = 10, scrollable = TRUE) {
  container <- if (scrollable) scrollPanel else identity

  htmltools::attachDependencies(
    container(
      tags$div(class = "gadget-content",
        tags$div(class = "gadget-absfill",
          style = sprintf("position: absolute; %s;", paddingToPos(padding)),
          ...
        )
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

# Copied verbatim from shiny, except replaced p_randomInt with sample.int
buildTabset <- function(tabs,
  ulClass,
  textFilter = NULL,
  id = NULL,
  selected = NULL) {

  # build tab nav list and tab content div

  # add tab input sentinel class if we have an id
  if (!is.null(id))
    ulClass <- paste(ulClass, "shiny-tab-input")

  tabNavList <- tags$ul(class = ulClass, id = id)
  tabContent <- tags$div(class = "tab-content")
  firstTab <- TRUE
  tabsetId <- sample.int(8999, 1) + 1000
  tabId <- 1
  for (divTag in tabs) {

    # check for text; pass it to the textFilter or skip it if there is none
    if (is.character(divTag)) {
      if (!is.null(textFilter))
        tabNavList <- tagAppendChild(tabNavList, textFilter(divTag))
      next
    }

    # compute id and assign it to the div
    thisId <- paste("tab", tabsetId, tabId, sep="-")
    divTag$attribs$id <- thisId
    tabId <- tabId + 1

    tabValue <- divTag$attribs$`data-value`

    # function to append an optional icon to an aTag
    appendIcon <- function(aTag, iconClass) {
      if (!is.null(iconClass)) {
        # for font-awesome we specify fixed-width
        if (grepl("fa-", iconClass, fixed = TRUE))
          iconClass <- paste(iconClass, "fa-fw")
        aTag <- tagAppendChild(aTag, icon(name = NULL, class = iconClass))
      }
      aTag
    }

    # check for a navbarMenu and handle appropriately
    if (inherits(divTag, "shiny.navbarmenu")) {

      # create the a tag
      aTag <- tags$a(href="#",
        class="dropdown-toggle",
        `data-toggle`="dropdown")

      # add optional icon
      aTag <- appendIcon(aTag, divTag$iconClass)

      # add the title and caret
      aTag <- tagAppendChild(aTag, divTag$title)
      aTag <- tagAppendChild(aTag, tags$b(class="caret"))

      # build the dropdown list element
      liTag <- tags$li(class = "dropdown", aTag)

      # build the child tabset
      tabset <- buildTabset(divTag$tabs, "dropdown-menu")
      liTag <- tagAppendChild(liTag, tabset$navList)

      # don't add a standard tab content div, rather add the list of tab
      # content divs that are contained within the tabset
      divTag <- NULL
      tabContent <- tagAppendChildren(tabContent,
        list = tabset$content$children)
    }
    # else it's a standard navbar item
    else {
      # create the a tag
      aTag <- tags$a(href=paste("#", thisId, sep=""),
        `data-toggle` = "tab",
        `data-value` = tabValue)

      # append optional icon
      aTag <- appendIcon(aTag, divTag$attribs$`data-icon-class`)

      # add the title
      aTag <- tagAppendChild(aTag, divTag$attribs$title)

      # create the li tag
      liTag <- tags$li(aTag)
    }

    if (is.null(tabValue)) {
      tabValue <- divTag$attribs$title
    }

    # If appropriate, make this the selected tab (don't ever do initial
    # selection of tabs that are within a navbarMenu)
    if ((ulClass != "dropdown-menu") &&
        ((firstTab && is.null(selected)) ||
            (!is.null(selected) && identical(selected, tabValue)))) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
      firstTab = FALSE
    }

    divTag$attribs$title <- NULL

    # append the elements to our lists
    tabNavList <- tagAppendChild(tabNavList, liTag)
    tabContent <- tagAppendChild(tabContent, divTag)
  }

  list(navList = tabNavList, content = tabContent)
}
