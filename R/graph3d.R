dropNulls <- function(x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
#' @examples
#' dat <- data.frame(x = c(1,1,2,2), y = c(1,2,1,2), z = c(1,2,3,4))
#' graph3d(dat, style = "bar", zMin = 0)
graph3d <- function(data, width = "100%", height = "100%", style = "surface",
                    showPerspective = TRUE, showGrid = TRUE, showShadow = FALSE,
                    keepAspectRatio = TRUE, verticalRatio = 0.5,
                    tooltip = TRUE, showLegend = TRUE,
                    cameraPosition = list(horizontal = 1, vertical = 0.5, distance = 2.8),
                    xMin = NULL, xMax = NULL, yMin = NULL, yMax = NULL, zMin = NULL, zMax = NULL,
                    elementId = NULL) {

  # forward options using x
  x = list(
    data = data,
    options1 = list(
      showPerspective = showPerspective,
      showGrid = showGrid,
      showShadow = showShadow,
      keepAspectRatio = keepAspectRatio,
      verticalRatio = verticalRatio
    ),
    options2 = dropNulls(list(
      width = width,
      height = height,
      style = style,
      tooltip = tooltip,
      showLegend = showLegend,
      cameraPosition = cameraPosition,
      xMin = xMin,
      xMax = xMax,
      yMin = yMin,
      yMax = yMax,
      zMin = zMin,
      zMax = zMax
    ))
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'graph3d',
    x,
    width = NULL,
    height = NULL,
    package = 'graph3d',
    elementId = elementId
  )
}

#' Shiny bindings for graph3d
#'
#' Output and render functions for using graph3d within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a graph3d
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name graph3d-shiny
#'
#' @export
graph3dOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'graph3d', width, height, package = 'graph3d')
}

#' @rdname graph3d-shiny
#' @export
renderGraph3d <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, graph3dOutput, env, quoted = TRUE)
}
