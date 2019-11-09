#' @importFrom htmlwidgets JS saveWidget
#' @export JS saveWidget
NULL

dropNulls <- function(x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#' @importFrom lazyeval lazy_eval f_text
#'
#' @export
#' @details See \url{https://visjs.github.io/vis-graph3d/docs/graph3d/index.html#Configuration_Options}.
#' @examples dat <- data.frame(x = c(1,1,2,2), y = c(1,2,1,2), z = c(1,2,3,4))
#' graph3d(dat, style = "bar", zMin = 0)
#' graph3d(dat, style = "bar", zMin = 0,
#'         tooltip = JS(c("function(xyz){",
#'                        "  var x = 'X: ' + xyz.x.toFixed(2);",
#'                        "  var y = 'Y: ' + xyz.y.toFixed(2);",
#'                        "  var z = 'Z: ' + xyz.z.toFixed(2);",
#'                        "  return  x + '<br/>' + y + '<br/>' + z;",
#'                        "}"))
#' )
#' dat <- expand.grid(
#'   x = seq(-4,4,length.out=100),
#'   y = seq(-4,4,length.out=100)
#' )
#' dat <- transform(dat, density = dnorm(x)*dnorm(y))
#' graph3d(dat, z = ~density, keepAspectRatio = FALSE, verticalRatio = 1)
graph3d <- function(data = NULL, x = ~x, y = ~y, z = ~z, filter = NULL,
                    xlab = NULL, ylab = NULL, zlab = NULL,
                    width = "100%", height = "100%", style = "surface",
                    showPerspective = TRUE, showGrid = TRUE, showShadow = FALSE,
                    keepAspectRatio = TRUE, verticalRatio = 0.5,
                    tooltip = TRUE, showLegend = TRUE,
                    cameraPosition = list(horizontal = 1,
                                          vertical = 0.5,
                                          distance = 2.8),
                    xMin = NULL, xMax = NULL, yMin = NULL, yMax = NULL,
                    zMin = NULL, zMax = NULL,
                    showAnimationControls = TRUE, animationInterval = 100,
                    animationPreload = TRUE,
                    elementId = NULL) {
  dat <- data.frame(
    x = lazy_eval(x, data),
    y = lazy_eval(y, data),
    z = lazy_eval(z, data)
  )
  if(!is.null(filter)){
    dat[["filter"]] <- lazy_eval(filter, data)
  }
  if(is.null(xlab)) xlab <- f_text(x)
  if(is.null(ylab)) ylab <- f_text(y)
  if(is.null(zlab)) zlab <- f_text(z)
  # forward options using x
  X <- list(
    data = dat,
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
      zMax = zMax,
      xLabel = xlab,
      yLabel = ylab,
      zLabel = zlab,
      showAnimationControls = showAnimationControls,
      animationInterval = animationInterval,
      animationPreload = animationPreload
    ))
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'graph3d',
    X,
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
