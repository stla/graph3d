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
#' @examples # 3d histogram ####
#' dat <- data.frame(x = c(1,1,2,2), y = c(1,2,1,2), z = c(1,2,3,4))
#' graph3d(dat, type = "bar", zMin = 0)
#' # with custom tooltips
#' graph3d(dat, type = "bar", zMin = 0,
#'         tooltip = JS(c("function(xyz){",
#'                        "  var x = 'X: ' + xyz.x.toFixed(2);",
#'                        "  var y = 'Y: ' + xyz.y.toFixed(2);",
#'                        "  var z = 'Z: ' + xyz.z.toFixed(2);",
#'                        "  return  x + '<br/>' + y + '<br/>' + z;",
#'                        "}"))
#' )
#'
#' # bivariate Gaussian density ####
#' dat <- expand.grid(
#'   x = seq(-4,4,length.out=100),
#'   y = seq(-4,4,length.out=100)
#' )
#' dat <- transform(dat, density = dnorm(x)*dnorm(y))
#' graph3d(dat, z = ~density, keepAspectRatio = FALSE, verticalRatio = 1)
#'
#' # animation ####
#' f <- function(x, y) sin(x/50) * cos(y/50) * 50 + 50
#' t_ <- seq(0, 2*pi, length.out = 90)[-90]
#' x_ <- y_ <- seq(0, 314, length.out = 50)
#' dat <- expand.grid(x = x_, y = y_, t = t_)
#' dat <- transform(dat, z = f(x*cos(t) - y*sin(t), x*sin(t) + y*cos(t)))
#' graph3d(dat, frame = ~t, tooltip = FALSE)
graph3d <- function(data = NULL,
                    x = ~x, y = ~y, z = ~z, frame = NULL, style = NULL,
                    type = "surface",
                    surfaceColors =
                      c("#FF0000", "#FFF000", "#00FF00", "#68E8FB", "#000FFF"),
                    xlab = NULL, ylab = NULL, zlab = NULL,
                    xValueLabel = NULL, yValueLabel = NULL, zValueLabel = NULL,
                    width = "100%", height = "100%",
                    showPerspective = TRUE, showGrid = TRUE, showShadow = FALSE,
                    keepAspectRatio = TRUE, verticalRatio = 0.5,
                    tooltip = TRUE,
                    showLegend = TRUE, legendLabel = NULL,
                    cameraPosition = list(horizontal = 1,
                                          vertical = 0.5,
                                          distance = 2.8),
                    xMin = NULL, xMax = NULL,
                    yMin = NULL, yMax = NULL,
                    zMin = NULL, zMax = NULL,
                    showAnimationControls = TRUE, animationInterval = 100,
                    animationPreload = TRUE, frameLabel = NULL,
                    elementId = NULL) {
  dat <- data.frame(
    x = lazy_eval(x, data),
    y = lazy_eval(y, data),
    z = lazy_eval(z, data)
  )
  if(!is.null(frame)){
    dat[["filter"]] <- lazy_eval(frame, data)
  }
  if(!is.null(style)){
    dat[["style"]] <- lazy_eval(style, data)
  }
  if(is.null(xlab)) xlab <- f_text(x)
  if(is.null(ylab)) ylab <- f_text(y)
  if(is.null(zlab)) zlab <- f_text(z)
  # forward options using x
  X <- list(
    data = dat,
    options = dropNulls(list(
      showPerspective = showPerspective,
      showGrid = showGrid,
      showShadow = showShadow,
      keepAspectRatio = keepAspectRatio,
      verticalRatio = verticalRatio,
      showAnimationControls = showAnimationControls,
      animationInterval = animationInterval,
      animationPreload = animationPreload,
      filterLabel = frameLabel,
      width = width,
      height = height,
      style = type,
      surfaceColors = surfaceColors,
      tooltip = tooltip,
      showLegend = showLegend,
      legendLabel = legendLabel,
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
      xValueLabel = xValueLabel,
      yValueLabel = yValueLabel,
      zValueLabel = zValueLabel
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
