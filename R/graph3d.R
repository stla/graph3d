#' @importFrom htmlwidgets JS saveWidget
#' @export JS saveWidget
NULL

dropNulls <- function(x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1L))]
}

#' 3D chart
#'
#' Generate an interactive 3D chart.
#'
#' @param data dataframe containing the data for the chart; if not \code{NULL},
#' the variables passed to \code{x}, \code{y}, \code{z}, \code{frame} and
#' \code{style} are searched among the columns of \code{data}
#' @param x a right-sided formula giving the variable for the locations of the
#' points on the x-axis; required
#' @param y a right-sided formula giving the variable for the locations of the
#' points on the y-axis; required
#' @param z a right-sided formula giving the variable for the locations of the
#' points on the z-axis; required
#' @param frame a right-sided formula giving the variable for the frames of the
#' animation; optional
#' @param style a right-sided formula required for \code{type="dot-color"}
#' and \code{type="dot-size"}; the variable given by this formula can be a
#' numeric vector for the data value appearing in the legend, or a list of
#' style properties; see the examples
#' @param type the type of the chart, one of \code{"bar"}, \code{"bar-color"},
#' \code{"bar-size"}, \code{"dot"}, \code{"dot-line"}, \code{"dot-color"},
#' \code{"dot-size"}, \code{"line"}, \code{"grid"}, or \code{"surface"}
#' @param surfaceColors a vector of colors for \code{type="surface"}, or a list
#' of the form
#' \code{list(hue = list(start=-360, end=360, saturation=50, brightness=100, colorStops=8))};
#' see the vis-graph3d documentation for more information
#' @param dataColor a string or a list; see the \code{type="line"} example and
#' the vis-graph3d documentation
#' @param xBarWidth,ybarWidth the widths of bars in x and y directions for
#' \code{type="bar"} and \code{type="bar-color"};
#' by default, the width is equal to the smallest distance between the data points
#' @param xlab string, the label on the x-axis
#' @param ylab string, the label on the y-axis
#' @param zlab string, the label on the z-axis
#' @param xValueLabel JavaScript function for custom formatting of the labels
#' along the x-axis, for example \code{JS("function(x){return (x * 100) + '\%'}")}
#' @param yValueLabel same as \code{xValueLabel} for the y-axis
#' @param zValueLabel same as \code{xValueLabel} for the z-axis
#' @param width,height the dimensions of the chart given as strings, in pixels
#' (e.g. \code{"400px"}) or percentages (e.g. \code{"80\%"})
#' @param backgroundColor
#' @param showPerspective
#' @param showGrid
#' @param showShadow
#' @param showXAxis
#' @param showYAxis
#' @param showZAxis
#' @param axisColor
#' @param keepAspectRatio
#' @param verticalRatio
#' @param tooltip
#' @param tooltipDelay
#' @param tooltipStyle
#' @param showLegend
#' @param legendLabel
#' @param cameraPosition
#' @param xCenter
#' @param yCenter
#' @param xMin
#' @param xMax
#' @param yMin
#' @param yMax
#' @param zMin
#' @param zMax
#' @param xStep
#' @param yStep
#' @param zStep
#' @param showAnimationControls
#' @param animationInterval
#' @param animationPreload
#' @param frameLabel
#' @param elementId
#'
#' @import htmlwidgets
#' @importFrom lazyeval lazy_eval f_text is_formula f_lhs f_rhs
#'
#' @export
#' @details See \url{https://visjs.github.io/vis-graph3d/docs/graph3d/index.html#Configuration_Options}.
#' @examples # 3d histogram ####
#' dat <- data.frame(x = c(1,1,2,2), y = c(1,2,1,2), z = c(1,2,3,4))
#' graph3d(dat, type = "bar", zMin = 0)
#' # change bar widths
#' graph3d(dat, type = "bar", zMin = 0, xBarWidth = 0.3, yBarWidth = 0.3)
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
#'
#' # scatterplot ####
#' dat <- iris
#' dat$style <- I(lapply(iris$Species, function(x){
#'   switch(as.character(x),
#'          setosa     = list(fill="red",   stroke="#'000"),
#'          versicolor = list(fill="green", stroke="#'000"),
#'          virginica  = list(fill="blue",  stroke="#'000"))
#' }))
#' graph3d(dat, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length,
#'         style = ~style, type = "dot-color", showLegend = FALSE)
#'
#' # line ####
#' t_ <- seq(0, 2*pi, length.out = 200)
#' dat <- data.frame(
#'   x = cos(t_),
#'   y = sin(t_),
#'   z = 2 * cos(3*t_)
#' )
#' graph3d(dat, type = "line", dataColor = list(strokeWidth = 5, stroke = "red"),
#'         verticalRatio = 1)
#'
#' # a complex function ####
#' dat <- expand.grid(
#'   x = seq(-1, 1, length.out = 100),
#'   y = seq(-1, 1, length.out = 100)
#' )
#' dat <- transform(dat, sine = sin(x + 1i*y))
#' dat <- transform(dat, modulus = Mod(sine), phase = Arg(sine))
#' graph3d(dat, z = ~modulus, style = ~phase, type = "dot-color",
#'         legendLabel = "phase")
graph3d <- function(data = NULL,
                    x = ~x, y = ~y, z = ~z, frame = NULL, style = NULL,
                    type = "surface",
                    surfaceColors =
                      c("#FF0000", "#FFF000", "#00FF00", "#68E8FB", "#000FFF"),
                    dataColor = NULL,
                    xBarWidth = NULL, yBarWidth = NULL,
                    xlab = NULL, ylab = NULL, zlab = NULL,
                    xValueLabel = NULL, yValueLabel = NULL, zValueLabel = NULL,
                    width = "100%", height = "100%",
                    backgroundColor = NULL,
                    showPerspective = TRUE, showGrid = TRUE, showShadow = FALSE,
                    showXAxis = TRUE, showYAxis = TRUE, showZAxis = TRUE,
                    axisColor = NULL,
                    keepAspectRatio = TRUE, verticalRatio = 0.5,
                    tooltip = TRUE, tooltipDelay = NULL, tooltipStyle = NULL,
                    showLegend = TRUE, legendLabel = NULL,
                    cameraPosition = list(horizontal = 1,
                                          vertical = 0.5,
                                          distance = 2.8),
                    xCenter = NULL, yCenter = NULL,
                    xMin = NULL, xMax = NULL,
                    yMin = NULL, yMax = NULL,
                    zMin = NULL, zMax = NULL,
                    xStep = NULL, yStep = NULL, zStep = NULL,
                    showAnimationControls = TRUE, animationInterval = 100,
                    animationPreload = TRUE, frameLabel = NULL,
                    elementId = NULL) {
  type <- match.arg(
    type,
    c(
      "surface",
      "line",
      "dot",
      "dot-line",
      "dot-color",
      "dot-size",
      "bar",
      "bar-color",
      "bar-size",
      "grid"
    )
  )
  if(!is_formula(x)){
    stop("`x` must be a right-sided formula.")
  }
  if(is.null(f_rhs(x)) || !is.null(f_lhs(x))){
    stop("`x` must be a right-sided formula.")
  }
  if(!is_formula(y)){
    stop("`y` must be a right-sided formula.")
  }
  if(is.null(f_rhs(y)) || !is.null(f_lhs(y))){
    stop("`y` must be a right-sided formula.")
  }
  if(!is_formula(z)){
    stop("`z` must be a right-sided formula.")
  }
  if(is.null(f_rhs(z)) || !is.null(f_lhs(z))){
    stop("`z` must be a right-sided formula.")
  }
  if(!is.null(data) && !is.element(w <- f_text(x), names(data))){
    stop(sprintf("Variable `%s` is not in the data.", w))
  }
  if(!is.null(data) && !is.element(w <- f_text(y), names(data))){
    stop(sprintf("Variable `%s` is not in the data.", w))
  }
  if(!is.null(data) && !is.element(w <- f_text(z), names(data))){
    stop(sprintf("Variable `%s` is not in the data.", w))
  }
  dat <- data.frame(
    x = lazy_eval(x, data),
    y = lazy_eval(y, data),
    z = lazy_eval(z, data)
  )
  if(!is.null(frame)){
    if(!is_formula(frame)){
      stop("`frame` must be a right-sided formula.")
    }
    if(is.null(f_rhs(frame)) || !is.null(f_lhs(frame))){
      stop("`frame` must be a right-sided formula.")
    }
    if(!is.null(data) && !is.element(w <- f_text(frame), names(data))){
      stop(sprintf("Variable `%s` is not in the data.", w))
    }
    dat[["filter"]] <- lazy_eval(frame, data)
  }
  if(!is.null(style)){
    if(!is_formula(style)){
      stop("`style` must be a right-sided formula.")
    }
    if(is.null(f_rhs(style)) || !is.null(f_lhs(style))){
      stop("`style` must be a right-sided formula.")
    }
    if(!is.null(data) && !is.element(w <- f_text(style), names(data))){
      stop(sprintf("Variable `%s` is not in the data.", w))
    }
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
      showXAxis = showXAxis,
      showYAxis = showYAxis,
      showZAxis = showZAxis,
      axisColor = axisColor,
      keepAspectRatio = keepAspectRatio,
      verticalRatio = verticalRatio,
      showAnimationControls = showAnimationControls,
      animationInterval = animationInterval,
      animationPreload = animationPreload,
      filterLabel = frameLabel,
      width = width,
      height = height,
      backgroundColor = backgroundColor,
      style = type,
      surfaceColors = surfaceColors,
      dataColor = dataColor,
      xBarWidth = xBarWidth,
      yBarWidth = yBarWidth,
      tooltip = tooltip,
      tooltipDelay = tooltipDelay,
      tooltipStyle = tooltipStyle,
      showLegend = showLegend,
      legendLabel = legendLabel,
      cameraPosition = cameraPosition,
      xCenter = xCenter,
      yCenter = yCenter,
      xMin = xMin,
      xMax = xMax,
      yMin = yMin,
      yMax = yMax,
      zMin = zMin,
      zMax = zMax,
      xStep = xStep,
      yStep = yStep,
      zStep = zStep,
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
#' @examples \donttest{library(shiny)
#' library(viridisLite)
#'
#' x <- y <- seq(-10, 10, length.out = 100)
#' dat <- expand.grid(x = x, y = y)
#' f <- function(x, y){
#'   r <- sqrt(x^2+y^2)
#'   10 * ifelse(r == 0, 1, sin(r)/r)
#' }
#' dat <- transform(dat, z = f(x, y))
#'
#' ui <- fluidPage(
#'   br(),
#'   fluidRow(
#'     column(
#'       width = 2,
#'       radioButtons("colors", "Colors",
#'                    c("viridis", "inferno", "magma", "plasma", "cividis"))
#'     ),
#'     column(
#'       width = 10,
#'       graph3dOutput("mygraph", height = "550px")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session){
#'
#'   Colors <- reactive({
#'     colors <- switch(
#'       input$colors,
#'       viridis = viridis(5),
#'       inferno = inferno(5),
#'       magma = magma(5),
#'       plasma = plasma(5),
#'       cividis = cividis(5)
#'     )
#'     substring(colors, 1L, 7L)
#'   })
#'
#'   output[["mygraph"]] <- renderGraph3d({
#'     graph3d(dat, surfaceColors = Colors(), showLegend = FALSE)
#'   })
#'
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @name graph3d-shiny
#'
#' @export
graph3dOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'graph3d', width, height,
                                 package = 'graph3d')
}

#' @rdname graph3d-shiny
#' @export
renderGraph3d <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, graph3dOutput, env, quoted = TRUE)
}
