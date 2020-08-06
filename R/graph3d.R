#' Objects imported from other packages
#' @description These objects are imported from other packages.
#'   Follow the links to their documentation:
#'   \code{\link[htmlwidgets:JS]{JS}},
#'   \code{\link[htmlwidgets:saveWidget]{saveWidget}}.
#' @importFrom htmlwidgets JS saveWidget
#' @export JS saveWidget
#' @name graph3d-imports
#' @aliases JS saveWidget
#' @docType import
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
#' @param xBarWidth,yBarWidth the widths of bars in x and y directions for
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
#' @param backgroundColor the background color of the chart, either a string
#' giving a HTML color (like \code{"red"} or \code{"#00CC00"}), or a list of
#' the form \code{list(fill="black", stroke="yellow", strokeWidth=3)};
#' \code{fill} is the chart fill color, \code{stroke} is the color of the chart
#' border, and \code{strokeWidth} is the border width in pixels
#' @param showPerspective logical; if \code{TRUE}, the graph is drawn in
#' perspective: points and lines which are further away are drawn smaller
#' @param showGrid logical; if \code{TRUE}, grid lines are drawn in the
#' x-y surface
#' @param showShadow logical, whether to show shadow on the graph
#' @param showXAxis logical; if \code{TRUE}, x-axis and x-axis labels are drawn
#' @param showYAxis logical; if \code{TRUE}, y-axis and y-axis labels are drawn
#' @param showZAxis logical; if \code{TRUE}, z-axis and z-axis labels are drawn
#' @param axisColor a HTML color given as a string, the color of the axis lines
#' and the text along the axes
#' @param gridColor a HTML color given as a string, the color of the grid lines
#' @param keepAspectRatio logical; if \code{TRUE}, the x-axis and the y-axis
#' keep their aspect ratio; if \code{FALSE}, the axes are scaled such that they
#' both have the same, maximum width
#' @param verticalRatio value between 0.1 and 1 which scales the vertical
#' size of the graph; when \code{keepAspectRatio=FALSE} and
#' \code{verticalRatio=1}, the graph will be a cube
#' @param tooltip logical, whether to see the tooltips, or a JavaScript
#' function to customize the tooltips; see the barplot example
#' @param tooltipDelay a number, the delay time in ms for the tooltip to appear
#' when the mouse cursor hovers over an x-y grid tile
#' @param tooltipStyle a list of tooltip style properties; see the vis-graph3d
#' documentation
#' @param showLegend logical, whether to see the legend if the graph type
#' supports it
#' @param legendLabel a string, the label of the legend
#' @param cameraPosition a list with three fields to set the initial rotation
#' and position if the camera: \code{horizontal}, a value in radians,
#' \code{vertical}, a value in radians between 0 and pi/2, and \code{distance},
#' the distance between 0.71 and 5 from the camera to the center of the graph
#' @param xCenter a string giving the horizontal center position of the graph
#' as a percentage (like \code{"50\%"}) or in pixels (like \code{"100px"});
#' default to \code{"55\%"}
#' @param yCenter same as \code{xCenter} for the vertical center position of
#' the graph; default to \code{"45\%"}
#' @param xMin minimum value for the x-axis; if not set, the smallest value of
#' \code{x} is used
#' @param xMax maximum value for the x-axis; if not set, the largest value of
#' \code{x} is used
#' @param yMin minimum value for the y-axis; if not set, the smallest value of
#' \code{y} is used
#' @param yMax maximum value for the y-axis; if not set, the largest value of
#' \code{y} is used
#' @param zMin minimum value for the z-axis; if not set, the smallest value of
#' \code{z} is used
#' @param zMax maximum value for the z-axis; if not set, the largest value of
#' \code{z} is used
#' @param xStep a number, the step size for the grid on the x-axis
#' @param yStep a number, the step size for the grid on the y-axis
#' @param zStep a number, the step size for the grid on the z-axis
#' @param showAnimationControls logical, only applicable when the graph
#' contains an animation (i.e. \code{frame} is not \code{NULL}), whether to
#' show the animation controls (buttons previous, start/stop, next, and a slider)
#' @param animationInterval a number, the animation interval in milliseconds;
#' default to 1000
#' @param animationPreload logical; if \code{FALSE}, the animation frames are
#' loaded as soon as they are requested; if \code{TRUE}, the animation frames
#' are automatically loaded in the background
#' @param frameLabel string, the label for the animation slider
#' @param onclick a JavaScript function to handle the click event on a point;
#' see the vis-graph3d documentation and the second example in
#' \code{\link{graph3d-shiny}}
#' @param elementId an id for the widget
#'
#' @import htmlwidgets
#' @importFrom lazyeval lazy_eval f_text is_formula f_lhs f_rhs
#'
#' @export
#' @details See the
#' \href{https://visjs.github.io/vis-graph3d/docs/graph3d/index.html#Configuration_Options}{vis-graph3d}
#' documentation.
#' @examples # 3d bar plot ####
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
                    axisColor = NULL, gridColor = NULL,
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
                    onclick = NULL,
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
      gridColor = gridColor,
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
      zValueLabel = zValueLabel,
      onclick = onclick
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
#' @param width,height dimensions, must be valid CSS units (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended
#' @param expr an expression that generates a \code{\link{graph3d}} HTML widget
#' @param env the environment in which to evaluate \code{expr}
#' @param quoted logical, whether \code{expr} is a quoted expression (with
#'   \code{quote()}); this is useful if you want to save an expression in a
#'   variable
#'
#' @examples if(interactive()) {
#'
#' # 'surfaceColors' example ####
#'
#' library(shiny)
#' library(viridisLite)
#' library(graph3d)
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
#'
#' }
#'
#' if(interactive()) {
#'
#' # 'onclick' example ####
#'
#' library(shiny)
#' library(graph3d)
#'
#' dat <- data.frame(x = rnorm(30), y = rnorm(30), z = rnorm(30))
#'
#' onclick <- c(
#'   "function(point){",
#'   "  Shiny.setInputValue('point', point);",
#'   "}"
#' )
#'
#' ui <- fluidPage(
#'   br(),
#'   fluidRow(
#'     column(
#'       width = 4,
#'       h4("You clicked:"),
#'       verbatimTextOutput("pointClicked")
#'     ),
#'     column(
#'       width = 8,
#'       graph3dOutput("mygraph", height = "550px")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session){
#'
#'   output[["mygraph"]] <- renderGraph3d({
#'     graph3d(dat, type = "dot", width = "550px", height = "550px",
#'             onclick = JS(onclick), tooltip = FALSE)
#'   })
#'
#'   output[["pointClicked"]] <- renderPrint({
#'     input[["point"]]
#'   })
#'
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
#' @name graph3d-shiny
#'
#' @importFrom htmlwidgets shinyWidgetOutput shinyRenderWidget
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
