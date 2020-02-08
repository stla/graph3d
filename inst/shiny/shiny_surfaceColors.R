library(shiny)
library(viridisLite)

x <- y <- seq(-10, 10, length.out = 100)
dat <- expand.grid(x = x, y = y)
f <- function(x, y){
  r <- sqrt(x^2+y^2)
  10 * ifelse(r == 0, 1, sin(r)/r)
}
dat <- transform(dat, z = f(x, y))

ui <- fluidPage(
  br(),
  fluidRow(
    column(
      width = 2,
      radioButtons("colors", "Colors",
                   c("viridis", "inferno", "magma", "plasma", "cividis"))
    ),
    column(
      width = 10,
      graph3dOutput("mygraph", height = "550px")
    )
  )
)

server <- function(input, output, session){

  Colors <- reactive({
    colors <- switch(
      input$colors,
      viridis = viridis(5),
      inferno = inferno(5),
      magma = magma(5),
      plasma = plasma(5),
      cividis = cividis(5)
    )
    substring(colors, 1L, 7L)
  })

  output[["mygraph"]] <- renderGraph3d({
    graph3d(dat, surfaceColors = Colors(), showLegend = FALSE,
            cameraPosition = list(horizontal = 1, vertical=0.5, distance = 1),
            width = "400px", height = "400px")
  })

}

shinyApp(ui, server)


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
