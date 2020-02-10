library(shiny)

dat <- data.frame(x = rnorm(30), y = rnorm(30), z = rnorm(30))

onclick <- c(
  "function(point){",
  "  Shiny.setInputValue('point', point);",
  "}"
)

ui <- fluidPage(
  br(),
  fluidRow(
    column(
      width = 4,
      h4("You clicked:"),
      verbatimTextOutput("pointClicked")
    ),
    column(
      width = 8,
      graph3dOutput("mygraph", height = "550px")
    )
  )
)

server <- function(input, output, session){

  output[["mygraph"]] <- renderGraph3d({
    graph3d(dat, type = "dot", width = "550px", height = "550px",
            onclick = JS(onclick), tooltip = FALSE)
  })

  output[["pointClicked"]] <- renderPrint({
    input[["point"]]
  })

}

shinyApp(ui, server)



#' library(shiny)
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


