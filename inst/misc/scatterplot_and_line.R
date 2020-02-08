
dat <- data.frame(x = c(1,1,2,2), y = c(1,2,1,2), z = c(1,2,3,4))
dat$style <- I(list(
  list(fill="red", stroke="#000"),
  list(fill="red", stroke="#999"),
  list(fill="red", stroke="#999"),
  list(fill="red", stroke="#999")
))
graph3d(dat, style=~style, type = "dot-color", zMin = 0)

dat <- iris
dat$style <- I(lapply(iris$Species, function(x){
  switch(as.character(x),
         setosa     = list(fill="red", stroke="#000"),
         versicolor = list(fill="green", stroke="#000"),
         virginica  = list(fill="blue", stroke="#000"))
}))

graph3d(dat, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length,
        style = ~style, type = "dot-color", showLegend = FALSE)

#' dat$style <- I(lapply(iris$Species, function(x){
#'   switch(as.character(x),
#'          setosa     = list(fill="red", stroke="#'000"),
#'          versicolor = list(fill="green", stroke="#'000"),
#'          virginica  = list(fill="blue", stroke="#'000"))
#' }))
#'
#' graph3d(dat, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length,
#'         style = ~style, type = "dot-color", showLegend = FALSE)


t_ <- seq(0, 2*pi, length.out = 150)
dat <- data.frame(
  x = cos(t_),
  y = sin(t_),
  z = 2 * cos(3*t_)
)
graph3d(dat, type = "line", dataColor = list(strokeWidth = 3))


#' t_ <- seq(0, 2*pi, length.out = 150)
#' dat <- data.frame(
#'   x = cos(t_),
#'   y = sin(t_),
#'   z = 2 * cos(3*t_)
#' )
#' graph3d(dat, type = "line", dataColor = list(strokeWidth = 3))
