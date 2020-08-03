# https://stackoverflow.com/questions/39945578/interpolation-of-2d-surface-in-3d-scene-javafx
f <- function(x, t){
  k <- 1
  1 / (1+16*t*t)^(1/4) *
    exp(-1/(1+4*t)^2 * (x-k*t)^2) *
    cos(0.5 * atan(-2*t) + 1/(1+4*t)^2 * (x*(k+2*x*t)) - k*k*t*t/4)
}

t_ <- seq(4, 8, length.out = 101)
x_ <- y_ <- seq(-15, 15, length.out = 50)
dat <- expand.grid(x = x_, y = y_, t = t_)
dat <- transform(dat, z = round(f(sqrt(x*x+y*y),t),12))

library(graph3d)
graph3d(dat, filter = ~t, tooltip = FALSE, verticalRatio = 1,
        showLegend = FALSE, showShadow = TRUE, showPerspective = FALSE)



