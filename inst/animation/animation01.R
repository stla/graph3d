f <- function(x, y) sin(x/50) * cos(y/50) * 50 + 50

t_ <- seq(0, 2*pi, length.out = 90)[-90]
x_ <- y_ <- seq(0, 314, length.out = 50)
dat <- expand.grid(x = x_, y = y_, t = t_)
dat <- transform(dat, z = f(x*cos(t) - y*sin(t), x*sin(t) + y*cos(t)))

library(graph3d)
graph3d(dat, filter = ~t, tooltip = FALSE)

dd <- subset(dat, t == 0)
graph3d(dd, tooltip = FALSE)
