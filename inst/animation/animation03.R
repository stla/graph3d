dat <- expand.grid(
  p = round(seq(-1, 1, length.out = 51)[-51], 2),
  x = seq(-4, 4, length.out = 72),
  y = seq(-4, 7, length.out = 99)
)
dat <- transform(
  dat,
  density = abs(p)*dnorm(x)*dnorm(y) + (1-abs(p))*dnorm(x,0,0.7)*dnorm(y,4,0.7)
)
graph3d(dat, z = ~density, filter = ~ p, showLegend = FALSE,
        keepAspectRatio = FALSE, verticalRatio = 1, tooltip = FALSE)
