dat <- expand.grid(
  alpha = round(seq(-4, 4, length.out = 51)[-51], 2),
  x = seq(-3, 3, length.out = 72),
  y = seq(-3, 3, length.out = 72)
)
dat <- transform(
  dat,
  density = 4*dnorm(x)*pnorm(abs(alpha)*x)*dnorm(y)*pnorm(abs(alpha)*y)
)
graph3d(dat, z = ~density, filter = ~alpha, showLegend = FALSE,
        keepAspectRatio = FALSE, verticalRatio = 1, tooltip = FALSE)

#####
dat <- expand.grid(
  alpha = round(seq(0, 2, length.out = 51)[-51], 2),
  x = seq(-3, 3, length.out = 72),
  y = seq(-3, 3, length.out = 72)
)
dat <- transform(
  dat,
  density = 4*dnorm(x)*dnorm(y) *
    ifelse(alpha < 1,
           pnorm((-4+8*alpha)*x)*pnorm((-4+8*alpha)*y),
           pnorm((4-8*(alpha-1))*x)*pnorm((4-8*(alpha-1))*y))
)
graph3d(dat, z = ~density, filter = ~alpha, showLegend = FALSE,
        keepAspectRatio = FALSE, verticalRatio = 1, tooltip = FALSE)
