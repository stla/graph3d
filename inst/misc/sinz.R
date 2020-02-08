dat <- expand.grid(
  x = seq(-1, 1, length.out = 100),
  y = seq(-1, 1, length.out = 100)
)
dat <- transform(dat, sine = sin(x + 1i*y))
dat <- transform(dat, modulus = Mod(sine), phase = Arg(sine))
graph3d(dat, z = ~modulus, style = ~phase, type = "dot-color", legendLabel = "phase")

#' dat <- expand.grid(
#'   x = seq(-1, 1, length.out = 100),
#'   y = seq(-1, 1, length.out = 100)
#' )
#' dat <- transform(dat, sine = sin(x + 1i*y))
#' dat <- transform(dat, modulus = Mod(sine), phase = Arg(sine))
#' graph3d(dat, z = ~modulus, style = ~phase, type = "dot-color",
#'         legendLabel = "phase")
