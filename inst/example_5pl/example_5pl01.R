n <- 25
x <- seq(48, 62, length.out = n)
A <- 30; D <- 100; B <- 5; xmid <- 55; S <- 25
( C <- xmid + log(2^(1/S) - 1)/B )
y0 <- A + (D-A) / (1 + exp(B*(C-x)))^S
curve(A + (D-A) / (1 + exp(B*(C-x)))^S, from = 48, to = 62, n = 300, col = red)
abline(v = xmid, col = "red", lwd = 2)
abline(h = (A+D)/2, col = "red", lwd = 2)

set.seed(3141)
y <- y0 + rnorm(n, 0, 6)
points(x, y)

library(drc)
fit_drc <- drm(y ~ x, data = data.frame(x=x, y=y),
           fct = L.5(fixed = c(NA, A, D, C, NA),
                     names = c("minusB", "A", "D", "C", "S")))
coef(fit_drc)
confint(fit_drc)

library(minpack.lm)
fit_minpack <- nlsLM(
  y ~ A + (D-A)/(1 + exp(B*(C-x)))^S,
  data = data.frame(x=x, y=y),
  start = c(B = 1, S = 1),
  lower = c(0,0),
  control = nls.lm.control(maxiter = 1024, maxfev=10000))
coef(fit_minpack)

lhd <- function(y, A, B, C, D, S, sigma){
  exp(sum(dnorm(y, A + (D-A)/(1 + exp(B*(C-x)))^S, sigma, log = TRUE)))
}

BandS <- expand.grid(
  B = seq(0.2, 30, length.out = 150),
  S = seq(0.1, 50, length.out = 150)
)

dat <- cbind(BandS, lhd = apply(BandS, 1, function(BS){
  lhd(y, A, BS[1], C, D, BS[2], 6)
}))

dat[which(dat$lhd == max(dat$lhd)), ]

library(graph3d)
graph3d(dat, ~B, ~S, ~lhd, type = "surface", keepAspectRatio = FALSE)


BandS <- expand.grid(
  B = seq(0.2, 6, length.out = 150),
  S = seq(0.1, 50, length.out = 150)
)

dat <- cbind(BandS, lhd = apply(BandS, 1, function(BS){
  lhd(y, A, BS[1], C, D, BS[2], 6)
}))

dat[which(dat$lhd == max(dat$lhd)), ]

library(graph3d)
graph3d(dat, ~B, ~S, ~lhd, type = "surface", keepAspectRatio = FALSE)







# RSS ###
rss <- function(y, A, B, C, D, S){
#  sum((y - (A + (D-A)/(1 + exp(log(2^(1/S)-1) + B*(xmid-x)))^S))^2)
  sum((y - (A + (D-A)/(1 + exp(B*(C-x)))^S))^2)
}
BandS <- expand.grid(B = seq(4.8, 5.2, len = 50), S = seq(10, 30, by = 0.2))
dat <- cbind(BandS, RSS = apply(BandS, 1, function(BS){
  rss(y, A, BS[1], C, D, BS[2])
}))

library(graph3d)
names(dat) <- c("x", "y", "z")
graph3d(dat, type = "surface", keepAspectRatio = FALSE)
dat[which(dat$z == min(dat$z)), ]

