A <- 30
D <- 100
xmid <- 50
B <- 5
S <- 25
C <- xmid + log(2^(1/S) - 1)/B
n <- 200
x <- seq(48, 52, length.out = n)
set.seed(31415)
y <- A + (D-A)/(1 + exp(log(2^(1/S)-1) + B*(xmid-x)))^S + rnorm(n, 0, 2)
plot(x, y)

BandS <- expand.grid(B = seq(0.2, 40, by = 0.4), S = seq(0.1, 50, by = 0.3))
BandS <- expand.grid(B = seq(4.5, 5.5, len = 50), S = seq(0.2, 30, by = 0.2))

loglhd <- function(y, A, B, xmid, D, S, sigma){
  sum(dnorm(y, mean = A + (D-A)/(1 + exp(log(2^(1/S)-1) + B*(xmid-x)))^S, sd = sigma, log = TRUE))
}

loglhd2 <- function(y, A, B, C, D, S, sigma){
  sum(dnorm(y, mean = A + (D-A)/(1 + exp(B*(C-x)))^S, sd = sigma, log = TRUE))
}

dat <- cbind(BandS, loglhd = apply(BandS, 1, function(BS){
  exp(loglhd(y, A, BS[1], xmid, D, BS[2], 2))
}))

dat2 <- cbind(BandS, loglhd = apply(BandS, 1, function(BS){
  exp(loglhd2(y, A, BS[1], C, D, BS[2], 2))
}))
dat2[which(dat2$loglhd == max(dat2$loglhd)),]

library(graph3d)
names(dat) <- c("x", "y", "z")
graph3d(dat, style = "surface", keepAspectRatio = FALSE)

library(drc)
fit <- drm(y ~ x, data = data.frame(x=x, y=y),
            fct = L.5(fixed = c(NA, A, D, xmid + log(2^(1/S) - 1)/B, NA),
                      names = c("minusB", "A", "D", "C", "S")))
coef(fit)

# drm(y ~ x, data = data.frame(x=x, y=y),
#     fct = L.5(fixed = c(NA, A, D, xmid + log(2^(1/S) - 1)/B, NA),
#               names = c("minusB", "A", "D", "C", "S")),
#     lowerl = c(-Inf,0), upperl = c(0, Inf))

library(minpack.lm)
fit <- nlsLM(
  y ~ A + (D-A)/(1 + exp(log(2^(1/S)-1) + B*(xmid-x)))^S,
  data = data.frame(x=x, y=y),
  start = c(B=3, S = 10),
  lower = c(0,0),
  control = nls.lm.control(maxiter = 1024, maxfev=10000))
coef(fit)


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
graph3d(dat, style = "surface", keepAspectRatio = FALSE)
dat[which(dat$z == min(dat$z)), ]

