A <- 30
D <- 100
xmid <- 50
B <- 5
S <- 25
n <- 30
x <- seq(48, 52, length.out = n)
set.seed(314)
y <- A + (D-A)/(1 + exp(log(2^(1/S)-1) + B*(xmid-x)))^S + rnorm(n, 0, 2)
plot(x, y)

BandS <- expand.grid(B = seq(0.2, 40, by = 0.4), S = seq(0.1, 50, by = 0.3))
BandS <- expand.grid(B = seq(1, 9, by = 0.1), S = seq(20, 30, by = 0.2))

loglhd <- function(y, A, B, xmid, D, S, sigma){
  sum(dnorm(y, mean = A + (D-A)/(1 + exp(log(2^(1/S)-1) + B*(xmid-x)))^S, sd = sigma, log = TRUE))
}

dat <- cbind(BandS, loglhd = apply(BandS, 1, function(BS){
  exp(loglhd(y, A, BS[1], xmid, D, BS[2], 2))
}))


library(graph3d)
names(dat) <- c("x", "y", "z")
graph3d(dat, style = "surface")
