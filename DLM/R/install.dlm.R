# install.packages("dlm")
library(dlm)

# first order polynomial model --------
# one of the simplest DLMs is the random walk plus noise model


## FF GG 为关系矩阵，V，W为 the observation and evolution variances 
dlm(FF = 1, V = 0.8, GG = 1, W = 0.1, m0 = 0, C0 = 100)


dlmModPoly(order = 1, dV = 0.8, dW = 0.1, C0 = 100)


myMod <- dlmModPoly()


FF(myMod)

W(myMod)

m0(myMod)

V(myMod) <- 0.8

myMod <- dlmModPoly() + dlmModSeas(4)

dlmModPoly(dV = 0.2, dW = c(0, 0.5)) %+% (dlmModSeas(4, dV = 0, dW = c(0, 0, 0.35)) + dlmModPoly(1, dV = 0.1, dW = 0.03))


# Time-varying models

u <- rnorm(25)
myMod <- dlmModReg(u, dV = 14.5)

myMod$JFF
head(myMod$X)


buildFun <- function(x) {
  dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2]))
}

fit <- dlmMLE(Nile, parm = c(0,0), build = buildFun)

fit$conv

dlmNile <- buildFun(fit$par)
V(dlmNile)

W(dlmNile)

StructTS(Nile, "level")



buildFun <- function(x) {
       m <- dlmModPoly(1, dV = exp(x[1]))
       m$JW <- matrix(1)
       m$X <- matrix(exp(x[2]), nc = 1, nr = length(Nile))
       j <- which(time(Nile) == 1899)
       m$X[j,1] <- m$X[j,1] * (1 + exp(x[3]))
   return(m)
}


fit <- dlmMLE(Nile, parm = c(0,0,0), build = buildFun)
fit$conv


dlmNileJump <- buildFun(fit$par)

V(dlmNileJump)

dlmNileJump$X[c(1, which(time(Nile) == 1899)), 1]


# 3 Filtering, smoothing and forecasting
nileJumpFilt <- dlmFilter(Nile, dlmNileJump)
plot(Nile, type = 'o', col = "seagreen")

lines(dropFirst(nileJumpFilt$m), type = 'o', pch = 20, col = "brown")

attach(nileJumpFilt)
v <- unlist(dlmSvd2var(U.C, D.C))
pl <- dropFirst(m) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(m) + qnorm(0.95, sd = sqrt(v[-1]))
detach()
lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")

# 3.2 Smoothing

nileJumpSmooth <- dlmSmooth(nileJumpFilt)
plot(Nile, type = 'o', col = "seagreen")
attach(nileJumpSmooth)
lines(dropFirst(s), type = 'o', pch = 20, col = "brown")
v <- unlist(dlmSvd2var(U.S, D.S))
pl <- dropFirst(s) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(s) + qnorm(0.95, sd = sqrt(v[-1]))
detach()
lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")


lGas <- log(UKgas)
dlmGas <- dlmModPoly() + dlmModSeas(4)
buildFun <- function(x) {
     diag(W(dlmGas))[2:3] <- exp(x[1:2])
     V(dlmGas) <- exp(x[3])
 return(dlmGas)
}
(fit <- dlmMLE(lGas, parm = rep(0, 3), build = buildFun))$conv

dlmGas <- buildFun(fit$par)
drop(V(dlmGas))
diag(W(dlmGas))[2:3]


gasSmooth <- dlmSmooth(lGas, mod = dlmGas)
x <- cbind(lGas, dropFirst(gasSmooth$s[,c(1,3)]))
colnames(x) <- c("Gas", "Trend", "Seasonal")
plot(x, type = 'o', main = "UK Gas Consumption")

# 3.3 Forecasting
gasFilt <- dlmFilter(lGas, mod = dlmGas)
gasFore <- dlmForecast(gasFilt, nAhead = 20)
sqrtR <- sapply(gasFore$R, function(x) sqrt(x[1,1]))
pl <- gasFore$a[,1] + qnorm(0.05, sd = sqrtR)
pu <- gasFore$a[,1] + qnorm(0.95, sd = sqrtR)
x <- ts.union(window(lGas, start = c(1982, 1)),
                +               window(gasSmooth$s[,1], start = c(1982, 1)),
                +               gasFore$a[,1], pl, pu)
plot(x, plot.type = "single", type = 'o', pch = c(1, 0, 20, 3, 3),
       col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"),
     ylab = "Log gas consumption")
legend("bottomright", legend = c("Observed","Smoothed (deseasonalized)",
                                   "Forecasted level", "90% probability limit"),
         bty = 'n', pch = c(1, 0, 20, 3, 3), lty = 1,
         col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"))


# Bayesian analysis of Dynamic Linear Models
plot(Nile, type = 'o', col = "seagreen")
nileFilt <- dlmFilter(Nile, dlmNile)
for (i in 1:10) {# 10 simulated "true" levels
  lines(dropFirst(dlmBSample(nileFilt)), col = "brown")
}


lmixnorm <- function(x, weights, means, sds) {
  log(crossprod(weights, exp(-0.5 * ((x - means) / sds)^2 + - log(sds))))
}


y <- arms(0, myldens = lmixnorm,
          indFunc = function(x,...) (x > (-100)) * (x < 100),
          n = 5000, weights = c(1, 3, 2),
          means = c(-10, 0, 10), sds = c(7, 5, 2))
summary(y)


library(MASS)
truehist(y, prob = TRUE, ylim = c(0, 0.08), bty = 'o')
curve(colSums(c(1, 3, 2) / 6 * dnorm(matrix(x, 3, length(x), TRUE),
            mean = c(-10, 0, 10), sd = c(7, 5, 2))),add = TRUE)
legend(-25, 0.07, "True density", lty = 1, bty = 'n')
