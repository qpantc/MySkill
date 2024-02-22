
# 1.dlm类或list ----

# X the design matrix
# addInt logical: should an intercept be added?
# dV variance of the observation noise.
# dW diagonal elements of the variance matrix of the system noise.
# m0 m0, the expected value of the pre-sample state vector. C0 C0, the variance matrix of the pre-sample state vector.

# a function that takes a vector of the same length as parm and returns an object of class dlm, or a list that may be interpreted as such.
## initial function 

## 1.1 dlmModReg Create a DLM representation of a regression model --------
# The function creates a dlm representation of a linear regression model.
x <- matrix(runif(6,4,10), nc = 2); x
dlmModReg(x)
dlmModReg(x, addInt = FALSE)


## 1.2 dlmModPoly Create an n-th order polynomial DLM --------

# order order of the polynomial model. The default corresponds to a stochastic linear trend.
# dV variance of the observation noise.
# dW diagonal elements of the variance matrix of the system noise.
# m0 m0, the expected value of the pre-sample state vector. C0 C0, the variance matrix of the pre-sample state vector.

## the default
dlmModPoly()
## random walk plus noise
dlmModPoly(1, dV = .3, dW = .01)

nileBuild <- function(par) {
  dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2]))
}




## 1.3 dlmModSeas Create a DLM for seasonal factors --------
# The function creates a DLM representation of seasonal component.

# frequency how many seasons?
# dV variance of the observation noise.
# dW diagonal elements of the variance matrix of the system noise.
# m0 m0, the expected value of the pre-sample state vector. C0 C0, the variance matrix of the pre-sample state vector.

dlmModSeas(4, dV = 3.2)



## 1.4 dlmModARMA Create a DLM representation of an ARMA process --------
# The function creates an object of class dlm representing a specified univariate or multivariate ARMA process

# ar a vector or a list of matrices (in the multivariate case) containing the autoregres- sive coefficients.
# ma a vector or a list of matrices (in the multivariate case) containing the moving average coefficients.
# sigma2 the variance (or variance matrix) of the innovations.
# dV the variance, or the diagonal elements of the variance matrix in the multivariate case, of the observation noise. V is assumed to be diagonal and it defaults to zero.
# m0 m0, the expected value of the pre-sample state vector. C0 C0, the variance matrix of the pre-sample state vector.

## ARMA(2,3)
dlmModARMA(ar = c(.5,.1), ma = c(.4,2,.3), sigma2=1)
## Bivariate ARMA(2,1)
dlmModARMA(ar = list(matrix(1:4,2,2), matrix(101:104,2,2)),
           ma = list(matrix(-4:-1,2,2)), sigma2 = diag(2))




## 1.5 dlmModTrig Create Fourier representation of a periodic DLM component  ------
# The function creates a dlm representing a specified periodic component.

# s the period, if integer.
# q number of harmonics in the DLM. om the frequency.
# tau the period, if not an integer.
# dV variance of the observation noise.
# dW a single number expressing the variance of the system noise.
# m0 m0, the expected value of the pre-sample state vector. C0 C0, the variance matrix of the pre-sample state vector.

dlmModTrig(s = 3)
dlmModTrig(tau = 3, q = 1) # same thing
dlmModTrig(s = 4) # for quarterly data
dlmModTrig(s = 4, q = 1)
dlmModTrig(tau = 4, q = 2) # a bad idea!
m1 <- dlmModTrig(tau = 6.3, q = 2); m1
m2 <- dlmModTrig(om = 2 * pi / 6.3, q = 2)
all.equal(unlist(m1), unlist(m2))




## 1.6 self defined dlm list ------
### multivariate local level -- seemingly unrelated time series
buildSu <- function(x) {
  Vsd <- exp(x[1:2])
  Vcorr <- tanh(x[3])
  V <- Vsd %o% Vsd
  V[1,2] <- V[2,1] <- V[1,2] * Vcorr
  
  Wsd <- exp(x[4:5])
  Wcorr <- tanh(x[6])
  W <- Wsd %o% Wsd
  W[1,2] <- W[2,1] <- W[1,2] * Wcorr
  return(list(
    m0 = rep(0,2),
    C0 = 1e7 * diag(2),
    FF = diag(2),
    GG = diag(2),
    V = V,
    W = W))
}

## multivariate local level model with homogeneity restriction
buildHo <- function(x) {
  
  Vsd <- exp(x[1:2])
  Vcorr <- tanh(x[3])
  
  V <- Vsd %o% Vsd
  V[1,2] <- V[2,1] <- V[1,2] * Vcorr
  return(list(
    m0 = rep(0,2),
    C0 = 1e7 * diag(2),
    FF = diag(2),
    GG = diag(2),
    V = V,
    W = x[4]^2 * V))
}


# 2. dlm 求解： dlmMLE dlmFilter ----------------------------------------------------------------------------------------------------------
# get Parameter in maximum likelihood estimation
# The function returns the MLE of unknown parameters in the specification of a state space model.

# input 
# y a vector, matrix, or time series of data.
# parm vector of initial values - for the optimization routine - of the unknown parameters.
# build a function that takes a vector of the same length as parm and returns an object of class dlm, or a list that may be interpreted as such.
# method passed to optim.
# ... additional arguments passed to optim and build.
# debug if debug=TRUE, the likelihood calculations are done entirely in R, otherwise C functions are used.

# output
# The function dlmMLE returns the value returned by optim.

data(NelPlo)
plot(NelPlo)

## case 1: multivariate local level -- seemingly unrelated time series

suMLE <- dlmMLE(NelPlo, rep(0,6), buildSu); suMLE
# 应用最优参数
buildSu(suMLE$par)[c("V","W")]

StructTS(NelPlo[,1], type="level") ## compare with W[1,1] and V[1,1]
StructTS(NelPlo[,2], type="level") ## compare with W[2,2] and V[2,2]


## multivariate local level model with homogeneity restriction
hoMLE <- dlmMLE(NelPlo, rep(0,4), buildHo); hoMLE
buildHo(hoMLE$par)[c("V","W")]


## my test mod
x <- matrix(runif(6,4,10), nc = 2); x

my_mod <-  dlmModReg(x) + dlmModSeas(11)

suMLE <- dlmMLE(NelPlo, rep(0,6), dlmModARMA); suMLE
buildSu(suMLE$par)[c("V","W")]


#  -------------------------------------------------------------------------------------------------------
# The functions applies Kalman filter 
# to compute filtered values of the state vectors, together with their variance/covariance matrices. 
# By default the function returns an object of class "dlmFiltered". 
# Methods for residuals and tsdiag for objects of class "dlmFiltered" exist.

# dlmSvd2var to obtain a variance matrix from its SVD, 
# dlmMLE for maximum likelihood estimation, 
# dlmSmooth for Kalman smoothing, 
# dlmBSample for drawing from the posterior distribution of the state vectors.

nileMLE <- dlmMLE(Nile, rep(0,2), nileBuild); nileMLE$conv

nileMod <- nileBuild(nileMLE$par)

V(nileMod)
W(nileMod)

nileFilt <- dlmFilter(Nile, nileMod)
nileSmooth <- dlmSmooth(nileFilt)
plot(cbind(Nile, nileFilt$m[-1], nileSmooth$s[-1]), plot.type='s',
     col=c("black","red","blue"), ylab="Level", main="Nile river", lwd=c(1,2,2))


(ar <- ARtransPars(rnorm(5)))
all( Mod(polyroot(c(1,-ar))) > 1 ) # TRUE


# dlmModARMA Create a DLM representation of an ARMA process
## ARMA(2,3)
dlmModARMA(ar = c(.5,.1), ma = c(.4,2,.3), sigma2=1)
## Bivariate ARMA(2,1)
dlmModARMA(ar = list(matrix(1:4,2,2), matrix(101:104,2,2)),
           ma = list(matrix(-4:-1,2,2)), sigma2 = diag(2))

nileFilt <- dlmMLE(Nile, rep(0,2), dlmModARMA); nileFilt$conv
nileMod <- dlmModARMA(nileFilt$par)

nileFilt <- dlmFilter(Nile, nileMod)
nileSmooth <- dlmSmooth(nileFilt)
plot(cbind(Nile, nileFilt$m[-1], nileSmooth$s[-1]), plot.type='s',
     col=c("black","red","blue"), ylab="Level", main="Nile river", lwd=c(1,2,2))
