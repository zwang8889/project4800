#' Double Variable Rejection Sampling
#'
#' This function implements two variables rejection sampling for rvs
#' with bounded support and which have a bounded pdf.
#'
#' Here are more details about the algorithm that we are using
#'
#'@param f the pdf that we are sampling from
#'@param N the number of attempted samples
#'@param lbx the lower bound of support x of f
#'@param lby the lower bound of support y of f
#'@param ubx the upper bound of support x of f
#'@param uby the upper bound of support y of f
#'
#'@return A vector containing samples from pdf
#'
#'@export
#'@examples
#'
#'
library(MASS)
library(ggplot2)
library(cubature)

# jointPFF <- function(x,y){
# x = x
# x = y
# ifelse(0<x1 & x1<1 & 0<x2 & x2<1 & 0<x1+x2 & x1+x2<1, 24*x1*x2, 0)}

twoDsample <- function(f, N=1000, lbx=-1000, ubx=1000, lby=-1000, uby=1000) {
  integral = adaptIntegrate(f,c(lbx,lby),c(ubx,uby),maxEval = 1000) $integral
  if (0.98>integral|integral>1.01) {
   stop("Error: not a pdf. The area under the function you given should be 1")
  }
  else if(lbx!=-1000&ubx!=1000&lby!=-1000&uby!=1000){
    pSX=runif(1,lbx,ubx)
    pSY=runif(1,lby,uby)
    two=c(pSX,pSY)
    maxf <- max(replicate(100000,f(c(runif(1,lbx,ubx),runif(1,lby,uby)))))
    twos=c()
    n=0
    while (n < N) {
      two <- c(runif(1,lbx,ubx),runif(1,lby,uby))
      if (runif(1, 0, maxf) < f(two)){
        twos = c(twos, two)
        n = n+1
      }
    }
    return(data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))]))}
  else{
    d_norm = function(x,mu,sig){
      x1 = x[1]
      x2 = x[2]
      mu1 = mu[1]
      mu2 = mu[2]
      sig1 = sig[1]
      sig2 = sig[2]
      exp(-1/2*((x1-mu1)^2/sig1^2 - 2*(x1-mu1)*(x2-mu2)/sig1/sig2 + (x2-mu2)^2/sig2^2))/(2*pi*sig1*sig2)
    }
    mid = c((ubx+lbx)/2,(uby+lby)/2)
    optimvalue = optim(mid,f, gr=NULL, lower = -Inf, upper = Inf, control = list(fnscale = -1))
    maxfvalue = optimvalue$value
    mu = c(optimvalue$par)
    sd = 2/maxfvalue
    C = maxfvalue/d_norm(c(mu[1],mu[2]),c(mu[1],mu[2]),c(sd,sd))
    twos = c()
    n = 0
    mat = matrix(c(sd,0,0,sd),2,2)
    
    while (n<N){
      two = mvrnorm(n=1,mu,mat)
      cond = C * d_norm(two, mu, c(sd,sd))
      if ( runif(1,0, cond) < f(two)){
        twos = c(twos,two)
        n = n+1
      }
    }
    return(data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))]))
  }
}