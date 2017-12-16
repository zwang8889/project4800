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
 jointPFF <- function(x){
 x1 = x[1]
 x2 = x[2]
 ifelse(0<x1 & x1<1 & 0<x2 & x2<1 & 0<x1+x2 & x1+x2<1, 24*x1*x2, 0)}
 a <- twoDsample(f = jointPFF, lbx = 0, ubx = 1, lby = 0, uby = 1)
 ggplot(a, aes(x, y)) +  geom_density_2d()
#'
library(MASS)
library(cubature)
library(ggplot2)
twoDsample <- function(f, N, lbx=-Inf, ubx=Inf, lby=-Inf, uby=Inf) {
  integral = integrate(f,c(lbx,lby),c(ubx,uby)) $integral
  if (0.98>integral|integral>1.01) {
   stop("Error: not a pdf. The area under the function you given should be 1")
 }
 else if(lbx!=-Inf&ubx!=Inf&lby!=-Inf&uby!=Inf){
   maxf=max(f(c(runnif(5000,lbx,ubx)),c(runif(5000,lby,uby))))
   sample = replicate(5000,{pSX=runnif(1,lbx,ubx);pSY=runif(1,lby,uby);if(runif(1,lbx,uby)<f(x=pSX,y=pSY))c(x=NA,y=NA)})
   sampleframe=data.frame(t(sample))
 }
  else{
    x=runif(10000,-10000,10000)
    y=runif(10000,-10000,10000)
    maxf=
    
  }
}


