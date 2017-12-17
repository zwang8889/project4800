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

library(ggplot2)
twoDsample <- function(f, N, lbx=-Inf, ubx=Inf, lby=-Inf, uby=Inf) {
  integral = adaptIntegrate(f,c(lbx,lby),c(ubx,uby)) $integral
  if (0.98>integral|integral>1.01) {
   stop("Error: not a pdf. The area under the function you given should be 1")
 }
 else if(lbx!=-Inf&ubx!=Inf&lby!=-Inf&uby!=Inf){
   maxf <- max(replicate(100000,f(c(runif(1,lbx,ubx),runif(1,lby,uby)))))
   twos=c()
   pSX=runif(1,lbx,ubx)
   pSY=runif(1,lby,uby)
   two=c(pSX,pSY)
   sample = data.frame(x=replicate(5000,{pSX;pSY;if(runif(1,0,maxf)<f(two)){
  twos=c(twos,two)}}))
 }}



