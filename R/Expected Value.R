#' Expectation of a function of random variables
#'
#' This function is an implementation of computing expected value for ramdon variables given function f.
#'
#' @param f the function that we would need to compute its expected value
#' @param rv the random variabel would be given
#'
#' @return the expected value of the given function
#' @export
#'
#' @examples
#'f<- function(x) {ifelse(-1< x & x < 0, 3*x^2, 0)}
#'w=oneDSample1(f,50000,-1,0)
#'Expected(f,w)
#'
#'f<- function(x) {1/pi/(1+x^2)}
#'w=oneDSample1(f,50000)
#'Expected(f,w)
#'
#'#jointPFF <- function(x){
#x1 = x[1]
#x2 = x[2]
#ifelse(0<x1 & x1<1 & 0<x2 & x2<1 , 24*x1*x2, 0)}
#w = twoDsample(f = jointPFF, N=10000,0,1,0,1)
#g=function(x,y){x*y}
#Expected(g,w)
#
# f <- function(x){
# x1 = x[1]
# x2 = x[2]
# ifelse(x2>0, 1/pi/(1+x1^2) * 0.05*exp(-0.05*x2), 0)}
# w=twoDsample(f = f, N=10000)
#g=function(x,y){x+y}
# 'Expected(g,w)

Expected <- function(f,rv){
  L <- length(rv)
  if (L==1){
    meanval <- mean(rv$x,na.rm=T)
    return(meanval)
  }
  else if (L==2){
    meanval <- mean(f(rv$x,rv$y),na.rm=T)
    return(meanval)
  }
  else {
    stop("Error: Please enter with a valid input format.")
  }
}
