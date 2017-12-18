#' Conditional probability calculation
#'
#' The goal of the function is used to compute the probability of the given ramdon variables.
#'
#' @param condition the condition that is given
#' @param rv the parameters we are entering
#'
#' @return the value of probability
#' @export 
#' @examples
#'f<- function(x) {ifelse(-1< x & x < 0, 3*x^2, 0)}
#'w=oneDSample1(f,50000,-1,0)
#'condition = function(x){x<-0.4}
#'probability(condition,w)
#'
#'f<- function(x) {1/pi/(1+x^2)}
#'w=oneDSample1(f,50000)
#'condition=function(x){x<4.5}
#'probability(f,w)
#'
#'f <- function(x){
#' x1 = x[1]
#' x2 = x[2]
#' ifelse(x2>0, 1/pi/(1+x1^2) * 0.05*exp(-0.05*x2), 0)}
#' w=twoDsample(f = f, N=10000)
#' condition = function(x,y){x<0.1 & y<2}
#' probability(condition,w)
#' 
#' jointPFF <- function(x){
#' x1 = x[1]
#' x2 = x[2]
#' ifelse(0<x1 & x1<1 & 0<x2 & x2<1 , 24*x1*x2, 0)}
#' w = twoDsample(f = jointPFF, N=10000)
#'condition=function(x,y){x<0.1 & y<2}
#'probability(condition,w)


probability = function(condition,rv){
  length = length(rv)
  if(length==1){
    return(mean(condition(rv$x),na.rm=T))
  }
  else if(length==2){
    return(mean(condition(rv$x,rv$y),na.rm=T))
  }
  else{
    stop("This is not a valid formula.")
  }}



