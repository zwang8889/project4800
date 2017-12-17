#' Conditional probability calculation
#' 
#' 
#' Here are some details about our program
#' @param condition the condition that is given
#' @param rv the parameters we are entering
#' 
#' @return the value of probability
#' @export as numbers
#' 
#
#'f<- function(x) {ifelse(-1< x & x < 0, 3*x^2, 0)}
#'w=oneDSample1(f,50000,-1,0)
#'condition = function(x){x<-0.4}
#'probability(condition,w)
#'
#'f<- function(x) {1/pi/(1+x^2)}
#'w=oneDSample1(f,50000)
#'condition=function(x){x<4.5}
#'probability(f,w)

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


  
