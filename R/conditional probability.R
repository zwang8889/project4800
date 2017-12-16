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
#'condition = function(x){x<0.4}
#'probability(condition,w)

probability = function(condition,rv){
  length = length(rv)
  if(length==1){
    return(mean(condition(rv))
  }
  else if(length==2){
    return(mean(condition(rv$x,rv$y))
  }
  else{
    stop("This is not a valid formula.")
  }}


  
