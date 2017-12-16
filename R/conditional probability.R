#' Conditional probability calculation
#' 
#' 
#' Here are some details about our program
#' @param condition the condition that is given
#' @param data the data we are entering
#' 
#' @return the value of probability
#' @export as numbers
#' 
#
f <- function(x) {ifelse(0 < x & x < 1, 4*x^3, 0)}
x1 = oneDSample1(f,10000)
c1 = function(x){x<0.4}
probability(c1,x1)

probability = function(condition,rv){
  length = length(rv)
  if(length==1){
    return(mean(condition(rv)))
  }
  else if(length==2){
    return(mean(condition(rv$x,rv$y)))
  }
  else{
    stop("This is not a valid formula.")
  }}


  
