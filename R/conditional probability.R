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

probability = function(condition,data){
  length = length(data)
  if(length==1){
    return(mean(condition(data)))
  }
  else if(length==2){
    return(mean(condition(data$x,data$y)))
  }
  else{
    stop("This is not a valid formula.")
  }}


  
