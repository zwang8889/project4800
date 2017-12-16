#' Single Variable Expected Value
#'
#' This function is an implementation of computing expected value for ramdon variables given pdf f.
#'
#' @param f the function that we will compute its expected value
#' @param rv the random variabel will be given
#'
#' @return the expected value of the given function
#' @export
#'
#' @examples
#'
#'
#'
#'
#'

Expected <- function(f,rv){
  L <- length(rv)
  if (len==1){
    meanval <- mean(f(rv))
    return(meanval)
  }

}
