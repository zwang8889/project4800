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
