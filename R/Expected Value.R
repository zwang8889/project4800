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

Expected <- function(f,rv){
  L <- length(rv)
  if (L==1){
    meanval <- mean(rv$x,na.rm=T)
    return(meanval)
  }
  else if (L==2){
    meanval <- mean(f(rv$x,rv$y))
    return(meanval)
  }
  else {
    stop("Error: Please enter with a valid input format.")
  }
}
