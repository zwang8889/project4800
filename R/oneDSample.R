#' Single Variable Rejection Sampling
#' abcd
#' This function implements single variable rehection sampling for rvs
#' with bounded support and which have ahve bounded pdf.
#' We expect N/maxf sampling function
#'
#' Finds up to N samples fron on rv with pdf f.
#'
#' Here are more details about the algorithm that we are using
#'
#'@param f the pdf that we are sampling from
#'@param N the number of attempted samples
#'@param lb the lower bound of support f
#'@param nb the upper bound of support f
#'@param mxaf bound on f
#'
#'@return A vector containing samples from pdf
#'@export
#'
#'@examples
#'
#'betaPDF <- function(x){
#'  ifelse(0<x&x <1, 2*x, 0)}
#'oneDSAMPLE(f = betaPDF, N=100, lb=0, ub = 1, maxf = 2)
#'hist(oneDSAMPLE(f = betaPDF, N=100, lb=0, ub = 1, maxf = 2))
#'
#'
oneDSample <- function(f, N, lb, ub, maxf) {
#  browser()
  ones <- runif(N,lb,ub)
  unis <- runif(N,0, maxf)
  ones[unis < f(ones)]
}
