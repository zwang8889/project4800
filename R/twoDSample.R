#' Double Variable Rejection Sampling
#'
#' This function implements two variables rejection sampling for rvs
#' with bounded support and which have a bounded pdf.
#'
#' Here are more details about the algorithm that we are using
#'
#'@param f the pdf that we are sampling from
#'@param N the number of attempted samples
#'@param lbx the lower bound of support x of f
#'@param lby the lower bound of support y of f
#'@param ubx the upper bound of support x of f
#'@param uby the upper bound of support y of f
#'
#'@return A vector containing samples from pdf
#'
#'@export
#'@examples
#'
#'
#'
#'
#'


twoDsample <- function(f, N, lbx, ubx, lby, uby) {
  if (c(lbx,ubx), c(lby,uby), integral >0.01) {
   stop("Error: not a pdf. The area under the function you given should be 1")
 }
 else{
   for
     }
}


