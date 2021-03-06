#' Single Variable Rejection Sampling
#'
#' This function implements one variables rejection sampling for rvs
#' with bounded support and which have a bounded pdf.
#'
#' Here are more details about the algorithm that we are using
#'
#'@param f the pdf that we are sampling from
#'@param N the number of attempted samples
#'@param lb the lower bound of support
#'@param ub the upper bound of support
#'
#'@return A vector containing samples from pdf
#'@export
#'
#'@examples
#'f<- function(x) {ifelse(-1< x & x < 0, 3*x^2, 0)}
#'w=oneDsample(f,50000,-1,0)
#'ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
#'
#'f<- function(x) {1/pi/(1+x^2)}
#'w=oneDSample1(f,50000)
#'ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
#'
#'f <- function(x) {dunif(x, -100, 100)}
#'a <- oneDSample1(f,500)
#'ggplot(a,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
#'
#'
#'f<- function(x){dnorm(x,-5000,5000)}
#'w=oneDSample1(f,50000)
#'ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
#'
oneDSample1<- function(f,N=10000,lb=Inf,ub=Inf,continuous=TRUE){
  if(continuous==T){
    val = integrate(f,lb,ub)$val
    if (0.98>val|val>1.001){
      stop("Error: This is not a valid pdf.The area under the function you given should be 1")
    }
    else{
      bound=seq(-50,50,0.0001)
      index=which(f(bound)>0)
      if (f(-50) == 0 & f(50) == 0 & mean(f(bound)) > 0){
        lb = min(bound[index])
        ub = max(bound[index])
      }
      if (f(-50) == 0 & f(50) > 0 & mean(f(bound)) > 0){
        lb = min(bdtest[index])
      }
      if (f(-50) > 0 & f(50) == 0 & mean(f(bound)) > 0){
        ub = max(bdtest[index])
      }
      if (lb != Inf & ub !=Inf){
        maxf<-max(f(runif(10000,lb,ub)))
        data.frame(x = replicate(N, {pSX <- runif(1, lb, ub);ifelse(runif(1,0,maxf) < f(pSX), pSX, NA)}))
      }
      else{
        x = runif(10000,-100,100)
        maxf= max(f(x))
        maxx = x[which(f(x)==max(f(x)))]
        minx = x[which(f(x)==min(f(x)))]
        alternatey=min((abs(minx-maxx)))
        sd = (2/max(f(x)))
        c= maxf/dnorm(maxx,maxx,sd)
        data.frame(x = replicate(10000, {pSX <- rnorm(1,maxx,sd); ifelse( runif(1,0,c*dnorm(1,maxx,sd)) < f(pSX), pSX, NA)}))
      }}}
  else{
    stop("You are not inputing a pdf.")
  }}
