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
#'f<- function(x) {-1< x & x < 0, x+1, 0)}
#'oneDSample1(f,50000,-1,0)
#'
#'f<- function(x) {ifelse(0<x &x<1, x^3,0)}
#'oneDSample1(f,50000,-1,0)
#'
#'
oneDSample1<- function(f,N=10000,lb,ub,method='normal'){
  if (abs(integrate(f,lb,ub)$val)>1.001){
    stop("Error: This is not a valid pdf.The area under the function you given should be 1")
  }
  else{
    if (method=='normal'){
      x = runif(10000,-1000,1000)
      maxf= max(f(x))
      maxx = x[which(f(x)==max(f(x)))]
      minx = y[which(f(x)==min(f(x)))]
      alternatey=min((abs(minx-maxx)))
      sd = (2/max(f(x)))
      c= maxf/dnorm(maxx,maxx,sd)
      data.frame(x = replicate(N, {sx <- rnorm(1,maxx,sd); ifelse( runif(1,0,c*dnorm(1,maxx,sd)) < f(pSX), pSX, NA)}))
    }
    else if(metod=='unif'){
      if(lb!=Inf & ub!=Inf){
        maxf<-max(f(runif(10000,lb,ub)))+1
        data.frame(x = replicate(N, {pSX <- runif(1, lb, ub);ifelse(runif(1,0,maxf) < f(pSP), pSX, NA)}))
      }}
    else {stop('You input an invalid method.')}
  }
}


#sampleplot<- funtion(oneDsample1){
#    ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
#  }
