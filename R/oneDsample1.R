#oneDsample
#f is the function
#lb is the lower bound
#ub is the upper bound
method<- readline(prompt='Please enter the method you want to use.[normal,uniform,standard only]:')
oneDSample1<- function(f,N=10000,lb,ub){
if (abs(integrate(f,lb,ub)$val)>1.001){
  stop("Error: This is not a valid pdf.The area under the function you given should be 1")
}
else{
  if ('no' %in% method){
    x = x[which(f(x)==max(f(x)))],
    y = x[which(f(x)==min(f(x)))],
  sampleData <- data.frame(t(sampleData))}
  
  if ('uni' %in% method){
    if(lb!=Inf & ub!=Inf){
      maxf<-max(f(runif(10000,lb,ub)))+1
      data.frame(x = replicate(N, {pSX <- runif(1, lb, ub);ifelse(runif(1,0,maxf) < f(pSP), pSX, NA)}))
  }}
  if ('stan' %in% method){
    sampleData <- replicate(50000,{pSX <- runif(1,lb,ub); ifelse(runif(1,0,maxf) < f(pSX),pSX,NA)})}
  
  else {stop('You input an invalid method.')}
}
  sampleplot<- funtion(oneDsample1){
  ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
  }}
