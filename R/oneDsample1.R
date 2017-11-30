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

    
  }
  if ('uni' %in% method){
    
  }
  if ('stan' %in% method){
    
  }
  else {stop('You input an invalid method.')}
}
  sampleplot<- funtion(oneDsample1){
  ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
  }}
