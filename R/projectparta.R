#oneDsample
#f is the function
#lb is the lower bound
#ub is the upper bound
method<- readline(prompt='Please enter the method you want to use.
                  [exponential,uniform,standard only]:')
if (abs(integrate(f,lb,ub)$val)>1.001){
  stop("Error: This is not a valid pdf.The area under the function you given should be 1")
}
elseif{
  if ('exp' %in% method){
  }
  if ('uni' %in% method){
  }
  if ('stan' %in% method){}
  elseif {stop('You input an invalid method.')}
}
