### logistic function ###
logistic <- cmpfun(function(x1, x2=0, b0=0, b1=1, b2=0, b12=0, ...) return(exp(b0 + b1 * x1 + b2 * x2 + b12 * x1 * x2) / (1 + exp(b0 + b1 * x1 + b2 * x2 + b12 * x1 * x2))))
attr(logistic, 'equationType') <- 'logistic'
logisticShift <- function(x1, x2=0, b0=0, b1=1, b11=0, ...) return(exp(b0 + b1 * (x1 - b11)) / (1 + exp(b0 + b1 * (x1 - b11))))
attr(logisticShift, 'equationType') <- 'logisticShift'

