### normalized gaussian function ###
gaussian <- cmpfun(function(x1, x2=0, mu1=0, mu2=0, sigma1=0, sigma2=0, rho=0, ...) {

	# writing this piece-by-piece because putting it in one line causes errors when predicting to rasters
	first <- ((x1 - mu1) / sigma1)^2
	prod <- ((2 * rho * (x1 - mu1) * (x2 - mu2)) / (sigma1 * sigma2))
	second <- ((x2 - mu2) / sigma2)^2
	denom <- 2 * (1 - rho^2)

	inside <- first - prod + second
	inside <- (-1 * inside) / denom
	
	expo <- exp(inside)
	
	expo
	
})
attr(gaussian, 'equationType') <- 'gaussian'

