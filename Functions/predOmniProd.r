#####################################################################
### functions to predict omniscient model with permuted interactions ###
#####################################################################
predOmniProd <- cmpfun(
	function(x1, x2, mu1, mu2, sigma1, sigma2, rho, product, extreme) {

		# product		pre-multiplied (and possibly permuted) product: (x1 - mu1) * (x2 - mu2)
		# extreme		most extreme value of product possible in this milueu... used to standardize result to (0, 1]
	
		# change sign of extreme product
		extreme <- ifelse(rho < 0, -abs(extreme), abs(extreme))
	
		# writing this piece-by-piece because putting it in one line causes errors when predicting to rasters
		first <- ((x1 - mu1) / sigma1)^2
		prod <- ((2 * rho * product) / (sigma1 * sigma2))
		second <- ((x2 - mu2) / sigma2)^2
		adjust <- 2 * (1 - rho^2)

		inside <- first - prod + second
		inside <- (-1 * inside) / adjust
		
		numer <- exp(inside)

		standard <- exp((2 * rho * extreme) / (2 * sigma1 * sigma2 * (1 - rho^2)))
		
		ans <- numer / standard
		
		return(ans)
	
	}
)
	
