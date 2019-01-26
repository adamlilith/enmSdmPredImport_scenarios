#####################################################################
### functions to predict omniscient model with permuted interactions ###
#####################################################################
predictOmniscientPermIa <- cmpfun(function(model, data, sim, permProd=TRUE, permProdRule=NULL, ...) {

	# model			model object
	# data			data to which to predict
	# permProd  	TRUE ==> permute product features using T1 and T2 according to permProdRule; FALSE ==> predict as normal
	# permProdRule 	Rule for how permutation of product features is applied. 'before' ==> Permute individual variable values then calculate product. 'after' ==> Calculate product then permute across these values. Ignored if permProd is NULL.
	# ...		arguments to pass to model() function

	stopifnot(attr(model, 'modelType')=='full')
	
	## logistic model or not permuting
	##################################
	if (attr(model, 'equationType')=='logistic' | !permProd) {
	
		# returning normal prediction
		pred <- predictModel(model=model, data=data, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

	## gaussian model with permutation
	##################################
	} else if (attr(model, 'equationType')=='gaussian') {

		# get T1 and T2 permuted
		if (permProdRule=='before') {
		
			T1 <- sample(data$T1, nrow(data))
			T2 <- if ('T2' %in% names(data)) { sample(data$T2, nrow(data)) } else { rep(NA, nrow(data)) }

		# get T1 and T2 as-is
		} else if (permProdRule=='after') {

			T1 <- data$T1
			T2 <- data$T2

		}
		
		product <- (T1 - mu1) * (T2 - mu2)

		# permute after product
		if (permProdRule=='after') product <- sample(product, length(product))
		
		# get maximum product from across landscape for standardization
		maxT1 <- sim$geography$T1$max
		minT1 <- sim$geography$T1$min
		
		maxT2 <- sim$geography$T2$max
		minT2 <- sim$geography$T2$min

		extreme <- max(
			maxT1 * maxT2,
			minT1 * minT2,
			maxT1 * minT2,
			minT1 * maxT2
		)
		
		pred <- predOmniProd(x1=data$T1, x2=data$T2, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho, product=product, extreme=extreme)
		
		# p <- pFunProd(x1=data$T1, x2=data$T2, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho, product=product)
		# pMax <- pFunProd(x1=mu1, x2=mu2, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho, product=maxProd)
		# pred <- p / pMax

	} # gaussian model
		
	pred
	
})
