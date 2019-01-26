####################################################################################
### function to test importance of interaction terms in Maxent or omniscient models ###
####################################################################################
iaImport <- cmpfun(function(
	model,
	vars,
	sim,
	perms,
	testPres,
	testContrast,
	predPres=NULL,
	predContrast=NULL,
	...
) {

# iaImport Evaluates importance of interactions in Maxent and omniscient models.
#
# model				Object of class "MaxEnt" or "omniscient"
# vars				Names of variables to test interactions of
# perms				Number of times to permute data
# testPres			Data frame with environment at test presence sites
# testContrast		Data frame with environment at test background or absence sites
# predPres			Predictions at test presences (if NULL or not supplied then will be computed)
# predContrast		Predictions at test background or absence sites (if NULL or not supplied then will be computed)
# ...				To pass to predictModel() function

	# predict to observed data (if not supplied)
	if (is.null(predPres)) predPres <- predictModel(model, testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
	if (is.null(predContrast)) predContrast <- predictModel(model, testContrast, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

	### test interaction beteween each pair of variables

	# by FIRST VARIABLE
	for (countVar1 in seq_along(vars)[-length(vars)]) {
	
		var1 <- vars[countVar1]

		# by SECOND VARIABLE
		for (countVar2 in (countVar1 + 1):length(vars)) {
		
			var2 <- vars[countVar2]
		
			# by product permutation RULE
			for (permProdRule in c('before', 'after')) {
				
				# containers for results
				auc <- cbi <- cor <- rep(NA, perms)
		
				# by PERMUTATIION
				for (perm in 1:perms) {
			
					# test presences and test CONTRAST points: permute BEFORE product
					permPresContrast <<- if (class(model)=='MaxEnt') {
						
						predictMaxEnt(
							x=model,
							data=rbind(testPres, testContrast),
							type='cloglog',
							permProd=list(c(var1, var2)),
							permProdRule=permProdRule,
							...
						)
					
					} else if (class(model)=='function') {
					
						predictOmniscientPermIa(
							model=model,
							data=rbind(testPres, testContrast),
							sim=sim,
							permProd=('T1' %in% c(var1, var2) & 'T2' %in% c(var1, var2)),
							permProdRule=permProdRule,
							...
						)
							
					}

					### evaluate
					predPresPerm <- permPresContrast[1:nrow(testPres)]
					predContrastPerm <- permPresContrast[(nrow(testPres) + 1):length(permPresContrast)]

					# NOTE: using ***OBSERVED*** contrast predictions as background
					auc[perm] <- aucWeighted(pres=predPresPerm, bg=predContrast, na.rm=TRUE)
					cbi[perm] <- contBoyce(pres=predPresPerm, bg=predContrast, numBins=1001, na.rm=TRUE)
					cor[perm] <- corTest(c(predPres, predContrast), c(predPresPerm, predContrastPerm))
					
				} # by permutation

				# summarize and remember
				auc <- mean(auc, na.rm=TRUE)
				cbi <- mean(cbi, na.rm=TRUE)
				cor <- mean(cor, na.rm=TRUE)
			
				# remember
				thisOut <- data.frame(auc, cbi, cor)

				names(thisOut) <- paste0(names(thisOut), '_perm', capIt(permProdRule), paste(vars, collapse=''))
				if (permProdRule == 'before') {
					before <- thisOut
				} else if (permProdRule == 'after') {
					after <- thisOut
				}
				
			} # by product permutation RULE

			# remember
			out <- cbind(before, after)
			
		} # by 2nd variable
		
	} # by first variable
	
	out
	
})
