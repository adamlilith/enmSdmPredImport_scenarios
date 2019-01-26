### function to evaluate predictor importance using permuted predictors (multivariate models)

mainEval <- function(
	outDir,
	algos=c('omniscient', 'brt', 'gam', 'maxent', 'rf'),
	type=c('multivariate', 'reduced', 'univariate'),
	iters=1:100,
	perms=30,
	ia=TRUE,
	strat=FALSE,
	overwrite=TRUE,
	fileAppend=NULL,
	verbose=1,
	...
) {

	# outDir		base directory for this sim
	# landscape		raster stack of landscape variables
	# algos			name(s) of algorithms ('omniscient', 'brt', 'gam', 'maxent', 'rf')
	# type			type of models to evaluate ('multivariate', 'reduced', 'univariate')
	# iters			integer list of simulation iterations to perform (e.g., 1:100)--each will have a different set of presences and background sites
	# perms			integer number of times to permute each variable
	# ia			logical if TRUE then evaluate interaction importance (Maxent and omniscient only)
	# strat			logical if TRUE then conduct additional importance tests using background sites stratified by predicted species range... stratifications are defined by stretching the prediction map to have a range of [0, 1], defining 10 stratifications (0 to 0.1, 0.1 to 0.2, etc.), then selecting sim$stats$numBg / 10 sites randomly located within each stratification
	# fileAppend	character string to file name of saved file... leave as NULL to ignore
	# overwrite		logical, overwrite evaluation file if it exists
	# verbose		<=0: no display of progress; 1: some progress; 2: detailed progress; >=3: very detailed progress
	# ...			other arguments

	# file prefix
	fileAppendStartSpace <- if (!is.null(fileAppend)) { paste0(' ', fileAppend) } else { '' }
	fileAppendEndSpace <- if (!is.null(fileAppend)) { paste0(fileAppend, ' ') } else { '' }
	
	dirCreate(paste0(outDir, '/evaluations'))
	
	# by ALGORITHM
	for (algo in algos) {
	
		if (verbose > 0) say('Evaluating ', paste(toupper(type), collapse=' '), ' ', toupper(algo), ' models:', post=0)
		
		# if already done and not doing overwrite
		if (!overwrite & file.exists(paste0(outDir, '/evaluations/Evaluations for ', paste(type, collapse=' '), ' ', toupper(algo), fileAppendStartSpace, '.RData'))) {

			if (verbose > 0) say('Already done \U2713!', post=0)
		
		# if NOT already done or doing overwrite
		} else {
		
			# output data frame for this algorithm
			perform <- data.frame()
		
			# by ITERATION
			for (iter in iters) {
		
				say(iter, post=0)
		
				# load simulation data
				load(paste0(outDir, '/!scenario data/', fileAppendEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))

				testPres <- sim$testData$testPres
				testAbs <- sim$testData$testAbs
				testBg <- sim$testData$testBg

				b0 <- sim$stats$b0
				b1 <- sim$stats$b1
				b2 <- sim$stats$b2
				b11 <- sim$stats$b11
				b12 <- sim$stats$b12
				mu1 <- sim$stats$mu1
				mu2 <- sim$stats$mu2
				sigma1 <- sim$stats$sigma1
				sigma2 <- sim$stats$sigma2
				rho <- sim$stats$rho
				
				### REMEMBER
				thisPerform <- data.frame(
					iter = sim$iter,
					algo = algo,
					perms = perms,
					response = attributes(sim$response)$equationType
				)
				
				thisPerform <- cbind(thisPerform, sim$stats)
				
				# remember stats for landscape
				for (i in seq_along(sim$geography)) {
					
					# add 
					subOut <- data.frame(
						min=sim$geography[[i]]$min,
						max=sim$geography[[i]]$max,
						rot=if (sim$stats$circle) {
							if (exists('rot', where=sim$geography[[i]])) {
								sim$geography[[i]]$rot
							} else {
								0 
							}
						} else {
							NA
						}
					)
					
					names(subOut) <- paste0(names(subOut), names(sim$geography)[i])
					thisPerform <- cbind(thisPerform, subOut)
					
				}
					
				###########################
				### MULTIVARIATE MODELS ###
				###########################

				if ('multivariate' %in% type) {
					
					if (verbose > 1) say('mv', post=0)
					
					# load model
					load(paste0(outDir, '/multivariate ', algo, '/', algo, fileAppendStartSpace, ' model ', prefix(iter, 3), '.Rdata'))
					
					### OBSERVED: presences and background sites
					############################################
					
					if (class(model) != 'logical') {
						
						# compute observed predictions
						predPres <- predictModel(model, testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
						predAbs <- predictModel(model, testAbs, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
						predBg <- predictModel(model, testBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

						### OBSERVED: stratified sampling of background
						###############################################
						
						if (strat) {
							
							if (verbose > 1) say('strat', post=0)
							
							set.seed(sim$seed)
							# make landscape
							if (iter == 1) {
								landscape <- genesis(sim$geography, circle=sim$stats$circle, nrow=sim$stats$landscapeSize)
							# re-make any random layers for next sim
							} else if (any(unlist(geography) %in% 'random')) {
								landscape <- genesis(sim$geography, circle=sim$stats$circle, nrow=sim$stats$landscapeSize)
							}
							
							if ('randomForest' %in% class(model)) {
								predMap <- landscape[[1]]
								predMap[] <- predictModel(model, as.data.frame(landscape))
							} else {
								predMap <- raster::predict(landscape, model, fun=predictModel, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							}
							
							predMap <- stretch(predMap, 0, 1)
							predMap <- round(10 * predMap + 0.5 + .Machine$double.eps)
							stratSites <- sampleRastStrat(predMap, n=sim$stats$numBg / 10, adjArea=FALSE)
							stratBgEnv <- as.data.frame(extract(landscape, stratSites))
							predStratBg <- predictModel(model, stratBgEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							
						} # if doing stratified evaluation
						
						### OBSERVED PERFORMANCE
						######################
						
						aucPresAbs <- aucWeighted(pres=predPres, bg=predAbs, na.rm=TRUE)
						aucPresBg <- aucWeighted(pres=predPres, bg=predBg, na.rm=TRUE)
						cbi <- contBoyce(pres=predPres, bg=predBg, na.rm=TRUE, numBins=1001)

						subOut <- data.frame(aucPresAbs, aucPresBg, cbi)
						names(subOut) <- c('aucPresAbsMulti', 'aucPresBgMulti', 'cbiMulti')
						thisPerform <- cbind(thisPerform, subOut)
						
						### PERMUTATED VARIABLE IMPORTANCE
						##################################

						if (verbose > 2) say('perm', post=0)
						
						# variable names
						vars <- sim$vars

						# by EACH VARIABLE
						for (i in seq_along(vars)) {
						
							thisVar <- vars[i]
							if (verbose > 2) say(thisVar, post=0)
							
							aucPresAbsPerm <- aucPresBgPerm <- cbiPerm <- corPresAbsPerm <- corPresBgPerm <- rep(NA, perms)
							
							if (strat) {
								corStratBgPerm <- rep(NA, perms)
								thisStratBgEnv <- stratBgEnv
							}
							
							# by PERMUTATION
							for (perm in 1:perms) {
								
								# permute presences/absences
								combo <- sampleAcross(testPres, testAbs, by=thisVar)
								testPresPerm <- combo$testPres
								testAbsPerm <- combo$testAbs
								
								# permute presences/background
								combo <- sampleAcross(testPres, testBg, by=thisVar)
								testPresPerm <- combo$testPres
								testBgPerm <- combo$testBg

								# permute stratified sample
								if (strat) thisStratBgEnv[ , thisVar] <- sample(thisStratBgEnv[ , thisVar])
								
								# compute permuted predictions
								predPresPerm <- predictModel(model, testPresPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
								predAbsPerm <- predictModel(model, testAbsPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
								predBgPerm <- predictModel(model, testBgPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
								if (strat) predStratBgPerm <- predictModel(model, thisStratBgEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
								
								# evaluate
								aucPresAbsPerm[perm] <- aucWeighted(pres=predPresPerm, bg=predAbsPerm, na.rm=TRUE)
								aucPresBgPerm[perm] <- aucWeighted(pres=predPresPerm, bg=predBgPerm, na.rm=TRUE)
								cbiPerm[perm] <- contBoyce(pres=predPresPerm, bg=predBgPerm, na.rm=TRUE, numBins=1001)
								corPresAbsPerm[perm] <- corTest(c(predPres, predAbs), c(predPresPerm, predAbsPerm))
								corPresBgPerm[perm] <- corTest(c(predPres, predBg), c(predPresPerm, predBgPerm))
								if (strat) corStratBgPerm[perm] <- corTest(predStratBg, predStratBgPerm)
							
							} # next permutation
							
							# average across permutations
							aucPresAbsPerm <- mean(aucPresAbsPerm, na.rm=TRUE)
							aucPresBgPerm <- mean(aucPresBgPerm, na.rm=TRUE)
							cbiPerm <- mean(cbiPerm, na.rm=TRUE)
							corPresAbsPerm <- mean(corPresAbsPerm, na.rm=TRUE)
							corPresBgPerm <- mean(corPresBgPerm, na.rm=TRUE)
							if (strat) corStratBgPerm <- mean(corStratBgPerm, na.rm=TRUE)
							
							# remember
							subOut <- data.frame(aucPresAbsPerm, aucPresBgPerm, cbiPerm, corPresAbsPerm, corPresBgPerm)
							if (strat) subOut <- cbind(subOut, corStratPerm)
							names(subOut) <- gsub(names(subOut), pattern='Perm', replacement='')
							names(subOut) <- paste0(names(subOut), 'Multi_perm', thisVar)
							
							thisPerform <- cbind(thisPerform, subOut)
						
						} # next variable

					# model failed to converge
					} else {
					
						subOut <- data.frame(aucPresAbs=NA, aucPresBg=NA, cbi=NA)
						names(subOut) <- c('aucPresAbsMulti', 'aucPresBgMulti', 'cbiMulti')
						thisPerform <- cbind(thisPerform, subOut)

						for (thisVar in vars) {
						
							subOut <- data.frame(aucPresAbsPerm=NA, aucPresBgPerm=NA, cbiPerm=NA, corPresAbsPerm=NA, corPresBgPerm=NA)
							if (strat) subOut <- cbind(subOut=NA, corStratPerm=NA)
							names(subOut) <- gsub(names(subOut), pattern='Perm', replacement='')
							names(subOut) <- paste0(names(subOut), 'Multi_perm', thisVar)
							
							thisPerform <- cbind(thisPerform, subOut)

						}
							
					}
						
					### INTERACTION IMPORTANCE
					##########################
					
					if (ia && length(vars) > 1 && algo %in% c('omniscient', 'maxent')) {

						if (verbose > 2) say('ia', post=0)
					
						iaTest <- iaImport(
							model=model,
							vars=vars,
							sim=sim,
							perms=perms,
							pres=predPres,
							contrast=predBg,
							testPres=sim$testData$testPres,
							testContrast=sim$testData$testBg,
							...
						)
						
						names(iaTest) <- paste0(names(iaTest), 'Multi')
						thisPerform <- cbind(thisPerform, iaTest)

					}
					
					### NATIVE algorithm importance
					###############################

					# importance in MAXENT
					if (algo == 'maxent') {
						
						if (verbose > 2) say('mx:import', post=0)
						
						mxVars <- names(model@presence)
						
						for (count in seq_along(mxVars)) {
					
							thisVar <- mxVars[count]
					
							nativeImp <- data.frame(
								contribution=model@results[paste0(thisVar, '.contribution'), ] / 100,
								permImport=model@results[paste0(thisVar, '.permutation.importance'), ] / 100,
								trainGainWithout=model@results[paste0('Training.gain.without.', thisVar), ] / 100,
								trainGainWithOnly=model@results[paste0('Training.gain.with.only.', thisVar), ] / 100
							)
							
							names(nativeImp) <- c(
								paste0('maxentMultiContrib', thisVar),
								paste0('maxentMultiPermImport', thisVar),
								paste0('maxentMultiTrainGainWithout', thisVar),
								paste0('maxentMultiTrainGainWithOnly', thisVar)
							)
							
							names(nativeImp) <- paste0(names(nativeImp))
							thisPerform <- cbind(thisPerform, nativeImp)

						} # next layer
					
					# importance in BRT
					} else if (algo == 'brt') {
						
						if (verbose > 2) say('brt:import', post=0)
						
						if (class(model) != 'logical') {
						
							for (count in seq_along(model$var.names)) {
							
								thisVar <- model$var.names[count]
							
								imp <- if (class(model) != 'logical') {
									model$contributions$rel.inf[which(model$contributions$var == thisVar)] / 100
								} else {
									NA
								}
								
								nativeImp <- data.frame(import = imp)
									
								names(nativeImp) <- paste0('brtMultiImport', thisVar)
								thisPerform <- cbind(thisPerform, nativeImp)
							
							}
							
							## GBM interaction importance
							if (ia) {
								
								if (class(model) != 'logical') {
									brtIa <- gbm.interactions(model)$interactions
								} else {
									brtIa <- matrix(NA, nrow=2, ncol=2)
									rownames(brtIa) <- colnames(brtIa) <- vars
								}

								for (count1 in 1:(length(vars) - 1)) {
								
									var1 <- vars[count1]
								
									for (count2 in (count1 + 1):length(vars)) {
									
										var2 <- vars[count2]
								
										if (any(rownames(brtIa) %in% var1) & any(colnames(brtIa) %in% var2)) {
									
											nativeImp <- data.frame(DUMMY=brtIa[rownames(brtIa) == var1, colnames(brtIa) == var2])
											nativeImp[1, 1] <- nativeImp[1, 1] / 100
											names(nativeImp) <- paste0('brtMultiNativeIa_', var1, 'x', var2)
											thisPerform <- cbind(thisPerform, nativeImp)
											
										}
								
									}
								
								}
								
							}
							
						# BRT model failed to converge (assumes there has been at least *one* successful evaluation!)
						} else {
						
							missingColumns <- names(perform)[grepl(names(perform), pattern='brtMultiImport')]
							for (missingColumn in missingColumns) {
							
								missing <- data.frame(DUMMY=NA)
								names(missing) <- missingColumn
								thisPerform <- cbind(thisPerform, missing)
							
							}

							if (ia) {
							
								missingColumns <- names(perform)[grepl(names(perform), pattern='brtMultiNativeIa_')]
								for (missingColumn in missingColumns) {
								
									missing <- data.frame(DUMMY=NA)
									names(missing) <- missingColumn
									thisPerform <- cbind(thisPerform, missing)
								
								}
								
							}
						
						} # BRT model failed to converge
						
					} # native algorithm importance
					
				} # if doing multivariate models
				
				######################
				### REDUCED MODELS ###
				######################

				if ('reduced' %in% type) {
					
					if (verbose > 1) say('red', post=0)
					
					# load model
					load(paste0(outDir, '/reduced ', algo, '/', algo, fileAppendStartSpace, ' model ', prefix(iter, 3), '.Rdata'))
					
					### OBSERVED: normal
					####################
					
					for (countVar in seq_along(sim$vars)) {

						# if model converged
						if (class(model[[countVar]]) != 'logical') {
						
							# compute observed predictions
							predPres <- predictModel(model[[countVar]], testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predAbs <- predictModel(model[[countVar]], testAbs, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predBg <- predictModel(model[[countVar]], testBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

							# OBSERVED PERFORMANCE
							aucPresAbs <- aucWeighted(pres=predPres, bg=predAbs, na.rm=TRUE)
							aucPresBg <- aucWeighted(pres=predPres, bg=predBg, na.rm=TRUE)
							cbi <- contBoyce(pres=predPres, bg=predBg, na.rm=TRUE, numBins=1001)

							subOut <- data.frame(aucPresAbs, aucPresBg, cbi)
							
						} else {
						
							subOut <- data.frame(aucPresAbs=NA, aucPresBg=NA, cbi=NA)
						
						}
						
						names(subOut) <- c(
							paste0('aucPresAbsRed_sans', sim$vars[countVar]),
							paste0('aucPresBgRed_sans', sim$vars[countVar]),
							paste0('cbiRed_sans', sim$vars[countVar])
						)
						thisPerform <- cbind(thisPerform, subOut)
						
					}
						
				} # if doing reduced model
					
				#########################
				### UNIVARIATE MODELS ###
				#########################

				if ('univariate' %in% type) {
					
					if (verbose > 1) say('uni', post=0)
					
					# load model
					load(paste0(outDir, '/univariate ', algo, '/', algo, fileAppendStartSpace, ' model ', prefix(iter, 3), '.Rdata'))
					
					### OBSERVED: normal
					
					for (countVar in seq_along(model)) {

						thisVar <- names(model)[[countVar]]
						thisVar <- substr(thisVar, 5, nchar(thisVar))
						
						# if model converged
						if (class(model[[countVar]]) != 'logical') {
						
							# compute observed predictions
							predPres <- predictModel(model[[countVar]], testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predAbs <- predictModel(model[[countVar]], testAbs, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predBg <- predictModel(model[[countVar]], testBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

							# OBSERVED PERFORMANCE
							aucPresAbs <- aucWeighted(pres=predPres, bg=predAbs, na.rm=TRUE)
							aucPresBg <- aucWeighted(pres=predPres, bg=predBg, na.rm=TRUE)
							cbi <- contBoyce(pres=predPres, bg=predBg, na.rm=TRUE, numBins=1001)
						
							subOut <- data.frame(aucPresAbs, aucPresBg, cbi)
						
						} else {

							subOut <- data.frame(aucPresAbs=NA, aucPresBg=NA, cbi=NA)
						
						}
						
						names(subOut) <- c(
							paste0('aucPresAbsUni_only', thisVar),
							paste0('aucPresBgUni_only', thisVar),
							paste0('cbiUni_only', thisVar)
						)
						thisPerform <- cbind(thisPerform, subOut)
						
					} # next variable
					
				} # if doing univariate model
			
				perform <- rbind(perform, thisPerform)
				rownames(perform) <- 1:nrow(perform)
				if (verbose >= 3) { say(''); print(perform) }
			
			} # next ITERATION

			save(perform, file=paste0(outDir, '/evaluations/Evaluations for ', paste(type, collapse=' '), ' ', toupper(algo), fileAppendStartSpace, '.RData'))
			rm(perform); gc()
			
		} # if NOT already done or doing overwrite

		if (verbose > 0) say('')
		
	} # next ALGORITHM

}
