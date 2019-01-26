## SDM PREDICTOR INFERENCE - FUNCTIONS
## Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org

### CONTENTS ###
### logistic function ###
### normalized gaussian function ###

### pre-generate non-random landscape layers ###
### calculate correlation between rotated landscape variables and make nice figure ###


############################################################################################################
### MASTER function to get training/test sites, calculate models, calculate importance, and save results ###
############################################################################################################

main <- function(
	species,
	response,
	landscape,
	geography,
	directory,
	numTrainPres=200,
	numTestPres=200,
	numBg=10000,
	iterToDo=iterToDo,
	maxPermIter=30,
	jFoldMax=100,
	algorithm='maxent',
	gamTensor=TRUE,
	suffix=NULL,
	verbose=1,
	plotResponse=TRUE,
	cbi=TRUE,
	interaction=TRUE,
	mu1=NA, mu2=NA, sigma1=NA, sigma2=NA, rho=NA,
	...
) {
	# species		raster with probability of species' presence
	# response		response function
	# landscape		raster stack with environmental variables
	# geography		list object with description of geography
	# directory		directory in which to save results
	# numTrainPres	number of training presences to use in model
	# numTestPres	number of test presences to use for model evaluation and predictor inference
	# numBg			number of background sites for model *training*
	# iterToDo		integer list of iterations to perform (e.g., 1:100)
	# maxPermIter	number of times to permute variable in permute test... results will be average across these permutations
	# jFoldMax		maximum number of j-folds for model evaluation... results will be average across j-folds
	# algorithm		name(s) of algorithms
	# gamTensor		TRUE construct interaction terms in GAM with tensor (te()) products; FALSE use s(x1, x2)
	# min, max		minimum and maximum values of environment on landscape
	# suffix		character string to paste onto file names
	# verbose		<=0: minimal display of progress; 1: some progress; 2: detailed progress; >=3: very detailed progress
	# plotResponse	TRUE ==> plot actual and modeled response curves for first iteration; FALSE ==> don't plot
	# cbi			TRUE ==> calculate Continuous Boyce Index (can be time-consuming if combined with testing for interaction importance)
	# interaction	TRUE ==> test for interaction importance (can be time-consuming)
	# mu1, mu2, sigma1, sigma2, rho		numeric values used for simulating species
	# ...			arguments (other than first variable) to pass to response()

	# for each SDM algorithm
	for (thisAlgorithm in algorithm) {

		if (verbose >= 0) say('Algorithm: ', toupper(thisAlgorithm), ' | ', date())
	
		# for each iteration sample landscape, train model, and evaluate
		for (iter in iterToDo) {

			tempDir <- makeTemp()
		
			if (verbose==1 & iter==1) {
				say('iteration ', iter, post=0)
			} else if (verbose==1 & iter > 1) {
				say(iter, post=0)
			} else if (verbose > 1) {
				say(date(), ' | ', toupper(thisAlgorithm), ' | iteration ', iter, post=0, pre=1)
			}

			### generate new random layers if needed
			########################################
			
			if (class(geography)=='list') { # if geography is a list
			
				remakeGeog <- if (iter > 1 & any(unlist(geography) %in% 'random')) { TRUE } else { FALSE }
				
			} else if (any(iter > 1 & geography %in% 'random')) { remakeGeog <- TRUE } else { remakeGeog <- FALSE } # if geography is character string
			
			if (remakeGeog) {
			
				names <- names(landscape)
				uniform <- calc(landscape[[1]], fun=function(x) { ifelse(is.na(x), NA, 1) }) # mask

				# get indices of random layers
				randIndex <- numeric()
				if (class(geography)=='list') {
					for (i in seq_along(geography)) {
						if (geography[[i]]$type=='random') randIndex <- c(randIndex, i)
					}
				} else {
					randIndex  <- which(geography=='random')
				}
			
				for (count in randIndex) {
					
					rand <- raster(matrix(runif(nrow(landscape)^2, geography[[randIndex]]$min, geography[[randIndex]]$max), nrow=nrow(landscape)))
					rand <- rand * uniform
					landscape[[count]] <- rand
					
				}
				
				names(landscape) <- names

			}
		
			### generate training/test and background sites
			###############################################

			# sample from areas with Pr(occ) >= minQuant
			# speciesQuant <- calc(species, fun=function(x) ifelse(x >= quantile(as.matrix(species), minQuant, na.rm=T), 1, NA))

			# create mask from which to sample (avoids inclusion of sites with NAs which throw errors in GAMs)
			mask <- sum(landscape)

			# estimate sample size needed to get sufficient presences and absences
			n <- round(1.5 * (numTrainPres + numTestPres)) # initial number of randomly located sites to draw
			prev <- cellStats(species, 'sum')  / ncell(species) # prevalence (including NA cells)
			while (n * prev < numTrainPres + numTestPres | n * (1 - prev) < numTestPres) { n <- round(n * 1.5) }

			# draw sites
			presAbs <- -Inf # initial sum of sampled presences
			while (sum(presAbs) < numTrainPres + numTestPres | sum(!presAbs) < numTestPres) {

				sites <- as.data.frame(randomPointsRobust(mask, n, prob=F))
				prOcc <- extract(species, sites)
				presAbs <- runif(nrow(sites)) <= prOcc
				
				n <- 1.5 * n
				
			}

			# training/test presences and background sites
			allPres <- sites[which(presAbs), ]
			trainPres <- allPres[1:numTrainPres, ]
			testPres <- allPres[(numTrainPres + 1):(numTrainPres + numTestPres), ]
			testAbs <- sites[which(!presAbs), ]
			testAbs <- testAbs[sample(rownames(testAbs), numTestPres), ]

			trainPresEnv <- as.data.frame(extract(landscape, trainPres))
			testPresEnv <- as.data.frame(extract(landscape, testPres))
			testAbsEnv <- as.data.frame(extract(landscape, testAbs))

			bgSitesTrain <- randomPointsRobust(mask, numBg, prob=F)
			bgEnvTrain <- as.data.frame(extract(landscape, bgSitesTrain))

			trainData <- cbind(c(rep(1, nrow(trainPresEnv)), rep(0, nrow(bgEnvTrain))), rbind(trainPresEnv, bgEnvTrain))
			names(trainData)[1] <- 'presBg'
			trainData <- trainData
			
			bgSitesTest <- randomPointsRobust(mask, numBg, prob=F)
			bgEnvTest <- as.data.frame(extract(landscape, bgSitesTest))

			# weigh presences equal to background sites
			modelWeights <- c(rep(1, nrow(trainPresEnv)), rep(nrow(trainPresEnv) / nrow(bgEnvTrain), nrow(bgEnvTrain)))

			### train full model
			####################

			if (verbose > 1) { say('| full model', post=0) }
			
			if (thisAlgorithm=='omniscient') {
			
				fullModel <- response
				attr(fullModel, 'modelType') <- 'full'
			
			} else if (thisAlgorithm=='glm') {
			
				fullModel <- trainGlm(
					data=trainData,
					resp = 'presBg',
					preds = names(trainData)[2:ncol(trainData)],
					family = 'binomial',
					use = 'glm',
					tooBig = 10E6,
					construct = FALSE,
					select = TRUE,
					quadratic = TRUE,
					cubic = TRUE,
					interaction = TRUE,
					interQuad = TRUE,
					presPerTermInitial = 10,
					presPerTermFinal = 20,
					initialTerms = Inf,
					w = TRUE,
					out = 'model',
					verbose = verbose > 2
				)

			} else if (thisAlgorithm=='maxent') {
			
				# tempDir <- makeTemp()

				fullModel <- trainMaxEnt(
					data=trainData,
					resp='presBg',
					preds=names(trainData)[2:ncol(trainData)],
					regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
					classes='default',
					testClasses=TRUE,
					forceLinear=TRUE,
					jackknife=TRUE,
					args='',
					out='model',
					anyway=TRUE,
					scratchDir='C:/ecology/!Scratch/_TEMP',
					verbose=(verbose > 2)
				)

				# delTemp(tempDir)
			
			} else if (thisAlgorithm=='maxnet') {
			
				fullModel <- trainMaxNet(
					data=trainData,
					resp='presBg',
					preds=names(trainData)[2:ncol(trainData)],
					regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
					classes='default',
					testClasses=TRUE,
					out='model',
					anyway=TRUE,
					verbose=verbose > 2
				)
			
			} else if (thisAlgorithm=='brt') {
			
				learn <- 0.01
				fullModel <- NULL

				while (is.null(fullModel)) {
					
					if (verbose > 2) { say(learn, '', post=0) }

					fullModel <- gbm.step(
						data=trainData,
						gbm.x=2:ncol(trainData),
						gbm.y='presBg',
						family='bernoulli',
						tree.complexity=3,
						learning.rate=learn,
						bag.fraction=0.5,
						max.trees=2000,
						step.size=50,
						plot.main=FALSE,
						plot.folds=FALSE,
						silent=verbose < 2,
						verbose=verbose >= 2,
						site.weights=modelWeights
					)
				
					learn <- learn / 10
					
				}
			
			} else if (thisAlgorithm=='gam') {
	
				# theseweights <- function(w) return(modelWeights)
	
				if (gamTensor) {
		
					gamFormula <- paste0('presBg ~ 1 + ', paste0('s(', names(landscape), ')', collapse=' + '))
					for (var1 in 1:(length(names(landscape)) - 1)) {
						for (var2 in (var1 + 1):length(names(landscape))) {
							gamFormula <- paste0(gamFormula, ' + te(', names(landscape)[var1], ', ', names(landscape)[var2], ')', collapse='')
						}
					}
					
				} else {
					
					gamFormula <- 'presBg ~ 1 + '
					for (var1 in 1:(length(names(landscape)) - 1)) {
						for (var2 in (var1 + 1):length(names(landscape))) {
							gamFormula <- paste0(gamFormula, ' + s(', names(landscape)[var1], ', ', names(landscape)[var2], ')', collapse='')
						}
					}
					
				}

				# remove function "weights" temporarily because causes an error in dredge()
				weightsFx <- weights
				weights <- NULL

				fullModel <- gam(
					formula=as.formula(gamFormula),
					family='binomial',
					data=trainData,
					weights=modelWeights,
					method='REML',
					optimizer=c('outer', 'newton'),
					scale=-1,
					select=TRUE,
					gamma=1,
					na.action='na.fail'
				)

				weights <- NULL
				
				modelWeights <<- modelWeights # declare global to avert error with dredge()
				
				# calculate all possible models to use later to get native importance
				gamMultiModel <- dredge(
					global.model=fullModel,
					rank='AICc',
					trace=FALSE
				)

				weights <- weightsFx
				
			}

			
			### train REDUCED models
			########################
			if (nlayers(landscape) > 2) {
			
				if (verbose > 1) { say('| reduced models ', post=0) }
			
				reducedModel <- list()

				for (count in 1:nlayers(landscape)) {

					reducedTrainData <- trainData[ , -which(names(landscape)[count]==names(trainData))]
					
					if (thisAlgorithm=='omniscient') {
					
						reducedModel[[count]] <- response
						attr(reducedModel[[count]], 'modelType') <- 'reduced'
						attr(reducedModel[[count]], 'reducedSans') <- names(landscape)[count]
					
					} else if (thisAlgorithm=='glm') {
					
						reducedModel[[count]] <- trainGlm(
							data=reducedTrainData,
							resp = 'presBg',
							preds = names(reducedTrainData)[2:ncol(reducedTrainData)],
							family = 'binomial',
							use = 'glm',
							tooBig = 10E6,
							construct = FALSE,
							select = TRUE,
							quadratic = TRUE,
							cubic = TRUE,
							interaction = TRUE,
							interQuad = TRUE,
							presPerTermInitial = 10,
							presPerTermFinal = 20,
							initialTerms = Inf,
							w = TRUE,
							out = 'model',
							verbose = verbose > 2
						)

					} else if (thisAlgorithm=='maxent') {
	
						# tempDir <- makeTemp()

						reducedModel[[count]] <- trainMaxEnt(
							data=reducedTrainData,
							resp='presBg',
							preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
							regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
							classes='default',
							testClasses=TRUE,
							forceLinear=TRUE,
							jackknife=TRUE,
							args='',
							out='model',
							anyway=TRUE,
							scratchDir='C:/ecology/!Scratch/_TEMP',
							verbose=(verbose > 2)
						)

						# delTemp(tempDir)
					
					} else if (thisAlgorithm=='maxnet') {
			
						reducedModel[[count]] <- trainMaxNet(
							data=reducedTrainData,
							resp='presBg',
							preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
							regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
							classes='default',
							testClasses=TRUE,
							out='model',
							anyway=TRUE,
							verbose=verbose > 2
						)
					
					} else if (thisAlgorithm=='brt') {
					
						learn <- 0.01
						donefit <- FALSE

						while (!donefit) {

							model <- gbm.step(
								data=reducedTrainData,
								gbm.x=2:ncol(reducedTrainData),
								gbm.y='presBg',
								family='bernoulli',
								tree.complexity=3,
								learning.rate=learn,
								bag.fraction=0.5,
								max.trees=2000,
								step.size=50,
								plot.main=FALSE,
								plot.folds=FALSE,
								silent=verbose < 3,
								verbose=verbose < 3,
								site.weights=modelWeights
							)
						
							if (!is.null(model)) {
								reducedModel[[count]] <- model
								donefit <- TRUE
							}
						
							learn <- learn / 10
							
						}
					
					} else if (thisAlgorithm=='gam') {

						names <- names(landscape)[-count]
					
						gamFormula <- paste0('presBg ~ 1 + ', paste0('s(', names, ', bs=\'cs\')', collapse=' + '))
						for (var1 in 1:(length(names) - 1)) {
							for (var2 in (var1 + 1):length(names)) {
								gamFormula <- paste0(gamFormula, ' + te(', names[var1], ', ', names[var2], ', bs=\'cs\')', collapse='')
							}
						}	

						reducedModel[[count]] <- gam(
							formula=as.formula(gamFormula),
							family='binomial',
							data=reducedTrainData,
							method='REML',
							optimizer=c('outer', 'newton'),
							scale=-1,
							select=TRUE,
							gamma=1,
							weights=modelWeights,
							na.action='na.fail'
						)
					
					}
					
				} # next reduced model
				
			} # if doing reduced models
			
			
			### train UNIVARIATE models
			###########################
			
			if (verbose > 1) { say('| univariate models', post=0) }
			
			univarModel <- list()
			
			for (count in 1:nlayers(landscape)) {
			
				univarTrainData <- trainData[ , c('presBg', names(trainData)[which(names(landscape)[count]==names(trainData))])]

				# omniscient model
				if (thisAlgorithm=='omniscient') {
				
					univarModel[[count]] <- response
					attr(univarModel[[count]], 'modelType') <- 'univariate'
					attr(univarModel[[count]], 'univarWith') <- names(landscape)[count]

				# GLM
				} else if (thisAlgorithm=='glm') {
				
					univarModel[[count]] <- trainGlm(
						data=univarTrainData,
						resp = 'presBg',
						preds = names(univarTrainData)[2:ncol(univarTrainData)],
						family = 'binomial',
						use = 'glm',
						tooBig = 10E6,
						construct = FALSE,
						select = TRUE,
						quadratic = TRUE,
						cubic = FALSE,
						interaction = FALSE,
						interQuad = FALSE,
						presPerTermInitial = 10,
						presPerTermFinal = 20,
						initialTerms = Inf,
						w = TRUE,
						out = 'model',
						verbose = verbose > 2
					)

				# maxent
				} else if (thisAlgorithm=='maxent') {

					# tempDir <- makeTemp()

					univarModel[[count]] <- trainMaxEnt(
						data=univarTrainData,
						resp='presBg',
						preds=names(univarTrainData)[2:ncol(univarTrainData)],
						regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
						classes='lqh',
						testClasses=TRUE,
						forceLinear=TRUE,
						jackknife=TRUE,
						args='',
						out='model',
						anyway=TRUE,
						scratchDir='C:/ecology/!Scratch/_TEMP',
						verbose=(verbose > 2)
					)
					# delTemp(tempDir)

				# MaxNet
				} else if (thisAlgorithm=='maxnet') {
		
					univarModel[[count]] <- trainMaxNet(
						data=univarTrainData,
						resp='presBg',
						preds=names(univarTrainData)[2:ncol(univarTrainData)],
						regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
						classes='lqh',
						testClasses=TRUE,
						out='model',
						anyway=TRUE,
						verbose=verbose > 2
					)
					
				### BRTs
				} else if (thisAlgorithm=='brt') {
				
					brtUnivTrainData <- univarTrainData
					brtUnivTrainData$DUMMY <- rep(1, nrow(brtUnivTrainData))

					learn <- 0.01
					donefit <- FALSE
					
					while (!donefit) {

						model <- gbm.step(
							data=brtUnivTrainData,
							gbm.x=2:ncol(brtUnivTrainData),
							gbm.y='presBg',
							family='bernoulli',
							tree.complexity=3,
							learning.rate=learn,
							bag.fraction=0.5,
							max.trees=2000,
							step.size=50,
							plot.main=FALSE,
							plot.folds=FALSE,
							silent=verbose < 3,
							verbose=verbose < 3,
							site.weights=modelWeights
						)
					
						if (!is.null(model)) {
							univarModel[[count]] <- model
							donefit <- TRUE
						}
					
						learn <- learn / 10
						
					}
					
				### GAMs
				} else if (thisAlgorithm=='gam') {
				
					gamFormula <- paste0('presBg ~ 1 + s(', names(landscape)[count], ')')

					univarModel[[count]] <- gam(
						as.formula(gamFormula),
						family='binomial',
						data=trainData,
						method='REML',
						optimizer=c('outer', 'newton'),
						scale=-1,
						select=TRUE,
						gamma=1,
						weights=modelWeights,
						na.action='na.fail'
					)
				
				} # if doing GAMs
				
			} # next univariate model

			delTemp(tempDir)
			
			### generate response curve plots ###
			#####################################

			if (iter==1 & plotResponse) {

				if (verbose > 1) { say('| response plot', post=0) }
			
				if (thisAlgorithm!='omniscient') {

					png(paste0(directory, '/model output - ', thisAlgorithm, ifelse(is.null(suffix), '', paste0(' - ', suffix)), '.png'), height=1000, width=1000, res=300)

						par(bty='n', fg=fg, bg=bg, mai=rep(0.025, 4), oma=c(0, 0, 0, 0), col.main=fg, mgp=0.2 * c(3, 1, 0))
						
						# make prediction with full model then classify into bins
						fullModelRast <- if (thisAlgorithm=='maxent') {
							predict(fullModel, landscape, na.rm=T)
						} else if (thisAlgorithm=='maxnet') {
							raster::predict(landscape, fullModel, fun=maxnet:::predict.maxnet, na.rm=T, type='cloglog')
						} else {
							predict(landscape, fullModel, response=TRUE, n.trees=model$gbm.call$best.trees, na.rm=T)
						}
						
						speciescol <- c(fg, col2) # low/high
						col <- colorRampPalette(speciescol)
						col <- col(100)
						
						plot(
							fullModelRast,
							legend=FALSE,
							axes=FALSE,
							breaks=seq(0, 1, length.out=length(col)),
							maxpixels=500000,
							main='Full Model'
						)
						
					dev.off()
					
				}

				png(paste0(directory, '/response curves - ', thisAlgorithm, ifelse(is.null(suffix), '', paste0(' - ', suffix)), '.png'), height=1000, width=1000, res=300)

					num <- 31 # number of points to make predictions at
					
					col=c('black', 'darkred', 'darkblue', 'darkgreen', 'darkyellow', 'darkpurple', 'red', 'blue', 'green', 'yellow', 'purple')
					lty <- c('solid', 'dashed', 'longdash', 'dotdash', 'dotted')
					cex <- 0.4
					# cex <- 0.8
					
					lty <- c('solid', 'blank')
					if (nlayers(landscape) > 2) lty <- c(lty, 'blank')
					lty <- c(lty, 'blank')
					lty <- rep(lty, nlayers(landscape))

					pch <- c(NA, 15); if (nlayers(landscape) > 2) { pch <- c(pch, 12) }; pch <- c(pch, 0)
					pch <- c(pch, NA, 16); if (nlayers(landscape) > 2) { pch <- c(pch, 10) }; pch <- c(pch, 1)
					if (nlayers(landscape) >= 3) { pch <- c(pch, NA, 17); if (nlayers(landscape) > 2) { pch <- c(pch, 11) }; pch <- c(pch, 2) }
					if (nlayers(landscape) >= 4) { pch <- c(pch, NA, 18); if (nlayers(landscape) > 2) { pch <- c(pch, 14) }; pch <- c(pch, 5) }
					if (nlayers(landscape) >= 5) { pch <- c(pch, NA, 15); if (nlayers(landscape) > 2) { pch <- c(pch, 12) }; pch <- c(pch, 0) }
					if (nlayers(landscape) >= 6) { pch <- c(pch, NA, 16); if (nlayers(landscape) > 2) { pch <- c(pch, 10) }; pch <- c(pch, 1) }
					if (nlayers(landscape) >= 7) { pch <- c(pch, NA, 17); if (nlayers(landscape) > 2) { pch <- c(pch, 11) }; pch <- c(pch, 2) }
					if (nlayers(landscape) >= 8) { pch <- c(pch, NA, 15); if (nlayers(landscape) > 2) { pch <- c(pch, 12) }; pch <- c(pch, 0) }
					
					## empty plot
					xmin <- min(as.numeric(unlist(geography)[which(names(unlist(geography)) %in% 'min')]))
					xmax <- max(as.numeric(unlist(geography)[which(names(unlist(geography)) %in% 'max')]))

					par(pty='m', mar=c(5, 4, 4, 2) / 1.6, oma=c(1, 1, 1, 1) * 0.1)
					
					plot(
						0, 0,
						xlim=c(xmin, xmax),
						ylim=c(0, 1),
						xlab='Variable',
						ylab='Response',
						col='white',
						cex.lab=0.6,
						cex.axis=0.5,
						mgp=par('mgp')/3,
						tcl=-0.2
					)

					## plot actual response and modeled response

					# generate dataFrame frame with mean value of each variable for use in predicting response
					meanDf <- data.frame(DUMMY=rep(1, num))
					for (count in 1:nlayers(landscape)) {
						
						thisMeanDf <- data.frame(x=rep(cellStats(landscape[[count]], 'mean'), num))
						names(thisMeanDf) <- names(landscape[[count]])
						meanDf <- cbind(meanDf, thisMeanDf)
						
					}

					counter <- 1 # index for col, lty, etc.

					# plot true response to each factor
					for (count in 1:nlayers(landscape)) {

						# find range of this variable across landscape
						xmin <- cellStats(landscape[[count]], 'min')
						xmax <- cellStats(landscape[[count]], 'max')

						# get x and y values
						x <- seq(xmin, xmax, length.out=num)
						if (grepl(x=names(landscape[[count]]), pattern='T')) {
						
							y <- if (!debug) {
									response(x1=if (count==1) { x } else { meanDf[ , 2] }, x2=if (count==2) { x } else { meanDf[ , 3] }, ...)
							} else if (debug & modelType=='logistic') {
								response(x1=if (count==1) { x } else { meanDf[ , 2] }, x2=if (count==2) { x } else { meanDf[ , 3] }, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12)
							} else if (debug & modelType=='gaussian') {
								response(x1=if (count==1) { x } else { meanDf[ , 2] }, x2=if (count==2) { x } else { meanDf[ , 3] }, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
							}
							
						} else {
							y <- rep(0.5, num)
						}

						# actual response
						lines(
							x, y,
							lwd=2,
							col=alpha(col[count], ifelse(grepl(x=names(landscape[[count]]), pattern='T'), 1, 0.5)),
							lty=lty[counter]
						)
						
						counter <- counter + 1
						
						# modeled response
						deltaMeanDf <- meanDf
						deltaMeanDf[ , names(landscape[[count]])] <- x
						
						predFull <- predictModel(model=fullModel, dataFrame=deltaMeanDf, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

						if (nlayers(landscape) > 2) {
							
							predReduced <- predictModel(model=reducedModel[[count]], dataFrame=deltaMeanDf, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
						
						}
						
						predUnivar <- predictModel(model=univarModel[[count]], dataFrame=deltaMeanDf, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

						# full model
						points(
							x,
							predFull,
							lwd=0.8,
							col=alpha(col[count], ifelse(grepl(x=names(landscape[[count]]), pattern='T'), 1, 0.5)),
							pch=pch[counter],
							cex=cex
						)

						counter <- counter + 1
						
						# reduced model
						if (nlayers(landscape) > 2) {
						
							points(
								x,
								predReduced,
								lwd=0.8,
								col=alpha(col[count], ifelse(grepl(x=names(landscape[[count]]), pattern='T'), 1, 0.5)),
								pch=pch[counter],
								cex=cex * 2
							)
						
							counter <- counter + 1
							
						}

						# univariate model
						points(
							x,
							predUnivar,
							lwd=0.8,
							col=alpha(col[count], ifelse(grepl(x=names(landscape[[count]]), pattern='T'), 1, 0.5)),
							pch=pch[counter],
							cex=cex * 2
						)

						counter <- counter + 1

					} # next variable

					# legend
					leg <- rep(names(landscape), each=ifelse(nlayers(landscape)==2, 3, 4))
					cond <- c('(true response)', '(full model)')
					if (nlayers(landscape) > 2) cond <- c(cond, '(reduced model)')
					cond <- c(cond, '(univ model)')
					leg <- paste(leg, cond)
					
					lwd <- c(2, 0.8)
					if (nlayers(landscape) > 2) lwd <- c(lwd, 0.8)
					lwd <- c(lwd, 0.8)
					lwd <- rep(lwd, nlayers(landscape))
					
					legend(
						x='bottomright',
						inset=0.01,
						legend=leg,
						lty=lty,
						col=alpha(rep(col[1:nlayers(landscape)], each=ifelse(nlayers(landscape) > 2, 4, 3)), rep(ifelse(grepl(names(landscape), pattern='T'), 1, 0.5), each=ifelse(nlayers(landscape) > 2, 4, 3))),
						lwd=lwd,
						pch=pch,
						cex=cex
					)
					
				dev.off()
				
			}

			### predict to TEST PRESENCES and TEST ABSENCES and evaluate using PRESENCES and ABSENCES
			#########################################################################################
			
			if (verbose > 1) { say('| pres vs abs', post=0) }
			
			# full models: one object for presences and one for absences
			# reduced models: making another list object with one sub list per model algorithm, each with one set of predictions per predictor
			# univariate models: yet another with sublists, one per model algorithm, each with one set of predictions per predictor
			
			### FULL model vs presences and absences

			predTestPresFull <- predictModel(fullModel, testPresEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
			predTestAbsFull <- predictModel(fullModel, testAbsEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
			predTestBgFull <- predictModel(fullModel, bgEnvTest, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

			### evaluate FULL model using presences and absences
			isna <- whichIsNaVec(predTestPresFull, predTestAbsFull)
			if (length(isna) > 0) {
				p <- predTestPresFull[-isna]
				a <- predTestAbsFull[-isna]
			} else {
				p <- predTestPresFull
				a <- predTestAbsFull
			}

			evalAbsFull <- evaluateMod(p=as.vector(p), a=as.vector(a), tr=seq(0, 1, by=0.01))
			if (cbi) cbiFull <- contBoyce(predAtPres=predTestPresFull, predAtBg=predTestBgFull, numClasses=10)
			if (cbi) cbiFullEs <- contBoyce(predAtPres=predTestPresFull, predAtBg=predTestBgFull, ecospat=TRUE)
			
			### full model with PERMUTED values vs presences and absences
			#############################################################
			
			if (verbose > 1) { say('| permuted pres vs abs', post=0) }
			
			aucAbsPerm <- corAbsFullVsPerm <- cbiPerm <- cbiPermEs <- list() # store AUC and COR and CBI test values, will *eventually* have one element per predictor
	
			for (count in 1:nlayers(landscape)) {

				aucAbsPerm[[count]] <- corAbsFullVsPerm[[count]] <- cbiPerm[[count]] <- cbiPermEs[[count]] <- rep(NA, maxPermIter)
				
				for (permIter in 1:maxPermIter) {

					# permute test pres/abs for AUC
					testPermSites <- rbind(testPresEnv, testAbsEnv)
					testPermSites[ , count] <- sample(testPermSites[ , count], nrow(testPermSites))
			
					testPresEnvPerm <- testPermSites[1:nrow(testPresEnv), ]
					testAbsEnvPerm <- testPermSites[(nrow(testAbsEnv) + 1):(nrow(testAbsEnv) + nrow(testAbsEnv)), ]

					predPresPerm <- predictModel(fullModel, testPresEnvPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
					predAbsPerm <- predictModel(fullModel, testAbsEnvPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

					eval <- evalCorrTest(predTestPresFull=predTestPresFull, predNonpresFull=predTestAbsFull, predPres=predPresPerm, predNonpres=predAbsPerm)
					aucAbsPerm[[count]][permIter] <- eval$eval@auc
					corAbsFullVsPerm[[count]][permIter] <- eval$cor

					# CBI... NOTE CBI is calculated using permuted test presence and permuted background predictions
					if (cbi) {
						
						testPermSites <- rbind(testPresEnv, bgEnvTest)
						testPermSites[ , count] <- sample(testPermSites[ , count], nrow(testPermSites))
				
						testPresEnvPerm <- testPermSites[1:nrow(testPresEnv), ]
						testBgEnvPerm <- testPermSites[(nrow(bgEnvTest) + 1):(nrow(bgEnvTest) + nrow(bgEnvTest)), ]

						predPresPerm <- predictModel(fullModel, testPresEnvPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
						predBgPerm <- predictModel(fullModel, testBgEnvPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, sigma1=sigma1, sigma2=sigma2, rho=rho)

						cbiPerm[[count]][permIter] <- contBoyce(predAtPres=predPresPerm, predAtBg=predBgPerm, numClasses=10)
						cbiPermEs[[count]][permIter] <- contBoyce(predAtPres=predPresPerm, predAtBg=predBgPerm, ecospat=TRUE)
												
					}

				} # next permutation iteration
					
			} # next landscape layer

			aucAbsPerm <- sapply(aucAbsPerm, mean)
			corAbsFullVsPerm <- sapply(corAbsFullVsPerm, mean)
			cbiPerm <- sapply(cbiPerm, mean, na.rm=T)
			cbiPermEs <- sapply(cbiPermEs, mean, na.rm=T)

			names(aucAbsPerm) <- names(corAbsFullVsPerm) <- names(cbiPerm) <- names(cbiPermEs) <- names(landscape)
			
			### evaluate interaction importance
			if (interaction & (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient')) {
					
				iaTestResults <- testIa(
					model=fullModel,
					landscape=landscape,
					predTestPresFull=predTestPresFull,
					predTestContrastFull=predTestAbsFull,
					testPresEnv=testPresEnv,
					testContrastEnv=testAbsEnv,
					allBgEnv=if (cbi) { bgEnvTest } else { NULL },
					maxPermIter=maxPermIter
				)

				aucAbsPermIA_permBeforeProd <- iaTestResults$aucPermBeforeProd
				corAbsPermIA_permBeforeProd <- iaTestResults$corPermBeforeProd
					
				aucAbsPermIA_permAfterProd <- iaTestResults$aucPermAfterProd
				corAbsPermIA_permAfterProd <- iaTestResults$corPermAfterProd
				
				cbiPermIA_permBeforeProd <- iaTestResults$cbiPermBeforeProd
				cbiPermIA_permAfterProd <- iaTestResults$cbiPermAfterProd

				cbiEsPermIA_permBeforeProd <- iaTestResults$cbiEsPermBeforeProd
				cbiEsPermIA_permAfterProd <- iaTestResults$cbiEsPermAfterProd

			}
			
			### evaluate REDUCED vs presences and absences
			##############################################
			
			if (nlayers(landscape) > 2) {
				
				aucAbsReduced <- corAbsFullVsReduced <- numeric() # store AUC anc COR test values, will have one element per predictor
				predTestPresReduced <- list() # predictions at test presences, one list item per predictor... remembering for later
				cbiReduced <- rep(NA, nlayers(landscape)) # store CBI, one element per predictor

				for (count in 1:nlayers(landscape)) {
			
					predTestPresReduced[[count]] <- predictModel(reducedModel[[count]], testPresEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
					predAbs <- predictModel(reducedModel[[count]], testAbsEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

					eval <- evalCorrTest(predTestPresFull=predTestPresFull, predNonpresFull=predTestAbsFull, predPres=predTestPresReduced[[count]], predNonpres=predAbs)
					
					# consolidate results
					aucAbsReduced <- c(aucAbsReduced, eval$eval@auc)
					corAbsFullVsReduced <- c(corAbsFullVsReduced, eval$cor)
				
					# necessary only if using predictions at background from REDUCED model for CBI
					if (cbi) {
					
						predBg <- predictModel(reducedModel[[count]], bgEnvTest, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
						
						# # CBI
						# # using predictions at background sites from FULL model
						# cbiReduced[count] <- contBoyce(predAtPres=predTestPresReduced[[count]], predAtBg=predTestBgFull, numClasses=10)
					
						# CBI
						# using predictions at background sites from REDUCED model
						cbiReduced[count] <- contBoyce(predAtPres=predTestPresReduced[[count]], predAtBg=predBg, numClasses=10)
						cbiEsReduced[count] <- contBoyce(predAtPres=predTestPresReduced[[count]], predAtBg=predBg, ecospat=TRUE)
				
					}

				}
				
				names(aucAbsReduced) <- names(corAbsFullVsReduced) <- names(cbiReduced) <- names(cbiEsReduced) <- names(landscape)

			} # if doing reduced model


			### evaluate UNIVARIATE vs presences and absences
			#################################################

			aucAbsUnivar <- corAbsFullVsUnivar <- cbiUnivar <- cbiEsUnivar <- rep(NA, nlayers(landscape)) # store AUC and COR and CBI test values, will have one element per predictor
			predTestPresUnivar <- list() # predictions at test presences, one list item per predictor... remembering for later

			for (count in 1:nlayers(landscape)) {

				predTestPresUnivar[[count]] <- predictModel(univarModel[[count]], testPresEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
				predAbs <- predictModel(univarModel[[count]], testAbsEnv, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

				eval <- evalCorrTest(predTestPresFull=predTestPresFull, predNonpresFull=predTestAbsFull, predPres=predTestPresUnivar[[count]], predNonpres=predAbs)

				# consolidate results
				aucAbsUnivar[count] <- eval$eval@auc
				corAbsFullVsUnivar[count] <- eval$cor

				# necessary only if using predictions at background from UNIVARIATE model for CBI
				if (cbi) {
					
					predTestBgUnivar <- predictModel(univarModel[[count]], bgEnvTest, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
					
					# # CBI
					# # using predictions at background sites from FULL model
					# cbiUnivar[count] <- contBoyce(predAtPres=predTestPresUnivar[[count]], predAtBg=predTestBgFull, numClasses=10)
					
					# CBI
					# using predictions at background site from UNIVARIATE model
					cbiUnivar[count] <- contBoyce(predAtPres=predTestPresUnivar[[count]], predAtBg=predTestBgUnivar, numClasses=10)
					cbiEsUnivar[count] <- contBoyce(predAtPres=predTestPresUnivar[[count]], predAtBg=predTestBgUnivar, ecospat=TRUE)
					
				}

			}

			names(aucAbsUnivar) <- names(corAbsFullVsUnivar) <- names(cbiUnivar) <- names(cbiEsUnivar) <- names(landscape)
			
			### evaluations using test PRESENCES and RANDOM BACKGROUND sites
			################################################################

			if (verbose > 1) { say('| pres vs bg', post=0) }
			
			aucBgFull <- rep(NA, jFoldMax) # one value per j-fold
			
			# the next 6 objects pertain to tests using TEST PRESENCES and BACKGROUND SITES
			# they are defined initially as lists, one element per predictor, each of which will eventually have jFoldMax values
			aucBgPerm <- corBgFullVsPerm <- list() 
			for (count in 1:nlayers(landscape)) aucBgPerm[[count]] <- corBgFullVsPerm[[count]] <- rep(NA, jFoldMax)
			
			if (nlayers(landscape) > 2) {
				aucBgReduced <- corBgFullVsReduced <- list()
				for (count in 1:nlayers(landscape)) aucBgReduced[[count]] <- corBgFullVsReduced[[count]] <- rep(NA, jFoldMax)
			}
			
			aucBgUnivar <- corBgFullVsUnivar <- list()
			for (count in 1:nlayers(landscape)) aucBgUnivar[[count]] <- corBgFullVsUnivar[[count]] <- rep(NA, jFoldMax)

			# define objects to store interaction importance... as lists, one element per pair of predictors
			if (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient') {
			
				aucBgPermIA_permBeforeProd <- corBgPermIA_permBeforeProd <- aucBgPermIA_permAfterProd <- corBgPermIA_permAfterProd <- list()
				for (count in 1:(0.5 * nlayers(landscape) * (nlayers(landscape) - 1))) aucBgPermIA_permBeforeProd[[count]] <- corBgPermIA_permBeforeProd[[count]] <- aucBgPermIA_permAfterProd[[count]] <- corBgPermIA_permAfterProd[[count]] <- rep(NA, jFoldMax)
				aucBgPermIA_permBeforeProd <- assignIaNames(aucBgPermIA_permBeforeProd)
				corBgPermIA_permBeforeProd <- assignIaNames(corBgPermIA_permBeforeProd)
				aucBgPermIA_permAfterProd <- assignIaNames(aucBgPermIA_permAfterProd)
				corBgPermIA_permAfterProd <- assignIaNames(corBgPermIA_permAfterProd)
				
			}
			
			### for each j fold
			for (j in 1:jFoldMax) {

				# draw number of test background sites equal to number of test presences
				theseTestBg <- bgEnvTest[sample(rownames(bgEnvTest), numTestPres), ]
			
				### evaluate FULL model using presences and background sites
				predTestBgFull <- predictModel(fullModel, theseTestBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

				isna <- whichIsNaVec(predTestPresFull, predTestBgFull)
				if (length(isna) > 0) {
					p <- predTestPresFull[-isna]
					a <- predTestBgFull[-isna]
				} else {
					p <- predTestPresFull
					a <- predTestBgFull
				}
				aucBgFull[j] <- evaluateMod(p=as.vector(p), a=as.vector(a), tr=seq(0, 1, by=0.01))@auc
				
				### SINGLE-VARIABLE IMPORTANCE of full model with PERMUTED values vs presences and background sites
				for (count in 1:nlayers(landscape)) {

					# !!! NOTE !!! *not* iterating permutations on the presumption that iterating across j folds is iterative enough!
					testPresAndBg <- rbind(testPresEnv, theseTestBg)
					testPresAndBg[ , count] <- sample(testPresAndBg[ , count], 2 * numTestPres)
					
					testPresEnvPerm <- testPresAndBg[1:nrow(testPresEnvPerm), ]
					testBgEnvPerm <- testPresAndBg[(nrow(testPresEnvPerm) + 1):nrow(testPresAndBg), ]
			
					predPres <- predictModel(fullModel, testPresEnvPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
					predBg <- predictModel(fullModel, testBgEnvPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
					
					eval <- evalCorrTest(predTestPresFull=predTestPresFull, predNonpresFull=predTestBgFull, predPres=predPres, predNonpres=predBg)
					
					# consolidate results
					aucBgPerm[[count]][j] <- eval$eval@auc
					corBgFullVsPerm[[count]][j] <- eval$cor

				}

				### INTERACTION IMPORTANCE full model with PERMUTED values vs presences and background sites
				if (interaction & (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient')) {

					# permuting just once because assuming j folds are more independent
					iaTestResults <- testIa(
						model=fullModel,
						landscape=landscape,
						predTestPresFull=predTestPresFull,
						predTestContrastFull=predTestBgFull,
						testPresEnv=testPresEnv,
						testContrastEnv=theseTestBg,
						allBgEnv=NULL,
						maxPermIter=1
					)

					# adding one value to each list element
					for (i in seq_along(iaTestResults$aucPermBeforeProd)) {
					
						aucBgPermIA_permBeforeProd[[i]][j] <- iaTestResults$aucPermBeforeProd[[i]]
						corBgPermIA_permBeforeProd[[i]][j] <- iaTestResults$corPermBeforeProd[[i]]
							
						aucBgPermIA_permAfterProd[[i]][j] <- iaTestResults$aucPermAfterProd[[i]]
						corBgPermIA_permAfterProd[[i]][j] <- iaTestResults$corPermAfterProd[[i]]
						
					}
					
				} # interaction importance
				
				### REDUCED vs presences and background sites
				if (nlayers(landscape) > 2) {
					
					for (count in 1:nlayers(landscape)) {
				
						predBg <- predictModel(reducedModel[[count]], theseTestBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

						eval <- evalCorrTest(predTestPresFull=predTestPresFull, predNonpresFull=predTestBgFull, predPres=predTestPresReduced[[count]], predNonpres=predBg)
						
						# consolidate results
						aucBgReduced[[count]][j] <- eval$eval@auc
						corBgFullVsReduced[[count]][j] <- eval$cor
							
					}
					
				} # if doing reduced model
				

				### UNIVARIATE vs presences and background sites
				for (count in 1:nlayers(landscape)) {

					predBg <- predictModel(univarModel[[count]], theseTestBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
						
					eval <- evalCorrTest(predTestPresFull=predTestPresFull, predNonpresFull=predTestBgFull, predPres=predTestPresUnivar[[count]], predNonpres=predBg)
					
					# consolidate results
					aucBgUnivar[[count]][j] <- eval$eval@auc
					corBgFullVsUnivar[[count]][j] <- eval$cor
						
				}

			} # next j-fold

			
			# calculate means across j-folds and curate variables
			aucBgFull <- mean(aucBgFull)
			
			aucBgPerm <- sapply(aucBgPerm, mean)
			corBgFullVsPerm <- sapply(corBgFullVsPerm, mean)
			names(aucBgPerm) <- names(corBgFullVsPerm) <- names(landscape)

			if (nlayers(landscape) > 2) {
			
				aucBgReduced <- sapply(aucBgReduced, mean)
				corBgFullVsReduced <- sapply(corBgFullVsReduced, mean)
				names(aucBgReduced) <- names(corBgFullVsReduced) <- names(landscape)

			}
			
			aucBgUnivar <- sapply(aucBgUnivar, mean)
			corBgFullVsUnivar <- sapply(corBgFullVsUnivar, mean)
			names(aucBgUnivar) <- names(corBgFullVsUnivar) <- names(landscape)

			# consolidate interaction importance
			if (interaction & (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient')) {
			
				aucBgPermIA_permBeforeProd <- sapply(aucBgPermIA_permBeforeProd, mean, na.rm=T)
				corBgPermIA_permBeforeProd <- sapply(corBgPermIA_permBeforeProd, mean, na.rm=T)
					
				aucBgPermIA_permAfterProd <- sapply(aucBgPermIA_permAfterProd, mean, na.rm=T)
				corBgPermIA_permAfterProd <- sapply(corBgPermIA_permAfterProd, mean, na.rm=T)

				# assign variables' names to list elements
				aucBgPermIA_permBeforeProd <- assignIaNames(x=aucBgPermIA_permBeforeProd)
				corBgPermIA_permBeforeProd <- assignIaNames(x=corBgPermIA_permBeforeProd)
				aucBgPermIA_permAfterProd <- assignIaNames(x=aucBgPermIA_permAfterProd)
				corBgPermIA_permAfterProd <- assignIaNames(x=corBgPermIA_permAfterProd)

			}
			
			### evaluations using test STRATIFIED BACKGROUND sites
			######################################################
			if (verbose > 1) { say('| stratified bg', post=0) }

			# the next 6 objects pertain to tests using STRATIFIED BACKGROUND sites
			# they are defined initially as lists, one element per predictor, each of which will eventually have jFoldMax values
			corBgFullVsPermStrat <- list() 
			for (count in 1:nlayers(landscape)) corBgFullVsPermStrat[[count]] <- rep(NA, jFoldMax)
			
			if (nlayers(landscape) > 2) {
				corBgFullVsReducedStrat <- list()
				for (count in 1:nlayers(landscape)) corBgFullVsReducedStrat[[count]] <- rep(NA, jFoldMax)
			}
			
			corBgFullVsUnivarStrat <- list()
			for (count in 1:nlayers(landscape)) corBgFullVsUnivarStrat[[count]] <- rep(NA, jFoldMax)

			# lists to store interaction importance
			if (interaction & (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient')) {

				corBgPermStratIA_permBeforeProd <- corBgPermStratIA_permAfterProd <- list()
				for (i in 1:(0.5 * nlayers(landscape) * (nlayers(landscape) - 1))) corBgPermStratIA_permBeforeProd[[i]] <- corBgPermStratIA_permAfterProd[[i]] <- rep(NA, jFoldMax)
				corBgPermStratIA_permBeforeProd <- assignIaNames(corBgPermStratIA_permBeforeProd)
				corBgPermStratIA_permAfterProd <- assignIaNames(corBgPermStratIA_permAfterProd)
				
			}

			# make prediction with full model then classify into bins
			fullModelRast <- if (thisAlgorithm=='maxent') {
				predict(fullModel, landscape, na.rm=T)
			} else if (thisAlgorithm=='maxnet') {
				raster::predict(landscape, fullModel, fun=maxnet:::predict.maxnet, na.rm=T, type='cloglog')
			} else {
				raster::predict(landscape, fullModel, fun=predictModel, response=TRUE, type='response', b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, n.trees=model$gbm.call$best.trees, na.rm=TRUE)
			}

			# integerSpecies <- round(stretch(fullModelRast, 0, 1) * 10 + 0.5 - .Machine$double.eps) # controlling for distrib of Pr(occ)
			integerSpecies <- round(fullModelRast * 10 + 0.5 - .Machine$double.eps) # NOT controlling for distribution of Pr(occ)

			### for each j fold
			for (j in 1:jFoldMax) {

				# generate stratified sample across species suitability
				theseTestBg <- sampleStratified(integerSpecies, numTestPres, sp=TRUE)
				theseTestBg <- as.data.frame(extract(landscape, theseTestBg))

				### evaluate FULL model using stratified background sites
				predTestBgFull <- predictModel(fullModel, theseTestBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)

				### full model with PERMUTED values vs STRATIFIED BACKGROUND sites
				for (count in 1:nlayers(landscape)) {

					# !!! NOTE !!! *not* iterating permutations on the presumption that 100 j folds is iterative enough!
					testBgEnvPerm <- theseTestBg
					testBgEnvPerm[ , count] <- sample(testBgEnvPerm[ , count], nrow(testBgEnvPerm))
			
					predBgPerm <- predictModel(fullModel, testBgEnvPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
					
					# calculate cor and remember
					corBgFullVsPermStrat[[count]][j] <- corrTest(x=predTestBgFull, y=predBgPerm)$cor

				}
				
				### INTERACTION IMPORTANCE full model with PERMUTED values vs STRATIFIED background sites
				if (interaction & (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient')) {

					# permuting just once because assuming j folds are more independent
					iaTestResults <- testIa(
						model=fullModel,
						landscape=landscape,
						predTestPresFull=NULL,
						predTestContrastFull=predTestBgFull,
						testPresEnv=NULL,
						testContrastEnv=theseTestBg,
						allBgEnv=NULL,
						maxPermIter=1
					)
					
					# adding one value to each list element
					for (i in seq_along(iaTestResults$corPermBeforeProd)) {
					
						corBgPermStratIA_permBeforeProd[[i]][j] <- iaTestResults$corPermBeforeProd[[i]]
						corBgPermStratIA_permAfterProd[[i]][j] <- iaTestResults$corPermAfterProd[[i]]
						
					}

				} # interaction importance
				
				### REDUCED vs STRATIFIED BACKGROUND sites
				if (nlayers(landscape) > 2) {
					
					for (count in 1:nlayers(landscape)) {

						predBg <- predictModel(reducedModel[[count]], theseTestBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2)
							
						predBg <- cullToShortest(predBg, predTestBgFull)
						predTestBgFullCull <- cullToShortest(predTestBgFull, predBg)
						
						# consolidate results
						corBgFullVsReducedStrat[[count]][j] <- corrTest(x=predTestBgFullCull, y=predBg)$cor
							
					}

					
				} # if doing reduced model
				
				### UNIVARIATE vs STRATIFIED BACKGROUND sites
				for (count in 1:nlayers(landscape)) {

					predBg <- predictModel(univarModel[[count]], theseTestBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

					predBg <- cullToShortest(predBg, predTestBgFull)
					predTestBgFullCull <- cullToShortest(predTestBgFull, predBg)

					# add small amount to avert dividing by 0 if var=0
					if (var(predBg, na.rm=T)==0) predBg <- ifelse(runif(length(predBg)) > 0.5, 1, -1) * 2 * rnorm(length(predBg), 0, .Machine$double.eps)
					
					# consolidate results
					corBgFullVsUnivarStrat[[count]][j] <- corrTest(x=predTestBgFullCull, y=predBg)$cor
						
				}

			} # next j-fold

			
			# calculate means across j-folds and curate variables
			corBgFullVsPermStrat <- sapply(corBgFullVsPermStrat, mean, na.rm=T)
			names(corBgFullVsPermStrat) <- names(landscape)

			if (nlayers(landscape) > 2) {
			
				corBgFullVsReducedStrat <- sapply(corBgFullVsReducedStrat, mean, na.rm=T)
				names(corBgFullVsReducedStrat) <- names(landscape)

			}
			
			corBgFullVsUnivarStrat <- sapply(corBgFullVsUnivarStrat, mean, na.rm=T)
			names(corBgFullVsUnivarStrat) <- names(landscape)

			# interaction importance
			if (interaction & (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient')) {
			
				corBgPermStratIA_permBeforeProd <- sapply(corBgPermStratIA_permBeforeProd, mean, na.rm=T)
				corBgPermStratIA_permAfterProd <- sapply(corBgPermStratIA_permAfterProd, mean, na.rm=T)

				corBgPermStratIA_permBeforeProd <- assignIaNames(corBgPermStratIA_permBeforeProd)
				corBgPermStratIA_permAfterProd <- assignIaNames(corBgPermStratIA_permAfterProd)
				
			}
			
			### REMEMBER results
			####################
			
			if (verbose > 1) { say('| remember') }
			
			# metadata
			thisEvalFrame <- data.frame(
				algorithm=thisAlgorithm,
				iter=iter,
				jFoldMax=jFoldMax,
				maxPermIter=maxPermIter,
				numTrainPres=numTrainPres,
				numTestPres=numTestPres,
				numBg=numBg,
				cbi=cbi,
				interaction=interaction,
				prevalence=cellStats(species, 'mean'),
				widthLandscape=nrow(landscape)
			)

			# remember response parameers
			if (attr(response, 'equationType')=='logistic' | attr(response, 'equationType')=='logisticShift') {

				subFrame <- data.frame(
					b0=b0,
					b1=b1,
					b2=b2,
					b11=b11,
					b12=b12
				)
			
			} else if (attr(response, 'equationType')=='gaussian') {
			
				subFrame <- data.frame(
					mu1=mu1,
					mu2=mu2,
					sigma1=sigma1,
					sigma2=sigma2
				)
			
			}

			thisEvalFrame <- cbind(thisEvalFrame, subFrame)

			# landscape extent (approximate for random layers)
			for (count in 1:nlayers(landscape)) {
			
				subFrame <- data.frame(
					varMin=cellStats(landscape[[count]], 'min'),
					varMax=cellStats(landscape[[count]], 'max')
				)
				
				names(subFrame) <- c(paste0('minVal', names(landscape)[[count]]), paste0('maxVal', names(landscape)[[count]]))
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
			}

			# performance of full model
			thisEvalFrame <- cbind(
				thisEvalFrame,
				data.frame(
					aucAbsFullModel=evalAbsFull@auc,
					aucBgFullModel=aucBgFull
				)
			)

			if (cbi) {
				thisEvalFrame <- cbind(
					thisEvalFrame,
					data.frame(
						cbiFull=cbiFull,
						cbiEsFull=cbiFull
					)
				)
			}
			
			### remember PERMUTE results
			############################

			# AUC
			for (count in 1:nlayers(landscape)) {
			
				subFrame <- data.frame(DUMMY=aucAbsPerm[count])
				names(subFrame) <- paste0('aucAbsPerm_perm', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				subFrame <- data.frame(DUMMY=aucBgPerm[count])
				names(subFrame) <- paste0('aucBgPerm_perm', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
			}

			# CBI
			if (cbi) {
				
				for (count in 1:nlayers(landscape)) {
				
					subFrame <- data.frame(DUMMY1=cbiPerm[count], DUMMY2=cbiPermEs[count])
					names(subFrame) <- c(paste0('cbiPerm_perm', names(landscape)[count]), paste0('cbiEsPerm_perm', names(landscape)[count]))
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
				}
				
			}

			# COR
			for (count in 1:nlayers(landscape)) {
			
				subFrame <- data.frame(DUMMY=corAbsFullVsPerm[count])
				names(subFrame) <- paste0('corAbsFullVsPerm_perm', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				subFrame <- data.frame(DUMMY=corBgFullVsPerm[count])
				names(subFrame) <- paste0('corBgFullVsPerm_perm', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				subFrame <- data.frame(DUMMY=corBgFullVsPermStrat[count])
				names(subFrame) <- paste0('corBgFullVsPermStrat_perm', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
			}
			
			# interaction importance
			if (interaction & (thisAlgorithm=='maxent' | thisAlgorithm=='omniscient')) {

				# tests vs ABSENCES
				containers <- c('aucAbsPermIA_permBeforeProd', 'aucAbsPermIA_permAfterProd', 'corAbsPermIA_permBeforeProd', 'corAbsPermIA_permAfterProd')
				if (cbi) containers <- c(containers, 'cbiPermIA_permBeforeProd', 'cbiPermIA_permAfterProd', 'cbiEsPermIA_permBeforeProd', 'cbiEsPermIA_permAfterProd')
				
				for (box in containers) {
				
					x <- get(box)

					for (i in seq_along(x)) {
						subFrame <- data.frame(DUMMY=x[i])
						names(subFrame) <- paste0(box, '_', names(x)[i])
						thisEvalFrame <- cbind(thisEvalFrame, subFrame)
					}
				
				}

				# tests vs RANDOM background sites
				containers <- c('aucBgPermIA_permBeforeProd', 'aucBgPermIA_permAfterProd', 'corBgPermIA_permBeforeProd', 'corBgPermIA_permAfterProd')

				for (box in containers) {
				
					x <- get(box)
				
					for (i in seq_along(x)) {
						subFrame <- data.frame(DUMMY=x[i])
						names(subFrame) <- paste0(box, '_', names(x)[i])
						thisEvalFrame <- cbind(thisEvalFrame, subFrame)
					}
				
				}

				# tests vs STRATIFIED background sites
				containers <- c('corBgPermStratIA_permBeforeProd', 'corBgPermStratIA_permAfterProd')

				for (box in containers) {
				
					x <- get(box)
				
					for (i in seq_along(x)) {
						subFrame <- data.frame(DUMMY=x[[i]])
						names(subFrame) <- paste0(box, '_', names(x))
						thisEvalFrame <- cbind(thisEvalFrame, subFrame)
					}
				
				}
				
			}
			
			### remember REDUCED results
			############################
			
			if (nlayers(landscape) > 2) {

				# AUC
				for (count in 1:nlayers(landscape)) {
				
					subFrame <- data.frame(DUMMY=aucAbsReduced[count])
					names(subFrame) <- paste0('aucAbsReduced_sans', names(landscape)[count])
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
					subFrame <- data.frame(DUMMY=aucBgReduced[count])
					names(subFrame) <- paste0('aucBgReduced_sans', names(landscape)[count])
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
				}

				# CBI
				if (cbi) {
				
					for (count in 1:nlayers(landscape)) {
					
						subFrame <- data.frame(DUMMY1=cbiReduced[count], DUMMY2=cbiEsReduced[count])
						names(subFrame) <- c(paste0('cbiReduced_sans', names(landscape)[count]), paste0('cbiEsReduced_sans', names(landscape)[count]))
						thisEvalFrame <- cbind(thisEvalFrame, subFrame)
					
					}
					
				}
				
				# COR
				for (count in 1:nlayers(landscape)) {
				
					subFrame <- data.frame(DUMMY=corAbsFullVsReduced[count])
					names(subFrame) <- paste0('corAbsFullVsReduced_sans', names(landscape)[count])
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)

					subFrame <- data.frame(DUMMY=corBgFullVsReduced[count])
					names(subFrame) <- paste0('corBgFullVsReduced_sans', names(landscape)[count])
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
					subFrame <- data.frame(DUMMY=corBgFullVsReducedStrat[count])
					names(subFrame) <- paste0('corBgFullVsReducedStrat_sans', names(landscape)[count])
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
				}
				
			}
			
			### remember UNIVARIATE results
			###############################

			# AUC
			for (count in 1:nlayers(landscape)) {
			
				subFrame <- data.frame(DUMMY=aucAbsUnivar[count])
				names(subFrame) <- paste0('aucAbsUnivar_just', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				subFrame <- data.frame(DUMMY=aucBgUnivar[count])
				names(subFrame) <- paste0('aucBgUnivar_just', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
			}
			
			# CBI
			if (cbi) {
			
				for (count in 1:nlayers(landscape)) {
				
					subFrame <- data.frame(DUMMY1=cbiUnivar[count], DUMMY2=cbiEsUnivar[count])
					names(subFrame) <- c(paste0('cbiUnivar_just', names(landscape)[count]), paste0('cbiEsUnivar_just', names(landscape)[count]))
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
				}
				
			}

			# COR
			for (count in 1:nlayers(landscape)) {
			
				subFrame <- data.frame(DUMMY=corAbsFullVsUnivar[count])
				names(subFrame) <- paste0('corAbsFullVsUnivar_just', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				subFrame <- data.frame(DUMMY=corBgFullVsUnivar[count])
				names(subFrame) <- paste0('corBgFullVsUnivar_just', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				subFrame <- data.frame(DUMMY=corBgFullVsUnivarStrat[count])
				names(subFrame) <- paste0('corBgFullVsUnivarStrat_just', names(landscape)[count])
				thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
			}

			### remember native variable importance
			#######################################
			
			# importance in Maxent
			if (thisAlgorithm=='maxent') {
				
				for (count in 1:nlayers(landscape)) {
			
					subFrame <- data.frame(
						contribution=fullModel@results[paste0(names(landscape)[count], '.contribution'), ] / 100,
						permImport=fullModel@results[paste0(names(landscape)[count], '.permutation.importance'), ] / 100,
						trainGainWithout=fullModel@results[paste0('Training.gain.without.', names(landscape)[count]), ] / 100,
						trainGainWithOnly=fullModel@results[paste0('Training.gain.with.only.', names(landscape)[count]), ] / 100
					)
					
					names(subFrame) <- c(
						paste0('maxentContrib', names(landscape)[count]),
						paste0('maxentPermImport', names(landscape)[count]),
						paste0('maxentTrainGainWithout', names(landscape)[count]),
						paste0('maxentTrainGainWithOnly', names(landscape)[count])
					)
			
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				} # next layer
			
			# importance in GLM
			} else if (thisAlgorithm=='glm') {
				
				hierPart <- hier.part(trainData$presBg, trainData[ , 2:ncol(trainData)], family='binomial')
				
				for (count in 1:nlayers(landscape)) {
				
					subFrame <- data.frame(DUMMY=hierPart$I.perc[names(landscape)[count], ] / 100)
					names(subFrame) <- paste0('glmHierPart_', names(landscape)[count])
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
				}
				
			# importance in BRT
			} else if (thisAlgorithm=='brt') {
				
				for (count in 1:nlayers(landscape)) {
				
					subFrame <- data.frame(
						import=fullModel$contributions$rel.inf[which(fullModel$contributions$var==names(landscape)[count])] / 100
					)
					
					names(subFrame) <- paste0('brtImport', names(landscape)[count])

					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
				
				}
				
				# GBM interaction importance
				brtIa <- gbm.interactions(fullModel)$interactions

				for (countONE in 1:(nlayers(landscape) - 1)) {
				
					for (countTWO in (countONE + 1):nlayers(landscape)) {
				
						if (any(rownames(brtIa) %in% names(landscape)[countONE]) & any(colnames(brtIa) %in% names(landscape)[countONE])) {
					
							subFrame <- data.frame(DUMMY=brtIa[rownames(brtIa)==names(landscape)[countONE], colnames(brtIa)==names(landscape)[countTWO]])
							names(subFrame) <- paste0('brtNativeIa_', names(landscape)[countONE], 'x', names(landscape)[countTWO])
							thisEvalFrame <- cbind(thisEvalFrame, subFrame)
							
						}
				
					}
				
				}
				
			# importance in GAM
			} else if (thisAlgorithm=='gam') {

				imp <- importance(gamMultiModel)

				# importance of each single variable
				for (var1 in 1:nlayers(landscape)) {

					var2 <- (1:nlayers(landscape))[-which(names(landscape)==names(landscape)[var1])] # index of var's != this var
					
					subFrame <- data.frame(
						import=imp[which(grepl(x=names(imp), pattern=names(landscape)[var1]) & !grepl(x=names(imp), pattern=names(landscape)[var2]))]
					)
					
					names(subFrame) <- paste0('gamAiccWeight', names(landscape)[var1])
			
					thisEvalFrame <- cbind(thisEvalFrame, subFrame)
			
				}

				# importance of interaction terms
				for (var1 in 1:(nlayers(landscape) - 1)) {
					for (var2 in 2:nlayers(landscape)) {
					
						subFrame <- data.frame(
							import=imp[which(grepl(x=names(imp), pattern=names(landscape)[var1]) & grepl(x=names(imp), pattern=names(landscape)[var2]))]
						)
						
						names(subFrame) <- paste0('gamAiccWeight', names(landscape)[var1], 'x', names(landscape)[var2])
				
						thisEvalFrame <- cbind(thisEvalFrame, subFrame)
					
					}
				}	
			
			} # native importance of GAM
				
			evalFrame <- if (exists('evalFrame', inherits=F)) { rbind(evalFrame, thisEvalFrame) } else { thisEvalFrame }
		
			if (verbose > 2) {
				say('')
				print(summary(evalFrame))
				say('')
			}

		} # next iteration

		rownames(evalFrame) <- 1:nrow(evalFrame)

		saveRDS(evalFrame, paste0(directory, '/Results - ', toupper(thisAlgorithm), ifelse(is.null(suffix), '', paste0(' - ', suffix)), ' - Set ', prefix(min(iterToDo), 2), ' to ', prefix(max(iterToDo), 2), '.rds'))

		rm(evalFrame, fullModel, univarModel); if (exists('reducedModel', inherits=F)) { rm(reducedModel) }; gc()
	
		say('')
		
	} # next algorithm

	say('')
	
}



# say('################################################')
# say('### pre-generate non-random landscape layers ###')
# say('################################################')

# dirCreate(paste0(workDir, '/Simulated Landscape Rasters'))

# geography <- list(
	# list(type='linear', min=min, max=max, rot=numeric())
# )

# rot <- c(0, 15, 22.5, 30, 45, 60, 67.5, 75, 90, 105, 112.5, 120, 135, 150, 157.5, 165)
# for (i in seq_along(rot)) {
	# cat('raster', i, 'rotation', rot[i], '\n'); flush.console()
	# thisGeography <- geography
	# thisGeography[[1]]$rot <- rot[i]
	# landscape <- genesis(thisGeography, circle=TRUE, verbose=FALSE)
	# landscape <- raster(landscape)
	# name <- paste0('linearRotation', gsub(x=as.character(rot[i]), pattern='[.]', replacement='pt'))
	# name <- gsub(pattern='-', x=name, replacement='Neg')
	# names(landscape) <- name
	# writeRaster(landscape[[1]], paste0(workDir, '/Simulated Landscape Rasters/linearFrom', sub(x=as.character(min), pattern='-', replacement='Neg'), 'To', sub(x=as.character(max), pattern='-', replacement='Neg'), 'Rotation', sub(x=sub(as.character(rot[i]), pattern='[.]', replacement='pt'), pattern='-', replacement='Neg')), format='GTiff', overwrite=T)
# }

# # # generate series of random rasters
# # for (rand in 1:1) {

	# # say(rand)
	# # geog <- list(list(type='random', min=min, max=max))
	# # rast <- genesis(geog, circle=TRUE)
	# # rast <- subset(rast, 1)
	# # names(rast) <- paste0('randomFrom', sub(x=as.character(min), pattern='-', replacement='Neg'), 'To', sub(x=as.character(min), pattern='-', replacement='Neg'), 'Rotation135')
	# # writeRaster(rast, paste0(workDir, '/Simulated Landscape Rasters/randomFrom', sub(x=as.character(min), pattern='-', replacement='Neg'), 'To', sub(x=as.character(max), pattern='-', replacement='Neg'), '_', rand), format='GTiff', overwrite=T)
	
# # }




# say('######################################################################################')
# say('### calculate correlation between rotated landscape variables and make nice figure ###')
# say('######################################################################################')

# landscape <- stack(
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation0.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation15.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation22pt5.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation30.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation45.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation60.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation67pt5.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation75.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation90.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation105.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation112pt5.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation120.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation135.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation150.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation157pt5.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation165.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation0_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation15_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation22pt5_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation30_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation45_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation60_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation67pt5_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation75_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation90_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation105_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation112pt5_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation120_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation135_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation150_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation157pt5_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/linearFromNeg1To1Rotation165_split.tif')),
	# raster(paste0(workDir, '/Simulated Landscape Rasters/randomFromNeg1To1_1.tif'))
# )
	
# rand <- randomPointsRobust(landscape, 10000, prob=FALSE)
# env <- as.data.frame(extract(landscape, rand))

# print(cor(env))

# write.csv(cor(env), paste0()orkDir, '/Correlations between Standard Environmental Variables.csv'))

# rot <- c(0, 45, 90, 135)

# cex.main <- 2.4
# col.main <- fg
# # landcol <- c('white', 'dodgerblue4')
# # landcol <- c('white', 'royalblue4')
# landcol <- c('white', 'burlywood4')

# col <- colorRampPalette(landcol)
# col <- col(100)
	
# png(paste0(workDir, '/Simulated Landscape Rasters/landscape layers.png'), width=3000, height=2000, res=300)

	# par(mfrow=c(2, 3), mai=c(1, 1, 1, 1) * 0.2, oma=c(1, 1, 1, 1), bty='n', bg=bg, fg=fg)

	# r0 <- raster(paste0(workDir, 'Simulated Landscape Rasters/linearFromNeg1To1Rotation0.tif'))
	# r45 <- raster(paste0(workDir, 'Simulated Landscape Rasters/linearFromNeg1To1Rotation45.tif'))
	# r90 <- raster(paste0(workDir, 'Simulated Landscape Rasters/linearFromNeg1To1Rotation90.tif'))
	# r135 <- raster(paste0(workDir, 'Simulated Landscape Rasters/linearFromNeg1To1Rotation135.tif'))
	# rRand <- raster(paste0(workDir, 'Simulated Landscape Rasters/randomFromNeg1To1_1.tif'))
	
	# landscape <- stack(r0, r45, r90, r135)
	
	# # linear rasters
	# for (i in 1:4) {
		# r <- subset(landscape, i)
		# plot(r, col=col, breaks=seq(minValue(r), maxValue(r), length.out=length(col)), legend=F, mapixels=500000, axes=F)
		# title(main=paste0('rotation ', rot[i], '°'), cex.main=cex.main, col.main=col.main, xpd=NA, line=0.1)
	# }

	# # random raster
	# plot(rRand, col=col, breaks=seq(minValue(rRand), maxValue(rRand), length.out=length(col)), legend=F, mapixels=500000, axes=F)
	# title(main='random', cex.main=cex.main, col.main=col.main, xpd=NA, line=0.1)

	# leg <- matrix(seq(max, min, length.out=100), nrow=100)
	# leg <- rbind(matrix(rep(NA, 5), ncol=1), leg)
	# leg <- rbind(leg, matrix(rep(NA, 5), ncol=1))
	# leg <- cbind(matrix(rep(NA, 2 * nrow(leg)), ncol=2), leg)
	# leg <- cbind(leg, matrix(rep(NA, 2 * nrow(leg)), ncol=2))
	# leg <- raster(leg)

	# # legend for landscape
	# plot(
		# x=leg,
		# legend=FALSE,
		# axes=FALSE,
		# col=col,
		# breaks=seq(minValue(leg), maxValue(leg), length.out=length(col)),
		# maxpixels=500000
	# )

	# text(x=0.5, y=1.05, xpd=NA, labels='1', col=fg, cex=cex.main * 1.4, adj=0.5)
	# text(x=0.5, y=-0.05, xpd=NA, labels='-1', col=fg, cex=cex.main * 1.4, adj=0.5)

# dev.off()


