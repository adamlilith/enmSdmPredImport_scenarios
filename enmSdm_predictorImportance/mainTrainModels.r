#######################################
### MASTER function to train models ###
#######################################

mainTrainModels <- function(
	outDir,
	vars,
	algos=c('omniscient', 'brt', 'gam', 'glm', 'maxent', 'rf'),
	type=c('multivariate', 'reduced', 'univariate'),
	iters=1:100,
	fileAppend=NULL,
	overwrite=FALSE,
	verbose=1,
	...
) {
	# outDir		base directory for this sim
	# vars			names of variables to use in model training
	# algos			name(s) of algorithms ('omniscient', 'brt', 'gam', 'maxent', 'rf')
	# type			type of models to train ('multivariate', 'reduced', 'univariate')
	# iters			integer list of simulation iterations to perform (e.g., 1:100)--each will have a different set of presences and background sites
	# fileAppend	character string to add to file name of saved file... leave as NULL to ignore
	# mu1, mu2, sigma1, sigma2, rho		numeric values used for simulating species
	# overwrite		logical, if TYRUE then overwrite existing model files; if FALSE then skip existing model files
	# verbose		<=0: no display of progress; 1: some progress; 2: detailed progress; >=3: very detailed progress
	# ...			arguments to pass to trainBrt(), trainGam(), trainGam(), and trainMaxEnt()

	# file prefix
	fileAppendStartSpace <- if (!is.null(fileAppend)) { paste0(' ', fileAppend) } else { '' }
	fileAppendEndSpace <- if (!is.null(fileAppend)) { paste0(fileAppend, ' ') } else { '' }

	# for each SDM algorithm
	for (algo in algos) {

		if (verbose >= 0) say('Modeling: ', toupper(algo), ' | ', date(), ' |', post=0)
	
		# for each iteration sample landscape, train model, and evaluate
		for (iter in iters) {

			if (verbose==1) {
				say(iter, post=0)
			} else if (verbose > 1) {
				say(date(), ' | ', toupper(algo), ' | simulation ', iter, post=0, pre=1)
			}

			### load training/test data
			###########################
			
			load(paste0(outDir, '/!scenario data/', fileAppendEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))
			trainData <- sim$trainData[ , c('presBg', vars)]
			
			### train MULTIVARIATE models
			#############################
			if ('multivariate' %in% type) {
				
				if (verbose > 0) { say('| mv', post=0) }
				
				# if overwriting models OK OR model doesn't exist
				if (overwrite | !file.exists(paste0(outDir, '/multivariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData'))) {
					
					if (algo=='omniscient') {
					
						out <- response
						attr(out, 'modelType') <- 'full'
					
					} else if (algo=='glm') {
					
						out <- trainGlm(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							construct=FALSE,
							verbose=verbose > 2,
							...
						)

					} else if (algo=='maxent') {
					
						out <- trainMaxEnt(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							scratchDir=paste0(tempDrive, '/ecology/!Scratch/_TEMP\\'),
							verbose=(verbose > 2),
							...
						)

					} else if (algo=='brt') {

						set.seed(sim$seed)
					
						out <- trainBrt(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							w=TRUE,
							verbose=(verbose > 2),
							...
						)
					
					} else if (algo=='gam') {
			
						out <- trainGam(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							construct=FALSE,
							verbose=(verbose > 2),
							...
						)
						
					} else if (algo=='rf') {
			
						trainData$presBg <- as.factor(trainData$presBg)

						out <- trainRf(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							importance=TRUE,
							verbose=(verbose > 2),
							...
						)
						
					}
					
					model <- if (is.null(out)) {
						FALSE
					} else {
						out
					}
					
					dirCreate(outDir, '/multivariate ', algo)
					save(model, file=paste0(outDir, '/multivariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData'))
					rm(model); gc()
					
				} # if overwriting models OK OR model doesn't exist
				
				if (verbose > 0) say('\U2713', post=0)
			
			} # if wanting multivariate models
			
			### train REDUCED models
			########################
			if ('reduced' %in% type) {
				
				if (length(vars) > 2) {
				
					if (verbose > 0) { say('red', post=0) }
						
					# if overwriting models OK OR model doesn't exist
					if (overwrite | !file.exists(paste0(outDir, '/reduced ', algo, '/', algo, fileAppendStartSpace, ' model ', prefix(iter, 3), '.Rdata'))) {
						
						model <- list()

						# for EACH variable
						for (count in seq_along(vars)) {

							reducedTrainData <- trainData[ , which(!(names(trainData) %in% vars[count]))]
							
							if (algo=='omniscient') {
							
								out <- response
								attr(out, 'modelType') <- 'reduced'
								attr(out, 'reducedSans') <- names(sim$geography)[count]
							
							} else if (algo=='glm') {
							
								out <- trainGlm(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									construct=FALSE,
									verbose=verbose > 2,
									...
								)

							} else if (algo=='maxent') {
			
								out <- trainMaxEnt(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									scratchDir=paste0(tempDrive, '/ecology/!Scratch/_TEMP\\'),
									verbose=(verbose > 2),
									...
								)
								
							} else if (algo=='brt') {
						
								set.seed(sim$seed)
						
								out <- trainBrt(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									verbose=(verbose > 2),
									...
								)
							
							} else if (algo=='gam') {

								out <- trainGam(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									construct=FALSE,
									verbose=(verbose > 2),
									...
								)
							
							} else if (algo=='rf') {

								reducedTrainData$presBg <- as.factor(reducedTrainData$presBg)
							
								out <- trainRf(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									importance=TRUE,
									verbose=(verbose > 2),
									...
								)
							
							}
						
							model[[count]] <- if (!is.null(out)) {
								out
							} else {
								FALSE
							}
							
							names(model)[[count]] <- paste0('sans', vars[count])
							
						} # next reduced model
					
						dirCreate(outDir, '/reduced ', algo)
					
						save(model, file=paste0(outDir, '/reduced ', algo, '/', algo, fileAppendStartSpace, ' model ', prefix(iter, 3), '.Rdata'))
						rm(model); gc()
						
					} # if overwriting models OK OR model doesn't exist

				} # if enough layers to do reduced models
			
				if (verbose > 0) say('\U2713', post=0)
			
			} # if wanting reduced models
			
			### train UNIVARIATE models
			###########################
			if ('univariate' %in% type) {
				
				if (verbose > 0) { say('uni', post=0) }
				
				# if overwriting models OK OR model doesn't exist
				if (overwrite | !file.exists(paste0(outDir, '/univariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData'))) {

					model <- list()
					
					for (count in seq_along(vars)) {
					
						if (verbose > 1) { say(vars[count], post=0) }
					
						univarTrainData <- trainData[ , c('presBg', vars[count])]

						# omniscient model
						if (algo=='omniscient') {
						
							out <- response
							attr(out, 'modelType') <- 'univariate'
							attr(out, 'univarWith') <- names(sim$geography)[count]

						# GLM
						} else if (algo=='glm') {
						
							out <- trainGlm(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								construct=FALSE,
								select=TRUE,
								verbose=verbose > 2,
								...
							)
								
						# maxent
						} else if (algo=='maxent') {

							out <- trainMaxEnt(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								scratchDir=paste0(tempDrive, '/ecology/!Scratch/_TEMP\\'),
								verbose=(verbose > 2),
								...
							)

						### BRTs
						} else if (algo=='brt') {

							set.seed(sim$seed)
						
							out <- trainBrt(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								w=TRUE,
								verbose=(verbose > 2),
								...
							)

						### GAMs
						} else if (algo=='gam') {

							out <- trainGam(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								construct=FALSE,
								select=FALSE,
								verbose=(verbose > 2),
								...
							)

						### RFs
						} else if (algo == 'rf') {

							univarTrainData$presBg <- as.factor(univarTrainData$presBg)
						
							out <- trainRf(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								importance=FALSE,
								verbose=(verbose > 2),
								...
							)

						}
						
						model[[count]] <- if (!is.null(out)) {
							out
						} else {
							FALSE
						}
						
						names(model)[[count]] <- paste0('only', vars[count])

					} # next univariate model

					dirCreate(outDir, '/univariate ', algo)
					save(model, file=paste0(outDir, '/univariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData'))
					rm(model); gc()
					
				} # if overwriting models OK OR model doesn't exist
					
				if (verbose > 0) say('\U2713', post=0)
					
			} # if wanting univariate models
			
		} # next iteration

		say('')
		
	} # next algorithm

}

