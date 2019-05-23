### SDM PREDICTOR INFERENCE - SCENARIOS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
###
### The purpose of this script is to assess the best set of "tuning" parameters for training BRTs and RFs in the scenarios. For BRTs, the process uses a grid search across learning rate, tree complexity, number of background sites, and possibly bag fraction (which is not explored here). Obviously this same search could be performed when applying BRTs to the data in the difference scenarios, but this pre-tuning shortens computational time considerably!
###
### For RFs the tuning process simply explores using different numbers of background sites.
###
### Note that this script is really not intended to be run on a single process all at once. Doing so will require substantial time commitments. Instead, the *data generation and modeling* steps can be split across multiple processes on the same computer by using different sets of iterations (variable "iters" below) for different processes on the same or different computers. The *evaluation* step can also be split across multiple processes so long as each process has access to the full set of simulated data and models for a given combination of learning rate, tree complexity, and number of background points. Summarizing results requires access to the full set of evaluations.

### CONTENTS ###
### initiate ###
### master settings ###

### [tune brt for logistic responses] modeling ###
### [tune brt for logistic responses] selecting optimal number of background sites ###
### [tune rf for logistic responses] selecting optimal number of background sites ###

### [tune brt for bivariate responses] modeling ###
### [tune brt for bivariate responses] selecting optimal number of background sites ###

### [tune rfs for logistic responses] modeling ###

################
### initiate ###
################

	memory.limit(memory.limit() * 2^30)
	rm(list=ls())
	options(keep.source=FALSE) # manage memory
	gc()
	print('')
	print(date())
	
#######################
### master settings ###
#######################

	# directory for modeling
	# source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/00 Tune BRTs & RFs.r')
	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')
	tempDrive <- 'C:'

	# # directory for modeling
	# # source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts/00 Tune BRTs & RFs.r')
	# setwd('H:/Global Change Program/Research/ENMs - Predictor Inference')
	# tempDrive <- 'C:'
	# # tempDrive <- 'D:'
	# # tempDrive <- 'E:'

	# directory names for simulated data, model, and model evaluations
	simDir <- '!scenario data' # subdirectory of each scenario's directory in which to store simulation data
	modelDir <- '' # subdirectory of each scenario's directory in which to store models
	evalDir <- 'evaluations' # subdirectory of each scenario's directory in which to store model evaluations
	
	# verbose <- 0 # minimal display
	verbose <- 1 # some display -- best for most scenarios
	# verbose <- 2 # much display
	# verbose <- Inf # all display
	debug <- FALSE; modelType <- 'does not matter' # for running code
	# debug <- TRUE; modelType <- 'logistic' # for debugging using logistic response
	# debug <- TRUE; modelType <- 'gaussian' # for debugging using Gaussian response

	# iterations
	iters <- 1:100 # iterations to do -- want 100 total
	# iters <- rev(1:100) # iterations to do -- want 100 total

	# algorithms
	algos <- c('brt', 'rf')
	# algos <- c('brt')
	# algos <- c('rf')
	
#########################
### tuning parameters ###
#########################	

	# number of training background sites to test for BRTs in "tuning" exercises
	numBgToTest <- c(10000, 1000, 200)
	# numBgToTest <- rev(c(10000, 1000, 200))
	# numBgToTest <- c(10000)
	# numBgToTest <- c(1000)
	# numBgToTest <- c(200)
	
	# BRT tuning parameters
	lr <- c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005) # learning rate
	tc <- c(1, 2, 3, 6) # tree complexity
	bf <- 0.6 # bag fraction
	maxTrees <- 8000 # maximum number of trees
	
#################
### functions ###
#################

	library(sp)
	library(rgdal)
	library(raster)
	library(rJava)
	options(java.parameters='-Xmx1g' )
	library(gbm)
	# library(party)
	library(dismo)
	library(omnibus)
	library(enmSdm)
	library(enmSdmPredImport)
	library(legendary)

say('#########################################################')
say('### [tune brt and rf for logistic responses] modeling ###')
say('#########################################################')

	say('This experiment tunes the settings to be used for BRTs and RFs using the simplest landscape.')

	thisOutDir <- 'tune brt & rf for logistic responses'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)
	simDir <- '/!scenario data - 10000 bg'
	scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	# define species
	b0 <- 0 # intercept
	b1 <- 2 # slope of P1
	b2 <- 1 # slope of P2
	b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	b12 <- 0 # slope of T1 * T2
	mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	response <- logistic
	
	# define landscape: one linear TRUE variable and one random FALSE variable
	geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# create data
	predImportMakeData(
		response=response,
		geography=geography,
		simDir=paste0(scenarioDir, '/!scenario data'),
		numTrainPres=200,
		numTestPres=200,
		numBg=10000,
		iters=iters,
		sizeNative=1024,
		overwrite=FALSE,
		fileFlag=NULL,
		b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		verbose=verbose,
		circle=FALSE
	)

	# train full models
	for (numBg in numBgToTest) {
	
		say('USING ', numBg, ' BACKGROUND SITES', level=2)

		predImportTrainModels(
			simDir=paste0(scenarioDir, '/!scenario data'),
			modelDir=paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg'),
			vars=c('T1', 'F1'),
			algos=algos,
			type=c('multivariate'),
			iters=iters,
			numBg=numBg,
			fileFlag=NULL,
			overwrite=FALSE,
			verbose=verbose,
			maxTrees=maxTrees,
			learningRate=lr, treeComplexity=tc, bagFraction=bf
		)
		
	}

	# evaluate
	for (numBg in numBgToTest) {
	
		say('USING ', numBg, ' BACKGROUND SITES', level=2)

		predImportEval(
			simDir=paste0(scenarioDir, '/!scenario data'),
			modelDir=paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg'),
			evalDir=paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg'),
			algos=algos,
			type=c('multivariate'),
			iters=iters,
			perms=30,
			ia=FALSE,
			overwrite=FALSE,
			fileFlag=NULL,
			verbose=verbose
		)
	
	}
	
say('######################################################################################')
say('### [tune brt for logistic responses] selecting optimal number of background sites ###')
say('######################################################################################')

	thisOutDir <- 'tune brt & rf for logistic responses'
	scenarioDir <- paste0('./Results/', thisOutDir)

	### load evaluations
	evals <- data.frame()
	for (numBg in numBgToTest) {
	
		load(paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg/Evaluations for multivariate BRT.RData'))
		evals <- rbind(evals, perform)
		
	}

	### plot
	png(paste0(scenarioDir, '/BRT - CBI ~ Number of Training Background Sites.png'))
		boxplot(cbiMulti ~ numTrainBgMulti, data=evals, ylab='CBI (Multivariate)', xlab='Number of training background sites')
	dev.off()

	### assess by number of BG sites
	best <- data.frame()
	for (numBg in numBgToTest) {
		
		x <- evals[evals$numTrainBgMulti == numBg, ]
		avg <- mean(x$cbiMulti, na.rm=TRUE)
		std <- sd(x$cbiMulti, na.rm=TRUE)
		
		say('Multivariate model CBI for ', numBg, ' background sites (mean +- SD): ', sprintf('%.3f', avg), ' +- ', sprintf('%.3f', std))
		
		best <- rbind(
			best,
			data.frame(
				numTrainBg=numBg,
				cbiMean=avg,
				cbiSd=std
			)
		)
		
	}
	
	### get best number of BG sites
	bestNumBg <- best$numTrainBg[which.max(best$cbiMean)]
	
	say('Best number of BG sites for BRT is ', bestNumBg, '. Using mean parameter set from this set.')
	
	### calculate BRT parameters for best set of BRT models
	params <- data.frame()
	
	for (iter in iters) {
	
		load(paste0(scenarioDir, '/models with ', prefix(bestNumBg, 5), ' bg/multivariate brt/brt model ', prefix(iter, 4), '.Rdata'))
	
		lr <- model$gbm.call$learning.rate
		tc <- model$gbm.call$tree.complexity
		bf <- model$gbm.call$bag.fraction
		nTrees <- model$gbm.call$best.trees
	
		params <- rbind(
			params,
			data.frame(
				bestNumBg=bestNumBg,
				learningRate=lr,
				treeComplexity=tc,
				bagFraction=bf,
				nTrees=nTrees
			)
		)
	
	}

	bestParams <- data.frame(
		bestNumBg=bestNumBg,
		learningRate_025quant=quantile(params$learningRate, 0.25),
		learningRate_mean=mean(params$learningRate),
		learningRate_075quant=quantile(params$learningRate, 0.75),
		treeComplexity_025quant=round(quantile(params$treeComplexity, 0.25)),
		treeComplexity_mean=round(mean(params$treeComplexity)),
		treeComplexity_075quant=round(quantile(params$treeComplexity, 0.75)),
		bagFraction=mean(params$bagFraction),
		maxTrees=50 * ceiling(1.1 * max(params$nTrees) / 50)
	)
	
	write.csv(bestParams, paste0(scenarioDir, '/Parameters of Best BRT Models.csv'), row.names=FALSE)

say('#####################################################################################')
say('### [tune rf for logistic responses] selecting optimal number of background sites ###')
say('#####################################################################################')

	thisOutDir <- 'tune brt & rf for logistic responses'
	scenarioDir <- paste0('./Results/', thisOutDir)

	### load evaluations
	evals <- data.frame()
	for (numBg in numBgToTest) {
	
		load(paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg/Evaluations for multivariate RF.RData'))
		evals <- rbind(evals, perform)
		
	}

	### plot
	png(paste0(scenarioDir, '/RF - CBI ~ Number of Training Background Sites.png'))
		boxplot(cbiMulti ~ numTrainBgMulti, data=evals, ylab='CBI (Multivariate)', xlab='Number of training background sites')
	dev.off()

	### assess by number of BG sites
	best <- data.frame()
	for (numBg in numBgToTest) {
		
		x <- evals[evals$numTrainBgMulti == numBg, ]
		avg <- mean(x$cbiMulti, na.rm=TRUE)
		std <- sd(x$cbiMulti, na.rm=TRUE)
		
		say('Multivariate model CBI for ', numBg, ' background sites (mean +- SD): ', sprintf('%.3f', avg), ' +- ', sprintf('%.3f', std))
		
		best <- rbind(
			best,
			data.frame(
				numTrainBg=numBg,
				cbiMean=avg,
				cbiSd=std
			)
		)
		
	}
	
	### get best number of BG sites
	bestNumBg <- best$numTrainBg[which.max(best$cbiMean)]
	
	say('Best number of BG sites for RF is ', bestNumBg, '.')
	
# say('########################################################')
# say('### [tune brt & rf for bivariate responses] modeling ###')
# say('########################################################')

	# say('Varying strength of variable 1 vs 2 on landscape with 2 linear variables')
	# say('Covariates include landscape rotation and rho')

	# ### define species
	# ##################
	
	# b0 <- NA # intercept
	# b1 <- NA # slope of P1
	# b2 <- NA # slope of P2
	# b11 <- NA # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- NA # slope of T1 * T2
	# mu1 <- 0
	# mu2 <- 0
	# sigma1 <- NA
	# sigma2 <- NA
	# rho <- NA

	# response <- gaussian

	# ### create progress frame
	# #########################
	# progress <- data.frame()
	# # rot <- c(22.5, 90, 157.5)
	# rot <- c(45, 90, 135)
	# # rho <- c(-0.75, 0, 0.75)
	# rho <- c(-0.5, 0, 0.5)
	# sigmas <- c(0.1, 0.3, 0.5)

	# progress <- expand.grid(rot=rot, rho=rho, sigma1=sigmas, sigma2=sigmas)
	# progress$string <- NA
	# for (i in 1:nrow(progress)) {
		# progress$string[i] <- paste0(
			# 'rot=', progress$rot[i],
			# ' rho=', progress$rho[i],
			# ' sigma1=', progress$sigma1[i],
			# ' sigma1=', progress$sigma2[i]
		# )
	# }

	# ### for each number of background sites
	# for (algo in algos) {
		
		# for (numBg in numBgToTest) {
		
			# say('NUMBER OF TRAINING BG: ', numBg, level=2)
		
			# thisOutDir <- paste0('tune brt & rf for bivariate responses/', prefix(numBg, 5), ' bg')
			# scenarioDir <- paste0('./Results/', thisOutDir)
			# dirCreate(scenarioDir)
			# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))'
			# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

			# dirCreate(scenarioDir, paste0('/!starts - ', algo))
			# dirCreate(scenarioDir, paste0('/!stops - ', algo))

			# # sets in progress or completed
			# started <- list.files(paste0(scenarioDir, paste0('/!starts - ', algo)))

			# # for each SCENARIO
			# while (length(started) < nrow(progress)) {
			
				# # get index of set needed doing
				# if (length(started)==0) {
					# doing <- 1
				# } else {
					# doing <- progress$string[-match(started, progress$string)][1]
					# doing <- which(progress$string==doing)
				# }
				# write.csv(progress$string[doing], paste0(scenarioDir, paste0('/!starts - ', algo, '/'), progress$string[doing]), row.names=FALSE)

				# rot <- progress$rot[doing]
				# thisRho <- progress$rho[doing]
				# thisSigma1 <- progress$sigma1[doing]
				# thisSigma2 <- progress$sigma2[doing]

				# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
			
				# ### define geography
				# ####################
				# geography <- list(
					# T1=list(type='linear', min=-1, max=1),
					# T2=list(type='linear', min=-1, max=1, rot=rot)
				# )

				# fileFlag <- paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
				
				# # create data
				# predImportMakeData(
					# response=response,
					# geography=geography,
					# simDir=paste0(scenarioDir, '/', simDir),
					# numTrainPres=200,
					# numTestPres=200,
					# numBg=10000,
					# circle=TRUE,
					# sizeNative=1024,
					# iters=iters,
					# overwrite=FALSE,
					# fileFlag=fileFlag,
					# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
					# verbose=verbose
				# )
				
				# # train full models
				# predImportTrainModels(
					# simDir=paste0(scenarioDir, '/', simDir),
					# modelDir=paste0(scenarioDir, '/', modelDir),
					# vars=c('T1', 'T2'),
					# algos=algo,
					# type='multivariate',
					# iters=iters,
					# numBg=numBg,
					# fileFlag=fileFlag,
					# tempDir=tempDir,
					# overwrite=FALSE,
					# verbose=verbose,
					# maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf
				# )

				# # evaluate
				# predImportEval(
					# simDir=paste0(scenarioDir, '/', simDir),
					# modelDir=paste0(scenarioDir, '/', modelDir),
					# evalDir=paste0(scenarioDir, '/', evalDir),
					# algos=algo,
					# type='multivariate',
					# iters=iters,
					# perms=30,
					# ia=FALSE,
					# overwrite=FALSE,
					# fileFlag=fileFlag,
					# verbose=verbose
				# )
					
				# # indicate this set complete and save
				# write.csv(progress$string[doing], paste0(scenarioDir, paste0('/!stops - ', algo, '/'), progress$string[doing]), row.names=FALSE)
				# started <- list.files(paste0(scenarioDir, paste0('/!starts - ', algo)))

			# } # next scenario
			
		# } # next number of BG sites
		
	# } # next algorithm

# say('#######################################################################################')
# say('### [tune brt for bivariate responses] selecting optimal number of background sites ###')
# say('#######################################################################################')
	
	# thisOutDir <- 'tune brt for bivariate responses'
	# scenarioDir <- paste0('./Results/', thisOutDir)

	# ### collate evaluations
	# #######################
	# evals <- data.frame()
	# for (numBg in numBgToTest) {

		# evalDir <- paste0('./Results/tune brt for bivariate responses/', prefix(numBg, 5), ' bg/evaluations')

		# thisEvals <- loadEvals(evalDir, algos='brt', redo=TRUE)
		# evals <- if (nrow(evals) == 0) {
			# thisEvals
		# } else {
			# merge(evals, thisEvals, all=TRUE)
		# }
		
	# }
	
	# ### plot
	# ########
	
	# png(paste0(scenarioDir, '/CBI ~ Number of Training Background Sites.png'))
		# boxplot(cbiMulti ~ numTrainBgMulti, data=evals, ylab='CBI (Multivariate)', xlab='Number of training background sites')
	# dev.off()

	# ### assess by number of BG sites
	# ################################
	
	# best <- data.frame()
	# for (numBg in numBgToTest) {
		
		# x <- evals[evals$numTrainBgMulti == numBg, ]
		# avg <- mean(x$cbiMulti, na.rm=TRUE)
		# std <- sd(x$cbiMulti, na.rm=TRUE)
		
		# say('Multivariate model CBI for ', numBg, ' background sites (mean +- SD): ', sprintf('%.3f', avg), ' +- ', sprintf('%.3f', std))
		
		# best <- rbind(
			# best,
			# data.frame(
				# numTrainBgMulti=numBg,
				# cbiMean=avg,
				# cbiSd=std
			# )
		# )
		
	# }
	
	# ### get best number of BG sites
	# bestNumBg <- best$numTrainBg[which.max(best$cbiMean)]
	
	# say('Best number of BG sites for BRT is ', bestNumBg, '. Using mean parameter set from this set.')
	
	# ### calculate BRT parameters for best set of BRT models
	# #######################################################
	
	# params <- data.frame()
	
	# modelFiles <- listFiles(paste0(scenarioDir, '/', prefix(bestNumBg, 5), ' bg/multivariate brt'), pattern='.RData')
	
	# for (modelFile in modelFiles) {

		# load(modelFile)
	
		# lr <- model$gbm.call$learning.rate
		# tc <- model$gbm.call$tree.complexity
		# bf <- model$gbm.call$bag.fraction
		# nTrees <- model$gbm.call$best.trees
	
		# params <- rbind(
			# params,
			# data.frame(
				# bestNumBg=bestNumBg,
				# learningRate=lr,
				# treeComplexity=tc,
				# bagFraction=bf,
				# nTrees=nTrees
			# )
		# )
	
	# }

	# bestParams <- data.frame(
		# bestNumBg=bestNumBg,
		# learningRate_025quant=quantile(params$learningRate, 0.25),
		# learningRate_mean=mean(params$learningRate),
		# learningRate_075quant=quantile(params$learningRate, 0.75),
		# treeComplexity_025quant=round(quantile(params$treeComplexity, 0.25)),
		# treeComplexity_mean=round(mean(params$treeComplexity)),
		# treeComplexity_075quant=round(quantile(params$treeComplexity, 0.75)),
		# bagFraction=mean(params$bagFraction),
		# maxTrees=50 * ceiling(1.1 * max(params$nTrees) / 50)
	# )
	
	# write.csv(bestParams, paste0(scenarioDir, '/Parameters of Best BRT Models.csv'), row.names=FALSE)

say('###############################################################################################')
say('###############################################################################################')
say('### END END END END END END END END END END END END END END END END END END END END END END ###')
say('###############################################################################################')
say('###############################################################################################')

say(date())


