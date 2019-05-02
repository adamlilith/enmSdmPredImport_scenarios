### SDM PREDICTOR INFERENCE - SCENARIOS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org

	memory.limit(memory.limit() * 2^30)
	rm(list=ls())
	options(keep.source=FALSE) # manage memory
	gc()
	print('')
	print(date())

### CONTENTS ###
### master settings ###
### functions ###

### [tune brt for logistic responses] modeling ###
### [tune brt for logistic responses] selecting optimal number of background sites ###

### [simple] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [sample size] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### [prevalence] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [extent] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [resolution] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### [correlated TRUE & FALSE] ###

### [tune brt for bivariate responses] modeling ###
### [tune brt for bivariate responses] selecting optimal number of background sites ###
### [bivariate] ###
### [extra false variable]
### [missing true variable] ###
	
#######################
### master settings ###
#######################

	# source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/Scenarios.r')
	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')
	tempDrive <- 'C:'

	# # source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts/Scenarios.r')
	# setwd('H:/Global Change Program/Research/ENMs - Predictor Inference')
	# tempDrive <- 'C:'
	# # tempDrive <- 'D:'
	# # tempDrive <- 'E:'

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

	# ## iterations
	# iters <- 1:100 # iterations to do -- want 100 total
	iters <- 1:2 # iterations to do -- want 100 total
	# iters <- 100:1 # iterations to do -- want 100 total

	# iters <- 1:50 # iterations to do
	# iters <- 51:100 # iterations to do

	# iters <- 1:25 # iterations to do
	# iters <- 26:50 # iterations to do
	# iters <- 51:75 # iterations to do
	# iters <- 76:100 # iterations to do

	algos <- c('omniscient', 'maxent', 'brt', 'gam')
	# algos <- c('omniscient', 'maxent', 'brt')
	# algos <- c('omniscient', 'maxent', 'brt')
	# algos <- c('omniscient', 'maxent')
	# algos <- c('omniscient')
	# algos <- c('maxent')
	# algos <- c('brt')
	# algos <- c('gam')
	# algos <- c('rf')

	# values of Maxent master regularization multiplier to try (Warren & Siefert 2008)
	regMult <- c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
	
	# number of training background sites to test for BRTs in "tuning" exercises
	numBgToTestForBrt <- c(10000, 1000, 200)
	# numBgToTestForBrt <- c(10000)
	# numBgToTestForBrt <- c(1000)
	# numBgToTestForBrt <- c(200)
	
#################
### functions ###
#################

	library(sp)
	library(rgdal)
	library(raster)
	library(rJava)
	options(java.parameters='-Xmx1g' )
	library(gbm)
	library(dismo)
	library(omnibus)
	library(enmSdm)
	library(enmSdmPredImport)
	library(legendary)

	tempDir <- paste0(tempDrive, '/ecology/!Scratch/_TEMP\\') # maxent temp directory

	# get vector of number of training background sites customized for each algorithm
	getNumBg <- function(algos, brtBg=200, gamBg=10000, maxentBg=10000, rfBg=200, otherBg=10000) {
	
		# algos		names of algorithms
		# brtBg, gamBg, maxentBg, rfBg, otherBg
		#			number of background sites for each algorithm
		
		n <- rep(NA, length(algos))
		names(n) <- algos
		
		n[algos %in% 'brt'] <- brtBg
		n[algos %in% 'gam'] <- gamBg
		n[algos %in% 'maxent'] <- maxentBg
		n[algos %in% 'rf'] <- rfBg
		n[is.na(n)] <- otherBg
	
		n
		
	}

	# return parameters for tuning BRTs
	# parameters are decided in the section "[tune brt for logistic responses] selecting optimal number of background sites"
	brtParams <- function(type) {

		# type	'logistic' ==> use parameter set from best set of models tuned on logistic (univariate) responses
		# type	'bivariate' ==> use parameter set from best set of models tuned on bivariate (bivariate) responses
	
		bestParams <- if (type %in% c('logistic', 'logisticShift')) {
			read.csv('./Results/tune brt for logistic responses/Parameters of Best BRT Models.csv')
		} else if (type == 'bivariate') {
			read.csv('./Results/tune brt for bivariate responses/Parameters of Best BRT Models.csv')
		}
		
		lr <<- unique(c(bestParams$learningRate_025quant, bestParams$learningRate_mean, bestParams$learningRate_075quant))

		tcLower <- c(bestParams$treeComplexity_025quant, bestParams$treeComplexity_mean)
		if (tcLower[1] == tcLower[2]) {
			tcLower <- unique(tcLower)
			if (tcLower > 1) tcLower <- c(tcLower - 1, tcLower)
		}
		
		tcUpper <- c(bestParams$treeComplexity_mean, bestParams$treeComplexity_075quant)
		if (tcUpper[1] == tcUpper[2]) {
			tcUpper <- unique(tcUpper)
			tcUpper <- c(tcUpper, tcUpper + 1)
		}
		
		tc <<- unique(c(tcLower, tcUpper))
		
		bf <<- bestParams$bagFraction
		maxTrees <<- bestParams$maxTrees
	
		brtBg <<- bestParams$bestNumBg
	
	}
	
# say('##################################################')
# say('### [tune brt for logistic responses] modeling ###')
# say('##################################################')

	# say('This experiment tunes the settings to be used for BRTs using the simplest landscape.')

	# thisOutDir <- 'tune brt for logistic responses'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	# response <- logistic
	
	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# # create data
	# predImportMakeData(
		# response=response,
		# geography=geography,
		# simDir=paste0(scenarioDir, '/!scenario data - 10000 bg'),
		# numTrainPres=200,
		# numTestPres=200,
		# numBg=10000,
		# iters=iters,
		# overwrite=FALSE,
		# filePrepend='10000 bg',
		# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		# verbose=verbose,
		# circle=FALSE
	# )

	# # BRT tuning parameters
	# lr <- c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005) # learning rate
	# tc <- c(1, 2, 3, 6) # tree complexity
	# bf <- 0.6 # bag fraction
	# maxTrees <- 8000 # maximum number of trees
	
	# # train full models
	# for (numBg in numBgToTestForBrt) {
	
		# say('USING ', numBg, ' BACKGROUND SITES', level=2)

		# predImportTrainModels(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg'),
			# vars=c('T1', 'F1'),
			# algos='brt',
			# type=c('multivariate'),
			# iters=iters,
			# numBg=numBg,
			# filePrepend=NULL,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=maxTrees,
			# learningRate=lr, treeComplexity=tc, bagFraction=bf
		# )
		
	# }

	# # evaluate
	# for (numBg in numBgToTestForBrt) {
	
		# say('USING ', numBg, ' BACKGROUND SITES', level=2)

		# predImportEval(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg'),
			# evalDir=paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg'),
			# algos='brt',
			# type=c('multivariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# filePrepend=NULL,
			# verbose=verbose
		# )
	
		# # from <- paste0(scenarioDir, '/evaluations models with ', numBg, ' bg/Evaluations for multivariate BRT.RData')
		# # to <- paste0(scenarioDir, '/evaluations models with ', numBg, ' bg/Evaluations for multivariate BRT ', prefix(numBg, 5), ' Background Training Sites.RData')
		# # file.rename(from, to)
		
		# # from <- to
		# # to <- paste0(scenarioDir, '/evaluations/Evaluations for multivariate BRT ', prefix(numBg, 5), ' Background Training Sites.RData')
		
		# # dirCreate(paste0(scenarioDir, '/evaluations'))
		# # file.copy(from, to)
	
	# }
	
# say('######################################################################################')
# say('### [tune brt for logistic responses] selecting optimal number of background sites ###')
# say('######################################################################################')

	# thisOutDir <- 'tune brt for logistic responses'
	# scenarioDir <- paste0('./Results/', thisOutDir)

	# ### load evaluations
	# evals <- data.frame()
	# for (numBg in numBgToTestForBrt) {
	
		# load(paste0(scenarioDir, '/models with ', prefix(numBg, 5), ' bg/Evaluations for multivariate BRT.RData'))
		
		# # if (!('numTrainBg' %in% names(perform))) {
			# # addition <- data.frame(numTrainBg=perform$numBg, numTestBg=perform$numBg)
			# # perform <- insertCol(addition, perform, at='response', before=FALSE)
		# # }
		
		# # if (!('rotT1' %in% names(perform))) {
			# # addition <- data.frame(rotT1=rep(NA, nrow(perform)))
			# # perform <- insertCol(addition, perform, at='maxT1', before=FALSE)
		# # }
		
		# # if (!('rotF1' %in% names(perform))) {
			# # addition <- data.frame(rotF1=rep(NA, nrow(perform)))
			# # perform <- insertCol(addition, perform, at='maxF1', before=FALSE)
		# # }
		
		# # while(any(grepl('Strat', names(perform)))) {
		
			# # strat <- which(grepl('Strat', names(perform)))[1]
			# # perform <- perform[ , -strat, drop=FALSE]
		
		# # }
		
		# evals <- rbind(evals, perform)
		
	# }

	# ### plot
	# png(paste0(scenarioDir, '/CBI ~ Number of Training Background Sites.png'))
		# boxplot(cbiMulti ~ numTrainBgMulti, data=evals, ylab='CBI (Multivariate)', xlab='Number of training background sites')
	# dev.off()

	# ### assess by number of BG sites
	# best <- data.frame()
	# for (numBg in numBgToTestForBrt) {
		
		# x <- evals[evals$numTrainBgMulti == numBg, ]
		# avg <- mean(x$cbiMulti, na.rm=TRUE)
		# std <- sd(x$cbiMulti, na.rm=TRUE)
		
		# say('Multivariate model CBI for ', numBg, ' background sites (mean +- SD): ', sprintf('%.3f', avg), ' +- ', sprintf('%.3f', std))
		
		# best <- rbind(
			# best,
			# data.frame(
				# numTrainBg=numBg,
				# cbiMean=avg,
				# cbiSd=std
			# )
		# )
		
	# }
	
	# ### get best number of BG sites
	# bestNumBg <- best$numTrainBg[which.max(best$cbiMean)]
	
	# say('Best number of BG sites is ', bestNumBg, '. Using mean parameter set from this set.')
	
	# ### calculate BRT parameters for best set of BRT models
	# params <- data.frame()
	
	# for (iter in iters) {
	
		# load(paste0(scenarioDir, '/models with ', prefix(bestNumBg, 5), ' bg/multivariate brt/brt model ', prefix(iter, 3), '.Rdata'))
	
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

# say('################')
# say('### [simple] ###')
# say('################')

	# thisOutDir <- 'simple'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	# response <- logistic
	
	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# # # create data
	# # predImportMakeData(
		# # response=response,
		# # geography=geography,
		# # simDir=paste0(scenarioDir, '/', simDir),
		# # numTrainPres=200,
		# # numTestPres=200,
		# # numBg=10000,
		# # iters=iters,
		# # overwrite=FALSE,
		# # filePrepend=NULL,
		# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		# # verbose=verbose,
		# # circle=FALSE
	# # )

	# brtParams('logistic')
	
	# # train full models
	# predImportTrainModels(
		# simDir=paste0(scenarioDir, '/', simDir),
		# modelDir=paste0(scenarioDir, '/', modelDir),
		# vars=c('T1', 'F1'),
		# algos=algos,
		# type=c('multivariate', 'univariate'),
		# iters=iters,
		# numBg=getNumBg(algos, brtBg=brtBg),
		# filePrepend=NULL,
		# overwrite=FALSE,
		# tempDir=tempDir,
		# verbose=verbose,
		# maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
		# regMult=regMult
	# )

	# # evaluate: MULTIVARIATE
	# predImportEval(
		# simDir=paste0(scenarioDir, '/', simDir),
		# modelDir=paste0(scenarioDir, '/', modelDir),
		# evalDir=paste0(scenarioDir, '/', evalDir),
		# algos=algos,
		# type=c('multivariate', 'univariate'),
		# iters=iters,
		# perms=30,
		# ia=TRUE,
		# overwrite=FALSE,
		# filePrepend=NULL,
		# verbose=verbose
	# )

# say('#####################')
# say('### [sample size] ###')
# say('#####################')

	# thisOutDir <- 'sample size'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	# response <- logistic
	
	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# # trainPresSet <- sort(c(2^(3:10), 2^(3:8) + 2^(2:7))) # too many!
	# trainPresSet <- 2^(3:10)
	# # trainPresSet <- rev(trainPresSet)

	# # BRT parameters for simple scenarios
	# brtParams('logistic')

	# # by TRAINING PRESENCE SAMPLE SIZE
	# for (n in trainPresSet) {
		
		# say('SAMPLE SIZE: Training presence sample size = ', n, level=1)
		
		# # create data
		# predImportMakeData(
			# response=response,
			# geography=geography,
			# simDir=paste0(scenarioDir, '/', simDir),
			# numTrainPres=n,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# overwrite=FALSE,
			# filePrepend=paste0('n = ', prefix(n, 4)),
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# verbose=verbose,
			# circle=FALSE
		# )

		# # train full models
		# predImportTrainModels(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# numBg=getNumBg(algos, brtBg=brtBg),
			# filePrepend=paste0('n = ', prefix(n, 4)),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
			# regMult=regMult
		# )

		# # evaluate
		# predImportEval(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# evalDir=paste0(scenarioDir, '/', evalDir),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=TRUE,
			# overwrite=FALSE,
			# filePrepend=paste0('n = ', prefix(n, 4)),
			# verbose=verbose
		# )

	# }

# say('####################')
# say('### [prevalence] ###')
# say('####################')

	# thisOutDir <- 'prevalence'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# ### define species
	# ##################
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# # BRT parameters for simple scenarios
	# brtParams('logistic')

	# # test each inflection point
	# # inflection points chosen to match prevalence of 0.95, 0.85, 0.75, 0.625, 0.5, 0.375, 0.25, 0.15, 0.05 as closely as possible
	# b11Set <- c(-1.74, -1.08, -0.7, -0.33, 0, 0.33, 0.7, 1.08, 1.74)
	# # b11Set <- rev(b11Set)
	
	# for (thisB11 in b11Set) {
	
		# say('PREVALANCE: Testing inflection points at ', thisB11, level=1)
		
		# # define species
		# response <- logisticShift

		# # create data
		# predImportMakeData(
			# response=response,
			# geography=geography,
			# simDir=paste0(scenarioDir, '/', simDir),
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# overwrite=FALSE,
			# filePrepend=paste0('b11 = ', sprintf('%.2f', thisB11)),
			# b0=b0, b1=b1, b2=b2, b11=thisB11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# verbose=verbose,
			# circle=FALSE
		# )
		
		# # train full models
		# predImportTrainModels(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# numBg=getNumBg(algos, brtBg=brtBg),
			# filePrepend=paste0('b11 = ', sprintf('%.2f', thisB11)),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
			# regMult=regMult
		# )

		# # evaluate: MULTIVARIATE
		# predImportEval(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# evalDir=paste0(scenarioDir, '/', evalDir),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# filePrepend=paste0('b11 = ', sprintf('%.2f', thisB11)),
			# verbose=verbose
		# )
			
	# } # next inflection point

# say('################')
# say('### [extent] ###')
# say('################')

	# thisOutDir <- 'extent'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	# response <- logisticShift
	
	# # BRT parameters for simple scenarios
	# brtParams('logistic')

	# # test each landscape size (increase number of cells and range of environment)
	# landSize <- data.frame(landSize=c(125, 251, 501, 1001, 2001, 4001, 8001), min=-1 * c(0.125, 0.25, 0.5, 1, 2, 4, 8), max=c(0.125, 0.25, 0.5, 1, 2, 4, 8))
	
	# landSize <- landSize[nrow(landSize):1, ]
	
	# for (countLandSize in 1:nrow(landSize)) {
	
		# say('EXTENT: Testing landscape size of ', landSize$landSize[countLandSize], ' cells...', level=1)
		
		# # define landscape
		# geography <- list(T1=list(type='linear', min=landSize$min[countLandSize], max=landSize$max[countLandSize]), F1=list(type='random', min=-1, max=1))

		# # define species

		# # create data
		# predImportMakeData(
			# response=response,
			# geography=geography,
			# simDir=paste0(scenarioDir, '/', simDir),
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# overwrite=FALSE,
			# filePrepend=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# verbose=verbose,
			# circle=FALSE,
			# size=landSize$landSize[countLandSize]
		# )
		
		# # train full models
		# predImportTrainModels(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# numBg=getNumBg(algos, brtBg=brtBg),
			# filePrepend=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
			# regMult=regMult
		# )

		# # evaluate
		# predImportEval(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# evalDir=paste0(scenarioDir, '/', evalDir),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# filePrepend=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# verbose=verbose
		# )
			
	# } # next inflection point

say('####################')
say('### [resolution] ###')
say('####################')

	thisOutDir <- 'resolution'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)
	scenario <- 'RESPONSE logistic(T1) MODEL (T1 F1) GEOG linear(T1) random(F1)'
	write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	simDirNative <- paste0(scenarioDir, '/', simDir, ' native')
	simDirSampled <- paste0(scenarioDir, '/', simDir, ' sampled')
	
	modelDirNative <- paste0(scenarioDir, '/models native')
	modelDirSampled <- paste0(scenarioDir, '/models sampled')
	
	evalDirNative <- paste0(scenarioDir, '/', evalDir, ' native')
	evalDirSampled <- paste0(scenarioDir, '/', evalDir, ' sampled')
	
	### define species
	##################

	b0 <- 0 # intercept
	b1 <- 2 # slope of P1
	b2 <- 1 # slope of P2
	b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	b12 <- 0 # slope of T1 * T2
	mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	response <- logistic

	### BRT parameters for simple scenarios
	#######################################
	
	brtParams('logistic')

	### create progress frame
	#########################

	progress <- data.frame()

	# test each grain size
	# sizesSampled <- 2^(6:14) # ORIGINAL
	sizesSampled <- 2^c(6, 10, 14)
	sizeNative <- 2^10
	
	# manipulate SAC
	noise <- c(0, 1/3, 2/3, 1)

	progress <- expand.grid(sizeSampled=sizesSampled, noise=noise)
	progress$string <- paste0('sizeSampled=', progress$sizeSampled, ' noise=', round(progress$noise, 2))

	### create data for scenarios
	#############################
	
	dirCreate(scenarioDir, '/!starts data creation')
	dirCreate(scenarioDir, '/!stops data creation')

	# sets in progress or completed
	started <- list.files(paste0(scenarioDir, '/!starts data creation'))

	# each job
	while (length(started) < nrow(progress)) {
	
		# get index of set needed doing
		if (length(started)==0) {
			doing <- 1
		} else {
			doing <- progress$string[-match(started, progress$string)][1]
			doing <- which(progress$string==doing)
		}
		write.csv(progress$string[doing], paste0(scenarioDir, '/!starts data creation/', progress$string[doing]), row.names=FALSE)

		say('data creation: ', progress$string[doing])
		
		# define landscape
		sizeSampled <- progress$sizeSampled[doing]
		noise <- progress$noise[doing]
		geography <- list(T1=list(type='linear', min=-1, max=1, noise=noise), F1=list(type='random', min=-1, max=1)) ### NEW!

		filePrepend <- progress$string[doing]
		
		# make data
		predImportMakeDataRes(
			response=response,
			geography=geography,
			simDirNative=simDirNative,
			simDirSampled=simDirSampled,
			numTrainPres=200,
			numTestPres=200,
			numBg=10000,
			iters=iters,
			overwrite=FALSE,
			filePrepend=filePrepend,
			b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			verbose=verbose,
			circle=FALSE,
			sizeNative=sizeNative,
			sizeSampled=sizeSampled
		)
	
		# indicate this set complete and save
		write.csv(progress$string[doing], paste0(scenarioDir, '/!stops data creation/', progress$string[doing]), row.names=FALSE)
		started <- list.files(paste0(scenarioDir, '/!starts data creation'))
		
	} # next job

	### model and evaluate
	######################
	
	for (algo in algos) {
		
		dirCreate(scenarioDir, '/!starts ', algo)
		dirCreate(scenarioDir, '/!stops ', algo)

		# sets in progress or completed
		started <- list.files(paste0(scenarioDir, '/!starts ', algo))

		# each job
		while (length(started) < nrow(progress)) {
		
			# get index of set needed doing
			if (length(started)==0) {
				doing <- 1
			} else {
				doing <- progress$string[-match(started, progress$string)][1]
				doing <- which(progress$string==doing)
			}
			write.csv(progress$string[doing], paste0(scenarioDir, '/!starts ', algo, '/', progress$string[doing]), row.names=FALSE)

			say('modeling: ', algo, ' ', progress$string[doing], pre=2, level=2)
			
			# define landscape
			sizeSampled <- progress$sizeSampled[doing]
			noise <- progress$noise[doing]
			geography <- list(T1=list(type='linear', min=-1, max=1, noise=noise), F1=list(type='random', min=-1, max=1)) ### NEW!

			filePrepend <- progress$string[doing]
			
			# train full models
			predImportTrainModels(
				simDir=simDirSampled,
				modelDir=modelDirSampled,
				vars=c('T1', 'F1'),
				algos=algo,
				type=c('multivariate'),
				iters=iters,
				numBg=getNumBg(algos, brtBg=brtBg),
				filePrepend=filePrepend,
				tempDir=tempDir,
				overwrite=FALSE,
				verbose=verbose,
				maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
				regMult=regMult
			)

			# evaluate: SAMPLED
			predImportEval(
				simDir=simDirSampled,
				modelDir=modelDirSampled,
				evalDir=evalDirSampled,
				algos=algo,
				type=c('multivariate'),
				iters=iters,
				perms=30,
				ia=FALSE,
				overwrite=FALSE,
				filePrepend=filePrepend,
				verbose=verbose
			)
				
			# indicate this set complete and save
			write.csv(progress$string[doing], paste0(scenarioDir, '/!stops ', algo, '/', progress$string[doing]), row.names=FALSE)
			started <- list.files(paste0(scenarioDir, '/!starts ', algo))
				
		} # next job
		
	} # next algorithm

# say('#################################')
# say('### [correlated TRUE & FALSE] ###')
# say('#################################')

	# say('Vary strength correlation between a TRUE and FALSE variable.')

	# thisOutDir <- 'correlated TRUE & FALSE'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	# response <- logistic

	# rots <- seq(22.5, 157.5, by=22.5)
	
	# # BRT parameters for simple scenarios
	# brtParams('logistic')

	# for (rot in rots) {
	
		# say('Rotation between TRUE and FALSE is ', rot, 'deg', level=1)
	
		# filePrepend <- paste0('rot(F1)=', rot)
	
		# ### define geography
		# ####################
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1),
			# F1=list(type='linear', min=-1, max=1, rot=rot)
		# )

		# # # create data
		# # predImportMakeData(
			# # response=response,
			# # geography=geography,
			# # simDir=paste0(scenarioDir, '/', simDir),
			# # numTrainPres=200,
			# # numTestPres=200,
			# # numBg=10000,
			# # circle=TRUE,
			# # iters=iters,
			# # overwrite=FALSE,
			# # tempDir=tempDir,
			# # filePrepend=filePrepend,
			# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# # verbose=verbose
		# # )
		
		# # train full models
		# predImportTrainModels(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# numBg=getNumBg(algos, brtBg=brtBg),
			# filePrepend=filePrepend,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
			# regMult=regMult
		# )

		# # evaluate
		# predImportEval(
			# simDir=paste0(scenarioDir, '/', simDir),
			# modelDir=paste0(scenarioDir, '/', modelDir),
			# evalDir=paste0(scenarioDir, '/', evalDir),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=TRUE,
			# strat=FALSE,
			# overwrite=FALSE,
			# filePrepend=filePrepend,
			# verbose=verbose
		# )
			
	# } # next scenario

# say('###################################################')
# say('### [tune brt for bivariate responses] modeling ###')
# say('###################################################')

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
	# rot <- c(22.5, 90, 157.5)
	# rho <- c(-0.75, 0, 0.75)
	# sigmaValues <- c(0.1, 0.3, 0.5)

	# for (rot in rot) {
		# for (thisRho in rho) {
			# for (countSigma1 in seq_along(sigmaValues)) {
				# for (countSigma2 in countSigma1:length(sigmaValues)) {
					
					# line <- data.frame(
						# rot=rot,
						# rho=thisRho,
						# sigma1=sigmaValues[countSigma1],
						# sigma2=sigmaValues[countSigma2]
					# )
					# line$string <- paste(names(line), line, collapse=' ', sep='=')
					# progress <- rbind(progress, line)
					
				# }
			# }
		# }
	# }

	# # BRT tuning parameters
	# lr <- c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005) # learning rate
	# tc <- c(1, 2, 3, 6) # tree complexity
	# bf <- 0.6 # bag fraction
	# maxTrees <- 8000 # maximum number of trees

	# ### for each number of background sites
	# for (numBg in numBgToTestForBrt) {
	
		# say('NUMBER OF TRAINING BG: ', numBg, level=2)
	
		# thisOutDir <- paste0('tune brt for bivariate responses/', prefix(numBg, 5), ' bg')
		# scenarioDir <- paste0('./Results/', thisOutDir)
		# dirCreate(scenarioDir)
		# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))'
		# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

		# dirCreate(scenarioDir, '/!starts - brt')
		# dirCreate(scenarioDir, '/!stops - brt')

		# # sets in progress or completed
		# started <- list.files(paste0(scenarioDir, '/!starts - brt'))

		# # for each SCENARIO
		# while (length(started) < nrow(progress)) {
		
			# # get index of set needed doing
			# if (length(started)==0) {
				# doing <- 1
			# } else {
				# doing <- progress$string[-match(started, progress$string)][1]
				# doing <- which(progress$string==doing)
			# }
			# write.csv(progress$string[doing], paste0(scenarioDir, '/!starts - brt/', progress$string[doing]), row.names=FALSE)

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

			# filePrepend <- paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
			
			# # create data
			# predImportMakeData(
				# response=response,
				# geography=geography,
				# simDir=paste0(scenarioDir, '/', simDir),
				# numTrainPres=200,
				# numTestPres=200,
				# numBg=10000,
				# circle=TRUE,
				# iters=iters,
				# overwrite=FALSE,
				# filePrepend=filePrepend,
				# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
				# verbose=verbose
			# )
			
			# # train full models
			# predImportTrainModels(
				# simDir=paste0(scenarioDir, '/', simDir),
				# modelDir=paste0(scenarioDir, '/', modelDir),
				# vars=c('T1', 'T2'),
				# algos='brt',
				# type='multivariate',
				# iters=iters,
				# numBg=numBg,
				# filePrepend=filePrepend,
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
				# algos='brt',
				# type='multivariate',
				# iters=iters,
				# perms=30,
				# ia=FALSE,
				# overwrite=FALSE,
				# filePrepend=filePrepend,
				# verbose=verbose
			# )
				
			# # indicate this set complete and save
			# write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - brt/', progress$string[doing]), row.names=FALSE)
			# started <- list.files(paste0(scenarioDir, '/!starts - brt'))

		# } # next scenario
		
	# } # next number of BG sites

# say('#######################################################################################')
# say('### [tune brt for bivariate responses] selecting optimal number of background sites ###')
# say('#######################################################################################')
	
	# thisOutDir <- 'tune brt for bivariate responses'
	# scenarioDir <- paste0('./Results/', thisOutDir)

	# ### collate evaluations
	# #######################
	# evals <- data.frame()
	# for (numBg in numBgToTestForBrt) {

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
	# for (numBg in numBgToTestForBrt) {
		
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
	
	# say('Best number of BG sites is ', bestNumBg, '. Using mean parameter set from this set.')
	
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
	
# say('###################')
# say('### [bivariate] ###')
# say('###################')

	# say('Varying strength of variable 1 vs 2 on landscape with 2 linear variables')
	# say('Covariates include landscape rotation and rho')

	# thisOutDir <- 'bivariate'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

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
	# rot <- seq(22.5, 157.5, by=22.5)
	# rho <- seq(-0.75, 0.75, by=0.25)
	# sigma2Values <- seq(0.1, 0.5, by=0.1)

	# # to reduce redundant computations, forcing sigma2 to always be <= sigma1 since they're interchangeable
	# for (thisRot in rot) {
		# for (thisRho in rho) {
			# for (thisSigma1 in sigma2Values) {
				# for (thisSigma2 in seq(min(sigma2Values), thisSigma1, by=0.1)) {
					
					# line <- data.frame(
						# rot=thisRot,
						# rho=thisRho,
						# sigma1=thisSigma1,
						# sigma2=thisSigma2
					# )
					# line$string <- paste(names(line), line, collapse=' ', sep='=')
					# progress <- rbind(progress, line)
					
				# }
			# }
		# }
	# }

	# ### brt parameters
	# brtParams('bivariate')
	
	# ### by ALGORITHM
	# ################
	
	# for (algo in algos) {
		
		# dirCreate(scenarioDir, '/!starts - ', algo)
		# dirCreate(scenarioDir, '/!stops - ', algo)

		# # sets in progress or completed
		# started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		# # by SCENARIO
		# while (length(started) < nrow(progress)) {
		
			# # get index of set needed doing
			# if (length(started)==0) {
				# doing <- 1
			# } else {
				# doing <- progress$string[-match(started, progress$string)][1]
				# doing <- which(progress$string==doing)
			# }
			# write.csv(progress$string[doing], paste0(scenarioDir, '/!starts - ', algo, '/', progress$string[doing]), row.names=FALSE)

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

			# # # create data
			# # predImportMakeData(
				# # response=response,
				# # geography=geography,
				# # simDir=paste0(scenarioDir, '/', simDir),
				# # numTrainPres=200,
				# # numTestPres=200,
				# # numBg=10000,
				# # circle=TRUE,
				# # iters=iters,
				# # overwrite=FALSE,
				# # filePrepend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
				# # verbose=verbose
			# # )
			
			# # train full models
			# predImportTrainModels(
				# simDir=paste0(scenarioDir, '/', simDir),
				# modelDir=paste0(scenarioDir, '/', modelDir),
				# vars=c('T1', 'T2'),
				# algos=algo,
				# type=c('multivariate', 'univariate'),
				# iters=iters,
				# numBg=getNumBg(algo, brtBg=brtBg),
				# filePrepend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# tempDir=tempDir,
				# overwrite=FALSE,
				# verbose=verbose,
				# maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
				# regMult=regMult
			# )

			# # evaluate
			# predImportEval(
				# simDir=paste0(scenarioDir, '/', simDir),
				# modelDir=paste0(scenarioDir, '/', modelDir),
				# evalDir=paste0(scenarioDir, '/', evalDir),
				# algos=algo,
				# type=c('multivariate', 'univariate'),
				# iters=iters,
				# perms=30,
				# ia=TRUE,
				# overwrite=FALSE,
				# filePrepend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# verbose=verbose
			# )
				
			# # indicate this set complete and save
			# write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			# started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		# } # next scenario
		
	# } # next algorithm

say('##############################')
say('### [extra false variable] ###')
say('##############################')

	say('Range is determined by 2 TRUE variables but model is presented with these variables plus one FALSE variable.')
	say('Covariates include landscape rotation and rho')

	thisOutDir <- 'extra false variable'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)
	scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2) linear(F1))'
	write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	### define species
	##################
	
	b0 <- NA # intercept
	b1 <- NA # slope of P1
	b2 <- NA # slope of P2
	b11 <- NA # shift parameter... offset of inflection from 0 on landscape relative to T1
	b12 <- NA # slope of T1 * T2
	mu1 <- 0
	mu2 <- 0
	sigma1 <- NA
	sigma2 <- NA
	rho <- NA

	response <- gaussian

	### create progress frame
	#########################
	progress <- data.frame()
	rot <- c(22.5, 45.0, 67.5, 112.5, 135.0, 157.5, 202.5, 225, 247.5)
	rho <- c(-0.5, 0, 0.5)
	sigmas <- c(0.1, 0.3, 0.5)

	for (thisRot in rot) {
		for (thisRho in rho) {
			for (thisSigma1 in sigmas) {
				for (thisSigma2 in sigmas) {
					
					line <- data.frame(
						rot=thisRot,
						rho=thisRho,
						sigma1=thisSigma1,
						sigma2=thisSigma2
					)
					line$string <- paste(names(line), line, collapse=' ', sep='=')
					progress <- rbind(progress, line)
					
				}
			}
		}
	}

	# progress <- progress[nrow(progress):1, ]
	
	# ########################
	# ### CREATE SCENARIO DATA
	# ########################
	
	# # by SCENARIO
	# for (doing in 1:nrow(progress)) {
	
		# rot <- progress$rot[doing]
		# thisRho <- progress$rho[doing]
		# thisSigma1 <- progress$sigma1[doing]
		# thisSigma2 <- progress$sigma2[doing]

		# filePrepend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
		
		# ### define geography
		# ####################
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1),
			# T2=list(type='linear', min=-1, max=1, rot=90),
			# F1=list(type='linear', min=-1, max=1, rot=rot)
		# )

		# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
	
		# # create data
		# predImportMakeData(
			# response=response,
			# geography=geography,
			# simDir=paste0(scenarioDir, '/', simDir),
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# circle=TRUE,
			# iters=iters,
			# tempDir=tempDir,
			# overwrite=FALSE,
			# filePrepend=filePrepend,
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
			# verbose=verbose
		# )

	# } # next scenario
		
	##########################
	### MODEL AND EVALUATE ###
	##########################
	
	### brt parameters
	brtParams('bivariate')

	for (algo in algos) {
		
		dirCreate(scenarioDir, '/!starts - ', algo)
		dirCreate(scenarioDir, '/!stops - ', algo)

		# sets in progress or completed
		started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		# by SCENARIO
		while (length(started) < nrow(progress)) {
		
			# get index of set needed doing
			if (length(started)==0) {
				doing <- 1
			} else {
				doing <- progress$string[-match(started, progress$string)][1]
				doing <- which(progress$string==doing)
			}
			write.csv(progress$string[doing], paste0(scenarioDir, '/!starts - ', algo, '/', progress$string[doing]), row.names=FALSE)

			rot <- progress$rot[doing]
			thisRho <- progress$rho[doing]
			thisSigma1 <- progress$sigma1[doing]
			thisSigma2 <- progress$sigma2[doing]

			filePrepend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
			
			say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# train full models
			predImportTrainModels(
				simDir=paste0(scenarioDir, '/', simDir),
				modelDir=paste0(scenarioDir, '/', modelDir),
				vars=c('T1', 'T2', 'F1'),
				algos=algo,
				type=c('multivariate', 'reduced', 'univariate'),
				iters=iters,
				numBg=getNumBg(algo, brtBg=brtBg),
				filePrepend=filePrepend,
				overwrite=FALSE,
				verbose=verbose,
				maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
				regMult=regMult
			)

			# evaluate
			predImportEval(
				simDir=paste0(scenarioDir, '/', simDir),
				modelDir=paste0(scenarioDir, '/', modelDir),
				evalDir=paste0(scenarioDir, '/', evalDir),
				algos=algo,
				type=c('multivariate', 'reduced', 'univariate'),
				iters=iters,
				perms=30,
				ia=FALSE,
				strat=FALSE,
				overwrite=FALSE,
				filePrepend=filePrepend,
				verbose=verbose
			)
				
			# indicate this set complete and save
			write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		} # next scenario
		
	} # next algorithm

say('###############################')
say('### [missing true variable] ###')
say('###############################')

	say('Range is determined by 2 TRUE variables but model is presented with the one TRUE variable and one FALSE variable.')
	say('Covariates include landscape rotation and rho')

	thisOutDir <- 'missing true variable'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)
	scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2) linear(F1))'
	write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=FALSE)

	### define species
	##################
	
	b0 <- NA # intercept
	b1 <- NA # slope of P1
	b2 <- NA # slope of P2
	b11 <- NA # shift parameter... offset of inflection from 0 on landscape relative to T1
	b12 <- NA # slope of T1 * T2
	mu1 <- 0
	mu2 <- 0
	sigma1 <- NA
	sigma2 <- NA
	rho <- NA

	response <- gaussian

	### create progress frame
	#########################
	progress <- data.frame()
	rot <- c(22.5, 45.0, 67.5, 112.5, 135.0, 157.5, 202.5, 225, 247.5)
	rho <- c(-0.5, 0, 0.5)
	sigmas <- c(0.1, 0.3, 0.5)

	progress <- expand.grid(rot=rot, rho=rho, sigma1=sigmas, sigma2=sigmas)
	progress$string <- paste0('rot=', progress$rot, ' rho=', progress$rho, ' sigma1=', progress$sigma1, ' sigma2=', progress$sigma2)

	########################
	### CREATE SCENARIO DATA
	########################
	
	# by SCENARIO
	for (doing in 1:nrow(progress)) {
	
		rot <- progress$rot[doing]
		thisRho <- progress$rho[doing]
		thisSigma1 <- progress$sigma1[doing]
		thisSigma2 <- progress$sigma2[doing]

		filePrepend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
		
		### define geography
		####################
		geography <- list(
			T1=list(type='linear', min=-1, max=1),
			T2=list(type='linear', min=-1, max=1, rot=90),
			F1=list(type='linear', min=-1, max=1, rot=rot)
		)

		say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
	
		# create data
		predImportMakeData(
			response=response,
			geography=geography,
			simDir=paste0(scenarioDir, '/', simDir),
			numTrainPres=200,
			numTestPres=200,
			numBg=10000,
			circle=TRUE,
			iters=iters,
			overwrite=FALSE,
			filePrepend=filePrepend,
			b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
			verbose=verbose
		)

	} # next scenario
		
	##########################
	### MODEL AND EVALUATE ###
	##########################
	
	### brt parameters
	brtParams('bivariate')

	for (algo in algos) {
		
		dirCreate(scenarioDir, '/!starts - ', algo)
		dirCreate(scenarioDir, '/!stops - ', algo)

		# sets in progress or completed
		started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		# by SCENARIO
		while (length(started) < nrow(progress)) {
		
			# get index of set needed doing
			if (length(started)==0) {
				doing <- 1
			} else {
				doing <- progress$string[-match(started, progress$string)][1]
				doing <- which(progress$string==doing)
			}
			write.csv(progress$string[doing], paste0(scenarioDir, '/!starts - ', algo, '/', progress$string[doing]), row.names=FALSE)

			rot <- progress$rot[doing]
			thisRho <- progress$rho[doing]
			thisSigma1 <- progress$sigma1[doing]
			thisSigma2 <- progress$sigma2[doing]

			filePrepend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
			
			say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# train full models
			predImportTrainModels(
				simDir=paste0(scenarioDir, '/', simDir),
				modelDir=paste0(scenarioDir, '/', modelDir),
				vars=c('T1', 'F1'),
				algos=algo,
				type=c('multivariate', 'univariate'),
				iters=iters,
				numBg=getNumBg(algo, brtBg=brtBg),
				filePrepend=filePrepend,
				tempDir=tempDir,
				overwrite=FALSE,
				verbose=verbose,
				maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
				regMult=regMult
			)

			# evaluate
			predImportEval(
				simDir=paste0(scenarioDir, '/', simDir),
				modelDir=paste0(scenarioDir, '/', modelDir),
				evalDir=paste0(scenarioDir, '/', evalDir),
				algos=algo,
				type=c('multivariate', 'univariate'),
				iters=iters,
				perms=30,
				ia=FALSE,
				strat=FALSE,
				overwrite=FALSE,
				filePrepend=filePrepend,
				verbose=verbose
			)
				
			# indicate this set complete and save
			write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		} # next scenario
		
	} # next algorithm

say('###############################################################################################')
say('###############################################################################################')
say('### END END END END END END END END END END END END END END END END END END END END END END ###')
say('###############################################################################################')
say('###############################################################################################')

say(date())

