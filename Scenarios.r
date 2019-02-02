## SDM PREDICTOR INFERENCE - SCENARIOS
## Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org

	memory.limit(memory.limit() * 2^30)
	rm(list=ls())
	options(keep.source=FALSE) # manage memory
	gc()
	print('')
	print(date())

### CONTENTS ###
### master settings ###
### functions ###

### [tune brt and rf for logistic responses] ### RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [simple] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [sample size] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### [prevalence] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [extent] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [resolution] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### [correlated TRUE & FALSE] ###

### [tune brt for bivariate responses] ###
### [bivariate] ###
### [extra false variable]
### [missing true variable] ###
	
#######################
### master settings ###
#######################

	# source('C:/ecology/Drive/Research/ENMs - Predictor Inference/Scripts NEW/Scenarios.r')
	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')
	tempDrive <- 'C:'

	# source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts NEW/Scenarios.r')
	# setwd('H:/Global Change Program/Research/ENMs - Predictor Inference')
	# tempDrive <- 'D:'
	# tempDrive <- 'E:'

	# verbose <- 0 # minimal display
	verbose <- 1 # some display -- best for most scenarios
	# verbose <- 2 # much display
	# verbose <- Inf # all display
	debug <- FALSE; modelType <- 'does not matter' # for running code
	# debug <- TRUE; modelType <- 'logistic' # for debugging using logistic response
	# debug <- TRUE; modelType <- 'gaussian' # for debugging using Gaussian response

	# ## iterations
	iters <- 1:100 # iterations to do -- want 100 total
	# iters <- 100:1 # iterations to do -- want 100 total

	# iters <- 1:50 # iterations to do
	# iters <- 51:100 # iterations to do

	# iters <- 1:25 # iterations to do
	# iters <- 26:50 # iterations to do
	# iters <- 51:75 # iterations to do
	# iters <- 76:100 # iterations to do

	# algos <- c('omniscient', 'maxent', 'brt', 'gam')
	# algos <- c('gam', 'omniscient', 'maxent', 'brt')
	# algos <- c('omniscient', 'maxent', 'gam')
	# algos <- c('omniscient', 'maxent')
	# algos <- c('omniscient')
	algos <- c('maxent')
	# algos <- c('brt')
	# algos <- c('gam')
	# algos <- c('rf')

#################
### functions ###
#################

	library(compiler)
	library(sp);
	library(rgdal);
	library(raster);
	library(rJava);
	options(java.parameters='-Xmx1g' );
	library(dismo)
	library(scales)
	library(beanplot)
	library(omnibus)
	library(enmSdm)
	library(enmSdmPredImport)
	library(legendary)

	# files <- listFiles('C:/ecology/Drive/R/enmSdmPredImport/R')
	# for (thisFile in files) source(thisFile)

	tempDir <- paste0(tempDrive, '/ecology/!Scratch/_TEMP\\') # maxent temp directory

# say('#########################################')
# say('### [tune brt for logistic responses] ###')
# say('#########################################')

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
	# mainMakeData(
		# response=response,
		# geography=geography,
		# scenarioDir=scenarioDir,
		# numTrainPres=200,
		# numTestPres=200,
		# numBg=10000,
		# iters=iters,
		# overwrite=FALSE,
		# fileAppend=NULL,
		# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		# verbose=verbose,
		# circle=FALSE
	# )
	
	# # train full models
	# mainTrainModels(
		# scenarioDir=scenarioDir,
		# vars=c('T1', 'F1'),
		# algos='brt',
		# type=c('multivariate'),
		# iters=iters,
		# fileAppend=NULL,
		# overwrite=FALSE,
		# verbose=verbose,
		# maxTrees=8000,
		# learningRate=c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005), treeComplexity=c(1, 2, 3, 6), bagFraction=0.6
	# )

	# # evaluate: MULTIVARIATE
	# mainEvalModels(
		# scenarioDir=scenarioDir,
		# algos='brt',
		# type=c('multivariate'),
		# iters=iters,
		# perms=30,
		# ia=FALSE,
		# overwrite=FALSE,
		# fileAppend=NULL,
		# verbose=verbose
	# )

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
	# # mainMakeData(
		# # response=response,
		# # geography=geography,
		# # scenarioDir=scenarioDir,
		# # numTrainPres=200,
		# # numTestPres=200,
		# # numBg=10000,
		# # iters=iters,
		# # overwrite=FALSE,
		# # fileAppend=NULL,
		# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		# # verbose=verbose,
		# # circle=FALSE
	# # )

	# # train full models
	# mainTrainModels(
		# scenarioDir=scenarioDir,
		# vars=c('T1', 'F1'),
		# algos=algos,
		# type=c('multivariate', 'univariate'),
		# iters=iters,
		# fileAppend=NULL,
		# overwrite=FALSE,
		# tempDir=tempDir,
		# verbose=verbose,
		# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
		# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
	# )

	# # evaluate: MULTIVARIATE
	# mainEvalModels(
		# scenarioDir=scenarioDir,
		# algos=algos,
		# type=c('multivariate', 'univariate'),
		# iters=iters,
		# perms=30,
		# ia=TRUE,
		# overwrite=FALSE,
		# fileAppend=NULL,
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

	# trainPresSet <- sort(c(2^(3:9), 2^(3:8) + 2^(2:7)))
	# # trainPresSet <- rev(trainPresSet)
	
	# # by TRAINING PRESENCE SAMPLE SIZE
	# for (n in trainPresSet) {
		
		# say('SAMPLE SIZE: Training presence sample size = ', n, level=1)
		
		# # # create data
		# # mainMakeData(
			# # response=response,
			# # geography=geography,
			# # scenarioDir=scenarioDir,
			# # numTrainPres=n,
			# # numTestPres=200,
			# # numBg=10000,
			# # iters=iters,
			# # overwrite=FALSE,
			# # fileAppend=paste0('n = ', prefix(n, 4)),
			# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# # verbose=verbose,
			# # circle=FALSE
		# # )
		
		# # train full models
		# mainTrainModels(
			# scenarioDir=scenarioDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# fileAppend=paste0('n = ', prefix(n, 4)),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate
		# mainEvalModels(
			# scenarioDir=scenarioDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=TRUE,
			# overwrite=FALSE,
			# fileAppend=paste0('n = ', prefix(n, 4)),
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
	
	# # test each inflection point
	# # inflection points chosen to match prevalence of 0.95, 0.85, 0.75, 0.625, 0.5, 0.375, 0.25, 0.15, 0.05 as closely as possible
	# b11Set <- c(-1.74, -1.08, -0.7, -0.33, 0, 0.33, 0.7, 1.08, 1.74)
	
	# for (thisB11 in b11Set) {
	
		# say('PREVALANCE: Testing inflection points at ', thisB11, level=1)
		
		# # define species
		# response <- logisticShift

		# # # create data
		# # mainMakeData(
			# # response=response,
			# # geography=geography,
			# # scenarioDir=scenarioDir,
			# # numTrainPres=200,
			# # numTestPres=200,
			# # numBg=10000,
			# # iters=iters,
			# # overwrite=FALSE,
			# # fileAppend=paste0('b11 = ', sprintf('%.2f', thisB11)),
			# # b0=b0, b1=b1, b2=b2, b11=thisB11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# # verbose=verbose,
			# # circle=FALSE
		# # )
		
		# # train full models
		# mainTrainModels(
			# scenarioDir=scenarioDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# fileAppend=paste0('b11 = ', sprintf('%.2f', thisB11)),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate: MULTIVARIATE
		# mainEvalModels(
			# scenarioDir=scenarioDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# fileAppend=paste0('b11 = ', sprintf('%.2f', thisB11)),
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
	
	# # test each landscape size (increase number of cells and range of environment)
	# landSize <- data.frame(landSize=c(125, 251, 501, 1001, 2001, 4001, 8001), min=-1 * c(0.125, 0.25, 0.5, 1, 2, 4, 8), max=c(0.125, 0.25, 0.5, 1, 2, 4, 8))
	
	# for (countLandSize in 1:nrow(landSize)) {
	
		# say('EXTENT: Testing landscape size of ', landSize$landSize[countLandSize], ' cells...', level=1)
		
		# # define landscape
		# geography <- list(T1=list(type='linear', min=landSize$min[countLandSize], max=landSize$max[countLandSize]), F1=list(type='random', min=-1, max=1))

		# # define species
		# response <- logisticShift

		# # # create data
		# # mainMakeData(
			# # response=response,
			# # geography=geography,
			# # scenarioDir=scenarioDir,
			# # numTrainPres=200,
			# # numTestPres=200,
			# # numBg=10000,
			# # iters=iters,
			# # overwrite=FALSE,
			# # fileAppend=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# # verbose=verbose,
			# # circle=FALSE,
			# # size=landSize$landSize[countLandSize]
		# # )
		
		# # train full models
		# mainTrainModels(
			# scenarioDir=scenarioDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# fileAppend=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate
		# mainEvalModels(
			# scenarioDir=scenarioDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# fileAppend=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# verbose=verbose
		# )
			
	# } # next inflection point

# say('####################')
# say('### [resolution] ###')
# say('####################')

	# thisOutDir <- 'resolution'
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
	
	# # test each grain size
	# resolution <- 2^(7:13)
	# # resolution <- rev(2^(7:13)
	
	# for (countRes in seq_along(resolution)) {
	
		# thisRes <- resolution[countRes]
	
		# say('RESOLUTION: Testing resolution of ', thisRes, level=1)
		
		# # define landscape
		# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

		# ### define species
		# ##################
		# response <- logisticShift

		# # create data
		# rescale <- if (thisRes == 1024) { NULL } else if (thisRes < 1024) { 'agg' } else if (thisRes > 2014) { 'disagg' }
		# fact <- if (thisRes == 1024) { NA } else if (thisRes < 1024) { 1024 / thisRes } else if (thisRes > 2014) { thisRes / 1024 }
		
		# # mainMakeData(
			# # response=response,
			# # geography=geography,
			# # scenarioDir=scenarioDir,
			# # numTrainPres=200,
			# # numTestPres=200,
			# # numBg=10000,
			# # iters=iters,
			# # overwrite=FALSE,
			# # fileAppend=paste0('resolution = ', prefix(thisRes, 5)),
			# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# # verbose=verbose,
			# # circle=FALSE,
			# # size=thisRes,
			# # rescale=rescale,
			# # fact=fact,
			# # method='bilinear'			
		# # )
		
		# # train full models
		# mainTrainModels(
			# scenarioDir=scenarioDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# fileAppend=paste0('resolution = ', prefix(thisRes, 5)),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate
		# mainEvalModels(
			# scenarioDir=scenarioDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# fileAppend=paste0('resolution = ', prefix(thisRes, 5)),
			# verbose=verbose
		# )
			
	# } # next resolution

# say('##########################################')
# say('### [tune brt for bivariate responses] ###')
# say('##########################################')

	# say('Varying strength of variable 1 vs 2 on landscape with 2 linear variables')
	# say('Covariates include landscape rotation and rho')

	# thisOutDir <- 'tune brt for bivariate responses'
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

		# # create data
		# mainMakeData(
			# response=response,
			# geography=geography,
			# scenarioDir=scenarioDir,
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# circle=TRUE,
			# iters=iters,
			# overwrite=FALSE,
			# fileAppend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
			# verbose=verbose
		# )
		
		# # train full models
		# mainTrainModels(
			# scenarioDir=scenarioDir,
			# vars=c('T1', 'T2'),
			# algos='brt',
			# type='multivariate',
			# iters=iters,
			# fileAppend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
			# tempDir=tempDir,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=6000, learningRate=c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005), treeComplexity=c(1, 2, 3, 6), bagFraction=0.6
		# )

		# # evaluate
		# mainEvalModels(
			# scenarioDir=scenarioDir,
			# algos='brt',
			# type='multivariate',
			# iters=iters,
			# perms=30,
			# ia=TRUE,
			# overwrite=FALSE,
			# fileAppend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
			# verbose=verbose
		# )
			
		# # indicate this set complete and save
		# write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - brt/', progress$string[doing]), row.names=FALSE)
		# started <- list.files(paste0(scenarioDir, '/!starts - brt'))

	# } # next scenario

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
	
	# ### define species
	# ##################
	# response <- logistic

	# rots <- seq(22.5, 157.5, by=22.5)
	# # rots <- rev(seq(22.5, 157.5, by=22.5))
	# # rots <- rots[1] # MX #4
	# # rots <- rots[2] # MX #4
	# # rots <- rots[3] # MX #4
	# # rots <- rots[4] # MX #4
	# # rots <- rots[5] # MX #3
	# # rots <- rots[6] # MX #3
	# # rots <- rots[7] # MX #3
	
	# for (rot in rots) {
	
		# say('Rotation between TRUE and FALSE is ', rot, 'deg', level=1)
	
		# fileAppend <- paste0('rot(F1)=', rot)
	
		# ### define geography
		# ####################
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1),
			# F1=list(type='linear', min=-1, max=1, rot=rot)
		# )

		# # # create data
		# # mainMakeData(
			# # response=response,
			# # geography=geography,
			# # scenarioDir=scenarioDir,
			# # numTrainPres=200,
			# # numTestPres=200,
			# # numBg=10000,
			# # circle=TRUE,
			# # iters=iters,
			# # overwrite=FALSE,
			# # tempDir=tempDir,
			# # fileAppend=fileAppend,
			# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# # verbose=verbose
		# # )
		
		# # train full models
		# mainTrainModels(
			# scenarioDir=scenarioDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# fileAppend=fileAppend,
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=6000, learningRate=c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005), treeComplexity=c(1, 2, 3, 6), bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate
		# mainEvalModels(
			# scenarioDir=scenarioDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=TRUE,
			# strat=FALSE,
			# overwrite=FALSE,
			# fileAppend=fileAppend,
			# verbose=verbose
		# )
			
	# } # next scenario

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

	# # progress <- progress[nrow(progress):1, ]
	
	# # by ALGORITHM
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
			# # mainMakeData(
				# # response=response,
				# # geography=geography,
				# # scenarioDir=scenarioDir,
				# # numTrainPres=200,
				# # numTestPres=200,
				# # numBg=10000,
				# # circle=TRUE,
				# # iters=iters,
				# # overwrite=FALSE,
				# # fileAppend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
				# # verbose=verbose
			# # )
			
			# # train full models
			# mainTrainModels(
				# scenarioDir=scenarioDir,
				# vars=c('T1', 'T2'),
				# algos=algo,
				# type=c('multivariate', 'univariate'),
				# iters=iters,
				# fileAppend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# tempDir=tempDir,
				# overwrite=FALSE,
				# verbose=verbose,
				# maxTrees=6000, learningRate=0.005, treeComplexity=3, bagFraction=0.6,
				# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
			# )

			# # evaluate
			# mainEvalModels(
				# scenarioDir=scenarioDir,
				# algos=algo,
				# type=c('multivariate', 'univariate'),
				# iters=iters,
				# perms=30,
				# ia=TRUE,
				# overwrite=FALSE,
				# fileAppend=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
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

		# fileAppend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
		
		# ### define geography
		# ####################
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1),
			# T2=list(type='linear', min=-1, max=1, rot=90),
			# F1=list(type='linear', min=-1, max=1, rot=rot)
		# )

		# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
	
		# # create data
		# mainMakeData(
			# response=response,
			# geography=geography,
			# scenarioDir=scenarioDir,
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# circle=TRUE,
			# iters=iters,
			# tempDir=tempDir,
			# overwrite=FALSE,
			# fileAppend=fileAppend,
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
			# verbose=verbose
		# )

	# } # next scenario
		
	##########################
	### MODEL AND EVALUATE ###
	##########################
	
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

			fileAppend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
			
			say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# train full models
			mainTrainModels(
				scenarioDir=scenarioDir,
				vars=c('T1', 'T2', 'F1'),
				algos=algo,
				type=c('multivariate', 'reduced', 'univariate'),
				iters=iters,
				fileAppend=fileAppend,
				overwrite=FALSE,
				verbose=verbose,
				maxTrees=6000, learningRate=0.005, treeComplexity=3, bagFraction=0.6,
				regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
			)

			# evaluate
			mainEvalModels(
				scenarioDir=scenarioDir,
				algos=algo,
				type=c('multivariate', 'reduced', 'univariate'),
				iters=iters,
				perms=30,
				ia=FALSE,
				strat=FALSE,
				overwrite=FALSE,
				fileAppend=fileAppend,
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

		# fileAppend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
		
		# ### define geography
		# ####################
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1),
			# T2=list(type='linear', min=-1, max=1, rot=90),
			# F1=list(type='linear', min=-1, max=1, rot=rot)
		# )

		# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
	
		# # create data
		# mainMakeData(
			# response=response,
			# geography=geography,
			# scenarioDir=scenarioDir,
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# circle=TRUE,
			# iters=iters,
			# overwrite=FALSE,
			# fileAppend=fileAppend,
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
			# verbose=verbose
		# )

	# } # next scenario
		
	##########################
	### MODEL AND EVALUATE ###
	##########################
	
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

			fileAppend <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
			
			say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# train full models
			mainTrainModels(
				scenarioDir=scenarioDir,
				vars=c('T1', 'F1'),
				algos=algo,
				type=c('multivariate', 'univariate'),
				iters=iters,
				fileAppend=fileAppend,
				tempDir=tempDir,
				overwrite=FALSE,
				verbose=verbose,
				maxTrees=6000, learningRate=0.005, treeComplexity=3, bagFraction=0.6,
				regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
			)

			# evaluate
			mainEvalModels(
				scenarioDir=scenarioDir,
				algos=algo,
				type=c('multivariate', 'univariate'),
				iters=iters,
				perms=30,
				ia=FALSE,
				strat=FALSE,
				overwrite=FALSE,
				fileAppend=fileAppend,
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


