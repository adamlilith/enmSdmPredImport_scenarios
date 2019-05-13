### SDM PREDICTOR INFERENCE - SCENARIOS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
###
### The purpose of this script is to 1) generate simulate data for each scenario in each "experiment"; 2) apply species distribution models to each simulated data set; and 3) evaluate the ability of the models to measure variable importance.
###
### Note that this script is really not intended to be run on a single process all at once. Doing so will likely require a few years to complete on a modern desktop computer. Some of the simpler scenarios can be completed using a single process on a single computer in a few hours or a few days, but generally it is advisable to split jobs up by subsetting the set of parameters used to explore the parameters space, then training/testing models using the restricted set of parameters. For example, you could start a process for the [sample size] experiment using the smallest sample size, start another using the next-larger size, and so on. Obviously the utility of this is limited to the number of cores and memory available on your computer. You can speed things up by using more than one computer, of course. This strategy will be greatly facilitated by using computers that share a common server to which they can read/write. If you use this strategy, the best gains will occur if the master working directory in which the scripts, simulated data, models, and evaluations are saved is all on the server.
###
### The [resolution], [bivariate], [extra false variable], and [missing true variable] experiments are so intensive that they are set up to take advantage of "pseudo" high-performance computing across computers linked only by a common server to which they have read/write access (pseudoHPC: see https://github.com/adamlilith/pseudoHPC). Here, the code sets up a common data frame across processes of jobs to do. The processes then check to see if a particular job has been started.  If not, they take the job and write a small "starts" file to the server so other processes know this job is reserved. When the job is complete the process writes another small "stops" file indicating so, then checks the "starts" folder to see if there are remaining jobs to do. The user must make sure in the end that all jobs are completed. This may not occur if there is a runtime error or if access to the server is lost, for example. To ensure all jobs are complete simply delete the files in the "starts" folder and copy over the files in the respective "stops" folder. Then restart the experiment again.  The code will cycle through all jobs and determine if any are indeed complete.

### CONTENTS ###
### initiate ###
### master settings ###
### functions ###

### [simple] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [sample size] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### [prevalence] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [extent] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [resolution] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### [correlated TRUE & FALSE] ###

### [bivariate] ###
### [extra false variable] ###
### [missing true variable] ###

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

	# source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/01 Run Scenarios.r')
	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')
	tempDrive <- 'C:'

	# # source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts/01 Run Scenarios.r')
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
	iters <- 1:100 # iterations to do -- want 100 total
	# iters <- 100:1 # iterations to do -- want 100 total

	# modeloing algorithms
	# algos <- c('omniscient', 'bioclim', 'maxent', 'brt', 'gam', 'rf')
	algos <- c('omniscient')
	# algos <- c('glm') # not to run GLM you will need to remove all unnamed arguments in each predImportTrainModels() function, though tou can include other arguments that are accepted by glm()

	# values of Maxent master regularization multiplier to try (Warren & Siefert 2008)
	regMult <- c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
	
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
			read.csv('./Results/tune brt & rf for logistic responses/Parameters of Best BRT Models.csv')
			
		} else if (type == 'bivariate') {
			read.csv('./Results/tune brt & rf for bivariate responses/Parameters of Best BRT Models.csv')
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
	
say('################')
say('### [simple] ###')
say('################')

	thisOutDir <- 'simple'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)
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
	
	# define landscape
	geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# create data
	predImportMakeData(
		response=response,
		geography=geography,
		simDir=paste0(scenarioDir, '/', simDir),
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

	brtParams('logistic')
	
	# train full models
	predImportTrainModels(
		simDir=paste0(scenarioDir, '/', simDir),
		modelDir=paste0(scenarioDir, '/', modelDir),
		vars=c('T1', 'F1'),
		algos=algos,
		type=c('multivariate', 'univariate'),
		iters=iters,
		numBg=getNumBg(algos, brtBg=brtBg),
		fileFlag=NULL,
		overwrite=FALSE,
		tempDir=tempDir,
		verbose=verbose,
		maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
		regMult=regMult
	)

	# evaluate: MULTIVARIATE
	predImportEval(
		simDir=paste0(scenarioDir, '/', simDir),
		modelDir=paste0(scenarioDir, '/', modelDir),
		evalDir=paste0(scenarioDir, '/', evalDir),
		algos=algos,
		type=c('multivariate', 'univariate'),
		iters=iters,
		perms=30,
		ia=TRUE,
		overwrite=FALSE,
		fileFlag=NULL,
		verbose=verbose
	)

say('#####################')
say('### [sample size] ###')
say('#####################')

	thisOutDir <- 'sample size'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)
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
	
	# define landscape
	geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# trainPresSet <- sort(c(2^(3:10), 2^(3:8) + 2^(2:7))) # too many!
	trainPresSet <- 2^(3:10)
	# trainPresSet <- rev(trainPresSet)

	# BRT parameters for simple scenarios
	brtParams('logistic')

	# by TRAINING PRESENCE SAMPLE SIZE
	for (n in trainPresSet) {
		
		say('SAMPLE SIZE: Training presence sample size = ', n, level=1)
		
		userdata <<- NULL
		
		# create data
		predImportMakeData(
			response=response,
			geography=geography,
			simDir=paste0(scenarioDir, '/', simDir),
			numTrainPres=n,
			numTestPres=200,
			numBg=10000,
			iters=iters,
			sizeNative=1024,
			overwrite=FALSE,
			fileFlag=paste0('n = ', prefix(n, 4)),
			b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			verbose=verbose,
			circle=FALSE
		)

		# train full models
		predImportTrainModels(
			simDir=paste0(scenarioDir, '/', simDir),
			modelDir=paste0(scenarioDir, '/', modelDir),
			vars=c('T1', 'F1'),
			algos=algos,
			type=c('multivariate', 'univariate'),
			iters=iters,
			numBg=getNumBg(algos, brtBg=brtBg),
			fileFlag=paste0('n = ', prefix(n, 4)),
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
			algos=algos,
			type=c('multivariate', 'univariate'),
			iters=iters,
			perms=30,
			ia=TRUE,
			overwrite=FALSE,
			fileFlag=paste0('n = ', prefix(n, 4)),
			verbose=verbose
		)

	}

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

		# userdata <<- NULL

		# # create data
		# predImportMakeData(
			# response=response,
			# geography=geography,
			# simDir=paste0(scenarioDir, '/', simDir),
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# sizeNative=1024,
			# overwrite=FALSE,
			# fileFlag=paste0('b11 = ', sprintf('%.2f', thisB11)),
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
			# fileFlag=paste0('b11 = ', sprintf('%.2f', thisB11)),
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
			# fileFlag=paste0('b11 = ', sprintf('%.2f', thisB11)),
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

		# userdata <<- NULL

		# # create data
		# predImportMakeData(
			# response=response,
			# geography=geography,
			# simDir=paste0(scenarioDir, '/', simDir),
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# sizeNative=landSize$landSize[countLandSize],
			# overwrite=FALSE,
			# fileFlag=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
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
			# fileFlag=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
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
			# fileFlag=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
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
	sizeNative <- 2^10 # grain perceived by species
	sizesSampled <- 2^c(6, 8, 10, 12, 14) # grain at which environmental data is available
	
	# manipulate SAC
	noise <- c(0, 1/3, 2/3, 1)

	progress <- expand.grid(sizeResampled=sizesSampled, noise=noise)
	progress$string <- paste0('sizeResampled=', progress$sizeResampled, ' noise=', round(progress$noise, 2))

	### make data
	#############

	dirCreate(scenarioDir, '/!make data - starts')
	dirCreate(scenarioDir, '/!make data - stops')

	# sets in progress or completed
	started <- list.files(paste0(scenarioDir, '/!make data - starts'))

	# each job
	while (length(started) < nrow(progress)) {
	
		# get index of set needed doing
		if (length(started)==0) {
			doing <- 1
		} else {
			doing <- progress$string[-match(started, progress$string)][1]
			doing <- which(progress$string == doing)
		}
		write.csv(progress$string[doing], paste0(scenarioDir, '/!make data - starts/', progress$string[doing]), row.names=FALSE)

		say('### making data: ', progress$string[doing], pre=1)
		
		# define landscape
		sizeResampled <- progress$sizeResampled[doing]
		noise <- progress$noise[doing]
		geography <- list(T1=list(type='linear', min=-1, max=1, noise=noise), F1=list(type='random', min=-1, max=1)) ### NEW!

		fileFlag <- progress$string[doing]
		userdata <- data.frame(noise=noise)
		
		# make data... actually only need to do this once (not for each algorithm), but creates problems running pseudo-parallel and data creation is a distinct loop from training/evaluation because creation of high-resolution rasters takes a long time, so processes generating low-resolution data can finish fast and start training expecting there to be data for high-resolution scenarios
		predImportMakeData(
			response=response,
			geography=geography,
			simDir=paste0(scenarioDir, '/', simDir),
			numTrainPres=200,
			numTestPres=200,
			numBg=10000,
			iters=iters,
			circle=FALSE,
			sizeNative=sizeNative,
			sizeResampled=sizeResampled,
			overwrite=FALSE,
			fileFlag=fileFlag,
			userdata=userdata,
			b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			verbose=verbose
		)

		# indicate this set complete and save
		write.csv(progress$string[doing], paste0(scenarioDir, '/!make data - stops/', progress$string[doing]), row.names=FALSE)
		started <- list.files(paste0(scenarioDir, '/!make data - starts'))
			
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
				doing <- which(progress$string == doing)
			}
			write.csv(progress$string[doing], paste0(scenarioDir, '/!starts ', algo, '/', progress$string[doing]), row.names=FALSE)

			say('### modeling/evaluating: ', algo, ' ', progress$string[doing], pre=1)
			
			# define landscape
			sizeResampled <- progress$sizeResampled[doing]
			noise <- progress$noise[doing]
			geography <- list(T1=list(type='linear', min=-1, max=1, noise=noise), F1=list(type='random', min=-1, max=1)) ### NEW!

			fileFlag <- progress$string[doing]
			
			# wait until data has been created for this scenario
			# this is necessary because another process may be making the data while the curent one wants to start modeling that scenario
			simExists <- file.exists(paste0(scenarioDir, '/', simDir, '/sizeResampled=', sizeResampled, ' noise=', round(noise, 2), ' sim ', prefix(max(iters), 3), '.RData'))
			
			while (!simExists) {
			
				Sys.sleep(300)
				simExists <- file.exists(paste0(scenarioDir, '/', simDir, '/sizeResampled=', sizeResampled, ' noise=', round(noise, 2), ' sim ', prefix(max(iters), 3), '.RData'))
				
			}
				
			# train full models
			predImportTrainModels(
				simDir=paste0(scenarioDir, '/', simDir),
				modelDir=paste0(scenarioDir, '/', modelDir),
				vars=c('T1', 'F1'),
				algos=algo,
				type=c('multivariate'),
				iters=iters,
				numBg=getNumBg(algos, brtBg=brtBg),
				fileFlag=fileFlag,
				tempDir=tempDir,
				overwrite=FALSE,
				verbose=verbose,
				maxTrees=maxTrees, learningRate=lr, treeComplexity=tc, bagFraction=bf,
				regMult=regMult
			)

			# evaluate: SAMPLED
			predImportEval(
				simDir=paste0(scenarioDir, '/', simDir),
				modelDir=paste0(scenarioDir, '/', modelDir),
				evalDir=paste0(scenarioDir, '/', evalDir),
				algos=algo,
				type=c('multivariate'),
				iters=iters,
				perms=30,
				ia=FALSE,
				overwrite=FALSE,
				fileFlag=fileFlag,
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
	
		# fileFlag <- paste0('rot(F1)=', rot)
	
		# ### define geography
		# ####################
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1),
			# F1=list(type='linear', min=-1, max=1, rot=rot)
		# )

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
			# tempDir=tempDir,
			# fileFlag=fileFlag,
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# verbose=verbose
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
			# fileFlag=fileFlag,
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
			# fileFlag=fileFlag,
			# verbose=verbose
		# )
			
	# } # next scenario

say('###################')
say('### [bivariate] ###')
say('###################')

	say('Varying strength of variable 1 vs 2 on landscape with 2 linear variables')
	say('Covariates include landscape rotation and rho')

	thisOutDir <- 'bivariate'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)
	scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))'
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
	# rot <- seq(22.5, 157.5, by=22.5) # OLD
	rot <- seq(45, 135, by=45)
	# rho <- seq(-0.75, 0.75, by=0.25) # OLD
	rho <- seq(-0.5, 0.5, by=0.5)
	# sigma2Values <- seq(0.1, 0.5, by=0.1)
	sigma2Values <- seq(0.1, 0.5, by=0.2)

	# to reduce redundant computations, forcing sigma2 to always be <= sigma1 since they're interchangeable
	for (thisRot in rot) {
		for (thisRho in rho) {
			for (thisSigma1 in sigma2Values) {
				for (thisSigma2 in seq(min(sigma2Values), thisSigma1, by=0.1)) {
					
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

	### brt parameters
	brtParams('bivariate')
	
	### by ALGORITHM
	################
	
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

			say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			### define geography
			####################
			geography <- list(
				T1=list(type='linear', min=-1, max=1),
				T2=list(type='linear', min=-1, max=1, rot=rot)
			)

			# create data
			predImportMakeData(
				response=response,
				geography=geography,
				simDir=paste0(scenarioDir, '/', simDir),
				numTrainPres=200,
				numTestPres=200,
				numBg=10000,
				circle=TRUE,
				sizeNative=1024,
				iters=iters,
				overwrite=FALSE,
				fileFlag=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
				verbose=verbose
			)
			
			# train full models
			predImportTrainModels(
				simDir=paste0(scenarioDir, '/', simDir),
				modelDir=paste0(scenarioDir, '/', modelDir),
				vars=c('T1', 'T2'),
				algos=algo,
				type=c('multivariate', 'univariate'),
				iters=iters,
				numBg=getNumBg(algo, brtBg=brtBg),
				fileFlag=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
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
				ia=TRUE,
				overwrite=FALSE,
				fileFlag=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				verbose=verbose
			)
				
			# indicate this set complete and save
			write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		} # next scenario
		
	} # next algorithm

# say('##############################')
# say('### [extra false variable] ###')
# say('##############################')

	# say('Range is determined by 2 TRUE variables but model is presented with these variables plus one FALSE variable.')
	# say('Covariates include landscape rotation and rho')

	# thisOutDir <- 'extra false variable'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2) linear(F1))'
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
	# rot <- c(22.5, 45.0, 67.5, 112.5, 135.0, 157.5, 202.5, 225, 247.5)
	# rho <- c(-0.5, 0, 0.5)
	# sigmas <- c(0.1, 0.3, 0.5)

	# progress <- expand.grid(rot=rot, rho=rho, sigma1=sigmas, sigma2=sigmas)
	# progress$string <- paste0('rot=', progress$rot, ' rho=', progress$rho, ' sigma1=', progress$sigma1, ' sigma2=', progress$sigma2)

	# # progress <- progress[nrow(progress):1, ]
	
	# # ########################
	# # ### CREATE SCENARIO DATA
	# # ########################
	
	# # # by SCENARIO
	# # for (doing in 1:nrow(progress)) {
	
		# # rot <- progress$rot[doing]
		# # thisRho <- progress$rho[doing]
		# # thisSigma1 <- progress$sigma1[doing]
		# # thisSigma2 <- progress$sigma2[doing]

		# # fileFlag <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
		
		# # ### define geography
		# # ####################
		# # geography <- list(
			# # T1=list(type='linear', min=-1, max=1),
			# # T2=list(type='linear', min=-1, max=1, rot=90),
			# # F1=list(type='linear', min=-1, max=1, rot=rot)
		# # )

		# # say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
	
		# # # create data
		# # predImportMakeData(
			# # response=response,
			# # geography=geography,
			# # simDir=paste0(scenarioDir, '/', simDir),
			# # numTrainPres=200,
			# # numTestPres=200,
			# # numBg=10000,
			# # circle=TRUE,
			# # sizeNative=1024,
			# # iters=iters,
			# # tempDir=tempDir,
			# # overwrite=FALSE,
			# # fileFlag=fileFlag,
			# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
			# # verbose=verbose
		# # )

	# # } # next scenario
		
	# ##########################
	# ### MODEL AND EVALUATE ###
	# ##########################
	
	# ### brt parameters
	# brtParams('bivariate')

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

			# fileFlag <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
			
			# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# # train full models
			# predImportTrainModels(
				# simDir=paste0(scenarioDir, '/', simDir),
				# modelDir=paste0(scenarioDir, '/', modelDir),
				# vars=c('T1', 'T2', 'F1'),
				# algos=algo,
				# type=c('multivariate', 'reduced', 'univariate'),
				# iters=iters,
				# numBg=getNumBg(algo, brtBg=brtBg),
				# fileFlag=fileFlag,
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
				# type=c('multivariate', 'reduced', 'univariate'),
				# iters=iters,
				# perms=30,
				# ia=FALSE,
				# strat=FALSE,
				# overwrite=FALSE,
				# fileFlag=fileFlag,
				# verbose=verbose
			# )
				
			# # indicate this set complete and save
			# write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			# started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		# } # next scenario
		
	# } # next algorithm

# say('###############################')
# say('### [missing true variable] ###')
# say('###############################')

	# say('Range is determined by 2 TRUE variables but model is presented with the one TRUE variable and one FALSE variable.')
	# say('Covariates include landscape rotation and rho')

	# thisOutDir <- 'missing true variable'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2) linear(F1))'
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
	# rot <- c(22.5, 45.0, 67.5, 112.5, 135.0, 157.5, 202.5, 225, 247.5)
	# rho <- c(-0.5, 0, 0.5)
	# sigmas <- c(0.1, 0.3, 0.5)

	# progress <- expand.grid(rot=rot, rho=rho, sigma1=sigmas, sigma2=sigmas)
	# progress$string <- paste0('rot=', progress$rot, ' rho=', progress$rho, ' sigma1=', progress$sigma1, ' sigma2=', progress$sigma2)

	# ########################
	# ### CREATE SCENARIO DATA
	# ########################
	
	# # by SCENARIO
	# for (doing in 1:nrow(progress)) {
	
		# rot <- progress$rot[doing]
		# thisRho <- progress$rho[doing]
		# thisSigma1 <- progress$sigma1[doing]
		# thisSigma2 <- progress$sigma2[doing]

		# fileFlag <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
		
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
			# sizeNative=1024,
			# iters=iters,
			# overwrite=FALSE,
			# fileFlag=fileFlag,
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
			# verbose=verbose
		# )

	# } # next scenario
		
	# ##########################
	# ### MODEL AND EVALUATE ###
	# ##########################
	
	# ### brt parameters
	# brtParams('bivariate')

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

			# fileFlag <- paste0('rot(F1)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2)
			
			# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# # train full models
			# predImportTrainModels(
				# simDir=paste0(scenarioDir, '/', simDir),
				# modelDir=paste0(scenarioDir, '/', modelDir),
				# vars=c('T1', 'F1'),
				# algos=algo,
				# type=c('multivariate', 'univariate'),
				# iters=iters,
				# numBg=getNumBg(algo, brtBg=brtBg),
				# fileFlag=fileFlag,
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
				# ia=FALSE,
				# strat=FALSE,
				# overwrite=FALSE,
				# fileFlag=fileFlag,
				# verbose=verbose
			# )
				
			# # indicate this set complete and save
			# write.csv(progress$string[doing], paste0(scenarioDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			# started <- list.files(paste0(scenarioDir, '/!starts - ', algo))

		# } # next scenario
		
	# } # next algorithm

say('###############################################################################################')
say('###############################################################################################')
say('### END END END END END END END END END END END END END END END END END END END END END END ###')
say('###############################################################################################')
say('###############################################################################################')

say(date())


