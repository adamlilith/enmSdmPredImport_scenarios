## SDM PREDICTOR INFERENCE - ILLUSTRATIONS
## Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org

memory.limit(memory.limit() * 2^30)
rm(list=ls())
options(keep.source=FALSE) # manage memory
gc()

# # source('C:/ecology/Drive/Research/ENMs - Predictor Inference/Scripts NEW/Scenarios.r')
# setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')
# tempDrive <- 'C:'

# source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts NEW/Scenarios.r')
setwd('H:/Global Change Program/Research/ENMs - Predictor Inference')
tempDrive <- 'C:'
# tempDrive <- 'D:'
# tempDrive <- 'E:'

library(compiler); library(sp); library(rgdal); library(raster); library(rJava); options(java.parameters='-Xmx1g' ); library(dismo); library(gbm); library(mgcv); library(MuMIn); library(scales); library(beanplot); library(hier.part); library(omnibus); library(enmSdm)

files <- listFiles('./Scripts NEW/Functions')
for (thisFile in files) source(thisFile)

print('')
print(date())

#######################
### master settings ###
#######################

# verbose <- 0 # minimal display
verbose <- 1 # some display -- best for most scenarios
# verbose <- 2 # much display
# verbose <- Inf # all display
debug <- FALSE; modelType <- 'does not matter' # for running code
# debug <- TRUE; modelType <- 'logistic' # for debugging using logistic response
# debug <- TRUE; modelType <- 'gaussian' # for debugging using Gaussian response


### ILUSTRATIONS ###########################################################################

### DEMONSTRATIONS
### [Maxent gain ~ sample size]
### [algorithm-specific variable importance]

### EXPERIMENTS
### ### [tune brt and rf for logistic responses] ### RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [simple] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [sample size] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### QUESTIONS: Effect of prevalence, extent, and resolution?
### [prevalence] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [extent] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [resolution] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### QUESTION: Two true variables vs one another?  Effect of synergy, antagonism, and independence of variables in niche?
### [tune brt for bivariate responses] ###
### [bivariate] ###

# say('#########################################')
# say('### [tune brt for logistic responses] ###')
# say('#########################################')

	# say('This experiment tunes the settings to be used for BRTs using the simplest landscape.')

	# thisOutDir <- 'tune brt for logistic responses'
	# outDir <- paste0('./Results/', thisOutDir)
	# dirCreate(outDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# ### define species
	# ##################
	# response <- logistic
	# species <- paste0('response(x1=landscape[[1]], x2=0, b0=', b0, ', b1=', b1, ', b2=', b2, ', b11=', b11, ', b12=', b12, ')')

	# # create data
	# mainMakeData(
		# response=response,
		# species=species,
		# geography=geography,
		# outDir=outDir,
		# numTrainPres=200,
		# numTestPres=200,
		# numBg=10000,
		# iters=iters,
		# overwrite=FALSE,
		# suffix=NULL,
		# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		# verbose=verbose,
		# circle=FALSE
	# )
	
	# # train full models
	# mainTrainModels(
		# outDir=outDir,
		# vars=c('T1', 'F1'),
		# algos='brt',
		# type=c('multivariate'),
		# iters=iters,
		# suffix=NULL,
		# overwrite=FALSE,
		# verbose=verbose,
		# maxTrees=8000,
		# learningRate=c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005), treeComplexity=c(1, 2, 3, 6), bagFraction=0.6
	# )

	# # evaluate: MULTIVARIATE
	# mainEval(
		# outDir=outDir,
		# algos='brt',
		# type=c('multivariate'),
		# iters=iters,
		# perms=30,
		# ia=FALSE,
		# overwrite=FALSE,
		# suffix=NULL,
		# verbose=verbose
	# )

# say('################')
# say('### [simple] ###')
# say('################')

	# thisOutDir <- 'simple'
	# outDir <- paste0('./Results/', thisOutDir)
	# dirCreate(outDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# ### define species
	# ##################
	# response <- logistic
	# species <- paste0('response(x1=landscape[[1]], x2=0, b0=', b0, ', b1=', b1, ', b2=', b2, ', b11=', b11, ', b12=', b12, ')')

	# # # create data
	# # mainMakeData(
		# # response=response,
		# # species=species,
		# # geography=geography,
		# # outDir=outDir,
		# # numTrainPres=200,
		# # numTestPres=200,
		# # numBg=10000,
		# # iters=iters,
		# # overwrite=FALSE,
		# # suffix=NULL,
		# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		# # verbose=verbose,
		# # circle=FALSE
	# # )

	# # train full models
	# mainTrainModels(
		# outDir=outDir,
		# vars=c('T1', 'F1'),
		# algos=algos,
		# type=c('multivariate', 'univariate'),
		# iters=iters,
		# suffix=NULL,
		# overwrite=FALSE,
		# verbose=verbose,
		# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
		# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
	# )

	# # evaluate: MULTIVARIATE
	# mainEval(
		# outDir=outDir,
		# algos=algos,
		# type=c('multivariate', 'univariate'),
		# iters=iters,
		# perms=30,
		# ia=TRUE,
		# overwrite=FALSE,
		# suffix=NULL,
		# verbose=verbose
	# )

# say('#####################')
# say('### [sample size] ###')
# say('#####################')

	# thisOutDir <- 'sample size'
	# outDir <- paste0('./Results/', thisOutDir)
	# dirCreate(outDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

	# ### define species
	# ##################
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	# # define species
	# response <- logistic
	# species <- paste0('response(x1=landscape[[1]], x2=0, b0=', b0, ', b1=', b1, ', b2=', b2, ', b11=', b11, ', b12=', b12, ')')

	# trainPresSet <- sort(c(2^(3:9), 2^(3:8) + 2^(2:7)))
	
	# # by TRAINING PRESENCE SAMPLE SIZE
	# for (n in trainPresSet) {
		
		# say('SAMPLE SIZE: Training presence sample size = ', n, level=1)
		
		# # create data
		# mainMakeData(
			# response=response,
			# species=species,
			# geography=geography,
			# outDir=outDir,
			# numTrainPres=n,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# overwrite=FALSE,
			# suffix=paste0('n = ', prefix(n, 4)),
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# verbose=verbose,
			# circle=FALSE
		# )
		
		# # train full models
		# mainTrainModels(
			# outDir=outDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# suffix=paste0('n = ', prefix(n, 4)),
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate
		# mainEval(
			# outDir=outDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=TRUE,
			# overwrite=FALSE,
			# suffix=paste0('n = ', prefix(n, 4)),
			# verbose=verbose
		# )
		
	# }

say('####################')
say('### [prevalence] ###')
say('####################')

	thisOutDir <- 'prevalence'
	outDir <- paste0('./Results/', thisOutDir)
	dirCreate(outDir)
	scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

	# define landscape
	geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

	### define species
	##################
	b0 <- 0 # intercept
	b1 <- 2 # slope of P1
	b2 <- 1 # slope of P2
	b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	b12 <- 0 # slope of T1 * T2
	mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# test each inflection point
	# inflection points chosen to match prevalance of 0.95, 0.85, 0.75, 0.5, 0.25, 0.15, 0.05 as closely as possible
	b11Set <- c(-1.74, -1.08, -0.7, 0, 0.7, 1.08, 1.74)
	# b11Set <- rev(c(-1.74, -0.7, 0, 0.7, 1.08, 1.74))
	# b11Set <- c(-1.74, -1.08, -0.7, 0, 0.7, 1.08, 1.74)
	
	for (thisB11 in b11Set) {
	
		say('PREVALANCE: Testing inflection points at ', thisB11, level=1)
		
		# define species
		response <- logisticShift
		species <- paste0('response(x1=landscape[[1]], x2=0, b0=', b0, ', b1=', b1, ', b2=', b2, ', b11=', thisB11, ', b12=', b12, ')')

		# create data
		mainMakeData(
			response=response,
			species=species,
			geography=geography,
			outDir=outDir,
			numTrainPres=200,
			numTestPres=200,
			numBg=10000,
			iters=iters,
			overwrite=FALSE,
			suffix=paste0('b11 = ', sprintf('%.2f', thisB11)),
			b0=b0, b1=b1, b2=b2, b11=thisB11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			verbose=verbose,
			circle=FALSE
		)
		
		# train full models
		mainTrainModels(
			outDir=outDir,
			vars=c('T1', 'F1'),
			algos=algos,
			type=c('multivariate', 'univariate'),
			iters=iters,
			suffix=paste0('b11 = ', sprintf('%.2f', thisB11)),
			overwrite=FALSE,
			verbose=verbose,
			maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		)

		source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts NEW/Functions/mainEval.r')
		
		# evaluate: MULTIVARIATE
		mainEval(
			outDir=outDir,
			algos=algos,
			type=c('multivariate', 'univariate'),
			iters=iters,
			perms=30,
			ia=FALSE,
			overwrite=FALSE,
			suffix=paste0('b11 = ', sprintf('%.2f', thisB11)),
			verbose=verbose
		)
			
	} # next inflection point

# say('################')
# say('### [extent] ###')
# say('################')

	# thisOutDir <- 'extent'
	# outDir <- paste0('./Results/', thisOutDir)
	# dirCreate(outDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# # test each landscape size (increase number of cells and range of environment)
	# landSize <- data.frame(landSize=c(125, 251, 501, 1001, 2001, 4001), min=-1 * c(0.125, 0.25, 0.5, 1, 2, 4), max=c(0.125, 0.25, 0.5, 1, 2, 4))
	
	# for (countLandSize in 1:nrow(landSize)) {
	
		# say('EXTENT: Testing landscape size of ', landSize$landSize[countLandSize], ' cells...', level=1)
		
		# # define landscape
		# geography <- list(T1=list(type='linear', min=landSize$min[countLandSize], max=landSize$max[countLandSize]), F1=list(type='random', min=-1, max=1))

		# ### define species
		# ##################
		# response <- logisticShift
		# species <- paste0('response(x1=landscape[[1]], x2=0, b0=', b0, ', b1=', b1, ', b2=', b2, ', b11=', b11, ', b12=', b12, ')')

		# # create data
		# mainMakeData(
			# response=response,
			# species=species,
			# geography=geography,
			# outDir=outDir,
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# overwrite=FALSE,
			# suffix=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# verbose=verbose,
			# circle=FALSE,
			# size=landSize$landSize[countLandSize]
		# )
		
		# # train full models
		# mainTrainModels(
			# outDir=outDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# suffix=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate
		# mainEval(
			# outDir=outDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# suffix=paste0('landscape size = ', prefix(landSize$landSize[countLandSize], 4), ' cells'),
			# verbose=verbose
		# )
			
	# } # next inflection point

# say('####################')
# say('### [resolution] ###')
# say('####################')

	# thisOutDir <- 'resolution'
	# outDir <- paste0('./Results/', thisOutDir)
	# dirCreate(outDir)
	# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	# write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# # test each grain size
	# res <- c(31, 63, 125, 251, 501, 1001, 2001, 4001, 8001)
	
	# for (countRes in seq_along(res)) {
	
		# say('RESOLUTION: Testing resolution of ', res[countRes], level=1)
		
		# # define landscape
		# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))

		# ### define species
		# ##################
		# response <- logisticShift
		# species <- paste0('response(x1=landscape[[1]], x2=0, b0=', b0, ', b1=', b1, ', b2=', b2, ', b11=', b11, ', b12=', b12, ')')

		# # create data
		# mainMakeData(
			# response=response,
			# species=species,
			# geography=geography,
			# outDir=outDir,
			# numTrainPres=200,
			# numTestPres=200,
			# numBg=10000,
			# iters=iters,
			# overwrite=FALSE,
			# suffix=paste0('resolution = ', prefix(res[countRes], 4)),
			# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			# verbose=verbose,
			# circle=FALSE,
			# size=res[countRes]
		# )
		
		# # train full models
		# mainTrainModels(
			# outDir=outDir,
			# vars=c('T1', 'F1'),
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# suffix=paste0('resolution = ', prefix(res[countRes], 4)),
			# overwrite=FALSE,
			# verbose=verbose,
			# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
			# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
		# )

		# # evaluate
		# mainEval(
			# outDir=outDir,
			# algos=algos,
			# type=c('multivariate', 'univariate'),
			# iters=iters,
			# perms=30,
			# ia=FALSE,
			# overwrite=FALSE,
			# suffix=paste0('resolution = ', prefix(res[countRes], 4)),
			# verbose=verbose
		# )
			
	# } # next resolution

# say('##########################################')
# say('### [tune brt for bivariate responses] ###')
# say('##########################################')

	# say('Varying strenth of variable 1 vs 2 on landscape with 2 linear variables')
	# say('Covariates include landscape rotation and rho')

	# thisOutDir <- 'tune brt for bivariate responses'
	# outDir <- paste0('./Results/', thisOutDir)
	# dirCreate(outDir)
	# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))'
	# write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

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

	# # by ALGORITHM
	# for (algo in algos) {
		
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

		# dirCreate(outDir, '/!starts - ', algo)
		# dirCreate(outDir, '/!stops - ', algo)

		# # sets in progress or completed
		# started <- list.files(paste0(outDir, '/!starts - ', algo))

		# # for each SCENARIO
		# while (length(started) < nrow(progress)) {
		
			# # get index of set needed doing
			# if (length(started)==0) {
				# doing <- 1
			# } else {
				# doing <- progress$string[-match(started, progress$string)][1]
				# doing <- which(progress$string==doing)
			# }
			# write.csv(progress$string[doing], paste0(outDir, '/!starts - ', algo, '/', progress$string[doing]), row.names=FALSE)

			# rot <- progress$rot[doing]
			# thisRho <- progress$rho[doing]
			# thisSigma1 <- progress$sigma1[doing]
			# thisSigma2 <- progress$sigma2[doing]

			# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# ### define geography
			# ####################
			# geography <- list(
				# T1=list(type='linear', min=-1, max=1, pregen=FALSE),
				# T2=list(type='linear', min=-1, max=1, pregen=FALSE, rot=rot)
			# )

			# ### define species
			# ##################
			# species <- paste0('response(mu1=', mu1, ', mu2=', mu2, ', x1=landscape[[\'T1\']], x2=landscape[[\'T2\']], sigma1=', thisSigma1, ', sigma2=', thisSigma2, ', rho=', thisRho, ')')

			# # create data
			# mainMakeData(
				# response=response,
				# species=species,
				# geography=geography,
				# outDir=outDir,
				# numTrainPres=200,
				# numTestPres=200,
				# numBg=10000,
				# circle=TRUE,
				# iters=iters,
				# overwrite=FALSE,
				# suffix=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
				# verbose=verbose
			# )
			
			# # train full models
			# mainTrainModels(
				# outDir=outDir,
				# vars=c('T1', 'T2'),
				# algos='brt',
				# type='multivariate',
				# iters=iters,
				# suffix=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# overwrite=FALSE,
				# verbose=verbose,
				# maxTrees=6000, learningRate=c(0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005), treeComplexity=c(1, 2, 3, 6), bagFraction=0.6
			# )

			# # evaluate
			# mainEval(
				# outDir=outDir,
				# algos='brt',
				# type='multivariate',
				# iters=iters,
				# perms=30,
				# ia=TRUE,
				# overwrite=FALSE,
				# suffix=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# verbose=verbose
			# )
				
			# # indicate this set complete and save
			# write.csv(progress$string[doing], paste0(outDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			# started <- list.files(paste0(outDir, '/!starts - ', algo))

		# } # next scenario
		
	# } # next ALGORITHM

# say('###################')
# say('### [bivariate] ###')
# say('###################')

	# say('Varying strenth of variable 1 vs 2 on landscape with 2 linear variables')
	# say('Covariates include landscape rotation and rho')

	# thisOutDir <- 'bivariate'
	# outDir <- paste0('./Results/', thisOutDir)
	# dirCreate(outDir)
	# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))'
	# write.csv(scenario, paste0(outDir, '/!scenario - ', scenario, '.txt'), row.names=F)

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

	# # by ALGORITHM
	# for (algo in algos) {
		
		# ### create progress frame
		# #########################
		# progress <- data.frame()
		# rot <- c(22.5, 45, 67.5, 90, 112.5, 135, 157.5)
		# rho <- c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)
		# sigma2Values <- c(0.1, 0.2, 0.3, 0.4, 0.5)

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

		# dirCreate(outDir, '/!starts - ', algo)
		# dirCreate(outDir, '/!stops - ', algo)

		# # sets in progress or completed
		# started <- list.files(paste0(outDir, '/!starts - ', algo))

		# # for each SCENARIO
		# while (length(started) < nrow(progress)) {
		
			# # get index of set needed doing
			# if (length(started)==0) {
				# doing <- 1
			# } else {
				# doing <- progress$string[-match(started, progress$string)][1]
				# doing <- which(progress$string==doing)
			# }
			# write.csv(progress$string[doing], paste0(outDir, '/!starts - ', algo, '/', progress$string[doing]), row.names=FALSE)

			# rot <- progress$rot[doing]
			# thisRho <- progress$rho[doing]
			# thisSigma1 <- progress$sigma1[doing]
			# thisSigma2 <- progress$sigma2[doing]

			# say('rot = ', rot, ' | rho = ', thisRho, ' | sigma1 = ', thisSigma1, ' | sigma2 = ', thisSigma2, pre=2)
		
			# ### define geography
			# ####################
			# geography <- list(
				# T1=list(type='linear', min=-1, max=1, pregen=FALSE),
				# T2=list(type='linear', min=-1, max=1, pregen=FALSE, rot=rot)
			# )

			# ### define species
			# ##################
			# species <- paste0('response(mu1=', mu1, ', mu2=', mu2, ', x1=landscape[[\'T1\']], x2=landscape[[\'T2\']], sigma1=', thisSigma1, ', sigma2=', thisSigma2, ', rho=', thisRho, ')')

			# # # create data
			# # mainMakeData(
				# # response=response,
				# # species=species,
				# # geography=geography,
				# # outDir=outDir,
				# # numTrainPres=200,
				# # numTestPres=200,
				# # numBg=10000,
				# # circle=TRUE,
				# # iters=iters,
				# # overwrite=FALSE,
				# # suffix=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# # b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho,
				# # verbose=verbose
			# # )
			
			# # train full models
			# mainTrainModels(
				# outDir=outDir,
				# vars=c('T1', 'T2'),
				# algos=algo,
				# type=c('multivariate', 'univariate'),
				# iters=iters,
				# suffix=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# overwrite=FALSE,
				# verbose=verbose,
				# maxTrees=4000, learningRate=c(0.001, 0.0025), treeComplexity=3, bagFraction=0.6,
				# regMult=c(seq(0.5, 3, by=0.5), 4, 5, 7.5, 10)
			# )

			# # evaluate
			# mainEval(
				# outDir=outDir,
				# algos=algo,
				# type=c('multivariate', 'univariate'),
				# iters=iters,
				# perms=30,
				# ia=TRUE,
				# overwrite=FALSE,
				# suffix=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
				# verbose=verbose
			# )
				
			# # indicate this set complete and save
			# write.csv(progress$string[doing], paste0(outDir, '/!stops - ', algo, '/', progress$string[doing]), row.names=FALSE)
			# started <- list.files(paste0(outDir, '/!starts - ', algo))

		# } # next scenario
		
	# } # next ALGORITHM

say('###############################################################################################')
say('###############################################################################################')
say('### END END END END END END END END END END END END END END END END END END END END END END ###')
say('###############################################################################################')
say('###############################################################################################')

say(date())


