### SDM PREDICTOR INFERENCE - DEMONSTRATIONS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
### source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/Demonstrations.r')

	memory.limit(memory.limit() * 2^30)
	rm(list=ls())
	options(keep.source=FALSE) # manage memory
	print('')
	print(date())
	gc()

### CONTENTS ###
### user settings ###
### constants ###
### libraries and functions ###
### [demo: performance-based versus distance-based metrics of variable importance] ###
### [demo: pitfalls of relative variable importance] ###
	
#####################
### user settings ###
#####################	
	
	### Change these on a case-by-case basis. They won't affect the models or output.
	
	tempDrive <- 'C:'
	setwd(paste0(tempDrive, '/ecology/Drive/Research/ENMs - Predictor Inference'))

	# displaying progress
	verbose <- 1 # 0 ==> minimal, 1 ==> usually best, 2 ==> lots of display, >2 ==> all of it
	
#################
### constants ###
#################	

	### Changing these *does* affect models and output.

	set.seed(1234567890)

###############################
### libraries and functions ###
###############################	

	library(compiler)
	library(sp)
	library(rgdal)
	library(raster)
	library(rJava)
	options(java.parameters='-Xmx1g' )
	library(dismo)
	library(gbm)
	library(mgcv)
	library(MuMIn)
	library(scales)
	library(beanplot)
	library(hier.part)
	
	# The packages below are available on GitHub.
	# To install the latest versions use:
	# devtools::install_github('adamlilith/omnibus')
	# devtools::install_github('adamlilith/statisfactory')
	# devtools::install_github('adamlilith/enmSdm')
	
	library(omnibus)
	library(statisfactory)
	library(enmSdm)

say('######################################################################################')
say('### [demo: performance-based versus distance-based metrics of variable importance] ###')
say('######################################################################################')
	
	# Purpose: This demonstration creates a figure fin the main text in Smith & Santos. The purpose of the figure is to demonstrate the idiosyncrasies of distance-based metrics of predictor importance (versus performance-base metrics).
	
	# Scenario: Two species respond to the same variables. One responds weakly to variable #1 and the other strongly. They both also respond independently of variable #1 to variable #2. They both have logistic response functions to each variable. The variables are orthogonal to one another.
	
	# settings
	beta1sp1 <- 1.5 # slope of response of species 1 to focal variable (variable #1)
	beta1sp2 <- 6 # slope of response of species 2 to focal variable (variable #1)
	beta2 <- 1 # slope of response of both species to variable #2
	iters <- 100 # times to iterate correlation test
	landSize <- 101 # size of landscape (cells on a side)
	thresholds <- seq(0, 1, by=0.01) # thresholds to test
	
	# response function
	# x1, x2: values of the predictors
	# beta1, beta2: slopes of responses to predictors
	resp <- function(x1, x2, beta1, beta2) {
		out <- 1 / (1 + exp(-beta1 * x1 + -beta2 * x2))
		out
	}
	
	
	x1 <- rep(seq(-1, 1, length.out=landSize), landSize)
	x2 <- rep(seq(-1, 1, length.out=landSize), each=landSize)
	
	# actual responses to environment
	resp1 <- resp(x1=x1, x2=x2, beta1=beta1sp1, beta2=beta2)
	resp2 <- resp(x1=x1, x2=x2, beta1=beta1sp2, beta2=beta2)
	
	# simulate presences/absences
	presAbs1 <- ifelse(resp1 >= runif(length(resp1)), 1, 0)
	presAbs2 <- ifelse(resp2 >= runif(length(resp2)), 1, 0)

	presVals1 <- resp1[presAbs1 == 1]
	presVals2 <- resp2[presAbs2 == 1]
	
	absVals1 <- resp1[presAbs1 == 0]
	absVals2 <- resp2[presAbs2 == 0]
	
	# evaluate TRUE model (ie, the generative model)
	aucObs1 <- enmSdm::aucWeighted(presVals1, absVals1)
	aucObs2 <- enmSdm::aucWeighted(presVals2, absVals2)
	
	cbiObs1 <- enmSdm::contBoyce(presVals1, resp1)
	cbiObs2 <- enmSdm::contBoyce(presVals2, resp2)
	
	fpbObs1 <- mean(enmSdm::Fpb(presVals1, resp1, tr=thresholds))
	fpbObs2 <- mean(enmSdm::Fpb(presVals2, resp2, tr=thresholds))

	evaluation1 <- dismo::evaluate(presVals1, absVals1, tr=thresholds)
	evaluation2 <- dismo::evaluate(presVals2, absVals2, tr=thresholds)
	tholds1 <- dismo::threshold(evaluation1)
	tholds2 <- dismo::threshold(evaluation2)
	tssObs1 <- (sum((resp1 >= tholds1$spec_sens) * presAbs1) + sum((resp1 < tholds1$spec_sens) * presAbs1 == 0)) / landSize^2 - 1
	tssObs2 <- (sum((resp2 >= tholds2$spec_sens) * presAbs2) + sum((resp2 < tholds2$spec_sens) * presAbs2 == 0)) / landSize^2 - 1
	
	sensObs1 <- sum(presVals1 >= tholds1$spec_sens) / length(presVals1)
	sensObs2 <- sum(presVals2 >= tholds2$spec_sens) / length(presVals2)
	
	specObs1 <- sum(absVals1 < tholds1$spec_sens) / length(absVals1)
	specObs2 <- sum(absVals2 < tholds2$spec_sens) / length(absVals2)
	
	# permutation test
	evals <- data.frame()
	for (iter in 1:iters) {
		
		x1rand <- sample(x1, length(x1))
		respRand1 <- resp(x1=x1rand, x2=x2, beta1=beta1sp1, beta2=beta2)
		respRand2 <- resp(x1=x1rand, x2=x2, beta1=beta1sp2, beta2=beta2)
		
		presValsRand1 <- respRand1[presAbs1 == 1]
		presValsRand2 <- respRand2[presAbs2 == 1]
		
		absValsRand1 <- respRand1[presAbs1 == 0]
		absValsRand2 <- respRand2[presAbs2 == 0]
		
		# AUC
		aucRand1 <- enmSdm::aucWeighted(presValsRand1, absValsRand1)
		aucRand2 <- enmSdm::aucWeighted(presValsRand2, absValsRand2)
		
		# CBI
		cbiRand1 <- enmSdm::contBoyce(presValsRand1, resp1)
		cbiRand2 <- enmSdm::contBoyce(presValsRand2, resp2)

		# Fpb
		fpbRand1 <- mean(enmSdm::Fpb(presValsRand1, absValsRand1, tr=thresholds))
		fpbRand2 <- mean(enmSdm::Fpb(presValsRand2, absValsRand2, tr=thresholds))
		
		# TSS
		evaluationRand1 <- dismo::evaluate(presValsRand1, absValsRand1, tr=thresholds)
		evaluationRand2 <- dismo::evaluate(presValsRand2, absValsRand2, tr=thresholds)
		tholdsRand1 <- dismo::threshold(evaluationRand1)
		tholdsRand2 <- dismo::threshold(evaluationRand2)
		tssRand1 <- (sum((respRand1 >= tholds1$spec_sens) * presAbs1) + sum((respRand1 < tholds1$spec_sens) * presAbs1 == 0)) / landSize^2 - 1
		tssRand2 <- (sum((respRand2 >= tholds2$spec_sens) * presAbs2) + sum((respRand2 < tholds2$spec_sens) * presAbs2 == 0)) / landSize^2 - 1
		
		# sensitivity and specificity at TSS threshold
		sensRand1 <- sum(presValsRand1 >= tholds1$spec_sens) / length(presValsRand1)
		sensRand2 <- sum(presValsRand2 >= tholds2$spec_sens) / length(presValsRand2)
		
		specRand1 <- sum(absValsRand1 < tholds1$spec_sens) / length(absValsRand1)
		specRand2 <- sum(absValsRand2 < tholds2$spec_sens) / length(absValsRand2)
		
		evals <- rbind(
			evals,
			data.frame(
				iter = iter,
				aucRandSp1 = aucRand1,
				aucRandSp2 = aucRand2,
				cbiRandSp1 = cbiRand1,
				cbiRandSp2 = cbiRand2,
				fpbRandSp1 = fpbRand1,
				fpbRandSp2 = fpbRand2,
				tssRandSp1 = tssRand1,
				tssRandSp2 = tssRand2,
				sensRandSp1 = sensRand1,
				sensRandSp2 = sensRand2,
				specRandSp1 = specRand1,
				specRandSp2 = specRand2
			)
		)
		
	} # next iteration
	
	### summarize
	
	say('Change in probability of occurrence across environmental domain, species 1: ..... ', sprintf('%.2f', diff(range(resp1))))
	say('Change in probability of occurrence across environmental domain, species 2: ..... ', sprintf('%.2f', diff(range(resp2))))
	
	say('Observed AUC, species 1: ...................', sprintf('%.2f', aucObs1), pre=1)
	say('Observed AUC, species 2: ...................', sprintf('%.2f', aucObs2))
	
	say('Permuted AUC, species 1 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$aucRandSp1)), ' +- ', sprintf('%.2f', sd(evals$aucRandSp1)))
	say('Permuted AUC, species 2 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$aucRandSp2)), ' +- ', sprintf('%.2f', sd(evals$aucRandSp2)))
	
	say('Observed CBI, species 1: ...................', sprintf('%.2f', cbiObs1), pre=1)
	say('Observed CBI, species 2: ...................', sprintf('%.2f', cbiObs2))
	
	say('Permuted CBI, species 1 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$cbiRandSp1)), ' +- ', sprintf('%.2f', sd(evals$cbiRandSp1)))
	say('Permuted CBI, species 2 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$cbiRandSp2)), ' +- ', sprintf('%.2f', sd(evals$cbiRandSp2)))
	
	say('Observed mean Fpb, species 1: ...................', sprintf('%.2f', fpbObs1), pre=1)
	say('Observed mean Fpb, species 2: ...................', sprintf('%.2f', fpbObs2))
	
	say('Permuted mean Fpb, species 1 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$fpbRandSp1)), ' +- ', sprintf('%.2f', sd(evals$fpbRandSp1)))
	say('Permuted mean Fpb, species 2 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$fpbRandSp2)), ' +- ', sprintf('%.2f', sd(evals$fpbRandSp2)))
	
	say('Observed TSS, species 1: ...................', sprintf('%.2f', tssObs1), pre=1)
	say('Observed TSS, species 2: ...................', sprintf('%.2f', tssObs2))
	
	say('Permuted TSS, species 1 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$tssRandSp1)), ' +- ', sprintf('%.2f', sd(evals$tssRandSp1)))
	say('Permuted TSS, species 2 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$tssRandSp2)), ' +- ', sprintf('%.2f', sd(evals$tssRandSp2)))
	
	say('Observed Se @ TSS threshold, species 1: ...................', sprintf('%.2f', sensObs1), pre=1)
	say('Observed Se @ TSS threshold, species 2: ...................', sprintf('%.2f', sensObs2))
	
	say('Permuted Se @ TSS threshold, species 1 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$sensRandSp1)), ' +- ', sprintf('%.2f', sd(evals$sensRandSp1)))
	say('Permuted Se @ TSS threshold, species 2 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$sensRandSp2)), ' +- ', sprintf('%.2f', sd(evals$sensRandSp2)))
	
	say('Observed Sp @ TSS threshold, species 1: ...................', sprintf('%.2f', specObs1), pre=1)
	say('Observed Sp @ TSS threshold, species 2: ...................', sprintf('%.2f', specObs2))
	
	say('Permuted Sp @ TSS threshold, species 1 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$specRandSp1)), ' +- ', sprintf('%.2f', sd(evals$specRandSp1)))
	say('Permuted Sp @ TSS threshold, species 2 (mean +- sd): ..... ', sprintf('%.2f', mean(evals$specRandSp2)), ' +- ', sprintf('%.2f', sd(evals$specRandSp2)))
	
	
# say('##################################################')
# say('### [pitfalls of relative variable importance] ###')
# say('##################################################')

	# say('This demonstration illustrates how removing the most important variable results in the second-most important variable gaining equivalent performance even if it is a weak predictor of the distribution.', breaks=90)
	
	# thisOutDir <- 'demo - relative variable importance'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	
	# dirCreate(scenarioDir)
	# scenario <- 'RESPONSE gaussian(T1) + gaussian(T2) MODEL T1 T2 F1 GEOG linear(T1) linear(T2) linear(F1)'
	# write.csv(scenario, paste0(scenarioDir, '/!scenario - ', scenario, '.txt'), row.names=F)

	# # define species
	# b0 <- b1 <- b2 <- b11 <- b12 <- NA
	# mu1 <- mu2 <- 0
	# sigma1 <- 0.1
	# sigma2 <- 0.5
	# rho <- 0
	
	# # generate landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), T2=list(type='linear', min=-1, max=1, rot=45), F1=list(type='sin', min=-1, max=1, rot=-45, freq=2, offset=0))
	
	# ### define species
	# ##################
	# response <- logistic
	# species <- paste0('response(mu1=', mu1, ', mu2=', mu2, ', x1=landscape[[\'T1\']], x2=landscape[[\'T2\']], sigma1=', sigma1, ', sigma2=', sigma2, ', rho=', rho, ')')

	# # create data
	# mainMakeData(
		# response=response,
		# species=species,
		# geography=geography,
		# scenarioDir=scenarioDir,
		# numTrainPres=200,
		# numTestPres=200,
		# numBg=10000,
		# iters=1,
		# overwrite=FALSE,
		# suffix=NULL,
		# b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		# verbose=verbose,
		# circle=TRUE
	# )

	# # train full models
	# mainTrainModels(
		# scenarioDir=scenarioDir,
		# vars=c('T1', 'T2', 'F1'),
		# algos=c('maxent', 'brt'),
		# type=c('multivariate', 'reduced'),
		# iters=1,
		# suffix=NULL,
		# overwrite=FALSE,
		# verbose=verbose,
		# maxTrees=4000, learningRate=0.001, treeComplexity=2, bagFraction=0.6,
		# regMult=1
	# )

	# # evaluate
	# mainEval(
		# scenarioDir=scenarioDir,
		# algos=c('maxent', 'brt'),
		# type=c('multivariate', 'reduced'),
		# iters=1,
		# perms=30,
		# ia=FALSE,
		# overwrite=FALSE,
		# suffix=NULL,
		# verbose=verbose
	# )
	
	# ### results
	
omnibus::say('END', level=1, pre=1)
omnibus::say(date())


