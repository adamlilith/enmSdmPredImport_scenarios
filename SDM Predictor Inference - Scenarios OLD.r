## SDM PREDICTOR INFERENCE - SCENARIOS
## Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org

# source('C:/ecology/Drive/Research/ENMs - Predictor Inference/Scripts/SDM Predictor Inference - Scenarios.r')
# source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts/SDM Predictor Inference - Scenarios.r')
# source('H:/Global Change Program/Research/ENMs - Predictor Inference 2/Scripts/SDM Predictor Inference - Scenarios.r')

memory.limit(memory.limit() * 2^30)
rm(list=ls())
options(keep.source=FALSE) # manage memory
gc()

# library(compiler); library(sp); library(rgdal); library(raster); library(rJava); options(java.parameters='-Xmx1g' ); library(dismo); library(gbm); library(mgcv); library(MuMIn); library(TeachingDemos); library(scales); library(beanplot); library(hier.part); library(omnibus); library(enmSdm)
library(compiler); library(sp); library(rgdal); library(raster); library(rJava); options(java.parameters='-Xmx1g' ); library(dismo); library(gbm); library(mgcv); library(MuMIn); library(scales); library(beanplot); library(hier.part); library(ecospat); library(omnibus); library(enmSdm)

print('')
print(date())

#######################
### master settings ###
#######################

# # # directories
# resultsDir <- 'C:/ecology/Drive/Research/ENMs - Predictor Inference/Results/'
# workDir <- 'C:/ecology/Drive/Research/ENMs - Predictor Inference/'
# source('C:/ecology/Drive/Research/ENMs - Predictor Inference/Scripts/SDM Predictor Inference - Functions.r')

workDir <- 'H:/Global Change Program/Research/ENMs - Predictor Inference 2/'
resultsDir <- 'H:/Global Change Program/Research/ENMs - Predictor Inference 2/Results/'
source('H:/Global Change Program/Research/ENMs - Predictor Inference 2/Scripts/SDM Predictor Inference - Functions.r')
# tempDrive <- 'C:'
tempDrive <- 'D:'
# tempDrive <- 'E:'

# verbose <- 0 # minimal display
verbose <- 1 # some display
# verbose <- 2 # much display
# verbose <- Inf # all display
debug <- FALSE; modelType <- 'does not matter' # for running code
# debug <- TRUE; modelType <- 'logistic' # for debugging using logistic response
# debug <- TRUE; modelType <- 'gaussian' # for debugging using Gaussian response

## iterations
iterToDo <- 1:100 # iterations to do -- want 100 total

# iterToDo <- 1:50 # iterations to do -- want 100 total
# iterToDo <- 51:100 # iterations to do -- want 100 total

# iterToDo <- 1:25 # iterations to do
# iterToDo <- 26:50 # iterations to do
# iterToDo <- 51:75 # iterations to do
# iterToDo <- 76:100 # iterations to do

maxPermIter <- 30 # number of times to perform permutation within each iteration (default 30)
jFoldMax <- 30 # number of j folds for evaluating BG points (default 30)
numTrainPres <- 200 # number of training presences (default 200)
numTestPres <- 200 # number of test presences (default 200)

# algorithm <- c('omniscient', 'maxent', 'brt', 'gam')
# algorithm <- c('brt', 'gam')
algorithm <- c('omniscient')
# algorithm <- c('brt')
# algorithm <- c('gam')
# algorithm <- c('glm')
# algorithm <- c('maxent')
# algorithm <- c('maxnet')

numBg <- 10000 # number of training background sites
# minQuant <- 0.01 # background sites drawn from all cells with Pr(occ) >= quantile(raster, minQuant)... helps control for differences in prevalence NOT USED

min <- -1 # minimum value for linear/step raster
max <- 1 # maximum value for linear/step raster

# colors: manuscript color
bg <- 'white' # background color for plots
fg <- 'black' # foreground color for plots
col1 <- 'chartreuse4' # green
col2 <- 'orange' # orangish
col3 <- 'steelblue' # blueish
col4 <- 'firebrick' # red
col5 <- 'gray' # gray
col6 <- 'cornflowerblue' # blue

# colors: manuscript B&W
bg <- 'white' # background color for plots
fg <- 'black' # foreground color for plots
gray1 <- 'white'
gray2 <- 'gray80' # lightest
gray3 <- 'gray55' # darkest
gray4 <- 'gray30' # darkest

# logistic model parameters
b0 <- 0 # intercept
b1 <- 2 # slope of P1
b2 <- 1 # slope of P2
b11 <- 0 # shift parameter... offset of inflection from 0
b12 <- 0 # slope of P1 * P2

# gaussian model parameters
mu1 <- mu2 <- 0 # optimal values of predictors in gaussian model
sigma1 <- 0.2 # inverse of niche width for T1
sigma2 <- 0.4 # inverse of niche width for T2

### EXPERIMENTS & DEMONSTRATIONS ###########################################################################

### DEMONSTRATIONS
### [Maxent gain ~ sample size]
### [algorithm-specific variable importance]


### EXPERIMENTS
### [simple] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### [sample size] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### QUESTIONS: Effect of prevalence and extent?
### [prevalence] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###
### [extent] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

### QUESTION: Two true variables vs one another?  Effect of synergy, antagonism, and independence of variables in niche?
### [weak vs strong] RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2)) ###
   ### [weak vs strong] compile results ###
   ### [weak vs strong] generic plotting functions ###   
   ### [weak vs strong] analyzing relative strength of 2 TRUE variables with NO niche covariance and NO landscape correlation ###
   ### [weak vs strong] analyzing relative strength of 2 TRUE variables WITH ALL COMBINATIONS OF niche covariance AND landscape correlation ###
   ### [weak vs strong] analyzing relative strength of 2 TRUE variables NO niche covariance AND ALL VALUES OF landscape correlation ###
   ### [weak vs strong] interaction importance ###

### QUESTION: Deciding between two true variables and one correlated false variable
### [correlated false] RESPONSE gaussian(T1 T2) MODEL T1 T2 F1 GEOG cor(linear(T1) linear(T2) linear(F1))

### QUESTION: What happens when you omit a variable?
### [missing variable] RESPONSE gaussian(T1 T2) MODEL T1 F1 GEOG (cor(linear(T1) linear(T2) linear F1)) ###

### QUESTION: What combinations of factors cause concern?
### [experiment] RESPONSE random MODEL random GEOG random



### QUESTION: How well do the tests perform if there is noise in the driving variable(s)?
### [noise] RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2) ###

### QUESTION: Overparameterized model?
### [overparam] RESPONSE gaussian(T1 T2) MODEL T1 T2 F1-F10 GEOG cor(linear(T1-T2) linear(F1-F2)) splitlinear(F3-F6) rand(F7-F10) ###








### HOW PERFORM VS ALL FALSE VARIABLES ###
### [vs F] RESPONSE logistic(T1) MODEL F1 F2 GEOG cor(linear(T1) linear(linear) linear(F2)) ###



# say('###################################')
# say('### [Maxent gain ~ sample size] ###')
# say('###################################')

# directory <- '[Maxent gain ~ sample size]'
# dir.create(paste0(resultsDir, directory), recursive=T, showWarnings=F)

# response <- gaussian
# geography <- list(list(type='linear', min=min, max=max, pregen=TRUE), list(type='linear', min=min, max=max, rot=90, pregen=TRUE))
# say('RESPONSE', paste(deparse(response), collapse=' '), 'GEOGRAPHY', ifelse(class(geography)=='list', paste(unlist(geography), collapse=' '), paste(geography, collapse=' ')))

# # generate landscape
# landscape <- genesis(geography, circle=TRUE)
# names(landscape) <- c('T1', 'T2')

# species <- response(x1=subset(landscape, 1), x2=subset(landscape, 2), sigma1=sigma1, sigma2=sigma2)
# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=c(TRUE, TRUE), name='simple situation', bg=bg, fg=fg)

# ### generate presences/background sites
# prev <- cellStats(species, 'sum')  / ncell(species) # prevalence (including NA cells)

# # draw sites
# set.seed(17)

# presAbs <- -Inf # initial sum of sampled presences
# n <- numTrainPres <- 2000
# while (sum(presAbs) < numTrainPres) {

	# sites <- as.data.frame(randomPointsRobust(species, n, prob=FALSE))
	# prOcc <- extract(species, sites)
	# presAbs <- runif(nrow(sites)) <= prOcc
	
	# n <- round(1.5 * n)
	
# }

# # training/test presences and background sites
# allPres <- sites[which(presAbs), ]
# trainPresEnv <- as.data.frame(extract(landscape, allPres))

# bgSitesTrain <- randomPointsRobust(species, numBg, prob=FALSE)
# bgEnvTrain <- as.data.frame(extract(landscape, bgSitesTrain))

# ## train models for species, using 100 and 1000 presences
# gain <- data.frame()

# for (i in iterToDo) {

	# say('Calculating training gain for iteration ', i)

	# pres1 <- trainPresEnv[sample(1:nrow(allPres), 100), ]
	# pres2 <- trainPresEnv[sample(1:nrow(allPres), 1000), ]
	
	# train1 <- rbind(pres1, bgEnvTrain)
	# train2 <- rbind(pres2, bgEnvTrain)
	
	# presBg1 <- c(rep(1, 100), rep(0, nrow(bgEnvTrain)))
	# presBg2 <- c(rep(1, 1000), rep(0, nrow(bgEnvTrain)))
	
	# train1 <- cbind(presBg1, train1)
	# train2 <- cbind(presBg2, train2)
	
	# names(train1)[1] <- names(train2)[1] <- 'presBg'

	# tempDir <- makeTemp()

	# model1bg <- trainMaxEnt(
		# data=train1,
		# resp='presBg',
		# preds=c('T1', 'T2'),
		# regMult=1,
		# classes='default',
		# testClasses=FALSE,
		# args='addsamplestobackground=false',
		# out='model',
		# anyway=TRUE,
		# scratchDir=tempDir,
		# verbose=FALSE
	# )

	# model1presBg <- trainMaxEnt(
		# data=train1,
		# resp='presBg',
		# preds=c('T1', 'T2'),
		# regMult=1,
		# classes='default',
		# testClasses=FALSE,
		# args='addsamplestobackground=true',
		# out='model',
		# anyway=TRUE,
		# scratchDir=tempDir,
		# verbose=FALSE
	# )

	# model2bg <- trainMaxEnt(
		# data=train2,
		# resp='presBg',
		# preds=c('T1', 'T2'),
		# regMult=1,
		# classes='default',
		# testClasses=FALSE,
		# args='addsamplestobackground=false',
		# out='model',
		# anyway=TRUE,
		# scratchDir=tempDir,
		# verbose=FALSE
	# )

	# model2presBg <- trainMaxEnt(
		# data=train2,
		# resp='presBg',
		# preds=c('T1', 'T2'),
		# regMult=1,
		# classes='default',
		# testClasses=FALSE,
		# args='addsamplestobackground=true',
		# out='model',
		# anyway=TRUE,
		# scratchDir=tempDir,
		# verbose=FALSE
	# )

	# thisGainA <- data.frame(
		# iter=i,
		# n=100,
		# bgOnly=TRUE,
		# trainGainReg=model1bg@results['Unregularized.training.gain', ],
		# trainGainUnreg=model1bg@results['Regularized.training.gain', ],
		# trainGainWithOnlyT1=model1bg@results['Training.gain.with.only.T1', ],
		# trainGainWithoutT1=model1bg@results['Training.gain.without.T1', ],
		# entropy=model1bg@results['Entropy', ],
		# contribT1=model1bg@results['T1.contribution', ]
	# )
	
	# thisGainB <- data.frame(
		# iter=i,
		# n=100,
		# bgOnly=FALSE,
		# trainGainReg=model1presBg@results['Unregularized.training.gain', ],
		# trainGainUnreg=model1presBg@results['Regularized.training.gain', ],
		# trainGainWithOnlyT1=model1presBg@results['Training.gain.with.only.T1', ],
		# trainGainWithoutT1=model1presBg@results['Training.gain.without.T1', ],
		# entropy=model1presBg@results['Entropy', ],
		# contribT1=model1presBg@results['T1.contribution', ]
	# )
	
	# thisGainC <- data.frame(
		# iter=i,
		# n=1000,
		# bgOnly=TRUE,
		# trainGainReg=model2bg@results['Unregularized.training.gain', ],
		# trainGainUnreg=model2bg@results['Regularized.training.gain', ],
		# trainGainWithOnlyT1=model2bg@results['Training.gain.with.only.T1', ],
		# trainGainWithoutT1=model2bg@results['Training.gain.without.T1', ],
		# entropy=model2bg@results['Entropy', ],
		# contribT1=model2bg@results['T1.contribution', ]
	# )
	
	# thisGainD <- data.frame(
		# iter=i,
		# n=1000,
		# bgOnly=FALSE,
		# trainGainReg=model2presBg@results['Unregularized.training.gain', ],
		# trainGainUnreg=model2presBg@results['Regularized.training.gain', ],
		# trainGainWithOnlyT1=model2presBg@results['Training.gain.with.only.T1', ],
		# trainGainWithoutT1=model2presBg@results['Training.gain.without.T1', ],
		# entropy=model2presBg@results['Entropy', ],
		# contribT1=model2presBg@results['T1.contribution', ]
	# )
	
	# gain <- rbind(gain, thisGainA, thisGainB, thisGainC, thisGainD)

# }

# rownames(gain) <- 1:nrow(gain)

# say('################################################')
# say('### [algorithm-specific variable importance] ###')
# say('################################################')

# directory <- '[algorithm-specific variable importance]'
# dir.create(paste0(resultsDir, directory), recursive=T, showWarnings=F)
# scenario <- 'RESPONSE gaussian(T1) MODEL T1 F1 GEOG linear(T1) linear(T1) linear(T1)'
# write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

# response <- gaussian
# geography <- list(list(type='linear', min=min, max=max), list(type='linear', min=min, max=max, rot=67.6), list(type='linear', min=min, max=max, rot=135))
# c(22.5, 45, 67.5, 90, 112.5, 135, 157.5)
# cat ('RESPONSE', paste(deparse(response), collapse=' '), 'GEOGRAPHY', ifelse(class(geography)=='list', paste(unlist(geography), collapse=' '), paste(geography, collapse=' ')), '\n'); flush.console()

# # generate landscape
# landscape <- genesis(geography, circle=FALSE)
# names(landscape) <- c('T1', 'F1')

# # generate species
# species <- response(x1=subset(landscape, 1), b0=b0, b1=b1)
# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=rep(T, nlayers(landscape)), name='simple situation', bg=bg, fg=fg)

# ### calculate variable importance using algorithm-specific measures
# ###################################################################

	# # model!
	# main(
		# species=species,
		# response=response,
		# landscape=landscape,
		# geography=geography,
		# directory=paste0(resultsDir, directory),
		# numTrainPres=1000,
		# numTestPres=1000,
		# numBg=numBg,
		# iterToDo=iterToDo,
		# maxPermIter=maxPermIter,
		# jFoldMax=jFoldMax,
		# algorithm=c('omniscient', 'maxent', 'brt'),
		# suffix='Full Model',
		# verbose=verbose,
		# plotResponse=FALSE,
		# cbi=TRUE,
		# interaction=FALSE,
		# b0=b0, b1=b1, b2=0, b11=0, b12=0
	# )
	
	# omni <- readRDS(paste0(resultsDir, directory, '/Results - OMNISCIENT - Set 01 to 01.rds'))
	# mx <- readRDS(paste0(resultsDir, directory, '/Results - MAXENT - Set 01 to 01.rds'))
	# brt <- readRDS(paste0(resultsDir, directory, '/Results - BRT - Set 01 to 01.rds'))
	
	

say('################')
say('### [simple] ###')
say('################')

	directory <- '[simple]'
	dir.create(paste0(resultsDir, directory), recursive=T, showWarnings=F)
	scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
	write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

	### declare species' response and landscape characteristics
	response <- logistic
	geography <- list(list(type='linear', min=min, max=max), list(type='random', min=min, max=max))

	say('RESPONSE ', paste(deparse(response), collapse=' '), ' GEOGRAPHY ', ifelse(class(geography)=='list', paste(unlist(geography), collapse=' '), paste(geography, collapse=' ')))

	# generate landscape
	landscape <- genesis(geography, circle=FALSE)
	names(landscape) <- c('T1', 'F1')

	# generate species
	species <- response(x1=subset(landscape, 1), b0=b0, b1=b1)
	say('Mean Pr(occ) = ', cellStats(species, 'mean'))

	### make figure for illustrative purposes
	png(paste0(resultsDir, directory, '/response.png'), height=600, width=600, res=200)
	par(pty='s', cex.axis=0.6, cex.lab=0.8, tck=-0.025, mgp=0.5 * c(3, 1, 0), mar=0.5 * c(5, 4, 4, 2) + 0.1)
	plot(x=seq(min, max, by=0.01), y=response(x1=seq(min, max, by=0.01), b0=b0, b1=b1), type='l', xlab='T1', ylab='Pr(occ)', ylim=c(0, 1), lwd=2)
	dev.off()

	# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=rep(T, nlayers(landscape)), name='simple situation', bg=bg, fg=fg)

	# model!
	main(
		species=species,
		response=response,
		landscape=landscape,
		geography=geography,
		directory=paste0(resultsDir, directory),
		numTrainPres=numTrainPres,
		numTestPres=numTestPres,
		numBg=numBg,
		iterToDo=iterToDo,
		maxPermIter=maxPermIter,
		jFoldMax=jFoldMax,
		algorithm=algorithm,
		suffix=NULL,
		verbose=verbose,
		plotResponse=TRUE,
		cbi=TRUE,
		interaction=TRUE,
		b0=b0, b1=b1, b2=0, b11=0, b12=0, mu1=mu1, sigma1=sigma1, sigma2=sigma2
	)

	### make figure of landscape and species for illustrative purposes
	##################################################################
	
	geog <- list(`TRUE`=list(type='linear', min=-1, max=1), `FALSE`=list(type='random', min=-1, max=-1))
	
	landscape <- genesis(geog, circle=FALSE)

	
	
	# # analyze
	# #########

	# # load results

	# results <- list()
	# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern='Results - OMNISCIENT')
	# n <- 1; results[[n]] <- data.frame(); for (f in files) { theseResults <- readRDS(f); results[[n]] <- rbind(results[[n]], theseResults) }

	# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern='Results - MAXENT')
	# n <- 2; results[[n]] <- data.frame(); for (f in files) { theseResults <- readRDS(f); results[[n]] <- rbind(results[[n]], theseResults) }

	# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern='Results - GAM')
	# n <- 3; results[[n]] <- data.frame(); for (f in files) { theseResults <- readRDS(f); results[[n]] <- rbind(results[[n]], theseResults) }

	# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern='Results - BRT')
	# n <- 4; results[[n]] <- data.frame(); for (f in files) { theseResults <- readRDS(f); results[[n]] <- rbind(results[[n]], theseResults) }

	# mergedResults <- merge(results[[1]], results[[2]], all=TRUE)
	# mergedResults <- merge(mergedResults, results[[3]], all=TRUE)
	# mergedResults <- merge(mergedResults, results[[4]], all=TRUE)

	# ### plot mergedResults by algorithm: al interesting combinations of test statistics
	# for (case in c('permute cor random-bg', 'permute cor strat-bg', 'permute cor absences', 'permute cbi', 'permute auc random-bg', 'permute auc absences', 'univariate cor random-bg', 'univariate cor strat-bg', 'univariate cor absences', 'univariate cbi', 'univariate auc random-bg', 'univariate auc absences')) {
	# # for (case in c('permute cbi')) {

		# say(case)

		# if (case=='permute cor random-bg') {
		
			# prefixFileName <- 'COR PERMUTE' # first part of file name
			# suffixFileName <- ' - RANDOM BG' # last part of file name
			
			# resp <- 'cor' # type of response variable ('cor', 'auc', 'cbi', etc.)

			# # response variable(s)
			# needBase <- FALSE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- NULL
			# baselineEnm <- NULL
			# testOmni <- 'corBgFullVsPerm_perm'
			# testEnm <- 'corBgFullVsPerm_perm'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- NULL # text for legend
			# testName <- NULL # text for legend
			
			# ylab <- 'COR' # y-axis label
			# main <- 'Permute Test Using COR with Random Background' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1.1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('a) T1: Permute test (random background)', 'b) F1: Permute test (random background)')
		
		# } else if (case=='permute cor strat-bg') {
		
			# prefixFileName <- 'COR PERMUTE' # first part of file name
			# suffixFileName <- ' - STRATIFIED BG' # last part of file name
		
			# needBase <- FALSE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- NULL
			# baselineEnm <- NULL
			# testOmni <- 'corBgFullVsPermStrat_perm'
			# testEnm <- 'corBgFullVsPermStrat_perm'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- NULL # text for legend
			# testName <- NULL # text for legend
			
			# ylab <- 'COR' # y-axis label
			# main <- 'Permute Test Using COR with Stratified Background' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1.1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('a) T1: Permute test (stratified background)', 'b) F1: Permute test (stratified background)')
		
		# } else if (case=='permute cor absences') {
		
			# prefixFileName <- 'COR PERMUTE' # first part of file name
			# suffixFileName <- ' - ABSENCES' # last part of file name
		
			# # response variable(s)
			# needBase <- FALSE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- NULL
			# baselineEnm <- NULL
			# testOmni <- 'corAbsFullVsPerm_perm'
			# testEnm <- 'corAbsFullVsPerm_perm'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- NULL # text for legend
			# testName <- NULL # text for legend
			
			# ylab <- 'COR' # y-axis label
			# main <- 'Multivariate Permute Test Using COR with Absences' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1.1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('a) T1: Permute test (absences)', 'b) F1: Permute test (absences)')
		
		# } else if (case=='permute cbi') {
		
			# prefixFileName <- 'CBI MULTIVARIATE' # first part of file name
			# suffixFileName <- '- Permute Test' # last part of file name
		
			# # response variable(s)
			# needBase <- TRUE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- 'cbiFull'
			# baselineEnm <- 'cbiFull'
			# testOmni <- 'cbiPerm_perm'
			# testEnm <- 'cbiPerm_perm'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- 'full model' # text for legend
			# testName <- 'permuted' # text for legend
			
			# ylab <- 'CBI' # y-axis label
			# main <- 'Multivariate Non-Permuted (Baseline) vs Permuted (Test) Using CBI' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('a) T1: Permute test', 'b) F1: Permute test')
		
		# } else if (case=='permute auc random-bg') {

			# prefixFileName <- 'AUC MULTIVARIATE' # first part of file name
			# suffixFileName <- ' - RANDOM BG' # last part of file name

			# # response variable(s)
			# needBase <- TRUE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- 'aucBgFullModel'
			# baselineEnm <- 'aucBgFullModel'
			# testOmni <- 'aucBgPerm_perm'
			# testEnm <- 'aucBgPerm_perm'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- 'full model' # text for legend
			# testName <- 'permuted' # text for legend
			
			# ylab <- 'AUC' # y-axis label
			# main <- 'Multivariate Non-Permuted (Baseline) vs Permuted (Test) Using AUC with RANDOM BG' # graph master title
			
			# minVal <- 0 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('a) T1: Permute test (random background)', 'b) F1: Permute test (random background)')
		
		# } else if (case=='permute auc absences') {
		
			# prefixFileName <- 'AUC MULTIVARIATE' # first part of file name
			# suffixFileName <- ' - Absences' # last part of file name
		
			# # response variable(s)
			# needBase <- TRUE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- 'aucAbsFullModel'
			# baselineEnm <- 'aucAbsFullModel'
			# testOmni <- 'aucAbsPerm_perm'
			# testEnm <- 'aucAbsPerm_perm'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- 'full model' # text for legend
			# testName <- 'permuted' # text for legend
			
			# ylab <- 'AUC' # y-axis label
			# main <- 'Multivariate Non-Permuted (Baseline) vs Permuted (Test) Using AUC with Absences' # graph master title
			
			# minVal <- 0 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('a) T1: Permute test (absences)', 'b) F1: Permute test (absences)')
		
		# } else if (case=='univariate cor random-bg') {
		
			# prefixFileName <- 'COR UNIVARIATE' # first part of file name
			# suffixFileName <- ' - Random BG' # last part of file name
		
			# # response variable(s)
			# needBase <- FALSE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- NULL
			# baselineEnm <- NULL
			# testOmni <- 'corBgFullVsUnivar_just'
			# testEnm <- 'corBgFullVsUnivar_just'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- NULL # text for legend
			# testName <- NULL # text for legend
			
			# ylab <- 'COR' # y-axis label
			# main <- 'Univariate COR Test with Random Background' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1.1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('b) T1: Univariate test (random background)', 'c) F1: Univariate test (random background)')
		
		# } else if (case=='univariate cor strat-bg') {
		
			# prefixFileName <- 'COR UNIVARIATE' # first part of file name
			# suffixFileName <- ' - Stratified BG' # last part of file name
		
			# # response variable(s)
			# needBase <- FALSE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- NULL
			# baselineEnm <- NULL
			# testOmni <- 'corBgFullVsUnivarStrat_just'
			# testEnm <- 'corBgFullVsUnivarStrat_just'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- NULL # text for legend
			# testName <- NULL # text for legend
			
			# ylab <- 'COR' # y-axis label
			# main <- 'Univariate Test Using COR with Stratified Background' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1.1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('a) T1: Univariate test (stratified background)', 'b) F1: Univariate test (stratified background)')
		
		# } else if (case=='univariate cor absences') {
		
			# prefixFileName <- 'COR UNIVARIATE' # first part of file name
			# suffixFileName <- ' - Absences' # last part of file name
		
			# # response variable(s)
			# needBase <- FALSE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- NULL
			# baselineEnm <- NULL
			# testOmni <- 'corAbsFullVsUnivar_just'
			# testEnm <- 'corAbsFullVsUnivar_just'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- NULL # text for legend
			# testName <- NULL # text for legend
			
			# ylab <- 'COR' # y-axis label
			# main <- 'Univariate Test Using COR with Absences' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1.1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('c) T1: Univariate test (absences)', 'd) F1: Univariate test (absences)')
		
		# } else if (case=='univariate cbi') {
		
			# prefixFileName <- 'CBI UNIVARIATE' # first part of file name
			# suffixFileName <- '' # last part of file name
		
			# # response variable(s)
			# needBase <- TRUE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- 'cbiFull'
			# baselineEnm <- 'cbiFull'
			# testOmni <- 'cbiUnivar_just'
			# testEnm <- 'cbiUnivar_just'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- 'full model' # text for legend
			# testName <- 'univariate' # text for legend
			
			# ylab <- 'CBI' # y-axis label
			# main <- 'Multivariate (Baseline) vs Univariate (Test) Using CBI' # graph master title
			
			# minVal <- -1 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('c) T1: Univariate test', 'd) F1: Univariate test')
		
		# } else if (case=='univariate auc random-bg') {
		
			# prefixFileName <- 'AUC UNIVARIATE' # first part of file name
			# suffixFileName <- ' - Random BG' # last part of file name
		
			# # response variable(s)
			# needBase <- TRUE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- 'aucBgFullModel'
			# baselineEnm <- 'aucBgFullModel'
			# testOmni <- 'aucBgUnivar_just'
			# testEnm <- 'aucBgUnivar_just'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- 'full model' # text for legend
			# testName <- 'univariate' # text for legend
			
			# ylab <- 'AUC' # y-axis label
			# main <- 'Multivariate (Baseline) vs Univariate (Test) Using AUC with Random Background' # graph master title
			
			# minVal <- 0 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('c) T1: Univariate test (random background)', 'd) F1: Univariate test (random background)')
		
		# } else if (case=='univariate auc absences') {
		
			# prefixFileName <- 'AUC UNIVARIATE' # first part of file name
			# suffixFileName <- ' - Absences' # last part of file name
		
			# # response variable(s)
			# needBase <- TRUE # TRUE if need to plot baseline case, FALSE if not
			# baselineOmni <- 'aucAbsFullModel'
			# baselineEnm <- 'aucAbsFullModel'
			# testOmni <- 'aucAbsUnivar_just'
			# testEnm <- 'aucAbsUnivar_just'
			# invert <- FALSE # use 1 - metric if TRUE
			# baselineName <- 'full model' # text for legend
			# testName <- 'univariate' # text for legend
			
			# ylab <- 'AUC' # y-axis label
			# main <- 'Multivariate (Baseline) vs Univariate (Test) Using AUC with Absences' # graph master title
			
			# minVal <- 0 # minimum y-axis value *if* no y values are < this value
			# maxVal <- 1 # maximum y-axis value *if* no y values are < this value
			# panels <- c('c) T1: Univariate test (absences)', 'd) F1: Univariate test (absences)')
		
		# }

		png(paste0(resultsDir, directory, '/!Importance - ', prefixFileName, suffixFileName, '.png'), width=2000, height=1100, res=300)

			par(mfrow=c(1, 2), fg=fg, bg=bg, col.main=fg, col.axis=fg, col.lab=fg, mgp=0.7 * c(3, 1, 0), cex.lab=1.0, cex.main=1.2, cex.axis=0.7, lwd=1)

			# each panel
			for (variable in c('T1', 'F1')) {

				for (algo in c('omniscient', 'gam', 'maxent', 'brt')) {
					
					niceNames <- c('Omniscient', 'GAM', 'Maxent', 'BRT')

					# get this ENM's results for BASELINE (NOT permuted, etc.)
					if (needBase) {
						
						thisSdmResultsBaseline <- mergedResults[mergedResults$algorithm==algo, ifelse(algo=='omniscient', baselineOmni, baselineEnm)]
						if (invert) thisSdmResultsBaseline <- 1 - thisSdmResults
						thisSdmResultsBaseline <- data.frame(algorithm=algo, response=thisSdmResultsBaseline)
					
					}
					
					# get this ENM's results for TEST case (permuted, etc.)
					thisSdmResultsTest <- mergedResults[mergedResults$algorithm==algo, paste0(testEnm, variable)]
					if (invert) thisSdmResultsTest <- 1 - thisSdmResultsTest
					thisSdmResultsTest <- data.frame(algorithm=algo, response=thisSdmResultsTest)
					
					# blank plot
					if (algo=='omniscient') boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, main=NA, names=niceNames, ylab=ylab, cex.axis=0.5, cex.lab=0.8)
						
					# BASELINE bean plot--truncated at lower 2.5% and upper 2.5%, center bar represents median
					if (needBase) {

						# bar position
						at <- which(algo==c('omniscient', 'gam', 'maxent', 'brt')) - 0.1

						# beanplot
						beanplot(response ~ algorithm, data=thisSdmResultsBaseline, what=c(FALSE, TRUE, TRUE, FALSE), col=list(alpha(c(bg, col1, col1, fg), 0.8)), beanlines='median', bw='nrd0', at=at, add=TRUE, cutmin=quantile(thisSdmResultsBaseline$response, 0.025, na.rm=T), cutmax=quantile(thisSdmResultsBaseline$response, 0.975, na.rm=T), names='', beanlinewd=1, xaxt='n')
						
					}
					
					# bar position
					at <- which(algo==c('omniscient', 'gam', 'maxent', 'brt'))
					if (needBase) at <- at + 0.1

					# PERMUTED bean plot--truncated at lower 2.5% and upper 2.5%, center bar represents median
					beanplot(response ~ algorithm, data=thisSdmResultsTest, what=c(FALSE, TRUE, TRUE, FALSE), col=list(alpha(c(col6, col6, col6, fg), 0.7)), beanlines='median', bw='nrd0', at=at, add=TRUE, cutmin=quantile(thisSdmResultsTest$response, 0.025, na.rm=T), cutmax=quantile(thisSdmResultsTest$response, 0.975, na.rm=T), names=niceNames[at], beanlinewd=1, xaxt='n')
					
					# legend
					if (needBase) {
						legend('bottomright', inset=0, xpd=NA, bty='n', fill=c(bg, alpha(col6, 0.7)), legend=c(baselineName, testName), ncol=1, cex=0.6)
					}
					
					# panel labels
					coords <- par('usr')
					text(x=coords[1] - (0.31 * (coords[2] - coords[1])), y=coords[4] + 0.125 * (coords[4] - coords[3]), labels=ifelse(variable=='T1', panels[1], panels[2]), xpd=NA, pos=4, cex=0.9)
					
				}
				
			}
			
			title(main=main, sub=date(), outer=TRUE, line=-1, cex.main=0.7, cex.sub=0.4)
			
		dev.off()

	}

	# ### plot algorithm-specific measures of importance
	# png(paste0(resultsDir, directory, '/!Importance - Algorithm-specific Measures.png'), width=2000, height=2200, res=300)

		# par(mfrow=c(1, 1), fg=fg, bg=bg, col.main=fg, col.axis=fg, col.lab=fg, mgp=0.7 * c(3, 1, 0), cex.lab=1.0, cex.main=1.2, cex.axis=0.7, lwd=1)

		# # get algorithm-specific data for plotting
		# importT1 <- data.frame(
			# maxentContrib=results[[2]]$maxentContribT1,
			# maxentPermImport=results[[2]]$maxentPermImportT1,
			# maxentTrainGainWithout=results[[2]]$maxentTrainGainWithoutT1,
			# maxentTrainGainWithOnly=results[[2]]$maxentTrainGainWithOnlyT1,
			# gamAiccWeight=results[[3]]$gamAiccWeightT1,
			# brtImport=results[[4]]$brtImportT1
		# )
		
		# importF1 <- data.frame(
			# maxentContrib=results[[2]]$maxentContribF1,
			# maxentPermImport=results[[2]]$maxentPermImportF1,
			# maxentTrainGainWithout=results[[2]]$maxentTrainGainWithoutF1,
			# maxentTrainGainWithOnly=results[[2]]$maxentTrainGainWithOnlyF1,
			# gamAiccWeight=results[[3]]$gamAiccWeightF1,
			# brtImport=results[[4]]$brtImportF1
		# )
		
		# importT1$maxentTrainGainWithout <- importT1$maxentTrainGainWithout * 100
		# importT1$maxentTrainGainWithOnly <- importT1$maxentTrainGainWithOnly * 100
		
		# importF1$maxentTrainGainWithout <- importF1$maxentTrainGainWithout * 100
		# importF1$maxentTrainGainWithOnly <- importF1$maxentTrainGainWithOnly * 100
		
		# # establish empty plot
		# boxplot(matrix(1:ncol(importT1), ncol=ncol(importT1)), ylim=c(-0.1, 1), col=bg, border=bg, names=NA, ylab='Measure', main='Algorithm-specific Measures of Importance')
		
		# # plot each variable
		# for (thisImport in c('importT1', 'importF1')) {

			# import <- get(thisImport)
		
			# # for each measure of importance
			# for (i in 1:ncol(import)) {
				
				# at <- i + ifelse(thisImport=='importT1', -0.1, 0.1)
				
				# beanplot(
					# import[ , i],
					# what=c(FALSE, TRUE, TRUE, FALSE),
					# col=c(ifelse(thisImport=='importT1', col3, col2), ifelse(thisImport=='importT1', col3, col2), ifelse(thisImport=='importT1', col3, col2), fg),
					# beanlines='median',
					# bw='nrd0',
					# at=at,
					# add=TRUE,
					# cutmin=quantile(import[ , i], 0.025, na.rm=T),
					# cutmax=quantile(import[ , i], 0.975, na.rm=T),
					# names=NA,
					# beanlinewd=1,
					# tick=FALSE
				# )
				
			# }
		
		# }
		
		# # category labels
		# text(x=1:ncol(import), y=-0.25, adj=0.5, labels=c('MX\nContrib', 'MX\nPermute', 'MX Gain\nwithout\nx100', 'MX Gain\nwith\nx100', 'GAM\nAICc\nWeight', 'BRT\nImportance'), xpd=NA)
		
		# legend('bottomright', inset=0.01, legend=c('T1-M', 'F1'), fill=c(col3, col2))
		
	# dev.off()

# say('####################')
# say('### [prevalence] ###')
# say('####################')

# directory <- '[prevalence]'
# dir.create(paste0(resultsDir, directory), recursive=T, showWarnings=F)
# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
# say(scenario)
# write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

# inflections <- seq(-2, 2, by=0.5)

# # # create landscape
# response <- logisticShift
# geography <- list(list(type='linear', min=min, max=max), list(type='random', min=min, max=max))
# landscape <- genesis(geography, nrow=2002, circle=FALSE)
# names(landscape) <- c('T1', 'F1')

# # png(paste0(resultsDir, directory, '/response.png'), height=900, width=900, res=200)
	# # par(fg=fg, bg=bg, col.main=fg, col.axis=fg, col.lab=fg, mgp=0.7 * c(3, 1, 0), cex.lab=0.8, cex.main=1.4, cex.axis=0.8)
	# # plot(x=seq(min, max, by=0.01), y=response(x1=seq(min, max, by=0.01), b0=b0, b1=b1, b11=inflections[1]), type='l', lwd=2, xlab='TRUE', ylab='Occupancy', ylim=c(0, 1), xlim=c(min, max))
	# # for (a in 2:length(inflections)) lines(x=seq(min, max, by=0.01), y=response(x1=seq(min, max, by=0.01), b0=b0, b1=b1, b11=inflections[a]), lty=a, lwd=2)
	# # # legend('topleft', title='Inflection at:', legend=inflections, lty=c('solid', 'dashed', 'dotted', 'dotdash', 'solid'), lwd=c(1, 1, 1, 1, 2), inset=0.05, bty='n')
# # dev.off()

# for (countInflection in seq_along(inflections)) {

	# say('------------------ inflection = ', inflections[countInflection])

	# # generate species
	# species <- response(x1=subset(landscape, 1), b0=b0, b1=b1, b11=inflections[countInflection])
	# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

	# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=rep(T, nlayers(landscape)), name=paste0('range inflection at ', inflections[countInflection]), bg=bg, fg=fg)

	# # model!
	# main(
		# species=species,
		# landscape=landscape,
		# geography=geography,
		# response=response,
		# directory=paste0(resultsDir, directory),
		# numTrainPres=numTrainPres,
		# numTestPres=numTestPres,
		# iterToDo=iterToDo,
		# jFoldMax=jFoldMax,
		# numBg=numBg,
		# algorithm=algorithm,
		# suffix=paste0('range inflection at ', sub(x=inflections[countInflection], pattern='[.]', replacement='pt')),
		# verbose=verbose,
		# interaction=FALSE,
		# cbi=TRUE,
		# b0=b0, b1=b1, b11=inflections[countInflection]
	# )

# } # next prevalence

# # analyze
# #########

# for (algo in c('maxent', 'gam', 'brt')) {
# # for (algo in c('maxent')) {

	# say(algo)

	# # load simulation results
	# if (exists('master')) rm(master)
	# for (pattern in c('OMNISCIENT', toupper(algo))) {

		# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern=paste0('Results - ', toupper(pattern)))
		
		# for (f in files) {
			# theseResults <- readRDS(f)
			# if (pattern==toupper(algo)) theseResults <- theseResults[ , names(master)]
			# master <- if (exists('master')) { rbind(master, theseResults) } else { theseResults }
		# }

	# }

	# xVarName <- 'prevalence'

	# thisMaster <- master[order(master[ , xVarName]), ]
	# thisAgg <- aggregate(thisMaster[ , 2:ncol(thisMaster)], by=list(thisMaster$algorithm, thisMaster[ , xVarName]), median, na.rm=T)
	# thisAgg$algorithm <- thisAgg[ , xVarName] <- NULL
	# names(thisAgg)[1:2] <- c('algorithm', xVarName)

	# # PERMUTE COR: OMNISCIENT (pres vs abs) ENM (strat bg) ENM (random bg)
	# plotVsScalar2Panel(
		# results=thisMaster,
		# resultsAgg=thisAgg,
		# xVarName=xVarName,
		# models=c('omniscient', algo, algo, algo),
		# resp=c('corBgFullVsPerm_perm', 'corBgFullVsPerm_perm', 'corAbsFullVsPerm_perm', 'corBgFullVsPermStrat_perm'),
		# respVarSpecific=c(TRUE, TRUE, TRUE, TRUE),
		# invert=TRUE,
		# off=c(-0.03, -0.01, 0.01, 0.03) / 2,
		# pch=c(21, 24, 23, 22),
		# col1=c(bg, col1, bg, col4),
		# col2=c(col1, 'black', col4, 'black'),
		# colLine=c(col1, col1, col4, col4),
		# colError=c(col1, col1, col4, col4),
		# leg=c('Omniscient', paste0(toupper(algo), ' (background)'), paste0(toupper(algo), ' (absences)'), paste0(toupper(algo), ' (stratified)')),
		# variables=c('T1', 'F1'),
		# panels=c('a) T1-M: Permute test', 'b) F1: Permute test'),
		# xlim=c(0, 1),
		# ylim=c(-0.05, 1),
		# xlog=FALSE,
		# legPos=c('bottomleft', 'topleft'),
		# legInset=0.01,
		# legCex=0.7,
		# xlab='Prevalence',
		# ylab='COR',
		# filename=paste0(resultsDir, directory, '/', '!Prevalence - PERMUTED COR - ', toupper(algo))
	# )

	# # PERMUTE CBI: OMNISCIENT (pres vs abs) ENM (strat bg) ENM (random bg)
	# plotVsScalar2Panel(
		# results=thisMaster,
		# resultsAgg=thisAgg,
		# xVarName=xVarName,
		# models=c('omniscient', 'omniscient', algo, algo),
		# resp=c('cbiFull', 'cbiPerm_perm', 'cbiFull', 'cbiPerm_perm'),
		# respVarSpecific=c(FALSE, TRUE, FALSE, TRUE),
		# invert=FALSE,
		# off=c(-0.03, -0.01, 0.01, 0.03) / 2,
		# pch=c(21, 21, 22, 22),
		# col1=c(bg, col1, bg, col4),
		# col2=c(col1, 'black', col4, 'black'),
		# colLine=c(col1, col1, col4, col4),
		# colError=c(col1, col1, col4, col4),
		# leg=c('Omniscient (full)', 'Omniscient (permuted)', paste(toupper(algo), '(full)'), paste(toupper(algo), '(permuted)')),
		# variables=c('T1', 'F1'),
		# panels=c('a) T1-M: Permute test', 'b) F1: Permute test'),
		# xlim=c(0, 1),
		# ylim=c(-1, 1),
		# xlog=FALSE,
		# legPos=c('bottomleft', 'bottomleft'),
		# legInset=0.01,
		# legCex=0.7,
		# xlab='Prevalence',
		# ylab='Continuous Boyce Index',
		# filename=paste0(resultsDir, directory, '/', '!Prevalence - PERMUTED CBI - ', toupper(algo))
	# )

# } # next algorithm

# ### plot MAXENT-specific measures
# if (exists('master')) rm(master)
# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern='Results - MAXENT')
	
# for (f in files) {
	# thisSdmResults <- readRDS(f)
	# master <- if (exists('master')) { rbind(master, thisSdmResults) } else { thisSdmResults }
# }

# master$maxentTrainGainWithOnlyT1 <- master$maxentTrainGainWithOnlyT1 * 100
# master$maxentTrainGainWithOnlyF1 <- master$maxentTrainGainWithOnlyF1 * 100

# xVarName <- 'prevalence'
# thisMaster <- master[order(master[ , xVarName]), ]
# thisAgg <- aggregate(thisMaster[ , 2:ncol(thisMaster)], by=list(thisMaster[ , xVarName]), median, na.rm=T)
# thisAgg[ , xVarName] <- NULL
# names(thisAgg)[1] <- xVarName
# thisAgg$algorithm <- 'maxent'

# # MAXENT-specific metrics
# plotVsScalar2Panel(
	# results=thisMaster,
	# resultsAgg=thisAgg,
	# xVarName=xVarName,
	# models=c('maxent', 'maxent', 'maxent'),
	# resp=c('maxentContrib', 'maxentPermImport', 'maxentTrainGainWithOnly'),
	# respVarSpecific=c(TRUE, TRUE, TRUE),
	# off=c(-0.015, 0, 0.015),
	# pch=c(21, 22, 23),
	# col1=c(col3, col4, col5),
	# col2=c('black', 'black', 'black'),
	# colLine=c(col3, col4, col5),
	# colError=c(col3, col4, col5),
	# leg=c('% Gain (full model)', 'Permutation (AUC)', 'Gain (univariate model)'),
	# variables=c('T1', 'F1'),
	# panels=c('a) T1-M', 'b) F1'),
	# xlim=c(0, 1),
	# ylim=c(0, 1),
	# legPos=c('bottomleft', 'topright'),
	# legInset=0.01,
	# legCex=0.6,
	# invert=FALSE,
	# xlab='Prevalence',
	# ylab='Importance',
	# filename=paste0(resultsDir, directory, '/', '!Prevalence - MAXENT-Specific Metrics')
# )



# ### illustrate stratified random sampling

# # landscape and species
# geography <- list(list(type='linear', min=-1, max=2), list(type='random', min=min, max=max))
# landscape <- genesis(geography, nrow=2002, circle=FALSE)
# names(landscape) <- c('T1', 'F1')
# response <- logisticShift
# species <- response(x1=subset(landscape, 1), b0=b0, b1=b1, b11=2)
# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

# # training/test presences
# presences <- randomPoints(species, 2000)
# presenceProb <- extract(species, presences)
# presences <- presences[presenceProb >= runif(length(presenceProb)), ]
# presences <- presences[1:100, ]

# # random and stratified test background sites
# testBg <- randomPoints(species, 50)
# speciesMapInteger <- round(species * 10 + 0.5 - .Machine$double.eps)
# testBgStrat <- sampleStratified(speciesMapInteger, 50, sp=TRUE)

# testPresVal <- extract(species, presences)
# testBgVal <- extract(species, testBg)
# testBgStratVal <- extract(species, testBgStrat)

# png(paste0(resultsDir, directory, '/Example of Random vs Stratified Sampling.png'), height=1000, width=2000, res=300)

	# par(mfrow=c(1, 2), bty='n', bg=bg, fg=fg, mar=rep(1.6, 4))

	# plot(species, xlab=NA, ylab=NA, main=NA, legend=F, axes=FALSE)
	# points(presences, pch=16, cex=0.7)
	# points(testBg, cex=0.7)
	# text(x=-0.05, y=1.15, xpd=NA, labels='a) Prediction raster, random sites', pos=4, cex=0.8)

	# plot(speciesMapInteger, xlab=NA, ylab=NA, main=NA, legend=F, axes=FALSE)
	# points(testBgStrat, cex=0.5, pch=3, col='black')
	# text(x=-0.05, y=1.15, xpd=NA, labels='b) Stratified raster, stratified sites', pos=4, cex=0.8)
		
# dev.off()

# say('#####################')
# say('### [sample size] ###')
# say('#####################')

# directory <- '[sample size]'
# dir.create(paste0(resultsDir, directory), recursive=T, showWarnings=F)
# scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
# write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

# response <- logistic
# geography <- list(list(type='linear', min=min, max=max), list(type='random', min=min, max=max))

# cat ('RESPONSE', paste(deparse(response), collapse=' '), 'GEOGRAPHY', ifelse(class(geography)=='list', paste(unlist(geography), collapse=' '), paste(geography, collapse=' ')), '\n'); flush.console()

# png(paste0(resultsDir, directory, '/response.png'), height=600, width=600, res=200)
# par(pty='s', cex.axis=0.6, cex.lab=0.8, tck=-0.025, mgp=0.5 * c(3, 1, 0), mar=0.5 * c(5, 4, 4, 2) + 0.1)
# plot(x=seq(min, max, by=0.01), y=response(x1=seq(min, max, by=0.01), b0=b0, b1=b1), type='l', xlab='T1', ylab='Pr(occ)', ylim=c(0, 1), lwd=2)
# dev.off()

# # generate landscape
# landscape <- genesis(geography, circle=FALSE)
# names(landscape) <- c('T1', 'F1')

# # generate species
# species <- response(x1=subset(landscape, 1), b0=b0, b1=b1)
# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=rep(T, nlayers(landscape)), name='simple situation', bg=bg, fg=fg)

# # for (n in c(10, 20, 30, 40, 50, 60, 70, 80, 100, 200, 400, 800)) {
# for (n in c(600, 700, 900, 1000)) {

	# say('Sample size: n = ', n, ' ======================================')

	# # model!
	# main(
		# species=species,
		# response=response,
		# landscape=landscape,
		# geography=geography,
		# directory=paste0(resultsDir, directory),
		# numTrainPres=n,
		# numTestPres=numTestPres,
		# numBg=numBg,
		# iterToDo=iterToDo,
		# maxPermIter=maxPermIter,
		# jFoldMax=jFoldMax,
		# algorithm=algorithm,
		# suffix=paste0('number of training presences = ', n),
		# verbose=verbose,
		# plotResponse=TRUE,
		# cbi=TRUE,
		# interaction=TRUE,
		# b0=b0, b1=b1, b2=0, b11=0, b12=0
	# )

# }

# # training call for algorithms that often yield errors (BRTs)
# for (n in c(50, 60, 70, 90)) {

	# say('Sample size: n = ', n, ' ======================================')

	# worked <- 'DUMMY'

	# thisIter <- min(iterToDo)
	# while (thisIter <= max(iterToDo)) {
		
		# # model!
		# worked <- try(
			# main(
				# species=species,
				# response=response,
				# landscape=landscape,
				# geography=geography,
				# directory=paste0(resultsDir, directory),
				# numTrainPres=n,
				# numTestPres=numTestPres,
				# numBg=numBg,
				# iterToDo=thisIter,
				# maxPermIter=maxPermIter,
				# jFoldMax=jFoldMax,
				# algorithm=algorithm,
				# suffix=paste0('number of training presences = ', n),
				# verbose=verbose,
				# plotResponse=TRUE,
				# cbi=TRUE,
				# interaction=TRUE,
				# b0=b0, b1=b1, b2=0, b11=0, b12=0
			# ),
			# silent=TRUE
		# )
		
		# if (class(worked) != 'try-error') {
			# thisIter <- thisIter + 1
			# worked <- 'DUMMY'
		# }
		
	# }
	
# }

# ## process results
# ##################

	# ### aggregate data and save
	# resultsFiles <- list.files(paste0(resultsDir, directory), pattern='Results - ', full.names=TRUE)
	# results <- merge(data.frame(), readRDS(resultsFiles[1]), all=TRUE)
	# for (thisFiles in resultsFiles) {
		
		# thisResults <- readRDS(thisFiles)
		# results <- merge(results, thisResults, all=TRUE)

	# }

	# resultsAgg <- aggregate(results, by=list(results$algorithm, results$numTrainPres), FUN='mean', na.rm=TRUE)
	# resultsAgg$algorithm <- resultsAgg$numTrainPres <- NULL
	# names(resultsAgg)[1:2] <- c('algorithm', 'numTrainPres')

	# saveRDS(results, paste0(resultsDir, directory, '/Results - ALL.rds'))
	# saveRDS(resultsAgg, paste0(resultsDir, directory, '/Results - AVERAGED.rds'))

	# ### load aggregated data
	# results <- readRDS(paste0(resultsDir, directory, '/Results - ALL.rds'))
	# resultsAgg <- readRDS(paste0(resultsDir, directory, '/Results - AVERAGED.rds'))

	# # AUC with absences
	# plotVsScalar2Panel(
		# results=results,
		# resultsAgg=resultsAgg,
		# xVarName='numTrainPres',
		# models=c('omniscient', 'brt', 'gam', 'maxent'),
		# resp=c('aucAbsFullModel', 'aucAbsFullModel', 'aucAbsFullModel', 'aucAbsFullModel'),
		# respVarSpecific=c(FALSE, FALSE, FALSE, FALSE),
		# off=c(-0.03, -0.015, 0.015, 0.03),
		# pch=c(21, 22, 23, 24),
		# col1=c(col1, col2, col3, col4),
		# col2=c('black', 'black', 'black', 'black'),
		# colLine=c(col1, col2, col3, col4),
		# colError=c(col1, col2, col3, col4),
		# leg=c('Omniscient', 'BRT', 'GAM', 'MAXENT'),
		# variables=c('T1', 'F1'),
		# panels=c('a) T1', 'b) F1'),
		# xlim=log10(c(8, 850)),
		# ylim=c(0, 1),
		# xlog=TRUE,
		# legPos=c('bottomright', 'bottomright'),
		# legInset=0.01,
		# legCex=0.6,
		# invert=FALSE,
		# xlab='Training Sample Size',
		# ylab='AUC',
		# cexAxis=0.5,
		# las=2,
		# padj=0.5,
		# tck=-0.023,
		# filename=paste0(resultsDir, directory, '/', '!Sample Size - Multivariate - AUC vs Presence & Absence')
	# )

	# # COR with presences/background
	# plotVsScalar2Panel(
		# results=results,
		# resultsAgg=resultsAgg,
		# xVarName='numTrainPres',
		# models=c('omniscient', 'brt', 'gam', 'maxent'),
		# resp=c('corBgFullVsPerm_perm', 'corBgFullVsPerm_perm', 'corBgFullVsPerm_perm', 'corBgFullVsPerm_perm'),
		# respVarSpecific=c(TRUE, TRUE, TRUE, TRUE),
		# off=c(-0.03, -0.015, 0.015, 0.03),
		# pch=c(21, 22, 23, 24),
		# col1=c(col1, col2, col3, col4),
		# col2=c('black', 'black', 'black', 'black'),
		# colLine=c(col1, col2, col3, col4),
		# colError=c(col1, col2, col3, col4),
		# leg=c('Omniscient', 'BRT', 'GAM', 'MAXENT'),
		# variables=c('T1', 'F1'),
		# panels=c('a) T1', 'b) F1'),
		# xlim=log10(c(8, 850)),
		# ylim=c(0, 1),
		# xlog=TRUE,
		# legPos=c('topright', 'bottomright'),
		# legInset=0.01,
		# legCex=0.6,
		# invert=FALSE,
		# xlab='Training Sample Size',
		# ylab='COR',
		# cexAxis=0.5,
		# las=2,
		# padj=0.5,
		# tck=-0.023,
		# filename=paste0(resultsDir, directory, '/', '!Sample Size - Multivariate - COR vs Pres & BG')
	# )

	# # COR with stratified background
	# plotVsScalar2Panel(
		# results=results,
		# resultsAgg=resultsAgg,
		# xVarName='numTrainPres',
		# models=c('omniscient', 'brt', 'gam', 'maxent'),
		# resp=c('corBgFullVsPermStrat_perm', 'corBgFullVsPermStrat_perm', 'corBgFullVsPermStrat_perm', 'corBgFullVsPermStrat_perm'),
		# respVarSpecific=c(TRUE, TRUE, TRUE, TRUE),
		# off=c(-0.03, -0.015, 0.015, 0.03),
		# pch=c(21, 22, 23, 24),
		# col1=c(col1, col2, col3, col4),
		# col2=c('black', 'black', 'black', 'black'),
		# colLine=c(col1, col2, col3, col4),
		# colError=c(col1, col2, col3, col4),
		# leg=c('Omniscient', 'BRT', 'GAM', 'MAXENT'),
		# variables=c('T1', 'F1'),
		# panels=c('a) T1', 'b) F1'),
		# xlim=log10(c(8, 850)),
		# ylim=c(0, 1),
		# xlog=TRUE,
		# legPos=c('topright', 'bottomright'),
		# legInset=0.01,
		# legCex=0.6,
		# invert=FALSE,
		# xlab='Training Sample Size',
		# ylab='COR',
		# cexAxis=0.5,
		# las=2,
		# padj=0.5,
		# tck=-0.023,
		# filename=paste0(resultsDir, directory, '/', '!Sample Size - Multivariate - COR vs Stratified BG')
	# )

	# # CBI
	# plotVsScalar2Panel(
		# results=results,
		# resultsAgg=resultsAgg,
		# xVarName='numTrainPres',
		# models=rep(c('omniscient', 'brt', 'gam', 'maxent'), each=2),
		# resp=rep(c('cbiFull', 'cbiPerm_perm'), 4),
		# respVarSpecific=rep(c(FALSE, TRUE), 4),
		# off=rep(c(-0.03, -0.015, 0.015, 0.03), each=2) / 1.5 + c(-0.01, 0) / 1.5,
		# pch=rep(c(21, 22, 23, 24), each=2),
		# col1=c('white', col1, 'white', col2, 'white', col3, 'white', col4),
		# col2=c(col1, 'black', col2, 'black', col3, 'black', col4, 'black'),
		# colLine=rep(c(col1, col2, col3, col4), each=2),
		# colError=rep(c(col1, col2, col3, col4), each=2),
		# ltyError=rep(c('dotted', 'solid'), 4),
		# leg=c('Omniscient (full)', 'Omniscient (permuted)', 'BRT (full)', 'BRT (permuted)', 'GAM (full)', 'GAM (permuted)', 'MAXENT (full)', 'MAXENT (permuted)'),
		# variables=c('T1', 'F1'),
		# panels=c('a) T1', 'b) F1'),
		# xlim=log10(c(8, 850)),
		# ylim=c(-1, 1),
		# xlog=TRUE,
		# legPos=c('bottomright', 'bottomright'),
		# legInset=0.01,
		# legCex=0.5,
		# legNcol=2,
		# invert=FALSE,
		# xlab='Training Sample Size',
		# ylab='CBI',
		# cexAxis=0.5,
		# las=2,
		# padj=0.5,
		# tck=-0.023,
		# filename=paste0(resultsDir, directory, '/', '!Sample Size - Multivariate - CBI')
	# )

# say('################')
# say('### [extent] ###')
# say('################')

# directory <- '[extent]'
# # dir.create(paste0(resultsDir, directory), recursive=T, showWarnings=F)
# # scenario <- 'RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1)'
# # say(scenario)
# # write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)


# theseExtent <- c(125, 251, 501, 1001, 2001, 4001)
# theseMax <- c(0.125, 0.25, 0.5, 1, 2, 4)
# theseMin <- -1 * theseMax

# # n <- 6
# # theseExtent <- theseExtent[n]
# # theseMax <- theseMax[n]
# # theseMin <- theseMin[n]
	
# response <- logistic
# geography <- list(list(type='linear', min=NA, max=NA), list(type='random', min=min, max=max))

# cat ('RESPONSE', paste(deparse(response), collapse=' '), 'GEOGRAPHY', ifelse(class(geography)=='list', paste(unlist(geography), collapse=' '), paste(geography, collapse=' ')), '\n'); flush.console()

# png(paste0(resultsDir, directory, '/response.png'), height=400, width=300 * length(theseMin), res=300)

	# par(mfrow=c(1, length(theseMin)), mai=c(1.02, 0.82, 0.82, 0.42) / 4)
	
	# for (i in seq_along(theseMin)) {
	
		# x <- seq(min(theseMin[i]), max(theseMax[i]), by=0.01)
		
		# plot(x=x, y=response(x1=x, b0=b0, b1=b1), type='l', xlab='T1', ylab='Pr(occupancy)', ylim=c(0, 1), cex.lab=0.8, cex.axis=0.5, mgp=c(3, 0.5, 0) / 4, lab=c(3, 3, 7), tcl=-0.2)
	
		# presIndex <- which(response(x1=x, b0=b0, b1=b1) >= runif(length(x)))
	
		# # mean pres/abs location 
		# points(x=mean(x[presIndex]), y=response(x1=mean(x[presIndex]), b0=b0, b1=b1), pch=16, cex=1.4)
		# points(x=mean(x[-presIndex]), y=response(x1=mean(x[-presIndex]), b0=b0, b1=b1), pch=1, cex=1.4)
		# points(x=0, y=0.5, pch=4, cex=1.4)

	# }
		
# dev.off()

# for (countExtent in seq_along(theseExtent)) {

	# geography[[1]]$min <- theseMin[countExtent]
	# geography[[1]]$max <- theseMax[countExtent]

	# say('min = ', theseMin[countExtent], ' | max = ', theseMax[countExtent])

	# # generate landscape
	# landscape <- genesis(geography, nrow=theseExtent[countExtent], circle=FALSE)
	# names(landscape) <- c('T1', 'F1')
	
	# # generate species
	# species <- response(x1=subset(landscape, 1), b0=b0, b1=b1)
	# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

	# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=rep(T, nlayers(landscape)), name=paste0('landscape extent = ', theseExtent[countExtent], ' rows'), bg=bg, fg=fg)

	# # model!
	# main(
		# species=species,
		# landscape=landscape,
		# geography=geography,
		# response=response,
		# directory=paste0(resultsDir, directory),
		# numTrainPres=numTrainPres,
		# numTestPres=numTestPres,
		# iterToDo=iterToDo,
		# jFoldMax=jFoldMax,
		# numBg=numBg,
		# algorithm=algorithm,
		# suffix=paste0('landscape extent = ', theseExtent[countExtent], ' rows'),
		# verbose=verbose,
		# interaction=FALSE,
		# cbi=TRUE,
		# b0=b0, b1=b1
	# )

# } # next landscape extent

## analyze
##########

# # calculate mean difference in Pr(presence) for presences and absences as function of extent
# for (countExtent in seq_along(theseExtent)) {

	# say('Extent: ', theseExtent[countExtent], pre=2)

	# geography[[1]]$min <- theseMin[countExtent]
	# geography[[1]]$max <- theseMax[countExtent]

	# say('min = ', theseMin[countExtent], ' | max = ', theseMax[countExtent])

	# # generate landscape
	# landscape <- genesis(geography, nrow=theseExtent[countExtent], circle=FALSE)
	# names(landscape) <- c('T1', 'F1')
	
	# # generate species
	# species <- response(x1=subset(landscape, 1), b0=b0, b1=b1)

	# # calculate mean Pr(pres) at presence and absence sites
	# sites <- randomPoints(species, 12000)
	# probPres <- extract(species, sites)
	# presAbs <- probPres >= runif(12000)
	# pres <- sample(which(presAbs == 1), 5000)
	# abs <- sample(which(presAbs == 0), 5000)

	# probPres <- probPres[c(pres, abs)]
	# presAbs <- presAbs[c(pres, abs)]
	# sites <- sites[c(pres, abs), ]
	
	# env <- as.data.frame(extract(landscape, sites))
	
	# say('Mean prevalance: ', round(cellStats(species, 'mean'), 2))
	# say('Mean probability of presence (+-sd): ', round(mean(probPres[presAbs==1]), 2), ' +- ', round(sd(probPres[presAbs==1]), 2))
	# say('Mean probability of absence (+-sd): ', round(mean(probPres[presAbs==0]), 2), round(sd(probPres[presAbs==0]), 2))
	# say('Mean difference in probability of presence and absence: ', round(mean(probPres[presAbs==1] - probPres[presAbs==0]), 2), ' +- ', round(sd(probPres[presAbs==1] - probPres[presAbs==0]), 2))
	
	# say('Mean value of T1-M at presence (+-sd): ', round(mean(env$T1[presAbs==1]), 2), ' +- ', round(sd(env$T1[presAbs==1]), 2))
	# say('Mean value of T1-M at absence (+-sd): ', round(mean(env$T1[presAbs==0]), 2), round(sd(env$T1[presAbs==0]), 2))
	# say('Mean difference in T1-M at presence and absence: ', round(mean(env$T1[presAbs==1] - env$T1[presAbs==0]), 2), ' +- ', round(sd(env$T1[presAbs==1] - env$T1[presAbs==0]), 2))
	
# }

# ### analyze
# ###########

	# ### load simulation results
	# if (exists('master')) rm(master)
	# for (pattern in c('OMNISCIENT', 'MAXENT', 'BRT', 'GAM')) {

		# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern=paste0('Results - ', toupper(pattern)))
		
		# for (f in files) {
			# theseResults <- readRDS(f)
			# if (pattern!='OMNISCIENT') theseResults <- theseResults[ , names(master)]
			# master <- if (exists('master')) { rbind(master, theseResults) } else { theseResults }
		# }

	# }

	# master$rangeT1 <- log(master$maxValT1 - master$minValT1, 2)
	# xVarName <- 'rangeT1'

	# thisMaster <- master[order(master[ , xVarName]), ]
	# thisAgg <- aggregate(thisMaster[ , 2:ncol(thisMaster)], by=list(thisMaster$algorithm, thisMaster[ , xVarName]), median, na.rm=T)
	# thisAgg$algorithm <- thisAgg[ , xVarName] <- NULL
	# names(thisAgg)[1:2] <- c('algorithm', xVarName)

	# ### plot
		
	# # for (thisAlgo in c('maxent', 'brt', 'gam')) {
	# for (thisAlgo in c('maxent')) {

		# say('Plotting results for ', thisAlgo)

		# # PERMUTE COR: OMNISCIENT (pres vs abs) ENM (strat bg) ENM (random bg)
		# plotVsScalar2Panel(
			# results=thisMaster,
			# resultsAgg=thisAgg,
			# xVarName=xVarName,
			# models=c('omniscient', thisAlgo, thisAlgo, thisAlgo),
			# resp=c('corAbsFullVsPerm_perm', 'corBgFullVsPerm_perm', 'corAbsFullVsPerm_perm', 'corBgFullVsPermStrat_perm'),
			# respVarSpecific=c(TRUE, TRUE, TRUE, TRUE),
			# invert=TRUE,
			# off=c(-0.09, -0.03, 0.03, 0.09),
			# pch=c(21, 22, 23, 24),
			# col1=c('white', gray2, gray3, gray4),
			# col2=c('black', 'black', 'black', 'black'),
			# colLine=c('black', gray2, gray3, gray4),
			# colError=c('black', gray2, gray3, gray4),
			# leg=c('Omniscient', paste0(toupper(thisAlgo), ' (background)'), paste0(toupper(thisAlgo), ' (absences)'), paste0(toupper(thisAlgo), ' (stratified)')),
			# variables=c('T1', 'F1'),
			# panels=c('a) T1-M: Permute test', 'b) F1: Permute test'),
			# xlim=c(-2.2, 3.2),
			# ylim=c(-0.05, 1),
			# xlog=FALSE,
			# legPos=c('bottomright', 'topright'),
			# legInset=0.01,
			# legCex=0.7,
			# xlab=expression('log'[2]*'(Range of T1)'),
			# ylab='COR',
			# filename=paste0(resultsDir, directory, '/', '!Extent - PERMUTE COR - OMNISCIENT Pres vs Abs - ', toupper(thisAlgo), ' Random, Absences, and Stratified BG')
		# )

		# # PERMUTE CBI: OMNISCIENT (pres vs random bg) ENM (strat bg) ENM (random bg)
		# plotVsScalar2Panel(
			# results=thisMaster,
			# resultsAgg=thisAgg,
			# xVarName=xVarName,
			# models=c('omniscient', 'omniscient', thisAlgo, thisAlgo),
			# resp=c('cbiFull', 'cbiPerm_perm', 'cbiFull', 'cbiPerm_perm'),
			# respVarSpecific=c(FALSE, TRUE, FALSE, TRUE),
			# invert=FALSE,
			# off=c(-0.09, -0.03, 0.03, 0.09),
			# pch=c(21, 21, 22, 22),
			# col1=c('white', col1, 'white', col4),
			# col2=c('black', 'black', 'black', 'black'),
			# colLine=c('black', col1, 'black', col4),
			# colError=c('black', col1, 'black', col4),
			# leg=c('Omniscient (full model)', 'Omniscient (permuted)', paste0(toupper(thisAlgo), ' (full model)'), paste0(toupper(thisAlgo), ' (permuted)')),
			# variables=c('T1', 'F1'),
			# panels=c('a) T1-M: Permute test', 'b) F1: Permute test'),
			# xlim=c(-2.2, 3.2),
			# ylim=c(-1, 1),
			# xlog=FALSE,
			# legPos=c('bottomright', 'bottomright'),
			# legInset=0.01,
			# legCex=0.6,
			# xlab=expression('log'[2]*'(Range of T1)'),
			# ylab='CBI',
			# filename=paste0(resultsDir, directory, '/', '!Extent - PERMUTE CBI - OMNISCIENT Pres vs Random BG - ', toupper(thisAlgo), ' Random BG')
		# )

		# # PERMUTE AUC: OMNISCIENT (pres vs random bg) ENM (strat bg) ENM (random bg)
		# plotVsScalar2Panel(
			# results=thisMaster,
			# resultsAgg=thisAgg,
			# xVarName=xVarName,
			# models=c('omniscient', 'omniscient', thisAlgo, thisAlgo),
			# resp=c('aucBgFullModel', 'aucBgPerm_perm', 'aucBgFullModel', 'aucBgPerm_perm'),
			# respVarSpecific=c(FALSE, TRUE, FALSE, TRUE),
			# invert=FALSE,
			# off=c(-0.09, -0.03, 0.03, 0.09),
			# pch=c(21, 21, 22, 22),
			# col1=c('white', gray3, 'white', gray4),
			# col2=c('black', 'black', 'black', 'black'),
			# colLine=c('black', gray3, 'black', gray4),
			# colError=c('black', gray3, 'black', gray4),
			# leg=c('Omniscient (full model)', 'Omniscient (permuted)', paste0(toupper(thisAlgo), ' (full model)'), paste0(toupper(thisAlgo), ' (permuted)')),
			# variables=c('T1', 'F1'),
			# panels=c('a) T1-M: Permute test', 'b) F1: Permute test'),
			# xlim=c(-2.2, 3.2),
			# ylim=c(0, 1),
			# xlog=FALSE,
			# legPos=c('bottomright', 'bottomright'),
			# legInset=0.01,
			# legCex=0.6,
			# xlab=expression('log'[2]*'(Range of T1)'),
			# ylab='AUC',
			# filename=paste0(resultsDir, directory, '/', '!Extent - PERMUTE AUC - OMNISCIENT Pres vs Random BG - ', toupper(thisAlgo), ' Random BG')
		# )

	# }

# ### plot MAXENT-specific measures
# #################################

# if (exists('master')) rm(master)
# files <- list.files(paste0(resultsDir, directory), full.names=T, pattern='Results - MAXENT')
	
# for (f in files) {
	# thisSdmResults <- readRDS(f)
	# master <- if (exists('master')) { rbind(master, thisSdmResults) } else { thisSdmResults }
# }

# master$maxentTrainGainWithOnlyT1 <- master$maxentTrainGainWithOnlyT1 * 100
# master$maxentTrainGainWithOnlyF1 <- master$maxentTrainGainWithOnlyF1 * 100

# master$rangeT1 <- log(master$maxValT1 - master$minValT1, 2)
# xVarName <- 'rangeT1'

# thisMaster <- master[order(master[ , xVarName]), ]
# thisAgg <- aggregate(thisMaster[ , 2:ncol(thisMaster)], by=list(thisMaster[ , xVarName]), median, na.rm=T)
# thisAgg[ , xVarName] <- NULL
# names(thisAgg)[1] <- xVarName
# thisAgg$algorithm <- 'maxent'

# # MAXENT-specific metrics
# plotVsScalar2Panel(
	# results=thisMaster,
	# resultsAgg=thisAgg,
	# xVarName=xVarName,
	# models=c('maxent', 'maxent', 'maxent'),
	# resp=c('maxentContrib', 'maxentPermImport', 'maxentTrainGainWithOnly'),
	# respVarSpecific=c(TRUE, TRUE, TRUE),
	# invert=FALSE,
	# off=c(-0.06, 0, 0.06),
	# pch=c(21, 22, 23),
	# col1=c(col3, col4, col5),
	# col2=c('black', 'black', 'black'),
	# colLine=c(col3, col4, col5),
	# colError=c(col3, col4, col5),
	# leg=c('% Gain (full model)', 'Permutation (AUC)', 'Gain (univariate model)'),
	# variables=c('T1', 'F1'),
	# panels=c('c) T1-M: Maxent-specific measures', 'd) F1: Maxent-specific measures'),
	# xlim=c(-2.2, 3.2),
	# ylim=c(0, 1),
	# legPos=c('right', 'topright'),
	# legInset=0.01,
	# legCex=0.6,
	# xlab=expression('log'[2]*'(Range of T1)'),
	# ylab='Metric',
	# filename=paste0(resultsDir, directory, '/', '!Extent - MAXENT-Specific Metrics')
# )

say('#########################')
say('### [weak vs strong]  ###')
say('#########################')

say('Varying strenth of variable 1 vs 2 on landscape with 2 linear variables')
say('Covariates include landscape rotation and rho')

directory <- '[weak vs strong]'
dirCreate(paste0(resultsDir, directory))
scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))'
say(scenario)
write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

response <- gaussian

geography <- list(
	list(type='linear', min=min, max=max, pregen=TRUE),
	list(type='linear', min=min, max=max, pregen=TRUE, rot=NA)
)

# create progress frame
if (exists('progress')) rm(progress)
rot <- c(22.5, 45, 67.5, 90, 112.5, 135, 157.5)
rho <- c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)
sigma2Values <- c(0.1, 0.2, 0.3, 0.4, 0.5)

for (rot in rot) {
	for (thisRho in rho) {
		for (thisSigma1 in sigma2Values) {
			for (thisSigma2 in seq(min(sigma2Values), thisSigma1, by=0.1)) {
				
				line <- data.frame(
					rot=rot,
					rho=thisRho,
					sigma1=thisSigma1,
					sigma2=thisSigma2
				)
				line$string <- paste(names(line), line, collapse=' ', sep='=')
				progress <- if (exists('progress')) { rbind(progress, line) } else { line }
				
			}
		}
	}
}

# # # progress <- progress[nrow(progress):1, ]

dirCreate(paste0(resultsDir, directory, '/starts - ', algorithm))
dirCreate(paste0(resultsDir, directory, '/stops - ', algorithm))

# sets in progress or completed
started <- list.files(paste0(resultsDir, directory, '/starts - ', algorithm))

while (length(started) < nrow(progress)) {

	# get index of set needed doing
	if (length(started)==0) {
		doing <- 1
	} else {
		doing <- progress$string[-match(started, progress$string)][1]
		doing <- which(progress$string==doing)
	}
	write.csv(progress$string[doing], paste0(resultsDir, directory, '/starts - ', algorithm, '/', progress$string[doing]), row.names=F)

	rot <- progress$rot[doing]
	thisRho <- progress$rho[doing]
	thisSigma1 <- progress$sigma1[doing]
	thisSigma2 <- progress$sigma2[doing]

	say('rot = ', rot, ' rho = ', thisRho, ' sigma1 = ', thisSigma1, ' sigma2 = ', thisSigma2)

	# create landscape
	geography[[2]]$rot <- rot
	landscape <- genesis(geography, circle=TRUE)
	names(landscape) <- c('T1', 'T2')
	
	# generate species
	species <- response(mu1=mu1, mu2=mu2, x1=subset(landscape, 1), x2=subset(landscape, 2), sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho)
	say('Mean Pr(occ) = ', cellStats(species, 'mean'))

	# model!
	evalFrame <- main(
		species=species,
		landscape=landscape,
		geography=geography,
		directory=paste0(resultsDir, directory),
		numTrainPres=numTrainPres,
		numTestPres=numTestPres,
		iterToDo=iterToDo,
		jFoldMax=jFoldMax,
		numBg=numBg,
		algorithm=algorithm,
		suffix=paste0('rot(T2)=', rot, ' rho=', thisRho, ' sigma1=', thisSigma1, ' sigma2=', thisSigma2),
		verbose=verbose,
		response=response,
		plotResponse=FALSE,
		cbi=TRUE,
		interaction=TRUE,
		mu1=mu1, mu2=mu2, sigma1=thisSigma1, sigma2=thisSigma2, rho=thisRho
	)

	# indicate this set complete and save
	write.csv(progress$string[doing], paste0(resultsDir, directory, '/stops - ', algorithm, '/', progress$string[doing]), row.names=F)	
	started <- list.files(paste0(resultsDir, directory, '/starts - ', algorithm))
	
} # next set

# say('   ########################################')
# say('   ### [weak vs strong] compile results ###')
# say('   ########################################')

	# # table with correlation between T1 and T2 as function of rotation of T2 versus T1
	# landRotVsCor <- read.csv(paste0(workDir, '/Correlations between Standard Environmental Variables.csv'), as.is=TRUE, row.names=1)
	
	# # create empty data frame
	# if (exists('results')) rm(results)

	# allFiles <- list.files(paste0(resultsDir, directory), pattern='.rds', full.names=TRUE)
	# if (any(grepl(allFiles, pattern='!Compiled Results'))) allFiles <- allFiles[-which(grepl(allFiles, pattern='!Compiled Results'))]

	# # process each file
	# for (thisFile in allFiles) {

		# say('Loading files... % done: ', 100 * which(allFiles %in% thisFile) / length(allFiles))
	
		# thisResults <- readRDS(thisFile)
		
		# # assign rho
		# thisResults$rho <- as.numeric(substr(thisFile, regexpr(text=thisFile, pattern='rho=', fixed=TRUE) + 4, stop=regexpr(text=thisFile, pattern='sigma1', fixed=TRUE) - 2))

		# # assign landscape correlation based on rotation of T2 vs T1
		# rotT2 <- as.numeric(substr(thisFile, regexpr(text=thisFile, pattern='rot(T2)=', fixed=TRUE) + 8, stop=regexpr(text=thisFile, pattern='rho=', fixed=TRUE) - 2))
		# thisResults$rotT2 <- rotT2
		# thisResults$landscapeCor <- landRotVsCor['linearFromNeg1To1Rotation0', paste0('linearFromNeg1To1Rotation', sub(rotT2, pattern='[.]', replacement='pt'))]
		
		# if (exists('results')) {

			# missingFromThisResults <- if (!all(names(results) %in% names(thisResults))) { which(!(names(results) %in% names(thisResults))) } else { NA }
			# missingFromResults <- if (!all(names(thisResults) %in% names(results))) { which(!(names(thisResults) %in% names(results))) } else { NA }
			
			# if (!is.na(sum(missingFromThisResults))) {
				# for (newCol in missingFromThisResults) {
					# thisResults$NEW <- NA
					# names(thisResults)[ncol(thisResults)] <- names(results)[newCol]
				# }
			# }
		
			# if (!is.na(sum(missingFromResults))) {
				# for (newCol in missingFromResults) {
					# results$NEW <- NA
					# names(results)[ncol(results)] <- names(thisResults)[newCol]
				# }
			# }
				
			# results <- results[ , order(names(results))]
			# thisResults <- thisResults[ , order(names(results))]
			# results <- rbind(results, thisResults)

		# } else {
			# results <- thisResults
		# }
		
	# }
	
	# results$sigmaRatio <- results$sigma2 / results$sigma1

	# results$cbi <- results$interaction <- NULL
	# results$algorithm <- as.character(results$algorithm)
	
	# saveRDS(results, paste0(resultsDir, directory, '/!Compiled Results.rds'))

# say('   ###################################################')
# say('   ### [weak vs strong] generic plotting functions ###')
# say('   ###################################################')
	
	# # function to plot annuli for each pair of sigma values
	# plotSigmaVsSigma <- function(center1, lower1, upper1, center2, lower2, upper2, col1, col2, type='annulus', ...) {
	
		# # center1, lower1, upper1, center2, lower2, upper2	Matrices with central values, upper/lower tail values for each pair of sigma values
		# # col1, col2	Colors for each algorithm's or variable's models
		# # type			'annulus' ==> overlapping annuli subplots
		# #				'bar'	  ==> side-by-side bar plots
		# # ...			arguments to pass to annulus() like "force0"
	
		# plot(x=0, y=0, col='white', xlim=c(0.05, 0.55), ylim=c(0.05, 0.55), xlab=expression(paste('Niche width in T1 (', sigma[1], ')', sep='')), ylab=expression(paste('Niche width in T2 (', sigma[2], ')', sep='')), xpd=NA)
	
		# zeros <- matrix(rep(0, length(center1)), ncol=ncol(center1))
		# adjLower1 <- pmax(zeros, lower1)
		# adjLower2 <- pmax(zeros, lower2)
	
		# adjCenter1 <- pmax(zeros, center1)
		# adjCenter2 <- pmax(zeros, center2)
	
		# adjUpper1 <- pmax(zeros, upper1)
		# adjUpper2 <- pmax(zeros, upper2)
	
		# for (row in 1:5) {
		
			# for (col in 1:5) {

				# if (!is.na(adjCenter1[row, col])) {
				
					# ## annuli plots
					# if (type=='annulus') {
					
						# # scale line
						# lines(x=c(col / 10, col / 10 + 0.05), y=rep(row / 10, 2), lwd=0.5)
						# lines(x=rep(col / 10, 2), y=c(row / 10, row / 10 + 0.005), lwd=0.5)
						# lines(x=rep(col / 10 + 0.025, 2), y=c(row / 10, row / 10 + 0.005), lwd=0.5)
						# lines(x=rep(col / 10 + 0.05, 2), y=c(row / 10, row / 10 + 0.005), lwd=0.5)

						# # plot first annulus
						# annulus(x=col / 10, y=row / 10, inner=adjLower1[row, col] / 20, outer=adjUpper1[row, col] / 20, col=alpha(col1, 0.5), ...)

						# # plot second annulus
						# annulus(x=col / 10, y=row / 10, inner=adjLower2[row, col] / 20, outer=adjUpper2[row, col] / 20, col=alpha(col2, 0.5), ...)

						# # center circles... add "!" if inner radius is < 0
						# if (lower1[row, col] < 0) {
							# text(col / 10, row / 10, labels='!', pos=3)
						# }
						# if (lower2[row, col] < 0) {
							# text(col / 10, row / 10, labels='!', pos=3)
						# }
						# symbols(x=col / 10, y=row / 10, circles=adjCenter1[row, col] / 20, fg=col1, add=TRUE, inches=F, lwd=0.5)
						# symbols(x=col / 10, y=row / 10, circles=adjCenter2[row, col] / 20, fg=col2, add=TRUE, inches=F, lwd=0.5)

						# # add asterisk if annulus 1 is completely outside annulus 2's CI
						# if (lower1[row, col] > upper2[row, col] | upper1[row, col] < lower2[row, col]) {
							# text(col / 10, row / 10, labels='*', pos=1, cex=1.6)
						# }
						
					# ## bar plots
					# } else {

						# size <- 0.07
						# xOffset <- col / 10 - 0.05
						# yOffset <- row / 10 - 0.05
						
						# axisOffset <- -0.035
						
						# # subplot axes
						# lines(x=c(0, 1) * size + xOffset, y=c(axisOffset, axisOffset) * size + yOffset, lwd=0.5)
						# lines(x=c(axisOffset, axisOffset) * size + xOffset, y=c(0, 1) * size + yOffset, lwd=0.5)
						
						# # y-axis tick marks
						# lines(x=c(axisOffset, 2 * axisOffset) * size + xOffset, y=c(0, 0) * size + yOffset, lwd=0.5)
						# lines(x=c(axisOffset, 2 * axisOffset) * size + xOffset, y=c(0.5, 0.5) * size + yOffset, lwd=0.5)
						# lines(x=c(axisOffset, 2 * axisOffset) * size + xOffset, y=c(1, 1) * size + yOffset, lwd=0.5)
						
						# # add first bar
						# polygon(x=c(0.2, 0.6, 0.6, 0.2) * size + xOffset, y=c(lower1[row, col], lower1[row, col], upper1[row, col], upper1[row, col]) * size + yOffset, col=alpha(col1, 0.5), border=alpha(col1, 0.8), lwd=0.5)
						
						# # add first bar median
						# lines(x=c(0.2, 0.6) * size + xOffset, y=c(center1[row, col], center1[row, col]) * size + yOffset, col=alpha(col1, 0.8), lend='butt')
						
						# # add second bar
						# polygon(x=c(0.4, 0.8, 0.8, 0.4) * size + xOffset, y=c(lower2[row, col], lower2[row, col], upper2[row, col], upper2[row, col]) * size + yOffset, col=alpha(col2, 0.5), border=alpha(col2, 0.8), lwd=0.5)
						
						# # add second bar median
						# lines(x=c(0.4, 0.8) * size + xOffset, y=c(center2[row, col], center2[row, col]) * size + yOffset, col=alpha(col2, 0.8), lend='butt')
						
						# # add asterisk if no overlap between bars
						# if (lower1[row, col] > upper2[row, col] | lower2[row, col] > upper1[row, col]) text(x=0.5 * size + xOffset, y=size + yOffset, labels='*', xpd=NA, col='black', cex=1.4)
						
						
					# }
				
				# }
							
			# } # next column
			
		# } # next row
		
	# } # END function
	
	# ## function to return matrices with central value, lower tail, and upper tail across simulations
	# collateImport <- function(results, algo='brt', variable='T1', contrast='corBg', strat='', ia='', iaBeforeAfter='', rho=0, landscapeCor, invert=TRUE, fill=FALSE, ...) {
	
		# # algo		algorithm name
		# # results	data frame with results
		# # variable	'T1', 'F1', etc.
		# # contrast	metric name plus modifiers... for calling correct column in results
		# # strat		'Strat' or '' (stratified BG)
		# # ia		'IA' for interaction importance or '' for single-variable importance
		# # iaBeforeAfter NULL
		# # rho		rho (niche covariance term)
		# # landscapeCor correlation on landscape
		# # invert	logical, if TRUE the returns 1 - test statistic values (i.e., TRUE for correlation test, FALSE for AUC or CBI)
		# # fill		logical, if TRUE then fill in blank values (ie where there was no combination of sigma1 and sigma2 simulated) with opposite values from sigma2 and sigma1

		# # collate matrices of variable importance
		# generic <- matrix(rep(NA, 25), nrow=5)
		# rownames(generic) <- paste0('sigma1_', sub(seq(0.1, 0.5, by=0.1), pattern='[.]', replacement='pt'))
		# colnames(generic) <- paste0('sigma2_', sub(seq(0.1, 0.5, by=0.1), pattern='[.]', replacement='pt'))
		# center <- upper <- lower <- generic # medians, end of lower/upper tail

		# for (s1 in 1:5) {

			# for (s2 in 1:s1) {
				
				# values <- results[
					# results$algorithm==algo & round(results$sigma1, 1)==round(s1 / 10, 1) & round(results$sigma2, 1)==round(s2 / 10, 1) & round(1000 * results$rho)==round(1000 * rho) & round(1000 * results$landscapeCor)==round(1000 * landscapeCor),
					# paste0(contrast, 'Perm', strat, ia=ia, '_perm', ifelse(iaBeforeAfter=='', '', paste0(iaBeforeAfter, '_')), variable)
				# ]
	

				# if (invert) values <- 1 - values
				
				# center[s1, s2] <- median(values, na.rm=T)
				# upper[s1, s2] <- quantile(values, 0.975, na.rm=T)
				# lower[s1, s2] <- quantile(values, 0.025, na.rm=T)
			
			# }

		# }

		# center <- t(center)
		# upper <- t(upper)
		# lower <- t(lower)

		# # fill in combinations of sigma1 and sigma2 that were not simulated by using combinations of sigma2 and sigma1 that were simulated
		# if (fill) {
		
			# for (i in 1:nrow(center)) {
				# for (j in 1:i) {
					# center[i, j]  <- center[j, i]
				# }
			# }
		
			# for (i in 1:nrow(lower)) {
				# for (j in 1:i) {
					# lower[i, j]  <- lower[j, i]
				# }
			# }
		
			# for (i in 1:nrow(upper)) {
				# for (j in 1:i) {
					# upper[i, j]  <- upper[j, i]
				# }
			# }
		
		# }
		
		# out <- list(center=center, lower=lower, upper=upper)

		# return(out)
	
	# } ## END function

# say('   ##############################################################################################################################')
# say('   ### [weak vs strong] analyzing relative strength of 2 TRUE variables with NO niche covariance and NO landscape correlation ###')
# say('   ##############################################################################################################################')

	# dirCreate(resultsDir, directory, '/r=0 rho=0')

	# results <- readRDS(paste0(resultsDir, directory, '/!Compiled Results.rds'))
	# eps <- 1e-6
	# # results <- results[results$rho < eps & results$rho > -eps & results$rotT2 == 90, ]
	# results <- results[results$rho == 0 & results$rotT2 == 90, ]
	
	# ### PLOT
	
	# ## DESIGN:
	# # 3 panels
		# # 1st: T1 (ENM vs OMNI)
		# # 2nd: T2 (ENM vs OMNI)
		# # 3rd: T1 vs T2 (ENM)
	
	# # plot parameters
	# colOmni <- '#a50026' # reddish
	# colEnm <- '#006837' # greenish
	
	# colT1 <- '#7f3b08'
	# colT2 <- '#2d004b'

	# plotType <- 'annulus'
	# # plotType <- 'bar'
	
	# # legend text size
	# cex <- 0.9

	# for (algo in c('maxent', 'gam', 'brt')) {
	# # for (algo in c('maxent')) {
		
		# # for (metric in c('cor absences', 'cor bg', 'cor stratified bg', 'auc absences', 'auc bg')) {
		# for (metric in c('cor absences')) {

			# say(algo, ' ', metric)
	
			# # test metric (abbrevaited for column name string) for OMNISCIENT and ENM, stratified BG or not, string to append to file name, main title
			# if (metric=='cbi') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corAbsFullVs'
				# strat <- ''
				# legendScaleLabel <- 'COR'
				# fileDescrip <- paste0(toupper(algo), ' - CBI - ', toupper(algo))
				# main <- paste0(toupper(algo), ' - CBI - ', toupper(algo))
				# invert <- FALSE # use 1 - test value for interpretation
			# } else if (metric=='cor absences') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corAbsFullVs'
				# strat <- ''
				# legendScaleLabel <- 'COR'
				# fileDescrip <- paste0(toupper(algo), ' - COR - OMNISCIENT Pres vs Abs ', toupper(algo), ' Pres vs Absences')
				# main <- paste0(toupper(algo), ' - COR Test - OMNISCIENT Pres vs Abs ', toupper(algo), ' Pres vs Absences')
				# invert <- TRUE # use 1 - test value for interpretation
			# } else if (metric=='cor bg') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corBgFullVs'
				# strat <- ''
				# legendScaleLabel <- 'COR'
				# fileDescrip <- paste0(toupper(algo), ' - COR - OMNISCIENT Pres vs Abs ', toupper(algo), ' Pres vs BG')
				# main <- paste0(toupper(algo), ' - COR Test - OMNISCIENT Pres vs Abs ', toupper(algo), ' Pres vs BG')
				# invert <- TRUE # use 1 - test value for interpretation
			# } else if (metric=='cor stratified bg') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corBgFullVs'
				# strat <- 'Strat'
				# legendScaleLabel <- 'COR'
				# fileDescrip <- paste0(toupper(algo), ' - COR - OMNISCIENT Pres vs Abs ', toupper(algo), ' Stratified BG')
				# main <- paste0(toupper(algo), ' - COR Test - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Stratified BG')
				# invert <- TRUE # use 1 - test value for interpretation
			# } else if (metric=='auc absences') {
				# contrastOmni <- 'aucAbs'
				# contrastEnm <- 'aucAbs'
				# strat <- ''
				# legendScaleLabel <- 'AUC'
				# fileDescrip <- paste0(toupper(algo), ' - AUC - OMNISCIENT Pres vs Abs ', toupper(algo), ' Pres vs Absences')
				# main <- paste0(toupper(algo), ' - AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Pres vs Absences')
				# invert <- FALSE # use 1 - test value for interpretation
			# } else if (metric=='auc bg') {
				# contrastOmni <- 'aucAbs'
				# contrastEnm <- 'aucBg'
				# strat <- ''
				# legendScaleLabel <- 'AUC'
				# fileDescrip <- paste0(toupper(algo), ' - AUC - OMNISCIENT Pres vs Abs ', toupper(algo), ' Pres vs BG')
				# main <- paste0(toupper(algo), ' - AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Pres vs BG')
				# invert <- FALSE # use 1 - test value for interpretation
			# }
			
			# png(paste0(resultsDir, '/', directory, '/r=0 rho=0/!WEAK vs STRONG - ', toupper(plotType), ' Plot - ', fileDescrip ,'.png'), width=3600, height=1200, res=600)

				# par(mfrow=c(1, 3), cex=0.4, mar=0.575 * c(5, 4, 4, 2) + 0.2, mgp=c(2.2, 1, 0), pty='s', cex.lab=1.2)

				# ## Tx vs OMNISCIENT
				# for (variable in c('T1', 'T2')) {

					# # collate variable importance matrices
					# importOmni <- collateImport(algo='omniscient', results=results, variable=variable, contrast=contrastOmni, strat='', rho=0, landscapeCor=min(abs(results$landscapeCor)), invert=invert)
					# importEnm <- collateImport(algo=algo, results=results, variable=variable, contrast=contrastEnm, strat=strat, rho=0, landscapeCor=min(abs(results$landscapeCor)), invert=invert)
					
					# # plot
					# plotSigmaVsSigma(center1=importEnm$center, lower1=importEnm$lower, upper1=importEnm$upper, center2=importOmni$center, lower2=importOmni$lower, upper2=importOmni$upper, col1=colEnm, col2=colOmni, cex=0.6, type=plotType)
					
					# ## legend
					
					# if (plotType=='annulus') {
					
						# #  (annuli)
					
						# # center of annuli
						# x <- 0.14
						# y <- 0.46
					
						# # nudge text by this amount from x coordinate
						# nudgeText <- -0.01
					
						# # omniscient
						# inner1 <- 0.40 / 10
						# outer1 <- 0.62 / 10
						# center1 <- mean(c(inner1, outer1))
						
						# # ENM
						# inner2 <- 0.32 / 10
						# outer2 <- 0.93 / 10
						# center2 <- 0.69 / 10
										
						# # scale line
						# lines(x=c(x, x + 0.1), y=rep(y, 2), lwd=0.5)
						# lines(x=rep(x, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0
						# lines(x=rep(x + 0.05, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0.5
						# lines(x=rep(x + 0.1, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=1

						# # plot ENM CI
						# annulus(x=x, y=y, inner=inner2, outer=outer2, col=alpha(colEnm, 0.5))
						
						# # plot OMNI model CI
						# annulus(x=x, y=y, inner=inner1, outer=outer1, col=alpha(colOmni, 0.5))

						# # median circles
						# symbols(x=x, y=y, circles=center2, fg=colEnm, add=TRUE, inches=F, lwd=0.5)
						# symbols(x=x, y=y, circles=center1, fg=colOmni, add=TRUE, inches=F, lwd=0.5)

						# # labels: data 1
						# arrows(x0=x - inner1, x1=x - inner1, y0=0.35, y1=y, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x - inner1 + nudgeText, y=0.36, labels='Lower tail (OMNI)', pos=4, xpd=NA, col=colOmni, cex=cex)
						
						# arrows(x0=x - center1, x1=x - center1, y0=0.31, y1=y, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x - center1 + nudgeText, y=0.32, labels='Median (OMNI)', pos=4, xpd=NA, col=colOmni, cex=cex)
						
						# arrows(x0=x - outer1, x1=x - outer1, y0=0.27, y1=y, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x - outer1 + nudgeText, y=0.28, labels='Upper tail (OMNI)', pos=4, xpd=NA, col=colOmni, cex=cex)
					
						# # labels: data 1
						# arrows(x0=x + 0.09, x1=x, y0=y + outer2, y1=y + outer2, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x + 0.09 + nudgeText, y=y + outer2, labels=paste0('Upper tail (', toupper(algo), ')'), pos=4, xpd=NA, col=colEnm, cex=0.8)
						
						# arrows(x0=x + 0.09, x1=x, y0=y + center2, y1=y + center2, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x + 0.09 + nudgeText, y=y + center2, labels=paste0('Median (', toupper(algo), ')'), pos=4, xpd=NA, col=colEnm, cex=0.8)
						
						# arrows(x0=x + 0.09, x1=x, y0=y + inner2, y1=y + inner2, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x + 0.09 + nudgeText, y=y + inner2, labels=paste0('Lower tail (', toupper(algo), ')'), pos=4, xpd=NA, col=colEnm, cex=0.8)
					
						# # scale labels
						# text(x=x + c(0, 0.05, 0.1), y=rep(y, 3), labels=c(0, 0.5, 1), pos=1)
						
					# # bar plot
					# } else {
					
						# xScale <- 0.14
						# yScale <- 0.14
						# xNudge <- 0.13
						# yNudge <- 0.4
					
						# # bars
						# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge - 0.01, y=yScale * c(0.40, 0.40, 0.92, 0.92) + yNudge, col=alpha(colEnm, 0.5), border=colEnm)
						# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge + 0.01, y=yScale * c(0.62, 0.62, 0.82, 0.82) + yNudge, col=alpha(colOmni, 0.5), border=colOmni)
						
						# # median lines
						# lines(x=xScale * c(0, 0.25) + xNudge - 0.01, y=yScale * c(0.56, 0.56) + yNudge, col=colEnm)
						# lines(x=xScale * c(0, 0.25) + xNudge + 0.01, y=yScale * c(0.71, 0.71) + yNudge, col=colOmni)
						
						# # scale bar
						# lines(x=c(0.08, 0.08) + 0.03, y=c(0.4, 0.55))
						# lines(x=c(0.08, 0.075) + 0.03, y=c(0.4, 0.4))
						# lines(x=c(0.08, 0.075) + 0.03, y=rep(mean(c(0.4, 0.55)), 2))
						# lines(x=c(0.08, 0.075) + 0.03, y=c(0.55, 0.55))
						
						# # scale labels
						# text(x=0.08 + 0.03, y=0.4, labels='0', pos=2)
						# text(x=0.08 + 0.03, y=mean(c(0.4, 0.55)), labels='0.5', pos=2)
						# text(x=0.08 + 0.03, y=0.55, labels='1', pos=2)
					
						# # scale title
						# text(0.05, 0.475, labels=legendScaleLabel, srt=90)
					
						# # bar labels
						# text(0.125 + 0.03, y=0.43, labels=toupper(algo), srt=90, pos=2, col=colEnm)
						# text(0.145 + 0.03, y=0.43, labels='OMNISCIENT', srt=90, pos=2, col=colOmni)
						
					# }
					
					# # panel label
					# text(x=-0.05, y=0.59, labels=paste0(ifelse(variable=='T1', 'a) ', 'b) '), variable, '-M: ', toupper(algo), ' vs. OMNISCIENT'), xpd=NA, pos=4, cex=1.1)
						
				# } # next variable (T1/T2)

				# ## T1 vs T2
					
					# # collate variable importance matrices
					# importT1 <- collateImport(algo=algo, results=results, variable='T1', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=min(abs(results$landscapeCor)), invert=invert)
					# importT2 <- collateImport(algo=algo, results=results, variable='T2', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=min(abs(results$landscapeCor)), invert=invert)
					
					# # plot
					# plotSigmaVsSigma(center1=importT1$center, lower1=importT1$lower, upper1=importT1$upper, center2=importT2$center, lower2=importT2$lower, upper2=importT2$upper, col1=colT1, col2=colT2, cex=0.6, type=plotType)
					
					# ## legend
					
					# # annulus legend
					# if (plotType=='annulus') {
					
						# # center of annuli
						# x <- 0.14
						# y <- 0.46
					
						# # nudge text by this amount from x coordinate
						# nudgeText <- -0.01
					
						# # T1
						# inner2 <- 0.52 / 10
						# outer2 <- 0.92 / 10
						# center1 <- mean(c(inner1, outer1))
						
						# # T2
						# inner1 <- 0.25 / 10
						# outer1 <- 0.45 / 10
						# center2 <- mean(c(inner2, outer2))
										
						# # scale line
						# lines(x=c(x, x + 0.1), y=rep(y, 2), lwd=0.5)
						# lines(x=rep(x, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0
						# lines(x=rep(x + 0.05, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0.5
						# lines(x=rep(x + 0.1, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=1

						# # plot T1 CI
						# annulus(x=x, y=y, inner=inner2, outer=outer2, col=alpha(colT1, 0.5))
						
						# # plot T2 CI
						# annulus(x=x, y=y, inner=inner1, outer=outer1, col=alpha(colT2, 0.5))

						# # median circles
						# symbols(x=x, y=y, circles=center2, fg=colT1, add=TRUE, inches=F, lwd=0.5)
						# symbols(x=x, y=y, circles=center1, fg=colT2, add=TRUE, inches=F, lwd=0.5, lty='dashed')

						# # labels: data 2
						# arrows(x0=x - inner1, x1=x - inner1, y0=0.35, y1=y, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x - inner1 + nudgeText, y=0.36, labels='Lower tail (T2)', pos=4, xpd=NA, col=colT2, cex=cex)
						
						# arrows(x0=x - center1, x1=x - center1, y0=0.31, y1=y, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x - center1 + nudgeText, y=0.32, labels='Median (T2)', pos=4, xpd=NA, col=colT2, cex=cex)
						
						# arrows(x0=x - outer1, x1=x - outer1, y0=0.27, y1=y, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x - outer1 + nudgeText, y=0.28, labels='Upper tail (T2)', pos=4, xpd=NA, col=colT2, cex=cex)
					
						# # labels: data 1
						# arrows(x0=x + 0.09, x1=x, y0=y + outer2, y1=y + outer2, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x + 0.09 + nudgeText, y=y + outer2, labels='Upper tail (T1)', pos=4, xpd=NA, col=colT1, cex=cex)
						
						# arrows(x0=x + 0.09, x1=x, y0=y + center2, y1=y + center2, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x + 0.09 + nudgeText, y=y + center2, labels='Median (T1)', pos=4, xpd=NA, col=colT1, cex=cex)
						
						# arrows(x0=x + 0.09, x1=x, y0=y + inner2, y1=y + inner2, xpd=NA, lwd=0.4, length=0.05, angle=15, col='black')
						# text(x=x + 0.09 + nudgeText, y=y + inner2, labels='Lower tail (T1)', pos=4, xpd=NA, col=colT1, cex=cex)
					
						# # scale labels
						# text(x=x + c(0, 0.05, 0.1), y=rep(y, 3), labels=c(0, 0.5, 1), pos=1)
					
					# # bar legend
					# } else {
					
						# xScale <- 0.14
						# yScale <- 0.14
						# xNudge <- 0.13
						# yNudge <- 0.4
					
						# # bars
						# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge - 0.01, y=yScale * c(0.22, 0.22, 0.52, 0.52) + yNudge, col=alpha(colT1, 0.5), border=colT1)
						# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge + 0.01, y=yScale * c(0.47, 0.47, 0.98, 0.98) + yNudge, col=alpha(colT2, 0.5), border=colT2)
						
						# # median lines
						# lines(x=xScale * c(0, 0.25) + xNudge - 0.01, y=yScale * c(0.37, 0.37) + yNudge, col=colT1)
						# lines(x=xScale * c(0, 0.25) + xNudge + 0.01, y=yScale * c(0.77, 0.77) + yNudge, col=colT2)
						
						# # scale bar
						# lines(x=c(0.08, 0.08) + 0.03, y=c(0.4, 0.55))
						# lines(x=c(0.08, 0.075) + 0.03, y=c(0.4, 0.4))
						# lines(x=c(0.08, 0.075) + 0.03, y=rep(mean(c(0.4, 0.55)), 2))
						# lines(x=c(0.08, 0.075) + 0.03, y=c(0.55, 0.55))
						
						# # scale labels
						# text(x=0.08 + 0.03, y=0.4, labels='0', pos=2)
						# text(x=0.08 + 0.03, y=mean(c(0.4, 0.55)), labels='0.5', pos=2)
						# text(x=0.08 + 0.03, y=0.55, labels='1', pos=2)
					
						# # scale title
						# text(0.05, 0.475, labels=legendScaleLabel, srt=90)
					
						# # bar labels
						# text(0.145 + 0.03, y=0.40, labels='T2', srt=90, pos=2, col=colT2)
						# text(0.125 + 0.03, y=0.40, labels='T1', srt=90, pos=2, col=colT1)						
						
					# }
					
					# # panel label
					# text(x=-0.05, y=0.59, labels=paste0('c) T1 vs. T2 (both ', toupper(algo), ')'), xpd=NA, pos=4, cex=1.1)
						
				# ## main title
				# title(main=main, outer=TRUE, line=-0.5, cex.main=1)
				
			# dev.off()
					
		# } # next metric type
		
	# } # next algorithm

# say('   ############################################################################################################################################')
# say('   ### [weak vs strong] analyzing relative strength of 2 TRUE variables WITH ALL COMBINATIONS OF niche covariance AND landscape correlation ###')
# say('   ############################################################################################################################################')

	# dirCreate(resultsDir, directory, '/r=any rho=any')

	# landRotVsCor <- read.csv(paste0(workDir, '/Correlations between Standard Environmental Variables.csv'), as.is=TRUE, row.names=1)
	
	# results <- readRDS(paste0(resultsDir, directory, '/!Compiled Results.rds'))
	
	# ### PLOT
	
	# ## DESIGN:
	# # 9x9 panels
		# # landscape correlation (r) changes across columns, middle column is r=0
		# # niche covariace (rho) changes across rows, middle row is rho=0
	
	# # plot parameters
	# colOmni <- '#67001f' # reddish
	# colEnm <- '#053061' # blueish
	
	# colT1 <- '#7f3b08'
	# colT2 <- '#2d004b'

	# plotType <- 'annulus'
	# # plotType <- 'bar'
	
	# # legend text size
	# cex <- 0.4

	# for (algo in c('maxent', 'gam', 'brt')) {
	# # for (algo in c('maxent')) {
		
		# for (metric in c('cor stratified bg')) {
		# # for (metric in c('stratified bg')) {

			# say(metric)
	
			# # test metric (abbrevaited for column name string) for OMNISCIENT and ENM, stratified BG or not, string to append to file name, main title
			# if (metric=='cor absences') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corAbsFullVs'
				# strat <- ''
				# fileDescrip <- paste0('COR - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# main <- paste0('COR Test - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# invert <- TRUE # use 1 - test value for interpretation
			# } else if (metric=='cor bg') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corBgFullVs'
				# strat <- ''
				# fileDescrip <- paste0('COR - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# main <- paste0('COR Test - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# invert <- TRUE # use 1 - test value for interpretation
			# } else if (metric=='cor stratified bg') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corBgFullVs'
				# strat <- 'Strat'
				# fileDescrip <- paste0('COR - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Stratified BG')
				# main <- paste0('COR Test - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Stratified BG')
				# invert <- TRUE # use 1 - test value for interpretation
			# } else if (metric=='auc absences') {
				# contrastOmni <- 'aucAbs'
				# contrastEnm <- 'aucAbs'
				# strat <- ''
				# fileDescrip <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# main <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# invert <- FALSE # use 1 - test value for interpretation
			# } else if (metric=='auc bg') {
				# contrastOmni <- 'aucAbs'
				# contrastEnm <- 'aucBg'
				# strat <- ''
				# fileDescrip <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# main <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# invert <- FALSE # use 1 - test value for interpretation
			# }
			
			# png(paste0(resultsDir, '/', directory, '/r=any rho=any/!WEAK vs STRONG - ', toupper(plotType), ' Plot - ', fileDescrip ,'.png'), width=700 * 9 * 3, height=700 * 7, res=600)

				# # layout:
				# # topmost row is for title of entire figure
				# # second row is title for each of three main panels (ENM vs OMNI for T1, ENM vs ONI for F1, ENM T1 vs F1)
				# # third row (one per column) is title for value of rho
				# # left column (one per row) in each panel is title for values of landscape correlation
				# # individual subpanels are for a particular value of rho and landscape correlation (row is landscape correlation--7 values, col is rho--9 values)
			
				# # individual subpanels
				# layout <- matrix(rep(1:(9 * 7), each=3), ncol=9 * 3, byrow=TRUE)
				# layout <- layout[rep(1:nrow(layout), each=3), ]

				# # column labels
				# colLab <- matrix(rep((1:9) + max(layout), each=3), nrow=1)
				# layout <- rbind(colLab, layout)
				
				# # row labels
				# rowLab <- matrix(max(layout) + 1)
				# rowLab <- rbind(rowLab, matrix(rep((1:7) + max(layout) + 1, each=3), ncol=1))
				# layout <- cbind(rowLab, layout)
				
				# # panel labels
				# panelLabel <- matrix(rep(1, ncol(layout)) + max(layout), nrow=1)
				# layout <- rbind(panelLabel, layout)
				
				# # combine 3 panels
				# layout <- cbind(layout, layout + max(layout), layout + 2 * max(layout))
				
				# # master title
				# layout <- rbind(matrix(rep(1 + max(layout), ncol(layout)), nrow=1), layout)
				
				# layout(layout)
			
				# par(cex=0.4, mar=0.4 * c(5, 4, 4, 2) + 0.2, mgp=c(2, 1, 0), pty='s', cex.lab=1.2)

				# ## Tx vs OMNISCIENT
				# for (variable in c('T1', 'T2')) {

					# # landscape correlation (rows)
					# for (landscapeCor in rev(sort(unique(results$landscapeCor)))) {
				
						# # rho (columns)
						# for (rho in seq(-0.8, 0.8, by=0.2)) {
					
							# say(algo, ' ', metric, ' ', variable, ' ', landscapeCor, ' ', rho)

							# theseResults <- results[results$rho==rho & results$landscapeCor==landscapeCor, ]
				
							# # collate variable importance matrices
							# importOmni <- collateImport(algo='omniscient', results=results, variable=variable, contrast=contrastOmni, strat='', rho=rho, landscapeCor=landscapeCor, invert=invert)
							# importEnm <- collateImport(algo=algo, results=results, variable=variable, contrast=contrastEnm, strat=strat, rho=rho, landscapeCor=landscapeCor, invert=invert)
							
							# # plot
							# plotSigmaVsSigma(center1=importEnm$center, lower1=importEnm$lower, upper1=importEnm$upper, center2=importOmni$center, lower2=importOmni$lower, upper2=importOmni$upper, col1=colEnm, col2=colOmni, cex=0.6, type=plotType, force0=TRUE)
							
							# # title(paste0(variable, ' rho=', rho, ' r=', round(landscapeCor, 2)))
										
							# ## legend
							# if (landscapeCor==rev(sort(unique(results$landscapeCor)))[1] & rho==-0.8) {
								
								# if (plotType=='annulus') {
								
									# #  (annuli)
								
									# # center of annuli
									# x <- 0.14
									# y <- 0.46
								
									# # nudge text by this amount from x coordinate
									# nudgeText <- -0.01
								
									# # omniscient
									# inner1 <- 0.40 / 10
									# outer1 <- 0.62 / 10
									# center1 <- mean(c(inner1, outer1))
									
									# # ENM
									# inner2 <- 0.32 / 10
									# outer2 <- 0.93 / 10
									# center2 <- 0.69 / 10
													
									# # scale line
									# lines(x=c(x, x + 0.1), y=rep(y, 2), lwd=0.5)
									# lines(x=rep(x, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0
									# lines(x=rep(x + 0.05, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0.5
									# lines(x=rep(x + 0.1, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=1

									# # plot ENM CI
									# annulus(x=x, y=y, inner=inner2, outer=outer2, col=alpha(colEnm, 0.5))
									
									# # plot OMNI model CI
									# annulus(x=x, y=y, inner=inner1, outer=outer1, col=alpha(colOmni, 0.5))

									# # median circles
									# symbols(x=x, y=y, circles=center2, fg=colEnm, add=TRUE, inches=F, lwd=0.5)
									# symbols(x=x, y=y, circles=center1, fg=colOmni, add=TRUE, inches=F, lwd=0.5)

									# # labels: data 1
									# arrows(x0=x - inner1, x1=x - inner1, y0=0.35, y1=y, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
									# text(x=x - inner1 + nudgeText, y=0.36, labels='Lower tail (OMNI)', pos=4, xpd=NA, col=colOmni, cex=cex)
									
									# arrows(x0=x - center1, x1=x - center1, y0=0.31, y1=y, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
									# text(x=x - center1 + nudgeText, y=0.32, labels='Median (OMNI)', pos=4, xpd=NA, col=colOmni, cex=cex)
									
									# arrows(x0=x - outer1, x1=x - outer1, y0=0.27, y1=y, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
									# text(x=x - outer1 + nudgeText, y=0.28, labels='Upper tail (OMNI)', pos=4, xpd=NA, col=colOmni, cex=cex)
								
									# # labels: data 1
									# arrows(x0=x + 0.09, x1=x, y0=y + outer2, y1=y + outer2, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
									# text(x=x + 0.09 + nudgeText, y=y + outer2, labels=paste0('Upper tail (', toupper(algo), ')'), pos=4, xpd=NA, col=colEnm, cex=cex)
									
									# arrows(x0=x + 0.09, x1=x, y0=y + center2, y1=y + center2, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
									# text(x=x + 0.09 + nudgeText, y=y + center2, labels=paste0('Median (', toupper(algo), ')'), pos=4, xpd=NA, col=colEnm, cex=cex)
									
									# arrows(x0=x + 0.09, x1=x, y0=y + inner2, y1=y + inner2, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
									# text(x=x + 0.09 + nudgeText, y=y + inner2, labels=paste0('Lower tail (', toupper(algo), ')'), pos=4, xpd=NA, col=colEnm, cex=cex)
								
									# # scale labels
									# text(x=x + c(0, 0.05, 0.1), y=rep(y, 3), labels=c(0, 0.5, 1), pos=1, cex=cex)
									
								# # bar plot
								# } else {
								
									# xScale <- 0.14
									# yScale <- 0.14
									# xNudge <- 0.13
									# yNudge <- 0.4
								
									# # bars
									# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge - 0.01, y=yScale * c(0.40, 0.40, 0.92, 0.92) + yNudge, col=alpha(colEnm, 0.5), border=colEnm)
									# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge + 0.01, y=yScale * c(0.62, 0.62, 0.82, 0.82) + yNudge, col=alpha(colOmni, 0.5), border=colOmni)
									
									# # median lines
									# lines(x=xScale * c(0, 0.25) + xNudge - 0.01, y=yScale * c(0.56, 0.56) + yNudge, col=colEnm)
									# lines(x=xScale * c(0, 0.25) + xNudge + 0.01, y=yScale * c(0.71, 0.71) + yNudge, col=colOmni)
									
									# # scale bar
									# lines(x=c(0.08, 0.08) + 0.03, y=c(0.4, 0.55))
									# lines(x=c(0.08, 0.075) + 0.03, y=c(0.4, 0.4))
									# lines(x=c(0.08, 0.075) + 0.03, y=rep(mean(c(0.4, 0.55)), 2))
									# lines(x=c(0.08, 0.075) + 0.03, y=c(0.55, 0.55))
									
									# # scale labels
									# text(x=0.08 + 0.03, y=0.4, labels='0', pos=2)
									# text(x=0.08 + 0.03, y=mean(c(0.4, 0.55)), labels='0.5', pos=2)
									# text(x=0.08 + 0.03, y=0.55, labels='1', pos=2)
								
									# # scale title
									# text(0.05, 0.475, labels=legendScaleLabel, srt=90)
								
									# # bar labels
									# text(0.125 + 0.03, y=0.43, labels=toupper(algo), srt=90, pos=2, col=colEnm)
									# text(0.145 + 0.03, y=0.43, labels='OMNISCIENT', srt=90, pos=2, col=colOmni)

								# } # annulus/bar legend
								
							# } # legend
									
						# }
						
					# }
					
					# # column labels (rho)
					# for (rho in seq(-0.8, 0.8, by=0.2)) {
					
						# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
						# text(0, 0, labels=paste0('rho = ', rho), cex=2.4, xpd=NA)
					
					# }

					# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
					
					# # row labels (landscape correlation)
					# for (landscapeCor in rev(sort(unique(results$landscapeCor)))) {
					
						# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
						# text(0, 0, labels=paste0('r = ', round(landscapeCor, 2)), cex=2.4, xpd=NA, srt=90)
					
					# }
					
					# # large panel label
					# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
					# text(0, 0, labels=paste0(variable, '-M: ', toupper(algo), ' vs OMNISCIENT'), cex=3, xpd=NA)
				
				# }

				# ## T1 vs T2
				# for (landscapeCor in rev(sort(unique(results$landscapeCor)))) {
			
					# for (rho in seq(-0.8, 0.8, by=0.2)) {
				
						# say(algo, ' ', metric, ' T1 vs T2 ', rho, ' ', landscapeCor)
			
						# # collate variable importance matrices
						# importT1 <- collateImport(algo=algo, results=results, variable='T1', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)
						# importT2 <- collateImport(algo=algo, results=results, variable='T2', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)
						
						# # plot
						# plotSigmaVsSigma(center1=importT1$center, lower1=importT1$lower, upper1=importT1$upper, center2=importT2$center, lower2=importT2$lower, upper2=importT2$upper, col1=colT1, col2=colT2, cex=0.6, type=plotType, force0=TRUE)
						
						# # title(paste0('T1 vs T2 rho=', rho, ' r=', round(landscapeCor, 2)))
						
						
						# ## legend
						# if (landscapeCor==rev(sort(unique(results$landscapeCor)))[1] & rho==-0.8) {

							# # annulus legend
							# if (plotType=='annulus') {
							
								# # center of annuli
								# x <- 0.14
								# y <- 0.46
							
								# # nudge text by this amount from x coordinate
								# nudgeText <- -0.01
							
								# # T1
								# inner1 <- 0.25 / 10
								# outer1 <- 0.45 / 10
								# center1 <- mean(c(inner1, outer1))
								
								# # T2
								# inner2 <- 0.52 / 10
								# outer2 <- 0.92 / 10
								# center2 <- mean(c(inner2, outer2))
												
								# # scale line
								# lines(x=c(x, x + 0.1), y=rep(y, 2), lwd=0.5)
								# lines(x=rep(x, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0
								# lines(x=rep(x + 0.05, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=0.5
								# lines(x=rep(x + 0.1, 2), y=c(y, y - 0.005), lwd=0.5) # tick at x=1

								# # plot T1 CI
								# annulus(x=x, y=y, inner=inner2, outer=outer2, col=alpha(colT1, 0.5))
								
								# # plot T2 CI
								# annulus(x=x, y=y, inner=inner1, outer=outer1, col=alpha(colT2, 0.5))

								# # median circles
								# symbols(x=x, y=y, circles=center2, fg=colT1, add=TRUE, inches=F, lwd=0.5)
								# symbols(x=x, y=y, circles=center1, fg=colT2, add=TRUE, inches=F, lwd=0.5, lty='dashed')

								# # labels: data 2
								# arrows(x0=x - inner1, x1=x - inner1, y0=0.35, y1=y, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
								# text(x=x - inner1 + nudgeText, y=0.36, labels='Lower tail (T2)', pos=4, xpd=NA, col=colT2, cex=cex)
								
								# arrows(x0=x - center1, x1=x - center1, y0=0.31, y1=y, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
								# text(x=x - center1 + nudgeText, y=0.32, labels='Median (T2)', pos=4, xpd=NA, col=colT2, cex=cex)
								
								# arrows(x0=x - outer1, x1=x - outer1, y0=0.27, y1=y, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
								# text(x=x - outer1 + nudgeText, y=0.28, labels='Upper tail (T2)', pos=4, xpd=NA, col=colT2, cex=cex)
							
								# # labels: data 1
								# arrows(x0=x + 0.09, x1=x, y0=y + outer2, y1=y + outer2, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
								# text(x=x + 0.09 + nudgeText, y=y + outer2, labels='Upper tail (T1)', pos=4, xpd=NA, col=colT1, cex=cex)
								
								# arrows(x0=x + 0.09, x1=x, y0=y + center2, y1=y + center2, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
								# text(x=x + 0.09 + nudgeText, y=y + center2, labels='Median (T1)', pos=4, xpd=NA, col=colT1, cex=cex)
								
								# arrows(x0=x + 0.09, x1=x, y0=y + inner2, y1=y + inner2, xpd=NA, lwd=0.4, length=0.02, angle=15, col='black')
								# text(x=x + 0.09 + nudgeText, y=y + inner2, labels='Lower tail (T1)', pos=4, xpd=NA, col=colT1, cex=cex)
							
								# # scale labels
								# text(x=x + c(0, 0.05, 0.1), y=rep(y, 3), labels=c(0, 0.5, 1), pos=1, cex=cex)
							
							# # bar legend
							# } else {
							
								# xScale <- 0.14
								# yScale <- 0.14
								# xNudge <- 0.13
								# yNudge <- 0.4
							
								# # bars
								# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge - 0.01, y=yScale * c(0.22, 0.22, 0.52, 0.52) + yNudge, col=alpha(colT1, 0.5), border=colT1)
								# polygon(x=xScale * c(0, 0.25, 0.25, 0) + xNudge + 0.01, y=yScale * c(0.47, 0.47, 0.98, 0.98) + yNudge, col=alpha(colT2, 0.5), border=colT2)
								
								# # median lines
								# lines(x=xScale * c(0, 0.25) + xNudge - 0.01, y=yScale * c(0.37, 0.37) + yNudge, col=colT1)
								# lines(x=xScale * c(0, 0.25) + xNudge + 0.01, y=yScale * c(0.77, 0.77) + yNudge, col=colT2)
								
								# # scale bar
								# lines(x=c(0.08, 0.08) + 0.03, y=c(0.4, 0.55))
								# lines(x=c(0.08, 0.075) + 0.03, y=c(0.4, 0.4))
								# lines(x=c(0.08, 0.075) + 0.03, y=rep(mean(c(0.4, 0.55)), 2))
								# lines(x=c(0.08, 0.075) + 0.03, y=c(0.55, 0.55))
								
								# # scale labels
								# text(x=0.08 + 0.03, y=0.4, labels='0', pos=2)
								# text(x=0.08 + 0.03, y=mean(c(0.4, 0.55)), labels='0.5', pos=2)
								# text(x=0.08 + 0.03, y=0.55, labels='1', pos=2)
							
								# # scale title
								# text(0.05, 0.475, labels=legendScaleLabel, srt=90)
							
								# # bar labels
								# text(0.145 + 0.03, y=0.40, labels='T2', srt=90, pos=2, col=colT2)
								# text(0.125 + 0.03, y=0.40, labels='T1', srt=90, pos=2, col=colT1)						
								
							# }
							
						# } # legend
						
					# }
					
				# }
				
				# # column labels (rho)
				# for (rho in seq(-0.8, 0.8, by=0.2)) {
				
					# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
					# text(0, 0, labels=paste0('rho = ', rho), cex=2.4, xpd=NA)
				
				# }

				# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
				
				# # row labels (landscape correlation)
				# for (landscapeCor in rev(sort(unique(results$landscapeCor)))) {
				
					# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
					# text(0, 0, labels=paste0('r = ', round(landscapeCor, 2)), cex=2.4, xpd=NA, srt=90)
				
				# }
				
				# # large panel label
				# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
				# text(0, 0, labels='T1-M vs T2-M', cex=3, xpd=NA)

				# # master title
				# plot(0, 0, col='white', fg='white', col.lab=alpha('white', 0), col.axis=alpha('white', 0))
				# text(0, 0, labels=fileDescrip, cex=4, xpd=NA)
				# # title(main=fileDescrip, outer=TRUE, line=-2, cex=3)
	
			# dev.off()
	
		# } # next metric type
		
	# } # next algorithm

# say('   ####################################################################################################################################')
# say('   ### [weak vs strong] analyzing relative strength of 2 TRUE variables NO niche covariance AND ALL VALUES OF landscape correlation ###')
# say('   ####################################################################################################################################')

	# dirCreate(resultsDir, directory, '/r=any rho=0')

	# landRotVsCor <- read.csv(paste0(workDir, '/Correlations between Standard Environmental Variables.csv'), as.is=TRUE, row.names=1)
	
	# results <- readRDS(paste0(resultsDir, directory, '/!Compiled Results.rds'))
	
	# ### PLOT
	
	# ## DESIGN:
	# # 9 panels, each row is a given value of sigma 1 and sigma2, left column is T1 (ENM vs OMNI), middle T2 (ENM vs OMNI), and right T1 vs T2 (both ENM)
	# # x-axis is landscape correlation
	# # y-axis is test statistic
	# # plotted are trend lines for each model (ENM and OMNISCIENT) overlayed with bars for each value of landscape correlation
	# # exploring a small subset of values of sigma1 and sigma2
		
	# # plot parameters
	# colOmni <- '#67001f' # reddish
	# colEnm <- '#053061' # blueish
	
	# colT1 <- '#7f3b08'
	# colT2 <- '#2d004b'

	# # legend text size
	# cex <- 0.4

	# # nudge barpots this much left/right from given value of landscape correlation
	# xOffset <- 0.03
	
	# # bar width
	# barWidth <- 0.10
	
	# # data frame containing sets of values of sigma1 and sigma2 to be explored
	# sigmaSets <- data.frame(
		# sigma1=c(0.1, 0.5, 0.5),
		# sigma2=c(0.1, 0.5, 0.1)
	# )
	
	# for (algo in c('maxent', 'gam', 'brt')) {
	# # for (algo in c('maxent')) {
		
		# for (metric in c('cor stratified bg')) {

			# # test metric (abbrevaited for column name string) for OMNISCIENT and ENM, stratified BG or not, string to append to file name, main title
			# if (metric=='cor absences') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corAbsFullVs'
				# strat <- ''
				# fileDescrip <- paste0('COR - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# main <- paste0('COR Test - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# invert <- TRUE # use 1 - test value for interpretation
				# testStat <- 'COR'
			# } else if (metric=='cor bg') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corBgFullVs'
				# strat <- ''
				# fileDescrip <- paste0('COR - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# main <- paste0('COR Test - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# invert <- TRUE # use 1 - test value for interpretation
				# testStat <- 'COR'
			# } else if (metric=='cor stratified bg') {
				# contrastOmni <- 'corAbsFullVs'
				# contrastEnm <- 'corBgFullVs'
				# strat <- 'Strat'
				# fileDescrip <- paste0('COR - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Stratified BG')
				# main <- paste0('COR Test - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Stratified BG')
				# invert <- TRUE # use 1 - test value for interpretation
				# testStat <- 'COR'
			# } else if (metric=='auc absences') {
				# contrastOmni <- 'aucAbs'
				# contrastEnm <- 'aucAbs'
				# strat <- ''
				# fileDescrip <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# main <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' Absences')
				# invert <- TRUE # use 1 - test value for interpretation
				# testStat <- 'AUC'
			# } else if (metric=='auc bg') {
				# contrastOmni <- 'aucAbs'
				# contrastEnm <- 'aucBg'
				# strat <- ''
				# fileDescrip <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# main <- paste0('AUC - OMNISCIENT Pres vs Abs - ', toupper(algo), ' BG')
				# invert <- TRUE # use 1 - test value for interpretation
				# testStat <- 'AUC'
			# }
			
			# png(paste0(resultsDir, '/', directory, '/r=any rho=0/!WEAK vs STRONG - ', fileDescrip ,'.png'), width=850 * 3, height=850 * 3, res=600)

				# par(mfrow=c(3, 3), cex=cex, mar=0.8 * c(5, 4, 4, 2) + 0.2, mgp=c(2, 1, 0), pty='m', cex.lab=1.2, lwd=0.8)
			
				# # for each set of sigma1 and sigma2
				# for (countSet in 1:nrow(sigmaSets)) {

					# say(algo, ' ', metric, ' sigmaSet ', sigmaSets$sigma1[countSet], ' ', sigmaSets$sigma2[countSet])
				
					# ## ENM vs OMNISCIENT
					# for (variable in c('T1', 'T2')) {
				
						# # setup plot
						# plot(x=sort(unique(results$landscapeCor)), y=sort(unique(results$landscapeCor)), ylim=c(0, 1), xlim=c(-1, 1), xlab='Correlation on landscape', ylab=testStat, col='white')
				
						# omniCenter <- enmCenter <- numeric() # stores values of center of test statistic for qa given set of sigmas across values of landscape correlation
						
						# # for each value of landscape correlation plot TRENDLINE
						# for (landscapeCor in sort(unique(results$landscapeCor))) {
						
							# # get results for this scenario
							# importOmni <- collateImport(algo='omniscient', results=results, variable=variable, contrast=contrastOmni, strat='', rho=0, landscapeCor=landscapeCor, invert=invert)
							# importEnm <- collateImport(algo=algo, results=results, variable=variable, contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)

							# # get center values for this sigma set
							# thisOmniCenter <- importOmni$center[
								# rownames(importOmni$center)==paste0('sigma2_', sub(sigmaSets$sigma2[countSet], pattern='[.]', replacement='pt')),
								# colnames(importOmni$center)==paste0('sigma1_', sub(sigmaSets$sigma1[countSet], pattern='[.]', replacement='pt'))
							# ]
							
							# thisEnmCenter <- importEnm$center[
								# rownames(importEnm$center)==paste0('sigma2_', sub(sigmaSets$sigma2[countSet], pattern='[.]', replacement='pt')),
								# colnames(importEnm$center)==paste0('sigma1_', sub(sigmaSets$sigma1[countSet], pattern='[.]', replacement='pt'))
							# ]
							
							# omniCenter <- c(omniCenter, thisOmniCenter)
							# enmCenter <- c(enmCenter, thisEnmCenter)
													
						# } # next landscape correlation value
						
						# # trendlines
						# lines(sort(unique(results$landscapeCor)) - xOffset, omniCenter, col=alpha(colOmni, 0.5), lwd=1)
						# lines(sort(unique(results$landscapeCor)) + xOffset, enmCenter, col=alpha(colEnm, 0.5), lwd=1)
						
						# # for each value of landscape correlation plot BARS
						# for (landscapeCor in sort(unique(results$landscapeCor))) {
						
							# # get results for this scenario
							# importOmni <- collateImport(algo='omniscient', results=results, variable=variable, contrast=contrastOmni, strat='', rho=0, landscapeCor=landscapeCor, invert=invert)
							# importEnm <- collateImport(algo=algo, results=results, variable=variable, contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)

							# # get indices for given values of sigma1 and sigma2
							# thisOne <- c(
								# which(rownames(importOmni$center)==paste0('sigma2_', sub(sigmaSets$sigma2[countSet], pattern='[.]', replacement='pt'))),
								# which(colnames(importOmni$center)==paste0('sigma1_', sub(sigmaSets$sigma1[countSet], pattern='[.]', replacement='pt')))
							# )
						
							# # plot rectangles
							# polygon(x=c(landscapeCor - xOffset, landscapeCor - xOffset + barWidth, landscapeCor - xOffset + barWidth, landscapeCor - xOffset),
								# y=c(importOmni$lower[thisOne[1], thisOne[2]], importOmni$lower[thisOne[1], thisOne[2]], importOmni$upper[thisOne[1], thisOne[2]], importOmni$upper[thisOne[1], thisOne[2]]),
								# col=alpha(colOmni, 0.5),
								# border=alpha(colOmni, 0.8)
							# )
								
							# polygon(x=c(landscapeCor + xOffset, landscapeCor + xOffset + barWidth, landscapeCor + xOffset + barWidth, landscapeCor + xOffset),
								# y=c(importEnm$lower[thisOne[1], thisOne[2]], importEnm$lower[thisOne[1], thisOne[2]], importEnm$upper[thisOne[1], thisOne[2]], importEnm$upper[thisOne[1], thisOne[2]]),
								# col=alpha(colEnm, 0.5),
								# border=alpha(colEnm, 0.9)
							# )
							
							# # median lines
							# lines(x=c(landscapeCor - xOffset, landscapeCor - xOffset + barWidth), y=c(importOmni$center[thisOne[1], thisOne[2]], importOmni$center[thisOne[1], thisOne[2]]), col=alpha(colOmni, 0.9), lwd=1)
							# lines(x=c(landscapeCor + xOffset, landscapeCor + xOffset + barWidth), y=c(importEnm$center[thisOne[1], thisOne[2]], importEnm$center[thisOne[1], thisOne[2]]), col=alpha(colEnm, 0.9), lwd=1)
							
							# # asterisks
							# if (importOmni$lower[thisOne[1], thisOne[2]] > importEnm$upper[thisOne[1], thisOne[2]] | importOmni$upper[thisOne[1], thisOne[2]] < importEnm$lower[thisOne[1], thisOne[2]]) {
							
								# text(x=landscapeCor, y=0.95, labels='*', cex=1.4)
								
							# }
						
						# } # next landcape correlation
					
						# # panel label
						# label <- if (countSet==1 & variable=='T1') {
							# substitute(paste('a) T1-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
						# } else if (countSet==1 & variable=='T2') {
							# substitute(paste('b) T2-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
						# } else if (countSet==2 & variable=='T1') {
							# substitute(paste('d) T1-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
						# } else if (countSet==2 & variable=='T2') {
							# substitute(paste('e) T2-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
						# } else if (countSet==3 & variable=='T1') {
							# substitute(paste('g) T1-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
						# } else if (countSet==3 & variable=='T2') {
							# substitute(paste('h) T2-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
						# }
						
						# text(x=-1.65, y=1.15, labels=label, xpd=NA, pos=4, cex=1.2)
						
						# # legend
						# # pos <- if (importEnm$upper[thisOne[1], thisOne[2]] > 1 - importEnm$lower[thisOne[1], thisOne[2]]) {
							# # 'bottomleft'
						# # } else {
							# # 'topleft'
						# # }
						
						# if (countSet==1) legend('bottomleft', ncol=2, inset=-0.02, bty='n', legend=c('OMNISCIENT', toupper(algo)), fill=c(alpha(colOmni, 0.5), alpha(colEnm, 0.5)), border=c(alpha(colOmni, 0.5), alpha(colEnm, 0.5)), cex=0.8)
					
					# } # T1 or T2
					
					# ## ENM T1 vs T2

					# # setup plot
					# plot(x=sort(unique(results$landscapeCor)), y=sort(unique(results$landscapeCor)), ylim=c(0, 1), xlim=c(-1, 1), xlab='Correlation on landscape', ylab=testStat, col='white')
			
					# enmCenterT1 <- enmCenterT2 <- numeric() # stores values of center of test statistic for qa given set of sigmas across values of landscape correlation
					
					# # for each value of landscape correlation plot TRENDLINE
					# for (landscapeCor in sort(unique(results$landscapeCor))) {
					
						# # get results for this scenario
						# importEnmT1 <- collateImport(algo=algo, results=results, variable='T1', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)
						# importEnmT2 <- collateImport(algo=algo, results=results, variable='T2', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)

						# # get center values for this sigma set
						# thisEnmCenterT1 <- importEnmT1$center[
							# rownames(importEnmT1$center)==paste0('sigma2_', sub(sigmaSets$sigma2[countSet], pattern='[.]', replacement='pt')),
							# colnames(importEnmT1$center)==paste0('sigma1_', sub(sigmaSets$sigma1[countSet], pattern='[.]', replacement='pt'))
						# ]
						
						# thisEnmCenterT2 <- importEnmT2$center[
							# rownames(importEnmT2$center)==paste0('sigma2_', sub(sigmaSets$sigma2[countSet], pattern='[.]', replacement='pt')),
							# colnames(importEnmT2$center)==paste0('sigma1_', sub(sigmaSets$sigma1[countSet], pattern='[.]', replacement='pt'))
						# ]
						
						# enmCenterT1 <- c(enmCenterT1, thisEnmCenterT1)
						# enmCenterT2 <- c(enmCenterT2, thisEnmCenterT2)
												
					# } # next landscape correlation value
					
					# # trendlines
					# lines(sort(unique(results$landscapeCor)) - xOffset, enmCenterT1, col=alpha(colT1, 0.5), lwd=1)
					# lines(sort(unique(results$landscapeCor)) + xOffset, enmCenterT2, col=alpha(colT2, 0.5), lwd=1)
					
					# # plot vars for each value of landscape correlation
					
					# # for each value of landscape correlation plot BARS
					# for (landscapeCor in sort(unique(results$landscapeCor))) {
					
						# # get results for this scenario
						# importEnmT1 <- collateImport(algo=algo, results=results, variable='T1', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)
						# importEnmT2 <- collateImport(algo=algo, results=results, variable='T2', contrast=contrastEnm, strat=strat, rho=0, landscapeCor=landscapeCor, invert=invert)

						# # get indices for given values of sigma1 and sigma2
						# thisOne <- c(
							# which(rownames(importOmni$center)==paste0('sigma2_', sub(sigmaSets$sigma2[countSet], pattern='[.]', replacement='pt'))),
							# which(colnames(importOmni$center)==paste0('sigma1_', sub(sigmaSets$sigma1[countSet], pattern='[.]', replacement='pt')))
						# )
					
						# # plot rectangles
						# polygon(x=c(landscapeCor - xOffset, landscapeCor - xOffset + barWidth, landscapeCor - xOffset + barWidth, landscapeCor - xOffset),
							# y=c(importEnmT1$lower[thisOne[1], thisOne[2]], importEnmT1$lower[thisOne[1], thisOne[2]], importEnmT1$upper[thisOne[1], thisOne[2]], importEnmT1$upper[thisOne[1], thisOne[2]]),
							# col=alpha(colT1, 0.5),
							# border=alpha(colT1, 0.8)
						# )
							
						# polygon(x=c(landscapeCor + xOffset, landscapeCor + xOffset + barWidth, landscapeCor + xOffset + barWidth, landscapeCor + xOffset),
							# y=c(importEnmT2$lower[thisOne[1], thisOne[2]], importEnmT2$lower[thisOne[1], thisOne[2]], importEnmT2$upper[thisOne[1], thisOne[2]], importEnmT2$upper[thisOne[1], thisOne[2]]),
							# col=alpha(colT2, 0.5),
							# border=alpha(colT2, 0.9)
						# )
						
						# # median lines
						# lines(x=c(landscapeCor - xOffset, landscapeCor - xOffset + barWidth), y=c(importEnmT1$center[thisOne[1], thisOne[2]], importEnmT1$center[thisOne[1], thisOne[2]]), col=alpha(colOmni, 0.9), lwd=1)
						# lines(x=c(landscapeCor + xOffset, landscapeCor + xOffset + barWidth), y=c(importEnmT2$center[thisOne[1], thisOne[2]], importEnmT2$center[thisOne[1], thisOne[2]]), col=alpha(colEnm, 0.9), lwd=1)
						
						# # asterisks
						# if (importEnmT1$lower[thisOne[1], thisOne[2]] > importEnmT2$upper[thisOne[1], thisOne[2]] | importEnmT1$upper[thisOne[1], thisOne[2]] < importEnmT2$lower[thisOne[1], thisOne[2]]) {
						
							# text(x=landscapeCor, y=0.95, labels='*', cex=1.4)
							
						# }
					
					# } # next landcape correlation
				
					# # panel label
					# label <- if (countSet==1) {
						# substitute(paste('c) T1-M vs T2-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
					# } else if (countSet==2) {
						# substitute(paste('f) T1-M vs T2-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
					# } else if (countSet==3) {
						# substitute(paste('i) T1-M vs T2-M: ', sigma[1], '=', sig1, ', ', sigma[2], '=', sig2, sep=''), list(sig1=sigmaSets$sigma1[countSet], sig2=sigmaSets$sigma2[countSet]))
					# }
					
					# text(x=-1.65, y=1.15, labels=label, xpd=NA, pos=4, cex=1.2)
					
					# # legend
					# # pos <- if (importEnmT1$upper[thisOne[1], thisOne[2]] > 1 - importEnmT2$lower[thisOne[1], thisOne[2]]) {
						# # 'bottomleft'
					# # } else {
						# # 'topleft'
					# # }
					
					# if (countSet==1) legend('bottomleft', ncol=2, inset=-0.02, bty='n', legend=c('T1', 'T2'), fill=c(alpha(colT1, 0.5), alpha(colT2, 0.5)), border=c(alpha(colT1, 0.5), alpha(colT2, 0.5)), cex=0.8)
					
				# } # next set of sigmas
			
			# title(main, outer=TRUE, line=-1, cex=1.3)
			
			# dev.off()
	
		# } # next metric type
		
	# } # next algorithm
	
# say('   ###############################################')
# say('   ### [weak vs strong] interaction importance ###')
# say('   ###############################################')
	
	# say('Plot interaction importance as a function of rho.')
	
	# dirCreate(resultsDir, directory, '/interaction importance r=select rho=any')

	# landRotVsCor <- read.csv(paste0(workDir, '/Correlations between Standard Environmental Variables.csv'), as.is=TRUE, row.names=1)
	
	# results <- readRDS(paste0(resultsDir, directory, '/!Compiled Results.rds'))

	# ### GENERALIZE
	# cex <- 0.8
	# cex.main <- 0.6
	# cex.lab <- 0.6
	# cex.axis <- 0.6

	# colOmni <- '#67001f' # reddish
	# colEnm <- '#053061' # blueish
	
	
	# ### PLOT
	
	# ## DESIGN:
	# # 3x3 panels
	# # landscape correlation changes from postive to negative across rows of panels (3 values)
	# # values of sigma1 and sigma2 change across rows of panels
	# # x-axis of each panel is rho
	# # y axis of each panel is interaction importance
	
	# for (algo in c('maxent')) {
		
		# # for each type of interaction importance test (permute before or after product is taken)
		# for (iaType in c('Before', 'After')) {
			
			# png(paste0(resultsDir, directory, '/interaction importance r=select rho=any/!WEAK vs STRONG Interaction Importance - ', toupper(algo), ' COR - Permutation  ', iaType, ' Product.png'), width=1800, height=1800, res=300)
						
				# par(mfrow=c(3, 3), mar=0.4 * c(5, 4, 4, 2) + 0.2, mgp=c(1, 0.25, 0), pty='m', lwd=1, cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, tck=-0.025)
					
				# set1 <- c(0.1, 0.1)
				# set2 <- c(0.5, 0.1)
				# set3 <- c(0.5, 0.5)

				# # for each pair of sigma values
				# for (sigma1and2 in c('set1', 'set2', 'set3')) {
				
					# set <- get(sigma1and2)
				
					# # for each landscape correlation
					# for (landscapeCor in sort(unique(results$landscapeCor))[c(2, 4, 6)]) {

						# if (verbose > 0) say(algo, ' ', iaType, ' ', set, ' ', landscapeCor)
						
						# # to store top 97.5th and bottom 2.5th percentile values
						# yUpperOmni <- yCenterOmni <- yLowerOmni <- yUpperEnm <- yCenterEnm <- yLowerEnm <- numeric()
					
						# # get values of test metric
						# for (rho in sort(unique(results$rho))) {

							# # collate importance values
							# importOmni <- collateImport(algo='omniscient', results=results, variable='T1xT2', contrast='corAbs', strat='', ia='IA', iaBeforeAfter=paste0(iaType, 'Prod'), rho=rho, landscapeCor=landscapeCor, invert=TRUE, fill=TRUE)
							
							# importEnm <- collateImport(algo=algo, results=results, variable='T1xT2', contrast='corBg', strat='Strat', ia='IA', iaBeforeAfter=paste0(iaType, 'Prod'), rho=rho, landscapeCor=landscapeCor, invert=TRUE, fill=TRUE)
						
							# rowName <- paste0('sigma2_', gsub(set[1], pattern='[.]', replacement='pt'))
							# colName <- paste0('sigma1_', gsub(set[2], pattern='[.]', replacement='pt'))

							# # make lists of upper/center/lower percentiles for each value of rho
							# yUpperOmni <- c(yUpperOmni, importOmni$upper[rownames(importOmni$upper) %in% rowName, colnames(importOmni$upper) %in% colName])
							# yCenterOmni <- c(yCenterOmni, importOmni$center[rownames(importOmni$center) %in% rowName, colnames(importOmni$center) %in% colName])
							# yLowerOmni <- c(yLowerOmni, importOmni$lower[rownames(importOmni$lower) %in% rowName, colnames(importOmni$lower) %in% colName])
								
							# yUpperEnm <- c(yUpperEnm, importEnm$upper[rownames(importEnm$upper) %in% rowName, colnames(importEnm$upper) %in% colName])
							# yCenterEnm <- c(yCenterEnm, importEnm$center[rownames(importEnm$center) %in% rowName, colnames(importEnm$center) %in% colName])
							# yLowerEnm <- c(yLowerEnm, importEnm$lower[rownames(importEnm$lower) %in% rowName, colnames(importEnm$lower) %in% colName])
						
						# } # next rho
						
						# allRho <- sort(unique(results$rho))
						
						# ## plot
						# plot(c(-1.1, 1.1), rep(1, 2), col='white', xlab='Rho', ylab='COR', ylim=c(0, 1))
						# title(main=paste0('r=', round(landscapeCor, 2), ' sigma1=', set[1], ' sigma2=', set[2]), line=0.3, cex.main=cex.main)
						
						# # legend
						# if (landscapeCor == sort(unique(results$landscapeCor))[c(2, 4, 6)][1] & sigma1and2=='set1') {
							
							# legend('bottomright', cex=0.5, fill=c(alpha(colOmni, 0.5), alpha(colEnm, 0.5)), border=c(colOmni, colEnm), legend=c('OMNI', toupper(algo)), bty='n')
							
						# }
						
						# # plot parameters
						# colOmni <- '#67001f' # reddish
						# colEnm <- '#053061' # blueish
						
						# barWidth <- 0.05 # bar width
						# offset <- 0.03 # offset of omniscient from ENM bar
						
						# # plot bar for each value of rho
						# for (i in seq_along(allRho)) {

							# # bars: omniscient
							# polygon(x=c(allRho[i] - barWidth, allRho[i] + barWidth,  allRho[i] + barWidth, allRho[i] - barWidth) - offset,
								# y=c(yLowerOmni[i], yLowerOmni[i], yUpperOmni[i], yUpperOmni[i]),
								# col=alpha(colOmni, 0.5),
								# border=colOmni
							# )
							
							# lines(x=c(allRho[i] - barWidth, allRho[i] + barWidth) - offset, y=c(yCenterOmni[i], yCenterOmni[i]), col=colOmni)
						
							# # bars: ENM
							# polygon(x=c(allRho[i] - barWidth, allRho[i] + barWidth,  allRho[i] + barWidth, allRho[i] - barWidth) + offset,
								# y=c(yLowerEnm[i], yLowerEnm[i], yUpperEnm[i], yUpperEnm[i]),
								# col=alpha(colEnm, 0.5),
								# border=colEnm
							# )
						
							# lines(x=c(allRho[i] - barWidth, allRho[i] + barWidth) + offset, y=c(yCenterEnm[i], yCenterEnm[i]), col=colEnm)
							
							# if (yLowerOmni[i] > yUpperEnm[i] | yUpperOmni[i] < yLowerEnm[i]) text(x=allRho[i], y=1.02, labels='*')
						
					# } # next landscape correlation
					
					# title(main=paste0('Interaction Importance - ', toupper(algo), ' - COR - Permutation ', iaType, ' Product'), outer=TRUE, line=-0.7)
					
				# } # next set of sigma values
				
			# } # next combination of sigma1 and sigma2
			
			# dev.off()
			
		# } # next interation permutation type
			
	# } # next algorithm

# say('##########################')
# say('### [correlated false] ###')
# say('##########################')

# say('WHY DOES SPECIES RANGE CHANGE WITH ROTATION OF F1 (see LANDSCAPE FIGURES IN RESULTS FOLDER)?????????????/')

# directory <- '[correlated false]'
# dir.create(paste0(resultsDir, directory), recursive=T, showWarnings=F)
# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 T2 F1 GEOG cor(linear(T1) linear(T2) linear(F1))'
# say(scenario)
# write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

# response <- gaussian

# geography <- list(
	# list(type='linear', min=min, max=max, pregen=TRUE),
	# list(type='linear', min=min, max=max, pregen=TRUE, rot=90),
	# list(type='linear', min=min, max=max, pregen=FALSE, rot=NA)
# )

# rot <- c(22.5, 45, 67.5, 112.5, 135, 157.5, 202.5, 225, 247.5, 292.5, 315, 337.5)

# dirCreate(paste0(resultsDir, directory, '/starts - ', algorithm))
# dirCreate(paste0(resultsDir, directory, '/stops - ', algorithm))

# # rotate third landscape variable
# for (thisRot in rot) {

	# # create landscape
	# geography[[3]]$rot <- thisRot
	# geography[[3]]$pregen <- (thisRot < 180)
	# landscape <- genesis(geography, circle=TRUE)
	# names(landscape) <- c('T1', 'T2', 'F1')
	
	# for (rho in seq(-0.75, 0.75, by=0.25)) {

		# say('rot = ', thisRot, ' rho = ', rho, ' sigma1 = ', sigma1, ' sigma2 =', sigma2)
	
		# write.csv(NA, paste0(resultsDir, directory, '/starts - ', algorithm, '/', 'rot=', thisRot, 'rho=', rho, 'sigma1=', sigma1, 'sigma2', sigma2), row.names=F)
	
		# # generate species
		# species <- response(mu1=mu1, mu2=mu2, x1=subset(landscape, 1), x2=subset(landscape, 2), sigma1=sigma1, sigma2=sigma2, rho=rho)
		# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

		# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=c(T, T, F), name=paste0('rot(F1)=', thisRot, ' rho=', rho, ', sigma1=', sigma1, ' sigma2=', sigma2), bg=bg, fg=fg)

		# # model!
		# main(
			# species=species,
			# response=response,
			# landscape=landscape,
			# geography=geography,
			# directory=paste0(resultsDir, directory),
			# numTrainPres=numTrainPres,
			# numTestPres=numTestPres,
			# iterToDo=iterToDo,
			# jFoldMax=jFoldMax,
			# numBg=numBg,
			# algorithm=algorithm,
			# suffix=paste0('rot(F1)=', thisRot, ' rho=', rho, ' sigma1=', sigma1, ' sigma2=', sigma2),
			# verbose=verbose,
			# interaction=FALSE,
			# cbi=TRUE,
			# mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho
		# )
		
		# write.csv(NA, paste0(resultsDir, directory, '/stops - ', algorithm, '/', 'rot=', thisRot, 'rho=', rho, 'sigma1=', sigma1, 'sigma2', sigma2), row.names=F)
		
	# } # next rho
	
# } # next rotation of second landscape variable

# # ### plot

# # ## load data files -- ALL SDMs
# # if (exists('master')) rm(master)
# # for (pattern in c('OMNISCIENT', 'MAXENT')) {

	# # files <- list.files(paste0(resultsDir, directory), full.names=T, pattern=paste0('Results - ', toupper(pattern)))
	
	# # for (f in files) {

	# # load(f)

		# # if (grepl(f, pattern='rot[(]F1[)]=22pt5')) evalFrame$corFALSEwithT1 <- 0.924428913503058
		# # if (grepl(f, pattern='rot[(]F1[)]=45')) evalFrame$corFALSEwithT1 <- 0.709186921823756
		# # if (grepl(f, pattern='rot[(]F1[)]=67pt5')) evalFrame$corFALSEwithT1 <- 0.38608424191768
		# # if (grepl(f, pattern='rot[(]F1[)]=90')) evalFrame$corFALSEwithT1 <- 0.00319479379821185
		# # if (grepl(f, pattern='rot[(]F1[)]=112pt5')) evalFrame$corFALSEwithT1 <- -0.381040125933902
		# # if (grepl(f, pattern='rot[(]F1[)]=135')) evalFrame$corFALSEwithT1 <- -0.706939685633619
		# # if (grepl(f, pattern='rot[(]F1[)]=157pt5')) evalFrame$corFALSEwithT1 <- -0.924074122738334
		
		# # if (grepl(f, pattern='rot[(]F1[)]=22pt5')) evalFrame$corFALSEwithT2 <- 0.384304340021029
		# # if (grepl(f, pattern='rot[(]F1[)]=45')) evalFrame$corFALSEwithT2 <- 0.707281944552962
		# # if (grepl(f, pattern='rot[(]F1[)]=67pt5')) evalFrame$corFALSEwithT2 <- 0.923691652761138
		# # if (grepl(f, pattern='rot[(]F1[)]=90')) evalFrame$corFALSEwithT2 <- 1
		# # if (grepl(f, pattern='rot[(]F1[)]=112pt5')) evalFrame$corFALSEwithT2 <- 0.923335827305056
		# # if (grepl(f, pattern='rot[(]F1[)]=135')) evalFrame$corFALSEwithT2 <- 0.705011029659109
		# # if (grepl(f, pattern='rot[(]F1[)]=157pt5')) evalFrame$corFALSEwithT2 <- 0.379257644205097
		
		# # if (grepl(f, pattern='rho=-0pt75')) evalFrame$rho <- -0.75
		# # if (grepl(f, pattern='rho=-0pt25')) evalFrame$rho <- -0.25
		# # if (grepl(f, pattern='rho=-0pt5')) evalFrame$rho <- -0.5
		# # if (grepl(f, pattern='rho=0')) evalFrame$rho <- 0
		# # if (grepl(f, pattern='rho=0pt25')) evalFrame$rho <- 0.25
		# # if (grepl(f, pattern='rho=0pt5')) evalFrame$rho <- 0.5
		# # if (grepl(f, pattern='rho=0pt75')) evalFrame$rho <- 0.75
		
		# # if (pattern=='MAXENT') evalFrame <- evalFrame[ , names(master)]
		
		# # master <- if (exists('master')) { rbind(master, evalFrame) } else { evalFrame }
		
	# # }

# # }

# # # customizations
# # xVarName <- 'corFALSEwithT1'

# # # one set of panels per rho
# # for (rho in c(-0.5, 0, 0.5)) {

	# # thisMaster <- master[master$rho==rho, ]
	# # thisMaster <- thisMaster[order(thisMaster[ , xVarName]), ]
	# # thisAgg <- aggregate(thisMaster[ , 2:ncol(thisMaster)], by=list(thisMaster$algorithm, thisMaster[ , xVarName]), median, na.rm=T)
	# # thisAgg$algorithm <- thisAgg[ , xVarName] <- NULL
	# # names(thisAgg)[1:2] <- c('algorithm', xVarName)

	# # # OMNISCIENT vs MAXENT
	# # plotVsScalar3Panel(
		# # xVarName=xVarName,
		# # models=c('omniscient', 'maxent'),
		# # resp=c('corAbsFullVsPerm_perm', 'corBgFullVsPermStrat_perm'),
		# # off=c(-0.015, 0.015),
		# # pch=c(16, 17),
		# # col=c(col1, col2),
		# # leg=c('OMNISCIENT', 'MAXENT'),
		# # variables=c('T1', 'T2', 'F1'),
		# # main=c('TRUE (weaker)', 'TRUE (stronger)', 'FALSE'),
		# # xlim=c(-1, 1),
		# # legPos=c('bottomleft', 'topleft', 'bottomleft'),
		# # legInset=0.025,
		# # legCex=1.25,
		# # xlab='Correlation of FALSE with weak TRUE',
		# # ylab='Correlation',
		# # filename=paste0('!PERMUTE Test - Vs Correlation with T1 Variable - Rho = ', rho)
	# # )
	
# # }

# # # MAXENT-specific metrics
# # ## load data files -- ALL SDMs
# # if (exists('master')) rm(master)
# # files <- list.files(paste0(resultsDir, directory), full.names=T, pattern=('Results - MAXENT'))

# # for (f in files) {

	# # load(f)

	# # if (grepl(f, pattern='rot[(]F1[)]=22pt5')) evalFrame$corFALSEwithT1 <- 0.924428913503058
	# # if (grepl(f, pattern='rot[(]F1[)]=45')) evalFrame$corFALSEwithT1 <- 0.709186921823756
	# # if (grepl(f, pattern='rot[(]F1[)]=67pt5')) evalFrame$corFALSEwithT1 <- 0.38608424191768
	# # if (grepl(f, pattern='rot[(]F1[)]=90')) evalFrame$corFALSEwithT1 <- 0.00319479379821185
	# # if (grepl(f, pattern='rot[(]F1[)]=112pt5')) evalFrame$corFALSEwithT1 <- -0.381040125933902
	# # if (grepl(f, pattern='rot[(]F1[)]=135')) evalFrame$corFALSEwithT1 <- -0.706939685633619
	# # if (grepl(f, pattern='rot[(]F1[)]=157pt5')) evalFrame$corFALSEwithT1 <- -0.924074122738334
	
	# # if (grepl(f, pattern='rot[(]F1[)]=22pt5')) evalFrame$corFALSEwithT2 <- 0.384304340021029
	# # if (grepl(f, pattern='rot[(]F1[)]=45')) evalFrame$corFALSEwithT2 <- 0.707281944552962
	# # if (grepl(f, pattern='rot[(]F1[)]=67pt5')) evalFrame$corFALSEwithT2 <- 0.923691652761138
	# # if (grepl(f, pattern='rot[(]F1[)]=90')) evalFrame$corFALSEwithT2 <- 1
	# # if (grepl(f, pattern='rot[(]F1[)]=112pt5')) evalFrame$corFALSEwithT2 <- 0.923335827305056
	# # if (grepl(f, pattern='rot[(]F1[)]=135')) evalFrame$corFALSEwithT2 <- 0.705011029659109
	# # if (grepl(f, pattern='rot[(]F1[)]=157pt5')) evalFrame$corFALSEwithT2 <- 0.379257644205097
	
	# # if (grepl(f, pattern='rho=-0pt75')) evalFrame$rho <- -0.75
	# # if (grepl(f, pattern='rho=-0pt25')) evalFrame$rho <- -0.25
	# # if (grepl(f, pattern='rho=-0pt5')) evalFrame$rho <- -0.5
	# # if (grepl(f, pattern='rho=0')) evalFrame$rho <- 0
	# # if (grepl(f, pattern='rho=0pt25')) evalFrame$rho <- 0.25
	# # if (grepl(f, pattern='rho=0pt5')) evalFrame$rho <- 0.5
	# # if (grepl(f, pattern='rho=0pt75')) evalFrame$rho <- 0.75
	
	# # if (exists('master')) evalFrame <- evalFrame[ , names(master)]
	
	# # master <- if (exists('master')) { rbind(master, evalFrame) } else { evalFrame }
		
# # }

# # master$maxentTrainGainWithOnlyT1 <- master$maxentTrainGainWithOnlyT1 * 100
# # master$maxentTrainGainWithOnlyT2 <- master$maxentTrainGainWithOnlyT2 * 100
# # master$maxentTrainGainWithOnlyF1 <- master$maxentTrainGainWithOnlyF1 * 100

# # for (rho in c(-0.5, 0, 0.5)) {

	# # thisMaster <- master[master$rho==rho, ]
	# # thisMaster <- thisMaster[order(thisMaster[ , xVarName]), ]
	# # thisAgg <- aggregate(thisMaster[ , 2:ncol(thisMaster)], by=list(thisMaster[ , xVarName]), median, na.rm=T)
	# # thisAgg$algorithm <- thisAgg[ , xVarName] <- NULL
	# # names(thisAgg)[1] <- xVarName
	# # thisAgg$algorithm <- 'maxent'

	# # # MAXENT
	# # plotVsScalar3Panel(
		# # xVarName=xVarName,
		# # models=c('maxent', 'maxent', 'maxent'),
		# # resp=c('maxentContrib', 'maxentTrainGainWithOnly', 'maxentPermImport'),
		# # off=c(-0.02, 0, 0.02),
		# # pch=c(16, 17, 18),
		# # col=c(col3, col4, col5),
		# # leg=c('Contribution (% gain)', 'Gain with only this factor (x100)', 'Permutation (AUC)'),
		# # variables=c('T1', 'T2', 'F1'),
		# # main=c('TRUE (weaker)', 'TRUE (stronger)', 'FALSE'),
		# # xlim=c(-1, 1),
		# # legPos=c('topleft', 'bottomleft', 'topleft'),
		# # legInset=0.05,
		# # legCex=0.85,
		# # xlab='Correlation of FALSE with weak TRUE',
		# # ylab='Importance',
		# # filename=paste0('!MAXENT-Specific Tests - Vs Correlation with T1 Variable - Rho = ', rho)
	# # )
	
# # }


# say('##########################')
# say('### [missing variable] ###')
# say('##########################')

# directory <- '[missing variable]'
# dirCreate(paste0(resultsDir, directory))
# dirCreate(paste0(resultsDir, directory, '/starts - ', algorithm))
# dirCreate(paste0(resultsDir, directory, '/stops - ', algorithm))

# scenario <- 'RESPONSE gaussian(T1 T2) MODEL T1 F1 GEOG (cor(linear(T1) linear(T2) linear F1))'
# say(scenario)
# write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

# response <- gaussian

# rot <- c(22.5, 45, 67.5, 112.5, 135, 157.5, 202.5, 225, 247.5)
# # rot <- c(22.5, 45, 67.5, 112.5, 135, 157.5, 202.5, 225, 247.5)
# # rot <- rot[1] # 22.5 IP
# rot <- rot[2] # 45 IP
# # rot <- rot[3] # 67.5
# # rot <- rot[4] # 112.5
# # rot <- rot[5] # 135
# # rot <- rot[6] # 157.5
# # rot <- rot[7] # 202.5
# # rot <- rot[8] # 225
# # rot <- rot[9] # 247.5

# # rotate third landscape variable
# for (thisRot in rot) {

	# for (rho in seq(-0.75, 0.75, by=0.25)) {
	# # for (rho in 0) {
	
		# say('rot = ', thisRot, ' rho = ', rho, ' sigma1 = ', sigma1, ' sigma2 = ', sigma2)
	
		# write.csv(NA, paste0(resultsDir, directory, '/starts - ', algorithm, '/', 'rot=', thisRot, ' rho=', rho, ' sigma1=', sigma1, ' sigma2=', sigma2), row.names=F)
	
		# geography <- list(
			# list(type='linear', min=min, max=max, pregen=TRUE),
			# list(type='linear', min=min, max=max, pregen=TRUE, rot=90),
			# list(type='linear', min=min, max=max, pregen=(thisRot < 180), rot=thisRot)
		# )

		# # create landscape
		# geography[[3]]$rot <- thisRot
		# landscape <- genesis(geography, circle=TRUE)
		# names(landscape) <- c('T1', 'T2', 'F1')
		
		# # generate species
		# species <- response(mu1=mu1, mu2=mu2, x1=subset(landscape, 1), x2=subset(landscape, 2), sigma1=sigma1, sigma2=sigma2, rho=rho)
		# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

		# # plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=c(T, T, F), name=paste0('rot(F1)=', sub(as.character(thisRot), pattern='[.]', replacement='pt'), ' rho=', sub(as.character(rho), pattern='[.]', replacement='pt'), ', sigma2=', sigma2, pattern='[.]', replacement='pt')), bg=bg, fg=fg)
		
		# landscape <- subset(landscape, c(1, 3))
		# geography <- list(geography[[1]], geography[[3]])

		# # model!
		# main(
			# species=species,
			# response=response,
			# landscape=landscape,
			# geography=geography,
			# directory=paste0(resultsDir, directory),
			# numTrainPres=numTrainPres,
			# numTestPres=numTestPres,
			# iterToDo=iterToDo,
			# jFoldMax=jFoldMax,
			# numBg=numBg,
			# algorithm=algorithm,
			# suffix=paste0('rot(F1)=', thisRot, ' rho=', rho, ' sigma1=', sigma1, ' sigma2=', sigma2),
			# verbose=verbose,
			# interaction=FALSE,
			# cbi=TRUE,
			# mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho
		# )

		# write.csv(NA, paste0(resultsDir, directory, '/stops - ', algorithm, '/', 'rot=', thisRot, ' rho=', rho, ' sigma1=', sigma1, ' sigma2=', sigma2), row.names=F)
		
	# } # next rho
	
# } # next rotation of second landscape variable



# say('####################')
# say('### [experiment] ###')
# say('####################')

# say('Using outcome-based scenario selection to determine 1) what combinations of factors inappropriately reduce importance of TRUE variables and 2) increase importance of FALSE variables.')

# # generalization
# nrow <- 501 # rows/columns in a raster

# directory <- '[experiment]'
# dirCreate(paste0(resultsDir, directory))
# scenario <- 'RESPONSE random MODEL random GEOG random'
# say(scenario)
# write.csv(scenario, paste0(resultsDir, directory, '/!scenario - ', scenario, '.txt'), row.names=F)

# ### make raster to help obtain observed Pr(pres) on border cells
# bordersSquare <- onesSquare <- matrix(rep(1, nrow^2), nrow=nrow)

# # position rasters for generating circular landscape
# x <- matrix(rep(seq(-1, 1, length.out=nrow), nrow), nrow=nrow, byrow=T)
# y <- matrix(rep(seq(1, -1, length.out=nrow), each=nrow), nrow=nrow, byrow=T)
# dist <- sqrt(x^2 + y^2)
# template <- ifelse(dist <= 1, 1, NA)

# onesCircle <- onesSquare * template
# bordersCircle <- bordersSquare * NA

# for (row in 2:(nrow(onesCircle) - 1)) {
	# for (col in 2:(ncol(onesCircle) - 1)) {
		# if (is.na(onesCircle[row, col + 1]) || is.na(onesCircle[row + 1, col + 1]) || is.na(onesCircle[row + 1, col]) || is.na(onesCircle[row + 1 , col - 1 ]) || is.na(onesCircle[row, col - 1]) || is.na(onesCircle[row - 1, col - 1]) || is.na(onesCircle[row - 1, col]) || is.na(onesCircle[row - 1, col + 1])) {
			# bordersCircle[row, col] <- 1
		# }
	# }
# }

# bordersSquare <- raster(bordersSquare)
# bordersCircle <- raster(bordersCircle)
# projection(bordersSquare) <- projection(bordersCircle) <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# # get random sites for use when calculating correlation between layers
# geography <- list()
# geography[[1]] <- list(type='uniform')
# uniformSquare <- genesis(geography, circle=FALSE, nrow=nrow)
# uniformCircle <- genesis(geography, circle=TRUE, nrow=nrow)
# sitesSquare <- randomPointsRobust(uniformSquare, 11000, prob=F)
# sitesCircle <- randomPointsRobust(uniformCircle, 11000, prob=F)
	
# for (i in 1:1000) {

	# say('=============================================================================================')
	# say('model ', i, ' ', date())

	# ### geography
	# numLayer <- sample(2:10, 1)
	# # numLayer <- 3

	# # shape
	# circle <- sample(c(TRUE, FALSE), 1)
	
	# ### generate landscape
	# types <- c('linear', 'random', 'step', 'hinge', 'gaussian', 'sin')
	
	# # while there are at least two layers with absoluet value of pairwise correlation > a threshold, continue trying to make new landscapes!
	# maxAbsolCor <- Inf
	
	# while (maxAbsolCor > 0.7) {
		
		# for (n in 1:numLayer) {

			# type <- sample(types, 1)

			# min <- max <- at <- from <- to <- center1 <- center2 <- sd1 <- sd2 <- rho <- wavelength <- offset <- NA
			
			# if (type=='linear') {
				# min <- runif(1, -1, -0.1) 
				# max <- runif(1, 0.1, 1)
			# } else if (type=='random') {
				# min <- runif(1, -1, -0.1)
				# max <- runif(1, 0.1, 1)
			# } else if (type=='step') {
				# min <- runif(1, -1, -0.1)
				# max <- runif(1, 0.1, 1)
				# at <- runif(1, min, max)
			# } else if (type=='hinge') {
				# min <- runif(1, -1, 0)
				# max <- runif(1, 0, 1)
				# from <- runif(1, min, 0)
				# to <- runif(1, 0, max)
			# } else if (type=='gaussian') {
				# center1 <- runif(1, -1, 1)
				# center2 <- runif(1, -1, 1)
				# sd1 <- runif(1, 0.05, 1)
				# sd2 <- runif(1, 0.05, 1)
				# rho <- runif(1)
			# } else if (type=='sin') {
				# min <- runif(1, -1, 0)
				# max <- runif(1, 0, 1)
				# offset <- runif(1, -1, 1)
				# wavelength <- runif(1, 0, 2)
			# }
			
			# rot <- if (circle) { runif(1, 0, 180) } else { NA }
			
			# if (n==1) {
				# geography <- list()
				# geography[[1]] <- list(type=type, min=min, max=max, rot=rot, randOrient=TRUE, at=at, from=from, to=to, center1=center1, center2=center2, sd1=sd1, sd2=sd2, rho=rho, wavelength=wavelength, offset=offset)
			# } else {
				# geography[[length(geography) + 1]] <- list(type=type, min=min, max=max, rot=rot, randOrient=TRUE, at=at, from=from, to=to, center1=center1, center2=center2, sd1=sd1, sd2=sd2, rho=rho, wavelength=wavelength, offset=offset)
			# }

		# }

		# landscape <- genesis(geography=geography, nrow=nrow, circle=circle)
		
		# env <- if (circle) { as.data.frame(extract(landscape, sitesCircle)) } else { as.data.frame(extract(landscape, sitesSquare)) }
		# if (nlayers(landscape)==2) {
			# maxAbsolCor <- abs(cor(env[ , 1], env[ , 2], use='complete.obs'))
		# } else {
			# maxAbsolCor <- numeric()
			# for (countOne in 1:(nlayers(landscape) - 1)) {
				# for (countTwo in (countOne + 1):nlayers(landscape)) {
					# maxAbsolCor <- c(maxAbsolCor, cor(env[ , countOne], env[ , countTwo], use='complete.obs'))
				# }
			# }
			# maxAbsolCor <- max(abs(maxAbsolCor))
		# }
	
	# } # while correlation between any two layers is too high

	# ### species

	# # species has univariate or bivariate response
	# univariate <- sample(c(TRUE, FALSE), 1, prob=c(0.5, 0.5))

	# # name landscape layers appropriately
	# if (univariate) {
		# names(landscape) <- c('T1', paste0('F', 1:(nlayers(landscape) - 1)))
	# } else {
		# names(landscape)[1:2] <- c('T1', 'T2')
		# if (nlayers(landscape) > 2) names(landscape)[3:nlayers(landscape)] <- paste0('F', 1:(nlayers(landscape) - 2))
	# }
	# names(env) <- names(landscape)
	
	# # covariance in species' response
	# covar <- if (!univariate) { FALSE } else { sample(c(TRUE, FALSE), 1, prob=c(0.5, 0.5)) }

	# # response function
	# b0 <- b1 <- b2 <- b12 <- mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# if (sample(1:2, 1)==1) {
		# response <- logistic
		# responseText <- 'logistic'
		# b0 <- runif(1, -1, 1)
		# b1 <- runif(1, 0, 2)
		# b2 <- if (univariate) { 0 } else { runif(1, -2, 2) }
		# b12 <- if (!covar) { 0 } else { runif(1, -2, 2) }
		# species <- response(b0=b0, b1=b1, b2=b2, b12=b12, x1=subset(landscape, 1), x2=subset(landscape, 2))
	# } else {
		# response <- gaussian
		# responseText <- 'gaussian'
		# mu1 <- mu2 <- 0
		# sigma1 <- runif(1, 0, 0.5)
		# sigma2 <- if (univariate) { Inf } else { runif(1, 0, 0.5) }
		# rho <- if (!covar) { 0 } else { runif(1, -1, 1) }
		# x2 <- if (univariate) { raster(onesSquare) * mu2 } else { subset(landscape, 2) }
		# species <- response(mu1=mu1, mu2=mu2, x1=subset(landscape, 1), x2=x2, sigma1=sigma1, sigma2=sigma2, rho=rho)
	# }
	
	# prevalence <- cellStats(species, 'mean')

	# # calculate borderness statistics
	# speciesBorder <- if (circle) { species * bordersCircle } else { species * bordersSquare }
	# maxBorder <- cellStats(speciesBorder, 'max')
	# borderness <- maxBorder / prevalence
	
	# # calculate maximum correlation between T variable and other TRUE variale and any FALSE variable
	# corT1T2 <- if (univariate) { NA } else { cor(env$T1, env$T2, use='complete.obs') }
	# corT1F <- NA
	# if (any(grepl(names(env), pattern='F'))) {
		# for (count in which(grepl(names(env), pattern='F'))[1]:ncol(env)) corT1F <- c(corT1F, cor(env$T1, env[ , count], use='complete.obs'))
	# }
	# mostCorT1F <- if (length(corT1F)==1 && is.na(corT1F)) { NA } else { sign(corT1F[which.max(abs(corT1F))]) * max(abs(corT1F), na.rm=T) }
	# meanAbsolCorT1F <- if (length(corT1F)==1 && is.na(corT1F)) { NA } else { mean(abs(corT1F), na.rm=T) }

	# # remove TRUE variable
	# missingVar <- ifelse(numLayer==2, FALSE, sample(c(FALSE, TRUE), 1))
	# if (missingVar) landscape <- dropLayer(landscape, 2)

	# ### SDM
	# numTrainPres <- max(nlayers(landscape) * 10, sample(20:400, 1)) # at least 10 presences for each variable modeler enters into model
	
	# # algorithm <- sample(c('brt', 'gam', 'maxent'), 1)
	# algorithm <- sample(c('brt', 'maxent'), 1)
	# # algorithm <- sample(c('gam'), 1)

	# ### file suffix
	# details1 <- paste0(' alg=', algorithm, ' n=', numTrainPres)
	# details2 <- paste0(' resp=', ifelse(responseText=='gaussian', 'gauss', 'logist'), ' type=', ifelse(univariate, 'univ', 'biv'), ' covar=', ifelse(covar, 'T', 'F'), ' prev=', round(prevalence, 2), ' border=', round(borderness, 2))
	# details3 <- paste0(' circle=', ifelse(circle, 'T', 'F'), ' miss=', ifelse(missingVar, 'T', 'F'), ' layers=', numLayer, ' typeT1=', geography[[1]]$type, ' typeT2=', ifelse(univariate, 'NA', geography[[2]]$type), ' corT1T2=', round(corT1T2, 2), ' meanAbsolCorT1F=', round(meanAbsolCorT1F, 2), ' mostCorT1F=', round(mostCorT1F, 2))
	
	# details <- paste0(details1, details2, details3)

	# say(details1, '\n', details2, '\n', details3, '\n')
	
	# suffix <- paste(algorithm, types, ifelse(univariate, 'univariate', 'multivariate'), round(runif(1) * 10^9))
	
	# # SDM
	# evalFrame <- main(
		# species=species,
		# landscape=landscape,
		# geography=geography,
		# directory=paste0(resultsDir, directory),
		# numTrainPres=numTrainPres,
		# numTestPres=numTestPres,
		# iterToDo=1,
		# jFoldMax=jFoldMax,
		# numBg=numBg,
		# algorithm=algorithm,
		# suffix=suffix,
		# verbose=Inf,
		# response=response,
		# plotResponse=FALSE,
		# cbi=TRUE,
		# interaction=FALSE,
		# mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho, b0=b0, b1=b1, b2=b2, b12=b12
	# )
	
	# if (class(evalFrame)!='try-error') {

		# # get results and remove file... perhaps vain attempt to avoid cases where computer is interruped and we get the first file but not the second
		# sdmResults <- readRDS(paste0(resultsDir, directory, '/Results - ', toupper(algorithm), ' - ', suffix, ' - Set 01 to 01.rds'))
		# file.remove(paste0(resultsDir, directory, '/Results - ', toupper(algorithm), ' - ', suffix, ' - Set 01 to 01.rds'))
		# sdmResults <- sdmResults[ , !grepl(names(sdmResults), pattern=algorithm)]
		
		# # omniscient model
		# evalFrame <- try(main(
			# species=species,
			# landscape=landscape,
			# geography=geography,
			# directory=paste0(resultsDir, directory),
			# numTrainPres=numTrainPres,
			# numTestPres=numTestPres,
			# iterToDo=1,
			# jFoldMax=jFoldMax,
			# numBg=numBg,
			# algorithm='omniscient',
			# suffix=suffix,
			# verbose=-1,
			# response=response,
			# plotResponse=FALSE,
			# cbi=TRUE,
			# interaction=FALSE,
			# mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho, b0=b0, b1=b1, b2=b2, b12=b12
		# ), silent=TRUE)

		# if (class(evalFrame)!='try-error') {		
			
			# # get results and remove file
			# omniResults <- readRDS(paste0(resultsDir, directory, '/Results - OMNISCIENT - ', suffix, ' - Set 01 to 01.rds'))
			# file.remove(paste0(resultsDir, directory, '/Results - OMNISCIENT - ', suffix, ' - Set 01 to 01.rds'))
			
			# # remember
			# results <- list()

			# results$algorithm <- algorithm
			# results$numTrainPres <- numTrainPres

			# results$response <- responseText
			# results$responseType <- ifelse(univariate, 'univariate', 'bivariate')
			# results$covarBetweenTrue <- covar
			# results$prevalence <- prevalence
			# results$borderness <- borderness
			
			# results$circle <- circle
			# results$missingVar <- missingVar
			# results$numLayers <- numLayer
			# results$layerTypeTrue1 <- geography[[1]]$type
			# results$layerTypeTrue2 <- ifelse(univariate, NA, geography[[2]]$type)
			# results$corT1T2 <- corT1T2
			# results$meanAbsolCorT1F <- meanAbsolCorT1F
			# results$mostCorT1F <- mostCorT1F
			# results$geography <- geography

			# results$results <- rbind(sdmResults, omniResults)

			# saveRDS(results, paste0(resultsDir, directory, '/Results - ', suffix, '.rds'))
			
			# say('completed!')
			
		# }
		
	# }
	
# }

# say('###################################################################################################################')
# say('### [overparameterized] RESPONSE gaussian(T1 T2) MODEL T1 T2 F1 F2 F3 GEOG cor(linear(T1, T2, F1, F2)) F3 F4 F5 ###')
# say('###################################################################################################################')

# directory <- 'C:/ecology/Drive/Research/ENMs - Predictor Inference/Results/[overparameterized] RESPONSE gaussian(T1 T2) MODEL T1 T2 F1 F2 F3 GEOG cor(linear(T1, T2, F1, F2)) F3 F4 F5'
# dir.create(directory, recursive=T, showWarnings=F)

# response <- gaussian

# rot <- c(22.5, 45, 67.5, 112.5, 135, 157.5)

# # rotate third landscape variable
# for (thisRot in rot) {

	# # for (rho in c(-0.5, 0, 0.5)) {
	# # for (rho in c(-0.5)) {
	# # for (rho in c(0)) {
	# for (rho in c(0.5)) {
	
		# # for (thisSigma2 in c(sigma1, sigma2)) {
		# for (thisSigma2 in c(sigma1)) {
		# # for (thisSigma2 in c(sigma2)) {
		
			# say('rot = ', thisRot, 'rho = ', rho, ' sigma2 =', thisSigma2)
		
			# geography <- list(
				# list(type='linear', min=min, max=max, pregen=TRUE),
				# list(type='linear', min=min, max=max, pregen=TRUE, rot=90),
				# list(type='linear', min=min, max=max, pregen=TRUE, rot=thisRot)
			# )

			# # create landscape
			# geography[[3]]$rot <- thisRot
			# landscape <- genesis(geography, circle=TRUE)
			# names(landscape) <- c('T1', 'T2', 'F1')
			
			# # generate species
			# species <- response(mu1=mu1, mu2=mu2, x1=subset(landscape, 1), x2=subset(landscape, 2), sigma1=sigma1, sigma2=thisSigma2, rho=rho)
			# say('Mean Pr(occ) = ', cellStats(species, 'mean'))

			# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=c(T, T, F), name=paste0('rot(F1)=', sub(as.character(thisRot), pattern='[.]', replacement='pt'), ' rho=', sub(as.character(rho), pattern='[.]', replacement='pt'), ', sigma2=', thisSigma2), bg=bg, fg=fg)
			
			# landscape <- subset(landscape, c(1, 3))
			# geography <- list(geography[[1]], geography[[3]])

			# # model!
			# main(
				# species=species,
				# landscape=landscape,
				# geography=geography,
				# directory=paste0(resultsDir, directory),
				# numTrainPres=numTrainPres,
				# numTestPres=numTestPres,
				# iterToDo=iterToDo,
				# jFoldMax=jFoldMax,
				# numBg=numBg,
				# algorithm=algorithm,
				# suffix=paste0('rot(F1)=', sub(as.character(thisRot), pattern='[.]', replacement='pt'), ' rho=', sub(as.character(rho), pattern='[.]', replacement='pt'), ' sigma2=', sigma2),
				# verbose=verbose,
				# response=response,
				# mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=thisSigma2, rho=rho
			# )
			
		# }
		
	# } # next rho
	
# } # next rotation of second landscape variable
























# say('#########################################################################################')
# say('### explore interaction between correlation in environment and correlation in species ###')
# say('#########################################################################################')

# rast2Cont <- function(r, levels=quantile(r, c(0.25, 0.5, 0.75))) {

	# xmin <- extent(r)@xmin
	# xmax <- extent(r)@xmax
	# ymin <- extent(r)@ymin
	# ymax <- extent(r)@ymax
	# rx <- seq(xmin, xmax, length.out=ncol(r))
	# ry <- seq(ymin, ymax, length.out=nrow(r))
	# rz <- t(as.matrix(r))
	# rz <- rz[ , ncol(rz):1] # reshape

	# ## get contour lines and convert to SpatialLinesDataFrame
	# # say('Converting to contour lines...')
	# cl <- contourLines(rx, ry, rz, levels=levels) 
	# cl <- ContourLines2SLDF(cl)
	# return(cl)

# }

# raster2contourPolys <- function(r, levels=NULL) {

	# library(rgeos)
	# library(maptools)

	# ## set-up levels
	# rmin <- cellStats(r, 'min')
	# rmax <- cellStats(r, 'max')

	# if (is.null(levels)) levels <- seq(rmin + 0.1 * (rmax - rmin), rmax - 0.1 * (rmax - rmin), length.out=5)
	
	# levels <- sort(levels)
	# plevels <- c(rmin, levels, rmax) # pad with raster range
	# llevels <- paste(plevels[-length(plevels)], plevels[-1], sep=' - ')  
	# llevels[1] <- paste('<', min(levels))
	# llevels[length(llevels)] <- paste(">", max(levels))

	# ## convert raster object to matrix so it can be fed into contourLines
	# xmin <- extent(r)@xmin
	# xmax <- extent(r)@xmax
	# ymin <- extent(r)@ymin
	# ymax <- extent(r)@ymax
	# rx <- seq(xmin, xmax, length.out=ncol(r))
	# ry <- seq(ymin, ymax, length.out=nrow(r))
	# rz <- t(as.matrix(r))
	# rz <- rz[ ,ncol(rz):1] # reshape

	# ## get contour lines and convert to SpatialLinesDataFrame
	# # cat("Converting to contour lines...\n")
	# cl <- contourLines(rx, ry, rz, levels=levels) 
	# cl <- ContourLines2SLDF(cl)

	# ## extract coordinates to generate overall boundary polygon
	# xy <- coordinates(r)[which(!is.na(values(r))),]
	# i <- chull(xy)
	# b <- xy[c(i,i[1]),]
	# b <- SpatialPolygons(list(Polygons(list(Polygon(b, hole = FALSE)), "1")))

	# ## add buffer around lines and cut boundary polygon
	# # cat("Converting contour lines to polygons...\n")
	# bcl <- gBuffer(cl, width = 0.0001) # add small buffer so it cuts bounding poly
	# cp <- gDifference(b, bcl)

	# ## restructure and make polygon number the ID
	# polys <- list() 
	# for(j in seq_along(cp@polygons[[1]]@Polygons)) {
	# polys[[j]] <- Polygons(list(cp@polygons[[1]]@Polygons[[j]]),j)
	# }
	# cp <- SpatialPolygons(polys)
	# cp <- SpatialPolygonsDataFrame(cp, data.frame(id=seq_along(cp)))

	# ## cut the raster by levels
	# rc <- cut(r, breaks=plevels)

	# ## loop through each polygon, create internal buffer, select points and define overlap with raster
	# # cat("Adding attributes to polygons...\n")
	# l <- character(length(cp))
	# for(j in seq_along(cp)) {
	# p <- cp[cp$id==j,] 
	# bp <- gBuffer(p, width = -max(res(r))) # use a negative buffer to obtain internal points
	# if(!is.null(bp)) {
	  # xy <- SpatialPoints(coordinates(bp@polygons[[1]]@Polygons[[1]]))[1]
	  # l[j] <- llevels[extract(rc,xy)]
	# } 
	# else { 
	  # xy <- coordinates(gCentroid(p)) # buffer will not be calculated for smaller polygons, so grab centroid
	  # l[j] <- llevels[extract(rc,xy)]
	# } 
	# }

	# ## assign level to each polygon
	# cp$level <- factor(l, levels=llevels)
	# cp$min <- plevels[-length(plevels)][cp$level]
	# cp$max <- plevels[-1][cp$level]  
	# cp <- cp[!is.na(cp$level),] # discard small polygons that did not capture a raster point
	# dataFrame <- unique(cp@dataFrame[,c("level","min","max")]) # to be used after holes are defined
	# dataFrame <- dataFrame[order(dataFrame$min),]
	# row.names(dataFrame) <- dataFrame$level
	# llevels <- dataFrame$level

	# ## define depressions in higher levels (ie holes)
	# # # cat("Defining holes...\n")
	# # spolys <- list()
	# # p <- cp[cp$level==llevels[1],] # add deepest layer
	# # p <- gUnaryUnion(p)
	# # spolys[[1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[1])
	# # for(i in seq(length(llevels)-1)) {
	# # p1 <- cp[cp$level==llevels[i+1],] # upper layer
	# # p2 <- cp[cp$level==llevels[i],] # lower layer
	# # x <- numeric(length(p2)) # grab one point from each of the deeper polygons
	# # y <- numeric(length(p2))
	# # id <- numeric(length(p2))
	# # for(j in seq_along(p2)) {
	  # # xy <- coordinates(p2@polygons[[j]]@Polygons[[1]])[1,]
	  # # x[j] <- xy[1]; y[j] <- xy[2]
	  # # id[j] <- as.numeric(p2@polygons[[j]]@ID)
	# # }
	# # xy <- SpatialPointsDataFrame(cbind(x,y), data.frame(id=id))
	# # holes <- over(xy, p1)$id
	# # holes <- xy$id[which(!is.na(holes))]
	# # if(length(holes)>0) {
	  # # p2 <- p2[p2$id %in% holes,] # keep the polygons over the shallower polygon
	  # # p1 <- gUnaryUnion(p1) # simplify each group of polygons
	  # # p2 <- gUnaryUnion(p2)
	  # # p <- gDifference(p1, p2) # cut holes in p1      
	# # } else { p <- gUnaryUnion(p1) }
	# # spolys[[i+1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[i+1]) # add level 
	# # }
	# cp <- SpatialPolygons(spolys, pO=seq_along(llevels), proj4string=CRS(proj4string(r))) # compile into final object
	# cp <- SpatialPolygonsDataFrame(cp, dataFrame)
	# cp

# }



# directory <- 'explore interaction between correlation in environment and correlation in species'
# dir.create(directory, showWarnings=F, recursive=T)

# geography <- list(
	# list(type='linear', min=min, max=max, pregen=TRUE),
	# list(type='linear', min=min, max=max, pregen=TRUE, rot=NA)
# )

# template <- raster('C:/ecology/Drive/Research/ENMs - Predictor Inference/Simulated Landscape Rasters/linearFromNeg1To1Rotation0.tif')
# sites <- xyFromCell(template, 1:ncell(template))

# # plot
# par(mfrow=c(1, 3), pty='s')

# for (rot in c(90)) {

	# geography[[2]]$rot <- rot
	# landscape <- genesis(geography, circle=TRUE)

	# for (rho in c(-0.5, 0, 0.5)) {
	
		# # get polygon depicting distribution of environment across landscape
		# env <- data.frame(T1=c(t(as.matrix(landscape[[1]]))), T2=c(t(as.matrix(landscape[[2]]))), species=c(t(as.matrix(species))))
		# names(env) <- c('T1', 'T2', 'species')
		# env <- env[-which(is.na(rowSums(env))), ]
		
		# envBin <- raster(hist2d(env$T1, env$T2, show=FALSE, nbins=201)$counts)
		# envCont <- rast2Cont(envBin, levels=c(0.1, 0.9))[1, ]
	
		# # make contour from species' raster and get environmental dataFrame there
		# species <- gaussian(x1=subset(landscape, 1), x2=subset(landscape, 2), rho=rho, sigma1=0.4, sigma2=0.2)
		# speciesCont <- rast2Cont(species, levels=c(0.1, 0.9))[1, ]
		# speciesCont <- SpatialPolygons(list(Polygons(list(Polygon(matrix(unlist(coordinates(speciesCont)), ncol=2, byrow=FALSE), hole=FALSE)), ID[1])))
		# projection(speciesCont) <- projection(landscape)
		# speciesEnv <- as.data.frame(extract(landscape, speciesCont))
		# names(speciesEnv) <- c('T1', 'T2')
		
		# bin <- raster(t(hist2d(speciesEnv, show=FALSE, nbins=201)$counts))
		# projection(bin) <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs' # Mollweide (equal area)

		# speciesCont <- rast2Cont(bin, levels=c(0.1, 0.9))[1, ]
		
		# OK!
		
		
		
		
		# # speciesCont <- SpatialPolygons(list(Polygons(list(Polygon(matrix(unlist(coordinates(speciesCont)), ncol=2, byrow=FALSE), hole=FALSE)), ID[1])))
		
		# plot(
		

		# # speciesCont <- rast2Cont(species, levels=c(0.1, 0.9))[1, ]
		# speciesCont <- raster2contourPolys(species, levels=c(0.1, 0.9))
		# speciesEnv <- as.data.frame(extract(landscape, speciesCont))
		# names(speciesEnv) <- c('T1', 'T2')

		# bin <- raster(t(hist2d(speciesEnv, show=FALSE)$counts))
		# projection(bin) <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs' # Mollweide (equal area)

		# envCont <- raster2contourPolys(bin, levels=c(0.1, 0.5))
		
		# plot(envCont)
		# plot(speciesCont, add=T)
		
	# }
	
# }



		# env <- data.frame(T1=c(t(as.matrix(landscape[[1]]))), T2=c(t(as.matrix(landscape[[2]]))), species=c(t(as.matrix(species))))
		# names(env) <- c('T1', 'T2', 'species')
		# env <- env[-which(is.na(rowSums(env))), ]
		
		# smoothScatter(env[ , c('T1', 'T2')], xlab='T1', ylab='T2', nrpoints=0, xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), main=paste0('rho=', rho), transformation=function(x) x^1)
		# points(env[which(env$species >= 0.5), c('T1', 'T2')], pch=16, col=alpha('red', 0.5))
		
	# }

# }	
		# contour(hist2d(env[ , c('T1', 'T2')], show=FALSE)$counts, levels=0.95)
		# kde <- kde2d(env$T1, env$T2, n=200)
		# envHist

	

	
	
	# bin <- data.frame(T1=seq(min, max, length.out=200), T2=seq(min, max, length.out=200), species=seq(0, 1, length.out=200))
	# bin$freqT1 <- hist(env$T1, breaks=c(bin$T1, 1), plot=FALSE)$counts
	# bin$freqT2 <- hist(env$T2, breaks=c(bin$T2, 1), plot=FALSE)$counts
	

	# smoothScatter(env[ , c('T1', 'T2')], xlab='T1', ylab='T2', nrpoints=0, xlim=c(-1.1, 1.1), ylim=c(-1.1, 1.1))
	# points(env[which(env$species >= 0.5), c('T1', 'T2')], pch=16, col=alpha('red', 0.5))


# smoothScatter(envUncor, xlab='T1', ylab='T2', nrpoints=0)















# print('###################################################################################')
# print('### [noise] RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2) ###')
# print('###################################################################################')

# directory <- 'C:/ecology/Drive/Research/ENMs - Predictor Inference/Results/[noise] RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2)'
# dir.create(directory, recursive=T, showWarnings=F)

# geography <- list(
	# list(type='linear', noisy=FALSE, noisep=0.1, min=min, max=max, pregen=TRUE),
	# list(type='linear', min=min, max=max, pregen=TRUE, rot=45)
# )


# # create landscape
# landscape <- genesis(geography, circle=TRUE)
# names(landscape) <- c('T1', 'T2')

# response <- gaussian

# for (rho in c(-0.5, 0, 0.5)) {

	# for (sigma2 in c(0.4, 0.1)) {
		
		# for (noisep in c(0, 0.1, 0.2, 0.4, 0.6, 0.8)) {
			
			# cat('noisep', noisep, 'rho', rho, 'sigma2', sigma2, '\n'); flush.console()

			# # generate species
			# species <- stretch(response(mu1=mu1, mu2=mu2, x1=subset(landscape, 1), x2=subset(landscape, 2), sigma1=sigma1, sigma2=sigma2, rho=rho), 0, 1)
			# cat('Mean Pr(occ) =', cellStats(species, 'mean'), '\n'); flush.console()
			
			# geography[[1]]$noisy <- TRUE
			# geography[[1]]$noisep <- noisep

			# noisyLandscape <- noisy(landscape, geography)
			# names(noisyLandscape) <- c('T1', 'T2')
			
			# plotGeog(landscape=noisyLandscape, species=species, directory=paste0(resultsDir, directory), inModel=rep(T, nlayers(landscape)), name=paste0('noisep=', noisep, ' rho=', rho, ' sigma2=', sigma2), bg=bg, fg=fg)
			
			# # model!
			# evalFrame <- main(
				# species=species,
				# landscape=noisyLandscape,
				# geography=geography,
				# directory=paste0(resultsDir, directory),
				# numTrainPres=numTrainPres,
				# numTestPres=numTestPres,
				# iterToDo=iterToDo,
				# jFoldMax=jFoldMax,
				# numBg=numBg,
				# algorithm=algorithm,
				# suffix=paste0('noisep=', noisep, ' rho=', rho, ' sigma2=', sigma2),
				# verbose=verbose
			# )
			
		# } # next noise
					
	# } # next sigma2

# } # next rho
	
# # get correlations between variables on landscape
# corFrame <- read.csv('C:/ecology/Drive/Research/ENMs - Predictor Inference/Correlations between Standard Environmental Variables.csv', as.is=T, row.names=1)

# for (thisAlgorithm in algorithm) {

	# if (exists('x')) rm(x)
	
	# for (thisRot in rot) {
	
		# for (rho in c(-0.5, 0, 0.5)) {

			# load(paste0(directory, '/Results - ', toupper(thisAlgorithm), ' - rot(T2)=', thisRot, ' rho=', rho, '.Rdata'))
			# evalFrame$rot <- thisRot
			# evalFrame$rho <- rho
			# x <- if (exists('x')) { rbind(x, evalFrame) } else { evalFrame }
		
		# }
	
	# }
	
	# x <- aggregate(x, by=list(x$rot, x$rho), mean, na.rm=T)
	# x$rot <- x$rho <- NULL
	# names(x)[1:2] <- c('rot', 'rho')
	
	# # add correlations between landscape variables to dataFrame frame
	# x$cor <- NA
	# for (i in 1:nrow(x)) x$cor[i] <- corFrame['linearFromNeg1To1Rotation0', paste0('linearFromNeg1To1Rotation', x$rot[i])]
	
	# ### plot
	# png(paste0('C:/ecology/Drive/Research/ENMs - Predictor Inference/Results/[T vs T] RESPONSE gaussian(T1 T2) MODEL T1 T2 GEOG cor(linear(T1) linear(T2))/results - ', thisAlgorithm, ' - sigma2=', sigma2, '.png'), width=14 * 200, height=12 * 200, res=600)
	
	# landcol <- c('white', 'dodgerblue4') # low/high
	# speciescol <- c('gray20', 'springgreen3') # low/high
	
	# cex.main <- 0.8
	# cex.lab <- 1
	# cex.axis <- 0.6
	
	# pointCexMult <- 5
	
	# layout <- matrix(
		# c(
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
			# 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2
		# ),
		# nrow=12, byrow=T
	# )
	
	# layout(layout)
	
	# par(bty='n', fg=fg, bg=bg, fig=getFig(layout, 1), new=FALSE, mai=rep(0.3, 4), tck=-0.025, mgp=c(1.25, 0.35, 0), oma=c(1, 1, 1, 1) * 0.1)
	# par(fig=getFig(layout, 1), new=FALSE)
	
	# # T1
	# plot(
		# x=x$rho,
		# y=x$cor,
		# xlim=c(-0.6, 0.6),
		# ylim=c(-1, 1),
		# xlab=expression(rho),
		# ylab='Correlation on landscape',
		# pch=1,
		# cex=x$corBgFullVsPerm_permT1 * pointCexMult,
		# col=col1,
		# col.lab=fg,
		# axes=F,
		# xpd=NA,
		# cex.lab=cex.lab,
		# cex.axis=cex.axis
	# )
	
	# # T2
	# points(
		# x=x$rho,
		# y=x$cor,
		# cex=x$corBgFullVsPerm_permT2 * pointCexMult,
		# col=col2,
		# xpd=NA
	# )

	# axis(1, labels=c(-0.5, 0, 0.5), at=c(-0.5, 0, 0.5), col.lab=fg, col.axis=fg, cex.lab=cex.lab, cex.axis=cex.axis)
	# axis(2, labels=c(-1, -0.5, 0, 0.5, 1), at=c(-1, -0.5, 0, 0.5, 1), col.lab=fg, col.axis=fg, cex.lab, cex.axis=cex.axis)
	
	# ## legend: correlation magnitude
	# points(
		# x=rep(-1.25, 4),
		# y=0.6 + c(0.3, 0.1, -0.1, -0.3),
		# col=fg,
		# cex=pointCexMult * c(1, 0.75, 0.5, 0.25),
		# xpd=NA,
		# pch=1
	# )
	
	# text(
		# x=rep(-1.25, 4),
		# y=0.6 + c(0.3, 0.1, -0.1, -0.3) - c(0.105, 0.085, 0.065, 0.05),
		# labels=c(1, 0.75, 0.5, 0.25),
		# cex=cex.axis,
		# xpd=NA,
		# col=fg
	# )

	# ## legend: variable
	# points(
		# x=rep(-1.25, 2),
		# y=c(-0.2, -0.4),
		# col=c(col1, col2),
		# cex=pointCexMult * c(0.5, 0.5),
		# xpd=NA,
		# pch=1
	# )
	
	# text(
		# x=rep(-1.25, 2),
		# y=c(-0.2, -0.4) - 0.085,
		# labels=c('T1', 'T2'),
		# cex=cex.axis,
		# xpd=NA,
		# col=c(col1, col2)
	# )

	# text(x=-1.25, y=1.025, labels='Correlation', col=fg, xpd=NA, cex=cex.axis)

	# text(x=-1.4, y=1.15, labels='a) Relative importance', xpd=NA, col=fg, cex=cex.main, adj=0)
	
	# ## plot species rasters
	
	# colSpp <- colorRampPalette(speciescol)
	# colSpp <- colSpp(100)

	# par(fig=getFig(layout, 2), new=TRUE)
	# axis(1, labels=c(-0.5, 0, 0.5), at=c(-0.5, 0, 0.5), col.lab=fg, col.axis=fg, cex.lab=cex.lab, cex.axis=cex.axis)
	# text(x=0, y=-1.225, labels=expression(rho), cex=cex.lab, col=fg, xpd=NA)

	# par(fig=getFig(layout, 2), new=TRUE)
	# axis(2, labels=c(-1, -0.5, 0, 0.5, 1), at=c(-1, -0.5, 0, 0.5, 1), col.lab=fg, col.axis=fg, cex.lab, cex.axis=cex.axis)
	# text(x=-0.875, y=0, srt=90, labels='Correlation on landscape', cex=cex.lab, col=fg, xpd=NA)

	# text(x=-0.9, y=1.15, labels='b) Species\' range', xpd=NA, col=fg, cex=cex.main, adj=0)

	# x1 <- raster('C:/ecology/Drive/Research/ENMs - Predictor Inference/Simulated Landscape Rasters/linearFromNeg1To1Rotation0.tif')
	# for (i in 1:nrow(x)) {

		# if (abs(abs(x$cor[i]) - 0.864771798) > 0.01) {
		
			# par(fig=getFig(layout, 2), new=TRUE)
			# x2 <- raster(paste0('C:/ecology/Drive/Research/ENMs - Predictor Inference/Simulated Landscape Rasters/linearFromNeg1To1Rotation', x$rot[i], '.tif'))
			# spp <- stretch(gaussian(mu1=mu1, mu2=mu2, x1=x1, x2=x2, sigma1=sigma1, sigma2=sigma2, rho=x$rho[i]), 0, 1)
		
			# if (x$rho[i]!=0.5) {
			
				# subplot(
					# fun=plot(
						# spp,
						# legend=FALSE,
						# axes=FALSE,
						# col=colSpp,
						# breaks=seq(0, 1, length.out=length(colSpp)),
						# maxpixels=500000
					# ),
					# x=x$rho[i] + ifelse(x$rho[i]==-0.5, 0.175, 0),
					# y=x$cor[i],
					# vadj=0.5, hadj=0.5,
					# size=c(0.3, 0.3)
				# )
				
			# } else {

				# subplot(
					# fun=plot(
						# spp,
						# legend=FALSE,
						# axes=FALSE,
						# col=colSpp,
						# breaks=seq(0, 1, length.out=length(colSpp)),
						# maxpixels=500000
					# ),
					# x=x$rho[i] - 0.175,
					# y=x$cor[i],
					# vadj=0.5, hadj=0.5,
					# size=c(0.3, 0.3)
				# )
			
			# }
			
		# }
	
	# }
	
	# dev.off()
	
# }	

# print('#####################################################################################################################################')
# print('### [overparam] RESPONSE gaussian(T1 T2) MODEL T1 T2 F1-F10 GEOG cor(linear(T1-T2) linear(F1-F2)) splitlinear(F3-F6) rand(F7-F10) ###')
# print('#####################################################################################################################################')

# directory <- 'C:/ecology/Drive/Research/ENMs - Predictor Inference/Results/[overparam] RESPONSE gaussian(T1 T2) MODEL T1 T2 F1-F10 GEOG cor(linear(T1-T2) linear(F1-F2)) splitlinear(F3-F6) rand(F7-F10)'
# dir.create(directory, recursive=T, showWarnings=F)

# geography <- list(
	# list(type='linear', min=min, max=max, pregen=TRUE),
	# list(type='linear', min=min, max=max, rot=90, pregen=TRUE),
	# list(type='linear', min=min, max=max, rot=45, pregen=TRUE),
	# list(type='linear', min=min, max=max, rot=120, pregen=TRUE),
	# list(type='linear', split=TRUE, min=min, max=max, rot=30, pregen=TRUE),
	# list(type='linear', split=TRUE, min=min, max=max, rot=60, pregen=TRUE),
	# list(type='linear', split=TRUE, min=min, max=max, rot=90, pregen=TRUE),
	# list(type='linear', split=TRUE, min=min, max=max, rot=120, pregen=TRUE),
	# list(type='random', min=min, max=max, pregen=FALSE),
	# list(type='random', min=min, max=max, pregen=FALSE),
	# list(type='random', min=min, max=max, pregen=FALSE),
	# list(type='random', min=min, max=max, pregen=FALSE)
# )
	
# landscape <- genesis(geography, circle=TRUE)
# names(landscape) <- c('T1', 'T2', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10')
	
# response <- gaussian

# for (rho in c(-0.5, 0, 0.5)) {
	
	# cat('RESPONSE', paste(deparse(response), collapse=' '), '\n')
	# cat('GEOGRAPHY', paste(unlist(geography), collapse=' '), '\n')
	# cat('RHO', rho, '\n'); flush.console()

	# # generate species
	# species <- stretch(response(mu1=mu1, mu2=mu2, x1=subset(landscape, 1), x2=subset(landscape, 2), sigma1=sigma1, sigma2=sigma2, rho=rho), 0, 1)
	# cat('Mean Pr(occ) =', cellStats(species, 'mean'), '\n'); flush.console()
	
	# plotGeog(landscape=landscape, species=species, directory=paste0(resultsDir, directory), inModel=rep(TRUE, 12), name=paste0('rho=', rho), bg=bg, fg=fg, legend=FALSE)

	# # # model!
	# # evalFrame <- main(
		# # species=species,
		# # landscape=landscape,
		# # geography=geography,
		# # directory=paste0(resultsDir, directory),
		# # numTrainPres=numTrainPres,
		# # numTestPres=numTestPres,
		# # iterToDo=iterToDo,
		# # jFoldMax=jFoldMax,
		# # numBg=numBg,
		# # algorithm=algorithm,
		# # suffix=paste0('rho=', rho),
		# # verbose=verbose
	# # )
		
# } # next rho

# rho <- c(-0.5, 0, 0.5)
# for (thisAlgorithm in algorithm) {

	# x <- list()
	# for (i in 1:3) {

		# load(paste0(directory, '/Results - ', toupper(thisAlgorithm), ' - rho=', rho[i], '.Rdata'))
		
		# x[[i]] <- evalFrame

	# }

	# # standard panel plots, one per rho
	# plotPanels(x=x, directory=paste0(resultsDir, directory), suffix=toupper(thisAlgorithm), main=paste(toupper(thisAlgorithm), paste('rho =', rho)), var=c('T1', 'T2', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10'), metric='cor', contrast='bg', bg=bg, fg=fg, perm=TRUE, reduced=FALSE, univ=FALSE)

	# plotPanels(x=x, directory=paste0(resultsDir, directory), suffix=toupper(thisAlgorithm), main=paste(toupper(thisAlgorithm), paste('rho =', rho)), var=c('T1', 'T2', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10'), metric='cor', contrast='bg', bg=bg, fg=fg, perm=FALSE, reduced=FALSE, univ=TRUE)

	# plotPanels(x=x, directory=paste0(resultsDir, directory), suffix=toupper(thisAlgorithm), main=paste(toupper(thisAlgorithm), paste('rho =', rho)), var=c('T1', 'T2', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10'), metric='cor', contrast='bg', bg=bg, fg=fg, perm=FALSE, reduced=TRUE, univ=FALSE)
	
# }




































































# print('###################################################')
# print('### testing method using Maxent standard output ###')
# print('###################################################')

# directory <- 'C:/ecology/Drive/Research/ENMs - Predictor Inference/Results/RESPONSE gaussian-bivariate GEOG linear linear with correlation plus random=runif'
# dir.create(directory, recursive=T, showWarnings=F)

# response <- list(type='univariate linear', fun=function(x, a=1) return(exp(a * x) / (1 + exp(a * x))))
# geography <- list(list(type='linear', min=min, max=max), list(type='random', min=min, max=max))

# cat ('RESPONSE', paste(deparse(response), collapse=' '), 'GEOGRAPHY', paste(geography, collapse=' '), '\n'); flush.console()

# # create landscape
# landscape <- genesis(geography, circle=TRUE)
# names(landscape) <- c('T1', 'F1')

# # generate species
# x <- subset(landscape, 1)
# species <- response(subset(landscape, 1), a=1)

# # generate training/test and background sites
# presAbs <- -Inf # initial sum of sampled presences
# n <- round(1.1 * (numBg + 2 * numTrainPres + numTestPres)) # initial number of randomly located sites to draw

# while (sum(presAbs) < numTrainPres + numTestPres) {
	
	# sites <- as.data.frame(randomPointsRobust(species, n, prob=F))
	# prOcc <- extract(species, sites)
	# presAbs <- runif(length(sites)) <= prOcc
	
	# n <- round(n * 1.1)
	
# }

# # training/test presences and background sites
# allPres <- sites[presAbs, ]
# trainPres <- allPres[1:numTrainPres, ]
# testPres <- allPres[(numTrainPres + 1):(numTrainPres + numTestPres), ]
# testAbs <- sites[which(!presAbs), ]
# testAbs <- testAbs[sample(rownames(testAbs), numTestPres), ]

# trainPresEnv <- as.data.frame(extract(landscape, trainPres))
# testPresEnv <- as.data.frame(extract(landscape, testPres))
# testAbsEnv <- as.data.frame(extract(landscape, testAbs))

# bgSites <- randomPointsRobust(landscape, numBg, prob=F)
# bgEnvTrain <- as.data.frame(extract(landscape, bgSites))

# png(paste0(directory, '/species.png'), width=1000, height=1000)
# plot(species)
# points(trainPres, pch=16, cex=1)
# points(testPres, pch=16, cex=1, col='blue')
# points(testAbs, pch=16, cex=1, col='red')
# dev.off()

# model <- maxent(
	# x=rbind(trainPresEnv, bgEnvTrain),
	# p=c(rep(1, nrow(trainPresEnv)), rep(0, nrow(bgEnvTrain))),
	# removeDuplicates=FALSE,
	# path=paste0(directory, '/Maxent Trial'),
	# args=c(
		# paste0('betamultiplier=', 1),
		# 'linear=true',
		# 'quadratic=true',
		# 'product=true',
		# 'threshold=true',
		# 'hinge=true',
		# 'responsecurves=false',
		# 'jackknife=false',
		# 'askoverwrite=false',
		# 'removeduplicates=false',
		# 'addsamplestobackground=false',
		# 'responsecurves=true',
		# 'jackknife=true',
		# 'threads=4'
	# )
# )


say('###############################################################################################')
say('###############################################################################################')
say('### END END END END END END END END END END END END END END END END END END END END END END ###')
say('###############################################################################################')
say('###############################################################################################')

say(date())


