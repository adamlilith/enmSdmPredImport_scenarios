
say('################')
say('### [simpleBUG] ###')
say('################')

	setwd('C:/ecology/!Scratch')

	verbose <- 1 # some display -- best for most scenarios
	# verbose <- 2 # much display
	# verbose <- Inf # all display
	debug <- FALSE; modelType <- 'does not matter' # for running code
	# debug <- TRUE; modelType <- 'logistic' # for debugging using logistic response
	# debug <- TRUE; modelType <- 'gaussian' # for debugging using Gaussian response

	# ## iterations
	iters <- 1:20 # iterations to do -- want 100 total
	algos <- c('omniscient')
	
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


	thisOutDir <- 'simpleBUG'
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
	
	# ls <- genesis(geography, circle=FALSE)
	
	# t1sum <- f1sum <- rep(NA, nrow(ls))
	# t1 <- as.matrix(ls[[1]])
	# f1 <- as.matrix(ls[[2]])
	# for (i in 1:nrow(ls)) {
	
		# t1sum[i] <- sum(t1[nrow(ls) - i + 1, ])
		# f1sum[i] <- sum(f1[nrow(ls) - i + 1, ])
		
	# }
	
	# par(mfrow=c(2, 2))
	# plot(t1sum)
	# plot(f1sum)
	# plot(cumsum(t1sum))
	# plot(cumsum(f1sum))

	# summary(lm(f1sum ~ seq_along(f1sum)))
	
	# mean(t1)
	# mean(f1)
	
ff <- listFiles('C:/ecology/Drive/R/enmSdm/R')
for (f in ff) source(f)	
	
ff <- listFiles('C:/ecology/Drive/R/enmSdmPredImport/R')
for (f in ff) source(f)	
	
	# create data
	predImportMakeData(
		response=response,
		geography=geography,
		simDir=paste0(scenarioDir, '/sims'),
		numTrainPres=200,
		numTestPres=200,
		numBg=10000,
		iters=iters,
		sizeNative=1024,
		overwrite=TRUE,
		fileFlag=NULL,
		b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
		verbose=verbose,
		circle=FALSE
	)

	coeff <- numeric()
	for (i in 1:20) {
	
		load(paste0('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Results/simple/!scenario data/sim ', prefix(i, 4), '.RData'))

		png(paste0('C:/Ecology/!Scratch/traintest ', prefix(i, 4), '.png'), width=1500, height=1000)
			
			par(mfrow=c(2, 3))
			hist(sim$trainDataPresBg$T1[sim$trainDataPresBg$presBg==1], xlim=c(-1, 1), main='T1 @ pres', breaks=20)
			hist(sim$trainDataPresBg$T1[sim$trainDataPresBg$presBg==0], xlim=c(-1, 1), main='T1 @ bg', breaks=20)
			hist(sim$testData$testPres$T1, xlim=c(-1, 1), main='T1 @ pres', breaks=20)

			hist(sim$trainDataPresBg$F1[sim$trainDataPresBg$presBg==1], xlim=c(-1, 1), main='F1 @ pres', breaks=20)
			hist(sim$trainDataPresBg$F1[sim$trainDataPresBg$presBg==0], xlim=c(-1, 1), main='F1 @ bg', breaks=20)
			hist(sim$testData$testPres$F1, xlim=c(-1, 1), main='F1 @ pres', breaks=20)
		
		dev.off()
	
		index <- 1:200
		l <- lm(sim$trainDataPresBg$F1[sim$trainDataPresBg$presBg==1] ~ index)
		coeff <- c(coeff, coefficients(l)[2])
	
	
	}

	# for (i in 1:20) {
	
		# say('------------------- ', i, pre=2)
		# load(paste0('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Results/simple/multivariate maxent/maxent model ', prefix(i, 4), '.RData'))
		# print(model@lambdas)
		
	# }
	
	