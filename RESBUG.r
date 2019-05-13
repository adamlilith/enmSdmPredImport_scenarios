say('################')
say('### [resbug] ###')
say('################')

	thisOutDir <- 'resbug'
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
	# sizesSampled <- 2^c(6, 8, 10, 12, 14) # grain at which environmental data is available
	sizesSampled <- 2^10 # grain at which environmental data is available
	
	iters <- 1:50
	
	# manipulate SAC
	# noise <- c(0, 1/3, 2/3, 1)
	noise <- c(0)

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
			simDir=paste0(scenarioDir, '/resample'),
			numTrainPres=200,
			numTestPres=200,
			numBg=10000,
			iters=iters,
			circle=FALSE,
			sizeNative=sizeNative,
			sizeResampled=sizeResampled,
			overwrite=TRUE,
			fileFlag=fileFlag,
			userdata=userdata,
			b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			verbose=verbose
		)

		predImportMakeData(
			response=response,
			geography=geography,
			simDir=paste0(scenarioDir, '/no resample'),
			numTrainPres=200,
			numTestPres=200,
			numBg=10000,
			iters=iters,
			circle=FALSE,
			sizeNative=sizeNative,
			sizeResampled=NULL,
			overwrite=TRUE,
			fileFlag=fileFlag,
			userdata=userdata,
			b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho,
			verbose=verbose
		)

		# indicate this set complete and save
		write.csv(progress$string[doing], paste0(scenarioDir, '/!make data - stops/', progress$string[doing]), row.names=FALSE)
		started <- list.files(paste0(scenarioDir, '/!make data - starts'))
			
	} # next job

	for (i in iters) {
		png(paste0('C:/ecology/!Scratch/F1 new ', prefix(i, 3), '.png'))
		par(mfrow=c(1, 2))
		load(paste0(scenarioDir, '/resample/sizeResampled=1024 noise=0 sim ', prefix(i, 3), '.Rdata'))
		hist(sim$trainData$F1[sim$trainData$presBg == 1], main='resampled')
		
		load(paste0(scenarioDir, '/no resample/sizeResampled=1024 noise=0 sim ', prefix(i, 3), '.Rdata'))
		hist(sim$trainData$F1[sim$trainData$presBg == 1], main='no sampled')
		dev.off()
	
	}