########################################################
### MASTER function to create training and test data ###
########################################################
mainMakeData <- function(
	response,
	species,
	geography,
	outDir,
	numTrainPres=200,
	numTestPres=200,
	numBg=10000,
	iters=1:100,
	circle=FALSE,
	size=1001,
	fileAppend=NULL,
	b0=NA, b1=NA, b2=NA, b11=NA, b12=NA, mu1=NA, sigma1=NA, sigma2=NA, rho=NA,
	overwrite=FALSE,
	verbose=1,
	...
) {
	# speciesMap		raster with probability of speciesMap' presence
	# geography		list object with description of geography
	# outDir		outDir in which to save results
	# numTrainPres	number of training presences to use in model
	# numTestPres	number of test presences to use for model evaluation and predictor inference
	# numBg			number of background sites for model *training*
	# iters			integer list of simulation iterations to perform (e.g., 1:100)--each will have a different set of presences and background sites
	# circle		logical, if TRUE then force landsape to be a circle (versus square)
	# size			integer, number of cells along each side of the landscape (must be an odd number)
	# fileAppend	character string to add to file name file names... leave as NULL to ignore
	# overwrite		logical, if TRUE then overwrite existing sim objects
	# verbose		<=0: minimal display of progress; 1: some progress; 2: detailed progress; >=3: very detailed progress
	# ...			other arguments (not used)

	if (verbose >= 0) say('Creating simulation data for ', max(iters), ' simulations:', post=0)

	# file prefix
	fileAppendStartSpace <- if (!is.null(fileAppend)) { paste0(' ', fileAppend) } else { '' }
	fileAppendEndSpace <- if (!is.null(fileAppend)) { paste0(fileAppend, ' ') } else { '' }

	# flag to indicate if any data creation iterations were skipped
	skippedAny <- FALSE
	
	# for each simulation sample landscape, train model, and evaluate
	for (iter in iters) {

		# DO NOT re-create data
		if (!overwrite && file.exists(paste0(outDir, '/!scenario data/', fileAppendEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))) {
			
			say(iter, '\U2713', post=0)
			skippedAny <- TRUE
			
		# RE-CREATE DATA
		} else {
		
			if (verbose > 0) say(iter, post=0)
	
			### generate new random layers if needed
			########################################
			
			seed <- as.numeric(Sys.time())
			set.seed(seed)

			# make landscape
			if (iter == 1 | skippedAny) {

				landscape <<- genesis(geography, circle=circle, size=size, verbose=verbose > 1)
				speciesMap <- eval(parse(text=as.character(species)), envir=1)

				dirCreate(outDir, '/maps')
				png(paste0(outDir, '/maps/map', fileAppendStartSpace, '.png'), width=1200, height=1200)
					names(speciesMap) <- 'species'
					plot(stack(speciesMap, landscape))
				dev.off()

			# re-make any random layers for next sim
			} else if (any(unlist(geography) %in% 'random')) {
				landscape <<- genesis(geography, circle=circle, size=size)
				speciesMap <- eval(parse(text=as.character(species)), envir=1)
			} else if (!exists('speciesMap', inherits=FALSE)) {
				speciesMap <- eval(parse(text=as.character(species)), envir=1)
			}

			### generate training/test and background sites
			###############################################

			# create mask from which to sample (avoids inclusion of sites with NAs)
			mask <- sum(landscape)

			# estimate sample size needed to get sufficient presences and absences
			n <- round(1.5 * (numTrainPres + numTestPres)) # initial number of randomly located sites to draw
			prev <- cellStats(speciesMap, 'sum')  / ncell(speciesMap) # prevalence (including NA cells)
			
			while (n * prev < numTrainPres + numTestPres | n * (1 - prev) < numTestPres) { n <- round(n * 1.5) }

			# draw sites
			presAbs <- -Inf # initial sum of sampled presences
			while (sum(presAbs) < numTrainPres + numTestPres | sum(!presAbs) < numTestPres) {

				sites <- sampleRast(x=mask, n=n, adjArea=FALSE, replace=TRUE, prob=FALSE)
				prOcc <- extract(speciesMap, sites)
				presAbs <- runif(nrow(sites)) <= prOcc
				
				n <- 1.5 * n
				
			}

			# training/test presences and absences
			allPresSites <- sites[which(presAbs), ]
			trainPresSites <- allPresSites[1:numTrainPres, ]
			testPresSites <- allPresSites[(numTrainPres + 1):(numTrainPres + numTestPres), ]
			testAbsSites <- sites[which(!presAbs), ]
			testAbsSites <- testAbsSites[sample(1:nrow(testAbsSites), numTestPres), ]

			trainPres <- as.data.frame(extract(landscape, trainPresSites))
			testPres <- as.data.frame(extract(landscape, testPresSites))
			testAbs <- as.data.frame(extract(landscape, testAbsSites))

			# trainint/test random background sites
			bgSitesTrain <- sampleRast(x=mask, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			bgEnvTrain <- as.data.frame(extract(landscape, bgSitesTrain))

			# compile training data
			presBg <- data.frame(presBg=c(rep(1, nrow(trainPres)), rep(0, nrow(bgEnvTrain))))
			trainData <- cbind(presBg, rbind(trainPres, bgEnvTrain))
			
			# compile test data
			testBgSites <- sampleRast(x=mask, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			testBg <- as.data.frame(extract(landscape, testBgSites))
			
			# prevalance
			prev <- cellStats(speciesMap, 'mean')
			
			# remember
			sim <- list()
			sim$iter <- iter
			sim$seed <- seed
			
			sim$stats <- data.frame(numTrainPres=numTrainPres, numTestPres=numTestPres, numBg=numBg, prev=prev, circle=circle, landscapeSize=nrow(landscape), b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
			sim$response <- response
			sim$vars <- names(landscape)
			sim$geography <- geography
			sim$trainData <- trainData
			sim$testData <- list()
			sim$testData$testPres <- testPres
			sim$testData$testAbs <- testAbs
			sim$testData$testBg <- testBg
			
			class(sim) <- c('sim', class(sim))
			
			dirCreate(outDir, '/!scenario data')
			save(sim, file=paste0(outDir, '/!scenario data/', fileAppendEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))
			gc()
			
		} # re-create data?
		
	} # next simulation
	
	say('')
	
}
