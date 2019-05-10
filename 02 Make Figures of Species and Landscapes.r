### SDM PREDICTOR INFERENCE - ILLUSTRATIONS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
### source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/02 Make Figures of Species and Landscapes.r')

	memory.limit(memory.limit() * 2^30)
	rm(list=ls())
	options(keep.source=FALSE) # manage memory
	gc()
	print('')
	print(date())

### CONTENTS ###
### libraries ###
### variables and settings ###
### case-specific functions ###

### [simple] landscape and species ###
### [extent] landscape and species ###
### [prevalence] landscape and species ###
### [resolution] landscape and species ###

### [correlated] calculate correlation between variables as a function of their rotation ###
### [correlated TRUE & FALSE] landscape and species ###

### [bivariate] landscape and species: NO niche covariance, YES landscape correlation ###
### [bivariate] landscape and species: YES niche covariance, NO landscape correlation ###
### [bivariate] collate evaluations ###

### [simple] simulation results ###
### [extent] simulation results ###
### [prevalence] simulation results ###
### [resolution] simulation results ###
### [correlated TRUE & FALSE] simulation results ###
### [bivariate] statistics ###
### [bivariate] landscape correlation x niche covariance annulus plots ###
### [bivariate] landscape correlation x niche covariance bar plots for CBI ###

#################
### libraries ###
#################

	library(compiler)
	library(sp)
	library(rgdal)
	library(raster)
	library(RColorBrewer)
	library(rgeos)
	library(rJava)
	options(java.parameters='-Xmx1g' )
	library(dismo)
	library(plotrix)
	library(fpCompare)
	library(scales)
	library(omnibus)
	library(enmSdm)
	library(enmSdmPredImport)
	library(legendary)

##############################
### variables and settings ###
##############################

	### working directory
	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')

	# algorithms

	# # set 1
	algos <- c('omniscient', 'gam', 'maxent', 'brt')
	sdmAlgos <- c('gam', 'maxent', 'brt')

	# # set 2
	# algos <- c('omniscient', 'bioclim', 'glm', 'rf')
	# sdmAlgos <- c('bioclim', 'glm', 'rf')
	
	allAlgos <- c('omniscient', 'bioclim', 'gam', 'glm', 'maxent', 'brt', 'rf')
	
	### landscape and species colors

	# species
	greens <- colorRampPalette(c('white', 'darkgreen'))
	greens <- greens(101)

	# landscape
	land <- colorRampPalette(c('#543005', '#bf812d', '#f5f5f5', '#35978f', '#003c30'))
	land <- land(101)
	
	
###############################
### case-specific functions ###
###############################
	
	# create abbreviated function names
	algosShort <- function(x) {
	
		# x character vector
		x[x == 'omniscient'] <- 'OMNI'
		x[x == 'bioclim'] <- 'BIO'
		x[x == 'brt'] <- 'BRT'
		x[x == 'gam'] <- 'GAM'
		x[x == 'maxent'] <- 'MAX'
		x[x == 'rf'] <- 'RF'
		x[x == 'glm'] <- 'GLM'
		
		x
		
	}
	
# say('######################################')
# say('### [simple] landscape and species ###')
# say('######################################')
	
	# say('Wanting a simple illustration of the landscape and species in a "simple" scenario.')
	
	# thisOutDir <- 'simple'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)

	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))
	# landscape <- genesis(geography, circle=FALSE)
	
	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	# species <- logistic(x1=landscape[['T1']], x2=landscape[['F1']], b0=b0, b1=b1, b11=b11, b12=b12)

	# # extent (for border)
	# ext <- extent(landscape)
	# ext <- as(ext, 'SpatialPolygons')

	# png(paste0(scenarioDir, '/Illustration - SIMPLE Scenario Landscape and Species.png'), width=1200, height=600, res=300)
	
		# par(mfrow=c(1, 3), oma=rep(0, 4), mar=c(1, 0, 2, 0), fg='white', col.axis='white')

		# plot(ext)
		# plot(landscape[['T1']], breaks=seq(-1, 1, length.out=length(land) - 1), col=land, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('a) TRUE variable', adj=c(0, -0.05), cex=0.9, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=land, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
		
		# par(fg='white')
		# plot(ext)
		# plot(landscape[['F1']], breaks=seq(-1, 1, length.out=length(land) - 1), col=land, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('b) FALSE variable', adj=c(0, -0.05), cex=0.9, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=land, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
		
		# par(fg='white')
		# plot(ext)
		# plot(species, breaks=seq(0, 1, length.out=length(land) - 1), col=greens, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('c) Species', adj=c(0, -0.05), cex=0.9, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(0, 0.5, 1), title='', col=greens, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
	
	# dev.off()
	
# say('######################################')
# say('### [extent] landscape and species ###')
# say('######################################')
	
	# say('Wanting a simple illustration of the landscape and species in the "extent" scenario.')
	
	# thisOutDir <- 'extent'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA

	# landSize <- data.frame(landSize=c(125, 251, 501, 1001, 2001, 4001, 8001), min=-1 * c(0.125, 0.25, 0.5, 1, 2, 4, 8), max=c(0.125, 0.25, 0.5, 1, 2, 4, 8))
	
	# breaks <- seq(min(landSize$min), max(landSize$max), length.out=length(land) - 1)
	
	# png(paste0(scenarioDir, '/Illustration - EXTENT Scenario Landscape and Species.png'), width=600 * 2, height=660, res=300)
		
		# # define largest landscape
		# geography <- list(T1=list(type='linear', min=landSize$min[nrow(landSize)], max=landSize$max[nrow(landSize)]), F1=list(type='random', min=-1, max=1))

		# landscape <- genesis(geography, size=landSize$landSize[nrow(landSize)], circle=FALSE)
		# species <- logisticShift(x1=landscape[['T1']], x2=landscape[['F1']], b0=b0, b1=b1, b11=b11, b12=b12)
		
		# ext <- extent(landscape)
		# ext <- as(ext, 'SpatialPolygons')
		
		# par(mfrow=c(1, 2), oma=c(1, 0.1, 1, 0.1), lwd=0.8)
		
		# # plot largest landscape
		# par(mar=c(0, 0, 0, 1))
		# plot(ext)
		# plot(landscape[['T1']], breaks=breaks, col=land, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# lab <- paste0('a) TRUE variable')
		# labelFig(lab, adj=c(-0.02, -0), cex=0.5, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.04, vert=FALSE, width=0.93, height=0.07, labels=seq(min(landSize$min), max(landSize$max), by=4), title='', col=land, labAdj=-1, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.4)

		# thisExt <- ext
		# for (countLand in 2:nrow(landSize)) {
		
			# length <- extent(thisExt)@xmax - extent(thisExt)@xmin
			# thisExt <- gBuffer(thisExt, width=-1 * length / 4)
			# plot(thisExt, add=TRUE)
			
		# }
		
		# # species
		# par(mar=c(0, 1, 0, 0))
		# par(fg='white')
		# plot(ext)
		# plot(species, breaks=seq(0, 1, length.out=length(greens) - 1), col=greens, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, add=TRUE)
		# lab <- paste0('b) Species')
		# labelFig(lab, adj=c(-0.02, -0), cex=0.5, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.04, vert=FALSE, width=0.93, height=0.07, labels=seq(0, 1, by=0.25), title='', col=greens, labAdj=-1, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.4)
	
		# thisExt <- ext
		# for (countLand in 2:nrow(landSize)) {
		
			# length <- extent(thisExt)@xmax - extent(thisExt)@xmin
			# thisExt <- gBuffer(thisExt, width=-1 * length / 4)
			# plot(thisExt, add=TRUE)
			
		# }

		# # labels: largest landscape
		# x <- -0.16
		# y <- 0.95
		# down <- 0.07
		# text(x, y, labels='largest\nlandscape', xpd=NA, cex=0.5)
		# arrows(x0=x, y0=y - down, x1=0, y1=y - down - 0.1, angle=15, length=0.075, xpd=NA, lwd=0.5)
		# arrows(x0=x, y0=y - down, x1=2 * x, y1=y - down - 0.1, angle=15, length=0.075, xpd=NA, lwd=0.5)

		# # labels: next largest landscape
		# x <- -0.16
		# y <- 0.65
		# down <- 0.1
		# text(x, y, labels='next\nlargest\nlandscape', xpd=NA, cex=0.5)
		# arrows(x0=x, y0=y - down, x1=0.25, y1=y - down - 0.1, angle=15, length=0.075, xpd=NA, lwd=0.5)
		# arrows(x0=x, y0=y - down, x1=-0.575, y1=y - down - 0.1, angle=15, length=0.075, xpd=NA, lwd=0.5)
		
		# # labels: smallest landscape
		# x <- -0.16
		# y <- 0.1
		# down <- -0.06
		# text(x, y, labels='smallest\nlandscape', xpd=NA, cex=0.5)
		# arrows(x0=x, y0=y - down, x1=0.49, y1=0.49, angle=15, length=0.075, xpd=NA, lwd=0.5)
		# arrows(x0=x, y0=y - down, x1=-0.82, y1=0.49, angle=15, length=0.075, xpd=NA, lwd=0.5)
		
		# title(sub=date(), cex.sub=0.2, outer=TRUE, line=0)
		
	# dev.off()

# say('##########################################')
# say('### [prevalence] landscape and species ###')		
# say('##########################################')		

	# say('Wanting a simple illustration of the landscape and species in the "prevalence" scenario.')
	
	# thisOutDir <- 'prevalence'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)

	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))
	# landscape <- genesis(geography, circle=FALSE)
	
	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	# response <- logisticShift
	
	# # test each inflection point
	# # inflection points chosen to match prevalence of 0.95, 0.85, 0.75, 0.625, 0.5, 0.375, 0.25, 0.15, 0.05 as closely as possible
	# # b11Set <- c(-1.74, -1.08, -0.7, -0.33, 0, 0.33, 0.7, 1.08, 1.74)
	# b11Set <- rev(c(-1.74, -0.7, 0, 0.7, 1.74)) # subsetted

	# # extent (for border)
	# ext <- extent(landscape)
	# ext <- as(ext, 'SpatialPolygons')

	# png(paste0(scenarioDir, '/Illustration - PREVALENCE Scenario Landscape and Species.png'), width=5 * 400, height=600, res=300)
	
		# par(mfrow=c(1, 5), oma=c(0, 0, 0, 3.1), mar=c(0, 0, 2, 0), fg='white', col.axis='white')

		# for (countB11 in seq_along(b11Set)) {
		
			# thisB11 <- b11Set[countB11]
		
			# species <- response(x1=landscape[['T1']], x2=landscape[['F1']], b0=b0, b1=b1, b11=thisB11, b12=b12)
			# prev <- cellStats(species, 'mean')
		
			# par(fg='white')
			# plot(ext)
			# plot(species, breaks=seq(0, 1, length.out=length(greens) - 1), col=greens, ann=FALSE, legend=FALSE, add=TRUE)
			# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
			# lab <- paste0(letters[countB11], ') Prevalence of ', sprintf('%.2f', prev))
			# labelFig(lab, adj=c(-0.02, -0.1), cex=0.9, col='black')
		
		# }
	
		# par(fg='black')
		# legendGrad('right', inset=-0.2, width=0.1, height=0.75, labels=c(0, 0.5, 1), title='Occurrence\nProbability', titleAdj=c(0.5, 0.95), col=greens, labAdj=0.7, xpd=NA, adjX=c(0, 0.5), adjY=c(0.055, 0.75), boxBorder=NA, cex=0.75)
		
	# dev.off()
	
# say('##########################################')
# say('### [resolution] landscape and species ###')
# say('##########################################')

	# say('Wanting a simple illustration of the landscape and species in the "resolution" scenario.')
	# say('Setup: 2 panels, one for TRUE, one for FALSE.')
	# say('Each panel: 3 columns for grain size, 4 rows for spatial autocorrelation.')
	
	# thisOutDir <- 'resolution'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)

	# # define landscape
	# geography <- list(T1=list(type='linear', min=-1, max=1, noise=0), F1=list(type='random', min=-1, max=1))
	
	# landscapeNative <- genesis(geography, size=1024, circle=FALSE)
	
	# # extent (for border)
	# ext <- extent(landscapeNative)
	# ext <- as(ext, 'SpatialPolygons')

	# # plots one layer of landscape
	# subplotLand <- function(grain, var, noise, main=' ', ylab=' ') {
			
		# # grain		grain size
		# # var		which variable to plot (T1 or F1)
		# # noise		noise level
		# # main		title or ' '
		# # ylab		ylab or ' '
			
		# thisNative <- landscapeNative
		# thisGeog <- geography
		# thisGeog[[var]]$noise <- noise
		# thisNative <- noisy(thisNative, thisGeog)
		# templateSampled <- raster(nrows=grain, ncols=grain, crs=raster::projection(landscapeNative), ext=extent(landscapeNative))
		# x <- resample(thisNative[[var]], templateSampled)

		# plot(ext)
		# mtext(main, side=3, xpd=NA, col='black', cex=0.65, line=-0.5)
		# mtext(ylab, side=2, xpd=NA, col='black', cex=0.75)
		# plot(x, breaks=seq(-1, 1, length.out=length(land) - 1), col=land, ann=FALSE, legend=FALSE, maxpixels=ncell(x), add=TRUE)
		# plot(ext, border='black', col=NA, xpd=NA, ann=FALSE, add=TRUE)
		
	# }

	# finest <- 2^14
	# native <- 2^10
	# coarsest <- 2^6
	
	# png(paste0(scenarioDir, '/Illustration - RESOLUTION Scenario Landscape and Species NEW.png'), width=2 * 3 * 380, height=4 * 400, res=300)
	
		# par(mfrow=c(4, 6), oma=c(1, 4.1, 4.1, 0), mar=c(0, 0, 0, 0), fg='white', col.axis='white', cex.main=0.8)

		# noises <- c(0, 1/3, 2/3, 1)
			
		# for (countNoise in seq_along(noises)) {
			
			# noise <- noises[countNoise]

			# if (countNoise == 1) {
				# main1 <- 'TRUE'
				# main2 <- 'FALSE'
			# } else {
				# main1 <- main2 <- ''
			# }
			
			# par(mar=c(0, 1, 0, 0))
			# subplotLand(grain=finest, var='T1', noise=noise, main=main1, ylab=round(noise, 2))
			# par(mar=c(0, 0, 0, 1))
			# subplotLand(grain=finest, var='F1', noise=noise, main=main2)
			
			# par(mar=c(0, 1, 0, 0))
			# subplotLand(grain=native, var='T1', noise=noise, main=main1)
			# par(mar=c(0, 0, 0, 1))
			# subplotLand(grain=native, var='F1', noise=noise, main=main2)
			
			# par(mar=c(0, 1, 0, 0))
			# subplotLand(grain=coarsest, var='T1', noise=noise, main=main1)
			# subplotLand(grain=coarsest, var='F1', noise=noise, main=main2)
			
		# }
			
		# repeatSpace <- 41
		# main <- paste0('Grain: 1/', finest, paste(rep(' ', repeatSpace), collapse=''), 'Grain: 1/', native, paste(rep(' ', repeatSpace), collapse=''),'Grain: 1/', coarsest)
		# title(main, outer=TRUE, line=1.25, cex.main=1.2, col='black', font=1)
		# text(-6.55, 2.35, labels='Proportion swapped\n\U2190lower autocorrelation                 higher autocorrelation\U2192', cex=1.3, srt=90, xpd=NA, col='black', font=2)
		
	# dev.off()
	
# say('############################################################################################')
# say('### [correlated] calculate correlation between variables as a function of their rotation ###')
# say('############################################################################################')

	# say('To speed further operations, pre-calculate correlation between two variables as a function of the rotation between them on the landscape.', breaks=80)

	# rots <- seq(22.5, 157.5, by=22.5)
	
	# correlations <- data.frame()
	# for (rot in rots) {
	
		# say(rot, post=0)
	
		# # generate landscape
		# geography <- list(
			# V1=list(type='linear', min=-1, max=1),
			# V2=list(type='linear', min=-1, max=1, rot=rot)
		# )
		
		# landscape <- genesis(geography, circle=TRUE)
		
		# correlation <- cor(
			# c(as.matrix(landscape[['V1']])),
			# c(as.matrix(landscape[['V2']])),
			# use='pairwise.complete.obs'
		# )
		
		# say(correlation)
		
		# correlations <- rbind(
			# correlations,
			# data.frame(
				# rot=rot,
				# cor=correlation
			# )
		# )
	
	# }
	
	# write.csv(correlations, './Results/Correlations between Variables as a Function of Rotation between Them.csv', row.names=FALSE)

# say('#######################################################')
# say('### [correlated TRUE & FALSE] landscape and species ###')
# say('#######################################################')

	# say('Wanting a simple illustration of the landscape and species in the "correlated TRUE & FALSE" scenario.')
	
	# thisOutDir <- 'correlated TRUE & FALSE'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)

	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA

	# # landscape rotations
	# # rots <- seq(22.5, 157.5, by=22.5)
	# rots <- c(22.5 + 22.5, 90, 157.5 - 22.5) # abbreviated

	# png(paste0(scenarioDir, '/Illustration - correlated TRUE & FALSE Scenario Landscape and Species.png'), width=5 * 400, height=600, res=300)
		
		# par(mfrow=c(1, 5), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), fg='white', col.axis='white')
		
		# # generate landscape
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1),
			# F1=list(type='linear', min=-1, max=1, rot=rots[1])
		# )
		
		# landscape <- genesis(geography, circle=TRUE)

		# # extent (for border)
		# ext <- extent(landscape)
		# ext <- as(ext, 'SpatialPolygons')
			
		# # TRUE
		# plot(ext)
		# plot(landscape[['T1']], breaks=seq(-1, 1, length.out=length(land) - 1), col=land, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('a) TRUE variable', adj=c(0, -0.15), cex=0.8, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=0.07, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=land, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.7)

		# # first FALSE
		# par(fg='white')
		# plot(ext)
		# plot(landscape[['F1']], breaks=seq(-1, 1, length.out=length(land) - 1), col=land, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# correlation <- cor(c(as.matrix(landscape[['T1']])), c(as.matrix(landscape[['F1']])), use='pairwise.complete.obs')
		# lab <- paste0('b) FALSE: Correlation = ', sprintf('%.2f', correlation))
		# labelFig(lab, adj=c(0, -0.15), cex=0.8, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=0.07, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=land, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.7)

		# # next FALSES
		# for (countRot in 2:length(rots)) {
		
			# rot <- rots[countRot]
		
			# # generate landscape
			# geography <- list(
				# T1=list(type='linear', min=-1, max=1),
				# F1=list(type='linear', min=-1, max=1, rot=rot)
			# )
			
			# landscape <- genesis(geography, circle=TRUE)

			# # FALSE
			# par(fg='white')
			# plot(ext)
			# plot(landscape[['F1']], breaks=seq(-1, 1, length.out=length(land) - 1), col=land, ann=FALSE, legend=FALSE, add=TRUE)
			# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
			# correlation <- cor(c(as.matrix(landscape[['T1']])), c(as.matrix(landscape[['F1']])), use='pairwise.complete.obs')
			# lab <- paste0(letters[countRot + 1], ') FALSE: Correlation = ', sprintf('%.2f', correlation))
			# labelFig(lab, adj=c(0, -0.15), cex=0.8, col='black')
			# par(fg='black')
			# legendGrad('bottom', inset=0.07, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=land, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.7)
			
		# }
		
		# # species
		# species <- logistic(x1=landscape[['T1']], x2=landscape[['F1']], b0=b0, b1=b1, b11=b11, b12=b12)
		
		# par(fg='white')
		# plot(ext)
		# plot(species, breaks=seq(0, 1, length.out=length(land) - 1), col=greens, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('e) Species', adj=c(0, -0.15), cex=0.8, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=0.07, vert=FALSE, width=0.93, height=0.1, labels=c(0, 0.5, 1), title='', col=greens, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.7)
			
	# dev.off()

# say('#########################################################################################')
# say('### [bivariate] landscape and species: NO niche covariance, YES landscape correlation ###')
# say('#########################################################################################')

	# say('Wanting a simple illustration of the landscape and species in the "bivariate" scenario with NO change in rho (Gaussian interaction term) and with landscape correlation (r != 0). Panels represent values of r, maps within panels vary sigma1 and sigma 2.', breaks=80)
	
	# thisOutDir <- 'bivariate'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)

	# # generalization
	# landscapeBorder <- 'black' # color of ring around landscape
	
	# # define species
	# b0 <- NA # intercept
	# b1 <- NA # slope of P1
	# b2 <- NA # slope of P2
	# b11 <- NA # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- NA # slope of T1 * T2
	# mu1 <- 0
	# mu2 <- 0
	# sigma1 <- NA
	# sigma2 <- NA
	# rho <- 0

	# # sigma2s <- seq(0.1, 0.5, by=0.1)
	# sigmas <- c(0.1, 0.3, 0.5) # abbreviated
	
	# # landscape rotations
	# # rots <- seq(22.5, 157.5, by=22.5)
	# rots <- rev(c(22.5 + 22.5, 90, 157.5 - 22.5)) # abbreviated

	# correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')
	
	# png(paste0(scenarioDir, '/Illustration - BIVARIATE Scenario Landscape and Species - No Niche Covariance with Landscape Correlation.png'), width=11 * 400, height=4 * 450, res=300)
		
		# par(mfrow=c(4, 11), oma=c(0, 3, 3, 0))

		# for (countPanel in seq_along(rots)) {
		# # for (countPanel in 1) {
		
			# rot <- rots[countPanel]
		
			# # generate landscape
			# geography <- list(
				# T1=list(type='linear', min=-1, max=1),
				# T2=list(type='linear', min=-1, max=1, rot=rot)
				# # T2=list(type='linear', min=-1, max=1)
			# )
		
			# landscape <- genesis(geography, circle=TRUE)

			# # rectangular extent
			# ext <- extent(landscape)
			# ext <- as(ext, 'SpatialPolygons')
			
			# # circular extent (for plotting)
			# center <- SpatialPoints(cbind(0.5, 0.5), CRS(projection(landscape)))
			# ring <- gBuffer(center, width=0.5, quadsegs=20)
			
			# column <- if (countPanel == 1) {
				# 1
			# } else if (countPanel == 2) {
				# 5
			# } else if (countPanel == 3) {
				# 9
			# }
			
			# par(mar=c(6, 1, 0, 0), mfg=c(1, column))
			
			# # legend bar
			# par(fg='black')
			# frame()
			# legendGrad('right', inset=0.035, vert=TRUE, width=0.17, height=1.7, labels=c(-1, 0, 1), title='', col=land, labAdj=0.5, xpd=NA, adjX=c(0, 0.5), adjY=c(0, 0.75), boxBorder=NA, cex=1.4, lwd=1, border=landscapeBorder)
			# text(-0.25, 0.3, 'Landscape', xpd=NA, cex=1.8, pos=4)

			# # major panel label
			# r <- correlations$cor[correlations$rot == rot]
			# r <- round(r, 2)
			# iffy <- if (countPanel == 2) { 'un' } else { '' }
			# lab <- paste0(letters[countPanel], ') Variables ', iffy, 'correlated (r = ', r, ')')
			# labelFig(lab, adj=c(-0.3, 0.42), cex=2.2, col='black')

			# # labels for sigma1 across top of species
			# par(mar=c(4, 1, 0, 0), fg='black')
			
			# # TRUE1
			# plot(ext, border=NA)
			# plot(landscape[['T1']], breaks=seq(-1, 1, length.out=length(land) - 1), col=land, legend=FALSE, add=TRUE, axes=FALSE)
			# plot(ring, add=TRUE, border=landscapeBorder, lwd=1)
			# lab <- paste0('T1')
			# labelFig(lab, adj=c(-0.03, -0.1), cex=1.8, col='black')
			
			# # sigma1 axis label
			# text(0.5, -0.4, labels='Niche width in T1', xpd=NA, col='black', cex=2)
			
			# # TRUE2
			# par(mar=c(4, 1, 0, 0), fg=NA)
			# plot(ext, border=NA)
			# plot(landscape[['T2']], breaks=seq(-1, 1, length.out=length(land) - 1), col=land, legend=FALSE, add=TRUE, axes=FALSE)
			# plot(ring, add=TRUE, border=landscapeBorder, lwd=1)
			# lab <- paste0('T2')
			# labelFig(lab, adj=c(-0.03, -0.1), cex=1.9, col='black')
			
			# par(mar=c(0.5, 0.5, 0.5, 0.5))

			# # plot species: vary niche width
			# for (countSigma2 in rev(seq_along(sigmas))) {
			# # for (countSigma2 in 1) {
			
				# sigma2 <- sigmas[countSigma2]
			
				# for (countSigma1 in seq_along(sigmas)) {
				
					# sigma1 <- sigmas[countSigma1]
				
					# # column and row of species maps
					# row <- 3 - countSigma2 + 2
					
					# if (countPanel == 1) {
						# column <- countSigma1 + 0
					# } else if (countPanel == 2) {
						# column <- countSigma1 + 4
					# } else if (countPanel == 3) {
						# column <- countSigma1 + 8
					# }
					
					# # species map
					# species <- enmSdmPredImport::gaussian(x1=landscape[['T1']], x2=landscape[['T2']], mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

					# # plot
					# par(fg=NA, mfg=c(row, column))
					# plot(ext, border=NA)
					# plot(species, breaks=seq(0, 1, length.out=length(greens) - 1), col=greens, legend=FALSE, add=TRUE, axes=FALSE)
					# plot(ring, add=TRUE, border=landscapeBorder, lwd=1)
					
					# # sigma1 label
					# if (countSigma2 == 3) {
						# lab <- bquote(sigma[1] * ' = ' * .(sigma1))
						# labelFig(lab, adj=c(0.15, 0.01), cex=2, col='black')
					# }
					
					# # sigma2 label
					# if (countSigma1 == 1) {
						# lab <- bquote(sigma[2] * ' = ' * .(sigma2))
						# text(-0.12, 0.5, labels=lab, srt=90, cex=2, xpd=NA, col='black')
					# }
					
					# # sigma2 axis label
					# if (countSigma1 == 1 & countSigma2 == 2) {
						# text(-0.34, 0.5, labels='Niche width in T2', xpd=NA, col='black', srt=90, cex=2)
					# }
					
				# } # next sigma1
				
			# } # next sigma2
			
		# } # next landscape rotation
		
	# dev.off()

# say('#########################################################################################')
# say('### [bivariate] landscape and species: YES niche covariance, NO landscape correlation ###')
# say('#########################################################################################')

	# say('Wanting a simple illustration of the landscape and species in the "bivariate" scenario with changing rho (Gaussian interaction term) and no landscape correlation (r = 0). Panels represent values of rho. Maps within panels vary sigma1 and sigma2.', breaks=80)
	
	# thisOutDir <- 'bivariate'
	# scenarioDir <- paste0('./Results/', thisOutDir)
	# dirCreate(scenarioDir)

	# # generalization
	# landscapeBorder <- 'black' # color of ring around landscape
	
	# # define species
	# b0 <- NA # intercept
	# b1 <- NA # slope of P1
	# b2 <- NA # slope of P2
	# b11 <- NA # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- NA # slope of T1 * T2
	# mu1 <- 0
	# mu2 <- 0
	# sigma1 <- NA
	# sigma2 <- NA
	# rhos <- c(-0.5, 0, 0.5)

	# # sigma2s <- seq(0.1, 0.5, by=0.1)
	# sigmas <- c(0.1, 0.3, 0.5) # abbreviated
	
	# png(paste0(scenarioDir, '/Illustration - BIVARIATE Scenario Landscape and Species - Niche Covariance with No Landscape Correlation.png'), width=11 * 400, height=3 * 450, res=300)
		
		# par(mfrow=c(3, 11), oma=c(0, 3, 6, 0))

		# for (countPanel in seq_along(rhos)) {
		# # for (countPanel in 1) {
		
			# rho <- rhos[countPanel]
		
			# # generate landscape
			# geography <- list(
				# T1=list(type='linear', min=-1, max=1),
				# T2=list(type='linear', min=-1, max=1, rot=90)
				# # T2=list(type='linear', min=-1, max=1)
			# )
		
			# landscape <- genesis(geography, circle=TRUE)

			# # rectangular extent
			# ext <- extent(landscape)
			# ext <- as(ext, 'SpatialPolygons')
			
			# # circular extent (for plotting)
			# center <- SpatialPoints(cbind(0.5, 0.5), CRS(projection(landscape)))
			# ring <- gBuffer(center, width=0.5, quadsegs=20)
			
			# column <- if (countPanel == 1) {
				# 1
			# } else if (countPanel == 2) {
				# 5
			# } else if (countPanel == 3) {
				# 9
			# }
			
			# par(mar=c(0.5, 0.5, 0.5, 0.5), mfg=c(1, column))

			# # plot species: vary niche width
			# for (countSigma2 in rev(seq_along(sigmas))) {
			# # for (countSigma2 in 1) {
			
				# sigma2 <- sigmas[countSigma2]
			
				# for (countSigma1 in seq_along(sigmas)) {
				
					# sigma1 <- sigmas[countSigma1]
				
					# # column and row of species maps
					# row <- 3 - countSigma2 + 1
					
					# if (countPanel == 1) {
						# column <- countSigma1 + 0
					# } else if (countPanel == 2) {
						# column <- countSigma1 + 4
					# } else if (countPanel == 3) {
						# column <- countSigma1 + 8
					# }
					
					# # species map
					# species <- enmSdmPredImport::gaussian(x1=landscape[['T1']], x2=landscape[['T2']], mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

					# # plot
					# par(fg=NA, mfg=c(row, column))
					# plot(ext, border=NA)
					# plot(species, breaks=seq(0, 1, length.out=length(greens) - 1), col=greens, legend=FALSE, add=TRUE, axes=FALSE)
					# plot(ring, add=TRUE, border=landscapeBorder, lwd=1)
					
					# # major panel label
					# if (countSigma1 == 2 & countSigma2 == 3) {
						
						# lab <- if (rho != 0) {
							# paste0(letters[countPanel], ') Niche covariance (rho = ', rho, ')')
						# } else {
							# paste0(letters[countPanel], ') No niche covariance (rho = ', rho, ')')
						# }
						
						# labelFig(lab, adj=c(0.5, 1.2), cex=2.2, col='black')
						
					# }

					# # sigma1 label
					# if (countSigma2 == 3) {
						# lab <- bquote(sigma[1] * ' = ' * .(sigma1))
						# labelFig(lab, adj=c(0.15, 0.05), cex=2, col='black')
					# }
					
					# # sigma2 label
					# if (countSigma1 == 1) {
						# lab <- bquote(sigma[2] * ' = ' * .(sigma2))
						# text(-0.12, 0.5, labels=lab, srt=90, cex=2, xpd=NA, col='black')
					# }
					
					# # sigma1 axis label
					# if (countSigma1 == 1 & countSigma2 == 3) {
					
						# lab <- if (rho != 0) {
							# 'Niche covariance'
						# } else {
							# 'No niche covariance'
						# }
						
						# lab <- bquote(.(letters[countPanel]) * ') ' * .(lab) * ' (' * rho * ' = ' * .(rho) * ')')
					
						# text(-0.5, 1.575, labels=lab, xpd=NA, col='black', srt=0, cex=2.2, pos=4)

					# }
					
					# # sigma1 axis label
					# if (countSigma1 == 2 & countSigma2 == 3) {
					
						# text(0.5, 1.35, labels='Niche width in T1', xpd=NA, col='black', srt=0, cex=2)
					# }
					
					# # sigma2 axis label
					# if (countSigma1 == 1 & countSigma2 == 2) {
						# text(-0.34, 0.5, labels='Niche width in T2', xpd=NA, col='black', srt=90, cex=2)
					# }
					
				# } # next sigma1
				
			# } # next sigma2
			
		# } # next landscape rotation
		
	# dev.off()

#################################
say('DONE!!!', level=1, deco='&')
say(date()) #####################
#################################
