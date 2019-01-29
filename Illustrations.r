## SDM PREDICTOR INFERENCE - ILLUSTRATIONS
## Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
## source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/Illustrations.r')

	memory.limit(memory.limit() * 2^30)
	rm(list=ls())
	options(keep.source=FALSE) # manage memory
	gc()
	print('')
	print(date())

### CONTENTS ###
### functions ###
### variables ###
### [simple] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###

#################
### functions ###
#################

	library(compiler)
	library(sp)
	library(rgdal)
	library(raster)
	library(RColorBrewer)
	library(rJava)
	options(java.parameters='-Xmx1g' )
	library(dismo)
	library(scales)
	library(beanplot)
	library(omnibus)
	library(enmSdm)
	library(enmSdmPredImport)
	library(legendary)

#################
### variables ###
#################

	grays <- gray(seq(0, 1, by=0.01))

	greens <- colorRampPalette(c('white', 'forestgreen'))
	greens <- greens(101)

	browns <- colorRampPalette(c('white', 'chocolate4'))
	browns <- browns(101)

say('#############################################################################')
say('### [simple] RESPONSE logistic(T1) MODEL T1 F1 GEOG linear(T1) random(F1) ###')
say('#############################################################################')
	
	say('Wanting a simple illustration of the landscape and species in a "simple" scenario.')
	
	thisOutDir <- 'simple'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)

	# define species
	b0 <- 0 # intercept
	b1 <- 2 # slope of P1
	b2 <- 1 # slope of P2
	b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	b12 <- 0 # slope of T1 * T2
	mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# define landscape
	geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))
	landscape <- genesis(geography, circle=FALSE)
	
	# define species
	species <- logistic(x1=landscape[['T1']], x2=landscape[['F1']], b0=b0, b1=b1, b11=b11, b12=b12)

	# extent (for border)
	ext <- extent(landscape)
	ext <- as(ext, 'SpatialPolygons')

	par(paste0(scenarioDir, '/Illustration - SIMPLE Scenario Landscape and Species.png'), width=1200, height=500, res=300)
	
		par(mfrow=c(1, 3), oma=rep(0.1, 4), mar=c(0, 0, 3, 3), fg='white')

		plot(landscape[['T1']], breaks=seq(-1, 1, length.out=length(grays) - 1), col=browns, ann=FALSE, legend=FALSE)
		plot(ext, add=TRUE, border='black', xpd=NA)
		labelFig('a) TRUE variable', adj=c(0, 0.1), cex=0.6, col='black')
		
		plot(landscape[['F1']], breaks=seq(-1, 1, length.out=length(grays) - 1), col=browns, ann=FALSE, legend=FALSE)
		plot(ext, add=TRUE, border='black', xpd=NA)
		labelFig('b) FALSE variable', adj=c(0, 0.1), cex=0.6)
		
		plot(species, breaks=seq(0, 1, length.out=length(grays) - 1), col=greens, ann=FALSE, legend=FALSE)
		plot(ext, add=TRUE, border='black', xpd=NA)
		labelFig('c) Species', adj=c(0, 0.1), cex=0.6)
	
	dev.off()
	
	
say('DONE!!!', level=1, deco='&')
say(date())


