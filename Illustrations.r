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
### variables and settings ###
### [simple] landscape and species ###
### [simple] simulation results ###

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
	# library(beanplot)
	library(tidyverse)
	library(omnibus)
	library(enmSdm)
	library(enmSdmPredImport)
	library(legendary)

	# create abbreviated function names
	algosShort <- function(x) {
	
		# x characater vector
		x[x == 'omniscient'] <- 'OMNI'
		x[x == 'brt'] <- 'BRT'
		x[x == 'gam'] <- 'GAM'
		x[x == 'maxent'] <- 'MAX'
		x[x == 'rf'] <- 'RF'
		x[x == 'glm'] <- 'GLM'
		
		x
		
	}
	
##############################
### variables and settings ###
##############################

	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')

	algos <- c('omniscient', 'gam', 'maxent', 'brt')
	
	### colors
	grays <- gray(seq(0, 1, by=0.01))

	greens <- colorRampPalette(c('white', 'forestgreen'))
	greens <- greens(101)

	browns <- colorRampPalette(c('white', 'chocolate4'))
	browns <- browns(101)

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
		# plot(landscape[['T1']], breaks=seq(-1, 1, length.out=length(grays) - 1), col=browns, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('a) TRUE variable', adj=c(0, -0.05), cex=0.9, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=browns, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
		
		# par(fg='white')
		# plot(ext)
		# plot(landscape[['F1']], breaks=seq(-1, 1, length.out=length(grays) - 1), col=browns, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('b) FALSE variable', adj=c(0, -0.05), cex=0.9, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=browns, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
		
		# par(fg='white')
		# plot(ext)
		# plot(species, breaks=seq(0, 1, length.out=length(grays) - 1), col=greens, ann=FALSE, legend=FALSE, add=TRUE)
		# plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		# labelFig('c) Species', adj=c(0, -0.05), cex=0.9, col='black')
		# par(fg='black')
		# legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(0, 0.5, 1), title='', col=greens, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
	
	# dev.off()

say('###################################')	
say('### [simple] simulation results ###')
say('###################################')

	scenarioDir <- './Results/simple'

	evals <- loadEvals(scenarioDir, algos=algos)
	n <- nrow(evals)

	evals <- data.frame(
		algo=rep(evals$algo, 6 * 3 * n),
		resp=c(rep('aucPresAbsMulti', 3 * n),
			rep('aucPresBgMulti', 3 * n),
			rep('cbiMulti', 3 * n),
			rep('aucPresAbsUni', 3 * n),
			rep('aucPresBgUni', 3 * n),
			rep('cbiUni', 3 * n)
		),
		model=c(rep('Multivariate', 3 * n),
			rep('Multivariate', 3 * n),
			rep('Multivariate', 3 * n),
			rep('Univariate', 3 * n),
			rep('Univariate', 3 * n),
			rep('Univariate', 3 * n)
		),
		metric=c(rep('AUCpa', 3 * n),
			rep('AUCbg', 3 * n),
			rep('CBI', 3 * n),
			rep('AUCpa', 3 * n),
			rep('AUCpb', 3 * n),
			rep('CBI', 3 * n)
		),
		manip=c(c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			c(rep('TRUE', n), rep('Full', n), rep('FALSE', n))
		),
		value=c(c(evals$aucPresAbsMulti_permT1, evals$aucPresAbsMulti, evals$aucPresAbsMulti_permF1),
			c(evals$aucPresBgMulti_permT1, evals$aucPresBgMulti, evals$aucPresBgMulti_permF1),
			c(evals$cbiMulti_permT1, evals$cbiMulti, evals$cbiMulti_permF1),
			c(evals$aucPresAbsUni_onlyT1, evals$aucPresAbsMulti, evals$aucPresAbsUni_onlyF1),
			c(evals$aucPresBgUni_onlyT1, evals$aucPresBgMulti, evals$aucPresBgUni_onlyF1),
			c(evals$cbiUni_onlyT1, evals$cbiMulti, evals$cbiUni_onlyF1)
		)
	)
			
	p <- ggplot(data=evals, mapping=aes(x=manip, y=value, fill=algo))
	p + geom_violin(mapping=aes(group=algo)) + facet_wrap(~ metric + model, ncol=3)
			
	evals <- data.frame(
		algo=c(evals$algo, evals$algo, evals$algo),
		type=c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
		aucPresAbsMulti=c(evals$aucPresAbsMulti_permT1, evals$aucPresAbsMulti, evals$aucPresAbsMulti_permF1),
		aucPresBgMulti=c(evals$aucPresBgMulti_permT1, evals$aucPresBgMulti, evals$aucPresBgMulti_permF1),
		cbiMulti=c(evals$cbiMulti_permT1, evals$cbiMulti, evals$cbiMulti_permF1),
		corPresAbsMulti=c(evals$corPresAbsMulti_permT1, rep(NA, n), evals$corPresAbsMulti_permF1),
		corPresBgMulti=c(evals$corPresBgMulti_permT1, rep(NA, n), evals$corPresBgMulti_permF1),
		aucPresAbsUni=c(evals$aucPresAbsUni_onlyT1, rep(NA, n), evals$aucPresAbsUni_onlyF1),
		aucPresBgUni=c(evals$aucPresBgUni_onlyT1, rep(NA, n), evals$aucPresBgUni_onlyF1),
		cbiUni=c(evals$cbiUni_onlyT1, rep(NA, n), evals$cbiUni_onlyF1)
	)

	x <- ggplot(evals, aes(x=type, color=algo)) +
		geom_violin() 


	
	# p <- ggplot(data=evals, mapping=aes(x=type, y=aucPresAbsMulti))
	# p + geom_violin() + scale_y_continuous(limits=c(0, 1)) + ylab('AUCpa')
	
	# ### AUC with presences/absences
	# resp <- 'aucPresAbsMulti'
	# respPermT1 <- 'aucPresAbsMulti_permT1'
	# respPermF1 <- 'aucPresAbsMulti_permF1'
	
	# ylab <- 'AUCpa'
	
	# plot(1, 1, xlim=c(0, length(algos)), ylim=c(0, 1), ylab=ylab, xaxt='n', col='white', bty='n', xlab='')
	# axis(1, at=seq_along(algos) - 0.5, col.axis='white')
	# usr <- par('usr')
	# y <- usr[3] - 0.1 * usr[4] - usr[3]
	# text(x=seq_along(algos) - 0.5, y=rep(y, length(algos)), labels=algosShort(algos), xpd=NA)
	
	# for (algo in algos) {
	
		# control <- evals[evals$algo==algo, resp]
		# resp1 <- evals[evals$algo==algo, respPermT1]
		# resp2 <- evals[evals$algo==algo, respPermF1]


	
	
#################################
say('DONE!!!', level=1, deco='&')
say(date()) #####################
#################################
