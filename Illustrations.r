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
	
		# x character vector
		x[x == 'omniscient'] <- 'OMNI'
		x[x == 'brt'] <- 'BRT'
		x[x == 'gam'] <- 'GAM'
		x[x == 'maxent'] <- 'MAX'
		x[x == 'rf'] <- 'RF'
		x[x == 'glm'] <- 'GLM'
		
		x
		
	}
	
	# plot rectangle representing inner xth quantile of response variable
	rect <- function(x, at, width=0.1, scale=TRUE, quants=c(0.025, 0.975), col='gray', border='black', ...) {
	
		# x		numeric vector of values
		# at	numeric position of center of rectangle along x axis
		# width	width of rectangle in plot units
		# scale	TRUE ==> scale width by number of models that converged (ie, x values are not NA)
		# quants 2-element numeric, quantiles for inner distribution
		# col, border  color of fill and border
		# ...	args for polygon
		
		if (scale) width <- width * sum(!is.na(x)) / length(x)
		lims <- quantile(x, quants, na.rm=TRUE)
		bottom <- lims[1]
		top <- lims[2]
		med <- median(x, na.rm=TRUE)
		
		left <- at - 0.5 * width
		right <- at + 0.5 * width
		
		polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col=col, border=border, xpd=NA, ...)
		lines(x=c(left, right), y=c(med, med), col=border, xpd=NA, ...)
		
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

# say('###################################')
# say('### [simple] simulation results ###')
# say('###################################')

	# scenarioDir <- './Results/simple'

	# evals <- loadEvals(scenarioDir, algos=algos)

	# # generalization
	# width <- 0.14 # bar width
	# nudge <- 0.22 # nudge left/right
	# figLabPos <- c(-0.15, 0.05) # position of figure label
	
	# ylabX1 <- -0.18 # position of inner y-axis label
	# ylabX2 <- -0.28 # position of outer y-axis label
	# labCex <- 0.55 # size of algorithm, y-axis, and figure labels
	
	# sublabY <- -0.08 # position of TRUE/FALSE variable sublabels
	# sublabCex <- 0.4 # size of TRUE/FALSE sublabels

	# colTrue <- 'darkolivegreen3'
	# borderTrue <- 'darkolivegreen'
	
	# colControl <- 'white'
	# borderControl <- 'black'
	
	# colFalse <- 'darkorange'
	# borderFalse <- 'darkorange4'
	
	# # master plot function
	# plotResp <- function(nudge, ylim, yTicks, ylab, lab, rand, respT1, respControl, respF1) {
		
		# # nudge 	amount to move bars in same group (algorithm) left or right
		# # ylim		y-axis limits
		# # ylab		y-axis label
		# # yTicks	position of tick marks on y-axis
		# # lab		figure label
		# # rand		value of response equal to "random prediction" (eg 0.5 for AUC or 0 for CBI)
		# # respT1	field name of response for TRUE variable
		# # respControl	field name of response for control case (or NULL if none)
		# # respF1	field name of response for FALSE variable
		
		# # adjust nudging of bars in same groups
		# if (is.null(respControl)) nudge <- nudge / 2
		
		# # base plot
		# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0.5, length(algos)), ylim=ylim)
		# labelFig(lab, adj=figLabPos, cex=labCex)
		# usr <- par('usr')

		# # gray background
		# left <- 1 - (2 + ifelse(is.null(respControl), 0.75, 0)) * nudge
		# right <- length(algos) + (2.5 + ifelse(is.null(respControl), 0.25, 0)) * nudge
		# polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col='gray85', border=NA, xpd=NA)
		# lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.4, xpd=NA)
		# for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		# for (i in 1:(length(algos) - 1)) lines(x=c(i + 0.5, i + 0.5), y=c(-1, 1), col='white', lwd=0.5, xpd=NA)
		
		# # x: variable and algorithm labels
		# axis(1, at=seq_along(algos), labels=rep('', length(algos)), tck=-0.03, lwd=0.8)
		# text(seq_along(algos) - nudge, y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep('TRUE', length(algos)), cex=sublabCex, xpd=NA, srt=90, pos=1, col=borderTrue)
		# if (!is.null(respControl)) text(seq_along(algos), y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep('Control', length(algos)), cex=sublabCex, xpd=NA, srt=90, pos=1, col=borderControl)
		# text(seq_along(algos) + nudge, y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep('FALSE', length(algos)), cex=sublabCex, xpd=NA, srt=90, pos=1, col=borderFalse)
		# text(seq_along(algos), y=rep(usr[3] + algoLabY * (usr[4] - usr[3]), length(algos)), labels=algosShort(algos), xpd=NA, cex=labCex)
		
		# # y: y-axis labels
		# axis(2, at=yTicks, labels=yTicks, tck=-0.03, lwd=0.8)
		# text(usr[1] + ylabX1 * (usr[2] - usr[1]), y=mean(yTicks), label='\U2190important       unimportant\U2192', srt=90, cex=0.9 * labCex, xpd=NA)
		# text(usr[1] + ylabX2 * (usr[2] - usr[1]), y=mean(yTicks), label=ylab, srt=90, cex=labCex, xpd=NA)

		# # responses
		# for (countAlgo in seq_along(algos)) {
		
			# algo <- algos[countAlgo]
		
			# true <- evals[evals$algo==algo, respT1]
			# if (!is.null(respControl)) control <- evals[evals$algo==algo, respControl]
			# false <- evals[evals$algo==algo, respF1]
		
			# if (!is.null(respControl)) rect(control, at=countAlgo, width=width, col=colControl, border=borderControl)
			# rect(true, at=countAlgo - nudge, width=width, col=colTrue, border=borderTrue, xpd=NA)
			# rect(false, at=countAlgo + nudge, width=width, col=colFalse, border=borderFalse, xpd=NA)
			
		# }
		
	# }
	
	# ### multivariate
	# ################
	
	# algoLabY <- -0.33 # position of algorithm labels

	# png(paste0(scenarioDir, '/Results - Multivariate Models.png'), width=900, height=1200, res=300)
		
		# par(mfrow=c(3, 2), oma=c(1, 1, 1, 1.4), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# # COR presence/absence multivariate
		# lab <- bquote('a) Multivariate COR'['pa'])
		# ylab <- bquote('COR'['pa'])
		# ylim <- c(-0.5, 1.1)
		# yTicks <- seq(-0.5, 1, by=0.5)
		# respT1 <- 'corPresAbsMulti_permT1'
		# respControl <- NULL
		# respF1 <- 'corPresAbsMulti_permF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# # COR presence/bg multivariate
		# lab <- bquote('b) Multivariate COR'['bg'])
		# ylab <- bquote('COR'['bg'])
		# ylim <- c(-0.5, 1.1)
		# yTicks <- seq(-0.5, 1, by=0.5)
		# respT1 <- 'corPresBgMulti_permT1'
		# respControl <- NULL
		# respF1 <- 'corPresBgMulti_permF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# # AUCpa multivariate
		# lab <- bquote('c) Multivariate AUC'['pa'])
		# ylab <- bquote('AUC'['pa'])
		# ylim <- c(0.25, 1)
		# yTicks <- seq(0.25, 1, by=0.25)
		# respT1 <- 'aucPresAbsMulti_permT1'
		# respControl <- 'aucPresAbsMulti'
		# respF1 <- 'aucPresAbsMulti_permF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0.5, respT1=respT1, respControl=respControl, respF1=respF1)

		# # AUCbg multivariate
		# lab <- bquote('d) Multivariate AUC'['bg'])
		# ylab <- bquote('AUC'['bg'])
		# ylim <- c(0.25, 1)
		# yTicks <- seq(0.25, 1, by=0.25)
		# respT1 <- 'aucPresBgMulti_permT1'
		# respControl <- 'aucPresBgMulti'
		# respF1 <- 'aucPresBgMulti_permF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0.5, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# # CBI multivariate
		# lab <- bquote('e) Multivariate CBI')
		# ylab <- bquote('CBI')
		# ylim <- c(-0.5, 1)
		# yTicks <- seq(-0.5, 1, by=0.5)
		# respT1 <- 'cbiMulti_permT1'
		# respControl <- 'cbiMulti'
		# respF1 <- 'cbiMulti_permF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# title(sub=date(), outer=TRUE, cex.sub=0.3, line=-0.2)
		
	# dev.off()

	# ### univariate
	# ##############
	
	# algoLabY <- -0.35 # position of algorithm labels

	# png(paste0(scenarioDir, '/Results - Univariate Models.png'), width=1200, height=400, res=300)
		
		# par(mfrow=c(1, 3), oma=c(1, 1, 1, 1.4), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# # # COR presence/absence univariate
		# # lab <- bquote('a) Univariate COR'['pa'])
		# # ylab <- bquote('COR'['pa'])
		# # ylim <- c(-0.5, 1.1)
		# # yTicks <- seq(-0.5, 1, by=0.5)
		# # respT1 <- 'corPresAbsMulti_permT1'
		# # respControl <- NULL
		# # respF1 <- 'corPresAbsMulti_permF1'

		# # plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# # # COR presence/bg univariate
		# # lab <- bquote('b) Univariate COR'['bg'])
		# # ylab <- bquote('COR'['bg'])
		# # ylim <- c(-0.5, 1.1)
		# # yTicks <- seq(-0.5, 1, by=0.5)
		# # respT1 <- 'corPresBgMulti_permT1'
		# # respControl <- NULL
		# # respF1 <- 'corPresBgMulti_permF1'

		# # plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# # AUCpa univariate
		# lab <- bquote('a) Univariate AUC'['pa'])
		# ylab <- bquote('AUC'['pa'])
		# ylim <- c(0.25, 1)
		# yTicks <- seq(0.25, 1, by=0.25)
		# respT1 <- 'aucPresAbsUni_onlyT1'
		# respControl <- 'aucPresAbsMulti'
		# respF1 <- 'aucPresAbsUni_onlyF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0.5, respT1=respT1, respControl=respControl, respF1=respF1)

		# # AUCbg univariate
		# lab <- bquote('b) Univariate AUC'['bg'])
		# ylab <- bquote('AUC'['bg'])
		# ylim <- c(0.25, 1)
		# yTicks <- seq(0.25, 1, by=0.25)
		# respT1 <- 'aucPresBgUni_onlyT1'
		# respControl <- 'aucPresBgMulti'
		# respF1 <- 'aucPresBgUni_onlyF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0.5, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# # CBI univariate
		# lab <- bquote('c) Univariate CBI')
		# ylab <- bquote('CBI')
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# respT1 <- 'cbiUni_onlyT1'
		# respControl <- 'cbiMulti'
		# respF1 <- 'cbiUni_onlyF1'

		# plotResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1)
		
		# title(sub=date(), outer=TRUE, cex.sub=0.3, line=-0.2)
		
	# dev.off()

	# ### report statistics
	# #####################
	
	# say('STATISTICS:', pre=2)
	
	# x <- evals$aucPresAbsMulti[evals$algo=='omniscient']
	# avg <- mean(x)
	# say('Mean AUCpa for unpermuted multivariate OMNI model is ', sprintf('%.2f', avg), '.')
	
	# x <- evals$aucPresBgMulti[evals$algo=='omniscient']
	# avg <- mean(x)
	# say('Mean AUCbg for unpermuted multivariate OMNI model is ', sprintf('%.2f', avg), '.')
	
	# x <- evals$brtMultiImportT1[evals$algo=='brt']
	# avg <- mean(x)
	# quants <- quantile(x, c(0.025, 0.975), na.rm=TRUE)
	# say('Mean algorithm-specific importance for multivariate BRT model for TRUE variable is ', sprintf('%.2f', avg), ' (inner 95% range:', sprintf('%.2f', quants[1]), '-', sprintf('%.2f', quants[2]), ')')
	
	# x <- evals$brtMultiImportF1[evals$algo=='brt']
	# avg <- mean(x)
	# quants <- quantile(x, c(0.025, 0.975), na.rm=TRUE)
	# say('Mean algorithm-specific importance for multivariate BRT model for FALSE variable is ', sprintf('%.2f', avg), ' (inner 95% range:', sprintf('%.2f', quants[1]), '-', sprintf('%.2f', quants[2]), ')')
	
	# x <- evals$aucPresAbsMulti[evals$algo=='brt']
	# successes <- sum(!is.na(x))
	# say('Number of times multivariate BRT converged (out of 100):', successes)
	
	# x <- evals$cbiUni_onlyT1[evals$algo=='brt']
	# successes <- sum(!is.na(x))
	# say('Number of times univariate BRT converged using just TRUE variable (out of 100):', successes)
	
	# x <- evals$cbiUni_onlyF1[evals$algo=='brt']
	# successes <- sum(!is.na(x))
	# say('Number of times univariate BRT converged using just FALSE variable (out of 100):', successes)
	
say('######################################')
say('### [extent] landscape and species ###')
say('######################################')
	
	say('Wanting a simple illustration of the landscape and species in a "simple" scenario.')
	
	thisOutDir <- 'simple'
	scenarioDir <- paste0('./Results/', thisOutDir)
	dirCreate(scenarioDir)

	# define landscape
	geography <- list(T1=list(type='linear', min=-1, max=1), F1=list(type='random', min=-1, max=1))
	landscape <- genesis(geography, circle=FALSE)
	
	# define species
	b0 <- 0 # intercept
	b1 <- 2 # slope of P1
	b2 <- 1 # slope of P2
	b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	b12 <- 0 # slope of T1 * T2
	mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	species <- logistic(x1=landscape[['T1']], x2=landscape[['F1']], b0=b0, b1=b1, b11=b11, b12=b12)

	# extent (for border)
	ext <- extent(landscape)
	ext <- as(ext, 'SpatialPolygons')

	png(paste0(scenarioDir, '/Illustration - SIMPLE Scenario Landscape and Species.png'), width=1200, height=600, res=300)
	
		par(mfrow=c(1, 3), oma=rep(0, 4), mar=c(1, 0, 2, 0), fg='white', col.axis='white')

		plot(ext)
		plot(landscape[['T1']], breaks=seq(-1, 1, length.out=length(grays) - 1), col=browns, ann=FALSE, legend=FALSE, add=TRUE)
		plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		labelFig('a) TRUE variable', adj=c(0, -0.05), cex=0.9, col='black')
		par(fg='black')
		legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=browns, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
		
		par(fg='white')
		plot(ext)
		plot(landscape[['F1']], breaks=seq(-1, 1, length.out=length(grays) - 1), col=browns, ann=FALSE, legend=FALSE, add=TRUE)
		plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		labelFig('b) FALSE variable', adj=c(0, -0.05), cex=0.9, col='black')
		par(fg='black')
		legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(-1, 0, 1), title='', col=browns, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
		
		par(fg='white')
		plot(ext)
		plot(species, breaks=seq(0, 1, length.out=length(grays) - 1), col=greens, ann=FALSE, legend=FALSE, add=TRUE)
		plot(ext, border='black', xpd=NA, ann=FALSE, add=TRUE)
		labelFig('c) Species', adj=c(0, -0.05), cex=0.9, col='black')
		par(fg='black')
		legendGrad('bottom', inset=-0.001, vert=FALSE, width=0.93, height=0.1, labels=c(0, 0.5, 1), title='', col=greens, labAdj=-0.8, xpd=NA, adjX=c(0, 1), adjY=c(0.6, 1), boxBorder=NA, cex=0.8)
	
	dev.off()












































































	
	
	# n <- nrow(evals)

	# evals <- data.frame(
		# algo=rep(evals$algo, 6 * 3 * n),
		# resp=c(rep('aucPresAbsMulti', 3 * n),
			# rep('aucPresBgMulti', 3 * n),
			# rep('cbiMulti', 3 * n),
			# rep('aucPresAbsUni', 3 * n),
			# rep('aucPresBgUni', 3 * n),
			# rep('cbiUni', 3 * n)
		# ),
		# model=c(rep('Multivariate', 3 * n),
			# rep('Multivariate', 3 * n),
			# rep('Multivariate', 3 * n),
			# rep('Univariate', 3 * n),
			# rep('Univariate', 3 * n),
			# rep('Univariate', 3 * n)
		# ),
		# metric=c(rep('AUCpa', 3 * n),
			# rep('AUCbg', 3 * n),
			# rep('CBI', 3 * n),
			# rep('AUCpa', 3 * n),
			# rep('AUCpb', 3 * n),
			# rep('CBI', 3 * n)
		# ),
		# manip=c(c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			# c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			# c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			# c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			# c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
			# c(rep('TRUE', n), rep('Full', n), rep('FALSE', n))
		# ),
		# value=c(c(evals$aucPresAbsMulti_permT1, evals$aucPresAbsMulti, evals$aucPresAbsMulti_permF1),
			# c(evals$aucPresBgMulti_permT1, evals$aucPresBgMulti, evals$aucPresBgMulti_permF1),
			# c(evals$cbiMulti_permT1, evals$cbiMulti, evals$cbiMulti_permF1),
			# c(evals$aucPresAbsUni_onlyT1, evals$aucPresAbsMulti, evals$aucPresAbsUni_onlyF1),
			# c(evals$aucPresBgUni_onlyT1, evals$aucPresBgMulti, evals$aucPresBgUni_onlyF1),
			# c(evals$cbiUni_onlyT1, evals$cbiMulti, evals$cbiUni_onlyF1)
		# )
	# )
			
	# p <- ggplot(data=evals, mapping=aes(x=manip, y=value, fill=algo))
	# p + geom_violin(mapping=aes(group=algo)) + facet_wrap(~ metric + model, ncol=3)
			
	# evals <- data.frame(
		# algo=c(evals$algo, evals$algo, evals$algo),
		# type=c(rep('TRUE', n), rep('Full', n), rep('FALSE', n)),
		# aucPresAbsMulti=c(evals$aucPresAbsMulti_permT1, evals$aucPresAbsMulti, evals$aucPresAbsMulti_permF1),
		# aucPresBgMulti=c(evals$aucPresBgMulti_permT1, evals$aucPresBgMulti, evals$aucPresBgMulti_permF1),
		# cbiMulti=c(evals$cbiMulti_permT1, evals$cbiMulti, evals$cbiMulti_permF1),
		# corPresAbsMulti=c(evals$corPresAbsMulti_permT1, rep(NA, n), evals$corPresAbsMulti_permF1),
		# corPresBgMulti=c(evals$corPresBgMulti_permT1, rep(NA, n), evals$corPresBgMulti_permF1),
		# aucPresAbsUni=c(evals$aucPresAbsUni_onlyT1, rep(NA, n), evals$aucPresAbsUni_onlyF1),
		# aucPresBgUni=c(evals$aucPresBgUni_onlyT1, rep(NA, n), evals$aucPresBgUni_onlyF1),
		# cbiUni=c(evals$cbiUni_onlyT1, rep(NA, n), evals$cbiUni_onlyF1)
	# )

	# x <- ggplot(evals, aes(x=type, color=algo)) +
		# geom_violin() 


	
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
