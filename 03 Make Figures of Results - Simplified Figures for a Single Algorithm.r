### SDM PREDICTOR INFERENCE - ILLUSTRATIONS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
### source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/03 Make Figures of Results - Simplified Figures for a Single Algorithm.r')
###
### The code in this document is intended to be run after all models have been calibrated and evaluated and recreates the figures used in the main body of the manuscript by Smith & Santos. For most experiments, it creates one figure per test statistic (AUCpa, AUCbg, CBI, CORpa, or CORbg) per SDM algorithm (sometimes just one per algorithm or test statistic is generated). Much of the code relies on a set of plotting functions which appear at the beginning of the document.

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

### [simple] simulation results ###
### [simple] statistics ###
### [sample size] simulation results ###
### [extent] simulation results ###
### [prevalence] simulation results ###
### [correlated TRUE & FALSE] simulation results ###
### [resolution] simulation results ###

### [bivariate] collate evaluations ###
### [bivariate] statistics ###
### [bivariate] niche covariance ###
### [niche breadth] simulation results ###

#################
### libraries ###
#################

	# CRAN
	library(sp)
	library(raster)
	library(RColorBrewer)
	library(plotrix)
	library(fpCompare)
	library(scales)

	# custom libraries (on GitHub, user account adamlilith)
	library(omnibus)
	library(enmSdm)
	library(statisfactory)
	library(enmSdmPredImport)
	library(legendary)

##############################
### variables and settings ###
##############################

	### working directory
	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')

	# algorithm to plot
	# sdmAlgo <- 'gam'
	sdmAlgo <- 'maxent'
	# sdmAlgo <- 'brt'

	algos <- c('omniscient', sdmAlgo)

	### For a given response range to be considered well-calibrated, it must be within +- 100 * calibTol% of the range of the OMNI model's range (eg if calibTol = 0.1 it must be within +- 10% of the OMNI models' range)
	### For a given response's median to be considered well-calibrated, it must be within the innermost +-100 * calibTol% quantile (eg, if calibTol = 0.1, it must be within the 40th and 60th quantiles)
	calibTol <- 0.1
	calibSymbol <- '\U2020'
	discrimSymbol <- '\U002A' # symbol to indicate OMNI and SDM can discriminate between variables
	nullSymbol <- '\U00D7' # symbol to indicate OMNI and SDM can discriminate between variables
	
	### colors of bars for scenarios with TRUE and FALSE variables
	##############################################################
	
	colOmniControl <- 'white' # unperturbed OMNI
	borderOmniControl <- 'black'# 'gray' # unperturbed OMNI
	
	colSdmControl <- 'gray'# 'white'# '#8da0cb' # unperturbed SDM (CB safe)
	borderSdmControl <- 'black'# '#7570b3' # unperturbed SDM (CB safe)
	
	colTrue <- '#7fbf7b' # perturbed SDM vs TRUE (CB safe)
	borderTrue <- '#1b7837' # perturbed SDM vs TRUE (CB safe)
	
	colFalse <- '#d6604d' # perturbed SDM vs FALSE (CB safe)
	borderFalse <- '#b2182b' # perturbed SDM vs FALSE (CB safe)

	### colors for scenarios with TRUE1 and TRUE2 variables
	#######################################################
	colSdmT1 <- '#a6dba0' # light green (CB safe)
	borderSdmT1 <- borderOmniT1 <- '#008837' # dark green (CB safe)
	
	colSdmT2 <- '#c2a5cf' # light purple (CB safe)
	borderSdmT2 <- borderOmniT2 <- '#7b3294' # dark purple (CB safe)

	### gray background
	panelBgCol <- 'gray87'
	
	### plot settings
	#################
	
	rectLwd <- 0.8 # line width of rectangles
	
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
		x[x == 'maxent'] <- 'Maxent'
		x[x == 'rf'] <- 'RF'
		x[x == 'glm'] <- 'GLM'
		
		x
		
	}
	
	###############################################################################
	### plot results for a series of response variables for multivariate models ###
	###############################################################################
	
	### This function makes one figure file for each response (CBI, AUCpa, etc.).
	### Each figure has three panels (one per algorithm).
	### Each panel has a user-specified variable along the x-axis (eg prevalence, extent, correlation) and the test statistic on teh y-axis (eg CBI, AUC, COR).
	### The function is specific to cases comparing a TRUE variable and a FALSE variable.
	multivariatePlotsTRUEvsFALSE <- function(
		scenarioDir,					# scenario directory
		evalDir,						# evaluation directory
		xCol,							# name of field in "evals" with x-axis values
		decs,							# number of decimals to show in x-axis variable tick mark labels
		xlab,							# x-axis label
		lab,							# plot label
		evals,							# data frame with evaluations
		expectHigher,					# TRUE or FALSE or NULL... which variable should have a higher value of
										# the test statistic if the variables can be discriminated accurately?
										# (TRUE or FALSE or NULL for neither)
		resps=c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg') # responses to plot
	) {
		
		for (resp in resps) {
			
			if (resp == 'Multivariate CBI') {
				
				ylim <- c(-1, 1)
				yTicks <- seq(-1, 1, by=0.5)
				ylab <- 'CBI'
				rand <- 0
				trueField <- 'cbiMulti_permT1'
				falseField <- 'cbiMulti_permF1'
				controlField <- 'cbiMulti'
				
			} else if (resp == 'Multivariate AUCpa') {

				ylim <- c(0, 1)
				yTicks <- seq(0, 1, by=0.25)
				ylab <- bquote('AUC'['pa'])
				rand <- 0.5
				trueField <- 'aucPresAbsMulti_permT1'
				falseField <- 'aucPresAbsMulti_permF1'
				controlField <- 'aucPresAbsMulti'
				
			} else if (resp == 'Multivariate AUCbg') {

				ylim <- c(0, 1)
				yTicks <- seq(0, 1, by=0.25)
				ylab <- bquote('AUC'['bg'])
				rand <- 0.5
				trueField <- 'aucPresBgMulti_permT1'
				falseField <- 'aucPresBgMulti_permF1'
				controlField <- 'aucPresBgMulti'
				
			} else if (resp == 'Multivariate CORpa') {

				ylim <- c(-0.25, 1)
				yTicks <- seq(-0.25, 1, by=0.25)
				ylab <- bquote('COR'['pa'])
				rand <- 0
				trueField <- 'corPresAbsMulti_permT1'
				falseField <- 'corPresAbsMulti_permF1'
				controlField <- NULL
				
			} else if (resp == 'Multivariate CORbg') {

				ylim <- c(-0.25, 1)
				yTicks <- seq(-0.25, 1, by=0.25)
				ylab <- bquote('COR'['bg'])
				rand <- 0
				trueField <- 'corPresBgMulti_permT1'
				falseField <- 'corPresBgMulti_permF1'
				controlField <- NULL
				
			}
			
			png(paste0(scenarioDir, '/', toupper(sdmAlgo), ' ', resp, '.png'), width=1414, height=1000, res=600)
				
					par(oma=c(0, 0, 0, 0), mar=c(1.2, 1.6, 0.6, 0.3), mgp=c(2, 0.2, 0), cex.axis=0.37, tcl=-0.2)
				
					plotScalarRespTRUEvsFALSE(xCol=xCol, decs=decs, xlab=xlab, algo=sdmAlgo, nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab=ylab, lab=lab, rand=rand, trueField=trueField, falseField=falseField, controlField=controlField, expectHigher=expectHigher)
				
			dev.off()
			
		} # next response
		
	}
			
	### This function makes one figure file for each response (CBI, AUCpa, etc.).
	### Each figure has three panels (one per algorithm).
	### Each panel has a user-specified variable along the x-axis (eg prevalence, extent, correlation) and the test statistic on teh y-axis (eg CBI, AUC, COR).
	### The function is specific to cases comparing a TRUE variable and another TRUE variable.
	multivariatePlotsTRUEvsTRUE <- function(
		scenarioDir,					# scenario directory
		evalDir,						# evaluation directory
		xCol,							# name of field in "evals" with x-axis values
		decs,							# number of decimals to show in x-axis variable tick mark labels
		filePrepend,					# name of plot to prepend to file name (e.g., "Niche Covariance" or "Niche Breadth")
		lab,							# figure label
		xlab,							# x-axis label
		evals,							# data frame with evaluations
		resps=c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg') # responses to plot
	) {
		
		for (resp in resps) {
			
			if (resp == 'Multivariate CBI') {
				
				ylim <- c(-1, 1)
				yTicks <- seq(-1, 1, by=0.25)
				ylab <- 'CBI'
				rand <- 0
				true1Field <- 'cbiMulti_permT1'
				true2Field <- 'cbiMulti_permT2'
				controlField <- 'cbiMulti'
				
			} else if (resp == 'Multivariate AUCpa') {

				ylim <- c(0, 1)
				yTicks <- seq(0, 1, by=0.25)
				ylab <- bquote('AUC'['pa'])
				rand <- 0.5
				true1Field <- 'aucPresAbsMulti_permT1'
				true2Field <- 'aucPresAbsMulti_permT2'
				controlField <- 'aucPresAbsMulti'
				
			} else if (resp == 'Multivariate AUCbg') {

				ylim <- c(0, 1)
				yTicks <- seq(0, 1, by=0.25)
				ylab <- bquote('AUC'['bg'])
				rand <- 0.5
				true1Field <- 'aucPresBgMulti_permT1'
				true2Field <- 'aucPresBgMulti_permT2'
				controlField <- 'aucPresBgMulti'
				
			} else if (resp == 'Multivariate CORpa') {

				ylim <- c(-0.25, 1)
				yTicks <- seq(-0.25, 1, by=0.25)
				ylab <- bquote('COR'['pa'])
				rand <- 0
				true1Field <- 'corPresAbsMulti_permT1'
				true2Field <- 'corPresAbsMulti_permT2'
				controlField <- NULL
				
			} else if (resp == 'Multivariate CORbg') {

				ylim <- c(-0.25, 1)
				yTicks <- seq(-0.25, 1, by=0.25)
				ylab <- bquote('COR'['bg'])
				rand <- 0
				true1Field <- 'corPresBgMulti_permT1'
				true2Field <- 'corPresBgMulti_permT2'
				controlField <- NULL
				
			}
			
			png(paste0(scenarioDir, '/', filePrepend, ' ', toupper(sdmAlgo), ' ', resp, '.png'), width=1414, height=1000, res=600)
				
					par(oma=c(0, 0, 0, 0), mar=c(1.2, 1.6, 0.6, 0.3), mgp=c(2, 0.2, 0), cex.axis=0.37, tcl=-0.2)
				
					plotScalarRespTRUEvsTRUE(xCol=xCol, decs=decs, xlab=xlab, algo=sdmAlgo, nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab=ylab, lab=lab, rand=rand, true1Field=true1Field, true2Field=true2Field, controlField=controlField)
				
			dev.off()
			
		} # next response
		
	}
			
	##############################
	### "scalar" plot function ###
	##############################
	
	### generic plot function for plots with a scalar along the x-axis and variable importance along the y
	### The x-axis can represent: prevalence, landscape extent, correlation between landscape variables, correlation between variables in shaping the niche, and so on. This function is intended to supply a thematic unity to plots of these types.
	### This function is best for plots with 1 column and 3 rows, one row per algorithm
	### The function assumes the variables of interest are TRUE and FALSE (ie not TRUE vs another TRUE)
	plotScalarRespTRUEvsFALSE <- function(
		xCol,
		decs,
		xlab,
		algo,
		nudge,
		subnudge,
		ylim,
		yTicks,
		ylab,
		lab,
		rand,
		trueField,
		falseField,
		controlField,
		expectHigher
	) {
		
		# general idea:
		# graph shows results for one algorithm plus OMNI
		# x-axis: independent variable (prevalence, extent, etc)
		# y-axis: variable importance
		# each level of x-variable range: three sets of bars per algorithm, one is control, one is TRUE, one for FALSE, sets of bars are staggered
		
		# xCol			name of column in evaluation data frame that has values for x axis
		# decs			NULL (use values of xCol as-is for x-axis tick labels) or an integer indicating number
						# of digits to display for x tick labels
		# xlab			x-axis label
		# algo			algorithm (not OMNI)
		# nudge 		amount to move sets of bars belonging to control/treatment model predictions relative to x-axis tick
		# subnudge		amount to move bars belonging to same control/treatment model predictions relative to x-axis tick
		# ylim			y-axis limits
		# ylab			y-axis label
		# yTicks		position of tick marks on y-axis
		# lab			figure label
		# rand			value of response equal to "random prediction" (eg 0.5 for AUC or 0 for CBI)
		# trueField		field name of response when TRUE is permuted
		# falseField		field name of response when FALSE is permuted
		# controlField	field name of response for control case (or NULL if none)
		# expectHigher  if test can discriminate, values of which variable should be higher (TRUE or FALSE or NULL for neither)

		### settings for plot formatting
		width <- 0.17 # bar width
		nudge <- 0.26 # nudge pair of bars for same algorithm left/right
		subnudge <- nudge / 4 # nudge bars within same class
		lwd <- 0.5 # line width of box borders
		figLabPos <- c(-0.2105, 0.03) # position of figure label
		
		legCex <- 0.42 # legend
		
		ylabX1 <- -0.095 # position of inner y-axis label
		ylabX2 <- -0.141 # position of outer y-axis label
		labCex <- 0.53 # size of algorithm, y-axis, and figure labels
		
		xlabY1 <- 0.02 # position of inner x-axis sublabels
		xlabY2 <- -0.15 # position of outer x-axis label
		
		lineDensity <- 110 # density of lines per inch for perturbed OMNI
	
		### plot
	
		# x-axis values
		x <- sort(unique(evals[ , xCol]))
		
		# base plot
		plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0.5, length(x)), ylim=ylim, tcl=-0.25)
		labelFig(lab, adj=figLabPos, cex=labCex)
		usr <- par('usr')
		
		# gray background
		left <- 1 - (2 + ifelse(is.null(controlField), 0.75, 0)) * nudge
		right <- length(x) + (2 + ifelse(is.null(controlField), 0.25, 0)) * nudge
		polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col=panelBgCol, border=NA, xpd=NA)
		lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.8 * lwd, xpd=NA)
		for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		for (i in 1:(length(x) - 1)) lines(x=c(i + 0.5, i + 0.5), y=c(-1, 1), col='white', lwd=0.5, xpd=NA)

		# x: axis labels
		axis(1, at=seq_along(x), labels=rep('', length(x)), tck=-0.02, lwd=lwd)
		xLabs <- if (!is.null(decs)) { sprintf(paste0('%.', decs, 'f'), x) } else { x }
		text(seq_along(x), y=rep(usr[3] + xlabY1 * (usr[4] - usr[3]), length(x)), labels=xLabs, cex=0.8 * labCex, xpd=NA, srt=0, pos=1, col='black')
		text(mean(seq_along(x)), y=usr[3] + xlabY2 * (usr[4] - usr[3]), labels=xlab, cex=labCex, xpd=NA, srt=0, col='black')
	
		# y: y-axis labels
		axis(2, at=yTicks, labels=yTicks, tck=-0.03, lwd=lwd)
		text(usr[1] + ylabX1 * (usr[2] - usr[1]), y=mean(yTicks), label='\U2190important       unimportant\U2192', srt=90, cex=0.9 * labCex, xpd=NA)
		text(usr[1] + ylabX2 * (usr[2] - usr[1]), y=mean(yTicks), label=ylab, srt=90, cex=labCex, xpd=NA)

		thisNudge <- length(algos) / 2
		
		# for each value of x
		for (countX in seq_along(x)) {
		
			thisX <- x[countX]

			# get data
			omniTrue <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, trueField]
			omniFalse <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, falseField]
			sdmTrue <- evals[evals$algo == algo & evals[ , xCol] == thisX, trueField]
			sdmFalse <- evals[evals$algo == algo & evals[ , xCol] == thisX, falseField]
		
			# if there is a distinct response for control/unperturbed models
			# used when using CBI or AUC
			if (!is.null(controlField)) {
		
				omniControl <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, controlField]
				sdmControl <- evals[evals$algo == algo & evals[ , xCol] == thisX, controlField]
			
				# unperturbed OMNI
				rect(omniControl, at=countX - nudge - subnudge, width=width, col=colOmniControl, border=NA, xpd=NA, lwd=rectLwd)
				rect(omniControl, at=countX - nudge - subnudge, width=width, density=lineDensity, col=colOmniControl, fill=colOmniControl, border=borderOmniControl, xpd=NA, lwd=rectLwd)
			
				# unperturbed SDM
				rect(sdmControl, at=countX - nudge + subnudge, width=width, col=colSdmControl, border=borderSdmControl, xpd=NA, lwd=rectLwd)

				leg <- c(
					'OMNI unpermuted',
					paste0('OMNI TRUE permuted'),
					paste0('OMNI FALSE permuted'),
					paste0(algosShort(algo), ' unpermuted'),
					paste0(algosShort(algo), ' TRUE permuted'),
					paste0(algosShort(algo), ' FALSE permuted')
				)
			
				par(lwd=0.5)
			
				legend('bottomleft', inset=c(0.03, 0.02), ncol=2, bty='n', legend=leg, cex=legCex, pch=22, pt.cex=0.9, pt.bg=c('white', 'white', 'white', colSdmControl, colTrue, colFalse), pt.lwd=rectLwd, col=c(borderOmniControl, borderTrue, borderFalse, borderSdmControl, borderTrue, borderFalse))
				
				adjust <- 0
				
			# if there is no distinct response for control/unperturbed
			# for when plotting correlation metric
			} else {

				omniControl <- sdmControl <- NULL
			
				# legend
				leg <- c(
					paste0('OMNI TRUE perm.'),
					paste0('OMNI FALSE perm.'),
					paste0(algosShort(algo), ' TRUE perm.'),
					paste0(algosShort(algo), ' FALSE perm.')
				)
			
				par(lwd=0.5)

				legend('bottomright', inset=c(0, 0.025), ncol=2, bty='n', legend=leg, cex=legCex, fill=c('white', 'white', colTrue, colFalse), border=c(borderTrue, borderFalse, borderTrue, borderFalse), xpd=NA)
				
				adjust <- 1/2 * nudge
				
			}
				
			### TRUE response
			rect(omniTrue, at=countX - adjust - subnudge, width=width, col='white', border=borderTrue, xpd=NA, lwd=rectLwd)
			rect(sdmTrue, at=countX - adjust + subnudge, width=width, col=colTrue, border=borderTrue, xpd=NA, lwd=rectLwd)

			### FALSE response
			rect(omniFalse, at=countX - adjust + nudge - subnudge, width=width, col='white', border=borderFalse, xpd=NA, lwd=rectLwd)
			rect(sdmFalse, at=countX - adjust + nudge + subnudge, width=width, col=colFalse, border=borderFalse, xpd=NA, lwd=rectLwd)

			### accuracy indicators
			if (!is.na(expectHigher)) {
			
				if (!all(is.na(sdmTrue)) & !all(is.na(sdmFalse))) {
		
					acc <- character()
		
					# discrimination
					wellDiscrim <- discriminatedTrueFalse(sdmTrue, sdmFalse, expectHigher=expectHigher) & discriminatedTrueFalse(omniTrue, omniFalse, expectHigher=expectHigher)
					if (!is.na(wellDiscrim) && wellDiscrim) acc <- c(acc, discrimSymbol)
					
					# calibration
					wellCalib <- calibratedTrueFalse(omniControl=omniControl, omniTrue=omniTrue, omniFalse=omniFalse, sdmControl=sdmControl, sdmTrue=sdmTrue, sdmFalse=sdmFalse, calibTol=calibTol)
					if (!is.na(wellCalib) && wellCalib) acc <- c(acc, calibSymbol)
					
					text(countX, ylim[2] + 0.12 * diff(ylim), labels=paste(acc, collapse=' '), xpd=NA, cex=1.3 * labCex, pos=1)
					
				}
				
			}
				
		} # next value along x axis
		
	}
	
	### generic plot function for plots with a scalar along the x-axis and variable importance along the y
	### The x-axis can represent: prevalence, landscape extent, correlation between landscape variables, correlation between variables in shaping the niche, and so on. This function is intended to supply a thematic unity to plots of these types.
	### This function is best for plots with 1 column and 3 rows, one row per algorithm
	### The function assumes the variables of interest are TRUE and another TRUE (ie not TRUE vs FALSE)
	plotScalarRespTRUEvsTRUE <- function(
		xCol,
		decs,
		xlab,
		algo,
		nudge,
		subnudge,
		ylim,
		yTicks,
		ylab,
		lab,
		rand,
		true1Field,
		true2Field,
		controlField
	) {
		
		# general idea:
		# graph shows results for one algorithm plus OMNI
		# x-axis: independent variable (prevalence, extent, etc)
		# y-axis: variable importance
		# each level of x-variable range: three sets of bars per algorithm, one is control, one is TRUE, one for FALSE, sets of bars are staggered
		
		# xCol			name of column in evaluation data frame that has values for x axis
		# decs			NULL (use values of xCol as-is for x-axis tick labels) or an integer indicating number
						# of digits to display for x tick labels
		# xlab			x-axis label
		# algo			algorithm (not OMNI)
		# nudge 		amount to move sets of bars belonging to control/treatment model predictions relative to x-axis tick
		# subnudge		amount to move bars belonging to same control/treatment model predictions relative to x-axis tick
		# ylim			y-axis limits
		# ylab			y-axis label
		# yTicks		position of tick marks on y-axis
		# lab			figure label
		# rand			value of response equal to "random prediction" (eg 0.5 for AUC or 0 for CBI)
		# trueField		field name of response when TRUE1 is permuted
		# falseField	field name of response when TRUE2 is permuted
		# controlField	field name of response for control case (or NULL if none)

		### settings for plot formatting
		width <- 0.17 # bar width
		nudge <- 0.26 # nudge pair of bars for same algorithm left/right
		subnudge <- nudge / 4 # nudge bars within same class
		lwd <- 0.5 # line width of box borders
		figLabPos <- c(-0.2105, 0.03) # position of figure label
		
		legCex <- 0.42 # legend
		
		ylabX1 <- -0.095 # position of inner y-axis label
		ylabX2 <- -0.141 # position of outer y-axis label
		labCex <- 0.53 # size of algorithm, y-axis, and figure labels
		
		xlabY1 <- 0.02 # position of inner x-axis sublabels
		xlabY2 <- -0.15 # position of outer x-axis label
		
		lineDensity <- 110 # density of lines per inch for perturbed OMNI
	
		### plot
	
		# x-axis values
		x <- sort(unique(evals[ , xCol]))
		
		# base plot
		plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0.5, length(x)), ylim=ylim, tcl=-0.25)
		labelFig(lab, adj=figLabPos, cex=labCex)
		usr <- par('usr')
		
		# gray background
		left <- 1 - (2 + ifelse(is.null(controlField), 0.75, 0)) * nudge
		right <- length(x) + (2 + ifelse(is.null(controlField), 0.25, 0)) * nudge
		polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col=panelBgCol, border=NA, xpd=NA)
		lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.8 * lwd, xpd=NA)
		for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		for (i in 1:(length(x) - 1)) lines(x=c(i + 0.5, i + 0.5), y=c(-1, 1), col='white', lwd=0.5, xpd=NA)

		# x: axis labels
		axis(1, at=seq_along(x), labels=rep('', length(x)), tck=-0.02, lwd=lwd)
		xLabs <- if (!is.null(decs)) { sprintf(paste0('%.', decs, 'f'), x) } else { x }
		text(seq_along(x), y=rep(usr[3] + xlabY1 * (usr[4] - usr[3]), length(x)), labels=xLabs, cex=0.8 * labCex, xpd=NA, srt=0, pos=1, col='black')
		text(mean(seq_along(x)), y=usr[3] + xlabY2 * (usr[4] - usr[3]), labels=xlab, cex=labCex, xpd=NA, srt=0, col='black')
	
		# y: y-axis labels
		axis(2, at=yTicks, labels=yTicks, tck=-0.03, lwd=lwd)
		text(usr[1] + ylabX1 * (usr[2] - usr[1]), y=mean(yTicks), label='\U2190important       unimportant\U2192', srt=90, cex=0.9 * labCex, xpd=NA)
		text(usr[1] + ylabX2 * (usr[2] - usr[1]), y=mean(yTicks), label=ylab, srt=90, cex=labCex, xpd=NA)

		thisNudge <- length(algos) / 2
		
		# for each value of x
		for (countX in seq_along(x)) {
		
			thisX <- x[countX]

			# get data
			omniTrue1 <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, true1Field]
			omniTrue2 <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, true2Field]
			sdmTrue1 <- evals[evals$algo == algo & evals[ , xCol] == thisX, true1Field]
			sdmTrue2 <- evals[evals$algo == algo & evals[ , xCol] == thisX, true2Field]
		
			# if there is a distinct response for control/unperturbed models
			# used when using CBI or AUC
			if (!is.null(controlField)) {
		
				omniControl <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, controlField]
				sdmControl <- evals[evals$algo == algo & evals[ , xCol] == thisX, controlField]
			
				# unperturbed OMNI
				rect(omniControl, at=countX - nudge - subnudge, width=width, col=colOmniControl, border=NA, xpd=NA, lwd=rectLwd)
				rect(omniControl, at=countX - nudge - subnudge, width=width, density=lineDensity, col=colOmniControl, fill=colOmniControl, border=borderOmniControl, xpd=NA, lwd=rectLwd)
			
				# unperturbed SDM
				rect(sdmControl, at=countX - nudge + subnudge, width=width, col=colSdmControl, border=borderSdmControl, xpd=NA, lwd=rectLwd)

				leg <- c(
					'OMNI unpermuted',
					paste0('OMNI T1 permuted'),
					paste0('OMNI T2 permuted'),
					paste0(algosShort(algo), ' unpermuted'),
					paste0(algosShort(algo), ' T1 permuted'),
					paste0(algosShort(algo), ' T2 permuted')
				)
			
				par(lwd=0.5)
			
				legend('bottomleft', inset=c(0.03, 0.02), ncol=2, bty='n', legend=leg, cex=legCex, pch=22, pt.cex=0.9, pt.bg=c('white', 'white', 'white', colSdmControl, colSdmT1, colSdmT2), pt.lwd=rectLwd, col=c(borderOmniControl, borderOmniT1, borderOmniT2, borderSdmControl, borderSdmT1, borderSdmT2))
				
				adjust <- 0
				
			# if there is no distinct response for control/unperturbed
			# for when plotting correlation metric
			} else {

				omniControl <- sdmControl <- NULL
			
				# legend
				leg <- c(
					paste0('OMNI T1 permuted'),
					paste0('OMNI T2 permuted'),
					paste0(algosShort(algo), ' T1 permuted'),
					paste0(algosShort(algo), ' T2 permuted')
				)
			
				par(lwd=0.5)

				legend('bottomright', inset=c(0, 0.025), ncol=2, bty='n', legend=leg, cex=legCex, fill=c('white', 'white', colSdmT1, colSdmT2), border=c(borderOmniT1, borderOmniT2, borderSdmT1, borderSdmT2), xpd=NA)
				
				adjust <- 1/2 * nudge
				
			}
				
			### TRUE1 response
			rect(omniTrue1, at=countX - adjust - subnudge, width=width, col='white', border=borderSdmT1, xpd=NA, lwd=rectLwd)
			rect(sdmTrue1, at=countX - adjust + subnudge, width=width, col=colSdmT1, border=borderSdmT1, xpd=NA, lwd=rectLwd)

			### TRUE2 response
			rect(omniTrue2, at=countX - adjust + nudge - subnudge, width=width, col='white', border=borderSdmT2, xpd=NA, lwd=rectLwd)
			rect(sdmTrue2, at=countX - adjust + nudge + subnudge, width=width, col=colSdmT2, border=borderSdmT2, xpd=NA, lwd=rectLwd)

			# ### accuracy indicators
			# if (!is.na(expectHigher)) {
			
				# if (!all(is.na(sdmTrue)) & !all(is.na(sdmFalse))) {
		
					# acc <- character()
		
					# # discrimination
					# wellDiscrim <- discriminatedTrueFalse(sdmTrue, sdmFalse, expectHigher=expectHigher) & discriminatedTrueFalse(omniTrue, omniFalse, expectHigher=expectHigher)
					# if (!is.na(wellDiscrim) && wellDiscrim) acc <- c(acc, discrimSymbol)
					
					# # calibration
					# wellCalib <- calibratedTrueFalse(omniControl=omniControl, omniTrue=omniTrue, omniFalse=omniFalse, sdmControl=sdmControl, sdmTrue=sdmTrue, sdmFalse=sdmFalse, calibTol=calibTol)
					# if (!is.na(wellCalib) && wellCalib) acc <- c(acc, calibSymbol)
					
					# text(countX, ylim[2] + 0.12 * diff(ylim), labels=paste(acc, collapse=' '), xpd=NA, cex=1.3 * labCex, pos=1)
					
				# }
				
			# }
				
		} # next value along x axis


	}
	
	############################################################################
	### plot rectangle representing inner x-th quantile of response variable ###
	############################################################################
	
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
		lines(x=c(left, right), y=c(med, med), col=border, xpd=NA, lwd=0.9, lend=1)
		
	}

	### TEST IF RESULTS DISCRIMINATE BETWEEN TRUE/FALSE
	# This function returns TRUE if the results indicate the SDM can differentiate between TRUE and FALSE
	# Responses are discriminated successfully if their inner 95% quantile distributions do not overlap
	discriminatedTrueFalse <- function(
		predTrue,			# results from TRUE
		predFalse,			# results from FALSE
		expectHigher		# logical, if test can discriminate, which values should be higher (TRUE or FALSE)... expected values are TRUE or FALSE
	) { 
		if (expectHigher) {
			(quantile(predTrue, 0.025, na.rm=TRUE) > quantile(predFalse, 0.975, na.rm=TRUE))
		} else if (!expectHigher) {
			(quantile(predTrue, 0.975, na.rm=TRUE) < quantile(predFalse, 0.025, na.rm=TRUE))
		}
	}
	
	### TEST IF RESULTS DISCRIMINATE BETWEEN TRUE1/TRUE2
	# This function returns TRUE if the results indicate the SDM can differentiate between TRUE1 and TRUE2
	# Responses are discriminated successfully if their inner 95% quantile distributions do not overlap
	discriminatedBivariate <- function(
		T1,			# results from TRUE1
		T2			# results from TRUE2
	) {
		(quantile(sdmT1, 0.975, na.rm=TRUE) < quantile(sdmT2, 0.025, na.rm=TRUE)) | # T2 higher
		(quantile(sdmT1, 0.025, na.rm=TRUE) > quantile(sdmT2, 0.975, na.rm=TRUE)) # T1 higher
	}
	
	### TEST IF RESULTS USING TRUE/FALSE VARIABLES ARE WELL-CALIBRATED
	### This function takes as arguments a set of results from treatment models (ie, model predictions with predictors permuted) and possibly control models (predictions with data as-observed). It tests if the SDM's range and median values are different from the OMNI models' range and median by a given tolerance. Output is logical (TRUE if well-calibrated, FALSE if not).
	### For a given response range to be considered well-calibrated, it must be within +- 100 * calibTol% of the range of the OMNI model's range (eg if calibTol = 0.1 it must be within +- 10% of the OMNI models' range)
	### For a given response's median to be considered well-calibrated, it must be within the innermost +-100 * calibTol% quantile (eg, if calibTol = 0.1, it must be within the 40th and 60th quantiles)
	calibratedTrueFalse <- function(
		omniControl,		# all response values for OMNI control model (NULL if none)
		omniTrue,			# all response values for OMNI with TRUE treatment
		omniFalse,			# all response values for OMNI with FALSE treatment
		sdmControl,			# all response values for SDM control model (NULL if none)
		sdmTrue,			# all response values for SDM with TRUE treatment
		sdmFalse,			# all response values for Sdm with FALSE treatment
		calibTol = 0.1		# maximum proportional difference between range and median necessary for results
							# from an SDM to be considered as "well-calibrated"
	) {	
	
		ok <- TRUE
	
		### calibration with TRUE, FALSE, and CONTROL results (ie, CBI or AUC tests)
		if (!is.null(omniControl) & !is.null(sdmControl)) {

			# control range
			omniControlRange <- diff(quantile(omniControl, c(0.025, 0.975), na.rm=TRUE))
			sdmControlRange <- diff(quantile(sdmControl, c(0.025, 0.975), na.rm=TRUE))
			ok <- ok * (sdmControlRange >= (1 - calibTol) * omniControlRange & sdmControlRange <= (1 + calibTol) * omniControlRange)

			# control median
			omniMedianQuant <- quantile(omniControl, c(0.5 - calibTol, 0.5 + calibTol), na.rm=TRUE)
			sdmMedian <- median(sdmControl, na.rm=TRUE)
			ok <- ok * (sdmMedian >= omniMedianQuant[1] & sdmMedian <= omniMedianQuant[2])
			
		}	
		
		# TRUE range
		omniTrueRange <- diff(quantile(omniTrue, c(0.025, 0.975), na.rm=TRUE))
		sdmTrueRange <- diff(quantile(sdmTrue, c(0.025, 0.975), na.rm=TRUE))
		ok <- ok * (sdmTrueRange >= (1 - calibTol) * omniTrueRange & sdmTrueRange <= (1 + calibTol) * omniTrueRange)

		# TRUE median
		omniMedian <- median(omniTrue, na.rm=TRUE)
		sdmMedian <- median(sdmTrue, na.rm=TRUE)
		ok <- ok * (sdmMedian >= omniMedian - calibTol * omniTrueRange & sdmMedian <= omniMedian + calibTol * omniTrueRange)
		
		# FALSE range
		omniFalseRange <- diff(quantile(omniFalse, c(0.025, 0.975), na.rm=TRUE))
		sdmFalseRange <- diff(quantile(sdmFalse, c(0.025, 0.975), na.rm=TRUE))
		ok <- ok * (sdmFalseRange >= (1 - calibTol) * omniFalseRange & sdmFalseRange <= (1 + calibTol) * omniFalseRange)

		# FALSE median
		omniMedian <- median(omniFalse, na.rm=TRUE)
		sdmMedian <- median(sdmFalse, na.rm=TRUE)
		ok <- ok * (sdmMedian >= omniMedian - calibTol * omniFalseRange & sdmMedian <= omniMedian + calibTol * omniFalseRange)
		
		ok
		
	}

	### TEST IF RESULTS USING TRUE1/TRUE2 VARIABLES ARE WELL-CALIBRATED
	### This function takes as arguments a set of results from treatment models (ie, model predictions with predictors permuted) and possibly control models (predictions with data as-observed). It tests if the SDM's range and median values are different from the OMNI models' range and median by a given tolerance. Output is logical (TRUE if well-calibrated, FALSE if not).
	### For a given response range to be considered well-calibrated, it must be within +- 100 * calibTol% of the range of the OMNI model's range (eg if calibTol = 0.1 it must be within +- 10% of the OMNI models' range)
	### For a given response's median to be considered well-calibrated, it must be within the innermost +-100 * calibTol% quantile (eg, if calibTol = 0.1, it must be within the 40th and 60th quantiles)
	calibratedBivariate <- function(
		omniControl,		# all response values for OMNI control model (NULL if none)
		omniT1,				# all response values for OMNI with TRUE treatment
		omniT2,				# all response values for OMNI with FALSE treatment
		sdmControl,			# all response values for SDM control model (NULL if none)
		sdmT1,				# all response values for SDM with TRUE treatment
		sdmT2,				# all response values for Sdm with FALSE treatment
		calibTol = 0.1		# maximum proportional difference between range and median necessary for results
							# from an SDM to be considered as "well-calibrated"
	) {	
	
		ok <- TRUE
	
		### calibration with TRUE, FALSE, and CONTROL results (ie, CBI or AUC tests)
		if (!is.null(omniControl) & !is.null(sdmControl)) {

			# control range
			omniControlRange <- diff(quantile(omniControl, c(0.025, 0.975), na.rm=TRUE))
			sdmControlRange <- diff(quantile(sdmControl, c(0.025, 0.975), na.rm=TRUE))
			ok <- ok * (sdmControlRange >= (1 - calibTol) * omniControlRange & sdmControlRange <= (1 + calibTol) * omniControlRange)

			# control median
			omniMedianQuant <- quantile(omniControl, c(0.5 - calibTol, 0.5 + calibTol), na.rm=TRUE)
			sdmMedian <- median(sdmControl, na.rm=TRUE)
			ok <- ok * (sdmMedian >= omniMedianQuant[1] & sdmMedian <= omniMedianQuant[2])
			
		}	
		
		# TRUE1 range
		omniT1Range <- diff(quantile(omniT1, c(0.025, 0.975), na.rm=TRUE))
		sdmT1Range <- diff(quantile(sdmT1, c(0.025, 0.975), na.rm=TRUE))
		ok <- ok * (sdmT1Range >= (1 - calibTol) * omniT1Range & sdmT1Range <= (1 + calibTol) * omniT1Range)

		# TRUE1 median
		omniMedian <- median(omniT1, na.rm=TRUE)
		sdmMedian <- median(sdmT1, na.rm=TRUE)
		ok <- ok * (sdmMedian >= omniMedian - calibTol * omniT1Range & sdmMedian <= omniMedian + calibTol * omniT1Range)
		
		# TRUE2 range
		omniT2Range <- diff(quantile(omniT2, c(0.025, 0.975), na.rm=TRUE))
		sdmT2Range <- diff(quantile(sdmT2, c(0.025, 0.975), na.rm=TRUE))
		ok <- ok * (sdmT2Range >= (1 - calibTol) * omniT2Range & sdmT2Range <= (1 + calibTol) * omniT2Range)

		# TRUE2 median
		omniMedian <- median(omniT2, na.rm=TRUE)
		sdmMedian <- median(sdmT2, na.rm=TRUE)
		ok <- ok * (sdmMedian >= omniMedian - calibTol * omniT2Range & sdmMedian <= omniMedian + calibTol * omniT2Range)
		
		ok
		
	}

# say('###################################')
# say('### [simple] simulation results ###')
# say('###################################')

	# scenarioDir <- './Results/simple'
	# evalDir <- paste0(scenarioDir, '/evaluations')

	# evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	# # generalization
	# width <- 0.1 # bar width
	# nudge <- 0.06 # nudge left/right
	# figLabPos <- c(-0.28, 0.03) # position of figure label
	
	# ylabX1 <- -0.14 # position of inner y-axis label
	# ylabX2 <- -0.21 # position of outer y-axis label
	# labCex <- 0.56 # size of algorithm, y-axis, and figure labels
	
	# # par settings
	# oma <- rep(0, 4)
	# mar <- c(1.2, 1.6, 0.6, 0.3)
	# mgp <- c(2, 0.14, 0)
	# cex.axis <- 0.4
	# lwd <- 0.6
	
	# # image size
	# figWidth <- 1000
	# figHeight <- 1000
		
	# # master plot function
	# plotSimpleResp <- function(nudge, ylim, yTicks, ylab, lab, rand, trueField, controlField, falseField, controlLab, omniHasFalse, expectHigher) {
		
		# # nudge 	amount to move bars in same group (algorithm) left or right
		# # ylim		y-axis limits
		# # ylab		y-axis label
		# # yTicks	position of tick marks on y-axis
		# # lab		figure label
		# # rand		value of response equal to "random prediction" (eg 0.5 for AUC or 0 for CBI)
		# # trueField	field name of response for TRUE variable
		# # controlField	field name of response for control case (or NULL if none)
		# # falseField	field name of response for FALSE variable
		# # controlLab character, name of bar representing "control" model/prediction
		# # omniHasFalse   logical, if TRUE then test discrimination and calibration accuracy of OMNI TRUE versus FALSE (TRUE for multivariate tests using performance metrics and FALSE for univariate tests using performance metrics)... FALSE for CORpa and CORbg
		# # expectHigher if test can discriminate, should values of TRUE or FALSE be higher ('true', 'false')
		
		# # # adjust nudging of bars in same groups
		# # if (is.null(controlField)) nudge <- nudge / 2
		
		# # base plot
		# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0, 1), ylim=ylim)
		# labelFig('a) Simple', adj=figLabPos, cex=0.9 * labCex, xpd=NA)
		# usr <- par('usr')

		# # gray background
		# left <- 0
		# right <- 1
		# polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col=panelBgCol, border=NA, xpd=NA)
		# lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.4, xpd=NA)
		# for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		
		# # x: variable labels
		# if (!is.null(controlField)) {
			# axis(1, at=c(0.15, 0.5, 0.85), labels=c('Unpermuted', 'TRUE\npermuted', 'FALSE\npermuted'), tck=-0.02, lwd=0.5, cex.axis=0.75 * labCex)
		# } else {
			# axis(1, at=c(0.25, 0.75), labels=c('TRUE perm', 'FALSE perm'), tck=-0.02, lwd=0.5)
		# }

		# # y: y-axis labels
		# axis(2, at=yTicks, labels=yTicks, tck=-0.03, lwd=0.5)
		# text(usr[1] + ylabX1 * (usr[2] - usr[1]), y=mean(yTicks), label='\U2190important       unimportant\U2192', srt=90, cex=0.9 * labCex, xpd=NA)
		# text(usr[1] + ylabX2 * (usr[2] - usr[1]), y=mean(yTicks), label=ylab, srt=90, cex=labCex, xpd=NA)

		# # get this for evaluating calibration/discrimination accuracy
		# omniControl <- if (!is.null(controlField)) {
			# evals[evals$algo=='omniscient', controlField]
		# } else {
			# NULL
		# }
		
		# omniTrue <- evals[evals$algo=='omniscient', trueField]
		# omniFalse <- evals[evals$algo=='omniscient', falseField]

		# # responses
		# algo <- algos[2]
	
		# sdmControl <- if (!is.null(controlField)) {
			# evals[evals$algo==algo, controlField]
		# } else {
			# NULL
		# }

		# sdmTrue <- evals[evals$algo==algo, trueField]
		# sdmFalse <- evals[evals$algo==algo, falseField]

		# # unperturbed OMNI and SDM
		# if (!is.null(omniControl)) {
		
			# x <- 0.15
			# rect(omniControl, at=x - nudge, width=width, col=colOmniControl, border=borderOmniControl, xpd=NA, lwd=rectLwd)
			# rect(sdmControl, at=x +  nudge, width=width, col=colSdmControl, border=borderSdmControl, xpd=NA, lwd=rectLwd)

		# }
			
		# x <- if (!is.null(omniControl)) { 0.5 } else { 0.33 }
		# rect(omniTrue, at=x - nudge, width=width, col='white', border=borderTrue, xpd=NA, lwd=rectLwd)
		# rect(sdmTrue, at=x +  nudge, width=width, col=colTrue, border=borderTrue, xpd=NA, lwd=rectLwd)
		
		# x <- if (!is.null(omniControl)) { 0.85 } else { 0.66 }
		# rect(omniFalse, at=x - nudge, width=width, col='white', border=borderFalse, xpd=NA, lwd=rectLwd)
		# rect(sdmFalse, at=x +  nudge, width=width, col=colFalse, border=borderFalse, xpd=NA, lwd=rectLwd)
		
	# }

	# ### multivariate: CBI
	# #####################
	
	# png(paste0(scenarioDir, '/', toupper(sdmAlgo), ' Multivariate CBI.png'), width=figWidth, height=figHeight, res=600)
		
		# par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		# lab <- bquote('')
		# ylab <- bquote('CBI')
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# trueField <- 'cbiMulti_permT1'
		# controlField <- 'cbiMulti'
		# falseField <- 'cbiMulti_permF1'
		# rand <- 0
		# omniHasFalse <- TRUE
		# expectHigher <- FALSE

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
	# dev.off()

	# ### multivariate: AUCpa
	# #######################
	
	# png(paste0(scenarioDir, '/', toupper(sdmAlgo), ' Multivariate AUCpa.png'), width=figWidth, height=figHeight, res=600)
		
		# par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		# lab <- bquote('')
		# ylab <- bquote('AUC'['pa'])
		# ylim <- c(0, 1)
		# yTicks <- seq(0, 1, by=0.25)
		# trueField <- 'aucPresAbsMulti_permT1'
		# controlField <- 'aucPresAbsMulti'
		# falseField <- 'aucPresAbsMulti_permF1'
		# rand <- 0.5
		# omniHasFalse <- TRUE
		# expectHigher <- FALSE

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
	# dev.off()

	# ### multivariate: AUCbg
	# #######################
	
	# png(paste0(scenarioDir, '/', toupper(sdmAlgo), ' AUCbg.png'), width=figWidth, height=figHeight, res=600)
		
		# par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		# lab <- bquote('')
		# ylab <- bquote('AUC'['bg'])
		# ylim <- c(0, 1)
		# yTicks <- seq(0, 1, by=0.25)
		# trueField <- 'aucPresBgMulti_permT1'
		# controlField <- 'aucPresBgMulti'
		# falseField <- 'aucPresBgMulti_permF1'
		# rand <- 0.5
		# omniHasFalse <- TRUE
		# expectHigher <- FALSE

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
	# dev.off()

	# ### multivariate: CORpa
	# #######################
	
	# png(paste0(scenarioDir, '/', toupper(sdmAlgo), ' CORpa.png'), width=figWidth, height=figHeight, res=600)
		
		# par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		# lab <- bquote('')
		# ylab <- bquote('COR'['pa'])
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# trueField <- 'corPresAbsMulti_permT1'
		# controlField <- NULL
		# falseField <- 'corPresAbsMulti_permF1'
		# rand <- 0
		# expectHigher <- FALSE

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
	# dev.off()

	# ### multivariate: CORbg
	# #######################
	
	# png(paste0(scenarioDir, '/', toupper(sdmAlgo), ' CORbg.png'), width=figWidth, height=figHeight, res=600)
		
		# par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		# lab <- bquote('')
		# ylab <- bquote('COR'['bg'])
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# trueField <- 'corPresBgMulti_permT1'
		# controlField <- NULL
		# falseField <- 'corPresBgMulti_permF1'
		# rand <- 0
		# omniHasFalse <- FALSE
		# expectHigher <- FALSE
		
		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
	# dev.off()

# say('########################################')
# say('### [sample size] simulation results ###')
# say('########################################')

	# # generalization
	# scenarioDir <- './Results/sample size' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'numTrainPres' # name of x-axis variable column in evaluation data frame
	# decs <- 0 # number of decimals to show in x-axis variable tick mark labels
	# xlab <- 'Number of calibration presences' # x-axis label
	# lab <- 'b) Training sample size'
	# expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# # responses to plot
	# resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	# # plot results for each response
	# multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, xlab=xlab, lab=lab, evals=evals, resps=resps, expectHigher=expectHigher)

# say('#######################################')
# say('### [prevalence] simulation results ###')
# say('#######################################')

	# # generalization
	# scenarioDir <- './Results/prevalence' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'prevalence' # name of x-axis variable column in evaluation data frame
	# decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	# lab <- 'a) Prevalence' # plot label
	# xlab <- 'Prevalence' # x-axis label
	# expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# # responses to plot
	# resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	
	# # plot multivariate model results
	# multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, lab=lab, xlab=xlab, evals=evals, resps=resps, expectHigher=expectHigher)

# say('###################################')
# say('### [extent] simulation results ###')
# say('###################################')

	# # generalization
	# scenarioDir <- './Results/extent' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'rangeT1' # name of x-axis variable column in evaluation data frame
	# decs <- NULL # number of decimals to show in x-axis variable tick mark labels
	# lab <- 'b) Extent' # plot label
	# xlab <- 'Study region extent (range of TRUE)' # x-axis label
	# expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# # responses to plot
	# resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	# evals$rangeT1 <- evals$maxT1 - evals$minT1

	# # plot multivariate model results
	# multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, lab=lab, xlab=xlab, evals=evals, resps=resps, expectHigher=expectHigher)

# say('####################################################')
# say('### [correlated TRUE & FALSE] simulation results ###')
# say('####################################################')

	# # generalization
	# scenarioDir <- './Results/correlated TRUE & FALSE' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'correlation' # name of x-axis variable column in evaluation data frame
	# decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	# # lab <- 'a) Collinearity' # plot label
	# lab <- '' # plot label
	# xlab <- 'Correlation between TRUE and FALSE' # x-axis label
	# expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# # responses to plot
	# resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	# correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')
	# evals$correlation <- correlations$cor[match(evals$rotF1, correlations$rot)]
	
	# # plot multivariate model results
	# multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, lab=lab, xlab=xlab, evals=evals, resps=resps, expectHigher=expectHigher)

# say('#######################################')
# say('### [resolution] simulation results ###')
# say('#######################################')

	# say('Layout:')
	# say('3 columns (resolution) and 4 rows (SAC).')
	# say('Each subpanel is a barplot showing response of OMNI and the SDM.')

	# # generalization
	# scenarioDir <- './Results/resolution' # scenario directory

	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(
		# evalDir=paste0(scenarioDir, '/evaluations'),
		# algos=algos,
		# save=TRUE,
		# redo=FALSE
	# )
	
	# # SAC
	# noises <- c(0, 1/3, 2/3, 1)
	# noisesNeat <- c('0', '1/3', '2/3', '1')
	
	# # grain size
	# grains <- c(2^14, 2^10, 2^6)

	# # plot settings
	# xSize <- 0.775 # maximum width of subplot containing bars
	# ySize <- 0.875 # maximum height of subplot containing bars
	# width <- 0.17 # width of bars as a proportion of subplot size (real width will be size * width)
	# tick <- 0.05 # length of subplot tick marks
	# lwd <- 0.4 # line width of bars
	# cexAxisLabel <- 0.5
	# cexPanelLabel <- 0.7
	# labCex <- 0.45 # size of algorithm, y-axis, and figure labels	
	# axisCex <- 0.37
	
	# lineDensity <- NULL
	
	# # function to plots bars as scaled subplots in a larger plot
	# # basically this just rescales the size and position of values and bars and send the information to rect()
	# scaleRespToSubplot <- function(resp, angle, xOffsetInSubplot, col, border, ...) {
	
		# # resp		values of response (not scaled)
		# # angle		NULL or angle of fill lines
		# # xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# # col, border  color for fill and border
		# # ...		other
	
		# respScaled <- resp * 0.5 * ySize + subplotPosY - 0 * 0.5 * ySize
		# at <- countGrain - 0.5 * xSize + xOffsetInSubplot * xSize
		# rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=col, border=border, angle=angle, density=s * lineDensity, lwd=rectLwd)
		
	# }
	
	# ### plot CBI, AUCpa, AUCbg
	# ##########################
	
	# respStatistics <- c('CBI', 'AUCpa', 'AUCbg')
	# respControls <- c('cbiMulti', 'aucPresAbsMulti', 'aucPresBgMulti')
	# resps <- c('cbiMulti_perm', 'aucPresAbsMulti_perm', 'aucPresBgMulti_perm')
	# expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination
	
	# # y position of guidelines in subplots
	# respRands <- c(0, 0.5, 0.5)
	# respMins <- c(-1, 0, 0)

	# # by TEST STATISTIC
	# for (countStat in seq_along(respStatistics)) {
	# # for (countStat in 1) {
	
		# respStatistic <- respStatistics[countStat]
		# controlField <- respControls[countStat]
		# resp <- resps[countStat]
	
		# respRand <- respRands[countStat]
		# respMin <- respMins[countStat]
		
		# png(paste0(scenarioDir, '/', toupper(sdmAlgo), ' ', respStatistic, '.png'), width=1000, height=1400, res=600)
		
			# par(oma=c(0, 0, 0, 0), mar=c(0.4, 1.8, 0.1, 0), mgp=c(1, 0, 0), cex.axis=axisCex, tcl=-0.2)

			# algo <- algos[2]
		
			# algoNice <- algosShort(algo)
	
			# xs <- seq_along(grains)
			# ys <- seq_along(noises)
			# xlim <- range(xs) + c(-0.5, 0.5)
			# ylim <- range(ys) + c(-0.5, 0.5)
			
			# ylab <- 'Spatial autocorrelation\n(proportion of cells swapped)\n\U2190lower autocorrelation     higher autocorrelation\U2192'
			
			# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
			# axis(1, at=xs, labels=NA, tck=-0.01, lwd=lwd, line=-0.45)
			# text(xs, y=rep(0.4, length(grains)), labels=paste0('1/', grains), xpd=NA, cex=axisCex)
			# axis(2, at=ys, labels=rev(noisesNeat), tck=-0.015, lwd=lwd, line=-0.03)
			# mtext('Grain size', side=1, cex=0.9 * labCex, line=-0.41)
			# labelFig('c) Spatial resolution and autocorrelation', adj=c(-0.2, -0.02), cex=0.9 * labCex, xpd=NA)
			
			# # by NOISE (SAC)
			# for (countNoise in seq_along(noises)) {
		
				# noise <- noises[countNoise]
				# subplotPosY <- length(noises) + 1 - countNoise # y-axis position of subplot
		
				# # by GRAIN
				# for (countGrain in seq_along(grains)) {
				
					# grain <- grains[countGrain]

					# # get response data
					# omniControl <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, controlField]
					# sdmControl <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, controlField]
					
					# omniTrue <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
					# omniFalse <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]
					
					# sdmTrue <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
					# sdmFalse <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]

					# # calculate and assign variables for lower/upper limits and median
					# whats <- c('Inner', 'Median', 'Outer')
					# for (modelType in c('sdm', 'omni')) {
						# for (variable in c('Control', 'True', 'False')) {
							
							# thisVar <- paste0(modelType, variable)
							# x <- get(thisVar)
							# quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

							# for (countWhat in seq_along(whats)) {
						
								# what <- whats[countWhat]
								# assign(paste0(thisVar, what), quants[countWhat])
								
							# }
						# }
					# }

					# s <- 0.8 # line density scalar for angled fills
					
					# # subplot y-axis
					# lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					
					# # subplot y-axis tick lines and labels
					# lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY + 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					# lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY, subplotPosY), lwd=lwd)
					# lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY - 0.5 * ySize), lwd=lwd)
					
					# # subplot y-axis labels
					# cex <- 0.3
					# offset <- 2.5
					# if (countGrain == 1) {

						# text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
						# text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY, labels=respRand, cex=cex, xpd=NA)
						# text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY - 0.5 * ySize, labels=respMin, cex=cex, xpd=NA)
						
					# }
					
					# # gray background
					# offsetInSubplot <- 0.1
					# rand <- 0
					# left <- countGrain - 0.54 * xSize + offsetInSubplot * xSize
					# right <- countGrain + 0.5 * xSize + 1.75 * offsetInSubplot * xSize
					# bottom <- subplotPosY - 0.5 * ySize
					# top <- subplotPosY + 0.5 * ySize
					# polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col=panelBgCol, border=NA, xpd=NA)
					# lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

					# # OMNI control (unpermuted)
					# scaleRespToSubplot(
						# resp=omniControl,
						# angle=NULL,
						# xOffsetInSubplot=0.2,
						# col=colOmniControl,
						# border=borderOmniControl
					# )
					
					# # SDM control (unpermuted)
					# scaleRespToSubplot(
						# resp=sdmControl,
						# angle=NULL,
						# xOffsetInSubplot=0.35,
						# col=colSdmControl,
						# border=borderSdmControl
					# )
					
					# # OMNI permuted T1
					# scaleRespToSubplot(
						# resp=omniTrue,
						# angle=NULL,
						# xOffsetInSubplot=0.55,
						# col='white',
						# border=borderTrue
					# )

					# # SDM permuted T1
					# scaleRespToSubplot(
						# resp=sdmTrue,
						# angle=NULL,
						# xOffsetInSubplot=0.7,
						# col=colTrue,
						# border=borderTrue
					# )
					
					# # OMNI permuted F1
					# scaleRespToSubplot(
						# resp=omniFalse,
						# angle=NULL,
						# xOffsetInSubplot=0.9,
						# col='white',
						# border=borderFalse
					# )
					
					# # SDM permuted F1
					# scaleRespToSubplot(
						# resp=sdmFalse,
						# angle=NULL,
						# xOffsetInSubplot=1.05,
						# col=colFalse,
						# border=borderFalse
					# )

					# ## accuracy indicators
					# sdmTrueNas <- sum(!is.na(sdmTrue))
					# sdmFalseNas <- sum(!is.na(sdmFalse))
					# if (sdmTrueNas > 1 & sdmFalseNas > 1) {
			
						# acc <- character()
			
						# # discrimination
						# wellDiscrim <- discriminatedTrueFalse(sdmTrue, sdmFalse, expectHigher=expectHigher) & discriminatedTrueFalse(omniTrue, omniFalse, expectHigher=expectHigher)
						# if (wellDiscrim) acc <- c(acc, discrimSymbol)
	
						# # calibration
						# wellCalib <- calibratedTrueFalse(omniControl=omniControl, omniTrue=omniTrue, omniFalse=omniFalse, sdmControl=sdmControl, sdmTrue=sdmTrue, sdmFalse=sdmFalse, calibTol=calibTol)
						# if (wellCalib) acc <- c(acc, calibSymbol)
						
						# x <- countGrain + 0.85 * xSize
						# y <- bottom + 0.1 * ySize
						# text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1.5 * labCex, pos=2)
						
					# }
						
				# } # next grain

			# } # next noise level (SAC)

			# # panel y-axis labels
			# mtext(ylab, side=2, cex=0.9 * labCex, line=-1.41, outer=TRUE)

			# title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		
		# dev.off()

	# } # next test statistic

# say('#######################################')
# say('### [bivariate] collate evaluations ###')
# say('#######################################')

	# evalDir <- './Results/bivariate/evaluations'
	# evals <- loadEvals(evalDir, algos=c('omniscient', 'brt', 'gam', 'maxent'), save=TRUE, redo=FALSE)

# say('##############################')
# say('### [bivariate] statistics ###')
# say('##############################')

	# scenarioDir <- './Results/bivariate'
	# load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))

	# # prevalence: all scenarios
	# x <- round(max(master$prev), 2)
	# say('Maximum real prevalence (mean suitability) across all simulations: ', sprintf('%.2f', x), post=2, pre=2)

	# say('SYMMETRICAL NICHE WIDTH, NO CORRELATION, NO COVARIANCE', level=2)
		
		# # OMNI multivariate CBI for "control" (unpermuted) model
		# x1 <- round(median(master$cbiMulti[master$algo == 'omniscient' & master$sigma1 %==% 0.1 & master$sigma1 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90]), 2)
		# x2 <- round(median(master$cbiMulti[master$algo == 'omniscient' & master$sigma1 %==% 0.3 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90]), 2)
		# x3 <- round(median(master$cbiMulti[master$algo == 'omniscient' & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90]), 2)

		# say('Median CBI for OMNISCIENT, unpermuted for sigma1 = sigma2 = 0.1 and rho = 0: ', sprintf('%.2f', x1))
		# say('Median CBI for OMNISCIENT, unpermuted for sigma1 = sigma2 = 0.3 and rho = 0: ', sprintf('%.2f', x2))
		# say('Median CBI for OMNISCIENT, unpermuted for sigma1 = sigma2 = 0.5 and rho = 0: ', sprintf('%.2f', x3), post=2)

		# # multivariate CBI for "treatment" (permuted) model
		# for (algo in algos) {
		
			# # multivariate CBI for permuted models, symmetrical niches
			# say(toupper(algo), ', symmetrical niche width, unpermuted')
			# x1 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90], na.rm=TRUE), 2)
			# x2 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.3 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90], na.rm=TRUE), 2)
			# x3 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.1 & master$sigma2 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90], na.rm=TRUE), 2)

			# say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.5, rho = 0: ', sprintf('%.2f', x1))
			# say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.3, sigma2 = 0.3, rho = 0: ', sprintf('%.2f', x2))
			# say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.1, sigma2 = 0.1, rho = 0: ', sprintf('%.2f', x3), post=2)
			
		# }
		
	# say('*A*SYMMETRICAL NICHE WIDTH, NO CORRELATION, NO COVARIANCE, T1', level=2)

		# for (algo in algos) {
		
			# # multivariate CBI for permuted models, asymmetrical niches
			# say(toupper(algo), ', asymmetrical niche width, permuted')
			# x <- master$cbiMulti_permT1[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90]
			# x1med <- median(x, na.rm=TRUE)
			# x1low <- quantile(x, 0.025, na.rm=TRUE)
			# x1high <- quantile(x, 0.975, na.rm=TRUE)
			
			# x <- master$cbiMulti_permT1[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90]
			# x2med <- median(x, na.rm=TRUE)
			# x2low <- quantile(x, 0.025, na.rm=TRUE)
			# x2high <- quantile(x, 0.975, na.rm=TRUE)
			
			# x <- master$cbiMulti_permT1[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90]
			# x3med <- median(x, na.rm=TRUE)
			# x3low <- quantile(x, 0.025, na.rm=TRUE)
			# x3high <- quantile(x, 0.975, na.rm=TRUE)

			# say('Median CBI for ', toupper(algo), ', permuted T1, for sigma1 = 0.5, sigma2 = 0.5, rho = 0: ', sprintf('%.2f', x1med), ' (inner 95% CI: ', sprintf('%.2f', x1low), '-', sprintf('%.2f', x1high), ')')
			# say('Median CBI for ', toupper(algo), ', permuted T1, for sigma1 = 0.5, sigma2 = 0.3, rho = 0: ', sprintf('%.2f', x2med), ' (inner 95% CI: ', sprintf('%.2f', x2low), '-', sprintf('%.2f', x2high), ')')
			# say('Median CBI for ', toupper(algo), ', permuted T1, for sigma1 = 0.5, sigma2 = 0.1, rho = 0: ', sprintf('%.2f', x3med), ' (inner 95% CI: ', sprintf('%.2f', x3low), '-', sprintf('%.2f', x3high), ')', post=2)
			
		# }
			
	# say('*A*SYMMETRICAL NICHE WIDTH, NO CORRELATION, NO COVARIANCE, T2', level=2)

		# for (algo in algos) {
		
			# # multivariate CBI for permuted models, asymmetrical niches
			# say(toupper(algo), ', asymmetrical niche width, permuted')
			# x <- master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90]
			# x1med <- median(x, na.rm=TRUE)
			# x1low <- quantile(x, 0.025, na.rm=TRUE)
			# x1high <- quantile(x, 0.975, na.rm=TRUE)
			
			# x <- master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90]
			# x2med <- median(x, na.rm=TRUE)
			# x2low <- quantile(x, 0.025, na.rm=TRUE)
			# x2high <- quantile(x, 0.975, na.rm=TRUE)
			
			# x <- master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90]
			# x3med <- median(x, na.rm=TRUE)
			# x3low <- quantile(x, 0.025, na.rm=TRUE)
			# x3high <- quantile(x, 0.975, na.rm=TRUE)

			# say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.5, rho = 0: ', sprintf('%.2f', x1med), ' (inner 95% CI: ', sprintf('%.2f', x1low), '-', sprintf('%.2f', x1high), ')')
			# say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.3, rho = 0: ', sprintf('%.2f', x2med), ' (inner 95% CI: ', sprintf('%.2f', x2low), '-', sprintf('%.2f', x2high), ')')
			# say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.1, rho = 0: ', sprintf('%.2f', x3med), ' (inner 95% CI: ', sprintf('%.2f', x3low), '-', sprintf('%.2f', x3high), ')', post=2)
			
		# }

# say('####################################')
# say('### [bivariate] niche covariance ###')
# say('####################################')

	# # generalization
	# scenarioDir <- './Results/bivariate' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'rho' # name of x-axis variable column in evaluation data frame
	# decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	# filePrepend <- 'Niche Covariance' # prepended to file name
	# xlab <- bquote('Niche covariance (' * rho * ')') # x-axis label
	# lab <- 'b) Niche covariance'

	# # responses to plot
	# resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	# evals <- evals[evals$rotT2 %==% 90 & evals$sigma1 %==% 0.3 & evals$sigma2 %==% 0.3, ]

	# # plot multivariate model results
	# multivariatePlotsTRUEvsTRUE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, filePrepend=filePrepend, lab=lab, xlab=xlab, evals=evals, resps=resps)
	
# say('##########################################')
# say('### [niche breadth] simulation results ###')
# say('##########################################')

	# say('Layout:')
	# say('3 columns (niche width in T1) and 3 rows (niche width in T2).')
	# say('Each subpanel is a barplot showing response of OMNI and the SDM.')

	# # generalization
	# scenarioDir <- './Results/bivariate' # scenario directory

	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(
		# evalDir=paste0(scenarioDir, '/evaluations'),
		# algos=algos,
		# save=TRUE,
		# redo=FALSE
	# )
	
	# # niche widths
	# sigmas <- c(1, 3, 5) / 10
	
	# # specify which treatment
	# rho <- 0
	# rotT2 <- 90
	
	# # plot settings
	# xSize <- 0.775 # maximum width of subplot containing bars
	# ySize <- 0.875 # maximum height of subplot containing bars
	# width <- 0.17 # width of bars as a proportion of subplot size (real width will be size * width)
	# tick <- 0.05 # length of subplot tick marks
	# lwd <- 0.6 # line width of bars
	# cexAxisLabel <- 0.5
	# cexPanelLabel <- 0.7
	# labCex <- 0.51 # size of algorithm, y-axis, and figure labels	
	# axisCex <- 0.42
	
	# lineDensity <- NULL
	
	# # function to plots bars as scaled subplots in a larger plot
	# # basically this just rescales the size and position of values and bars and send the information to rect()
	# scaleRespToSubplot <- function(resp, angle, xOffsetInSubplot, col, border, ...) {
	
		# # resp		values of response (not scaled)
		# # angle		NULL or angle of fill lines
		# # xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# # col, border  color for fill and border
		# # ...		other
	
		# # respScaled <- resp * 0.5 * ySize + subplotPosY - 0 * 0.5 * ySize
		# respScaled <- resp * 1 * ySize + subplotPosY - 1 * 0.5 * ySize
		# at <- countSigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		# rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=col, border=border, angle=angle, density=NA, lwd=rectLwd)
		
	# }
	
	# ### plot CBI, AUCpa, AUCbg
	# ##########################
	
	# respStatistics <- c('CBI', 'AUCpa', 'AUCbg')
	# respControls <- c('cbiMulti', 'aucPresAbsMulti', 'aucPresBgMulti')
	# resps <- c('cbiMulti_perm', 'aucPresAbsMulti_perm', 'aucPresBgMulti_perm')
	# expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination
	
	# # y position of guidelines in subplots
	# respRands <- c(0, 0.5, 0.5)
	# respMins <- c(-1, 0, 0)

	# # by TEST STATISTIC
	# # for (countStat in seq_along(respStatistics)) {
	# for (countStat in 1) {
	
		# respStatistic <- respStatistics[countStat]
		# controlField <- respControls[countStat]
		# resp <- resps[countStat]
	
		# respRand <- respRands[countStat]
		# respMin <- respMins[countStat]
		
		# png(paste0(scenarioDir, '/Niche Breadth ', toupper(sdmAlgo), ' ', respStatistic, ' for rho = ', rho, ' and rotT2 = ', rotT2, '.png'), width=1050, height=1000, res=600)
		
			# par(oma=c(0, 0, 0, 0), mar=c(1, 1.1, 0.4, 0), mgp=c(1, 0, 0), cex.axis=axisCex, tcl=-0.2)

			# algo <- algos[2]
			# algoNice <- algosShort(algo)
	
			# ats <- seq_along(sigmas)
			# xlim <- range(ats) + c(-0.5, 0.5)
			# ylim <- range(ats) + c(-0.5, 0.5)
			
			# xlab <- bquote('Niche breadth in T1 (' * sigma[1] * ')')
			# ylab <- bquote('Niche breadth in T2 (' * sigma[2] * ')')
			
			# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
			# axis(1, at=ats, labels=NA, tck=-0.015, lwd=lwd, line=-0.18)
			# text(ats, y=rep(0.3, length(sigmas)), labels=sigmas, xpd=NA, cex=axisCex)
			# axis(2, at=ats, labels=sigmas, tck=-0.015, lwd=lwd, line=-0.03)
			# mtext(xlab, side=1, cex=labCex, line=0.18)
			# mtext(ylab, side=2, cex=labCex, line=0.4)
			# labelFig('a) Niche breadth', adj=c(-0.2, 0.02), cex=labCex, xpd=NA)
			
			# # by sigma2 (rows)
			# for (countSigma2 in seq_along(sigmas)) {
		
				# sigma2 <- sigmas[countSigma2]
				# subplotPosY <- countSigma2 # y-axis position of subplot
		
				# # by sigma1 (columns)
				# for (countSigma1 in seq_along(sigmas)) {
				
					# sigma1 <- sigmas[countSigma1]

					# # get response data: standard (as simulated)
					# if (sigma1 >= sigma2) {

						# omniControl <- evals$cbiMulti[evals$algo == 'omniscient' & evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# omniT1 <- evals$cbiMulti_permT1[evals$algo == 'omniscient' & evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# omniT2 <- evals$cbiMulti_permT2[evals$algo == 'omniscient' & evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]

						# sdmControl <- evals$cbiMulti[evals$algo == algo & evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# sdmT1 <- evals$cbiMulti_permT1[evals$algo == algo & evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# sdmT2 <- evals$cbiMulti_permT2[evals$algo == algo & evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]

					# # get response data: flipping T1 and T2 since symmetrical
					# } else if (sigma1 < sigma2) {

						# omniControl <- evals$cbiMulti[evals$algo == 'omniscient' & evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# omniT1 <- evals$cbiMulti_permT2[evals$algo == 'omniscient' & evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# omniT2 <- evals$cbiMulti_permT1[evals$algo == 'omniscient' & evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						
						# sdmControl <- evals$cbiMulti[evals$algo == algo & evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# sdmT1 <- evals$cbiMulti_permT2[evals$algo == algo & evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]
						# sdmT2 <- evals$cbiMulti_permT1[evals$algo == algo & evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho  %==% rho & evals$rotT2 %==% rotT2]

					# }

					# # calculate and assign variables for lower/upper limits and median
					# whats <- c('Inner', 'Median', 'Outer')
					# for (modelType in c('sdm', 'omni')) {
						# for (variable in c('Control', 'T1', 'T2')) {
							
							# thisVar <- paste0(modelType, variable)
							# x <- get(thisVar)
							# quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

							# for (countWhat in seq_along(whats)) {
						
								# what <- whats[countWhat]
								# assign(paste0(thisVar, what), quants[countWhat])
								
							# }
						# }
					# }

					# # s <- 0.8 # line density scalar for angled fills
					
					# # subplot y-axis
					# lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					
					# # subplot y-axis tick lines and labels
					# lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize - tick * xSize), c(subplotPosY + 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					# lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize - tick * xSize), c(subplotPosY, subplotPosY), lwd=lwd)
					# lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize - tick * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY - 0.5 * ySize), lwd=lwd)
					
					# # subplot y-axis labels
					# cex <- 0.3
					# offset <- 2.5
					# if (countSigma1 == 1) {

						# text(countSigma1 - 0.5 * xSize - offset * tick * xSize, subplotPosY + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
						# text(countSigma1 - 0.5 * xSize - offset * tick * xSize, subplotPosY, labels=respRand, cex=cex, xpd=NA)
						# text(countSigma1 - 0.5 * xSize - offset * tick * xSize, subplotPosY - 0.5 * ySize, labels=respMin, cex=cex, xpd=NA)
						
					# }
					
					# # gray background
					# offsetInSubplot <- 0.1
					# rand <- 0
					# left <- countSigma1 - 0.54 * xSize + offsetInSubplot * xSize
					# right <- countSigma1 + 0.5 * xSize + 1.75 * offsetInSubplot * xSize
					# bottom <- subplotPosY - 0.5 * ySize
					# top <- subplotPosY + 0.5 * ySize
					# polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col=panelBgCol, border=NA, xpd=NA)
					# lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

					# # y <- subplotPosY + 0.25 * ySize
					# # lines(c(left, right), c(y, y), lwd=1 * lwd, col='white')

					# # y <- subplotPosY - 0.25 * ySize
					# # lines(c(left, right), c(y, y), lwd=1 * lwd, col='white')

					# # OMNI control (unpermuted)
					# scaleRespToSubplot(
						# resp=omniControl,
						# angle=NULL,
						# xOffsetInSubplot=0.2,
						# col=colOmniControl,
						# border=borderOmniControl
					# )
					
					# # SDM control (unpermuted)
					# scaleRespToSubplot(
						# resp=sdmControl,
						# angle=NULL,
						# xOffsetInSubplot=0.35,
						# col=colSdmControl,
						# border=borderSdmControl
					# )
					
					# # OMNI permuted T1
					# scaleRespToSubplot(
						# resp=omniT1,
						# angle=NULL,
						# xOffsetInSubplot=0.55,
						# col='white',
						# border=borderOmniT1
					# )

					# # SDM permuted T1
					# scaleRespToSubplot(
						# resp=sdmT1,
						# angle=NULL,
						# xOffsetInSubplot=0.7,
						# col=colSdmT1,
						# border=borderSdmT1
					# )
					
					# # OMNI permuted T2
					# scaleRespToSubplot(
						# resp=omniT2,
						# angle=NULL,
						# xOffsetInSubplot=0.9,
						# col='white',
						# border=borderOmniT2
					# )

					# # SDM permuted T2
					# scaleRespToSubplot(
						# resp=sdmT2,
						# angle=NULL,
						# xOffsetInSubplot=1.05,
						# col=colSdmT2,
						# border=borderSdmT2
					# )
					

					# ## accuracy indicators
					# sdmT1Nas <- sum(!is.na(sdmT1))
					# sdmT2Nas <- sum(!is.na(sdmT2))
					# if (sdmT1Nas > 1 & sdmT2Nas > 1) {
			
						# acc <- character()
			
						# # discrimination
						# wellDiscrim <- discriminatedBivariate(omniT1, omniT2) & discriminatedBivariate(sdmT1, sdmT2)
						# if (wellDiscrim) acc <- c(acc, discrimSymbol)
	
						# # calibration
						# wellCalib <- calibratedBivariate(omniControl=omniControl, omniT1=omniT1, omniT2=omniT2, sdmControl=sdmControl, sdmT1=sdmT1, sdmT2=sdmT2, calibTol=calibTol)
						# if (wellCalib) acc <- c(acc, calibSymbol)
						
						# x <- countSigma1 + 0.14 * xSize
						# y <- bottom + 0.1 * ySize
						# text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1.5 * labCex, pos=2)
						
					# }
						
				# } # next sigma1

			# } # next sigma2

			# title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		
		# dev.off()

	# } # next test statistic
		
#################################
say('DONE!!!', level=1, deco='&')
say(date()) #####################
#################################

