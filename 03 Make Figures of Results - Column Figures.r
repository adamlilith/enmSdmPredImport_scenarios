### SDM PREDICTOR INFERENCE - ILLUSTRATIONS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
### source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/03 Make Figures of Results - Column Figures.r')
###
### The code in this document is intended to be run after all models have been calibrated and evaluated. Most of the sections run extremely quickly except for the section that collates evaluation results for the [bivariate] experiment ("### [bivariate] collate evaluations ###") which can take several hours, depending on the number of scenarios modeled. The script contains code to flag cases where tests can discriminate between TRUE and FALSE variables of two TRUE variables, as well as cases where the results are well-calibrated with the omniscient model.

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
### [bivariate] landscape correlation x niche covariance bar plots for CBI ###
### [bivariate] landscape correlation x niche covariance bar plots for AUC ###
### [bivariate] landscape correlation x niche covariance bar plots for CORpa ###
### [bivariate] landscape correlation x niche covariance bar plots for CORbg ###

### [extra] investigating decline in performance of OMNI control at large extents ###
### [extra] sensitivity of CBI to outliers ###

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

	# algorithms
	algos <- c('omniscient', 'gam', 'maxent', 'brt')
	sdmAlgos <- c('gam', 'maxent', 'brt')

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
	colSdmT1 <- colOmniT1 <- '#a6dba0' # light green (CB safe)
	borderSdmT1 <- borderOmniT1 <- '#008837' # dark green (CB safe)
	
	colSdmT2 <- colOmniT2 <- '#c2a5cf' # light purple (CB safe)
	borderSdmT2 <- borderOmniT2 <- '#7b3294' # dark purple (CB safe)

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
		evals,							# data frame with evaluations
		expectHigher,					# TRUE or FALSE or NULL... which variable should have a higher value of
										# the test statistic if the variables can be discriminated accurately?
										# (TRUE or FALSE or NULL for neither)
		resps=c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg') # responses to plot
	) {
		
		for (resp in resps) {
			
			if (resp == 'Multivariate CBI') {
				
				ylim <- c(-1, 1)
				yTicks <- seq(-1, 1, by=0.25)
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
			
			png(paste0(scenarioDir, '/', resp, ' - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=4 * 250, height=4 * 720, res=600)
				
				par(mfrow=c(3, 1), oma=c(0, 0, 0, 0), mar=c(1.6, 2, 1, 0.5), mgp=c(2, 0.2, 0), cex.axis=0.425, tcl=-0.25)
				
				for (countAlgo in seq_along(sdmAlgos)) {

					algo <- sdmAlgos[countAlgo]
				
					lab <- paste0(letters[countAlgo], ') ', algosShort(algo))
					
					plotScalarRespTRUEvsFALSE(xCol=xCol, decs=decs, xlab=xlab, algo=algo, nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab=ylab, lab=lab, rand=rand, trueField=trueField, falseField=falseField, controlField=controlField, expectHigher=expectHigher)

				}
				
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
			
			png(paste0(scenarioDir, '/', resp, ' - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=4 * 250, height=4 * 720, res=600)
				
				par(mfrow=c(3, 1), oma=c(0, 0, 0, 0), mar=c(1.6, 2, 1, 0.5), mgp=c(2, 0.2, 0), cex.axis=0.425, tcl=-0.25)
				
				for (countAlgo in seq_along(sdmAlgos)) {

					algo <- sdmAlgos[countAlgo]
				
					lab <- paste0(letters[countAlgo], ') ', algosShort(algo))
					
					plotScalarRespTRUEvsTRUE(xCol=xCol, decs=decs, xlab=xlab, algo=algo, nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab=ylab, lab=lab, rand=rand, true1Field=true1Field, true2Field=true2Field, controlField=controlField)

				}
				
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
		width <- 0.14 # bar width
		nudge <- 0.26 # nudge pair of bars for same algorithm left/right
		subnudge <- nudge / 4 # nudge bars within same class
		lwd <- 0.8 # line width of box borders
		figLabPos <- c(-0.2250, 0.03) # position of figure label
		
		legCex <- 0.55 # legend
		
		ylabX1 <- -0.105 # position of inner y-axis label
		ylabX2 <- -0.165 # position of outer y-axis label
		labCex <- 0.65 # size of algorithm, y-axis, and figure labels
		
		xlabY1 <- -0 # position of inner x-axis sublabels (range of TRUE)
		xlabY2 <- -0.14 # position of outer x-axis label
		
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
		polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col='gray85', border=NA, xpd=NA)
		lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.8 * lwd, xpd=NA)
		for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		for (i in 1:(length(x) - 1)) lines(x=c(i + 0.5, i + 0.5), y=c(-1, 1), col='white', lwd=0.5, xpd=NA)

		# x: axis labels
		axis(1, at=seq_along(x), labels=rep('', length(x)), tck=-0.03, lwd=0.8)
		xLabs <- if (!is.null(decs)) { sprintf(paste0('%.', decs, 'f'), x) } else { x }
		text(seq_along(x), y=rep(usr[3] + xlabY1 * (usr[4] - usr[3]), length(x)), labels=xLabs, cex=0.8 * labCex, xpd=NA, srt=0, pos=1, col='black')
		text(mean(seq_along(x)), y=usr[3] + xlabY2 * (usr[4] - usr[3]), labels=xlab, cex=labCex, xpd=NA, srt=0, col='black')
	
		# y: y-axis labels
		axis(2, at=yTicks, labels=yTicks, tck=-0.03, lwd=0.8)
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
				rect(omniControl, at=countX - nudge - subnudge, width=width, col=colOmniControl, border=NA, xpd=NA, lwd=lwd)
				rect(omniControl, at=countX - nudge - subnudge, width=width, density=lineDensity, col=colOmniControl, fill=colOmniControl, border=borderOmniControl, xpd=NA, lwd=lwd)
			
				# unperturbed SDM
				rect(sdmControl, at=countX - nudge + subnudge, width=width, col=colSdmControl, border=borderSdmControl, xpd=NA, lwd=0.8)

				leg <- c(
					'OMNI unpermuted',
					paste0('OMNI TRUE perm.'),
					paste0('OMNI FALSE perm.'),
					paste0(algosShort(algo), ' unpermuted'),
					paste0(algosShort(algo), ' TRUE perm.'),
					paste0(algosShort(algo), ' FALSE perm.')
				)
			
				par(lwd=0.5)
			
				legend('bottomleft', inset=c(0.02, 0.02), ncol=2, bty='n', legend=leg, cex=legCex, fill=c('white', 'white', 'white', colSdmControl, colTrue, colFalse), border=c(borderOmniControl, borderTrue, borderFalse, borderSdmControl, borderTrue, borderFalse))
				
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

				legend('bottomright', inset=c(0, 0.025), ncol=2, bty='n', legend=leg, cex=legCex, fill=c('white', 'white', colTrue, colFalse), border=c(borderTrue, borderFalse, borderTrue, borderFalse))
				
				adjust <- 1/2 * nudge
				
			}
				
			### TRUE response
			rect(omniTrue, at=countX - adjust - subnudge, width=width, col='white', border=borderTrue, xpd=NA, lwd=lwd)
			rect(sdmTrue, at=countX - adjust + subnudge, width=width, col=colTrue, border=borderTrue, xpd=NA, lwd=lwd)

			### FALSE response
			rect(omniFalse, at=countX - adjust + nudge - subnudge, width=width, col='white', border=borderFalse, xpd=NA, lwd=lwd)
			rect(sdmFalse, at=countX - adjust + nudge + subnudge, width=width, col=colFalse, border=borderFalse, xpd=NA, lwd=lwd)

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
					
					text(countX, ylim[2] + 0.1 * diff(ylim), labels=paste(acc, collapse=' '), xpd=NA, cex=1.3 * labCex, pos=1)
					
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
		width <- 0.14 # bar width
		nudge <- 0.26 # nudge pair of bars for same algorithm left/right
		subnudge <- nudge / 4 # nudge bars within same class
		lwd <- 0.8 # line width of box borders
		figLabPos <- c(-0.2250, 0.03) # position of figure label
		
		legCex <- 0.6 # legend
		
		ylabX1 <- -0.105 # position of inner y-axis label
		ylabX2 <- -0.165 # position of outer y-axis label
		labCex <- 0.65 # size of algorithm, y-axis, and figure labels
		
		xlabY1 <- -0 # position of inner x-axis sublabels (range of TRUE)
		xlabY2 <- -0.14 # position of outer x-axis label
		
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
		polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col='gray85', border=NA, xpd=NA)
		lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.8 * lwd, xpd=NA)
		for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		for (i in 1:(length(x) - 1)) lines(x=c(i + 0.5, i + 0.5), y=c(-1, 1), col='white', lwd=0.5, xpd=NA)

		# x: axis labels
		axis(1, at=seq_along(x), labels=rep('', length(x)), tck=-0.03, lwd=0.8)
		xLabs <- if (!is.null(decs)) { sprintf(paste0('%.', decs, 'f'), x) } else { x }
		text(seq_along(x), y=rep(usr[3] + xlabY1 * (usr[4] - usr[3]), length(x)), labels=xLabs, cex=0.8 * labCex, xpd=NA, srt=0, pos=1, col='black')
		text(mean(seq_along(x)), y=usr[3] + xlabY2 * (usr[4] - usr[3]), labels=xlab, cex=labCex, xpd=NA, srt=0, col='black')
	
		# y: y-axis labels
		axis(2, at=yTicks, labels=yTicks, tck=-0.03, lwd=0.8)
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
				rect(omniControl, at=countX - nudge - subnudge, width=width, col=colOmniControl, border=NA, xpd=NA, lwd=lwd)
				rect(omniControl, at=countX - nudge - subnudge, width=width, density=lineDensity, col=colOmniControl, fill=colOmniControl, border=borderOmniControl, xpd=NA, lwd=lwd)
			
				# unperturbed SDM
				rect(sdmControl, at=countX - nudge + subnudge, width=width, col=colSdmControl, border=borderSdmControl, xpd=NA, lwd=0.8)

				leg <- c(
					'OMNI unpermuted',
					paste0('OMNI T1 perm.'),
					paste0('OMNI T2 perm.'),
					paste0(algosShort(algo), ' unpermuted'),
					paste0(algosShort(algo), ' T1 perm.'),
					paste0(algosShort(algo), ' T2 perm.')
				)
			
				par(lwd=0.5)
			
				legend('bottomright', inset=c(0, 0.05), ncol=2, bty='n', legend=leg, cex=legCex, fill=c('white', 'white', 'white', colSdmControl, colSdmT1, colSdmT2), border=c(borderOmniControl, borderSdmT1, borderSdmT2, borderSdmControl, borderSdmT1, borderSdmT2))
				
				adjust <- 0
				
			# if there is no distinct response for control/unperturbed
			# for when plotting correlation metric
			} else {

				omniControl <- sdmControl <- NULL
			
				# legend
				leg <- c(
					paste0('OMNI T1 perm.'),
					paste0('OMNI T2 perm.'),
					paste0(algosShort(algo), ' TRUE1 perm.'),
					paste0(algosShort(algo), ' TRUE2 perm.')
				)
			
				par(lwd=0.5)

				legend('bottomright', inset=c(0, 0.025), ncol=2, bty='n', legend=leg, cex=legCex, fill=c('white', 'white', colTrue, colFalse), border=c(borderTrue, borderFalse, borderTrue, borderFalse))
				
				adjust <- 1/2 * nudge
				
			}
				
			### TRUE1 response
			rect(omniTrue1, at=countX - adjust - subnudge, width=width, col='white', border=borderSdmT1, xpd=NA, lwd=lwd)
			rect(sdmTrue1, at=countX - adjust + subnudge, width=width, col=colSdmT1, border=borderSdmT1, xpd=NA, lwd=lwd)

			### TRUE2 response
			rect(omniTrue2, at=countX - adjust + nudge - subnudge, width=width, col='white', border=borderSdmT2, xpd=NA, lwd=lwd)
			rect(sdmTrue2, at=countX - adjust + nudge + subnudge, width=width, col=colSdmT2, border=borderSdmT2, xpd=NA, lwd=lwd)

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

say('###################################')
say('### [simple] simulation results ###')
say('###################################')

	scenarioDir <- './Results/simple'
	evalDir <- paste0(scenarioDir, '/evaluations')

	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	# generalization
	width <- 0.14 # bar width
	nudge <- 0.22 # nudge left/right
	figLabPos <- c(-0.15, 0.05) # position of figure label
	
	ylabX1 <- -0.18 # position of inner y-axis label
	ylabX2 <- -0.26 # position of outer y-axis label
	labCex <- 0.45 # size of algorithm, y-axis, and figure labels
	
	sublabY <- -0.07 # position of TRUE/FALSE variable sublabels
	sublabCex <- 0.38 # size of TRUE/FALSE sublabels

	algoLabY <- -0.62 # position of algorithm labels

	# par settings
	oma <- rep(0, 4)
	mar <- c(3.7, 1.6, 0.5, 0.5)
	mgp <- c(2, 0.2, 0)
	cex.axis <- 0.35
	lwd <- 0.6
	
	# image size
	figWidth <- 900
	figHeight <- 1100
	
	# master plot function
	plotSimpleResp <- function(nudge, ylim, yTicks, ylab, lab, rand, trueField, controlField, falseField, controlLab, omniHasFalse, expectHigher) {
		
		# nudge 	amount to move bars in same group (algorithm) left or right
		# ylim		y-axis limits
		# ylab		y-axis label
		# yTicks	position of tick marks on y-axis
		# lab		figure label
		# rand		value of response equal to "random prediction" (eg 0.5 for AUC or 0 for CBI)
		# trueField	field name of response for TRUE variable
		# controlField	field name of response for control case (or NULL if none)
		# falseField	field name of response for FALSE variable
		# controlLab character, name of bar representing "control" model/prediction
		# omniHasFalse   logical, if TRUE then test discrimination and calibration accuracy of OMNI TRUE versus FALSE (TRUE for multivariate tests using performance metrics and FALSE for univariate tests using performance metrics)... FALSE for CORpa and CORbg
		# expectHigher if test can discriminate, should values of TRUE or FALSE be higher ('true', 'false')
		
		# adjust nudging of bars in same groups
		if (is.null(controlField)) nudge <- nudge / 2
		
		# base plot
		plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0.5, length(algos)), ylim=ylim)
		labelFig(lab, adj=figLabPos, cex=labCex)
		usr <- par('usr')

		# gray background
		left <- 1 - (2 + ifelse(is.null(controlField), 0.75, 0)) * nudge
		right <- length(algos) + (2.5 + ifelse(is.null(controlField), 0.25, -0.3)) * nudge
		polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col='gray85', border=NA, xpd=NA)
		lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.4, xpd=NA)
		for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		for (i in 1:(length(algos) - 1)) lines(x=c(i + 0.5, i + 0.5), y=c(-1, 1), col='white', lwd=0.5, xpd=NA)
		
		# x: variable labels
		axis(1, at=seq_along(algos), labels=rep('', length(algos)), tck=-0.03, lwd=0.8)
		text(seq_along(algos) - nudge, y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep('TRUE permuted', length(algos)), cex=sublabCex, xpd=NA, srt=90, adj=c(1, 0.3), col=borderTrue)
		if (!is.null(controlField)) text(seq_along(algos), y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep(controlLab, length(algos)), cex=sublabCex, xpd=NA, srt=90, adj=c(1, 0.3), col='black')
		text(seq_along(algos) + nudge, y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep('FALSE permuted', length(algos)), cex=sublabCex, xpd=NA, srt=90, adj=c(1, 0.3), col=borderFalse)
		
		# x: algorithm labels
		text(seq_along(algos), y=rep(usr[3] + algoLabY * (usr[4] - usr[3]), length(algos)), labels=algosShort(algos), xpd=NA, cex=labCex)
		
		# y: y-axis labels
		axis(2, at=yTicks, labels=yTicks, tck=-0.03, lwd=0.8)
		text(usr[1] + ylabX1 * (usr[2] - usr[1]), y=mean(yTicks), label='\U2190important       unimportant\U2192', srt=90, cex=0.9 * labCex, xpd=NA)
		text(usr[1] + ylabX2 * (usr[2] - usr[1]), y=mean(yTicks), label=ylab, srt=90, cex=labCex, xpd=NA)

		# get this for evaluating calibration/discrimination accuracy
		omniControl <- if (!is.null(controlField)) {
			evals[evals$algo=='omniscient', controlField]
		} else {
			NULL
		}
		
		omniTrue <- evals[evals$algo=='omniscient', trueField]
		omniFalse <- evals[evals$algo=='omniscient', falseField]

		# responses
		for (countAlgo in seq_along(algos)) {
		
			algo <- algos[countAlgo]
		
			# responses
			sdmControl <- if (!is.null(controlField)) {
				evals[evals$algo==algo, controlField]
			} else {
				NULL
			}

			sdmTrue <- evals[evals$algo==algo, trueField]
			sdmFalse <- evals[evals$algo==algo, falseField]

			# colors
			if (algo == 'omniscient') {
				thisColControl <- 'white'
				thisBorderControl <- borderOmniControl
				thisColTrue <- 'white'
				thisBorderTrue <- borderTrue
				thisColFalse <- 'white'
				thisBorderFalse <- borderFalse
			} else {
				thisColControl <- colSdmControl
				thisBorderControl <- borderSdmControl
				thisColTrue <- colTrue
				thisBorderTrue <- borderTrue
				thisColFalse <- colFalse
				thisBorderFalse <- borderFalse
			}
			if (!is.null(controlField)) rect(sdmControl, at=countAlgo, width=width, col=thisColControl, border=thisBorderControl, lwd=0.8)
			
			rect(sdmTrue, at=countAlgo - nudge, width=width, col=thisColTrue, border=thisBorderTrue, xpd=NA, lwd=0.8)
			rect(sdmFalse, at=countAlgo + nudge, width=width, col=thisColFalse, border=thisBorderFalse, xpd=NA, lwd=0.8)
			
			# accuracy notations
			acc <- character()
		
			# discrimination
			wellDiscrimSdm <- discriminatedTrueFalse(sdmTrue, sdmFalse, expectHigher=expectHigher)
			if (omniHasFalse) {
				wellDiscrimOmni <- discriminatedTrueFalse(omniTrue, omniFalse, expectHigher=expectHigher)
				if (wellDiscrimSdm & wellDiscrimOmni) acc <- c(acc, discrimSymbol)
			} else {
				if (!is.na(wellDiscrimSdm) && wellDiscrimSdm) acc <- c(acc, discrimSymbol)
			}
			
			# calibration
			if (algo != 'omniscient' & omniHasFalse) {
				wellCalib <- calibratedTrueFalse(omniControl=omniControl, omniTrue=omniTrue, omniFalse=omniFalse, sdmControl=sdmControl, sdmTrue=sdmTrue, sdmFalse=sdmFalse, calibTol=calibTol)
				if (!is.na(wellCalib) && wellCalib) acc <- c(acc, calibSymbol)
			} else if (algo != 'omniscient' & !omniHasFalse) {
				wellCalib <- calibratedTrueFalse(omniControl=omniControl, omniTrue=omniTrue, omniFalse=omniFalse, sdmControl=sdmControl, sdmTrue=sdmTrue, sdmFalse=sdmFalse, calibTol=calibTol)
				if (!is.na(wellCalib) && wellCalib) acc <- c(acc, calibSymbol)
			}
			
			text(countAlgo , ylim[2] + 0.17 * diff(ylim), labels=paste(acc, collapse=' '), xpd=NA, cex=1.3 * labCex, pos=1)
			
		}
		
	}

	### multivariate: CBI
	#####################
	
	png(paste0(scenarioDir, '/Multivariate CBI.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('CBI')
		ylim <- c(-0.5, 1)
		yTicks <- seq(-0.5, 1, by=0.5)
		trueField <- 'cbiMulti_permT1'
		controlField <- 'cbiMulti'
		falseField <- 'cbiMulti_permF1'
		rand <- 0
		omniHasFalse <- TRUE
		expectHigher <- FALSE

		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

	### multivariate: AUCpa
	#######################
	
	png(paste0(scenarioDir, '/Multivariate AUCpa.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('AUC'['pa'])
		ylim <- c(0, 1)
		yTicks <- seq(0, 1, by=0.25)
		trueField <- 'aucPresAbsMulti_permT1'
		controlField <- 'aucPresAbsMulti'
		falseField <- 'aucPresAbsMulti_permF1'
		rand <- 0.5
		omniHasFalse <- TRUE
		expectHigher <- FALSE

		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

	### multivariate: AUCbg
	#######################
	
	png(paste0(scenarioDir, '/Multivariate AUCbg.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('AUC'['bg'])
		ylim <- c(0, 1)
		yTicks <- seq(0, 1, by=0.25)
		trueField <- 'aucPresBgMulti_permT1'
		controlField <- 'aucPresBgMulti'
		falseField <- 'aucPresBgMulti_permF1'
		rand <- 0.5
		omniHasFalse <- TRUE
		expectHigher <- FALSE

		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

	### multivariate: CORpa
	#######################
	
	png(paste0(scenarioDir, '/Multivariate CORpa.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('COR'['pa'])
		ylim <- c(0, 1)
		yTicks <- seq(0, 1, by=0.25)
		trueField <- 'corPresAbsMulti_permT1'
		controlField <- NULL
		falseField <- 'corPresAbsMulti_permF1'
		rand <- 0
		expectHigher <- FALSE

		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

	### multivariate: CORbg
	#######################
	
	png(paste0(scenarioDir, '/Multivariate CORbg.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('COR'['bg'])
		ylim <- c(0, 1)
		yTicks <- seq(0, 1, by=0.25)
		trueField <- 'corPresBgMulti_permT1'
		controlField <- NULL
		falseField <- 'corPresBgMulti_permF1'
		rand <- 0
		omniHasFalse <- FALSE
		expectHigher <- FALSE
		
		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

	### univariate: CBI
	###################
	
	png(paste0(scenarioDir, '/Univariate CBI.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('CBI')
		ylim <- c(-1, 1)
		yTicks <- seq(-1, 1, by=0.5)
		trueField <- 'cbiUni_onlyT1'
		controlField <- 'cbiMulti'
		falseField <- 'cbiUni_onlyF1'
		rand <- 0
		omniHasFalse <- FALSE
		expectHigher <- TRUE

		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Multivar', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

	### univariate: AUCpa
	#######################
	
	png(paste0(scenarioDir, '/Univariate AUCpa.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('AUC'['pa'])
		ylim <- c(0, 1)
		yTicks <- seq(0, 1, by=0.25)
		trueField <- 'aucPresAbsUni_onlyT1'
		controlField <- 'aucPresAbsMulti'
		falseField <- 'aucPresAbsUni_onlyF1'
		rand <- 0.5
		omniHasFalse <- FALSE
		expectHigher <- TRUE

		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

	### univariate: AUCbg
	#######################
	
	png(paste0(scenarioDir, '/Univariate AUCbg.png'), width=figWidth, height=figHeight, res=600)
		
		par(oma=oma, mar=mar, mgp=mgp, cex.axis=cex.axis, lwd=lwd)
		
		lab <- bquote('')
		ylab <- bquote('AUC'['bg'])
		ylim <- c(0, 1)
		yTicks <- seq(0, 1, by=0.25)
		trueField <- 'aucPresBgUni_onlyT1'
		controlField <- 'aucPresBgMulti'
		falseField <- 'aucPresBgUni_onlyF1'
		rand <- 0.5
		omniHasFalse <- FALSE
		expectHigher <- TRUE

		plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=rand, trueField=trueField, controlField=controlField, falseField=falseField, controlLab='Unpermuted', omniHasFalse=omniHasFalse, expectHigher=expectHigher)
		
		title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	dev.off()

say('###########################')
say('### [simple] statistics ###')
say('###########################')
	
	scenarioDir <- './Results/simple'
	evalDir <- paste0(scenarioDir, '/evaluations')

	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	
	# OMNI AUC
	x <- evals$aucPresAbsMulti[evals$algo=='omniscient']
	avg <- mean(x)
	say('Mean AUCpa for unpermuted multivariate OMNI model is ', sprintf('%.2f', avg), '.')
	
	x <- evals$aucPresBgMulti[evals$algo=='omniscient']
	avg <- mean(x)
	say('Mean AUCbg for unpermuted multivariate OMNI model is ', sprintf('%.2f', avg), '.', post=2)
	
	# BRT native importance
	x <- evals$brtMultiNativeImportT1[evals$algo=='brt']
	avg <- mean(x, na.rm=TRUE)
	quants <- quantile(x, c(0.025, 0.975), na.rm=TRUE)
	say('Mean algorithm-specific importance for multivariate BRT model for TRUE variable is ', sprintf('%.2f', avg), ' (inner 95% range:', sprintf('%.2f', quants[1]), '-', sprintf('%.2f', quants[2]), ')')
	
	x <- evals$brtMultiNativeImportF1[evals$algo=='brt']
	avg <- mean(x, na.rm=TRUE)
	quants <- quantile(x, c(0.025, 0.975), na.rm=TRUE)
	say('Mean algorithm-specific importance for multivariate BRT model for FALSE variable is ', sprintf('%.2f', avg), ' (inner 95% range:', sprintf('%.2f', quants[1]), '-', sprintf('%.2f', quants[2]), ')', post=2)
	
	# BRT performance and use of TRUE/FALSE
	x1 <- evals$brtMultiNativeImportT1[evals$algo=='brt']
	x2 <- evals$cbiMulti[evals$algo=='brt']
	x <- cor(x1, x2, use='complete.obs')
	say('Correlation between multivariate CBI and native-BRT TRUE importance: ', sprintf('%.2f', x))
	
	x1 <- evals$brtMultiNativeImportF1[evals$algo=='brt']
	x2 <- evals$cbiMulti[evals$algo=='brt']
	x <- cor(x1, x2, use='complete.obs')
	say('Correlation between multivariate CBI and native-BRT FALSE importance: ', sprintf('%.2f', x), post=2)
	
	# BRT AUC
	x <- evals$aucPresAbsMulti[evals$algo=='brt']
	successes <- sum(!is.na(x))
	say('Number of times multivariate BRT converged (out of 100): ', successes, post=2)
	
	# OMNI CBI
	x <- evals$cbiMulti[evals$algo=='omniscient']
	stats <- quantile(x, c(0.025, 0.5, 0.975))
	say('2.5th/median/97.5th quantiles of CBI for multivariate control OMNISCIENT model: ', paste(sprintf('%.2f', stats), collapse=' '))
	
	x <- evals$cbiMulti[evals$algo=='omniscient']
	stats <- quantile(x, c(0, 0.5, 1))
	say('Min/median/max CBI for multivariate control OMNISCIENT model: ', paste(sprintf('%.2f', stats), collapse=' '), post=2)
	
	# BRT CBI
	x <- evals$cbiUni_onlyT1[evals$algo=='brt']
	successes <- sum(!is.na(x))
	say('Number of times univariate BRT converged using just TRUE variable (out of 100): ', successes)
	
	x <- evals$cbiUni_onlyF1[evals$algo=='brt']
	say('Number of times univariate BRT converged using just FALSE variable (out of 100): ', successes)
	successes <- sum(!is.na(x))
	
say('########################################')
say('### [sample size] simulation results ###')
say('########################################')

	# generalization
	scenarioDir <- './Results/sample size' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'numTrainPres' # name of x-axis variable column in evaluation data frame
	decs <- 0 # number of decimals to show in x-axis variable tick mark labels
	xlab <- 'Number of calibration presences' # x-axis label
	expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# responses to plot
	resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	# plot results for each response
	multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, xlab=xlab, evals=evals, resps=resps, expectHigher=expectHigher)

say('#######################################')
say('### [prevalence] simulation results ###')
say('#######################################')

	# generalization
	scenarioDir <- './Results/prevalence' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'prevalence' # name of x-axis variable column in evaluation data frame
	decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	xlab <- 'Prevalence' # x-axis label
	expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# responses to plot
	resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	
	# plot multivariate model results
	multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, xlab=xlab, evals=evals, resps=resps, expectHigher=expectHigher)

say('###################################')
say('### [extent] simulation results ###')
say('###################################')

	# generalization
	scenarioDir <- './Results/extent' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'rangeT1' # name of x-axis variable column in evaluation data frame
	decs <- NULL # number of decimals to show in x-axis variable tick mark labels
	xlab <- 'Study region extent (range of TRUE)' # x-axis label
	expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# responses to plot
	resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	evals$rangeT1 <- evals$maxT1 - evals$minT1

	# plot multivariate model results
	multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, xlab=xlab, evals=evals, resps=resps, expectHigher=expectHigher)

say('####################################################')
say('### [correlated TRUE & FALSE] simulation results ###')
say('####################################################')

	# generalization
	scenarioDir <- './Results/correlated TRUE & FALSE' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'correlation' # name of x-axis variable column in evaluation data frame
	decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	xlab <- 'Correlation between TRUE and FALSE' # x-axis label
	expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination

	# responses to plot
	resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')
	evals$correlation <- correlations$cor[match(evals$rotF1, correlations$rot)]
	
	# plot multivariate model results
	multivariatePlotsTRUEvsFALSE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, xlab=xlab, evals=evals, resps=resps, expectHigher=expectHigher)

say('#######################################')
say('### [resolution] simulation results ###')
say('#######################################')

	say('Basic layout: Three panels side-by-side, one per algorithm.')
	say('Each panel has 3 columns (resolution) and 4 rows (SAC).')
	say('Each subpanel is a barplot showing response of OMNI and the SDM.')

	# generalization
	scenarioDir <- './Results/resolution' # scenario directory

	# load evaluations and calculate x-axis variable
	evals <- loadEvals(
		evalDir=paste0(scenarioDir, '/evaluations'),
		algos=algos,
		save=TRUE,
		redo=FALSE
	)
	
	# SAC
	noises <- c(0, 1/3, 2/3, 1)
	noisesRounded <- round(noises, 2)
	
	# grain size
	grains <- c(2^14, 2^10, 2^6)

	# plot settings
	xSize <- 0.775 # maximum width of subplot containing bars
	ySize <- 0.875 # maximum height of subplot containing bars
	width <- 0.17 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.05 # length of subplot tick marks
	lwd <- 0.4 # line width of bars
	cexAxisLabel <- 0.5
	cexPanelLabel <- 0.7
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels	

	lineDensity <- NULL
	
	# function to plots bars as scaled subplots in a larger plot
	# basically this just rescales the size and position of values and bars and send the information to rect()
	scaleRespToSubplot <- function(resp, angle, xOffsetInSubplot, col, border, ...) {
	
		# resp		values of response (not scaled)
		# angle		NULL or angle of fill lines
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# col, border  color for fill and border
		# ...		other
	
		respScaled <- resp * 0.5 * ySize + subplotPosY - 0 * 0.5 * ySize
		at <- countGrain - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=col, border=border, angle=angle, density=s * lineDensity, lwd=lwd)
		
	}
	
	### plot CBI, AUCpa, AUCbg
	##########################
	
	respStatistics <- c('CBI', 'AUCpa', 'AUCbg')
	respControls <- c('cbiMulti', 'aucPresAbsMulti', 'aucPresBgMulti')
	resps <- c('cbiMulti_perm', 'aucPresAbsMulti_perm', 'aucPresBgMulti_perm')
	expectHigher <- FALSE # expect values for FALSE to be higher than TRUE for successful discrimination
	
	# y position of guidelines in subplots
	respRands <- c(0, 0.5, 0.5)
	respMins <- c(-1, 0, 0)

	# by TEST STATISTIC
	for (countStat in seq_along(respStatistics)) {
	
		respStatistic <- respStatistics[countStat]
		controlField <- respControls[countStat]
		resp <- resps[countStat]
	
		respRand <- respRands[countStat]
		respMin <- respMins[countStat]
		
		png(paste0(scenarioDir, '/Results - ', respStatistic, ' - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=9 * 300, height=4 * 300, res=600)
		
			par(mfrow=c(1, 3), oma=c(2, 2, 1, 0), mar=c(0, 1.4, 0, 0), mgp=c(1, 0.2, 0), cex.axis=0.55)

			# by ALGO
			for (countAlgo in seq_along(sdmAlgos)) {
			
				algo <- sdmAlgos[countAlgo]
			
				algoNice <- algosShort(algo)
		
				xs <- seq_along(grains)
				ys <- seq_along(noises)
				xlim <- range(xs) + c(-0.5, 0.5)
				ylim <- range(ys) + c(-0.5, 0.5)
				
				ylab <- 'Spatial autocorrelation\n(proportion of cells swapped)\n\U2190lower autocorrelation     higher autocorrelation\U2192'
				
				plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
				axis(1, at=xs, labels=paste0('1/', grains), tck=-0.01, lwd=0.6, line=-0.25)
				axis(2, at=ys, labels=rev(noisesRounded), tck=-0.015, lwd=0.6, line=0.25)
				mtext('Grain size', side=1, cex=0.4, line=0.7)
				labelFig(paste0(letters[countAlgo], ') ', algoNice), adj=0.015, cex=0.625, xpd=NA)
				
				# by NOISE (SAC)
				for (countNoise in seq_along(noises)) {
			
					noise <- noises[countNoise]
					subplotPosY <- length(noises) + 1 - countNoise # y-axis position of subplot
			
					# by GRAIN
					for (countGrain in seq_along(grains)) {
					
						grain <- grains[countGrain]

						# get response data
						omniControl <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, controlField]
						sdmControl <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, controlField]
						
						omniTrue <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
						omniFalse <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]
						
						sdmTrue <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
						sdmFalse <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]

						# calculate and assign variables for lower/upper limits and median
						whats <- c('Inner', 'Median', 'Outer')
						for (modelType in c('sdm', 'omni')) {
							for (variable in c('Control', 'True', 'False')) {
								
								thisVar <- paste0(modelType, variable)
								x <- get(thisVar)
								quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

								for (countWhat in seq_along(whats)) {
							
									what <- whats[countWhat]
									assign(paste0(thisVar, what), quants[countWhat])
									
								}
							}
						}

						s <- 0.8 # line density scalar for angled fills
						
						# subplot y-axis
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
						
						# subplot y-axis tick lines and labels
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY + 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY, subplotPosY), lwd=lwd)
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY - 0.5 * ySize), lwd=lwd)
						
						# subplot y-axis labels
						cex <- 0.4
						offset <- 2.5
						if (countGrain == 1) {

							text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
							text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY, labels=respRand, cex=cex, xpd=NA)
							text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY - 0.5 * ySize, labels=respMin, cex=cex, xpd=NA)
							
						}
						
						# gray background
						offsetInSubplot <- 0.1
						rand <- 0
						left <- countGrain - 0.54 * xSize + offsetInSubplot * xSize
						right <- countGrain + 0.5 * xSize + 1.75 * offsetInSubplot * xSize
						bottom <- subplotPosY - 0.5 * ySize
						top <- subplotPosY + 0.5 * ySize
						polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

						# OMNI control (unpermuted)
						scaleRespToSubplot(
							resp=omniControl,
							angle=NULL,
							xOffsetInSubplot=0.2,
							col=colOmniControl,
							border=borderOmniControl
						)
						
						# SDM control (unpermuted)
						scaleRespToSubplot(
							resp=sdmControl,
							angle=NULL,
							xOffsetInSubplot=0.35,
							col=colSdmControl,
							border=borderSdmControl
						)
						
						# OMNI permuted T1
						scaleRespToSubplot(
							resp=omniTrue,
							angle=NULL,
							xOffsetInSubplot=0.55,
							col='white',
							border=borderTrue
						)

						# SDM permuted T1
						scaleRespToSubplot(
							resp=sdmTrue,
							angle=NULL,
							xOffsetInSubplot=0.7,
							col=colTrue,
							border=borderTrue
						)
						
						# OMNI permuted F1
						scaleRespToSubplot(
							resp=omniFalse,
							angle=NULL,
							xOffsetInSubplot=0.9,
							col='white',
							border=borderFalse
						)
						
						# SDM permuted F1
						scaleRespToSubplot(
							resp=sdmFalse,
							angle=NULL,
							xOffsetInSubplot=1.05,
							col=colFalse,
							border=borderFalse
						)

						## accuracy indicators
						sdmTrueNas <- sum(!is.na(sdmTrue))
						sdmFalseNas <- sum(!is.na(sdmFalse))
						if (sdmTrueNas > 1 & sdmFalseNas > 1) {
				
							acc <- character()
				
							# discrimination
							wellDiscrim <- discriminatedTrueFalse(sdmTrue, sdmFalse, expectHigher=expectHigher) & discriminatedTrueFalse(omniTrue, omniFalse, expectHigher=expectHigher)
							if (wellDiscrim) acc <- c(acc, discrimSymbol)
		
							# calibration
							wellCalib <- calibratedTrueFalse(omniControl=omniControl, omniTrue=omniTrue, omniFalse=omniFalse, sdmControl=sdmControl, sdmTrue=sdmTrue, sdmFalse=sdmFalse, calibTol=calibTol)
							if (wellCalib) acc <- c(acc, calibSymbol)
							
							x <- countGrain + 0.85 * xSize
							y <- bottom + 0.1 * ySize
							text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1.5 * labCex, pos=2)
							
						}
							
					} # next grain

				} # next noise level (SAC)
				
			} # next algorithm

			# panel y-axis labels
			mtext(ylab, side=2, cex=0.4, line=0, outer=TRUE)

			title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		
		dev.off()

	} # next test statistic

	### plot CORbg and CORpa
	########################

	respStatistics <- c('CORpa', 'CORbg')
	resps <- c('corPresAbsMulti_perm', 'corPresBgMulti_perm')
	
	respRand <- 0.5
	respMin <- 0
	expectHigher <- FALSE # expect values from FALSE to be higher

	for (countStat in seq_along(respStatistics)) {
		
		respStatistic <- respStatistics[countStat]
		resp <- resps[countStat]
	
		png(paste0(scenarioDir, '/Results - ', respStatistic, ' - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=9 * 300, height=4 * 300, res=600)
		
			par(mfrow=c(1, 3), oma=c(2, 2, 1, 0), mar=c(0, 1.4, 0, 0), mgp=c(1, 0.2, 0), cex.axis=0.55)

			# by ALGO
			for (countAlgo in seq_along(sdmAlgos)) {
			
				algo <- sdmAlgos[countAlgo]
			
				algoNice <- algosShort(algo)
		
				xs <- seq_along(grains)
				ys <- seq_along(noises)
				xlim <- range(xs) + c(-0.5, 0.5)
				ylim <- range(ys) + c(-0.5, 0.5)
				
				ylab <- 'Spatial autocorrelation\n(proportion of cells swapped)\n\U2190lower autocorrelation     higher autocorrelation\U2192'
				
				plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
				axis(1, at=xs, labels=paste0('1/', grains), tck=-0.01, lwd=0.6, line=-0.25)
				axis(2, at=ys, labels=rev(noisesRounded), tck=-0.015, lwd=0.6, line=0.25)
				mtext('Grain size', side=1, cex=0.4, line=0.7)
				labelFig(paste0(letters[countAlgo], ') ', algoNice), adj=0.015, cex=0.625, xpd=NA)
				
				# by NOISE (SAC)
				for (countNoise in seq_along(noises)) {
			
					noise <- noises[countNoise]
					subplotPosY <- length(noises) + 1 - countNoise # y-axis position of subplot
			
					# by GRAIN
					for (countGrain in seq_along(grains)) {
					
						grain <- grains[countGrain]

						# get response data
						omniTrue <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
						omniFalse <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]
						
						sdmTrue <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
						sdmFalse <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]

						# calculate and assign variables for lower/upper limits and median
						whats <- c('Inner', 'Median', 'Outer')
						for (modelType in c('sdm', 'omni')) {
							for (variable in c('True', 'False')) {
								
								thisVar <- paste0(modelType, variable)
								x <- get(thisVar)
								quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

								for (countWhat in seq_along(whats)) {
							
									what <- whats[countWhat]
									assign(paste0(thisVar, what), quants[countWhat])
									
								}
							}
						}

						s <- 0.8 # line density scalar for angled fills
						
						# subplot y-axis
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
						
						# subplot y-axis tick lines and labels
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY + 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY, subplotPosY), lwd=lwd)
						lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY - 0.5 * ySize), lwd=lwd)
						
						# subplot y-axis labels
						cex <- 0.4
						offset <- 2.5
						if (countGrain == 1) {

							text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
							text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY, labels=respRand, cex=cex, xpd=NA)
							text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY - 0.5 * ySize, labels=respMin, cex=cex, xpd=NA)
							
						}
						
						# gray background
						offsetInSubplot <- 0.1
						rand <- 0
						left <- countGrain - 0.54 * xSize + offsetInSubplot * xSize
						right <- countGrain + 0.5 * xSize + 1.75 * offsetInSubplot * xSize
						bottom <- subplotPosY - 0.5 * ySize
						top <- subplotPosY + 0.5 * ySize
						polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

						# OMNI permuted T1
						scaleRespToSubplot(
							resp=omniTrue,
							angle=45,
							xOffsetInSubplot=0.25,
							col='white',
							border=borderTrue
						)

						# SDM permuted T1
						scaleRespToSubplot(
							resp=sdmTrue,
							angle=NULL,
							xOffsetInSubplot=0.4,
							col=colTrue,
							border=borderTrue
						)
						
						# OMNI permuted F1
						scaleRespToSubplot(
							resp=omniFalse,
							angle=45,
							xOffsetInSubplot=0.7,
							col='white',
							border=borderFalse
						)
						
						# SDM permuted F1
						scaleRespToSubplot(
							resp=sdmFalse,
							angle=NULL,
							xOffsetInSubplot=0.85,
							col=colFalse,
							border=borderFalse
						)

						## accuracy indicators
						sdmTrueNas <- sum(!is.na(sdmTrue))
						sdmFalseNas <- sum(!is.na(sdmFalse))
						if (sdmTrueNas > 1 & sdmFalseNas > 1) {
				
							acc <- character()
				
							# discrimination
							wellDiscrim <- discriminatedTrueFalse(sdmTrue, sdmFalse, expectHigher=expectHigher) & discriminatedTrueFalse(omniTrue, omniFalse, expectHigher=expectHigher)
							if (wellDiscrim) acc <- c(acc, discrimSymbol)
		
							# calibration
							wellCalib <- calibratedTrueFalse(omniControl=NULL, omniTrue=omniTrue, omniFalse=omniFalse, sdmControl=NULL, sdmTrue=sdmTrue, sdmFalse=sdmFalse, calibTol=calibTol)
							if (wellCalib) acc <- c(acc, calibSymbol)
							
							x <- countGrain + 0.85 * xSize
							y <- bottom + 0.1 * ySize
							text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1.5 * labCex, pos=2)
							
						}

					} # next grain

				} # next noise level (SAC)
				
			} # next algorithm

			# panel y-axis labels
			mtext(ylab, side=2, cex=0.4, line=0, outer=TRUE)

			title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		
		dev.off()
		
	} # next test statistic
	
# # say('#######################################')
# # say('### [bivariate] collate evaluations ###')
# # say('#######################################')

	# # evalDir <- './Results/bivariate/evaluations'
	# # evals <- loadEvals(evalDir, algos=c('omniscient', 'brt', 'gam', 'maxent'), save=TRUE, redo=FALSE)

say('##############################')
say('### [bivariate] statistics ###')
say('##############################')

	scenarioDir <- './Results/bivariate'
	load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))

	# prevalence: all scenarios
	x <- round(max(master$prev), 2)
	say('Maximum real prevalence (mean suitability) across all simulations: ', sprintf('%.2f', x), post=2, pre=2)

	say('SYMMETRICAL NICHE WIDTH, NO CORRELATION, NO COVARIANCE', level=2)
		
		# OMNI multivariate CBI for "control" (unpermuted) model
		x1 <- round(median(master$cbiMulti[master$algo == 'omniscient' & master$sigma1 %==% 0.1 & master$sigma1 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90]), 2)
		x2 <- round(median(master$cbiMulti[master$algo == 'omniscient' & master$sigma1 %==% 0.3 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90]), 2)
		x3 <- round(median(master$cbiMulti[master$algo == 'omniscient' & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90]), 2)

		say('Median CBI for OMNISCIENT, unpermuted for sigma1 = sigma2 = 0.1 and rho = 0: ', sprintf('%.2f', x1))
		say('Median CBI for OMNISCIENT, unpermuted for sigma1 = sigma2 = 0.3 and rho = 0: ', sprintf('%.2f', x2))
		say('Median CBI for OMNISCIENT, unpermuted for sigma1 = sigma2 = 0.5 and rho = 0: ', sprintf('%.2f', x3), post=2)

		# multivariate CBI for "treatment" (permuted) model
		for (algo in algos) {
		
			# multivariate CBI for permuted models, symmetrical niches
			say(toupper(algo), ', symmetrical niche width, unpermuted')
			x1 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90], na.rm=TRUE), 2)
			x2 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.3 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90], na.rm=TRUE), 2)
			x3 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.1 & master$sigma2 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90], na.rm=TRUE), 2)

			say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.5, rho = 0: ', sprintf('%.2f', x1))
			say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.3, sigma2 = 0.3, rho = 0: ', sprintf('%.2f', x2))
			say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.1, sigma2 = 0.1, rho = 0: ', sprintf('%.2f', x3), post=2)
			
		}
		
	say('*A*SYMMETRICAL NICHE WIDTH, NO CORRELATION, NO COVARIANCE, T1', level=2)

		for (algo in algos) {
		
			# multivariate CBI for permuted models, asymmetrical niches
			say(toupper(algo), ', asymmetrical niche width, permuted')
			x <- master$cbiMulti_permT1[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90]
			x1med <- median(x, na.rm=TRUE)
			x1low <- quantile(x, 0.025, na.rm=TRUE)
			x1high <- quantile(x, 0.975, na.rm=TRUE)
			
			x <- master$cbiMulti_permT1[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90]
			x2med <- median(x, na.rm=TRUE)
			x2low <- quantile(x, 0.025, na.rm=TRUE)
			x2high <- quantile(x, 0.975, na.rm=TRUE)
			
			x <- master$cbiMulti_permT1[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90]
			x3med <- median(x, na.rm=TRUE)
			x3low <- quantile(x, 0.025, na.rm=TRUE)
			x3high <- quantile(x, 0.975, na.rm=TRUE)

			say('Median CBI for ', toupper(algo), ', permuted T1, for sigma1 = 0.5, sigma2 = 0.5, rho = 0: ', sprintf('%.2f', x1med), ' (inner 95% CI: ', sprintf('%.2f', x1low), '-', sprintf('%.2f', x1high), ')')
			say('Median CBI for ', toupper(algo), ', permuted T1, for sigma1 = 0.5, sigma2 = 0.3, rho = 0: ', sprintf('%.2f', x2med), ' (inner 95% CI: ', sprintf('%.2f', x2low), '-', sprintf('%.2f', x2high), ')')
			say('Median CBI for ', toupper(algo), ', permuted T1, for sigma1 = 0.5, sigma2 = 0.1, rho = 0: ', sprintf('%.2f', x3med), ' (inner 95% CI: ', sprintf('%.2f', x3low), '-', sprintf('%.2f', x3high), ')', post=2)
			
		}
			
	say('*A*SYMMETRICAL NICHE WIDTH, NO CORRELATION, NO COVARIANCE, T2', level=2)

		for (algo in algos) {
		
			# multivariate CBI for permuted models, asymmetrical niches
			say(toupper(algo), ', asymmetrical niche width, permuted')
			x <- master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90]
			x1med <- median(x, na.rm=TRUE)
			x1low <- quantile(x, 0.025, na.rm=TRUE)
			x1high <- quantile(x, 0.975, na.rm=TRUE)
			
			x <- master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90]
			x2med <- median(x, na.rm=TRUE)
			x2low <- quantile(x, 0.025, na.rm=TRUE)
			x2high <- quantile(x, 0.975, na.rm=TRUE)
			
			x <- master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90]
			x3med <- median(x, na.rm=TRUE)
			x3low <- quantile(x, 0.025, na.rm=TRUE)
			x3high <- quantile(x, 0.975, na.rm=TRUE)

			say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.5, rho = 0: ', sprintf('%.2f', x1med), ' (inner 95% CI: ', sprintf('%.2f', x1low), '-', sprintf('%.2f', x1high), ')')
			say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.3, rho = 0: ', sprintf('%.2f', x2med), ' (inner 95% CI: ', sprintf('%.2f', x2low), '-', sprintf('%.2f', x2high), ')')
			say('Median CBI for ', toupper(algo), ', permuted T2, for sigma1 = 0.5, sigma2 = 0.1, rho = 0: ', sprintf('%.2f', x3med), ' (inner 95% CI: ', sprintf('%.2f', x3low), '-', sprintf('%.2f', x3high), ')', post=2)
			
		}

say('####################################')
say('### [bivariate] niche covariance ###')
say('####################################')

	# generalization
	scenarioDir <- './Results/bivariate' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'rho' # name of x-axis variable column in evaluation data frame
	decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	xlab <- bquote('Niche covariance (' * rho * ')') # x-axis label

	# responses to plot
	resps <- c('Multivariate CBI', 'Multivariate AUCpa', 'Multivariate AUCbg', 'Multivariate CORpa', 'Multivariate CORbg')
	
	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	evals <- evals[evals$rotT2 %==% 90 & evals$sigma1 %==% 0.3 & evals$sigma2 %==% 0.3, ]

	# plot multivariate model results
	multivariatePlotsTRUEvsTRUE(scenarioDir=scenarioDir, evalDir=evalDir, xCol=xCol, decs=decs, xlab=xlab, evals=evals, resps=resps)
		
say('##############################################################################')
say('### [bivariate] landscape correlation x niche covariance bar plots for CBI ###')
say('##############################################################################')

	scenarioDir <- './Results/bivariate'
	load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))
	
	# STRATEGY:
	# 3x3-panel plot with each panel representing a different combination of landscape correlation (r) and niche covariance (rho)
	# landscape correlation changes across rows, niche covariance across columns
	# each panel is an xy plot with sigma1 and sigma2 as axes
	# each pair of simga1 and sigma2 have a bar plot displaying results for OMNI plus the focal algorithm
	# will be making one plot per algorithm

	# value of response indicating no different from random
	rand <- 0
	
	# generalization
	rhos <- c(-0.5, 0, 0.5)
	rots <- c(45, 90, 135)
	
	# settings
	sigmas <- c(1, 3, 5) / 10
	xSize <- 0.13 # maximum width of subplot containing bars
	ySize <- 0.15 # maximum height of subplot containing bars
	width <- 0.2 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.075 # length of subplot tick marks
	lwd <- 0.5 # line width of bars
	cexAxisLabel <- 0.25
	cexPanelLabel <- 0.3
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels
	
	correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')

	#####################################################
	### plot bars as scaled subplots in a larger plot ###
	#####################################################
	
	# basically this just rescales the size and position of values and bars and send the information to rect()
	rescaledRectForSubplot <- function(resp, xOffsetInSubplot, colFill, border, angle=NULL, colAngle=NULL, ...) {
	
		# resp		values of response (not scaled)
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# colFill	color of fill
		# border	border  color for border
		# angle		NULL or angle of fill lines
		# colAngle 	NULL or color of angle lines (if any)
		# ...		other

		lineDensity <- 110 # density of lines per inch for perturbed OMNI
		lineDensityScaling <- 1.2
		
		respScaled <- resp * ySize + sigma2 - 0.5 * ySize
		at <- sigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colFill, border=border, lwd=lwd)
		if (!is.null(angle)) rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colAngle, border=border, angle=angle, density=lineDensityScaling * lineDensity, lwd=lwd)
		
	}
	
	### by ALGORITHM
	for (algo in sdmAlgos) {
	# for (algo in 'brt') {
	
		algoNice <- algosShort(algo)
		png(paste0(scenarioDir, '/Results - Bar Plot for CBI - ', algoNice, '.png'), width=2 * 650, height=2 * 700, res=600)
	
		par(mfrow=c(3, 3), oma=c(2, 1, 0, 0.1), mar=c(0, 0.7, 0.3, 0), pty='s', mgp=c(3, 0.1, 0), cex.axis=1.15 * cexAxisLabel)
		
		countPanel <- 1
		
		# rows
		for (countRho in rev(seq_along(rhos))) {
		# for (countRho in 1) {
		
			rho <- rhos[countRho]
		
			# columns
			for (countRot in rev(seq_along(rots))) {
			# for (countRot in 1) {
	
				rot <- rots[countRot]
				
				omni <- master[master$algo == 'omniscient' & master$rho %==% rho & master$rotT2 %==% rot, ]
				sdm <- master[master$algo == algo & master$rho %==% rho & master$rotT2 %==% rot, ]
	
				lims <- c(min(sigmas) - 0.1, max(sigmas) + 0.1)
	
				plot(0, type='n', axes=FALSE, ann=FALSE, xlim=lims, ylim=lims, col=NA)
				axis(1, at=sigmas, labels=rep('', length(sigmas)), tck=-0.03, lwd=0.4, line=-0.1)
				axis(2, at=sigmas, labels=sigmas, tck=-0.03, lwd=0.4, line=0.1)
				text(sigmas, rep(min(sigmas) - 0.16, length(sigmas)), labels=sigmas, cex=cexAxisLabel, xpd=NA)
				if (countRot == length(rots)) mtext(bquote('Niche breadth in T2 (' * sigma[2] * ')'), side=2, line=0.55, at=mean(sigmas), cex=cexAxisLabel)
				if (countRho == 1) mtext(bquote('Niche breadth in T1 (' * sigma[1] * ')'), side=1, line=0, at=mean(sigmas), cex=cexAxisLabel)
				
				# plot each subplot in a panel
				for (sigma2 in sigmas) {
				
					for (sigma1 in sigmas) {
						
						# standard (as simulated)
						if (sigma1 >= sigma2) {

							omniControl <- omni$cbiMulti[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]
							omniT1 <- omni$cbiMulti_permT1[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]
							omniT2 <- omni$cbiMulti_permT2[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]

							sdmControl <- sdm$cbiMulti[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]
							sdmT1 <- sdm$cbiMulti_permT1[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]
							sdmT2 <- sdm$cbiMulti_permT2[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]

						# flipping T1 and T2 since symmetrical
						} else if (sigma1 < sigma2) {

							omniControl <- omni$cbiMulti[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							omniT1 <- omni$cbiMulti_permT2[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							omniT2 <- omni$cbiMulti_permT1[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							
							sdmControl <- sdm$cbiMulti[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							sdmT1 <- sdm$cbiMulti_permT2[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							sdmT2 <- sdm$cbiMulti_permT1[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]

						}

						# calculate and assign variables for lower/upper limits and median
						whats <- c('Inner', 'Median', 'Outer')
						for (modelType in c('sdm', 'omni')) {
							for (variable in c('Control', 'T1', 'T2')) {
								
								thisVar <- paste0(modelType, variable)
								x <- get(thisVar)
								quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

								for (countWhat in seq_along(whats)) {
							
									what <- whats[countWhat]
									assign(paste0(thisVar, what), quants[countWhat])
									
								}
							}
						}

						# subplot y-axis
						subaxisLwd <- 0.3
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize), c(sigma2 - 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
						
						# subplot y-axis tick lines and labels
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 + 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2, sigma2), lwd=subaxisLwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 - 0.5 * ySize, sigma2 - 0.5 * ySize), lwd=subaxisLwd)
						
						# subplot y-axis labels
						cex <- 0.2
						if (sigma1 == 0.1) {

							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2, labels=0.5, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 - 0.5 * ySize, labels=0, cex=cex, xpd=NA)
							
						}
						
						# gray background
						offsetInSubplot <- 0.075
						left <- sigma1 - 0.5 * xSize + offsetInSubplot * xSize
						right <- sigma1 + 0.5 * xSize + 3 * offsetInSubplot * xSize
						bottom <- sigma2 - 0.5 * ySize
						top <- sigma2 + 0.5 * ySize
						polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						lines(c(left, right), c(sigma2, sigma2), lwd=1.5 * lwd, col='white')

						# OMNI control (unpermuted)
						rescaledRectForSubplot(
							resp=omniControl,
							angle=NULL,
							xOffsetInSubplot=0.2,
							colFill='white',
							colAngle='black',
							border='black'
						)
						
						# SDM control (unpermuted)
						rescaledRectForSubplot(
							resp=sdmControl,
							angle=NULL,
							xOffsetInSubplot=0.35,
							colFill=colSdmControl,
							colAngle=NULL,
							border=borderSdmControl
						)
						
						# OMNI permuted T1
						rescaledRectForSubplot(
							resp=omniT1,
							angle=NULL,
							xOffsetInSubplot=0.55,
							colFill='white',
							colAngle=borderOmniT1,
							border=borderOmniT1
						)

						# SDM permuted T1
						rescaledRectForSubplot(
							resp=sdmT1,
							angle=NULL,
							xOffsetInSubplot=0.7,
							colFill=colSdmT1,
							colAngle=NULL,
							border=borderSdmT1
						)
						
						# OMNI permuted T2
						rescaledRectForSubplot(
							resp=omniT2,
							angle=NULL,
							xOffsetInSubplot=0.9,
							colFill='white',
							colAngle=borderOmniT2,
							border=borderOmniT2
						)
						
						# SDM permuted T2
						rescaledRectForSubplot(
							resp=sdmT2,
							angle=NULL,
							xOffsetInSubplot=1.05,
							colFill=colSdmT2,
							colAngle=NULL,
							border=borderSdmT2
						)

						# accuracy indicator(s)
						sdmT1Nas <- sum(!is.na(sdmT1))
						sdmT2Nas <- sum(!is.na(sdmT2))
						if (sdmT1Nas > 1 & sdmT2Nas > 1) {
				
							acc <- character()
				
							# discrimination
							wellDiscrim <- discriminatedBivariate(T1=omniT1, T2=omniT2) & discriminatedBivariate(T1=sdmT1, T2=sdmT2)
							if (wellDiscrim) acc <- c(acc, discrimSymbol)
							
							# calibration
							wellCalib <- calibratedBivariate(omniControl=omniControl, omniT1=omniT1, omniT2=omniT2, sdmControl=sdmControl, sdmT1=sdmT1, sdmT2=sdmT2, calibTol=calibTol)
							if (wellCalib) acc <- c(acc, calibSymbol)
							
							x <- left + 0.8 * xSize
							y <- bottom + 0.12 * ySize
							text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1 * labCex, pos=2)
							
						}

						# figure label
						r <- correlations$cor[correlations$rot == rot]
						r <- round(r, 2)
						letter <- letters[countPanel]
						lab <- bquote(.(letter) * ') r = ' * .(r) * ' and ' * rho * ' = ' * .(rho))
						labelFig(lab, adj=c(-0, 0), cex=cexPanelLabel)
						
					} # next sigma1
					
				} # next sigma2
		
				# legend
				if (countRho == 1 & countRot == 2) {
				
					cexLeg <- 0.375
					inset <- -0.475
					par(lwd=lwd)
				
					legend('bottom', inset=inset, xpd=NA, ncol=3, cex=cexLeg, bty='n',
						legend=c('OMNI unpermuted', paste0(algoNice, ' unpermuted'),
						'OMNI T1 permuted', paste0(algoNice, ' T1 permuted'),
						'OMNI T2 permuted', paste0(algoNice, ' T2 permuted')),
						fill=c('white', colSdmControl, 'white', colSdmT1, 'white', colSdmT2),
						border=c(borderOmniControl, borderSdmControl, borderSdmT1, borderSdmT1, borderSdmT2, borderSdmT2)
					)
					
				}
						
		
				countPanel <- countPanel + 1
		
			} # next rotation
			
		} # next rho

		title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		dev.off()
	
	} # next algorithm

say('##############################################################################')
say('### [bivariate] landscape correlation x niche covariance bar plots for AUC ###')
say('##############################################################################')

	scenarioDir <- './Results/bivariate'
	load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))
	
	# STRATEGY:
	# 3x3-panel plot with each panel representing a different combination of landscape correlation (r) and niche covariance (rho)
	# landscape correlation changes across rows, niche covariance across columns
	# each panel is an xy plot with sigma1 and sigma2 as axes
	# each pair of simga1 and sigma2 have a bar plot displaying results for OMNI plus the focal algorithm
	# will be making one plot per algorithm

	# value of response indicating no different from random
	rand <- 0.5

	# generalization
	rhos <- c(-0.5, 0, 0.5)
	rots <- c(45, 90, 135)
	
	# settings
	sigmas <- c(1, 3, 5) / 10
	xSize <- 0.13 # maximum width of subplot containing bars
	ySize <- 0.15 # maximum height of subplot containing bars
	width <- 0.2 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.075 # length of subplot tick marks
	lwd <- 0.5 # line width of bars
	cexAxisLabel <- 0.25
	cexPanelLabel <- 0.3
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels
	
	correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')

	#####################################################
	### plot bars as scaled subplots in a larger plot ###
	#####################################################
	
	# basically this just rescales the size and position of values and bars and send the information to rect()
	rescaledRectForSubplot <- function(resp, xOffsetInSubplot, colFill, border, angle=NULL, colAngle=NULL, ...) {
	
		# resp		values of response (not scaled)
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# colFill	color of fill
		# border	border  color for border
		# angle		NULL or angle of fill lines
		# colAngle 	NULL or color of angle lines (if any)
		# ...		other

		lineDensity <- 110 # density of lines per inch for perturbed OMNI
		lineDensityScaling <- 1.2
		
		respScaled <- resp * ySize + sigma2 - 0.5 * ySize
		at <- sigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colFill, border=border, lwd=lwd)
		if (!is.null(angle)) rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colAngle, border=border, angle=angle, density=lineDensityScaling * lineDensity, lwd=lwd)
		
	}

	### by TEST STATISTIC
	for (testStat in c('AUCpa', 'AUCbg')) {
				
		if (testStat == 'AUCpa') {

			controlField <- 'aucPresAbsMulti'
			trueField <- 'aucPresAbsMulti_permT1'
			respT2 <- 'aucPresAbsMulti_permT2'
			
		} else if (testStat == 'AUCbg') {
									
			controlField <- 'aucPresBgMulti'
			trueField <- 'aucPresBgMulti_permT1'
			respT2 <- 'aucPresBgMulti_permT2'
			
		}
		
		### by ALGORITHM
		for (algo in sdmAlgos) {
		# for (algo in 'brt') {
		
			algoNice <- algosShort(algo)
			png(paste0(scenarioDir, '/Results - Bar Plot for ', testStat, ' - ', algoNice, '.png'), width=2 * 650, height=2 * 700, res=600)
		
			par(mfrow=c(3, 3), oma=c(2, 1, 0, 0.1), mar=c(0, 0.7, 0.3, 0), pty='s', mgp=c(3, 0.1, 0), cex.axis=1.15 * cexAxisLabel)
			
			countPanel <- 1
			
			# rows
			for (countRho in seq_along(rhos)) {
			# for (countRho in 1) {
			
				rho <- rhos[countRho]
			
				# columns
				for (countRot in rev(seq_along(rots))) {
				# for (countRot in 1) {
		
					rot <- rots[countRot]
					
					omni <- master[master$algo == 'omniscient' & master$rho %==% rho & master$rotT2 %==% rot, ]
					sdm <- master[master$algo == algo & master$rho %==% rho & master$rotT2 %==% rot, ]
		
					lims <- c(min(sigmas) - 0.1, max(sigmas) + 0.1)
		
					plot(0, type='n', axes=FALSE, ann=FALSE, xlim=lims, ylim=lims, col=NA)
					axis(1, at=sigmas, labels=rep('', length(sigmas)), tck=-0.03, lwd=0.4, line=-0.1)
					axis(2, at=sigmas, labels=sigmas, tck=-0.03, lwd=0.4, line=0.1)
					text(sigmas, rep(min(sigmas) - 0.16, length(sigmas)), labels=sigmas, cex=cexAxisLabel, xpd=NA)
					if (countRot == length(rots)) mtext(bquote('Niche breadth in T2 (' * sigma[2] * ')'), side=2, line=0.55, at=mean(sigmas), cex=cexAxisLabel)
					if (countRho == length(rhos)) mtext(bquote('Niche breadth in T1 (' * sigma[1] * ')'), side=1, line=0, at=mean(sigmas), cex=cexAxisLabel)
					
					### plot each subplot
					for (sigma2 in sigmas) {
					
						for (sigma1 in sigmas) {
							
							# standard (as simulated)
							if (sigma1 >= sigma2) {

								omniControl <- omni[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2, controlField]
								omniT1 <- omni[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2, trueField]
								omniT2 <- omni[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2, respT2]

								sdmControl <- sdm[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2, controlField]
								sdmT1 <- sdm[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2, respT2]
								sdmT2 <- sdm[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2, trueField]

							# flipping T1 and T2 since symmetrical
							} else if (sigma1 < sigma2) {

								omniControl <- omni[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1, controlField]
								omniT1 <- omni[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1, respT2]
								omniT2 <- omni[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1, trueField]
								
								sdmControl <- sdm[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1, controlField]
								sdmT1 <- sdm[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1, respT2]
								sdmT2 <- sdm[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1, trueField]
								
							}

							# calculate and assign variables for lower/upper limits and median
							whats <- c('Inner', 'Median', 'Outer')
							for (modelType in c('sdm', 'omni')) {
								for (variable in c('Control', 'T1', 'T2')) {
									
									thisVar <- paste0(modelType, variable)
									x <- get(thisVar)
									quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

									for (countWhat in seq_along(whats)) {
								
										what <- whats[countWhat]
										assign(paste0(thisVar, what), quants[countWhat])
										
									}
								}
							}

							lineDensityScaling <- 1.2
							
							# subplot y-axis
							subaxisLwd <- 0.3
							lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize), c(sigma2 - 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
							
							# subplot y-axis tick lines and labels
							lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 + 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
							lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2, sigma2), lwd=subaxisLwd)
							lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 - 0.5 * ySize, sigma2 - 0.5 * ySize), lwd=subaxisLwd)
							
							# subplot y-axis labels
							cex <- 0.2
							if (sigma1 == 0.1) {

								text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
								text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2, labels=0.5, cex=cex, xpd=NA)
								text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 - 0.5 * ySize, labels=0, cex=cex, xpd=NA)
								
							}
							
							# gray background
							offsetInSubplot <- 0.075
							left <- sigma1 - 0.5 * xSize + offsetInSubplot * xSize
							right <- sigma1 + 0.5 * xSize + 3 * offsetInSubplot * xSize
							bottom <- sigma2 - 0.5 * ySize
							top <- sigma2 + 0.5 * ySize
							polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
							lines(c(left, right), c(sigma2, sigma2), lwd=1.5 * lwd, col='white')

							# OMNI control (unpermuted)
							rescaledRectForSubplot(
								resp=omniControl,
								angle=NULL,
								xOffsetInSubplot=0.2,
								colFill='white',
								colAngle='black',
								border='black'
							)
							
							# SDM control (unpermuted)
							rescaledRectForSubplot(
								resp=sdmControl,
								angle=NULL,
								xOffsetInSubplot=0.35,
								colFill=colSdmControl,
								colAngle=NULL,
								border=borderSdmControl
							)
							
							# OMNI permuted T1
							rescaledRectForSubplot(
								resp=omniT1,
								angle=NULL,
								xOffsetInSubplot=0.55,
								colFill='white',
								colAngle=borderOmniT1,
								border=borderOmniT1
							)

							# SDM permuted T1
							rescaledRectForSubplot(
								resp=sdmT1,
								angle=NULL,
								xOffsetInSubplot=0.7,
								colFill=colSdmT1,
								colAngle=NULL,
								border=borderSdmT1
							)
							
							# OMNI permuted T2
							rescaledRectForSubplot(
								resp=omniT2,
								angle=NULL,
								xOffsetInSubplot=0.9,
								colFill='white',
								colAngle=borderOmniT2,
								border=borderOmniT2
							)
							
							# SDM permuted T2
							rescaledRectForSubplot(
								resp=sdmT2,
								angle=NULL,
								xOffsetInSubplot=1.05,
								colFill=colSdmT2,
								colAngle=NULL,
								border=borderSdmT2
							)

							# accuracy indicator(s)
							sdmT1Nas <- sum(!is.na(sdmT1))
							sdmT2Nas <- sum(!is.na(sdmT2))
							if (sdmT1Nas > 1 & sdmT2Nas > 1) {
					
								acc <- character()
					
								# discrimination
								wellDiscrim <- discriminatedBivariate(T1=omniT1, T2=omniT2) & discriminatedBivariate(T1=sdmT1, T2=sdmT2)
								if (wellDiscrim) acc <- c(acc, discrimSymbol)
								
								# calibration
								wellCalib <- calibratedBivariate(omniControl=omniControl, omniT1=omniT1, omniT2=omniT2, sdmControl=sdmControl, sdmT1=sdmT1, sdmT2=sdmT2, calibTol=calibTol)
								if (wellCalib) acc <- c(acc, calibSymbol)
								
								x <- left + 0.8 * xSize
								y <- bottom + 0.12 * ySize
								text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1 * labCex, pos=2)
								
							}

							# figure label
							r <- correlations$cor[correlations$rot == rot]
							r <- round(r, 2)
							letter <- letters[countPanel]
							lab <- bquote(.(letter) * ') r = ' * .(r) * ' and ' * rho * ' = ' * .(rho))
							labelFig(lab, adj=c(-0, 0), cex=cexPanelLabel)
							
						} # next sigma1
						
					} # next sigma2
			
					# legend
					if (countRho == length(rhos) & countRot == 2) {
					
						cexLeg <- 0.375
						inset <- -0.475
						par(lwd=lwd)
					
						legend('bottom', inset=inset, xpd=NA, ncol=3, cex=cexLeg, bty='n',
							legend=c('OMNI unpermuted', paste0(algoNice, ' unpermuted'),
							'OMNI T1 permuted', paste0(algoNice, ' T1 permuted'),
							'OMNI T2 permuted', paste0(algoNice, ' T2 permuted')),
							fill=c('white', colSdmControl, 'white', colSdmT1, 'white', colSdmT2),
							border=c(borderOmniControl, borderSdmControl, borderSdmT1, borderSdmT1, borderSdmT2, borderSdmT2)
						)
						
					}
			
					countPanel <- countPanel + 1
			
				} # next rotation
				
			} # next rho

			title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
			dev.off()
		
		} # next algorithm
		
	} # next test statistic

say('################################################################################')
say('### [bivariate] landscape correlation x niche covariance bar plots for CORbg ###')
say('################################################################################')

	scenarioDir <- './Results/bivariate'
	load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))
	
	# STRATEGY:
	# 3x3-panel plot with each panel representing a different combination of landscape correlation (r) and niche covariance (rho)
	# landscape correlation changes across rows, niche covariance across columns
	# each panel is an xy plot with sigma1 and sigma2 as axes
	# each pair of simga1 and sigma2 have a bar plot displaying results for OMNI plus the focal algorithm
	# will be making one plot per algorithm

	# value of response indicating no different from random
	rand <- 0

	# generalization
	rhos <- c(-0.5, 0, 0.5)
	rots <- c(45, 90, 135)
	
	# settings
	sigmas <- c(1, 3, 5) / 10
	xSize <- 0.13 # maximum width of subplot containing bars
	ySize <- 0.15 # maximum height of subplot containing bars
	width <- 0.225 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.075 # length of subplot tick marks
	lwd <- 0.5 # line width of bars
	cexAxisLabel <- 0.25
	cexPanelLabel <- 0.3
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels
	
	correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')

	# # function to plots bars as scaled subplots in a larger plot
	# basically this just rescales the size and position of values and bars and send the information to rect()
	rescaledRectForSubplot <- function(resp, xOffsetInSubplot, colFill, border, angle=NULL, colAngle=NULL, ...) {
	
		# resp		values of response (not scaled)
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# colFill	color of fill
		# border	border  color for border
		# angle		NULL or angle of fill lines
		# colAngle 	NULL or color of angle lines (if any)
		# ...		other
	
		lineDensity <- 110 # density of lines per inch for perturbed OMNI
		lineDensityScaling <- 1.2

		respScaled <- resp * ySize + sigma2 - 0.5 * ySize
		at <- sigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colFill, border=border, lwd=lwd)
		if (!is.null(angle)) rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colAngle, border=border, angle=angle, density=lineDensityScaling * lineDensity, lwd=lwd)
		
	}
	
	# by ALGORITHM
	for (algo in sdmAlgos) {
	# for (algo in 'brt') {
	
		algoNice <- algosShort(algo)
		png(paste0(scenarioDir, '/Results - Bar Plot for CORbg - ', algoNice, '.png'), width=2 * 650, height=2 * 700, res=600)
	
		par(mfrow=c(3, 3), oma=c(2, 1, 0, 0.1), mar=c(0, 0.7, 0.3, 0), pty='s', mgp=c(3, 0.1, 0), cex.axis=1.15 * cexAxisLabel)
		
		countPanel <- 1
		
		# rows
		for (countRho in rev(seq_along(rhos))) {
		# for (countRho in 1) {
		
			rho <- rhos[countRho]
		
			# columns
			for (countRot in rev(seq_along(rots))) {
			# for (countRot in 1) {
	
				rot <- rots[countRot]
				
				omni <- master[master$algo == 'omniscient' & master$rho %==% rho & master$rotT2 %==% rot, ]
				sdm <- master[master$algo == algo & master$rho %==% rho & master$rotT2 %==% rot, ]
	
				lims <- c(min(sigmas) - 0.1, max(sigmas) + 0.1)
	
				plot(0, type='n', axes=FALSE, ann=FALSE, xlim=lims, ylim=lims, col=NA)
				axis(1, at=sigmas, labels=rep('', length(sigmas)), tck=-0.03, lwd=0.4, line=-0.1)
				axis(2, at=sigmas, labels=sigmas, tck=-0.03, lwd=0.4, line=0.1)
				text(sigmas, rep(min(sigmas) - 0.16, length(sigmas)), labels=sigmas, cex=cexAxisLabel, xpd=NA)
				if (countRot == length(rots)) mtext(bquote('Niche breadth in T2 (' * sigma[2] * ')'), side=2, line=0.55, at=mean(sigmas), cex=cexAxisLabel)
				if (countRho == 1) mtext(bquote('Niche breadth in T1 (' * sigma[1] * ')'), side=1, line=0, at=mean(sigmas), cex=cexAxisLabel)
				
				# plot each multi-annulus
				for (sigma2 in sigmas) {
				
					for (sigma1 in sigmas) {
						
						# standard (as simulated)
						if (sigma1 >= sigma2) {

							omniT1 <- omni$corPresBgMulti_permT1[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]
							omniT2 <- omni$corPresBgMulti_permT2[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]

							sdmT1 <- sdm$corPresBgMulti_permT1[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]
							sdmT2 <- sdm$corPresBgMulti_permT2[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]

						# flipping T1 and T2 since symmetrical
						} else if (sigma1 < sigma2) {

							omniT1 <- omni$corPresBgMulti_permT2[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							omniT2 <- omni$corPresBgMulti_permT1[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							
							sdmT1 <- sdm$corPresBgMulti_permT2[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							sdmT2 <- sdm$corPresBgMulti_permT1[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							
						}

						# calculate and assign variables for lower/upper limits and median
						whats <- c('Inner', 'Median', 'Outer')
						for (modelType in c('sdm', 'omni')) {
							for (variable in c('T1', 'T2')) {
								
								thisVar <- paste0(modelType, variable)
								x <- get(thisVar)
								quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

								for (countWhat in seq_along(whats)) {
							
									what <- whats[countWhat]
									assign(paste0(thisVar, what), quants[countWhat])
									
								}
							}
						}

						# subplot y-axis
						lineDensityScaling <- 1.2
						subaxisLwd <- 0.3
						
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize), c(sigma2 - 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
						
						# subplot y-axis tick lines and labels
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 + 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2, sigma2), lwd=subaxisLwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 - 0.5 * ySize, sigma2 - 0.5 * ySize), lwd=subaxisLwd)
						
						# subplot y-axis labels
						cex <- 0.2
						if (sigma1 == 0.1) {

							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2, labels=0.5, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 - 0.5 * ySize, labels=0, cex=cex, xpd=NA)
							
						}
						
						# gray background
						offsetInSubplot <- 0.075
						left <- sigma1 - 0.5 * xSize + offsetInSubplot * xSize
						right <- sigma1 + 0.5 * xSize + 3 * offsetInSubplot * xSize
						bottom <- sigma2 - 0.5 * ySize
						top <- sigma2 + 0.5 * ySize
						polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						lines(c(left, right), c(sigma2, sigma2), lwd=1.5 * lwd, col='white')

						# OMNI permuted T1
						rescaledRectForSubplot(
							resp=omniT1,
							angle=NULL,
							xOffsetInSubplot=0.25,
							colFill='white',
							colAngle=borderOmniT1,
							border=borderOmniT1
						)

						# SDM permuted T1
						rescaledRectForSubplot(
							resp=sdmT1,
							angle=NULL,
							xOffsetInSubplot=0.5,
							colFill=colSdmT1,
							colAngle=NULL,
							border=borderSdmT1
						)
						
						# OMNI permuted T2
						rescaledRectForSubplot(
							resp=omniT2,
							angle=NULL,
							xOffsetInSubplot=0.75,
							colFill='white',
							colAngle=borderOmniT2,
							border=borderOmniT2
						)
						
						# SDM permuted T2
						rescaledRectForSubplot(
							resp=sdmT2,
							angle=NULL,
							xOffsetInSubplot=1,
							colFill=colSdmT2,
							colAngle=NULL,
							border=borderSdmT2
						)

						# accuracy indicator(s)
						sdmT1Nas <- sum(!is.na(sdmT1))
						sdmT2Nas <- sum(!is.na(sdmT2))
						if (sdmT1Nas > 1 & sdmT2Nas > 1) {
				
							acc <- character()
				
							# discrimination
							wellDiscrim <- discriminatedBivariate(T1=sdmT1, T2=sdmT2)
							if (wellDiscrim) acc <- c(acc, discrimSymbol)
							
							# calibration
							wellCalib <- calibratedBivariate(omniControl=NULL, omniT1=omniT1, omniT2=omniT2, sdmControl=NULL, sdmT1=sdmT1, sdmT2=sdmT2, calibTol=calibTol)
							if (wellCalib) acc <- c(acc, calibSymbol)
							
							x <- left + 0.8 * xSize
							y <- bottom + 0.12 * ySize
							text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1 * labCex, pos=2)
							
						}

						# figure label
						r <- correlations$cor[correlations$rot == rot]
						r <- round(r, 2)
						letter <- letters[countPanel]
						lab <- bquote(.(letter) * ') r = ' * .(r) * ' and ' * rho * ' = ' * .(rho))
						labelFig(lab, adj=c(-0, 0), cex=cexPanelLabel)
						
					} # next sigma1
					
				} # next sigma2
		
				# legend
				if (countRho == 1 & countRot == 2) {
				
					cexLeg <- 0.375
					inset <- -0.475
					par(lwd=lwd)
				
					# foreground
					legend('bottom', inset=inset, xpd=NA, ncol=2, cex=cexLeg, bty='n',
						legend=c(
							'OMNI T1 permuted',
							paste0(algoNice, ' T1 permuted'),
							'OMNI T2 permuted',
							paste0(algoNice, ' T2 permuted')
						),
						fill=c('white', colSdmT1, 'white', colSdmT2),
						border=c(borderSdmT1, borderSdmT1, borderSdmT2, borderSdmT2)
					)
					
				}
						
		
				countPanel <- countPanel + 1
		
			} # next rotation
			
		} # next rho

		title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		dev.off()
	
	} # next algorithm

say('################################################################################')
say('### [bivariate] landscape correlation x niche covariance bar plots for CORpa ###')
say('################################################################################')

	scenarioDir <- './Results/bivariate'
	load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))
	
	# STRATEGY:
	# 3x3-panel plot with each panel representing a different combination of landscape correlation (r) and niche covariance (rho)
	# landscape correlation changes across rows, niche covariance across columns
	# each panel is an xy plot with sigma1 and sigma2 as axes
	# each pair of simga1 and sigma2 have a bar plot displaying results for OMNI plus the focal algorithm
	# will be making one plot per algorithm

	# value of response indicating no different from random
	rand <- 0

	# generalization
	rhos <- c(-0.5, 0, 0.5)
	rots <- c(45, 90, 135)
	
	# settings
	sigmas <- c(1, 3, 5) / 10
	xSize <- 0.13 # maximum width of subplot containing bars
	ySize <- 0.15 # maximum height of subplot containing bars
	width <- 0.225 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.075 # length of subplot tick marks
	lwd <- 0.5 # line width of bars
	cexAxisLabel <- 0.25
	cexPanelLabel <- 0.3
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels
	
	correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')

	# # function to plots bars as scaled subplots in a larger plot
	# basically this just rescales the size and position of values and bars and send the information to rect()
	rescaledRectForSubplot <- function(resp, xOffsetInSubplot, colFill, border, angle=NULL, colAngle=NULL, ...) {
	
		# resp		values of response (not scaled)
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# colFill	color of fill
		# border	border  color for border
		# angle		NULL or angle of fill lines
		# colAngle 	NULL or color of angle lines (if any)
		# ...		other
	
		lineDensity <- 110 # density of lines per inch for perturbed OMNI
		lineDensityScaling <- 1.2

		respScaled <- resp * ySize + sigma2 - 0.5 * ySize
		at <- sigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colFill, border=border, lwd=lwd)
		if (!is.null(angle)) rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colAngle, border=border, angle=angle, density=lineDensityScaling * lineDensity, lwd=lwd)
		
	}
	
	# by ALGORITHM
	for (algo in sdmAlgos) {
	# for (algo in 'brt') {
	
		algoNice <- algosShort(algo)
		png(paste0(scenarioDir, '/Results - Bar Plot for CORpa - ', algoNice, '.png'), width=2 * 650, height=2 * 700, res=600)
	
		par(mfrow=c(3, 3), oma=c(2, 1, 0, 0.1), mar=c(0, 0.7, 0.3, 0), pty='s', mgp=c(3, 0.1, 0), cex.axis=1.15 * cexAxisLabel)
		
		countPanel <- 1
		
		# rows
		for (countRho in rev(seq_along(rhos))) {
		# for (countRho in 1) {
		
			rho <- rhos[countRho]
		
			# columns
			for (countRot in rev(seq_along(rots))) {
			# for (countRot in 1) {
	
				rot <- rots[countRot]
				
				omni <- master[master$algo == 'omniscient' & master$rho %==% rho & master$rotT2 %==% rot, ]
				sdm <- master[master$algo == algo & master$rho %==% rho & master$rotT2 %==% rot, ]
	
				lims <- c(min(sigmas) - 0.1, max(sigmas) + 0.1)
	
				plot(0, type='n', axes=FALSE, ann=FALSE, xlim=lims, ylim=lims, col=NA)
				axis(1, at=sigmas, labels=rep('', length(sigmas)), tck=-0.03, lwd=0.4, line=-0.1)
				axis(2, at=sigmas, labels=sigmas, tck=-0.03, lwd=0.4, line=0.1)
				text(sigmas, rep(min(sigmas) - 0.16, length(sigmas)), labels=sigmas, cex=cexAxisLabel, xpd=NA)
				if (countRot == length(rots)) mtext(bquote('Niche breadth in T2 (' * sigma[2] * ')'), side=2, line=0.55, at=mean(sigmas), cex=cexAxisLabel)
				if (countRho == 1) mtext(bquote('Niche breadth in T1 (' * sigma[1] * ')'), side=1, line=0, at=mean(sigmas), cex=cexAxisLabel)
				
				# plot each multi-annulus
				for (sigma2 in sigmas) {
				
					for (sigma1 in sigmas) {
						
						# standard (as simulated)
						if (sigma1 >= sigma2) {

							omniT1 <- omni$corPresAbsMulti_permT1[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]
							omniT2 <- omni$corPresAbsMulti_permT2[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]

							sdmT1 <- sdm$corPresAbsMulti_permT1[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]
							sdmT2 <- sdm$corPresAbsMulti_permT2[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]

						# flipping T1 and T2 since symmetrical
						} else if (sigma1 < sigma2) {

							omniT1 <- omni$corPresAbsMulti_permT2[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							omniT2 <- omni$corPresAbsMulti_permT1[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							
							sdmT1 <- sdm$corPresAbsMulti_permT2[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							sdmT2 <- sdm$corPresAbsMulti_permT1[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							
						}

						# calculate and assign variables for lower/upper limits and median
						whats <- c('Inner', 'Median', 'Outer')
						for (modelType in c('sdm', 'omni')) {
							for (variable in c('T1', 'T2')) {
								
								thisVar <- paste0(modelType, variable)
								x <- get(thisVar)
								quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

								for (countWhat in seq_along(whats)) {
							
									what <- whats[countWhat]
									assign(paste0(thisVar, what), quants[countWhat])
									
								}
							}
						}

						lineDensityScaling <- 1.2
						
						# subplot y-axis
						subaxisLwd <- 0.3
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize), c(sigma2 - 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
						
						# subplot y-axis tick lines and labels
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 + 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=subaxisLwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2, sigma2), lwd=subaxisLwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 - 0.5 * ySize, sigma2 - 0.5 * ySize), lwd=subaxisLwd)
						
						# subplot y-axis labels
						cex <- 0.2
						if (sigma1 == 0.1) {

							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2, labels=0.5, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 - 0.5 * ySize, labels=0, cex=cex, xpd=NA)
							
						}
						
						# gray background
						offsetInSubplot <- 0.075
						left <- sigma1 - 0.5 * xSize + offsetInSubplot * xSize
						right <- sigma1 + 0.5 * xSize + 3 * offsetInSubplot * xSize
						bottom <- sigma2 - 0.5 * ySize
						top <- sigma2 + 0.5 * ySize
						polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						lines(c(left, right), c(sigma2, sigma2), lwd=1.5 * lwd, col='white')

						# OMNI permuted T1
						rescaledRectForSubplot(
							resp=omniT1,
							angle=NULL,
							xOffsetInSubplot=0.25,
							colFill='white',
							colAngle=borderOmniT1,
							border=borderOmniT1
						)

						# SDM permuted T1
						rescaledRectForSubplot(
							resp=sdmT1,
							angle=NULL,
							xOffsetInSubplot=0.5,
							colFill=colSdmT1,
							colAngle=NULL,
							border=borderSdmT1
						)
						
						# OMNI permuted T2
						rescaledRectForSubplot(
							resp=omniT2,
							angle=NULL,
							xOffsetInSubplot=0.75,
							colFill='white',
							colAngle=borderOmniT2,
							border=borderOmniT2
						)
						
						# SDM permuted T2
						rescaledRectForSubplot(
							resp=sdmT2,
							angle=NULL,
							xOffsetInSubplot=1,
							colFill=colSdmT2,
							colAngle=NULL,
							border=borderSdmT2
						)

						# accuracy indicator(s)
						sdmT1Nas <- sum(!is.na(sdmT1))
						sdmT2Nas <- sum(!is.na(sdmT2))
						if (sdmT1Nas > 1 & sdmT2Nas > 1) {
				
							acc <- character()
				
							# discrimination
							wellDiscrim <- discriminatedBivariate(T1=sdmT1, T2=sdmT2)
							if (wellDiscrim) acc <- c(acc, discrimSymbol)
							
							# calibration
							wellCalib <- calibratedBivariate(omniControl=NULL, omniT1=omniT1, omniT2=omniT2, sdmControl=NULL, sdmT1=sdmT1, sdmT2=sdmT2, calibTol=calibTol)
							if (wellCalib) acc <- c(acc, calibSymbol)
							
							x <- left + 0.8 * xSize
							y <- bottom + 0.12 * ySize
							text(x, y, labels=paste(acc, collapse=''), xpd=NA, cex=1 * labCex, pos=2)
							
						}

						# figure label
						r <- correlations$cor[correlations$rot == rot]
						r <- round(r, 2)
						letter <- letters[countPanel]
						lab <- bquote(.(letter) * ') r = ' * .(r) * ' and ' * rho * ' = ' * .(rho))
						labelFig(lab, adj=c(-0, 0), cex=cexPanelLabel)
						
					} # next sigma1
					
				} # next sigma2
		
				# legend
				if (countRho == 1 & countRot == 2) {
				
					cexLeg <- 0.375
					inset <- -0.475
					par(lwd=lwd)
				
					# foreground
					legend('bottom', inset=inset, xpd=NA, ncol=2, cex=cexLeg, bty='n',
						legend=c(
							'OMNI T1 permuted',
							paste0(algoNice, ' T1 permuted'),
							'OMNI T2 permuted',
							paste0(algoNice, ' T2 permuted')
						),
						fill=c('white', colSdmT1, 'white', colSdmT2),
						border=c(borderSdmT1, borderSdmT1, borderSdmT2, borderSdmT2)
					)
					
				}
						
		
				countPanel <- countPanel + 1
		
			} # next rotation
			
		} # next rho

		title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		dev.off()
	
	} # next algorithm

# say('#####################################################################################')
# say('### [extra] investigating decline in performance of OMNI control at large extents ###')
# say('#####################################################################################')

	# say('I want to investigate the why OMNI control seems to decline in performance as extent goes above 2048 cells on a side. I am guessing this is due to location of some test presences in highly unlikely locations (ie at very low values of TRUE).', breaks=100)
	
	# # generalization
	# scenarioDir <- 'H:/Global Change Program/Research/ENMs - Predictor Inference/Results/extent'
	# simDir <- paste0(scenarioDir, '/!scenario data')
	# evalDir <- paste0(scenarioDir, '/evaluations')

	# # threshold
	# threshold <- 0 # tabulate proportion of test presences less than this value for each simulation
	
	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	# evals$rangeT1 <- evals$maxT1 - evals$minT1
	
	# xCol <- 'rangeT1' # name of x-axis variable column in evaluation data frame
	# xlab <- 'Range of TRUE variable' # x-axis label

	# # landscape sizes
	# landscapeSizes <- sort(unique(evals$sizeNative))
	
	# # will store proportion of test presences less than the threshold for each simulation
	# test <- data.frame()
	
	# for (landscapeSize in landscapeSizes) {
	
		# for (iter in 1:100) {
	
			# load(paste0(simDir, '/landscape size = ', prefix(landscapeSize, 4), ' cells sim ', prefix(iter, 4), '.RData'))
			# proportLtThold <- sum(sim$testData$testPres$T1 < threshold) / length(sim$testData$testPres$T1)
			
			# cbiMulti <- evals$cbiMulti[evals$algo == 'omniscient' & evals$sizeNative == landscapeSize & evals$iter == iter]
			
			# test <- rbind(
				# test,
				# data.frame(
					# threshold = threshold,
					# landscapeSize = landscapeSize,
					# iter = iter,
					# proportLtThold = proportLtThold,
					# cbiMulti = cbiMulti
				# )
			# )
			
		# }
		
	# }
	
	# plot(0, 0, xlim=c(0, 1), ylim=c(-1, 1), xlab='Proportion of test presences\nwith TRUE < 0', ylab='CBI', col='white')
	
	# cols <- paste0('gray', round(100 / (1 + seq_along(landscapeSizes))))
	
	# for (count in seq_along(landscapeSizes)) {

		# landscapeSize <- landscapeSizes[count]
		# col <- cols[count]
		
		# x <- test$proportLtThold[test$landscapeSize == landscapeSize]
		# y <- test$cbiMulti[test$landscapeSize == landscapeSize]
		
		# xOrder <- order(x)
		# x <- x[xOrder]
		# y <- y[xOrder]
		
		# yTrans <- logitAdj(0.5 * (y + 1), epsilon=0.001)
		
		# lm <- lm(yTrans ~ x)
		# lmPred <- predict(lm)
		# lmPred <- (2 * probitAdj(lmPred, epsilon=0.001)) - 1
		
		# points(x, y, col=col, pch=count)
		# lines(x, lmPred, col=col, lwd=5)
		
	# }
	
	# legend('topright', legend=landscapeSizes, col=cols, lwd=3)

# say('##############################################')
# say('### [extra] sensitivity of CBI to outliers ###')
# say('##############################################')

	# say('The analysis "### [extra] investigating decline in performance of OMNI control at large extents ###" was based on a hypothesis that is correct, but not well-indicated by the analysis in that section. I discovered through trial-and-error that CBI is very sensitivity to improbable test presences (test presences in areas with very low probability of presence. This analysis will demonstrate this.', breaks=100)

	# scenarioDir <- './Results/extent' # scenario directory

	# # probability of presence of a single improbable presence
	# improbPres <- c(10^(-1:-4))
	
	# # background probability of presence
	# bg <- seq(0, 1, length.out=10000)

	# from <- 0.5
	# to <- 1
	
	# png(paste0(scenarioDir, '/Sensitivity of CBI to Improbable Test Presences.png'), width=1200, height=1200, res=600)
		
		# par(oma=rep(0, 4), mar=c(4, 4, 1, 1), cex=0.5)
		
		# plot(1, 1, col='white', xlab=bquote('Probability of improbable presence (log'['10']*')'), ylab='CBI', xlim=range(log10(improbPres)), ylim=c(0, 1))
		
		# for (i in seq_along(improbPres)) {
		
			# improb <- improbPres[i]
			# say(improb)
			# probPres <- seq(from, to, length.out=199)
			# cbi <- contBoyce(c(improb, probPres), bg, numBins=1001)
			# points(log10(improb), cbi, pch=16, col='red')
			
			# probPres <- seq(from, to, length.out=200)
			# cbi <- contBoyce(probPres, bg, numBins=1001)
			# points(log10(improb), cbi)
			
		# }
		
		# legend('bottomright', legend=c('with improbable presence', 'without improbable presence'), pch=c(16, 1), col=c('red', 'black'), bty='n')
		
	# dev.off()
		
#################################
say('DONE!!!', level=1, deco='&')
say(date()) #####################
#################################
