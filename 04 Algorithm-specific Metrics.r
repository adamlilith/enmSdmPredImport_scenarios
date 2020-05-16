### SDM PREDICTOR INFERENCE - ILLUSTRATIONS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
### source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/04 Algorithm-specific Metrics.r')
###
### The code in this document is intended to be run after all models have been calibrated and evaluated. It collates results for algorithm-specific metrics. These include AIC weights (GAMs), Maxent's permutation and contribution importance, and BRT's reduction in deviance * frequency of usage metrics.

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
	library(fpCompare)
	library(RColorBrewer)

	# custom libraries (on GitHub, user account adamlilith)
	library(omnibus)
	library(enmSdmPredImport)

##############################
### variables and settings ###
##############################

	### working directory
	setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')

	algos <- c('gam', 'maxent', 'brt')

	# pairs of responses for "scalar" experiments varying one covariate
	resps <- c('maxentMultiContrib', 'maxentMultiPermImport', 'maxentMultiTrainGainWithOnly', 'brtMultiNativeImport')
	respsNice <- c('Maxent Contribution', 'Maxent Permutation', 'Maxent Gain With Only', 'BRT Importance (Deviance Reduction x Frequency)')
	respsYLim <- list(c(0, 1), c(0, 1), c(NA, NA), c(0, 1))
	
	colTrue <- '#7fbf7b' # perturbed SDM vs TRUE (CB safe)
	borderTrue <- '#1b7837' # perturbed SDM vs TRUE (CB safe)
	
	colFalse <- '#d6604d' # perturbed SDM vs FALSE (CB safe)
	borderFalse <- '#b2182b' # perturbed SDM vs FALSE (CB safe)
	
	colT1 <- '#a6dba0' # light green (CB safe)
	borderT1 <- '#008837' # dark green (CB safe)
	
	colT2 <- '#c2a5cf' # light purple (CB safe)
	borderT2 <- '#7b3294' # dark purple (CB safe)

	panelBgCol <- 'gray87' # subpanel background
	
	rectLwd <- 0.8 # line width for sub-panel rectangle borders
	
	discrimSymbol <- '\U002A' # symbol to indicate OMNI and SDM can discriminate between variables
	
	
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

	#####################################################
	### plot scalar response: TRUE vs FALSE scenarios ###
	#####################################################
	
	plotScalarRespTrueVsFalse <- function(scenarioDir, evals, xCol, xlab, decs) {
	
		# scenarioDir		directory to which to save file
		# evals			data frame with evaluations
		# xCol			name of column with x-axis values
		# xlab			x-axis label
		# decs			number of decimal places in x-axis values to print
		
		xs <- sort(unique(evals[ , xCol]))
		xLabels <- round(xs, decs)

		# collate and plot each type of response
		for (countResp in seq_along(resps)) {
		
			resp <- resps[countResp]
			respNice <- respsNice[countResp]
			
			trueResp <- paste0(resp, 'T1')
			falseResp <- paste0(resp, 'F1')
			
			out <- list()

			# get response values for each value of x
			for (x in xs) {
			
				trueMetric <- evals[evals[ , xCol] == x, trueResp]
				falseMetric <- evals[evals[ , xCol] == x, falseResp]
				
				trueMetric <- as.numeric(na.omit(trueMetric))
				falseMetric <- as.numeric(na.omit(falseMetric))
				
				thisOut <- list(trueMetric, falseMetric)
				names(thisOut) <- paste('covariate', x)
				out <- c(out, thisOut)
			
			}
		
			# y-axis limits
			ylim <- respsYLim[[countResp]]
			if (anyNA(ylim)) ylim <- c(0, max(unlist(out)))
		
			png(paste0(scenarioDir, '/Algorithm-specific Importance - ', respNice, '.png'), width=1600, height=1200, res=300)
			
				par(mgp=c(0, 0.1, 0), oma=rep(0, 4), mar=c(2, 2, 0.6, 0.2), cex.lab=0.9, cex.axis=0.65, tck=-0.01)
				plot(seq_along(xs), rep(0, length(xs)), col='white', ylim=ylim, ann=FALSE, xaxt='n', bty='n')
				mtext(respNice, side=2, cex=1, line=0.9, xpd=NA)
				mtext(xlab, side=1, cex=1, line=0.9, xpd=NA)
				axis(1, at=seq_along(xs), labels=xLabels, xlab=xlab)
				
				for (countXVal in seq_along(out)) {
				
					if (countXVal %% 2 == 1) {
						col <- colTrue
						border <- borderTrue
						nudge <- -0.1
						trues <- out[[countXVal]]
					} else {
						col <- colFalse
						border <- borderFalse
						nudge <- 0.1

						# indicate successful discrimination
						falses <- out[[countXVal]]
						discrim <<- discriminatedTrueFalse(trues, falses, TRUE)

						if (discrim) {
							at <- round(0.01 + countXVal / 2)
							usr <- par('usr')
							y <- usr[4]
							text(at, y, labels=discrimSymbol, xpd=NA, cex=1.2)
						}
						
					}
					
					at <- round(0.01 + countXVal / 2) + nudge
						
					rect(out[[countXVal]], at=at, width=0.23, col=col, border=border, lwd=1.2)
					
				} # next rectangle
				
			dev.off()
			
		} # next response type
		
	}

	####################################################
	### plot scalar response: TRUE vs TRUE scenarios ###
	####################################################
	
	plotScalarRespTrueVsTrue <- function(scenarioDir, evals, xCol, xlab, decs, fileNameExtra) {
	
		# scenarioDir		directory to which to save file
		# evals			data frame with evaluations
		# xCol			name of column with x-axis values
		# xlab			x-axis label
		# decs			number of decimal places in x-axis values to print
		# fileNameExtra	extra text for file name
		
		xs <- sort(unique(evals[ , xCol]))
		xLabels <- round(xs, decs)

		# collate and plot each type of response
		for (countResp in seq_along(resps)) {
		
			resp <- resps[countResp]
			respNice <- respsNice[countResp]
			
			true1Resp <- paste0(resp, 'T1')
			true2Resp <- paste0(resp, 'T2')
			
			out <- list()

			# get response values for each value of x
			for (x in xs) {
			
				true1Metric <- evals[evals[ , xCol] == x, true1Resp]
				true2Metric <- evals[evals[ , xCol] == x, true2Resp]
				
				true1Metric <- as.numeric(na.omit(true1Metric))
				true2Metric <- as.numeric(na.omit(true2Metric))
				
				thisOut <- list(true1Metric, true2Metric)
				names(thisOut) <- paste('covariate', x)
				out <- c(out, thisOut)
			
			}
		
			# y-axis limits
			ylim <- respsYLim[[countResp]]
			if (anyNA(ylim)) ylim <- c(0, max(unlist(out)))
		
			png(paste0(scenarioDir, '/Algorithm-specific Importance - ', fileNameExtra, ' ', respNice, '.png'), width=1600, height=1200, res=300)
			
				par(mgp=c(0, 0.1, 0), oma=rep(0, 4), mar=c(2, 2, 0.6, 0.2), cex.lab=0.9, cex.axis=0.65, tck=-0.01)
				plot(seq_along(xs), rep(0, length(xs)), col='white', ylim=ylim, ann=FALSE, xaxt='n', bty='n')
				mtext(respNice, side=2, cex=1, line=0.9, xpd=NA)
				mtext(xlab, side=1, cex=1, line=0.9, xpd=NA)
				axis(1, at=seq_along(xs), labels=xLabels, xlab=xlab)
				
				# for each response
				for (countXVal in seq_along(out)) {
				
					# is this for TRUE or FALSE?
					if (countXVal %% 2 == 1) {
						col <- colT1
						border <- borderT1
						nudge <- -0.1
						trues1 <- out[[countXVal]]
					} else {
						col <- colT2
						border <- borderT2
						nudge <- 0.1

						# indicate successful discrimination
						trues2 <- out[[countXVal]]
						discrim <<- discriminatedBivariate(trues1, trues2)

						if (discrim) {
							at <- round(0.01 + countXVal / 2)
							usr <- par('usr')
							y <- usr[4]
							text(at, y, labels=discrimSymbol, xpd=NA, cex=1.2)
						}
					}
					
					# x position
					at <- round(0.01 + countXVal / 2) + nudge
						
					# plot it
					rect(out[[countXVal]], at=at, width=0.23, col=col, border=border, lwd=1.2)
					
				} # next rectangle
				
			dev.off()
			
		} # next response type
		
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
		sdmT1,			# results from TRUE1
		sdmT2			# results from TRUE2
	) {
		(quantile(sdmT1, 0.975, na.rm=TRUE) < quantile(sdmT2, 0.025, na.rm=TRUE)) | # T2 higher
		(quantile(sdmT1, 0.025, na.rm=TRUE) > quantile(sdmT2, 0.975, na.rm=TRUE)) # T1 higher
	}
	
say('###################################')
say('### [simple] simulation results ###')
say('###################################')

	scenarioDir <- './Results/simple'
	evalDir <- paste0(scenarioDir, '/evaluations')

	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	# nice names for responses
	respsSimple <- c('MAX\nContrib TRUE', 'MAX\nContrib FALSE', 'MAX\nPerm TRUE', 'MAX\nPerm FALSE', 'MAX Gain\nTRUE Only', 'MAX Gain\nFALSE Only', 'BRT\nTRUE', 'BRT\nFALSE')

	# colors
	colsSimple <- c('cornflowerblue', 'cornflowerblue', 'chartreuse', 'chartreuse', 'goldenrod3', 'goldenrod3', 'lavender', 'lavender')
	
	# responses
	out <- list(
		mxContribTrue = evals$maxentMultiContribT1[evals$algo == 'maxent'],
		mxContribFalse = evals$maxentMultiContribF1[evals$algo == 'maxent'],
	
		mxPermTrue = evals$maxentMultiPermImportT1[evals$algo == 'maxent'],
		mxPermFalse = evals$maxentMultiPermImportF1[evals$algo == 'maxent'],
	
		mxGainTrueOnly = evals$maxentMultiTrainGainWithOnlyT1[evals$algo == 'maxent'],
		mxGainFalseOnly = evals$maxentMultiTrainGainWithOnlyF1[evals$algo == 'maxent'],
	
		brtTrue = evals$brtMultiNativeImportT1[evals$algo == 'brt'],
		brtFalse = evals$brtMultiNativeImportF1[evals$algo == 'brt']
		
	)
	
	maxGainWithout <- max(out$mxGainTrueOnly, out$mxGainFalseOnly)
	out$mxGainTrueOnly <- out$mxGainTrueOnly / maxGainWithout
	out$mxGainFalseOnly <- out$mxGainFalseOnly / maxGainWithout
	
	png(paste0(scenarioDir, '/Algorithm-specific Importance.png'), width=1600, height=800, res=300)
	
		par(mgp=c(1, 0.2, 0), oma=rep(0, 4), mar=c(2, 2, 0.2, 0.2), cex.lab=0.6, cex.axis=0.35, tck=-0.01)
		plot(seq_along(out), seq_along(out), col='white', ann=FALSE, ylim=c(0, 1), xaxt='n')
		axis(1, at=seq_along(out), labels=respsSimple, line=0, cex.axis=0.5)
		mtext('Metric', side=2, cex=0.7, line=0.9)
		for (i in seq_along(out)) {
		
			x <- out[[i]]
			col <- colsSimple[i]
			rect(x, at=i, width=0.24, scale=TRUE, quants=c(0.025, 0.975), col=col, border='black')
		
		}
		# boxplot(out, ylab='Metric', varwidth=TRUE, notch=FALSE, names=respsSimple, ylim=c(0, 1), col=colsSimple)
		
	dev.off()

say('########################################')
say('### [sample size] simulation results ###')
say('########################################')

	# generalization
	scenarioDir <- './Results/sample size' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	
	xCol <- 'numTrainPres' # column with x-axis values
	xlab <- 'Calibration occurrences' # xlaxis label
	decs <- 0 # decimals
	
	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	plotScalarRespTrueVsFalse(scenarioDir, evals, xCol, xlab)

say('#######################################')
say('### [prevalence] simulation results ###')
say('#######################################')

	# generalization
	scenarioDir <- './Results/prevalence' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'prevalence' # name of x-axis variable column in evaluation data frame
	xlab <- 'Prevalence' # x-axis label
	decs <- 2 # decimals

	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	
	plotScalarRespTrueVsFalse(scenarioDir=scenarioDir, evals=evals, xCol=xCol, xlab=xlab, decs=decs)

say('###################################')
say('### [extent] simulation results ###')
say('###################################')

	# generalization
	scenarioDir <- './Results/extent' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'rangeT1' # name of x-axis variable column in evaluation data frame
	decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	xlab <- 'Study region extent (range of TRUE)' # x-axis label

	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	evals$rangeT1 <- evals$maxT1 - evals$minT1

	plotScalarRespTrueVsFalse(scenarioDir=scenarioDir, evals=evals, xCol=xCol, xlab=xlab, decs=decs)

say('####################################################')
say('### [correlated TRUE & FALSE] simulation results ###')
say('####################################################')

	# generalization
	scenarioDir <- './Results/correlated TRUE & FALSE' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'correlation' # name of x-axis variable column in evaluation data frame
	decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	xlab <- 'Correlation between TRUE and FALSE' # x-axis label

	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)

	correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')
	evals$correlation <- correlations$cor[match(evals$rotF1, correlations$rot)]
	
	plotScalarRespTrueVsFalse(scenarioDir=scenarioDir, evals=evals, xCol=xCol, xlab=xlab, decs=decs)

say('#######################################')
say('### [resolution] simulation results ###')
say('#######################################')

	say('Layout:')
	say('3 columns (resolution) and 4 rows (SAC).')
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
	width <- 0.23 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.05 # length of subplot tick marks
	lwd <- 0.4 # line width of bars
	cexAxisLabel <- 0.5
	cexPanelLabel <- 0.7
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels	
	axisCex <- 0.37
	

	lineDensity <- NULL
	
	# function to plots bars as scaled subplots in a larger plot
	# basically this just rescales the size and position of values and bars and send the information to rect()
	scaleRespToSubplot <- function(resp, angle, xOffsetInSubplot, col, border, ...) {
	
		# resp		values of response (not scaled)
		# angle		NULL or angle of fill lines
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# col, border  color for fill and border
		# ...		other
	
		respScaled <- if (respNice != 'Maxent Gain With Only') {
			resp * 0.5 * ySize + subplotPosY
		} else {
			(resp / respMax) * ySize -  0.5 * ySize + subplotPosY
		}
		at <- countGrain - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=col, border=border, angle=angle, density=0.8 * lineDensity, lwd=rectLwd)
		
	}
	
	### plot CBI, AUCpa, AUCbg
	##########################
	
	# by TEST STATISTIC
	for (countStat in seq_along(resps)) {
	# for (countStat in 1) {
	
		resp <- resps[countStat]
		respNice <- respsNice[countStat]
		subYLim<- respsYLim[[countStat]]
		
		png(paste0(scenarioDir, '/Algorithm-specific Importance - ', respNice, '.png'), width=1000, height=1400, res=600)
		
			par(oma=c(0, 0, 0, 0), mar=c(0.4, 2.2, 0.1, 0), mgp=c(1, 0, 0), cex.axis=axisCex, tcl=-0.2)

			xs <- seq_along(grains)
			ys <- seq_along(noises)
			xlim <- range(xs) + c(-0.5, 0.5)
			ylim <- range(ys) + c(-0.5, 0.5)
			
			plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
			axis(1, at=xs, labels=NA, tck=-0.01, line=-0.45)
			text(xs, y=rep(0.4, length(grains)), labels=paste0('1/', grains), xpd=NA, cex=axisCex)
			axis(2, at=ys, labels=rev(noisesRounded), tck=-0.015, lwd=lwd, line=-0.03)
			mtext('Grain size', side=1, cex=0.7 * labCex, line=-0.41)
			
			# by NOISE (SAC)
			for (countNoise in seq_along(noises)) {
		
				noise <- noises[countNoise]
				subplotPosY <- length(noises) + 1 - countNoise # y-axis position of subplot
		
				# by GRAIN
				for (countGrain in seq_along(grains)) {
				
					grain <- grains[countGrain]

					# get response data
					sdmTrue <- evals[evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
					sdmFalse <- evals[evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]
					
					sdmTrue <- as.numeric(na.omit(sdmTrue))
					sdmFalse <- as.numeric(na.omit(sdmFalse))

					# calculate and assign variables for lower/upper limits and median
					whats <- c('Inner', 'Median', 'Outer')
					for (modelType in c('sdm')) {
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

					# subplot y-axis
					lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					
					# subplot y-axis tick lines and labels
					lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY + 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY, subplotPosY), lwd=lwd)
					lines(c(countGrain - 0.5 * xSize, countGrain - 0.5 * xSize - tick * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY - 0.5 * ySize), lwd=lwd)
					
					# subplot y-axis labels
					cex <- 0.3
					offset <- 2.5
					
					# gray background
					offsetInSubplot <- 0.1
					rand <- 0
					left <- countGrain - 0.54 * xSize + offsetInSubplot * xSize
					right <- countGrain + 0.5 * xSize + 1.75 * offsetInSubplot * xSize
					bottom <- subplotPosY - 0.5 * ySize
					top <- subplotPosY + 0.5 * ySize
					polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col=panelBgCol, border=NA, xpd=NA)
					# lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

					# sub-panel y-axis values
					respMin <- 0
					respMax <- if (anyNA(subYLim)) {
						round(max(c(sdmTrue, sdmFalse)), 5)
					} else {
						1
					}
					
					text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY + 0.5 * ySize, labels=respMax, cex=cex, xpd=NA)
					text(countGrain - 0.5 * xSize - offset * tick * xSize, subplotPosY - 0.5 * ySize, labels=respMin, cex=cex, xpd=NA)
					
					# SDM TRUE
					scaleRespToSubplot(
						resp=sdmTrue,
						angle=NULL,
						xOffsetInSubplot=0.3,
						col=colTrue,
						border=borderTrue
					)
					
					# SDM permuted FALSE
					scaleRespToSubplot(
						resp=sdmFalse,
						angle=NULL,
						xOffsetInSubplot=0.8,
						col=colFalse,
						border=borderFalse
					)

					## accuracy indicators
					sdmTrueNas <- sum(!is.na(sdmTrue))
					sdmFalseNas <- sum(!is.na(sdmFalse))
					if (sdmTrueNas > 1 & sdmFalseNas > 1) {
			
						acc <- character()
			
						# discrimination
						wellDiscrim <- discriminatedTrueFalse(sdmTrue, sdmFalse, expectHigher=TRUE)
						if (wellDiscrim) {
	
							x <- countGrain + 0.85 * xSize
							y <- bottom + 0.1 * ySize * respMax
							text(x, y, labels=discrimSymbol, xpd=NA, cex=1.5 * labCex, pos=2)
							
						}
							
					}
						
				} # next grain

			} # next noise level (SAC)

			# panel y-axis labels
			ylab <- paste0(respNice, '\nSpatial autocorrelation\n(proportion of cells swapped)\n\U2190lower autocorrelation     higher autocorrelation\U2192')
			mtext(ylab, side=2, cex=0.7 * labCex, line=-1.8, outer=TRUE)

			title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		
		dev.off()

	} # next test statistic
	
say('####################################')
say('### [bivariate] niche covariance ###')
say('####################################')

	# generalization
	scenarioDir <- './Results/bivariate' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'rho' # name of x-axis variable column in evaluation data frame
	decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	xlab <- bquote('Niche covariance (' * rho * ')') # x-axis label
	fileNameExtra <- 'Niche Covariance'

	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=algos, save=TRUE, redo=FALSE)
	evals <- evals[evals$rotT2 %==% 90 & evals$sigma1 %==% 0.3 & evals$sigma2 %==% 0.3, ]

	plotScalarRespTrueVsTrue(scenarioDir=scenarioDir, evals=evals, xCol=xCol, xlab=xlab, decs=decs, fileNameExtra=fileNameExtra)
	
say('##########################################')
say('### [niche breadth] simulation results ###')
say('##########################################')

	say('Layout:')
	say('3 columns (resolution) and 4 rows (SAC).')
	say('Each subpanel is a barplot showing response of OMNI and the SDM.')

	# generalization
	scenarioDir <- './Results/bivariate' # scenario directory

	# load evaluations and calculate x-axis variable
	evals <- loadEvals(
		evalDir=paste0(scenarioDir, '/evaluations'),
		algos=algos,
		save=TRUE,
		redo=FALSE
	)

	# holding these constant
	rho <- 0
	rotT2 <- 90
	
	# SAC
	sigmas <- c(0.1, 0.3, 0.5)
	
	# plot settings
	xSize <- 0.775 # maximum width of subplot containing bars
	ySize <- 0.875 # maximum height of subplot containing bars
	width <- 0.23 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.05 # length of subplot tick marks
	lwd <- 0.4 # line width of bars
	cexAxisLabel <- 0.5
	cexPanelLabel <- 0.7
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels	
	axisCex <- 0.37
	
	lineDensity <- NULL
	
	# function to plots bars as scaled subplots in a larger plot
	# basically this just rescales the size and position of values and bars and send the information to rect()
	scaleRespToSubplot <- function(resp, angle, xOffsetInSubplot, col, border, ...) {
	
		# resp		values of response (not scaled)
		# angle		NULL or angle of fill lines
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# col, border  color for fill and border
		# ...		other
	
		respScaled <- if (respNice != 'Maxent Gain With Only') {
			resp * 0.5 * ySize + subplotPosY
		} else {
			(resp / respMax) * ySize -  0.5 * ySize + subplotPosY
		}
		at <- countSigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=col, border=border, angle=angle, density=0.8 * lineDensity, lwd=rectLwd)
		
	}
	
	### plot
	########
	
	# by TEST STATISTIC
	for (countStat in seq_along(resps)) {
	# for (countStat in 1) {
	
		resp <- resps[countStat]
		respNice <- respsNice[countStat]
		subYLim<- respsYLim[[countStat]]
		
		png(paste0(scenarioDir, '/Algorithm-specific Importance - Niche Breadth ', respNice, '.png'), width=1000, height=1000, res=600)
		
			par(oma=c(0, 0, 0, 0), mar=c(0.8, 1.1, 0.1, 0), mgp=c(1, 0, 0), cex.axis=axisCex, tcl=-0.2)

			xs <- seq_along(sigmas)
			ys <- seq_along(sigmas)
			xlim <- range(xs) + c(-0.5, 0.5)
			ylim <- range(ys) + c(-0.5, 0.5)
			
			xlab <- bquote('Niche breadth in T1 (' * sigma[1] * ')')
			
			plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
			axis(1, at=xs, labels=NA, tck=-0.01, line=-0.2, lwd=lwd)
			text(xs, y=rep(0.35, length(sigmas)), labels=sigmas, xpd=NA, cex=axisCex)
			axis(2, at=ys, labels=sigmas, tck=-0.015, lwd=lwd, line=-0.03)
			mtext(xlab, side=1, cex=0.7 * labCex, line=-0.11)
			ylab <- bquote('Niche breadth in T2 (' * sigma[2] * ')')
			mtext(ylab, side=2, cex=0.7 * labCex, line=-0.7, outer=TRUE)
			
			# by SIGMA2 (rows)
			for (countSigma2 in seq_along(sigmas)) {
		
				sigma2 <- sigmas[countSigma2]
				# subplotPosY <- length(sigmas) + 1 - countSigma2 # y-axis position of subplot
				subplotPosY <- countSigma2 # y-axis position of subplot
		
				# by SIGMA1
				for (countSigma1 in seq_along(sigmas)) {
				
					sigma1 <- sigmas[countSigma1]

					# get response data: standard (as simulated)
					if (sigma1 >= sigma2) {

						sdmT1 <- evals[evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho %==% rho & evals$rotT2 %==% rotT2, paste0(resp, 'T1')]
						sdmT2 <- evals[evals$sigma1 %==% sigma1 & evals$sigma2 %==% sigma2 & evals$rho %==% rho & evals$rotT2 %==% rotT2, paste0(resp, 'T2')]

					# get response data: flipping T1 and T2 since symmetrical
					} else if (sigma1 < sigma2) {

						sdmT1 <- evals[evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho %==% rho & evals$rotT2 %==% rotT2, paste0(resp, 'T1')]
						sdmT2 <- evals[evals$sigma1 %==% sigma2 & evals$sigma2 %==% sigma1 & evals$rho %==% rho & evals$rotT2 %==% rotT2, paste0(resp, 'T2')]

					}
					
					sdmT1 <- as.numeric(na.omit(sdmT1))
					sdmT2 <- as.numeric(na.omit(sdmT2))

					# calculate and assign variables for lower/upper limits and median
					whats <- c('Inner', 'Median', 'Outer')
					for (modelType in c('sdm')) {
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
					lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					
					# subplot y-axis tick lines and labels
					lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize - tick * xSize), c(subplotPosY + 0.5 * ySize, subplotPosY + 0.5 * ySize), lwd=lwd)
					lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize - tick * xSize), c(subplotPosY, subplotPosY), lwd=lwd)
					lines(c(countSigma1 - 0.5 * xSize, countSigma1 - 0.5 * xSize - tick * xSize), c(subplotPosY - 0.5 * ySize, subplotPosY - 0.5 * ySize), lwd=lwd)
					
					# subplot y-axis labels
					cex <- 0.2
					offset <- 2.5
					
					# gray background
					offsetInSubplot <- 0.1
					rand <- 0
					left <- countSigma1 - 0.54 * xSize + offsetInSubplot * xSize
					right <- countSigma1 + 0.5 * xSize + 1.75 * offsetInSubplot * xSize
					bottom <- subplotPosY - 0.5 * ySize
					top <- subplotPosY + 0.5 * ySize
					polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col=panelBgCol, border=NA, xpd=NA)
					# lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

					# sub-panel y-axis values
					respMin <- 0
					respMax <- if (anyNA(subYLim)) {
						round(max(c(sdmT1, sdmT2)), 5)
					} else {
						1
					}
					
					text(countSigma1 - 0.5 * xSize - offset * tick * xSize, subplotPosY + 0.5 * ySize, labels=respMax, cex=cex, xpd=NA)
					text(countSigma1 - 0.5 * xSize - offset * tick * xSize, subplotPosY - 0.5 * ySize, labels=respMin, cex=cex, xpd=NA)
					
					# SDM TRUE
					scaleRespToSubplot(
						resp=sdmT1,
						angle=NULL,
						xOffsetInSubplot=0.3,
						col=colT1,
						border=borderT1
					)
					
					# SDM permuted FALSE
					scaleRespToSubplot(
						resp=sdmT2,
						angle=NULL,
						xOffsetInSubplot=0.8,
						col=colT2,
						border=borderT2
					)

					## accuracy indicators
					sdmT1Nas <- sum(!is.na(sdmT1))
					sdmT2Nas <- sum(!is.na(sdmT2))
					if (sdmT1Nas > 1 & sdmT2Nas > 1) {
			
						acc <- character()
			
						# discrimination
						wellDiscrim <- discriminatedBivariate(sdmT1, sdmT2)
						if (wellDiscrim) {
	
							x <- countSigma1 + 0.85 * xSize
							y <- bottom + 0.1 * ySize * respMax
							text(x, y, labels=discrimSymbol, xpd=NA, cex=1.5 * labCex, pos=2)
							
						}
							
					}
						
				} # next sigma1

			} # next sigma2

			# panel y-axis labels

			title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		
		dev.off()

	} # next test statistic
	
#################################
say('DONE!!!', level=1, deco='&')
say(date()) #####################
#################################

