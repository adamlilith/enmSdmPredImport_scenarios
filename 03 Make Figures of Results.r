### SDM PREDICTOR INFERENCE - ILLUSTRATIONS
### Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org
### source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/03 Make Figures of Results.r')
###
### The code in this document is intended to be run after all models have been calibrated and evaluated. Most of the sections run extremely quickly except for the section that collates evaluation results for the [bivariate] experiment ("### [bivariate] collate evaluations ###") which can take many hours.

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
### [extent] simulation results ###
### [extent] investigating decline in performance of OMNI control at large extents ###
### sensitivity of CBI to outliers ###
### [prevalence] simulation results ###
### [resolution] simulation results ###
### [correlated TRUE & FALSE] simulation results ###

### [bivariate] collate evaluations ###
### [bivariate] statistics ###
### [bivariate] landscape correlation x niche covariance bar plots for CBI ###
### [bivariate] landscape correlation x niche covariance bar plots for COR ###

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
	library(statisfactory)
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
	
	### colors of bars for scenarios with TRUE and FALSE variables
	##############################################################
	
	colTrue <- '#7fbf7b' # perturbed SDM vs TRUE (CB safe)
	borderTrue <- '#1b7837' # perturbed SDM vs TRUE (CB safe)
	
	colFalse <- '#d6604d' # perturbed SDM vs FALSE (CB safe)
	borderFalse <- '#b2182b' # perturbed SDM vs FALSE (CB safe)

	colSdmControl <- 'gray'# 'white'# '#8da0cb' # unperturbed SDM (CB safe)
	borderSdmControl <- 'black'# '#7570b3' # unperturbed SDM (CB safe)
	
	colOmniControl <- 'black' # unperturbed OMNI
	borderOmniControl <- 'black'# 'gray' # unperturbed OMNI
	
	colOmniResp <- 'white' # perturbed OMNI
	borderOmniTrue <- '#1b7837'# 'black' # perturbed OMNI
	borderOmniFalse <- '#b2182b'# 'black' # perturbed OMNI


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
	
	# plot rectangle representing inner x-th quantile of response variable
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
	
	##############################
	### "scalar" plot function ###
	##############################
	
	# variables used by "scalar" plot function
	width <- 0.22 # bar width
	nudge <- 0.22 # nudge pair of bars for same algorithm left/right
	subnudge <- nudge / 3 # nudge bars within same algorithm left/right
	figLabPos <- c(-0.150, 0.05) # position of figure label
	
	legCex <- 0.34 # legend
	
	ylabX1 <- -0.15 # position of inner y-axis label
	ylabX2 <- -0.25 # position of outer y-axis label
	labCex <- 0.55 # size of algorithm, y-axis, and figure labels
	
	xlabY1 <- -0 # position of inner x-axis sublabels (range of TRUE)
	xlabY2 <- -0.23 # position of outer x-axis label
	
	lineDensity <- 110 # density of lines per inch for perturbed OMNI
	
	### generic plot function for plots with a scalar along the x-axis and variable importance along the y
	### The x-axis can represent: prevalence, landscape extent, correlation between landscape variables, correlation between variables in shaping the niche, and so on. This function is intended to supply a thematic unity to plots of these types.
	plotScalarResp <- function(
		xCol,
		decs,
		xlab,
		algo,
		variable,
		nudge,
		subnudge,
		ylim,
		yTicks,
		ylab,
		lab,
		rand,
		resp,
		respControl
	) {
		
		# general idea:
		# graph shows results for one algorithm plus OMNI
		# x-axis: independent variable (prevalence, extent, etc)
		# y-axis: variable importance
		# each level of TRUE variable range: two sets of bars per algorithm, one is control, one is TRUE or FALSE, sets of bars are staggered
		
		# xCol			name of column in evaluation data frame that has values for x axis
		# decs			NULL (use values of xCol as-is for x-axis tick labels) or an integer indicating number
						# of digits to display for x tick labels
		# xlab			x-axis label
		# algo			algorithm (not OMNI)
		# variable		either 'T1' or 'F1'
		# nudge 		amount to move sets of bars belonging to control/treatment model predictions relative to x-axis tick
		# subnudge		amount to move bars belonging to same control/treatment model predictions relative to x-axis tick
		# ylim			y-axis limits
		# ylab			y-axis label
		# yTicks		position of tick marks on y-axis
		# lab			figure label
		# rand			value of response equal to "random prediction" (eg 0.5 for AUC or 0 for CBI)
		# resp			field name of response (minus the variable name, ie "T1" or "F1")
		# respControl	field name of response for control case (or NULL if none)

		# format settings based on variable
		if (variable == 'T1') {
			colResp <- colTrue
			borderResp <- borderTrue
			variableName <- 'TRUE'
			borderOmniResp <- borderOmniTrue
		} else {
			colResp <- colFalse
			borderResp <- borderFalse
			variableName <- 'FALSE'
			borderOmniResp <- borderOmniFalse
		}

		# x-axis values
		x <- sort(unique(evals[ , xCol]))
		
		# base plot
		plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0.5, length(x)), ylim=ylim)
		labelFig(lab, adj=figLabPos, cex=labCex)
		usr <- par('usr')
		
		# gray background
		left <- 1 - (2.5 + ifelse(is.null(respControl), 0.75, 0)) * nudge
		right <- length(x) + (2.5 + ifelse(is.null(respControl), 0.25, 0)) * nudge
		polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col='gray85', border=NA, xpd=NA)
		lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.4, xpd=NA)
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
			omniResponse <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, paste0(resp, variable)]
			algoResponse <- evals[evals$algo == algo & evals[ , xCol] == thisX, paste0(resp, variable)]
		
			# if there is a distinct response for control/unperturbed models
			# used when using CBI or AUC
			if (!is.null(respControl)) {
		
				omniControl <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, respControl]
				algoControl <- evals[evals$algo == algo & evals[ , xCol] == thisX, respControl]
			
				# unperturbed OMNI
				rect(omniControl, at=countX - nudge - subnudge, width=width, col='white', border=NA, xpd=NA, lwd=0.5)
				rect(omniControl, at=countX - nudge - subnudge, width=width, density=lineDensity, col=colOmniControl, fill='white', border=borderOmniControl, xpd=NA, lwd=0.5)
			
				# unperturbed SDM
				rect(algoControl, at=countX - nudge + subnudge, width=width, col=colSdmControl, border=borderSdmControl, xpd=NA, lwd=0.5)

				# legend
				leg <- c(
					'OMNI control',
					paste0(algosShort(algo), ' control'),
					paste0('OMNI ', variableName,' permuted'),
					paste0(algosShort(algo), ' ', variableName, ' permuted')
				)
			
				par(lwd=0.5)
			
				legend('bottomright', inset=c(0, 0.05), ncol=2, bty='n', legend=leg, cex=legCex, fill=c(borderSdmControl, colSdmControl, borderResp, colResp), border=c(borderOmniControl, borderSdmControl, borderOmniResp, borderResp), density=c(lineDensity, NA, lineDensity, NA))
				
				# nudges for plotting response bars (below)
				omniRespNudge <- nudge - subnudge
				sdmRespNudge <- nudge + subnudge
			
			# if there is no distinct response for control/unperturbed
			# for when plotting correlation metric
			} else {

				# legend
				leg <- c(
					paste0('OMNI ', variableName,' permuted'),
					paste0(algosShort(algo), ' ', variableName, ' permuted')
				)
			
				par(lwd=0.5)

				legend('bottomright', inset=c(0, 0.025), ncol=1, bty='n', legend=leg, cex=legCex, fill=c(borderResp, colResp), border=c(borderOmniResp, borderResp), density=c(lineDensity, NA))

				# nudges for plotting response bars (below)
				omniRespNudge <- -1 * nudge + subnudge
				sdmRespNudge <- nudge - subnudge

			}
				
			# OMNI response
			rect(omniResponse, at=countX + omniRespNudge, width=width, col='white', border=NA, xpd=NA, lwd=0.5)
			rect(omniResponse, at=countX + omniRespNudge, width=width, col=colResp, density=lineDensity, border=borderOmniResp, xpd=NA, lwd=0.5)
	
			# perturbed SDM
			rect(algoResponse, at=countX + sdmRespNudge, width=width, col=colResp, border=borderResp, xpd=NA, lwd=0.5)
		
		}
		
	}
	
# say('###################################')
# say('### [simple] simulation results ###')
# say('###################################')

	# scenarioDir <- './Results/simple'
	# evalDir <- paste0(scenarioDir, '/evaluations')

	# evals <- loadEvals(evalDir, algos=allAlgos, save=TRUE, redo=TRUE)

	# # generalization
	# width <- 0.14 # bar width
	# nudge <- 0.22 # nudge left/right
	# figLabPos <- c(-0.15, 0.05) # position of figure label
	
	# ylabX1 <- -0.18 # position of inner y-axis label
	# ylabX2 <- -0.26 # position of outer y-axis label
	# labCex <- 0.45 # size of algorithm, y-axis, and figure labels
	
	# sublabY <- -0.07 # position of TRUE/FALSE variable sublabels
	# sublabCex <- 0.38 # size of TRUE/FALSE sublabels

	# # master plot function
	# plotSimpleResp <- function(nudge, ylim, yTicks, ylab, lab, rand, respT1, respControl, respF1, controlLab) {
		
		# # nudge 	amount to move bars in same group (algorithm) left or right
		# # ylim		y-axis limits
		# # ylab		y-axis label
		# # yTicks	position of tick marks on y-axis
		# # lab		figure label
		# # rand		value of response equal to "random prediction" (eg 0.5 for AUC or 0 for CBI)
		# # respT1	field name of response for TRUE variable
		# # respControl	field name of response for control case (or NULL if none)
		# # respF1	field name of response for FALSE variable
		# # controlLab character, name of bar representing "control" model/prediction
		
		# # adjust nudging of bars in same groups
		# if (is.null(respControl)) nudge <- nudge / 2
		
		# # base plot
		# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0.5, length(algos)), ylim=ylim)
		# labelFig(lab, adj=figLabPos, cex=labCex)
		# usr <- par('usr')

		# # gray background
		# left <- 1 - (2 + ifelse(is.null(respControl), 0.75, 0)) * nudge
		# right <- length(algos) + (2.5 + ifelse(is.null(respControl), 0.25, -0.3)) * nudge
		# polygon(x=c(left, right, right, left), y=c(min(yTicks), min(yTicks), max(yTicks), max(yTicks)), col='gray85', border=NA, xpd=NA)
		# lines(x=c(left, right), y=c(rand, rand), col='white', lwd=1.4, xpd=NA)
		# for (ats in yTicks) lines(x=c(left, right), y=c(ats, ats), col='white', lwd=0.5, xpd=NA)
		# for (i in 1:(length(algos) - 1)) lines(x=c(i + 0.5, i + 0.5), y=c(-1, 1), col='white', lwd=0.5, xpd=NA)
		
		# # x: variable labels
		# axis(1, at=seq_along(algos), labels=rep('', length(algos)), tck=-0.03, lwd=0.8)
		# text(seq_along(algos) - nudge, y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep('TRUE', length(algos)), cex=sublabCex, xpd=NA, srt=90, adj=c(1, 0.3), col=borderTrue)
		# if (!is.null(respControl)) text(seq_along(algos), y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep(controlLab, length(algos)), cex=sublabCex, xpd=NA, srt=90, adj=c(1, 0.3), col='black')
		# text(seq_along(algos) + nudge, y=rep(usr[3] + sublabY * (usr[4] - usr[3]), length(algos)), labels=rep('FALSE', length(algos)), cex=sublabCex, xpd=NA, srt=90, adj=c(1, 0.3), col=borderFalse)
		
		# # x: algorithm labels
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
		
			# if (!is.null(respControl)) rect(control, at=countAlgo, width=width, col='gray', border=borderOmniControl, lwd=0.8)
			# rect(true, at=countAlgo - nudge, width=width, col=colTrue, border=borderTrue, xpd=NA, lwd=0.8)
			# rect(false, at=countAlgo + nudge, width=width, col=colFalse, border=borderFalse, xpd=NA, lwd=0.8)
			
		# }
		
	# }

	# ### multivariate & univariate: CBI
	# ##################################
	
	# algoLabY <- -0.34 # position of algorithm labels

	# png(paste0(scenarioDir, '/Results - Multivariate & Univariate Models - CBI.png'), width=1800, height=900, res=600)
		
		# par(mfrow=c(1, 2), oma=rep(0, 4), mar=c(2.2, 2, 0.5, 0.5), mgp=c(2, 0.2, 0), cex.axis=0.35, lwd=0.6)
		
		# # CBI multivariate
		# lab <- bquote('a) Multivariate models: CBI')
		# ylab <- bquote('CBI')
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# respT1 <- 'cbiMulti_permT1'
		# respControl <- 'cbiMulti'
		# respF1 <- 'cbiMulti_permF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Control')
		
		# # CBI univariate
		# lab <- bquote('b) Univariate models: CBI')
		# ylab <- bquote('CBI')
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# respT1 <- 'cbiUni_onlyT1'
		# respControl <- 'cbiMulti'
		# respF1 <- 'cbiUni_onlyF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Multivar')
		
		# title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	# dev.off()

	# ### multivariate & univariate: AUCpa
	# ####################################
	
	# algoLabY <- -0.34 # position of algorithm labels

	# png(paste0(scenarioDir, '/Results - Multivariate & Univariate Models - AUCpa.png'), width=1800, height=900, res=600)
		
		# par(mfrow=c(1, 2), oma=rep(0, 4), mar=c(2.2, 2, 0.5, 0.5), mgp=c(2, 0.2, 0), cex.axis=0.35, lwd=0.6)
		
		# # multivariate
		# lab <- bquote('a) Multivariate models: AUC'['pa'])
		# ylab <- bquote('AUC'['pa'])
		# ylim <- c(0, 1)
		# yTicks <- seq(0, 1, by=0.25)
		# respT1 <- 'aucPresAbsMulti_permT1'
		# respControl <- 'aucPresAbsMulti'
		# respF1 <- 'aucPresAbsMulti_permF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Control')
		
		# # univariate
		# lab <- bquote('b) Univariate models: AUC'['pa'])
		# ylab <- bquote('AUC'['pa'])
		# ylim <- c(0, 1)
		# yTicks <- seq(0, 1, by=0.25)
		# respT1 <- 'aucPresAbsUni_onlyT1'
		# respControl <- 'aucPresAbsMulti'
		# respF1 <- 'aucPresAbsUni_onlyF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Multivar')
		
		# title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	# dev.off()

	# ### multivariate & univariate: AUCbg
	# ####################################
	
	# algoLabY <- -0.34 # position of algorithm labels

	# png(paste0(scenarioDir, '/Results - Multivariate & Univariate Models - AUCbg.png'), width=1800, height=900, res=600)
		
		# par(mfrow=c(1, 2), oma=rep(0, 4), mar=c(2.2, 2, 0.5, 0.5), mgp=c(2, 0.2, 0), cex.axis=0.35, lwd=0.6)
		
		# # multivariate
		# lab <- bquote('a) Multivariate models: AUC'['bg'])
		# ylab <- bquote('AUC'['bg'])
		# ylim <- c(0, 1)
		# yTicks <- seq(0, 1, by=0.25)
		# respT1 <- 'aucPresBgMulti_permT1'
		# respControl <- 'aucPresBgMulti'
		# respF1 <- 'aucPresBgMulti_permF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Control')
		
		# # univariate
		# lab <- bquote('b) Univariate models: AUC'['bg'])
		# ylab <- bquote('AUC'['bg'])
		# ylim <- c(0, 1)
		# yTicks <- seq(0, 1, by=0.25)
		# respT1 <- 'aucPresBgUni_onlyT1'
		# respControl <- 'aucPresBgMulti'
		# respF1 <- 'aucPresBgUni_onlyF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Multivar')
		
		# title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
	# dev.off()

	# ### multivariate COR
	# ####################
	
	# algoLabY <- -0.34 # position of algorithm labels

	# png(paste0(scenarioDir, '/Results - Multivariate & Univariate Models - COR.png'), width=1800, height=900, res=600)
		
		# par(mfrow=c(1, 2), oma=rep(0, 4), mar=c(2.2, 2, 0.5, 0.5), mgp=c(2, 0.2, 0), cex.axis=0.35, lwd=0.6)
		
		# # multivariate CORpa
		# lab <- bquote('a) Multivariate models: COR'['pa'])
		# ylab <- bquote('COR'['pa'])
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# respT1 <- 'corPresAbsMulti_permT1'
		# respControl <- NULL
		# respF1 <- 'corPresAbsMulti_permF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Control')
		
		# # multivariate CORbg
		# lab <- bquote('b) Multivariate models: COR'['bg'])
		# ylab <- bquote('COR'['bg'])
		# ylim <- c(-1, 1)
		# yTicks <- seq(-1, 1, by=0.5)
		# respT1 <- 'corPresBgMulti_permT1'
		# respControl <- NULL
		# respF1 <- 'corPresBgMulti_permF1'

		# plotSimpleResp(nudge=nudge, ylim=ylim, ylab=ylab, lab=lab, yTicks=yTicks, rand=0, respT1=respT1, respControl=respControl, respF1=respF1, controlLab='Control')
		
		# title(sub=date(), outer=TRUE, cex.sub=0.2, line=-0.82)
		
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
	
	# x <- evals$brtMultiNativeImportT1[evals$algo=='brt']
	# avg <- mean(x, na.rm=TRUE)
	# quants <- quantile(x, c(0.025, 0.975), na.rm=TRUE)
	# say('Mean algorithm-specific importance for multivariate BRT model for TRUE variable is ', sprintf('%.2f', avg), ' (inner 95% range:', sprintf('%.2f', quants[1]), '-', sprintf('%.2f', quants[2]), ')')
	
	# x <- evals$brtMultiNativeImportF1[evals$algo=='brt']
	# avg <- mean(x, na.rm=TRUE)
	# quants <- quantile(x, c(0.025, 0.975), na.rm=TRUE)
	# say('Mean algorithm-specific importance for multivariate BRT model for FALSE variable is ', sprintf('%.2f', avg), ' (inner 95% range:', sprintf('%.2f', quants[1]), '-', sprintf('%.2f', quants[2]), ')')
	
	# x <- evals$aucPresAbsMulti[evals$algo=='brt']
	# successes <- sum(!is.na(x))
	# say('Number of times multivariate BRT converged (out of 100): ', successes)
	
	# x <- evals$cbiMulti[evals$algo=='omniscient']
	# stats <- quantile(x, c(0.025, 0.5, 0.975))
	# say('2.5th/median/97.5th quantiles of CBI for multivariate control OMNISCIENT model: ', paste(sprintf('%.2f', stats), collapse=' '))
	
	# x <- evals$cbiMulti[evals$algo=='omniscient']
	# stats <- quantile(x, c(0, 0.5, 1))
	# say('Min/median/max CBI for multivariate control OMNISCIENT model: ', paste(sprintf('%.2f', stats), collapse=' '))
	
	# x <- evals$cbiUni_onlyT1[evals$algo=='brt']
	# successes <- sum(!is.na(x))
	# say('Number of times univariate BRT converged using just TRUE variable (out of 100): ', successes)
	
	# x <- evals$cbiUni_onlyF1[evals$algo=='brt']
	# say('Number of times univariate BRT converged using just FALSE variable (out of 100): ', successes)
	# successes <- sum(!is.na(x))
	
# say('########################################')
# say('### [sample size] simulation results ###')
# say('########################################')

	# # generalization
	# scenarioDir <- './Results/sample size' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'numTrainPres' # name of x-axis variable column in evaluation data frame
	# decs <- 0 # number of decimals to show in x-axis variable tick mark labels
	# xlab <- 'Number of presences' # x-axis label

	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=allAlgos, save=TRUE, redo=FALSE)
	
	# #### CBI multivariate
	# #####################
	
	# ylim <- c(-1, 1)
	# yTicks <- seq(-1, 1, by=0.25)
	# ylab <- 'CBI'
	# rand <- 0
	# resp <- 'cbiMulti_perm'
	# respControl <- 'cbiMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - CBI - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
	# ### COR multivariate
	# ####################
	
	# ylim <- c(-0.25, 1)
	# yTicks <- seq(-0.25, 1, by=0.25)
	# ylab <- bquote('COR'['bg'])
	# rand <- 0
	# resp <- 'corPresBgMulti_perm'
	# respControl <- NULL
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - COR - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

	# ### AUCpa
	# #########
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['pa'])
	# rand <- 0.5
	# resp <- 'aucPresAbsMulti_perm'
	# respControl <- 'aucPresAbsMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCpa - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

	# ### AUCbg
	# #########
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['bg'])
	# rand <- 0.5
	# resp <- 'aucPresBgMulti_perm'
	# respControl <- 'aucPresAbsMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCbg - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

# say('###################################')
# say('### [extent] simulation results ###')
# say('###################################')

	# # generalization
	# scenarioDir <- './Results/extent' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'rangeT1' # name of x-axis variable column in evaluation data frame
	# decs <- NULL # number of decimals to show in x-axis variable tick mark labels
	# xlab <- 'Range of TRUE variable' # x-axis label

	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=allAlgos, save=TRUE, redo=FALSE)
	# evals$rangeT1 <- evals$maxT1 - evals$minT1
	
	# ### CBI multivariate
	# ####################
	
	# ylim <- c(-1, 1)
	# yTicks <- seq(-1, 1, by=0.25)
	# ylab <- 'CBI'
	# rand <- 0
	# resp <- 'cbiMulti_perm'
	# respControl <- 'cbiMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - CBI - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
	# ### AUCpa multivariate
	# ######################
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['pa'])
	# rand <- 0
	# resp <- 'aucPresAbsMulti_perm'
	# respControl <- 'aucPresAbsMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCpa - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
	# ### AUCbg multivariate
	# ######################
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['bg'])
	# rand <- 0
	# resp <- 'aucPresBgMulti_perm'
	# respControl <- 'aucPresBgMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCbg - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
	# ### COR multivariate
	# ####################
	# ylim <- c(-0.25, 1)
	# yTicks <- seq(-0.25, 1, by=0.25)
	# ylab <- bquote('COR'['bg'])
	# rand <- 0
	# resp <- 'corPresBgMulti_perm'
	# respControl <- NULL
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - COR - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

# say('######################################################################################')
# say('### [extent] investigating decline in performance of OMNI control at large extents ###')
# say('######################################################################################')

	# say('I want to investigate the why OMNI control seems to decline in performance as extent goes above 2048 cells on a side. I am guessing this is due to location of some test presences in highly unlikely locations (ie at very low values of TRUE).', breaks=100)
	
	# # generalization
	# scenarioDir <- 'H:/Global Change Program/Research/ENMs - Predictor Inference/Results/extent'
	# simDir <- paste0(scenarioDir, '/!scenario data')
	# evalDir <- paste0(scenarioDir, '/evaluations')

	# # threshold
	# threshold <- 0 # tabulate proportion of test presences less than this value for each simulation
	
	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=allAlgos, save=TRUE, redo=FALSE)
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

# say('######################################')
# say('### sensitivity of CBI to outliers ###')
# say('######################################')

	# say('The analysis "### [extent] investigating decline in performance of OMNI control at large extents ###" was based on a hypothesis that is correct, but not well-indicated by the analysis in that section. I discovered through trial-and-error that CBI is very sensitivity to improbable test presences (test presences in areas with very low probability of presence. This analysis will demonstrate this.', breaks=100)

	# # probability of presence of a single improbable presence
	# improbPres <- c(0.9, 10^(-1:-9))
	
	# # probability of presence of probable presences
	# probPres <- seq(0.9, 1, length.out=199)
	
	# # background probability of presence
	# bg <- seq(eps(), 1, length.out=100000)
	
	# plot(1, 1, col='white', xlab=bquote('Probability of improbable presence (log'['10']*')'), ylab='CBI', xlim=range(log10(improbPres)), ylim=c(0, 1))
	
	# for (i in seq_along(improbPres)) {
	
		# improb <- improbPres[i]
		# say(improb)
		# cbi <- contBoyce(pres=c(probPres, improb), bg=bg, numBins=1001)
		# points(log10(improb), cbi, pch=16, col='red')
		
		# cbi <- contBoyce(pres=probPres, bg=bg, numBins=1001)
		# points(log10(improb), cbi)
		
	# }
	
# say('#######################################')
# say('### [prevalence] simulation results ###')
# say('#######################################')

	# # generalization
	# scenarioDir <- './Results/prevalence' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'prevalence' # name of x-axis variable column in evaluation data frame
	# decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	# xlab <- 'Prevalence' # x-axis label

	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=allAlgos, save=TRUE, redo=TRUE)
	
	# # generalization
	# width <- 0.22 # bar width
	# nudge <- 0.22 # nudge pair of bars for same algorithm left/right
	# subnudge <- nudge / 3 # nudge bars within same algorithm left/right
	# figLabPos <- c(-0.150, 0.05) # position of figure label
	
	# legCex <- 0.34 # legend
	
	# ylabX1 <- -0.15 # position of inner y-axis label
	# ylabX2 <- -0.25 # position of outer y-axis label
	# labCex <- 0.55 # size of algorithm, y-axis, and figure labels
	
	# xlabY1 <- -0 # position of inner x-axis sublabels (range of TRUE)
	# xlabY2 <- -0.23 # position of outer x-axis label

	# ### CBI multivariate
	# ####################
	
	# ylim <- c(-1, 1)
	# yTicks <- seq(-1, 1, by=0.25)
	# ylab <- 'CBI'
	# rand <- 0
	# resp <- 'cbiMulti_perm'
	# respControl <- 'cbiMulti'

	# png(paste0(scenarioDir, '/Results - Multivariate Models - CBI - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {
		# # for (countAlgo in 1) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
	# ### COR multivariate
	# ####################
	
	# ylim <- c(-0.25, 1)
	# yTicks <- seq(-0.25, 1, by=0.25)
	# ylab <- bquote('COR'['bg'])
	# rand <- 0
	# resp <- 'corPresBgMulti_perm'
	# respControl <- NULL
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - COR - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

	# ### AUCpa multivariate
	# ######################
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['pa'])
	# rand <- 0.5
	# resp <- 'aucPresAbsMulti_perm'
	# respControl <- 'aucPresAbsMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCpa - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

	# ### AUCbg multivariate
	# ######################
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['bg'])
	# rand <- 0.5
	# resp <- 'aucPresBgMulti_perm'
	# respControl <- 'aucPresBgMulti'
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCbg - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

# say('#######################################')
# say('### [resolution] simulation results ###')
# say('#######################################')

	# say('Basic layout: Three panels side-by-side, one per algorithm.')
	# say('Each panel has 3 columns (resolution) and 4 rows (SAC).')
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
	# noisesRounded <- round(noises, 2)
	
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
		# rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=col, border=border, angle=angle, density=s * lineDensity, lwd=lwd)
		
	# }
	
	# ### plot CBI, AUCpa, AUCbg
	# ##########################
	
	# respStatistics <- c('CBI', 'AUCpa', 'AUCbg')
	# respControls <- c('cbiMulti', 'aucPresAbsMulti', 'aucPresBgMulti')
	# resps <- c('cbiMulti_perm', 'aucPresAbsMulti_perm', 'aucPresBgMulti_perm')
	
	# # y position of guidelines in subplots
	# respRands <- c(0, 0.5, 0.5)
	# respMins <- c(-1, 0, 0)

	# # by TEST STATISTIC
	# for (countStat in seq_along(respStatistics)) {
	
		# respStatistic <- respStatistics[countStat]
		# respControl <- respControls[countStat]
		# resp <- resps[countStat]
	
		# respRand <- respRands[countStat]
		# respMin <- respMins[countStat]
		
		# png(paste0(scenarioDir, '/Results - ', respStatistic, ' - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=9 * 300, height=4 * 300, res=600)
		
			# par(mfrow=c(1, 3), oma=c(2, 2, 1, 0), mar=c(0, 1.4, 0, 0), mgp=c(1, 0.2, 0), cex.axis=0.55)

			# # by ALGO
			# for (countAlgo in seq_along(sdmAlgos)) {
			
				# algo <- sdmAlgos[countAlgo]
			
				# algoNice <- algosShort(algo)
		
				# xs <- seq_along(grains)
				# ys <- seq_along(noises)
				# xlim <- range(xs) + c(-0.5, 0.5)
				# ylim <- range(ys) + c(-0.5, 0.5)
				
				# ylab <- 'Proportion swapped\n\U2190lower autocorrelation     higher autocorrelation\U2192'
				
				# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
				# axis(1, at=xs, labels=paste0('1/', grains), tck=-0.01, lwd=0.6, line=-0.25)
				# axis(2, at=ys, labels=rev(noisesRounded), tck=-0.015, lwd=0.6, line=0.25)
				# mtext('Grain size', side=1, cex=0.4, line=0.7)
				# labelFig(paste0(letters[countAlgo], ') ', algoNice), adj=0.015, cex=0.625, xpd=NA)
				
				# # by NOISE (SAC)
				# for (countNoise in seq_along(noises)) {
			
					# noise <- noises[countNoise]
					# subplotPosY <- length(noises) + 1 - countNoise # y-axis position of subplot
			
					# # by GRAIN
					# for (countGrain in seq_along(grains)) {
					
						# grain <- grains[countGrain]

						# # get response data
						# omniControl <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, respControl]
						# sdmControl <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, respControl]
						
						# omniT1 <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
						# omniF1 <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]
						
						# sdmT1 <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
						# sdmF1 <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]

						# # calculate and assign variables for lower/upper limits and median
						# whats <- c('Inner', 'Median', 'Outer')
						# for (modelType in c('sdm', 'omni')) {
							# for (variable in c('Control', 'T1', 'F1')) {
								
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
						# cex <- 0.4
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
						# polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						# lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

						# # OMNI control (unpermuted)
						# scaleRespToSubplot(
							# resp=omniControl,
							# angle=45,
							# xOffsetInSubplot=0.2,
							# col='black',
							# border='black'
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
							# angle=45,
							# xOffsetInSubplot=0.55,
							# col=colTrue,
							# border=borderOmniTrue
						# )

						# # SDM permuted T1
						# scaleRespToSubplot(
							# resp=sdmT1,
							# angle=NULL,
							# xOffsetInSubplot=0.7,
							# col=colTrue,
							# border=borderSdmT1
						# )
						
						# # OMNI permuted F1
						# scaleRespToSubplot(
							# resp=omniF1,
							# angle=45,
							# xOffsetInSubplot=0.9,
							# col=colFalse,
							# border=borderOmniFalse
						# )
						
						# # SDM permuted F1
						# scaleRespToSubplot(
							# resp=sdmF1,
							# angle=NULL,
							# xOffsetInSubplot=1.05,
							# col=colFalse,
							# border=borderFalse
						# )

					# } # next grain

				# } # next noise level (SAC)
				
			# } # next algorithm

			# # panel y-axis labels
			# mtext(ylab, side=2, cex=0.4, line=0, outer=TRUE)

			# title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		
		# dev.off()

	# } # next test statistic
	
	# ### plot COR
	# ############
	
	# respStatistic <- 'COR'
	# resp <- 'corPresBgMulti_perm'

	# respRand <- 0.5
	# respMin <- 0

	# png(paste0(scenarioDir, '/Results - ', respStatistic, ' - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=9 * 300, height=4 * 300, res=600)
	
		# par(mfrow=c(1, 3), oma=c(2, 2, 1, 0), mar=c(0, 1.4, 0, 0), mgp=c(1, 0.2, 0), cex.axis=0.55)

		# # by ALGO
		# for (countAlgo in seq_along(sdmAlgos)) {
		
			# algo <- sdmAlgos[countAlgo]
		
			# algoNice <- algosShort(algo)
	
			# xs <- seq_along(grains)
			# ys <- seq_along(noises)
			# xlim <- range(xs) + c(-0.5, 0.5)
			# ylim <- range(ys) + c(-0.5, 0.5)
			
			# ylab <- 'Proportion swapped\n\U2190lower autocorrelation     higher autocorrelation\U2192'
			
			# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=xlim, ylim=ylim, col=NA)
			# axis(1, at=xs, labels=paste0('1/', grains), tck=-0.01, lwd=0.6, line=-0.25)
			# axis(2, at=ys, labels=rev(noisesRounded), tck=-0.015, lwd=0.6, line=0.25)
			# mtext('Grain size', side=1, cex=0.4, line=0.7)
			# labelFig(paste0(letters[countAlgo], ') ', algoNice), adj=0.015, cex=0.625, xpd=NA)
			
			# # by NOISE (SAC)
			# for (countNoise in seq_along(noises)) {
		
				# noise <- noises[countNoise]
				# subplotPosY <- length(noises) + 1 - countNoise # y-axis position of subplot
		
				# # by GRAIN
				# for (countGrain in seq_along(grains)) {
				
					# grain <- grains[countGrain]

					# # get response data
					# omniT1 <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
					# omniF1 <- evals[evals$algo == 'omniscient' & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]
					
					# sdmT1 <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'T1')]
					# sdmF1 <- evals[evals$algo == algo & evals$sizeResampled %==% grain & evals$noise %==% noise, paste0(resp, 'F1')]

					# # calculate and assign variables for lower/upper limits and median
					# whats <- c('Inner', 'Median', 'Outer')
					# for (modelType in c('sdm', 'omni')) {
						# for (variable in c('T1', 'F1')) {
							
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
					# cex <- 0.4
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
					# polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
					# lines(c(left, right), c(subplotPosY, subplotPosY), lwd=1.5 * lwd, col='white')

					# # OMNI permuted T1
					# scaleRespToSubplot(
						# resp=omniT1,
						# angle=45,
						# xOffsetInSubplot=0.25,
						# col=colTrue,
						# border=borderOmniTrue
					# )

					# # SDM permuted T1
					# scaleRespToSubplot(
						# resp=sdmT1,
						# angle=NULL,
						# xOffsetInSubplot=0.4,
						# col=colTrue,
						# border=borderSdmT1
					# )
					
					# # OMNI permuted F1
					# scaleRespToSubplot(
						# resp=omniF1,
						# angle=45,
						# xOffsetInSubplot=0.7,
						# col=colFalse,
						# border=borderOmniFalse
					# )
					
					# # SDM permuted F1
					# scaleRespToSubplot(
						# resp=sdmF1,
						# angle=NULL,
						# xOffsetInSubplot=0.85,
						# col=colFalse,
						# border=borderFalse
					# )

				# } # next grain

			# } # next noise level (SAC)
			
		# } # next algorithm

		# # panel y-axis labels
		# mtext(ylab, side=2, cex=0.4, line=0, outer=TRUE)

		# title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
	
	# dev.off()
	
# say('####################################################')
# say('### [correlated TRUE & FALSE] simulation results ###')
# say('####################################################')

	# # generalization
	# scenarioDir <- './Results/correlated TRUE & FALSE' # scenario directory
	# evalDir <- paste0(scenarioDir, '/evaluations')
	# xCol <- 'correlation' # name of x-axis variable column in evaluation data frame
	# decs <- 2 # number of decimals to show in x-axis variable tick mark labels
	# xlab <- 'Correlation between TRUE and FALSE' # x-axis label

	# # load evaluations and calculate x-axis variable
	# evals <- loadEvals(evalDir, algos=allAlgos, save=TRUE, redo=FALSE)

	# correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')
	# evals$correlation <- correlations$cor[match(evals$rotF1, correlations$rot)]
	
	# ### CBI multivariate
	# ####################
	
	# ylim <- c(-1, 1)
	# yTicks <- seq(-1, 1, by=0.25)
	# ylab <- 'CBI'
	# rand <- 0
	# resp <- 'cbiMulti_perm'
	# respControl <- 'cbiMulti'

	# png(paste0(scenarioDir, '/Results - Multivariate Models - CBI - ', paste0(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {
		# # for (countAlgo in 1) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
	# ### COR multivariate
	# ####################
	
	# ylim <- c(-0.5, 1)
	# yTicks <- seq(-0.5, 1, by=0.25)
	# ylab <- bquote('COR'['bg'])
	# rand <- 0
	# resp <- 'corPresBgMulti_perm'
	# respControl <- NULL
	
	# png(paste0(scenarioDir, '/Results - Multivariate Models - COR - ', paste(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()

	# ### AUCpa multivariate
	# ######################
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['pa'])
	# rand <- 0.5
	# resp <- 'aucPresAbsMulti_perm'
	# respControl <- 'aucPresAbsMulti'

	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCpa - ', paste0(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {
		# # for (countAlgo in 1) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
	# ### AUCbg multivariate
	# ######################
	
	# ylim <- c(0, 1)
	# yTicks <- seq(0, 1, by=0.25)
	# ylab <- bquote('AUC'['bg'])
	# rand <- 0.5
	# resp <- 'aucPresBgMulti_perm'
	# respControl <- 'aucPresBgMulti'

	# png(paste0(scenarioDir, '/Results - Multivariate Models - AUCbg - ', paste0(toupper(sdmAlgos), collapse=' '), '.png'), width=1800, height=2400, res=600)
		
		# par(mfrow=c(3, 2), oma=c(1, 0.5, 0.2, 0.1), mar=c(2.5, 2, 1, 1.2), mgp=c(2, 0.2, 0), cex.axis=0.425)
		
		# for (countAlgo in seq_along(sdmAlgos)) {
		# # for (countAlgo in 1) {

			# algo <- sdmAlgos[countAlgo]
		
			# lab <- paste0(letters[2 * countAlgo - 1], ') ', algosShort(algo), ' versus TRUE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='T1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)

			# lab <- paste0(letters[2 * countAlgo] , ') ', algosShort(algo), ' versus FALSE variable')
			# plotScalarResp(xCol=xCol, decs=decs, xlab=xlab, algo=algo, variable='F1', nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, resp, respControl)
		
		
		# }
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	# dev.off()
	
# say('#######################################')
# say('### [bivariate] collate evaluations ###')
# say('#######################################')

	# evalDir <- './Results/bivariate/evaluations'
	# evals <- loadEvals(evalDir, algos=c('omniscient', 'brt', 'gam', 'maxent'), save=TRUE, redo=FALSE)

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
			x1 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.5 & master$sigma2 %==% 0.5 & master$rho == 0 & master$rotT2 %==% 90]), 2)
			x2 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.3 & master$sigma2 %==% 0.3 & master$rho == 0 & master$rotT2 %==% 90]), 2)
			x3 <- round(median(master$cbiMulti_permT2[master$algo == algo & master$sigma1 %==% 0.1 & master$sigma2 %==% 0.1 & master$rho == 0 & master$rotT2 %==% 90]), 2)

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

	# generalization
	rhos <- c(-0.5, 0, 0.5)
	rots <- c(45, 90, 135)
	
	# settings
	sigmas <- c(1, 3, 5) / 10
	xSize <- 0.13 # maximum width of subplot containing bars
	ySize <- 0.15 # maximum height of subplot containing bars
	width <- 0.2 # width of bars as a proportion of subplot size (real width will be size * width)
	tick <- 0.075 # length of subplot tick marks
	lwd <- 0.3 # line width of bars
	cexAxisLabel <- 0.25
	cexPanelLabel <- 0.3
	
	correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')

	# # function to plots bars as scaled subplots in a larger plot
	# basically this just rescales the size and position of values and bars and send the information to rect()
	subRect0to1 <- function(resp, xOffsetInSubplot, colFill, border, angle=NULL, colAngle=NULL, ...) {
	
		# resp		values of response (not scaled)
		# xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# colFill	color of fill
		# border	border  color for border
		# angle		NULL or angle of fill lines
		# colAngle 	NULL or color of angle lines (if any)
		# ...		other
	
		respScaled <- resp * ySize + sigma2 - 0.5 * ySize
		at <- sigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colFill, border=border, lwd=lwd)
		if (!is.null(angle)) rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colAngle, border=border, angle=angle, density=lineDensityScaling * lineDensity, lwd=lwd)
		
	}
	
	# by ALGORITHM
	for (algo in sdmAlgos) {
	# for (algo in 'brt') {
	
		algoNice <- algosShort(algo)
		png(paste0(scenarioDir, '/Results - Bar Plot for CBI - ', algoNice, '.png'), width=2 * 650, height=2 * 700, res=600)
	
		par(mfrow=c(3, 3), oma=c(2, 1, 0, 0.1), mar=c(0, 0.7, 0.3, 0), pty='s', mgp=c(3, 0.1, 0), cex.axis=1.15 * cexAxisLabel)
		
		countPanel <- 1
		
		# rows
		for (countRho in seq_along(rhos)) {
		# for (countRho in 1) {
		
			rho <- rhos[countRho]
		
			# columns
			for (countRot in seq_along(rots)) {
			# for (countRot in 1) {
	
				rot <- rots[countRot]
				
				omni <- master[master$algo == 'omniscient' & master$rho %==% rho & master$rotT2 %==% rot, ]
				sdm <- master[master$algo == algo & master$rho %==% rho & master$rotT2 %==% rot, ]
	
				lims <- c(min(sigmas) - 0.1, max(sigmas) + 0.1)
	
				plot(0, type='n', axes=FALSE, ann=FALSE, xlim=lims, ylim=lims, col=NA)
				axis(1, at=sigmas, labels=rep('', length(sigmas)), tck=-0.03, lwd=0.4, line=-0.1)
				axis(2, at=sigmas, labels=sigmas, tck=-0.03, lwd=0.4, line=0.1)
				text(sigmas, rep(min(sigmas) - 0.16, length(sigmas)), labels=sigmas, cex=cexAxisLabel, xpd=NA)
				if (countRot == 1) mtext(bquote('Niche width in T2 (' * sigma[2] * ')'), side=2, line=0.55, at=mean(sigmas), cex=cexAxisLabel)
				if (countRho == length(rhos)) mtext(bquote('Niche width in T1 (' * sigma[1] * ')'), side=1, line=0, at=mean(sigmas), cex=cexAxisLabel)
				
				# plot each multi-annulus
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

						lineDensityScaling <- 1.2
						
						# subplot y-axis
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize), c(sigma2 - 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=lwd)
						
						# subplot y-axis tick lines and labels
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 + 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=lwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2, sigma2), lwd=lwd)
						lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 - 0.5 * ySize, sigma2 - 0.5 * ySize), lwd=lwd)
						
						# subplot y-axis labels
						cex <- 0.2
						if (sigma1 == 0.1) {

							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2, labels=0.5, cex=cex, xpd=NA)
							text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 - 0.5 * ySize, labels=0, cex=cex, xpd=NA)
							
						}
						
						# gray background
						offsetInSubplot <- 0.075
						rand <- 
						left <- sigma1 - 0.5 * xSize + offsetInSubplot * xSize
						right <- sigma1 + 0.5 * xSize + 3 * offsetInSubplot * xSize
						bottom <- sigma2 - 0.5 * ySize
						top <- sigma2 + 0.5 * ySize
						polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						lines(c(left, right), c(sigma2, sigma2), lwd=1.5 * lwd, col='white')

						# OMNI control (unpermuted)
						subRect0to1(
							resp=omniControl,
							angle=45,
							xOffsetInSubplot=0.2,
							colFill='white',
							colAngle='black',
							border='black'
						)
						
						# SDM control (unpermuted)
						subRect0to1(
							resp=sdmControl,
							angle=NULL,
							xOffsetInSubplot=0.35,
							colFill=colSdmControl,
							colAngle=NULL,
							border=borderSdmControl
						)
						
						# OMNI permuted T1
						subRect0to1(
							resp=omniT1,
							angle=45,
							xOffsetInSubplot=0.55,
							colFill='white',
							colAngle=borderOmniT1,
							border=borderOmniT1
						)

						# SDM permuted T1
						subRect0to1(
							resp=sdmT1,
							angle=NULL,
							xOffsetInSubplot=0.7,
							colFill=colSdmT1,
							colAngle=NULL,
							border=borderSdmT1
						)
						
						# OMNI permuted T2
						subRect0to1(
							resp=omniT2,
							angle=45,
							xOffsetInSubplot=0.9,
							colFill='white',
							colAngle=borderOmniT2,
							border=borderOmniT2
						)
						
						# SDM permuted T2
						subRect0to1(
							resp=sdmT2,
							angle=NULL,
							xOffsetInSubplot=1.05,
							colFill=colSdmT2,
							colAngle=NULL,
							border=borderSdmT2
						)

						# figure label
						r <- correlations$cor[correlations$rot == rot]
						r <- sprintf('%.2f', r)
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
				
					# background
					legend('bottom', inset=inset, xpd=NA, ncol=3, cex=cexLeg, bty='n',
						legend=c('OMNI control', paste0(algoNice, ' control'),
						'OMNI T1 permuted', paste0(algoNice, ' T1 permuted'),
						'OMNI T2 permuted', paste0(algoNice, ' T2 permuted')),
						fill=c('white', colSdmControl, 'white', colSdmT1, 'white', colSdmT2),
						border=c(borderOmniControl, borderSdmControl, borderOmniT1, borderSdmT1, borderOmniT2, borderSdmT2)
					)
					
					# foreground
					legend('bottom', inset=inset, xpd=NA, ncol=3, cex=cexLeg, bty='n',
						legend=c('OMNI control', paste0(algoNice, ' control'),
						'OMNI T1 permuted', paste0(algoNice, ' T1 permuted'),
						'OMNI T2 permuted', paste0(algoNice, ' T2 permuted')),
						fill=c(NA, colSdmControl, borderOmniT1, colSdmT1, borderOmniT2, colSdmT2),
						border=c(borderOmniControl, borderSdmControl, borderOmniT1, borderSdmT1, borderOmniT2, borderSdmT2),
						angle=c(45, NA, 45, NA, 45, NA),
						density=lineDensityScaling * lineDensity * c(1, NA, 1, NA, 1, NA)
					)
					
				}
						
		
				countPanel <- countPanel + 1
		
			} # next rotation
			
		} # next rho

		title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		dev.off()
	
	} # next algorithm

# say('##############################################################################')
# say('### [bivariate] landscape correlation x niche covariance bar plots for COR ###')
# say('##############################################################################')

	# scenarioDir <- './Results/bivariate'
	# load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))
	
	# # STRATEGY:
	# # 3x3-panel plot with each panel representing a different combination of landscape correlation (r) and niche covariance (rho)
	# # landscape correlation changes across rows, niche covariance across columns
	# # each panel is an xy plot with sigma1 and sigma2 as axes
	# # each pair of simga1 and sigma2 have a bar plot displaying results for OMNI plus the focal algorithm
	# # will be making one plot per algorithm

	# # generalization
	# rhos <- c(-0.5, 0, 0.5)
	# rots <- c(45, 90, 135)
	
	# # settings
	# sigmas <- c(1, 3, 5) / 10
	# xSize <- 0.13 # maximum width of subplot containing bars
	# ySize <- 0.15 # maximum height of subplot containing bars
	# width <- 0.225 # width of bars as a proportion of subplot size (real width will be size * width)
	# tick <- 0.075 # length of subplot tick marks
	# lwd <- 0.3 # line width of bars
	# cexAxisLabel <- 0.25
	# cexPanelLabel <- 0.3
	
	# correlations <- read.csv('./Results/Correlations between Variables as a Function of Rotation between Them.csv')

	# # # function to plots bars as scaled subplots in a larger plot
	# # basically this just rescales the size and position of values and bars and send the information to rect()
	# subRect0to1 <- function(resp, xOffsetInSubplot, colFill, border, angle=NULL, colAngle=NULL, ...) {
	
		# # resp		values of response (not scaled)
		# # xOffsetInSubplot placement along x-axis of subplot, specified as proportion of x-axis length
		# # colFill	color of fill
		# # border	border  color for border
		# # angle		NULL or angle of fill lines
		# # colAngle 	NULL or color of angle lines (if any)
		# # ...		other
	
		# respScaled <- resp * ySize + sigma2 - 0.5 * ySize
		# at <- sigma1 - 0.5 * xSize + xOffsetInSubplot * xSize
		# rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colFill, border=border, lwd=lwd)
		# if (!is.null(angle)) rect(x=respScaled, at=at, width=xSize * width, scale=TRUE, col=colAngle, border=border, angle=angle, density=lineDensityScaling * lineDensity, lwd=lwd)
		
	# }
	
	# # by ALGORITHM
	# for (algo in sdmAlgos) {
	# # for (algo in 'brt') {
	
		# algoNice <- algosShort(algo)
		# png(paste0(scenarioDir, '/Results - Bar Plot for COR - ', algoNice, '.png'), width=2 * 650, height=2 * 700, res=600)
	
		# par(mfrow=c(3, 3), oma=c(2, 1, 0, 0.1), mar=c(0, 0.7, 0.3, 0), pty='s', mgp=c(3, 0.1, 0), cex.axis=1.15 * cexAxisLabel)
		
		# countPanel <- 1
		
		# # rows
		# for (countRho in seq_along(rhos)) {
		# # for (countRho in 1) {
		
			# rho <- rhos[countRho]
		
			# # columns
			# for (countRot in seq_along(rots)) {
			# # for (countRot in 1) {
	
				# rot <- rots[countRot]
				
				# omni <- master[master$algo == 'omniscient' & master$rho %==% rho & master$rotT2 %==% rot, ]
				# sdm <- master[master$algo == algo & master$rho %==% rho & master$rotT2 %==% rot, ]
	
				# lims <- c(min(sigmas) - 0.1, max(sigmas) + 0.1)
	
				# plot(0, type='n', axes=FALSE, ann=FALSE, xlim=lims, ylim=lims, col=NA)
				# axis(1, at=sigmas, labels=rep('', length(sigmas)), tck=-0.03, lwd=0.4, line=-0.1)
				# axis(2, at=sigmas, labels=sigmas, tck=-0.03, lwd=0.4, line=0.1)
				# text(sigmas, rep(min(sigmas) - 0.16, length(sigmas)), labels=sigmas, cex=cexAxisLabel, xpd=NA)
				# if (countRot == 1) mtext(bquote('Niche width in T2 (' * sigma[2] * ')'), side=2, line=0.55, at=mean(sigmas), cex=cexAxisLabel)
				# if (countRho == length(rhos)) mtext(bquote('Niche width in T1 (' * sigma[1] * ')'), side=1, line=0, at=mean(sigmas), cex=cexAxisLabel)
				
				# # plot each multi-annulus
				# for (sigma2 in sigmas) {
				
					# for (sigma1 in sigmas) {
						
						# # standard (as simulated)
						# if (sigma1 >= sigma2) {

							# omniT1 <- omni$corPresBgMulti_permT1[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]
							# omniT2 <- omni$corPresBgMulti_permT2[omni$sigma1 %==% sigma1 & omni$sigma2 %==% sigma2]

							# sdmT1 <- sdm$corPresBgMulti_permT1[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]
							# sdmT2 <- sdm$corPresBgMulti_permT2[sdm$sigma1 %==% sigma1 & sdm$sigma2 %==% sigma2]

						# # flipping T1 and T2 since symmetrical
						# } else if (sigma1 < sigma2) {

							# omniT1 <- omni$corPresBgMulti_permT2[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							# omniT2 <- omni$corPresBgMulti_permT1[omni$sigma1 %==% sigma2 & omni$sigma2 %==% sigma1]
							
							# sdmT1 <- sdm$corPresBgMulti_permT2[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							# sdmT2 <- sdm$corPresBgMulti_permT1[sdm$sigma1 %==% sigma2 & sdm$sigma2 %==% sigma1]
							
						# }

						# # calculate and assign variables for lower/upper limits and median
						# whats <- c('Inner', 'Median', 'Outer')
						# for (modelType in c('sdm', 'omni')) {
							# for (variable in c('T1', 'T2')) {
								
								# thisVar <- paste0(modelType, variable)
								# x <- get(thisVar)
								# quants <- quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)

								# for (countWhat in seq_along(whats)) {
							
									# what <- whats[countWhat]
									# assign(paste0(thisVar, what), quants[countWhat])
									
								# }
							# }
						# }

						# lineDensityScaling <- 1.2
						
						# # subplot y-axis
						# lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize), c(sigma2 - 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=lwd)
						
						# # subplot y-axis tick lines and labels
						# lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 + 0.5 * ySize, sigma2 + 0.5 * ySize), lwd=lwd)
						# lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2, sigma2), lwd=lwd)
						# lines(c(sigma1 - 0.5 * xSize, sigma1 - 0.5 * xSize - tick * xSize), c(sigma2 - 0.5 * ySize, sigma2 - 0.5 * ySize), lwd=lwd)
						
						# # subplot y-axis labels
						# cex <- 0.2
						# if (sigma1 == 0.1) {

							# text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 + 0.5 * ySize, labels=1, cex=cex, xpd=NA)
							# text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2, labels=0.5, cex=cex, xpd=NA)
							# text(sigma1 - 0.5 * xSize - 3.35 * tick * xSize, sigma2 - 0.5 * ySize, labels=0, cex=cex, xpd=NA)
							
						# }
						
						# # gray background
						# offsetInSubplot <- 0.075
						# rand <- 
						# left <- sigma1 - 0.5 * xSize + offsetInSubplot * xSize
						# right <- sigma1 + 0.5 * xSize + 3 * offsetInSubplot * xSize
						# bottom <- sigma2 - 0.5 * ySize
						# top <- sigma2 + 0.5 * ySize
						# polygon(x=c(left, right, right, left), y=c(bottom, bottom, top, top), col='gray90', border=NA, xpd=NA)
						# lines(c(left, right), c(sigma2, sigma2), lwd=1.5 * lwd, col='white')

						# # OMNI permuted T1
						# subRect0to1(
							# resp=omniT1,
							# angle=45,
							# xOffsetInSubplot=0.25,
							# colFill='white',
							# colAngle=borderOmniT1,
							# border=borderOmniT1
						# )

						# # SDM permuted T1
						# subRect0to1(
							# resp=sdmT1,
							# angle=NULL,
							# xOffsetInSubplot=0.5,
							# colFill=colSdmT1,
							# colAngle=NULL,
							# border=borderSdmT1
						# )
						
						# # OMNI permuted T2
						# subRect0to1(
							# resp=omniT2,
							# angle=45,
							# xOffsetInSubplot=0.75,
							# colFill='white',
							# colAngle=borderOmniT2,
							# border=borderOmniT2
						# )
						
						# # SDM permuted T2
						# subRect0to1(
							# resp=sdmT2,
							# angle=NULL,
							# xOffsetInSubplot=1,
							# colFill=colSdmT2,
							# colAngle=NULL,
							# border=borderSdmT2
						# )

						# # figure label
						# r <- correlations$cor[correlations$rot == rot]
						# r <- sprintf('%.2f', r)
						# letter <- letters[countPanel]
						# lab <- bquote(.(letter) * ') r = ' * .(r) * ' and ' * rho * ' = ' * .(rho))
						# labelFig(lab, adj=c(-0, 0), cex=cexPanelLabel)
						
					# } # next sigma1
					
				# } # next sigma2
		
				# # legend
				# if (countRho == length(rhos) & countRot == 2) {
				
					# cexLeg <- 0.375
					# inset <- -0.475
					# par(lwd=lwd)
				
					# # background
					# legend('bottom', inset=inset, xpd=NA, ncol=3, cex=cexLeg, bty='n',
						# legend=c('OMNI T1 permuted', paste0(algoNice, ' T1 permuted'),
						# 'OMNI T2 permuted', paste0(algoNice, ' T2 permuted')),
						# fill=c('white', colSdmT1, 'white', colSdmT2),
						# border=c(borderOmniT1, borderSdmT1, borderOmniT2, borderSdmT2)
					# )
					
					# # foreground
					# legend('bottom', inset=inset, xpd=NA, ncol=3, cex=cexLeg, bty='n',
						# legend=c('OMNI T1 permuted', paste0(algoNice, ' T1 permuted'),
						# 'OMNI T2 permuted', paste0(algoNice, ' T2 permuted')),
						# fill=c(borderOmniT1, colSdmT1, borderOmniT2, colSdmT2),
						# border=c(borderOmniT1, borderSdmT1, borderOmniT2, borderSdmT2),
						# angle=c(45, NA, 45, NA),
						# density=lineDensityScaling * lineDensity * c(1, NA, 1, NA)
					# )
					
				# }
						
		
				# countPanel <- countPanel + 1
		
			# } # next rotation
			
		# } # next rho

		# title(sub=date(), cex.sub=0.4, outer=TRUE, line=3)
		# dev.off()
	
	# } # next algorithm

#################################
say('DONE!!!', level=1, deco='&')
say(date()) #####################
#################################
