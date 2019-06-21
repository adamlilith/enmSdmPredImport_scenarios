# source('C:/Ecology/Drive/Research/ENMs - Predictor Inference/Scripts/WORKING.r')

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

	### SCALAR PLOT SETTINGS
	width <- 0.14 # bar width
	nudge <- 0.26 # nudge pair of bars for same algorithm left/right
	subnudge <- nudge / 4 # nudge bars within same class
	lwd <- 0.8 # line width of box borders
	figLabPos <- c(-0.2250, 0.03) # position of figure label
	
	legCex <- 0.44 # legend
	
	ylabX1 <- -0.11 # position of inner y-axis label
	ylabX2 <- -0.175 # position of outer y-axis label
	labCex <- 0.65 # size of algorithm, y-axis, and figure labels
	
	xlabY1 <- -0 # position of inner x-axis sublabels (range of TRUE)
	xlabY2 <- -0.14 # position of outer x-axis label
	
	lineDensity <- 110 # density of lines per inch for perturbed OMNI

	### generic plot function for plots with a scalar along the x-axis and variable importance along the y
	### The x-axis can represent: prevalence, landscape extent, correlation between landscape variables, correlation between variables in shaping the niche, and so on. This function is intended to supply a thematic unity to plots of these types.
	### This function is best for plots with 1 column and 3 rows, one row per algorithm
	plotScalarResp3panel <- function(
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
		respTrue,
		respFalse,
		respControl
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
		# respTrue		field name of response when TRUE is permuted
		# respFalse		field name of response when FALSE is permuted
		# respControl	field name of response for control case (or NULL if none)

		# x-axis values
		x <- sort(unique(evals[ , xCol]))
		
		# base plot
		plot(0, type='n', axes=FALSE, ann=FALSE, xlim=c(0.5, length(x)), ylim=ylim, tcl=-0.25)
		labelFig(lab, adj=figLabPos, cex=labCex)
		usr <- par('usr')
		
		# gray background
		left <- 1 - (2 + ifelse(is.null(respControl), 0.75, 0)) * nudge
		right <- length(x) + (2 + ifelse(is.null(respControl), 0.25, 0)) * nudge
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
			omniResponseTrue <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, respTrue]
			omniResponseFalse <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, respFalse]
			algoResponseTrue <- evals[evals$algo == algo & evals[ , xCol] == thisX, respTrue]
			algoResponseFalse <- evals[evals$algo == algo & evals[ , xCol] == thisX, respFalse]
		
			# if there is a distinct response for control/unperturbed models
			# used when using CBI or AUC
			if (!is.null(respControl)) {
		
				omniControl <- evals[evals$algo == 'omniscient' & evals[ , xCol] == thisX, respControl]
				algoControl <- evals[evals$algo == algo & evals[ , xCol] == thisX, respControl]
			
				# unperturbed OMNI
				rect(omniControl, at=countX - nudge - subnudge, width=width, col=colOmniControl, border=NA, xpd=NA, lwd=lwd)
				rect(omniControl, at=countX - nudge - subnudge, width=width, density=lineDensity, col=colOmniControl, fill=colOmniControl, border=borderOmniControl, xpd=NA, lwd=lwd)
			
				# unperturbed SDM
				rect(algoControl, at=countX - nudge + subnudge, width=width, col=colSdmControl, border=borderSdmControl, xpd=NA, lwd=0.8)

				# # legend
				# leg <- c(
					# 'OMNI control',
					# paste0('OMNI TRUE permuted'),
					# paste0('OMNI FALSE permuted'),
					# paste0(algosShort(algo), ' control'),
					# paste0(algosShort(algo), ' TRUE permuted'),
					# paste0(algosShort(algo), ' FALSE permuted')
				# )
			
				leg <- c(
					'OMNI control',
					paste0('OMNI TRUE'),
					paste0('OMNI FALSE'),
					paste0(algosShort(algo), ' control'),
					paste0(algosShort(algo), ' TRUE'),
					paste0(algosShort(algo), ' FALSE')
				)
			
				par(lwd=0.5)
			
				legend('bottomright', inset=c(0, 0.05), ncol=2, bty='n', legend=leg, cex=legCex, fill=c('white', 'white', 'white', colSdmControl, colTrue, colFalse), border=c(borderOmniControl, borderTrue, borderFalse, borderSdmControl, borderTrue, borderFalse))
				# , density=c(lineDensity, NA, lineDensity, NA))
				
				# nudges for plotting response bars (below)
				omniRespNudge <- nudge - subnudge
				sdmRespNudge <- nudge + subnudge
			
			# if there is no distinct response for control/unperturbed
			# for when plotting correlation metric
			} else {

				# # legend
				# leg <- c(
					# paste0('OMNI ', variableName,' permuted'),
					# paste0(algosShort(algo), ' ', variableName, ' permuted')
				# )
			
				# par(lwd=0.5)

				# legend('bottomright', inset=c(0, 0.025), ncol=1, bty='n', legend=leg, cex=legCex, fill=c(borderResp, colResp), border=c(borderOmniResp, borderResp), density=c(lineDensity, NA))

				# nudges for plotting response bars (below)
				omniRespNudge <- -1 * nudge + subnudge
				sdmRespNudge <- nudge - subnudge

			}
				
			# TRUE response
			rect(omniResponseTrue, at=countX - subnudge, width=width, col='white', border=borderTrue, xpd=NA, lwd=lwd)
			rect(algoResponseTrue, at=countX + subnudge, width=width, col=colTrue, border=borderTrue, xpd=NA, lwd=lwd)

			# FALSE response
			rect(omniResponseFalse, at=countX + nudge - subnudge, width=width, col='white', border=borderFalse, xpd=NA, lwd=lwd)
			rect(algoResponseFalse, at=countX + nudge + subnudge, width=width, col=colFalse, border=borderFalse, xpd=NA, lwd=lwd)

			# significance star
			if (quantile(algoResponseTrue, 0.975, na.rm=TRUE) < quantile(algoResponseFalse, 0.025, na.rm=TRUE)) {
				text(countX, 1.05 * ylim[2], labels='*', xpd=NA)
			}
			
		}
		
	}

say('########################################')
say('### [sample size] simulation results ###')
say('########################################')

	# generalization
	scenarioDir <- './Results/sample size' # scenario directory
	evalDir <- paste0(scenarioDir, '/evaluations')
	xCol <- 'numTrainPres' # name of x-axis variable column in evaluation data frame
	decs <- 0 # number of decimals to show in x-axis variable tick mark labels
	xlab <- 'Number of presences' # x-axis label

	# load evaluations and calculate x-axis variable
	evals <- loadEvals(evalDir, algos=allAlgos, save=TRUE, redo=FALSE)
	
	#### CBI multivariate
	#####################
	
	ylim <- c(-1, 1)
	yTicks <- seq(-1, 1, by=0.25)
	ylab <- 'CBI'
	rand <- 0
	respTrue <- 'cbiMulti_permT1'
	respFalse <- 'cbiMulti_permF1'
	respControl <- 'cbiMulti'
	
	png(paste0(scenarioDir, '/Results - Multivariate Models - CBI - ', paste(toupper(sdmAlgos), collapse=' '), ' 3 panel.png'), width=4 * 250, height=4 * 720, res=600)
		
		par(mfrow=c(3, 1), oma=c(0, 0, 0, 0), mar=c(1.6, 2, 1, 0.5), mgp=c(2, 0.2, 0), cex.axis=0.425, tcl=-0.25)
		
		for (countAlgo in seq_along(sdmAlgos)) {
		# for (countAlgo in 1) {

			algo <- sdmAlgos[countAlgo]
		
			lab <- paste0(letters[countAlgo], ') ', algosShort(algo))
			plotScalarResp3panel(xCol=xCol, decs=decs, xlab=xlab, algo=algo, nudge=nudge, subnudge=subnudge, ylim=ylim, yTicks=yTicks, ylab, lab, rand, respTrue, respFalse, respControl)

		}
		
		# title(sub=date(), outer=TRUE, line=0, cex.sub=0.3)
		
	dev.off()
	
