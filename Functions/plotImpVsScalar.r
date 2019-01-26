##############################################
### function to plots importance vs scalar ###
##############################################

### panel plotting function -- called by main layout "plotGrad" function
plotImpVsScalar <- function(dataFrame, x, xlab, main, test, var, metric, contrast, strat, miny=0, legend=NA, fg=par('fg'), bg=par('bg'), col=par('fg')) {
	
	# dataFrame		list object, with each element being a data.frame
	# x				numeric list of x-values investigated
	# xlab			x-axis label
	# main			title of plot
	# test			'permute' or 'univar'
	# var			character list names of variables
	# metric		'cor' or 'auc'
	# contrast		'abs' or 'bg'
	# strat			TRUE ==> use stratified BG (only works if contrast='bg'); FALSE ==> depending on "contrast", use test pres/abs or test pres/bg; 
	# miny			minimum value of y
	# legend		keyword describing position of legend (e.g., 'bottomright')
	# fg			foreground color
	# bg			background color
	# col			color in which to plot dataFrame
	
	# col <- if (length(var)==3) { c('white', 'gray60', 'gray35') } else { c('white', 'gray60') }
	pch <- if (length(var)==3) { c(16, 15, 17) } else { c(16, 17) }
	cex.pt <- 1 # points cex
	cex.main <- 0.6
	cex.lab <- 0.6
	cex.axis <- 0.5

	contrast <- ifelse(contrast=='bg', 'Bg', 'Abs')
	test <- if (test=='permute' & !strat) {
		'FullVsPerm_perm'
	} else if (test=='permute' & strat) {
		'FullVsPermStrat_perm'
	} else if (test=='univar' & !strat) {
		'FullVsUnivar_just'
	} else if (test=='univar' & strat) {
		'FullVsUnivarStrat_just'
	}
	
	# plot empty plot
	plot(
		x=c(0, x),
		y=c(0, x/x),
		col=bg,
		col.lab=fg,
		col.axis=fg,
		xlab=xlab,
		ylab=ifelse(metric=='cor', 'Correlation', 'AUC'),
		xlim=c(0, max(x) + 0.5 * min(x)),
		ylim=c(miny, 1.05),
		cex.lab=cex.lab,
		cex.axis=cex.axis
	)

	# plot each predictor variable
	nudge <- -0.00625 * max(x)
	for (countVar in seq_along(var)) {
	
		# collate mean y values plus SDs
		y <- errUpper <- errLower <- numeric()
		for (countX in seq_along(dataFrame)) {

			y <- c(y, mean(dataFrame[[countX]][, paste0(metric, contrast, test, var[countVar])], na.rm=T))
			# errUpper <- c(err, 1.96 * sd(logit((dataFrame[[countX]][ , paste0(metric, contrast, test, var[countVar])] + 1) / 2), na.rm=T))
			errUpper <- c(errUpper, quantile(dataFrame[[countX]][ , paste0(metric, contrast, test, var[countVar])], 0.975, na.rm=T))
			# errLower <- c(err, 1.96 * sd(logit((dataFrame[[countX]][ , paste0(metric, contrast, test, var[countVar])] + 1) / 2), na.rm=T))
			errLower <- c(errLower, quantile(dataFrame[[countX]][ , paste0(metric, contrast, test, var[countVar])], 0.025, na.rm=T))
			
		}

		# error bars
		# errUpper <- logit(y) + err
		# errLower <- logit(y) - err
		# errUpper <- probit(errUpper)
		# errLower <- probit(errLower)
	
		for (i in seq_along(x)) lines(x=c(x[i], x[i]) + nudge, y=c(errUpper[i], errLower[i]), col=ifelse(grepl(x=var[countVar], pattern='T'), 'black', 'gray50'))
	
		points(
			x=x + nudge,
			y=y,
			pch=pch[countVar],
			type='b',
			lwd=1.6,
			cex=cex.pt,
			col=ifelse(grepl(x=var[countVar], pattern='T'), 'black', 'gray50')
		)
	
		nudge <- nudge + nudge * 2
	
	}
		
	if (!is.na(legend)) {
		
		legend(
			x=legend,
			inset=0.03,
			legend=var,
			pch=pch,
			pt.cex=cex.pt,
			cex=cex.lab,
			col=ifelse(grepl(x=var, pattern='T'), 'black', 'gray50')
		)
		
	}
	
	text(x=-0.175 * (max(x) + 0.5 * min(x)), y=1.2, labels=main, xpd=NA, cex=cex.main, adj=0)

}

