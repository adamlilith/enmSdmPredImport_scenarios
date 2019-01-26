################################################################
### function to create panel plots of measures of importance ###
################################################################
plotPanels <- function(x, directory, suffix=NA, main=NA, var=c('T1', 'F1'), metric='cor', contrast='bg', strat=FALSE, bg=par('bg'), fg=par('fg'), perm=TRUE, reduced=TRUE, univ=TRUE, miny=NULL) {

	# a wrapper for subPlots, will make one plot per dataFrame frame in x
	
	# x		list object of dataframes... each will be plotted seperately
	# directory	
	# main	character list of plot titles
	# suffix character to suffix to file name
	# var	names of variables for which to plot importance
	# metric  'cor' ==> use correlation analysis, 'auc' ==> use auc analysis
	# contrast 'bg' ==> plot metrics that use BG, 'abs' ==> plots metrics that use true absences
	# strat TRUE ==> if "contrast" is "bg" then use stratified background
	# bg	background color
	# fg	foreground color
	# perm, reduced, univ  TRUE ==> plot this kind of response
	# miny	minimum y-axis value

	suffix <- if (is.na(suffix)) { '' } else { paste(suffix, '- ') }
	
	### get minimum y value
	contrast <- paste0(toupper(substr(contrast, 1, 1)), substr(contrast, 2, nchar(contrast)))

	dataFrame <- numeric()

	# get minimum y value
	for (i in seq_along(x)) {

		# full vs permuted model
		if (perm) for (count in seq_along(var)) dataFrame <- c(dataFrame, x[[i]][ , paste0(metric, contrast, 'FullVsPerm_perm', var[count])])

		# full vs univariate
		if (univ) for (count in seq_along(var)) dataFrame <- c(dataFrame, x[[i]][ , paste0(metric, contrast, 'FullVsUnivar_just', var[count])])

		# full vs reduced model
		if (reduced) if (length(var) > 2) for (count in seq_along(var)) dataFrame <- c(dataFrame, x[[i]][ , paste0(metric, contrast, 'FullVsReduced_sans', var[count])])
		
	}
	
	miny <- if (is.null(miny)) { min(dataFrame, na.rm=T) } else { miny }
	
	### master plot
	png(
		paste0(directory, '/results - ', suffix, ifelse(metric=='cor', 'correlation', 'performance'), ' - ', ifelse(contrast=='Bg', 'background as contrast', 'absences as contrast'), ifelse(strat, ' (stratified) ', ''), ' -',  ifelse(perm, ' permute', ''), ifelse(reduced, ' reduced', ''), ifelse(univ, ' univariate', ''), '.png'),
		width=if (length(x)==1) { 2000 } else if (length(x)==3) { 3 * 2000 } else if (length(x)==4) { 2 * 2000 } else if (length(x)==5) { 3 * 2000 } ,
		height=if (length(x)==1) { 1200 } else if (length(x)==3) { 2 * 1000 } else if (length(x)==4) { 2 * 1200 } else if (length(x)==5) { 1200 } ,
		res=600
	)
	
	par(mai=c(0.5, 0.5, 0.3, 0.1), bg=bg, fg=fg, col.main=fg, col.axis=fg, col.lab=fg)
	
	if (length(x)==3) par(mfrow=c(1, 3))
	if (length(x)==4) par(mfrow=c(2, 2))
	if (length(x)==5) par(mfrow=c(1, 5))

	for (i in seq_along(x)) subPlots(x=x[[i]], main=main[i], count=i, panels=length(x), var=var, miny=miny, metric=metric, contrast=contrast, strat=strat, perm=perm, reduced=reduced, univ=univ)
	
	dev.off()
	

}

