####################################################################
### function to create individual plot of measures of importance ###
####################################################################

subPlots <- function(x, main=NA, count=1, panels=length(x), var=c('T1', 'F1'), miny=0, metric='cor', contrast='bg', strat=FALSE, perm=TRUE, reduced=TRUE, univ=TRUE) {

	# x			evalFrame
	# main		title text
	# count		number indicating the number of the plot in the panel (1 = first, 2 = second, etc.)
	# panels 	total number of plots to graph
	# var		names of variables for which to plot importance
	# miny		minimum value of y axis
	# metric  	'cor' ==> use correlation analysis, 'auc' ==> use auc analysis
	# contrast 	'bg' ==> plot metrics that use BG, 'abs' ==> plots metrics that use true absences
	# strat TRUE ==> if "contrast" is "bg" then use stratified background
	# bg		background color
	# fg		foreground color
	# perm, reduced, univ  TRUE ==> plot this kind of response

	### formatting
	cex.main <- if (panels==1) { 0.8 } else if (panels==3) { 1.2 } else if (panels==4) { 0.8 } else if (panels==5) { 0.8 }
	cex.lab <- if (panels==1) { 0.5 } else if (panels==3) { 1 } else if (panels==4) { 0.75 } else if (panels==5) { 0.6 }
	cex.axis <- if (panels==1) { 0.4 } else if (panels==3) { 0.8 } else if (panels==4) { 0.55 } else if (panels==5) { 0.8 }
	tck <- -0.025
	mgp <- if (panels==1) { c(1.4, 0.3, 0) } else if (panels==3) { c(1.8, 0.4, 0) } else if (panels==4) { c(1.6, 0.35, 0) } else if (panels==5) { c(1.4, 0.35, 0) }

	mainy <- if (panels==1) { 1.2 } else if (panels==3) { 1.1 } else if (panels==4) { 1.2 } else if (panels==5) { 1.2 } # y position of main title
	caty <- if (panels <= 3) { 0.15 } else if (panels==4) { 0.225 } else if (panels==5) { 0.15 } # nudge factor for category labels
	
	par(cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, tck=tck, mgp=mgp)
	
	col <- c('brown1', 'forestgreen')
	if (length(var) > 2) col <- c(col, 'cornflowerblue')

	border <- c('firebrick4', 'green3')
	if (length(var) > 2) border <- c(border, 'lightblue')
	
	contrast <- paste0(toupper(substr(contrast, 1, 1)), substr(contrast, 2, nchar(contrast)))

	### create dataFrame matrix
	
	test <- if (!strat) {
		'FullVsPerm_perm'
	} else if (strat) {
		'FullVsPermStrat_perm'
	}
	
	# permute test
	if (perm) {

		dataFrame <- matrix(x[ , paste0(metric, contrast, test, var[1])], ncol=1)
		colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[1])
		
		# full vs permuted model
		for (i in 2:length(var)) {
			dataFrame <- cbind(dataFrame, x[ , paste0(metric, contrast, test, var[i])])
			colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[i])
		}
		
	}

	test <- if (!strat) {
		'FullVsUnivar_just'
	} else if (strat) {
		'FullVsUnivarStrat_just'
	}

	
	# univariate test
	if (perm & univ) {

		for (i in seq_along(var)) {
			dataFrame <- cbind(dataFrame, x[ , paste0(metric, contrast, test, var[i])])
			colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[i])
		}
	
	} else if (!perm & univ) {
	
		dataFrame <- matrix(x[ , paste0(metric, contrast, test, var[1])], ncol=1)
		colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[1])
		
		# full vs permuted model
		for (i in 2:length(var)) {
			dataFrame <- cbind(dataFrame, x[ , paste0(metric, contrast, test, var[i])])
			colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[i])
		}
		
	}

	test <- if (!strat) {
		'FullVsReduced_sans'
	} else if (strat) {
		'FullVsReducedStrat_sans'
	}

	# reduced test
	if ((perm | univ) & reduced) {

		for (i in seq_along(var)) {
			dataFrame <- cbind(dataFrame, x[ , paste0(metric, contrast, test, var[i])])
			colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[i])
		}
	
	} else if (reduced) {
	
		dataFrame <- matrix(x[ , paste0(metric, contrast, test, var[1])], ncol=1)
		colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[1])
		
		# full vs permuted model
		for (i in 2:length(var)) {
			dataFrame <- cbind(dataFrame, x[ , paste0(metric, contrast, test, var[i])])
			colnames(dataFrame)[ncol(dataFrame)] <- paste0(metric, contrast, test, var[i])
		}
		
	}
	
	### plot
	boxplot(
		x=dataFrame,
		names=NA,
		col=if (sum(c(perm, reduced, univ)==1)) { col[1] } else { rep(col, each=length(var)) },
		border=par('fg'),
		main=NA,
		ylim=c(miny, 1),
		ylab=ifelse(metric=='cor', 'Correlation', 'AUC'),
		notch=F
	)

	# title
	if (!is.na(main)) text(x=-0.07, y=mainy, labels=paste0(letters[count], ') ', main), xpd=NA, cex=cex.main, adj=0, col=par('fg'))
	
	# x-axis labels
	srt <- 0
	adj <- 0.5
	
	# category labels
	if (sum(c(perm, reduced, univ))==1) {

		text(x=1:ncol(dataFrame), y=miny - caty * (1 - miny), cex=cex.lab, srt=srt, labels=var, xpd=NA, adj=adj)
		
	} else {

		if (perm) text(x=which(grepl(x=colnames(dataFrame), pattern='perm')), y=miny - caty * (1 - miny), cex=cex.lab, srt=srt, labels=paste('Permuted\n', var), xpd=NA, adj=adj)
		if (univ) text(x=which(grepl(x=colnames(dataFrame), pattern='Univar')), y=miny - caty * (1 - miny), cex=cex.lab, srt=srt, labels=paste('Univar\n', var), xpd=NA, adj=adj)
		if (reduced) text(x=which(grepl(x=colnames(dataFrame), pattern='Reduced')), y=miny - caty * (1 - miny), cex=cex.lab, srt=srt, labels=paste('Reduced\n', var), xpd=NA, adj=adj)
			
	}
	
}

