##############################################
### function to plots importance vs scalar ###
##############################################

plotGrad <- function(dataFrame, x, xlab, var, metric, strat=FALSE, miny=0, directory, suffix='', fg=par('fg'), bg=par('bg'), col1='black', col2='black', legend='bottomright') {

	# dataFrame		list object, with each element a data.frame
	# x				numeric list of x values investigated
	# xlab			x-axis label
	# var			character list names of variables
	# metric		'cor' or 'auc'
	# strat			TRUE for BG plot, use stratifeid BG
	# miny			minimum value of y
	# directory			directory to which to save plot
	# suffix		character string to suffix to file name of plot
	# leg			location of where to put legend

	png(paste0(directory, '/extent plots', ifelse(suffix=='', '', paste0(' - ', suffix)), '.png'), width=2400, height=1200, res=300)
	
	### plot importance vs extent
	par(mfcol=c(2, 3), mai=c(1, 1.4, 1, 1) * 0.3, mgp=c(0.9, 0.2, 0), tck=-0.025, fg=fg, bg=bg)

	# vs absences
	plotImpVsScalar(dataFrame=dataFrame, x=x, xlab=xlab, main='a) Full model vs. permuted model (test presences & absences)', test='permute', var=c('T1', 'F1'), metric='cor', contrast='abs', strat=FALSE, miny=0, legend=legend, fg=fg, bg=bg, col=col1)
	plotImpVsScalar(dataFrame=dataFrame, x=x, xlab=xlab, main='c) Full model vs. univariate model (test presences & absences)', test='univar', var=c('T1', 'F1'), metric='cor', contrast='abs', strat=FALSE, miny=0, legend=NA, fg=fg, bg=bg, col=col1)

	plotImpVsScalar(dataFrame=dataFrame, x=x, xlab=xlab, main='b) Full model vs. permuted model (test presences & background sites)', test='permute', var=c('T1', 'F1'), metric='cor', contrast='bg', strat=FALSE, miny=0, legend=NA, fg=fg, bg=bg, col=col2)
	plotImpVsScalar(dataFrame=dataFrame, x=x, xlab=xlab, main='d) Full model vs. univariate model (test presences & background sites)', test='univar', var=c('T1', 'F1'), metric='cor', contrast='bg', strat=FALSE, miny=0, legend=NA, fg=fg, bg=bg, col=col2)
	
	plotImpVsScalar(dataFrame=dataFrame, x=x, xlab=xlab, main='e) Full model vs. permuted model (stratified background sites)', test='permute', var=c('T1', 'F1'), metric='cor', contrast='bg', strat=TRUE, miny=0, legend=NA, fg=fg, bg=bg, col=col2)
	plotImpVsScalar(dataFrame=dataFrame, x=x, xlab=xlab, main='f) Full model vs. univariate model (stratified background sites)', test='univar', var=c('T1', 'F1'), metric='cor', contrast='bg', strat=TRUE, miny=0, legend=NA, fg=fg, bg=bg, col=col2)

	if (suffix!='') title(toupper(suffix), cex.main=0.6, outer=T, line=-0.5)
	
	dev.off()
	
}

