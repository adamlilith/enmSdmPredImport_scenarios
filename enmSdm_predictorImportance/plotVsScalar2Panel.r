######################################################################
### function to plot response vs user-specified scalar -- 2 panels ###
######################################################################
## NB this function is better than plotGrad() for 2-panel plots

plotVsScalar2Panel <- function(
	results,
	resultsAgg,
	xVarName,
	models,
	resp,
	respVarSpecific,
	invert,
	off,
	pch,
	col1,
	col2,
	colError,
	colLine,
	ltyError=rep('solid', length(colError)),
	variables,
	panels,
	xlim,
	ylim,
	xlog=FALSE,
	leg,
	legPos,
	legInset,
	legCex,
	legNcol=1,
	xlab,
	ylab,
	cexAxis=0.45,
	filename,
	...
) {

	# Each panel represents a different variable (T1, T2)
	# Can plot unlimited number of series per panel
	# Each panel can also have more than one SDM.

	
	# results			Data frame with results... must have one column named in xVarName and one column named in resp
	# resultsAgg		Data frame with results aggregated by levels of xVarName... must have one column named in xVarName and one column named in resp
	# xVarName			Name of x variable
	# models			Names of SDMs defining each series. Must have same number of values as resp.
	# resp				Response variables of each series. Must have same number of values as models.
	# respVarSpecific	Logical, indicates if name of response variable shoudl be appended to values in resp. One per value in resp.
	# invert			Logical, if TRUE then subtract importance metric from 1 before plotting
	# off				X-axis offset for each series. Must have same number of values as models. When xlog is FALSE this is the absolute difference between the normal x position and the new position.  When xlog is TRUE then this is the fractional distance.
	# pch				List of pch for each series. Must have same number of values as models.
	# col1, col2		List of colors for each series. col1 is the "primary" color (color inside points).  col2 is the "secondary" color (point outlines).  Each must have same number of values as models.
	# colError			Error bar color.  Must have same number of values as models.
	# colLine			Trend line color.  Must have same number of values as models.
	# ltyError			lty of error bars. Must have same number of values as models.
	# colBg				List of colors for point background. Must have same number of values as models.
	# variables			T1, T2, F1, etc. List only one or two.
	# panels			Panel titles. List two.
	# xlim, ylim		x- and y-axis axis limits
	# xlog				TRUE ==> log x axis
	# leg				Names of items in legend. Must have same number of values as models.
	# legPos			2-element character list of position words for legend
	# legInset			Legend inset value
	# legCex			cex for legend
	# legNcol			number of legend columns
	# xlab, ylab		x- and y-axis labels
	# cexAxis			axis number size
	# filename			Filename with path but sans extension

	# example
	# plotVsGrad2Panel(
		# results=dataFrame,
		# resultsAgg=dataFrameAgg,
		# xVarName=xVarName,
		# models=c('omniscient', 'maxent', 'maxent'),
		# resp=c('corAbsFullVsPerm_perm', 'corBgFullVsPermStrat_perm', 'maxentContrib'),
		# off=c(-0.01, 0, 0.01),
		# pch=c(16, 17, 18),
		# col1=c(col1, col2, col3),
		# col2=c('black', 'black', 'black'),
		# leg=c('OMNISCIENT', 'MAXENT (PERMUTE)', 'MAXENT (%gain)'),
		# variables=c('T2', 'F1'),
		# panels=c('a) TRUE', 'b) FALSE'),
		# xlim=c(-1, 1),
		# ylim=c(-1, 1),
		# legPos=c('bottomleft', 'topright'),
		# legInset=0.1,
		# legCex=0.8,
		# xlab='Rho',
		# ylab='Correlation',
		# filename='File name with no directory or extension'
	# )

	# get list of x-values
	x <- sort(unique(results[ , xVarName]))
	xOrig <- xLabs <- x
	print(xLabs)
	if (all(xLabs < 1)) xLabs <- round(xLabs, 2)
	print(xLabs)
	if (xlog) x <- log10(x)
	
	png(paste0(filename, '.png'), width=1800, height=1000, res=300)

		par(mfrow=c(1, 2), fg=fg, bg=bg, col.main=fg, col.axis=fg, col.lab=fg, mgp=0.4 * c(3, 1, 0), cex.main=0.8, cex.lab=0.6, cex.axis=cexAxis, mar=0.5 * c(5, 4, 4, 2) + 0.1)

		# for each landscape variable
		for (variable in variables) {

			plot(x=c(1, 1), y=c(1, 1), xlim=xlim, ylim=ylim, type='n', bty='n', axes=TRUE, xlab=xlab, ylab=ylab, xaxt='n')
			axis(1, at=x, labels=xLabs, ...)

			# for each SDM
			for (countModel in seq_along(models)) {

			say(models[countModel])
			
				# name of response variable
				respVar <- if (respVarSpecific[countModel]) {
					paste0(resp[countModel], variable)
				} else {
					resp[countModel]
				}
				
				plotData <- results[which(results$algorithm==models[countModel]), ]
				plotDataAgg <- resultsAgg[which(resultsAgg$algorithm==models[countModel]), ]
				
				# # beanplot version
				# thisSdmResults <- data.frame(
					# by=plotData[ , xVarName],
					# response=plotData[ , respVar]
				# )
				
				# at <- sort(unique(resultsAgg[ , xVarName])) + off[countModel]

				# beanplot(
					# response ~ by,
					# data=thisSdmResults,
					# what=c(FALSE, TRUE, FALSE, FALSE),
					# col=list(c(col1[countModel], col1[countModel], col1[countModel], col2)),
					# beanlines='median',
					# xaxp=c(0.1, 0.9, 7),
					# log='auto',
					# tick=FALSE,
					# bw='nrd0',
					# at=at,
					# add=ifelse(countModel==1, FALSE, TRUE),
					# cutmin=quantile(thisSdmResults$response, 0.025),
					# cutmax=quantile(thisSdmResults$response, 0.975),
					# names=NA,
					# beanlinewd=1,
					# maxstripline=0.01 * (range(at)[2] - range(at)[1]),
					# boxwex=0.5 * (range(at)[2] - range(at)[1]),
					# xlim=xlim,
					# ylim=c(-1, 1),
					# maxwidth=0.1,
					# main=ifelse(variable==variables[1], main[1], main[2])
				# )
				
				# if (countModel==1) {
				
					# axis(
						# side=1,
						# at=pretty(thisSdmResults$by),
						# labels=pretty(thisSdmResults$by)
					# )
					
				# }
				
				### point-and-line version
				
				# error bars
				for (i in seq_along(x)) {

					y <- c(quantile(plotData[plotData[ , xVarName]==xOrig[i], respVar], 0.025, na.rm=T), quantile(plotData[plotData[ , xVarName]==xOrig[i], respVar], 0.975, na.rm=T))
					if (invert) y <- 1 - y

					thisX <- if (xlog) {
						x[i] + rep(x[i], 2) * off[countModel]
					} else {
						rep(x[i], 2) + off[countModel]
					}
					
					lines(
						x=thisX,
						y=y,
						col=colError[countModel],
						lwd=1,
						xpd=NA,
						lty=ltyError[countModel]
					)
					
				}

				# lines
				y <- plotDataAgg[match(plotDataAgg[ , xVarName], xOrig), respVar]
				if (invert) y <- 1 - y
				
				thisX <- if (xlog) {
					x + x * off[countModel]
				} else {
					x + off[countModel]
				}
					
				lines(
					x=thisX,
					y=y,
					col=colLine[countModel],
					lwd=1,
					xpd=NA
				)
				
				# points
				points(
					x=thisX,
					y=y,
					col=ifelse(pch[countModel] >= 21, col2[countModel], col1[countModel]),
					bg=ifelse(pch[countModel] >= 21, col1[countModel], NA),
					pch=pch[countModel],
					cex=1.2,
					lwd=1,
					xpd=NA
				)
				
			}

			legend(
				ifelse(variable==variables[1], legPos[1], legPos[2]),
				inset=legInset,
				legend=leg,
				col=col2,
				pt.bg=col1,
				pch=pch,
				lwd=1,
				cex=legCex,
				bty='n',
				pt.cex=0.9,
				ncol=legNcol
			)
			
			# panel labels
			coords <- par('usr')
			xTitle <- coords[1] - (0.2 * (coords[2] - coords[1]))
				
			text(x=xTitle, y=coords[4] + 0.075 * (coords[4] - coords[3]), labels=ifelse(variable=='T1', panels[1], panels[2]), xpd=NA, pos=4, cex=0.7)
			
		}

	dev.off()

}

