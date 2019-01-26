######################################################################
### function to plot response vs user-specified scalar -- 3 panels ###
######################################################################
## NB this function is better than plotGrad() for 2-panel plots

plotVsScalar3Panel <- function(xVarName, models, resp, off, pch, col, variables, main, xlim, leg, legPos, legInset, legCex, xlab, ylab, filename, ...) {

	# Each panel represents a different variable (T1, T2, F1)
	# Can plot unlimited number of series per panel
	# Each panel can also have more than one SDM.
	
	# xVarName			name of x variable
	# models			names of SDMs defining each series. Must have same number of values as resp.
	# resp				response variables of each series. Must have same number of values as models.
	# off				x-axis offset for each series. Must have same number of values as models.
	# pch				List of pch for each series. Must have same number of values as models.
	# col				List of colors for each series. Must have same number of values as models.
	# variables			T1, T2, F1, etc. List only one or two or three.
	# main				plot titles. List three.
	# xlim				x-axis limits
	# leg				Names of items in legend. Must have same number of values as models.
	# legPos			2-element character list of position words for legend
	# legInset			Legend inset value
	# legCex			cex for legend
	# xlab, ylab		x- and y-axis labels
	# filename			Filename sans extension

	# example
	# plotVsGrad2Panel(
		# xVarName=xVarName,
		# models=c('omniscient', 'maxent', 'maxent'),
		# resp=c('corAbsFullVsPerm_perm', 'corBgFullVsPermStrat_perm', 'maxentContrib'),
		# off=c(-0.01, 0, 0.01),
		# pch=c(16, 17, 18),
		# col=c(col1, col2, col3),
		# leg=c('OMNISCIENT', 'MAXENT (PERMUTE)', 'MAXENT (%gain)'),
		# variables=c('T1', 'T2', 'F1'),
		# main=c('TRUE (weaker)', 'TRUE (stronger)', 'FALSE'),
		# xlim=c(-1, 1),
		# legPos=c('bottomleft', 'topright', 'topright'),
		# legInset=0.1,
		# legCex=0.8,
		# xlab='Rho',
		# ylab='Correlation',
		# filename='File name with no directory or extension'
	# )
	
	x <- sort(unique(thisMaster[ , xVarName]))

	png(paste0(resultsDir, directory, '/', filename, '.png'), width=2700, height=1000, res=300)

		par(mfrow=c(1, 3), fg=fg, bg=bg, col.main='white', col.axis='white', col.lab='white', mgp=0.6 * c(3, 1, 0), cex.main=1.6, cex.lab=1.4, cex.axis=0.9, mar=0.7 * c(5, 4, 4, 2) + 0.1)

		# for each landscape variable
		for (countVariable in seq_along(variables)) {

			plot(x=c(0, 0), y=c(0, 1), xlim=xlim, type='n', bty='n', axes=TRUE, xlab=xlab, ylab=ylab, main=main[countVariable])
			axis(2)

			# for each SDM
			for (countModel in seq_along(models)) {

				respVar <- paste0(resp[countModel], variables[countVariable])

				plotData <- thisMaster[which(thisMaster$algorithm==models[countModel]), ]
				plotDataAgg <- thisAgg[which(thisAgg$algorithm==models[countModel]), ]

				# error bars
				for (i in seq_along(x)) {
					lines(
						x=rep(x[i], 2) + off[countModel],
						y=c(quantile(plotData[plotData[ , xVarName]==x[i], respVar], 0.025, na.rm=T), quantile(plotData[plotData[ , xVarName]==x[i], respVar], 0.975, na.rm=T)),
						col=col[countModel], lwd=1.4, lty='dashed'
					)
				}

				# points
				points(
					x=x + off[countModel],
					y=plotDataAgg[match(plotDataAgg[ , xVarName], x), respVar],
					type='b',
					col=col[countModel],
					bg=col[countModel],
					pch=pch[countModel],
					cex=1.8,
					lwd=1
				)
				
			}

			legend(legPos[countVariable], inset=legInset, legend=leg, col=col, pch=pch, lwd=1, cex=legCex, bty='n', pt.cex=1.2)
			
		}

	dev.off()

}

