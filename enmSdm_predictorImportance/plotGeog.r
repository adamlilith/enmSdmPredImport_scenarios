##############################################
### function to plot landscape and species ###
##############################################
plotGeog <- function(landscape, species, outDir, inModel=rep(T, nlayers(landscape)), name=NULL, bg, fg, legend=FALSE) {

	# landscape		raster stack
	# species		species raster
	# outDir			outDir
	# inModel		T/F vector indicating if each variable in landscape was included in model
	# name			character to suffix to filename
	# fg			color of foreground (text)
	# bg			color of background (canvas)
	# legend		TRUE/FALSE include legends

	# for print/BW
	# landcol <- c('darkgreen', 'white')
	# speciescol <- c('darkred', 'white')

	# for slides
	landcol <- c('white', 'dodgerblue4') # low/high
	speciescol <- c('gray20', 'springgreen3') # low/high
	
	if (nlayers(landscape)==2) {
	
		cex <- 2
	
		layout <- matrix(
			c(1, 1, 1, 1, 2, 2, 2, 2, 4,
			  1, 1, 1, 1, 2, 2, 2, 2, 4,
			  1, 1, 1, 1, 2, 2, 2, 2, 4,
			  1, 1, 1, 1, 2, 2, 2, 2, 4,
			  6, 6, 3, 3, 3, 3, 5, 7, 7,
			  6, 6, 3, 3, 3, 3, 5, 7, 7,
			  6, 6, 3, 3, 3, 3, 5, 7, 7,
			  6, 6, 3, 3, 3, 3, 5, 7, 7
			 ), nrow=8, byrow=T
		)
		
	} else if (nlayers(landscape)==3) {
	
		cex <- 2.1
	
		layout <- matrix(
			c(
				1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 5,
				1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 5,
				1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 5,
				1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 5,
				1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 5,
				3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 6,
				3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 6,
				3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 6,
				3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 6,
				3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 6
			 ), nrow=10, byrow=T
		)
	
	} else if (nlayers(landscape)==12) {
	
		cex <- 1.2
		
		layout <- matrix(
			c(
				1,  2,  3,  4, 13, 13, 13,
				5,  6,  7,  8, 13, 13, 13,
				9, 10, 11, 12, 13, 13, 13
			), nrow=3, byrow=T
		)
	
	}

	# setup plot device
	file <- if (is.null(name)) { paste0(outDir, '/landscape and species.png') } else { paste0(outDir, '/landscape and species - ', name, '.png') }
	
	png(file,
		width=if (nlayers(landscape)==2) { 2000 } else if (nlayers(landscape)==3) { 2200 } else if (nlayers(landscape)==12) { 2600 },
		height=if (nlayers(landscape)==2) { 2000 } else if (nlayers(landscape)==3) { 2000 } else if (nlayers(landscape)==12) { 1200 },
		res=200
	)

	### landscape
	
	col <- colorRampPalette(landcol)
	col <- col(100)
	
	par(bty='n', fg=fg, bg=bg, mai=rep(0.025, 4), oma=c(0, 0, 0, 0))
	
	for (i in 1:nlayers(landscape)) {

		par(fig=getFig(layout, i), new=(i!=1))
	
		rast <- subset(landscape, i)
	
		plot(
			x=rast,
			legend=FALSE,
			axes=FALSE,
			col=col,
			breaks=seq(minValue(rast), maxValue(rast), length.out=length(col)),
			maxpixels=500000,
			add=(i!=1)
		)
		
		par(fig=getFig(layout, i))
		text(x=0, y=1, labels=paste0(names(rast), ifelse(inModel[i], '-M', '-X')), cex=cex, xpd=NA, col=fg, xpd=NA, adj=0)
		
	}

	### species

	col <- colorRampPalette(speciescol)
	col <- col(100)
	
	par(fig=getFig(layout, i + 1), new=TRUE)
	
	plot(
		x=species,
		legend=FALSE,
		axes=FALSE,
		col=col,
		breaks=seq(0, 1, length.out=length(col)),
		maxpixels=500000,
		add=TRUE
	)
		
	par(fig=getFig(layout, i + 1))
	text(x=0, y=1, labels='Species', cex=cex, xpd=NA, col=fg, xpd=NA, adj=0)
	

	### legends
	
	if (legend) {
		
		leg <- matrix(seq(maxValue(rast), minValue(rast), length.out=100), nrow=100)
		leg <- rbind(matrix(rep(NA, 5), ncol=1), leg)
		leg <- rbind(leg, matrix(rep(NA, 5), ncol=1))
		leg <- cbind(matrix(rep(NA, 1 * nrow(leg)), ncol=1), leg)
		leg <- cbind(leg, matrix(rep(NA, 1 * nrow(leg)), ncol=1))
		leg <- raster(leg)
		
		# legend for landscape
		col <- colorRampPalette(landcol)
		col <- col(100)
		
		par(fig=getFig(layout, i + 2), new=TRUE)

		plot(
			x=leg,
			legend=FALSE,
			axes=FALSE,
			col=col,
			breaks=seq(minValue(rast), maxValue(rast), length.out=length(col)),
			maxpixels=500000,
			add=TRUE
		)

		par(fig=getFig(layout, i + 2), new=TRUE)
		text(x=0.5, y=1, xpd=NA, labels='high', col=fg, cex=cex * 0.8, adj=0.5)

		par(fig=getFig(layout, i + 2), new=TRUE)
		text(x=0.5, y=-0, xpd=NA, labels='low', col=fg, cex=cex * 0.8, adj=0.5)

		# legend for species
		col <- colorRampPalette(speciescol)
		col <- col(100)
		
		par(fig=getFig(layout, i + 3), new=TRUE)

		plot(
			x=leg,
			legend=FALSE,
			axes=FALSE,
			col=col,
			breaks=seq(minValue(rast), maxValue(rast), length.out=length(col)),
			maxpixels=500000,
			add=TRUE
		)

		par(fig=getFig(layout, i + 3), new=TRUE)
		text(x=0.5, y=1, xpd=NA, labels='Pr(occ)=1', col=fg, cex=cex * 0.8, adj=0.5)

		par(fig=getFig(layout, i + 3), new=TRUE)
		text(x=0.5, y=-0, xpd=NA, labels='Pr(occ)=0', col=fg, cex=cex * 0.8, adj=0.5)
		
	} # legends

	dev.off()
	
}


