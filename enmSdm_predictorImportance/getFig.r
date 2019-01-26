###################################################
### function to return values for par(fig=xxxx) ###
###################################################

getFig <- function(layout, n) {

	# legend	matrix
	# n			number of graph to plot (corresponds to value(s) in layout

	## get start/and column and row for this graph
	
	# convert layout to binary
	layout <- layout==n
	
	# get min/max column/row
	minCol <- min(which(colSums(layout) > 0))
	maxCol <- max(which(colSums(layout) > 0))
	minRow <- min(which(rowSums(layout) > 0))
	maxRow <- max(which(rowSums(layout) > 0))

	## calculate fig values
	fig <- c((minCol - 1) / ncol(layout), maxCol / ncol(layout), 1 - maxRow / nrow(layout), 1 - (minRow - 1) / nrow(layout))
	
	return(fig)

}


