######################################################################################################################################
### function to assign names of two variables to lists where each element represents values pertaining to a particular interaction ###
######################################################################################################################################
assignIaNames <- function(x, land=landscape) {

	# x			list object
	# land		raster stack

	# assign names of variables to attributes of each element
	count <- 0
	for (countVar1 in 1:(nlayers(land) - 1)) {
		for (countVar2 in (countVar1 + 1):nlayers(land)) {
			count <- count + 1
			var1 <- names(land)[countVar1]
			var2 <- names(land)[countVar2]
			if (class(x)=='list') { attr(x[[count]], 'variables') <- paste0(var1, 'x', var2) } else
				if (class(x)=='numeric') { names(x)[count] <- paste0(var1, 'x', var2) }
		}
	}

	return(x)

}

