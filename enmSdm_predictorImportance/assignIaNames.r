#' Assign names of two variables to lists
#' 
#' This is a helper function that assigns names of two variables to lists where each element represents values pertaining to a particular interaction.
#' @param x		List object.
#' @param land	Raster stack with named layers.
#' @value List object with named elements.
#' @export

assignIaNames <- function(x, land) {

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

	x

}

