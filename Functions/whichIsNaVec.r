### function to return index of elements of sets of vectors in which at least one vector has an NA value ###
# NB: vectors don't have the be of same length, but results may be less useful if they are not!
whichIsNaVec <- function(...) {

	# ... set of vectors
	
	x <- list(...)
	isna <- numeric()
	for (i in seq_along(x)) isna <- c(isna, which(is.na(x[[i]])))
	isna <- sort(unique(isna))
	return(isna)
	
}

