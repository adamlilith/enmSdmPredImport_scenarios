#' Robust correlation test
#'
#' Correlation test robust to \code{NA}s in the data.
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @return Pearson's correlation coefficient.
#' @export

corTest <- cmpfun(function(x, y) {

	# cull to shortest vector
	out <- omnibus::cull(x, y)
	x <- out$x
	y <- out$y
	
	out <- if (all(is.na(x)) | all(is.na(y))) {
		NA
	} else {
		cor(x, y, use='complete.obs')
	}
	
	out
	
})
