### function to implement correlation test
corTest <- cmpfun(function(x, y) {

	# x, y			predictions made at test sites

	# cull to shortest vector
	out <- cull(x, y)
	x <- out$x
	y <- out$y
	
	out <- if (all(is.na(x)) | all(is.na(y))) {
		NA
	} else {
		cor(x, y, use='complete.obs')
	}
	
	out
	
})
