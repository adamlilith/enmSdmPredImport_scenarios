#############################################
### function to create noise on landscape ###
#############################################
noisy <- function(landscape, geography) {

	## adds noise to one or more variables
	# landscape		landscape object
	# geography		geography object
	
	names <- names(landscape)
	
	# for each layer
	for (i in seq_along(geography)) {
	
		if (any(names(unlist(geography[[i]])) %in% 'noisy')) {
		
			if (as.logical(unlist(geography[[i]])[which(names(unlist(geography[[i]])) %in% 'noisy')])) {
			
				# proportion to swap
				p <- as.numeric(unlist(geography[[i]])[which(names(unlist(geography[[i]])) %in% 'noisep')])
			
				val <- c(as.matrix(subset(landscape, i)))
				cellNum <- seq_along(val)
				notNa <- which(!is.na(val))
				
				swap <- sample(notNa, round(p * length(notNa)))
				
				swapA <- swap[1:(floor( 0.5 * length(swap)))]
				swapB <- swap[((floor( 0.5 * length(swap))) + 1):(((floor( 0.5 * length(swap))) + 1) + length(swapA) - 1)]
				
				newvalA <- val[swapB]
				newvalB <- val[swapA]
				
				val[swapA] <- newvalA
				val[swapB] <- newvalB
			
				r <- raster(matrix(val, ncol=ncol(landscape), byrow=F))
				r <- setMinMax(r)
				projection(r) <- projection(landscape)
				
				landscape[[i]] <- r
			
			}
		
		} 
	
	}

	names(landscape) <- names
	
	landscape
	
}

