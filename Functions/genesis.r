#' Generate a raster stack representing variables on a landscape.
#' This function generates a raster stack in which each layer represents a variable. Each variable has one of several spatial patterns: linear, uniform, random, step, hinge, Gaussian, or sine. Random noise can also be added to any one of these patterns, and the layer pattern can be split in half (e.g., increasing in the "east" side and decreasing in the "west" side). The landscape can be circular or square. Layers can be rotated around their center to have different orientations relative to other rasters.
#' @param geography List object composed of lists, with one list per layer on the landscape. The name of the sublists correspond to the names of the layers to be generated. Each sublist has some of the following components (some are required, some optional):
#' @details text describing parameter inputs in more detail.
#' \itemize{
	#' \item \code{type} Character, specifies the spatial pattern of the variable's values. Depending on the type, different subsequent items in the list can be used to specify the parameters of the pattern:
	#' \itemize{
		#' \item \code{uniform}: Spatially uniform with all values equal to 1.
		#' \item \code{random}: Uniformly randomly drawn values across the range specified by:
		#' \itemize{
			#' \item \code{min} and \code{max}: Numeric, minimum and maximum value.
		#' }
		#' \item \code{linear}: Variable changes linearly across the landscape with these parameters:
		#' \itemize{
			#' \item \code{min} and \code{max}: Numeric, minimum and maximum value.
		#' }
		#' \item \code{step}: A step function pattern with these parameters:
		#' \itemize{
			#' \item \code{min} and \code{max}: Numeric, ,inimum and maximum value below and above the step.
			#' \item \code{at} Numeric, location of the step specified such that the bottom ("south") edge of the raster is -1 and the top ("north") edge is 1.
		#' }
		#' \item \code{gaussian}: A spatially Gaussian distribution with these parameters:
		#' \itemize{
			#' \item \code{center1} and \code{center2}: Numeric, position on (or off) the landscape where the center of the distribution lies.
			#' \item \code{sd1} and \code{sd2}: Numeric, standard deviation in the x- and y-directions.
			#' \item \code{rho} Numeric, rotation (interaction between x and y). 0 creates no rotation with values closer to 1 collapsing into a univariate distribution in the x-direction and closer to -1 in the y-direction.
		#' }
		#' \item \code{sin} A sine wave with these parameters:
		#' \itemize{
			#' \item \code{freq} Numeric, frequency (assumes going from one edge of the landscape to the other is equal to one wavelength).
			#' \item \code{offset} Numeric, value to add/subtract from the position the the sine wave (when this is 0 then the value of \code{sine(0) = 1} is positioned at the edge of the landscape).
			#' \item \code{min} and \code{max} Numeric, amplitude of the sine wave.
		#' }
		#' \item In addition to the \code{type}-specific parameters, any \code{type} can have also have these parameters:
		#' \itemize{
			#' \item \code{rot} Numeric, degrees by which to rotate the raster relative to "north". This is useful for manipulating the correlation between layers.
			#' \item \code{randOrient} Logical, if \code{TRUE} then then one of the following occurs, each with equal probability: 1) keep layer as-is; 2) reflect in the left-right ("east-west") direction; 3) reflect in the up-down ("north-south") direction; or 4) reflect in the "east-west" then the "north-south" directions.
			#' \item \code{noisy} Logical, if \code{TRUE} then add random noise by randomly swapping values across cells after the pattern specified by \code{type} has been created. Note that swapping ensures the original frequency distribution of the values of the variable is retained.
			#' \item \code{split} Logical, if \code{TRUE} then before any rotation swap values between the upper left ("northwest") corner and the lower right ("southeast") corners of the raster.
			#'
		#' }
	#' }
#' }
#' @param size Integer, number of rows/columns in each landscape raster.
#' @param rescale Either \code{agg} in which case the \code{link[raster]{aggregate}} function is called after the landscape is created, \emph{or} \code{disagg} in which case the \code{link[raster]{disaggregate}} function is called after the landscape is created, \emph{or} \code{NULL} (default) in which case no dis/aggregation is performed. Arguments to can be passed to \code{aggregate} or \code{disaggregate} using \code{...}.
#' @param circle Logical, if \code{TRUE} then the raster stack is cropped to a circle with values outside the circle left as \code{NA}. If \code{FALSE} (default), then the stack is left as a square.
#' @returns A raster stack.
#' @examples


##################################################################
### function to create landscape of raster stack of predictors ###
##################################################################
genesis <- function(
	geography,
	size=1001,
	rescale=NULL,
	circle=TRUE,
	verbose=FALSE,
	...
) {

	# position rasters... needed if generating from scratch
	x <- matrix(rep(seq(-1, 1, length.out=size), size), nrow=size, byrow=TRUE)
	y <- matrix(rep(seq(1, -1, length.out=size), each=size), nrow=size, byrow=TRUE)

	# position rasters... needed if generating from scratch
	dist <- sqrt(x^2 + y^2)

	# position rasters... needed if generating from scratch
	template <- ifelse(dist <= 1, 1, NA)
	template <- raster(template)

	# generate each raster or obtain from disk
	# for (i in seq_along(geography)) {
	for (i in seq_along(geography)) {

		if (verbose) say('Creating landscape layer ', i)
	
		if (any(names(unlist(geography[[i]])) %in% 'pregen')) {
			
			# get pre-generated raster
			if (geography[[i]]$pregen) {
				usepregen <- TRUE
			} else {
				usepregen <- FALSE
			}
			
		} else {
			usepregen <- FALSE
		}

		### use pre-generated raster
		if (usepregen) {

			mat <- raster(
				paste0(workDir, '/Simulated Landscape Rasters/',
					geography[[i]]$type,
					'From',
					sub(pattern='[-]', replacement='Neg', x=as.character(min)),
					'To',
					sub(pattern='[-]', replacement='Neg', x=as.character(max)),
					'Rotation',
					ifelse(any(names(unlist(geography[[i]])) %in% 'rot'), sub(as.character(geography[[i]]$rot), pattern='[.]', replacement='pt'), 0),
					ifelse(any(names(unlist(geography[[i]])) %in% 'split'), '_split', ''),
					'.tif'
				)
			)
		
		### generate raster from scratch
		} else {
				
			# uniform values
			mat <- if (geography[[i]]$type=='uniform') {
			
				matrix(rep(1, size^2), nrow=size)
		
			# random values
			} else if (geography[[i]]$type=='random') {
			
				matrix(runif(n=size^2, min=geography[[i]]$min, max=geography[[i]]$max), nrow=size)

			# linear
			} else if (geography[[i]]$type=='linear') {
		
				matrix(rep(seq(geography[[i]]$max, geography[[i]]$min, length.out=size), each=size), nrow=size, byrow=T)
			
			# step
			} else if (geography[[i]]$type=='step') {
			
				(y >= geography[[i]]$at) * geography[[i]]$max + (y < geography[[i]]$at) * geography[[i]]$min
			
			# hinge
			} else if (geography[[i]]$type=='hinge') {
			
				(y >= geography[[i]]$to) * geography[[i]]$max +
				(y >= geography[[i]]$from & y < geography[[i]]$to) * (((geography[[i]]$max - geography[[i]]$min) / (geography[[i]]$to - geography[[i]]$from)) * (y - geography[[i]]$from) + geography[[i]]$min) +
				(y < geography[[i]]$from) * geography[[i]]$min
			
			# gaussian
			} else if (geography[[i]]$type=='gaussian') {

				gaussian(x1=x, x2=y, mu1=geography[[i]]$center1, mu2=geography[[i]]$center2, sigma1=geography[[i]]$sd1, sigma2=geography[[i]]$sd2, rho=geography[[i]]$rho)
			
			# sine
			} else if (geography[[i]]$type=='sin') {
			
				(geography[[i]]$max  - geography[[i]]$min) * ((sin(geography[[i]]$freq * pi * y - geography[[i]]$offset) + 1) / 2) + geography[[i]]$min
			
			}
			
			# split (swap) values of raster from top to bottom on one half of raster
			if (any(names(unlist(geography[[i]])) %in% 'split')) {

				if (geography[[i]]$split) mat[1:size, 1:round( 0.5 * ncol(mat))] <- mat[size:1, round( 0.5 * ncol(mat)):1]
				
			}
		
			# rotate raster
			if (any(names(geography[[i]])=='rot')) if (!is.na(geography[[i]]$rot)) mat <- rotMatrix(x=mat, rot=geography[[i]]$rot)
		
			# random orientation for rasters of "linear-ish" type
			if (any(names(geography[[i]])=='randOrient')) if (!is.na(geography[[i]]$randOrient)) {
				# orient <- sample(0:3, 1)
				# if (orient==1) mat <- rotMatrix(mat, 90)
				# if (orient==2) mat <- rotateMatrix(mat, 180)
				# if (orient==3) mat <- rotateMatrix(mat, 270)
				mat <- rotateMat(mat, runif(1, 0, 360 - eps()))
			}
		
			if (class(mat)!='RasterLayer') mat <- raster(mat)
			
			# trim values to circle
			if (circle) mat <- mat * template
			
			
		}

		# stack
		landscape <- if (!exists('landscape', inherits=FALSE)) { stack(mat) } else { stack(landscape, mat) }

	} # next raster

	# add noise to any raster that needs it
	landscape <- noisy(landscape, geography)

	# rescale
	if (!is.null(rescale)) {

		landscape <- if (rescale == 'agg') {
			raster::aggregate(landscape, ...)
		} else if (rescale == 'disagg') {
			raster::disaggregate(landscape, ...)
		}
	}
	
	projection(landscape) <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs' # Mollweide (equal area)
	landscape <- setMinMax(landscape)
	
	# name landscape
	names(landscape) <- names(geography)
	
	landscape
	
}
