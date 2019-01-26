##################################################################
### function to predict omniscient, Maxent, BRT, and GAM model ###
##################################################################
predictModel <- function(model, data, ...) {

	# model		model object
	# data		data to which to predict
	# ...		arguments to pass to predict() function
	
	# if ('gbm' %in% class(model) & !any('DUMMY' %in% names(data))) data$DUMMY <- rep(1, nrow(data))

	# randomForest: add factor levels to factors missing a level compared to training data
	if ('randomForest' %in% class(model)) {
	
		for (thisPred in names(model$forest$xlevels)) {
	
			if (class(data[ , thisPred]) == 'factor') {
			
				trainingLevels <- unlist(model$forest$xlevels[match(thisPred, names(model$forest$xlevels))])
				dataLevels <- levels(data[ , thisPred])
				
				missingLevels <- trainingLevels[!(trainingLevels %in% dataLevels)]
				
				if (length(missingLevels) > 0) levels(data[ , thisPred]) <- c(levels(data[ , thisPred]), missingLevels)
				
			}
			
		}
		
	}

	########################
	### omniscient model ###
	########################
	pred <- if (nrow(data) == 0 | 'logical' %in% class(model)) {

		rep(NA, nrow(data))
	
	} else if ('function' %in% class(model)) {
	
		### full model
		if (attr(model, 'modelType')=='full') {
			
			model(
				x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
				x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
				...
			)
			
		### reduced model
		} else if (attr(model, 'modelType')=='reduced') {
		
			if (grepl(attr(model, 'reducedSans'), pattern='T1')) {
				
				model(
					x1=rep(0, nrow(data)),
					x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
					...
				)
				
			} else if (grepl(attr(model, 'reducedSans'), pattern='T2')) {
			
				model(
					x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
					x2=rep(0, nrow(data)),
					...
				)
			
			} else {
			
				model(
					x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
					x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
					...
				)
				
			}
		
		### univariate model
		} else if (attr(model, 'modelType')=='univariate') {
		
			if (grepl(attr(model, 'univarWith'), pattern='T1')) {
				
				model(
					x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
					x2=rep(0, nrow(data)),
					...
				)
				
			} else if (grepl(attr(model, 'univarWith'), pattern='T2')) {

				model(
					x1=rep(0, nrow(data)),
					x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
					...
				)
				
			} else {
			
				model(
					x1=rep(0, nrow(data)),
					x2=rep(0, nrow(data)),
					...
				)
				
			}
		
		}
	
	#####################
	### Maxent models ###
	#####################
	} else if ('MaxEnt' %in% class(model)) {
	
		predictMaxEnt(x=model,
			data=data,
			type = 'cloglog',
			...
		)
	
	#######################
	### standard models ###
	#######################
	} else {
	
		predict(
			object=model,
			x=data,
			newdata=data,
			n.trees=model$gbm.call$best.trees,
			type=ifelse('randomForest' %in% class(model), 'prob', 'response'),
			response=TRUE,
			na.rm=TRUE,
			...
		)
	
	}

	if ('randomForest' %in% class(model)) pred <- pred[ , 2]

	pred
	
}

