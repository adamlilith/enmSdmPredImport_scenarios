### function to implement evaluate() and correlation test while ensuring no errors from NAs result ###
evalCorrTest <- function(predTestPresFull, predNonpresFull, predPres, predNonpres) {

	# predTestPresFull, predNonpresFull		predictions by full model at test sites
	# predPres, predNonpres						predictions by "test" model at same sites

	eval <- list()
	
	# remove any element with NA from test model's predictions
	isna <- whichIsNaVec(predPres, predNonpres)

	if (length(isna) > 0) {
		p <- predPres[-isna]
		a <- predNonpres[-isna]
	} else {
		p <- predPres
		a <- predNonpres
	}

	# evaluation
	eval$eval <- evaluateMod(p=as.vector(p), a=as.vector(a), tr=seq(0, 1, by=0.01))
	
	# correlation test
	isna <- whichIsNaVec(predTestPresFull, predNonpresFull, predPres, predNonpres)
	if (length(isna) > 0) {
		Xp <- predTestPresFull[-isna]
		Xa <- predNonpresFull[-isna]
		Yp <- predPres[-isna]
		Ya <- predNonpres[-isna]
	} else {
		Xp <- predTestPresFull
		Xa <- predNonpresFull
		Yp <- predPres
		Ya <- predNonpres
	}
	
	Xp <- cullToShortest(Xp, Xa, Yp, Ya)
	Xa <- cullToShortest(Xa, Xp, Yp, Ya)
	Yp <- cullToShortest(Yp, Xa, Xa, Ya)
	Ya <- cullToShortest(Ya, Xp, Xa, Yp)
	
	if (!is.na(var(c(Xp, Xa), na.rm=T)) && var(c(Xp, Xa), na.rm=T) < .Machine$double.eps) {
		Xp <- Xp + ifelse(runif(length(Xp)) > 0.5, 1, -1) * 2 * rnorm(length(Xp), 0, 2 * .Machine$double.eps)
		Xa <- Xa + ifelse(runif(length(Xa)) > 0.5, 1, -1) * 2 * rnorm(length(Xa), 0, 2 * .Machine$double.eps)
	}
	
	if (!is.na(var(c(Xp, Xa), na.rm=T)) && var(c(Yp, Ya), na.rm=T) < .Machine$double.eps) {
		Yp <- Yp + ifelse(runif(length(Yp)) > 0.5, 1, -1) * 2 * rnorm(length(Yp), 0, 2 * .Machine$double.eps)
		Ya <- Ya + ifelse(runif(length(Ya)) > 0.5, 1, -1) * 2 * rnorm(length(Ya), 0, 2 * .Machine$double.eps)
	}
	
	eval$cor <- cor(logit(c(Xp, Xa)), logit(c(Yp, Ya)))
	
	return(eval)
	
}

