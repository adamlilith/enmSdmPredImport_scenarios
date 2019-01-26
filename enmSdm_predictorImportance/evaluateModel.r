#' Evaluate niche model
#'
#' This function evaluates models and is adapted from the \code{\link[dismo]{evaluate}} function in the \pkg{dismo} package. It is robust to predictions that are all \code{NA} or \code{NaN}
#' @param p Vector of predictions at presence sites.
#' @param a Vector of predictions at absence sites.
#' @return S4 object with model evaluation statistics.
#' @export

evaluateMod <- function (p, a, tr, ...) 
{
    p <- na.omit(p)
    a <- na.omit(a)
    np <- length(p)
    na <- length(a)
	
    # if (na == 0 | np == 0) {
        # stop("cannot evaluate a model without absence and presence data that are not NA")
    # }
    if (missing(tr)) {
        if (length(p) > 1000) {
            tr <- as.vector(quantile(p, 0:1000/1000))
        }
        else {
            tr <- p
        }
        if (length(a) > 1000) {
            tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
        }
        else {
            tr <- c(tr, a)
        }
        tr <- sort(unique(round(tr, 8)))
        tr <- c(tr - 1e-04, tr[length(tr)] + c(0, 1e-04))
    }
    else {
        tr <- sort(as.vector(tr))
    }
    N <- na + np
    xc <- new("ModelEvaluation")
    xc@presence = p
    xc@absence = a
    R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)

	auc <- as.numeric(if (is.na(R)) { NA } else { R/(na * np) })
    xc@auc <- auc

	cr <- try(cor.test(c(p, a), c(rep(1, length(p)), rep(0, length(a)))), 
        silent = TRUE)
    if (class(cr) != "try-error") {
        xc@cor <- cr$estimate
        xc@pcor <- cr$p.value
    }

	res <- matrix(ncol = 4, nrow = length(tr))
	colnames(res) <- c("tp", "fp", "fn", "tn")
	xc@t <- tr

	if (np > 0 & na > 0) {
		
		for (i in seq_along(tr)) {
			res[i, 1] <- length(p[p >= tr[i]])
			res[i, 2] <- length(a[a >= tr[i]])
			res[i, 3] <- length(p[p < tr[i]])
			res[i, 4] <- length(a[a < tr[i]])
		}

	}
		
	xc@confusion = res
	a = res[, 1]
	b = res[, 2]
	c = res[, 3]
	d = res[, 4]

	xc@np <- as.integer(np)
	xc@na <- as.integer(na)
	xc@prevalence = (a + c)/N
	xc@ODP = (b + d)/N
	xc@CCR = (a + d)/N
	xc@TPR = a/(a + c)
	xc@TNR = d/(b + d)
	xc@FPR = b/(b + d)
	xc@FNR = c/(a + c)
	xc@PPP = a/(a + b)
	xc@NPP = d/(c + d)
	xc@MCR = (b + c)/N
	xc@OR = (a * d)/(c * b)
	prA = (a + d)/N
	prY = (a + b)/N * (a + c)/N
	prN = (c + d)/N * (b + d)/N
	prE = prY + prN
	xc@kappa = (prA - prE)/(1 - prE)
    return(xc)
	
}


