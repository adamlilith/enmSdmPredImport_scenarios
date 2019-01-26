## SDM PREDICTOR INFERENCE - FIGURES
## Adam B. Smith | Missouri Botanical Garden | adam.smith@mobot.org

memory.limit(memory.limit() * 2^30)
rm(list=ls())
options(keep.source=FALSE) # manage memory
gc()

# # source('C:/ecology/Drive/Research/ENMs - Predictor Inference/Scripts NEW/Figures.r')
# setwd('C:/ecology/Drive/Research/ENMs - Predictor Inference')

# source('H:/Global Change Program/Research/ENMs - Predictor Inference/Scripts NEW/Figures.r')
setwd('H:/Global Change Program/Research/ENMs - Predictor Inference')

library(compiler); library(sp); library(rgdal); library(raster); library(rJava); options(java.parameters='-Xmx1g' ); library(dismo); library(gbm); library(mgcv); library(MuMIn); library(scales); library(beanplot); library(hier.part); library(stringr); library(omnibus); library(enmSdm)

files <- listFiles('./Scripts NEW/Functions')
for (thisFile in files) source(thisFile)

print('')
print(date())

# dirCreate('C:/ecology/Drive/Research/SDMs - Predictor Inference/Figures')
# figDir <- 'C:/ecology/Drive/Research/SDMs - Predictor Inference/Figures/'

################
### CONTENTS ###
################

### SETTINGS ###
### [tune brt and rf for logistic responses] ###
### [simple] ###
### [sample size] ###
### [prevalence] ###
### [extent] ###
### [resolution] ###
### [tune brt for bivariate responses] ###
### [bivariate -- collate evaluations] ###
### [bivariate -- NO landscape correlation and NO niche covariance] ###

################
### SETTINGS ###
################

	bg <- 'white'
	fg <- 'black'

	# colors from colorbrewer.org for qualitative palette
	# lightOmni <- '#fdae6b' # orangish
	lightOmni <- 'gray'
	lightBrt <- '#80b1d3' # bluish
	lightGam <- '#8dd3c7' # greenish
	lightMaxent <- '#fb8072' # reddish
	lights <- c(lightOmni, lightBrt, lightGam, lightMaxent)

	# darkOmni <- '#d94801' # orangish
	darkOmni <- 'black' # orangish
	darkBrt <- '#377eb8' # bluish
	darkGam <- '#4daf4a' # greenish
	darkMaxent <- '#e41a1c' # reddish
	darks <- c(darkOmni, darkBrt, darkGam, darkMaxent)

	colMulti <- 'white' #
	colLightT1 <- '#a1d99b' # greenish
	colLightF1 <- '#fc9272' # reddish
	
	colDarkT1 <- '#4daf4a' # greenish
	colDarkT2 <- '#984ea3' # purplish
	colDarkF1 <- '#e41a1c' # reddish
	
	allAlgos <- c('omniscient', 'brt', 'gam', 'maxent')
	niceAlgos <- c('Omniscient', 'BRT', 'GAM', 'Maxent')
	allAlgosSansOmniscient <- c('brt', 'gam', 'maxent')
	algoPch <- c(21, 22, 23, 25)

	# allAlgos <- c('omniscient', 'maxent')
	# niceAlgos <- c('Omniscient', 'Maxent')
	# allAlgosSansOmniscient <- c('maxent')
	# algoPch <- c(21, 25)
	
#################
### FUNCTIONS ###
#################

	orderAlgos <- function(x) {
	
		# input is a list of algorithm names (e.g., 'omnicient', 'brt', 'gam', 'maxent')
		# output is a vector of indices corresponding to input that will re-order such that omniscient is first, brt second, gam third, and maxent fourth
		
		x <- c(which(x == 'omniscient'), which(x == 'brt'), which(x == 'gam'), which(x == 'maxent'))
		x
		
	}
	
# say('#########################################')
# say('### [tune brt for logistic responses] ###')
# say('#########################################')

	# thisOutDir <- './Results/tune brt for logistic responses'

	# say('Loading tuning parameters for each of 100 models...')
	
	# lr <- tc <- ntrees <- rep(NA, 100)
	
	# for (i in 1:100) {
		
		# say(i, post=ifelse(i %% 20 == 0, 1, 0))
		
		# load(paste0(thisOutDir, '/multivariate brt/brt ', prefix(i, 3), '.Rdata'))
		# lr[i] <- model$gbm.call$learning.rate
		# tc[i] <- model$gbm.call$tree.complexity
		# ntrees[i] <- model$gbm.call$best.trees
		
	# }		

	# load(paste0(thisOutDir, '/evaluations/Evaluations for multivariate BRT.Rdata'))

	# say('AUC vs pres/abs | mean: ', mean(perform$aucPresAbsMulti), ' | 95% CI: ', quantile(perform$aucPresAbsMulti, 0.025), '-', quantile(perform$aucPresAbsMulti, 0.975))
	# say('CBI | mean: ', mean(perform$cbiMulti), ' | 95% CI: ', quantile(perform$cbiMulti, 0.025), '-', quantile(perform$cbiMulti, 0.975))
	
	# par(mfrow=c(1, 2))
	# plot(ntrees, perform$cbiMulti)
	# points(ntrees, perform$cbiMulti, col=tc, pch=16)
	# legend('bottomright', legend=sort(unique(tc)), col=sort(unique(tc)), pch=16)

	# lrs <- sort(unique(lr))
	# plot(ntrees, perform$cbiMulti)
	# for (i in seq_along(lrs)) points(ntrees[lr == lrs[i]], perform$cbiMulti[lr == lrs[i]], col=i, pch=16)
	# legend('bottomright', legend=lrs, col=seq_along(lrs), pch=16)

	# say('Based on this choosing lr = 0.001, tc = 2, ntrees = 4000 for logistic responses.')
	
# say('################')
# say('### [simple] ###')
# say('################')

	# thisOutDir <- './Results/simple'

	# ### load and collate data

	# results <- loadEvals(thisOutDir)
	
	# # bean plot--truncated at lower 2.5% and upper 2.5%, center bar represents median
	# ### CBI - multivariate vs permuted
	# ##################################
	
		# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=1100, height=1100, res=300)
		
			# main <- 'CBI - Multivariate vs Permuted'
			
			# mainVar <- 'cbiMulti'
			# testVar1 <- 'cbiMulti_permT1'
			# testVar2 <- 'cbiMulti_permF1'
			
			# mainName <- 'unpermuted'
			# testName1 <- 'permuted TRUE'
			# testName2 <- 'permuted FALSE'
			
			# minVal <- -0.75
			# maxVal <- 1
			# ylab <- 'CBI\n\U2190 more important    less important\U2192'
			
			# par(mgp=c(1, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
			# lines(c(0, 5), c(0, 0), col='gray80', lwd=1)
				
			# for (i in seq_along(allAlgos)) {
			
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]

				# data <- results[results$algo == thisAlgo, mainVar]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(colMulti, colMulti, colMulti, fg), border='black', beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i, maxwidth=0.5, overallline='median')
				
				# data <- results[results$algo == thisAlgo, testVar1]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), colDarkT1), border=colDarkT1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i - 0.19, maxwidth=0.5, overallline='median')
				
				# data <- results[results$algo == thisAlgo, testVar2]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), colDarkF1), border=colDarkF1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i + 0.19, maxwidth=0.5, overallline='median')
				
			# }
			
			# legend('bottom', inset=-0.3, xpd=NA, fill=c(colMulti, colLightT1, colLightF1), legend=c(mainName, testName1, testName2), ncol=3, cex=0.5)
		
		# dev.off()
		
	# ### CBI - multivariate vs univariate
	# ####################################

		# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Univariate.png'), width=1100, height=1100, res=300)
		
			# main <- 'CBI - Multivariate vs Univariate'
			
			# mainVar <- 'cbiMulti'
			# testVar1 <- 'cbiUni_onlyT1'
			# testVar2 <- 'cbiUni_onlyF1'
			
			# mainName <- 'multivariate'
			# testName1 <- 'univariate TRUE'
			# testName2 <- 'univariate FALSE'
			
			# minVal <- -0.8
			# maxVal <- 1
			# ylab <- 'CBI\n\U2190 less important    more important\U2192'
			
			# par(mgp=c(1, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
			# lines(c(0, 5), c(0, 0), col='gray80', lwd=1)
				
			# for (i in seq_along(allAlgos)) {
			
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]

				# data <- results[results$algo == thisAlgo, mainVar]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(colMulti, colMulti, colMulti, fg), beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i, maxwidth=0.5, overallline='median')

				# data <- results[results$algo == thisAlgo, testVar1]
				# successes <- sum(!is.na(data))
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), colDarkT1), border=colDarkT1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i - 0.19, maxwidth=0.5, overallline='median')
				# text(i - 0.19, minVal - 0.1 * (maxVal - minVal), adj=c(0.5, 0.5), labels=paste0('n = ', successes), col=colDarkT1, cex=0.37, xpd=NA)
				
				# data <- results[results$algo == thisAlgo, testVar2]
				# successes <- sum(!is.na(data))
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), colDarkF1), border=colDarkF1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i + 0.19, maxwidth=0.5, overallline='median')
				# text(i + 0.19, minVal - 0.1 * (maxVal - minVal), adj=c(0.5, 0.5), labels=paste0('n = ', successes), col=colDarkF1, cex=0.37, xpd=NA)
				
			# }
			
			# legend('bottom', inset=-0.3, xpd=NA, fill=c(colMulti, colLightT1, colLightF1), legend=c(mainName, testName1, testName2), ncol=3, cex=0.5)
		
		# dev.off()
		
	# ### COR - presences & BG
	# ########################
	
		# png(paste0(thisOutDir, '/!Importance - COR - Presences & BG.png'), width=1100, height=1100, res=300)
		
			# main <- 'COR - Presences & BG'
			
			# testVar1 <- 'corPresBgMulti_permT1'
			# testVar2 <- 'corPresBgMulti_permF1'
			
			# testName1 <- 'permuted TRUE'
			# testName2 <- 'permuted FALSE'
			
			# minVal <- -0.1
			# maxVal <- 1
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# par(mgp=c(1, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
			# lines(c(0, 5), c(0, 0), col='gray80', lwd=1)
			
			# for (i in seq_along(allAlgos)) {
			
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]

				# data <- results[results$algo == thisAlgo, testVar1]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), colDarkT1), border=colDarkT1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i - 0.19, maxwidth=0.5, overallline='median')
				
				# data <- results[results$algo == thisAlgo, testVar2]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), colDarkF1), border=colDarkF1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i + 0.19, maxwidth=0.5, overallline='median')
				
			# }
			
			# legend('bottom', inset=-0.3, xpd=NA, fill=c(colLightT1, colLightF1), legend=c(testName1, testName2), cex=0.5, ncol=2)
		
		# dev.off()
		
	# ### COR - presences & absences
	# ##############################
	
		# png(paste0(thisOutDir, '/!Importance - COR - Presences & Absences.png'), width=1100, height=1100, res=300)
		
			# main <- 'COR - Presences & Absences'
			
			# testVar1 <- 'corPresAbsMulti_permT1'
			# testVar2 <- 'corPresAbsMulti_permF1'
			
			# testName1 <- 'permuted TRUE'
			# testName2 <- 'permuted FALSE'
			
			# minVal <- -0.1
			# maxVal <- 1
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# par(mgp=c(1, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
			# lines(c(0, 5), c(0, 0), col='gray80', lwd=1)
				
			# for (i in seq_along(allAlgos)) {
			
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]

				# data <- results[results$algo == thisAlgo, testVar1]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), colDarkT1), border=colDarkT1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i - 0.19, maxwidth=0.5, overallline='median')
				
				# data <- results[results$algo == thisAlgo, testVar2]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), colDarkF1), border=colDarkF1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i + 0.19, maxwidth=0.5, overallline='median')
				
			# }
			
			# legend('bottom', inset=-0.3, xpd=NA, fill=c(colLightT1, colLightF1), legend=c(testName1, testName2), cex=0.5, ncol=2)
		
		# dev.off()

	# ### COR - stratified background
	# ##############################
	
		# png(paste0(thisOutDir, '/!Importance - COR - Stratified Background.png'), width=1100, height=1100, res=300)
		
			# main <- 'COR - Stratified Background'
			
			# testVar1 <- 'corStratBgMulti_permT1'
			# testVar2 <- 'corStratBgMulti_permF1'
			
			# testName1 <- 'permuted TRUE'
			# testName2 <- 'permuted FALSE'
			
			# minVal <- -0.1
			# maxVal <- 1
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# par(mgp=c(1, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
			# lines(c(0, 5), c(0, 0), col='gray80', lwd=1)
				
			# for (i in seq_along(allAlgos)) {
			
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]

				# data <- results[results$algo == thisAlgo, testVar1]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), colDarkT1), border=colDarkT1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i - 0.19, maxwidth=0.5, overallline='median')
				
				# data <- results[results$algo == thisAlgo, testVar2]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), colDarkF1), border=colDarkF1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i + 0.19, maxwidth=0.5, overallline='median')
				
			# }
			
			# legend('bottom', inset=-0.3, xpd=NA, fill=c(colLightT1, colLightF1), legend=c(testName1, testName2), cex=0.5, ncol=2)
		
		# dev.off()

# say('#####################')
# say('### [sample size] ###')
# say('#####################')

	# thisOutDir <- './Results/sample size'

	# ## load and collate data
	
	# results <- loadEvals(thisOutDir)
	# save(results, file=paste0(thisOutDir, '/evaluations/!All Evaluations Collated.RData'))
	
	# load(paste0(thisOutDir, '/evaluations/!All Evaluations Collated.Rdata'))
	# sampleSize <- sort(unique(results$numTrainPres))
	# sampleSize <- 2^(3:9)
	# results <- results[results$numTrainPres %in% sampleSize, ]
	
	# ### CBI - multivariate vs permuted
	# ##################################
	
	# # offset <- seq(0.925, 1.075, length.out=4) # if plotting ALL levels of sample size
	# offset <- seq(0.90, 1.1, length.out=4) # if plotting 2^x levels of sample size
	
	# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=2000, height=1100, res=300)
	# par(mfrow=c(1, 2), mgp=c(1.2, 0.3, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.5, mar=c(5, 3, 4, 1))

		# # by VARIABLE
		# for (thisVar in c('T1', 'F1')) {
	
			# xlim <- c(0.85, 2.75)
			# ylim <- c(-1, 1)
			# ylab <- 'CBI\n\U2190 more important    less important\U2192'
			
			# lab <- if (thisVar == 'T1') {
				# 'a) Permutation importance of TRUE variable'
			# } else {
				# 'b) Permutation importance of FALSE variable'
			# }

			# plot(1, 1, col='white', xlab='Number of training presences', ylab=ylab, ylim=ylim, xlim=xlim, xlog=TRUE, xaxt='n')
			# # axis(1, at=log10(sampleSize), labels=sampleSize, cex.axis=0.365)
			# axis(1, at=log10(sampleSize), labels=sampleSize)
			# text(0.3, 1.28, xpd=NA, labels=lab, pos=4, cex=0.635)
			
			# ### unpermuted

			# mainVar <- 'cbiMulti'
			
			# # by ALGORITHM
			# for (i in seq_along(allAlgos)) {
		
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]
				# pch <- algoPch[i]

				# # error bars
				# for (thisN in sampleSize) {
				
					# set <- results[results$algo == thisAlgo & results$numTrainPres == thisN, ]
					# vals <- set[ , mainVar]
					# low <- quantile(vals, 0.025, na.rm=TRUE)
					# high <- quantile(vals, 0.975, na.rm=TRUE)
					# thisSample <- sum(!is.na(vals))
					# x <- log10(offset[i] * thisN)
					# lines(c(x, x), c(low, high), lty='solid', col=colDark)
				
				# }
				
				# # trendlines
				# set <- results[results$algo == thisAlgo, ]
				# set$algo <- set$circle <- set$response <- NULL
				# set <- aggregate(set, by=list(set$numTrainPres), FUN=median, na.rm=TRUE)
				# set <- set[order(set$numTrainPres), ]
				
				# # lines(set$numTrainPres + offset[i], set[ , mainVar], col=colDark)
				# points(log10(set$numTrainPres * offset[i]), set[ , mainVar], col=colDark, bg=colLight, pch=pch, cex=0.8)
				
			# }
			
			# ### permuted

			# offset <- 1.03 * offset
			# mainVar <- paste0('cbiMulti_perm', thisVar)
			
			# # by ALGORITHM
			# for (i in seq_along(allAlgos)) {
		
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]
				# pch <- algoPch[i]

				# # error bars
				# for (thisN in sampleSize) {
				
					# set <- results[results$algo == thisAlgo & results$numTrainPres == thisN, ]
					# vals <- set[ , mainVar]
					# low <- quantile(vals, 0.025, na.rm=TRUE)
					# high <- quantile(vals, 0.975, na.rm=TRUE)
					# thisSample <- sum(!is.na(vals))
					# x <- log10(offset[i] * thisN)
					# lines(c(x, x), c(low, high), lty='dotted', col=colDark)
				
				# }
				
				# # trendlines
				# set <- results[results$algo == thisAlgo, ]
				# set$algo <- set$circle <- set$response <- NULL
				# set <- aggregate(set, by=list(set$numTrainPres), FUN=median, na.rm=TRUE)
				# set <- set[order(set$numTrainPres), ]
				
				# # lines(set$numTrainPres + offset[i], set[ , mainVar], col=colDark, lty='solid')
				# points(log10(set$numTrainPres * offset[i]), set[ , mainVar], col=colDark, bg=bg, pch=pch, cex=0.8)
				
			# }
			
			# legend('bottomright', inset=0.01, xpd=NA, pch=c(algoPch, algoPch), pt.bg=c(lights, rep(bg, 4)), col=c(darks, darks), ncol=2, cex=0.37, legend=c(paste(niceAlgos, 'unpermuted'), paste(niceAlgos, 'permuted')), bty='n', pt.cex=0.7)
	
		# }
	
	# dev.off()

	# ### COR - presences & BG
	# ########################

	# # offset <- seq(0.95, 1.05, length.out=2) # if plotting all levels of sample size
	# offset <- seq(0.90, 1.1, length.out=4) # if plotting 2^x levels of sample size

	# png(paste0(thisOutDir, '/!Importance - COR Presences & BG.png'), width=1000, height=1100, res=300)
	# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))

		# xlim <- c(0.85, 2.75)
		# ylim <- c(-1, 1)
		# ylab <- 'COR\n\U2190 more important    less important\U2192'
		
		# plot(1, 1, col='white', xlab='Number of training presences', ylab=ylab, ylim=ylim, xlim=xlim, xlog=TRUE, xaxt='n')
		# # axis(1, at=log10(sampleSize), labels=sampleSize, cex.axis=0.365)
		# axis(1, at=log10(sampleSize), labels=sampleSize)
		
		# ### permuted

		# for (thisVar in c('T1', 'F1')) {
			
			# mainVar <- paste0('corPresBgMulti_perm', thisVar)
			
			# # by ALGORITHM
			# for (i in seq_along(allAlgos)) {
		
				# # thisOffset <- ifelse(thisVar == 'T1', offset[1], offset[2]) + i / 30 - mean((1:4) / 30)
				# thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.02, 0.02)
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- ifelse(thisVar == 'T1', colLightT1, bg)
				# colDark <- ifelse(thisVar == 'T1', colDarkT1, colDarkF1)
				# pch <- algoPch[i]

				# # error bars
				# for (thisN in sampleSize) {
				
					# set <- results[results$algo == thisAlgo & results$numTrainPres == thisN, ]
					# vals <- set[ , mainVar]
					# low <- quantile(vals, 0.025, na.rm=TRUE)
					# high <- quantile(vals, 0.975, na.rm=TRUE)
					# thisSize <- sum(!is.na(vals))
					# x <- log10(thisOffset * thisN)
					# lines(c(x, x), c(low, high), lty=ifelse(thisVar=='T1', 'solid', 'dotted'), col=colDark)
				
				# }
				
				# # trendlines
				# set <- results[results$algo == thisAlgo, ]
				# set$algo <- set$circle <- set$response <- NULL
				# set <- aggregate(set, by=list(set$numTrainPres), FUN=median, na.rm=TRUE)
				# set <- set[order(set$numTrainPres), ]
				
				# # lines(set$numTrainPres + thisOffset, set[ , mainVar], col=colDark, lty='solid')
				# points(log10(set$numTrainPres * thisOffset), set[ , mainVar], col=colDark, bg=colLight, pch=pch)
				
			# }
			
		# } # next variable
		
		# legend('bottomleft', inset=0.01, xpd=NA, pch=algoPch, pt.bg=rep(c(colLightT1, bg), each=4), col=rep(c(colDarkT1, colDarkF1), each=4), ncol=2, cex=0.4, legend=c(paste(niceAlgos, 'TRUE'), paste(niceAlgos, 'FALSE')), bty='n', pt.cex=0.7)

	# dev.off()

say('####################')
say('### [prevalence] ###')
say('####################')

	thisOutDir <- './Results/prevalence'

	b11s <- c(-1.74, -1.08, -0.7, -0.33, 0, 0.33, 0.7, 1.08, 1.74)
	
	### load and collate data
	
	results <- loadEvals(thisOutDir)
	save(results, file=paste0(thisOutDir, '/evaluations/!All Evaluations Collated.Rdata'))
	
	load(paste0(thisOutDir, '/evaluations/!All Evaluations Collated.Rdata'))
	
	### CBI - multivariate vs permuted
	##################################
	
		offset <- seq(-0.025, 0.025, length.out=4)
	
		png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=2000, height=1100, res=300)
		par(mfrow=c(1, 2), mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			# by VARIABLE
			for (thisVar in c('T1', 'F1')) {
		
				minVal <- -0.65
				ylab <- 'CBI\n\U2190 more important    less important\U2192'
				
				lab <- if (thisVar == 'T1') {
					'a) Permutation importance of TRUE variable'
				} else {
					'b) Permutation importance of FALSE variable'
				}
				
				plot(1, 1, col='white', xlab='Prevalance', ylab=ylab, ylim=c(minVal, 1), xlim=c(0, 1))
				text(-0.32, 1.2, xpd=NA, labels=lab, pos=4, cex=0.7)
				
				### unpermuted

				mainVar <- 'cbiMulti'
				
				# by ALGORITHM
				for (i in seq_along(allAlgos)) {
			
					thisAlgo <- allAlgos[i]
					niceAlgo <- niceAlgos[i]
					colLight <- lights[i]
					colDark <- darks[i]
					pch <- algoPch[i]

					# error bars
					for (b11 in b11s) {
					
						set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						prev <- unique(set$prev)
						vals <- set[ , mainVar]
						low <- quantile(vals, 0.025, na.rm=TRUE)
						high <- quantile(vals, 0.975, na.rm=TRUE)
						n <- sum(!is.na(vals))
						lines(c(prev, prev) + offset[i], c(low, high), lty='solid', col=colDark)
					
					}
					
					# trendlines
					set <- results[results$algo == thisAlgo, ]
					set$algo <- set$circle <- set$response <- NULL
					sampleSize <- set
					set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					set <- set[order(set$prev), ]

					sampleSize$n <- 1
					if (anyNA(sampleSize[ , mainVar])) sampleSize <- sampleSize[-which(is.na(sampleSize[ , mainVar])), ]
					sampleSize <- aggregate(sampleSize, by=list(sampleSize$b11), FUN=sum, na.rm=TRUE)
					say(thisAlgo, ' sample size:', paste('| prev:', round(set$prev, 2), ' n:', sampleSize$n, collapse=' '))
					
					# lines(set$prev + offset[i], set[ , mainVar], col=colDark)
					points(set$prev + offset[i], set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
					
				}
				
				### permuted

				offset <- offset + 0.005
				mainVar <- paste0('cbiMulti_perm', thisVar)
				
				# by ALGORITHM
				for (i in seq_along(allAlgos)) {
			
					thisAlgo <- allAlgos[i]
					niceAlgo <- niceAlgos[i]
					colLight <- lights[i]
					colDark <- darks[i]
					pch <- algoPch[i]

					# error bars
					for (b11 in b11s) {
					
						set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						prev <- unique(set$prev)
						vals <- set[ , mainVar]
						low <- quantile(vals, 0.025, na.rm=TRUE)
						high <- quantile(vals, 0.975, na.rm=TRUE)
						n <- sum(!is.na(vals))
						lines(c(prev, prev) + offset[i], c(low, high), lty='dotted', col=colDark)
					
					}
					
					# trendlines
					set <- results[results$algo == thisAlgo, ]
					set$algo <- set$circle <- set$response <- NULL
					set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					set <- set[order(set$prev), ]
					
					# lines(set$prev + offset[i], set[ , mainVar], col=colDark, lty='solid')
					points(set$prev + offset[i], set[ , mainVar], col=colDark, bg=bg, pch=pch)
					
				}
				
				legend('bottomleft', inset=0.01, xpd=NA, pch=c(algoPch, algoPch), pt.bg=c(lights, rep(bg, 4)), col=c(darks, darks), ncol=2, cex=0.4, legend=c('Omniscient unpermuted', 'BRT unpermuted', 'GAM unpermuted', 'Maxent unpermuted', 'Omniscient permuted', 'BRT permuted', 'GAM permuted', 'Maxent permuted'), bty='n', pt.cex=0.7)
		
			}
		
		dev.off()
			
	### COR - presences & BG
	########################
	
		offset <- seq(-0.025, 0.025, length.out=4)
	
		png(paste0(thisOutDir, '/!Importance - COR Presences & BG.png'), width=1000, height=1100, res=300)
		par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			minVal <- -0.6
			ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			plot(1, 1, col='white', xlab='Prevalance', ylab=ylab, ylim=c(minVal, 1), xlim=c(0, 1))
			
			### permuted

			for (thisVar in c('T1', 'F1')) {
				
				mainVar <- paste0('corPresBgMulti_perm', thisVar)
				
				# by ALGORITHM
				for (i in seq_along(allAlgos)) {
			
					thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.01, 0.01)
					thisAlgo <- allAlgos[i]
					niceAlgo <- niceAlgos[i]
					colLight <- ifelse(thisVar == 'T1', colLightT1, bg)
					colDark <- ifelse(thisVar == 'T1', colDarkT1, colDarkF1)
					pch <- algoPch[i]

					# error bars
					for (b11 in b11s) {
					
						set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						prev <- unique(set$prev)
						vals <- set[ , mainVar]
						low <- quantile(vals, 0.025, na.rm=TRUE)
						high <- quantile(vals, 0.975, na.rm=TRUE)
						n <- sum(!is.na(vals))
						lines(c(prev, prev) + thisOffset, c(low, high), lty=ifelse(thisVar=='T1', 'solid', 'dotted'), col=colDark)
					
					}
					
					# trendlines
					set <- results[results$algo == thisAlgo, ]
					set$algo <- set$circle <- set$response <- NULL
					set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					set <- set[order(set$prev), ]
					
					# lines(set$prev + thisOffset, set[ , mainVar], col=colDark, lty='solid')
					points(set$prev + thisOffset, set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				}
				
			} # next variable
			
			legend('bottomleft', inset=0.01, xpd=NA, pch=algoPch, pt.bg=rep(c(colLightT1, bg), each=4), col=rep(c(colDarkT1, colDarkF1), each=4), ncol=2, cex=0.4, legend=c(paste(niceAlgos, 'TRUE'), paste(niceAlgos, 'FALSE')), bty='n', pt.cex=0.7)
	
		dev.off()
			
	### COR - stratified BG
	#######################
	
		offset <- seq(-0.025, 0.025, length.out=4)
	
		png(paste0(thisOutDir, '/!Importance - COR Stratified BG.png'), width=1000, height=1100, res=300)
		par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			minVal <- -0.6
			ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			plot(1, 1, col='white', xlab='Prevalance', ylab=ylab, ylim=c(minVal, 1), xlim=c(0, 1))
			
			### permuted

			for (thisVar in c('T1', 'F1')) {
				
				mainVar <- paste0('corStratBgMulti_perm', thisVar)
				
				# by ALGORITHM
				for (i in seq_along(allAlgos)) {
			
					thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.01, 0.01)
					thisAlgo <- allAlgos[i]
					niceAlgo <- niceAlgos[i]
					colLight <- ifelse(thisVar == 'T1', colLightT1, bg)
					colDark <- ifelse(thisVar == 'T1', colDarkT1, colDarkF1)
					pch <- algoPch[i]

					# error bars
					for (b11 in b11s) {
					
						set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						prev <- unique(set$prev)
						vals <- set[ , mainVar]
						low <- quantile(vals, 0.025, na.rm=TRUE)
						high <- quantile(vals, 0.975, na.rm=TRUE)
						n <- sum(!is.na(vals))
						lines(c(prev, prev) + thisOffset, c(low, high), lty=ifelse(thisVar=='T1', 'solid', 'dotted'), col=colDark)
					
					}
					
					# trendlines
					set <- results[results$algo == thisAlgo, ]
					set$algo <- set$circle <- set$response <- NULL
					set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					set <- set[order(set$prev), ]
					
					# lines(set$prev + thisOffset, set[ , mainVar], col=colDark, lty='solid')
					points(set$prev + thisOffset, set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				}
				
			} # next variable
			
			legend('bottomleft', inset=0.01, xpd=NA, pch=algoPch, pt.bg=rep(c(colLightT1, bg), each=4), col=rep(c(colDarkT1, colDarkF1), each=4), ncol=2, cex=0.4, legend=c(paste(niceAlgos, 'TRUE'), paste(niceAlgos, 'FALSE')), bty='n', pt.cex=0.7)
	
		dev.off()
		
# say('################')
# say('### [extent] ###')
# say('################')

	# thisOutDir <- './Results/extent'

	# ### load and collate data
	# # results <- loadEvals(thisOutDir)
	# # save(results, file=paste0(thisOutDir, '/evaluations/!All Evaluations Collated.Rdata'))
	
	# load(paste0(thisOutDir, '/evaluations/!All Evaluations Collated.Rdata'))
	# results$rangeT1 <- results$maxT1 - results$minT1
	
	# ### amount of T1 necessary to change probability of presence from 0.05 to 0.95
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# T1 <- seq(-16, 16, by=0.01)
	# prob <- logisticShift(x1=T1, x2=0, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12)
	# lowerT1 <- T1[which.min(abs(prob - 0.025))]
	# upperT1 <- T1[which.min(abs(prob - 0.975))]
	
	# say('Changing T1 from ', lowerT1, ' to ', upperT1, ' alters probability of presence from 0.025 to 0.975, respectively')
	
	# ### CBI - multivariate vs permuted
	# ##################################
	
		# offset <- seq(-0.15, 0.15, length.out=4)
		# offset <- offset - 0.5 * (1/3) * (offset[2] - offset[1])
		
		# ranges <- sort(unique(results$rangeT1))
	
		# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=2000, height=1100, res=300)
		# par(mfrow=c(1, 2), mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			# # by VARIABLE
			# for (thisVar in c('T1', 'F1')) {
		
				# xlim <- c(-2.2, 4.2)
				# minVal <- -1.17
				# ylab <- 'CBI\n\U2190 more important    less important\U2192'
				
				# lab <- if (thisVar == 'T1') {
					# 'a) Permutation importance of TRUE variable'
				# } else {
					# 'b) Permutation importance of FALSE variable'
				# }
	
				# plot(1, 1, col='white', xlab='Range of TRUE Variable', ylab=ylab, ylim=c(minVal, 1), xlim=xlim, xaxt='n', xlog=TRUE)
				# axis(1, at=-2:4, labels=ranges)
				# text(-4.25, 1.23, xpd=NA, labels=lab, pos=4, cex=0.7)
				
				# ### unpermuted

				# mainVar <- 'cbiMulti'
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- lights[i]
					# colDark <- darks[i]
					# pch <- algoPch[i]

					# # error bars
					# for (thisRange in ranges) {
					
						# set <- results[results$algo == thisAlgo & results$rangeT1 == thisRange, ]
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(log(c(thisRange, thisRange), base=2) + offset[i], c(low, high), lty='solid', col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$rangeT1), FUN=median, na.rm=TRUE)
					# set <- set[order(set$rangeT1), ]
					
					# # lines(log(set$rangeT1) + offset[i], set[ , mainVar], col=colDark)
					# points(log(set$rangeT1, base=2) + offset[i], set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				# }
				
				# ### permuted

				# offset <- offset + (1/3) * (offset[2] - offset[1])
				# mainVar <- paste0('cbiMulti_perm', thisVar)
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- lights[i]
					# colDark <- darks[i]
					# pch <- algoPch[i]

					# # error bars
					# for (thisRange in ranges) {
					
						# set <- results[results$algo == thisAlgo & results$rangeT1 == thisRange, ]
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(log(c(thisRange, thisRange), base=2) + offset[i], c(low, high), lty='dotted', col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$rangeT1), FUN=median, na.rm=TRUE)
					# set <- set[order(set$rangeT1), ]
					
					# # lines(log(set$rangeT1, base=2) + offset[i], set[ , mainVar], col=colDark, lty='solid')
					# points(log(set$rangeT1, base=2) + offset[i], set[ , mainVar], col=colDark, bg=bg, pch=pch)
					
				# }
				
				# legend('bottomleft', inset=0.01, xpd=NA, pch=c(algoPch, algoPch), pt.bg=c(lights, rep(bg, 4)), col=c(darks, darks), ncol=2, cex=0.4, legend=c('Omniscient unpermuted', 'BRT unpermuted', 'GAM unpermuted', 'Maxent unpermuted', 'Omniscient permuted', 'BRT permuted', 'GAM permuted', 'Maxent permuted'), bty='n', pt.cex=0.7)
		
			# }
		
		# dev.off()
			
	# ### COR - presences & BG
	# ########################
	
		# offset <- seq(-0.17, 0.17, length.out=4)
	
		# ranges <- sort(unique(results$rangeT1))
	
		# png(paste0(thisOutDir, '/!Importance - COR Presences & BG.png'), width=1000, height=1100, res=300)
		# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			# xlim <- c(-2.2, 4.2)
			# minVal <- -0.6
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# plot(1, 1, col='white', xlab='Range of TRUE Variable', ylab=ylab, ylim=c(minVal, 1), xlim=xlim, xaxt='n', xlog=TRUE)
			# axis(1, at=-2:4, labels=ranges)
			
			# ### permuted

			# for (thisVar in c('T1', 'F1')) {
				
				# mainVar <- paste0('corPresBgMulti_perm', thisVar)
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.025, 0.025)
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- ifelse(thisVar == 'T1', colLightT1, bg)
					# colDark <- ifelse(thisVar == 'T1', colDarkT1, colDarkF1)
					# pch <- algoPch[i]

					# # error bars
					# for (thisRange in ranges) {
					
						# set <- results[results$algo == thisAlgo & results$rangeT1 == thisRange, ]
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(log(c(thisRange, thisRange), base=2) + thisOffset, c(low, high), lty=ifelse(thisVar=='T1', 'solid', 'dotted'), col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$rangeT1), FUN=median, na.rm=TRUE)
					# set <- set[order(set$rangeT1), ]
					
					# # lines(log(set$rangeT1) + thisOffset, set[ , mainVar], col=colDark, lty='solid')
					# points(log(set$rangeT1, base=2) + thisOffset, set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				# }
				
			# } # next variable
			
			# legend('bottomleft', inset=0.01, xpd=NA, pch=algoPch, pt.bg=rep(c(colLightT1, bg), each=4), col=rep(c(colDarkT1, colDarkF1), each=4), ncol=2, cex=0.4, legend=c(paste(niceAlgos, 'TRUE'), paste(niceAlgos, 'FALSE')), bty='n', pt.cex=0.7)
	
		# dev.off()

# say('######################################')
# say('### [bivariate -- collate results] ###')
# say('######################################')

	# thisOutDir <- './Results/bivariate'

	# say('Loading results...')
	# results <- data.frame()
	# for (algo in allAlgos) {
	
		# files <- listFiles(paste0(thisOutDir, '/evaluations'), pattern=toupper(algo))
		
		# thisResults <- data.frame()
		# for (file in files) {
		
			# say(file)
		
			# this <- readRDS(file)
			
			# rotAt <- str_locate_all(pattern='rot', file)
			# rhoAt <- str_locate_all(pattern='rho', file)
			# rot <- as.numeric(substr(file, rotAt[[1]][1] + 8, rhoAt[[1]][1] - 2))
			# this$rotT2 <- rot
			
			# thisResults <- rbind(thisResults, this)
		
		# }
		
		# results <- merge(results, thisResults, all=TRUE)
	
	# }
	
	# say('Calculating correlation between T1 and T2... rotation:', post=0)
	
	# results$corT1T2 <- NA
	
	# for (rot in c(22.5, 45, 67.5, 90, 112.5, 135, 157.5)) {
	
		# say(rot, post=0)
	
		# geography <- list(
			# T1=list(type='linear', min=-1, max=1, pregen=FALSE),
			# T2=list(type='linear', min=-1, max=1, pregen=FALSE, rot=rot)
		# )
		
		# landscape <- genesis(geography)
		
		# T1 <- c(as.matrix(landscape[['T1']]))
		# T2 <- c(as.matrix(landscape[['T2']]))
		
		# corT1T2 <- stats::cor(T1, T2, use='complete.obs')
		
		# results$corT1T2[results$rot == rot] <- corT1T2
	
	# }
	
	# say('')
	
# say('####################')
# say('### [resolution] ###')
# say('####################')

	# thisOutDir <- './Results/resolution'

	# # ### load and collate data
	# # results <- loadEvals(thisOutDir)
	# # save(results, file=paste0(thisOutDir, '/evaluations/!All Evaluations Collated.Rdata'))
	
	# load(paste0(thisOutDir, '/evaluations/!All Evaluations Collated.Rdata'))
	# results$resMult <- 1024 / results$landscapeSize
	# resMult <- sort(unique(results$resMult))
	
	# ### CBI - multivariate vs permuted
	# ##################################
	
		# offset <- seq(-0.17, 0.17, length.out=4)
		# offset <- offset - 0.5 * (1/3) * (offset[2] - offset[1])
		
		# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=2000, height=1100, res=300)
		# par(mfrow=c(1, 2), mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.6, cex.axis=0.425, mar=c(5, 3, 4, 1))
	
			# # by VARIABLE
			# for (thisVar in c('T1', 'F1')) {
		
				# ats <- seq_along(resMult)
				# xlim <- range(seq_along(resMult)) + c(-0.5, 0.5)
				# minVal <- -1
				# ylab <- 'CBI\n\U2190 more important    less important\U2192'
				
				# lab <- if (thisVar == 'T1') {
					# 'a) Permutation importance of TRUE variable'
				# } else {
					# 'b) Permutation importance of FALSE variable'
				# }
	
				# labs <- paste0('\U00D7', resMult)
	
				# plot(1, 1, col='white', xlab='Resolution (relative to native)', ylab=ylab, ylim=c(minVal, 1), xlim=xlim, xaxt='n', xlog=TRUE)
				# axis(1, at=ats, labels=labs)
				# text(0.45, 1.23, xpd=NA, labels=lab, pos=4, cex=0.7)
				
				# ### unpermuted

				# mainVar <- 'cbiMulti'
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- lights[i]
					# colDark <- darks[i]
					# pch <- algoPch[i]

					# # error bars
					# for (countRes in seq_along(resMult)) {
					
						# set <- results[results$algo == thisAlgo & results$resMult == resMult[countRes], ]
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(c(countRes, countRes) + offset[i], c(low, high), lty='solid', col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$resMult), FUN=median, na.rm=TRUE)
					# set <- set[order(set$resMult), ]
					
					# # lines(log(set$rangeT1) + offset[i], set[ , mainVar], col=colDark)
					# points(ats + offset[i], set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				# }
				
				# ### permuted

				# offset <- offset + (3/7) * (offset[2] - offset[1])
				# mainVar <- paste0('cbiMulti_perm', thisVar)
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- lights[i]
					# colDark <- darks[i]
					# pch <- algoPch[i]

					# # error bars
					# for (countRes in seq_along(resMult)) {
					
						# set <- results[results$algo == thisAlgo & results$resMult == resMult[countRes], ]
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(c(countRes, countRes) + offset[i], c(low, high), lty='dotted', col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$resMult), FUN=median, na.rm=TRUE)
					# set <- set[order(set$resMult), ]
					
					# # lines(log(set$rangeT1, base=2) + offset[i], set[ , mainVar], col=colDark, lty='solid')
					# points(ats + offset[i], set[ , mainVar], col=colDark, bg=bg, pch=pch)
					
				# }
				
				# legend('bottomleft', inset=0.01, xpd=NA, pch=c(algoPch, algoPch), pt.bg=c(lights, rep(bg, 4)), col=c(darks, darks), ncol=2, cex=0.4, legend=c('Omniscient unpermuted', 'BRT unpermuted', 'GAM unpermuted', 'Maxent unpermuted', 'Omniscient permuted', 'BRT permuted', 'GAM permuted', 'Maxent permuted'), bty='n', pt.cex=0.7)
		
			# }
		
		# dev.off()
			
	# ### COR - presences & BG
	# ########################
	
		# offset <- seq(-0.19, 0.19, length.out=4)
	
		# png(paste0(thisOutDir, '/!Importance - COR Presences & BG.png'), width=1000, height=1100, res=300)
		# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.425, mar=c(5, 3, 4, 1))
	
			# ats <- seq_along(resMult)
			# xlim <- range(seq_along(resMult)) + c(-0.5, 0.5)
			# minVal <- -1
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# labs <- paste0('\U00D7', resMult)
			
			# plot(1, 1, col='white', xlab='Resolution (relative to native)', ylab=ylab, ylim=c(minVal, 1), xlim=xlim, xaxt='n', xlog=TRUE)
			# axis(1, at=ats, labels=labs)
			
			# ### permuted

			# for (thisVar in c('T1', 'F1')) {
				
				# mainVar <- paste0('corPresBgMulti_perm', thisVar)
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.045, 0.045)
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- ifelse(thisVar == 'T1', colLightT1, bg)
					# colDark <- ifelse(thisVar == 'T1', colDarkT1, colDarkF1)
					# pch <- algoPch[i]

					# # error bars
					# for (countRes in seq_along(resMult)) {
					
						# set <- results[results$algo == thisAlgo & results$resMult == resMult[countRes], ]
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(c(countRes, countRes) + thisOffset, c(low, high), lty=ifelse(thisVar=='T1', 'solid', 'dotted'), col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$resMult), FUN=median, na.rm=TRUE)
					# set <- set[order(set$resMult), ]
					
					# # lines(log(set$landscapeSize) + thisOffset, set[ , mainVar], col=colDark, lty='solid')
					# points(ats + thisOffset, set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				# }
				
			# } # next variable
			
			# legend('bottomleft', inset=0.01, xpd=NA, pch=algoPch, pt.bg=rep(c(colLightT1, bg), each=4), col=rep(c(colDarkT1, colDarkF1), each=4), ncol=2, cex=0.4, legend=c(paste(niceAlgos, 'TRUE'), paste(niceAlgos, 'FALSE')), bty='n', pt.cex=0.7)
	
		# dev.off()


# say('##########################################')
# say('### [tune brt for bivariate responses] ###')
# say('##########################################')

	# thisOutDir <- './Results/tune brt for bivariate responses'

	# ### collate evaluation with BRT parameters
	# say('Loading tuning parameters for each model...')

		# # create progress frame
		# progress <- data.frame()
		# rot <- c(22.5, 90, 157.5)
		# rho <- c(-0.75, 0, 0.75)
		# sigmaValues <- c(0.1, 0.3, 0.5)

		# for (rot in rot) {
			# for (thisRho in rho) {
				# for (countSigma1 in seq_along(sigmaValues)) {
					# for (countSigma2 in countSigma1:length(sigmaValues)) {
						
						# line <- data.frame(
							# rot=rot,
							# rho=thisRho,
							# sigma1=sigmaValues[countSigma1],
							# sigma2=sigmaValues[countSigma2]
						# )
						# line$string <- paste(names(line), line, collapse=' ', sep='=')
						# progress <- rbind(progress, line)
						
					# }
				# }
			# }
		# }

		# # load model and assign parametrization to model performance
		# for (countProgress in 1:nrow(progress)) {

			# say(paste(progress[countProgress, ]))
		
			# learningRate <- treeComplexity <- nTrees <- rep(NA, 100)
			
			# for (countModel in 1:100) {
		
				# load(paste0(thisOutDir, '/multivariate brt/brt rot(T2)=', progress$rot[countProgress], ' rho=', progress$rho[countProgress], ' sigma1=', progress$sigma1[countProgress], ' sigma2=', progress$sigma2[countProgress], ' model ', prefix(countModel, 3), '.RData'))
			
				# learningRate[countModel] <- model$gbm.call$learning.rate
				# treeComplexity[countModel] <- model$gbm.call$tree.complexity
				# nTrees[countModel] <- model$gbm.call$best.trees
				
			# }
			
			# load(paste0(thisOutDir, '/evaluations/Evaluations for multivariate BRT rot(T2)=', progress$rot[countProgress], ' rho=', progress$rho[countProgress], ' sigma1=', progress$sigma1[countProgress], ' sigma2=', progress$sigma2[countProgress], '.RData'))
			
			# perform$learningRate <- learningRate
			# perform$treeComplexity <- treeComplexity
			# perform$nTrees <- nTrees

			# save(perform, file=paste0(thisOutDir, '/evaluations/Evaluations for multivariate BRT rot(T2)=', progress$rot[countProgress], ' rho=', progress$rho[countProgress], ' sigma1=', progress$sigma1[countProgress], ' sigma2=', progress$sigma2[countProgress], '.RData'))
			
		# }

	# ### collate and save/load evaluations
	
		# # results <- loadEvals(thisOutDir, 'brt')
		# # save(results, file=paste0(thisOutDir, '/evaluations/!All Evaluations Collated.RData'))
		
		# load(paste0(thisOutDir, '/evaluations/!All Evaluations Collated.RData'))
		
	# ### decide on best combination of BRT parameters
		
		# say('AUC vs pres/abs | mean: ', mean(results$aucPresAbsMulti), ' | 95% CI: ', quantile(results$aucPresAbsMulti, 0.025), '-', quantile(results$aucPresAbsMulti, 0.975))
		
		# say('CBI | mean: ', mean(results$cbiMulti), ' | 95% CI: ', quantile(results$cbiMulti, 0.025), '-', quantile(results$cbiMulti, 0.975))
	
		# par(mfrow=c(1, 2))
		# plot(results$nTrees, results$cbiMulti)
		# points(results$nTrees, results$cbiMulti, col=results$treeComplexity, pch=16)
		# legend('bottom', legend=sort(unique(results$treeComplexity)), col=sort(unique(results$treeComplexity)), pch=16, cex=0.8)

		# learningRates <- sort(unique(results$learningRate))
		# plot(results$nTrees, results$cbiMulti)
		# for (i in seq_along(learningRates)) points(results$nTrees[results$learningRate == learningRates[i]], results$cbiMulti[results$learningRate == learningRates[i]], col=i, pch=16)
		# legend('bottom', legend=learningRates, col=seq_along(learningRates), pch=16, cex=0.8)

		# say('Based on this choosing lr = 0.005, tc = 3, ntrees = 6000 for bivariate responses.')
	
# say('##########################################')
# say('### [bivariate -- collate evaluations] ###')
# say('##########################################')

	# thisOutDir <- './Results/bivariate'
	
	# ### create progress frame
	# progress <- data.frame()
	# rot <- c(22.5, 45, 67.5, 90, 112.5, 135, 157.5)
	# rho <- c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)
	# sigma2Values <- c(0.1, 0.2, 0.3, 0.4, 0.5)

	# for (thisRot in rot) {
		# for (thisRho in rho) {
			# for (thisSigma1 in sigma2Values) {
				# for (thisSigma2 in seq(min(sigma2Values), thisSigma1, by=0.1)) {
					
					# line <- data.frame(
						# rot=thisRot,
						# rho=thisRho,
						# sigma1=thisSigma1,
						# sigma2=thisSigma2
					# )
					# line$string <- paste(names(line), line, collapse=' ', sep='=')
					# progress <- rbind(progress, line)
					
				# }
			# }
		# }
	# }

	# for (algo in allAlgos) {
		
		# results <- data.frame()
		# for (i in 1:nrow(progress)) {
		
			# suffix <- paste0('rot(T2)=', progress$rot[i], ' rho=', progress$rho[i], ' sigma1=', progress$sigma1[i], ' sigma2=', progress$sigma2[i])
			# say(algo, ' ', suffix)
			# thisResults <- readRDS(paste0(thisOutDir, '/evaluations/Evaluations for multivariate univariate ', toupper(algo), ' ', suffix, '.rds'))
			# thisResults$rotT2 <- progress$rot[i]
			# results <- rbind(results, thisResults)
		
		# }
		
		# rownames(results) <- 1:nrow(results)
		
		# saveRDS(results, paste0(thisOutDir, '/evaluations/!Collated Evaluations for ', toupper(algo), '.rds'))
		# rm(results)
		
	# }

# say('#######################################################################')
# say('### [bivariate -- NO landscape correlation and NO niche covariance] ###')
# say('#######################################################################')

	# thisOutDir <- './Results/bivariate'

	# scaleBy <- 23 # value by which to rescale annuli
	
	# # load omniscient results
	# omni <- readRDS(paste0(thisOutDir, '/evaluations/!Collated Evaluations for OMNISCIENT.rds'))
	# omni <- omni[omni$rho == 0 & omni$rotT2 == 90, ]
	# omni$sigma1 <- round(omni$sigma1, 1)
	# omni$sigma2 <- round(omni$sigma2, 1)

	# ### CBI multivariate unpermuted vs multivariate permuted
	# ########################################################
	
	# respControl <- 'cbiMulti'
	# respManip <- 'cbiMulti_perm'
	
	# # for (algo in allAlgosSansOmniscient) {
	# for (algo in 'maxent') {
	
		# enm <- readRDS(paste0(thisOutDir, '/evaluations/!Collated Evaluations for ', toupper(algo), '.rds'))
		# enm <- enm[enm$rho == 0 & enm$rotT2 == 90, ]

		# par(mfrow=c(1, 3), pty='s', mgp=c(2.5, 1, 0))
		
		# # by PREDICTOR
		# for (thisVar in c('T1', 'T2')) {
			
			# ### create plot with sigma1 and sigma2 as axes, each combination has a torus, left side indicating distribution of omni and right side distribution of the ENM
			# plot(0, 0, col='white', xlim=c(0, 0.55), ylim=c(0, 0.55), xlab=expression(paste('Niche breadth on T1 (', sigma[1], ')')), ylab=expression(paste('Niche breadth on T1 (', sigma[2], ')')))
			# lab <- if (thisVar == 'T1') { 'a) Permutation test for T1' } else { 'b) Permutation test for T2' }
			# text(-0.085, 0.6, labels=lab, pos=4, xpd=NA)

			# ### OMNISCIENT vs ENM
			
			# # by SIGMA1
			# for (sigma1 in round(seq(0.1, 0.5, by=0.1), 1)) {

				# # by SIGMA2
				# for (sigma2 in round(seq(0.1, 0.5, by=0.1), 1)) {
				
					# x <- sigma1
					# y <- sigma2

					# if (sigma2 > sigma1) {
						# thisSigma1 <- sigma2
						# thisSigma2 <- sigma1
						# resp <- if (thisVar == 'T1') { 'T2' } else { 'T1' }
					# } else {
						# thisSigma1 <- sigma1
						# thisSigma2 <- sigma2
						# resp <- if (thisVar == 'T1') { 'T1' } else { 'T2' }
					# }
					
					# manipCol <- if (thisVar == 'T1') { colDarkT1 } else { colDarkT2 }
					
					# # OMNISCIENT UNPERMUTED (left side of annulus)
					# vals <- omni[round(omni$sigma1, 1) == round(thisSigma1, 1) & round(omni$sigma2, 1) == round(thisSigma2, 1), respControl]
					# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
					# quants <- quants / scaleBy
					
					# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha('black', 0.5), force0=TRUE)
				
					# # OMNISCIENT PERMUTED (left side of annulus)
					# vals <- omni[round(omni$sigma1, 1) == round(thisSigma1, 1) & round(omni$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp)]
					# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
					# quants <- quants / scaleBy
					
					# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha(manipCol, 0.5), force0=TRUE)
				
					# # ENM UNPERMUTED (right side of annulus)
					# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), respControl]
					# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
					# quants <- quants / scaleBy
					
					# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(0, 180), col=alpha('black', 0.5), force0=TRUE)
				
					# # ENM PERMUTED (right side of annulus)
					# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp)]
					# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
					# quants <- quants / scaleBy
					
					# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(0, 180), col=alpha(manipCol, 0.5), force0=TRUE)
					
					# # scale bar
					# lines(c(x, x), c(y, y + 1 / scaleBy))
					# lines(c(x, x + 0.005), c(y, y))
					# lines(c(x, x + 0.005), c(y, y) + 0.5 * 1 / scaleBy)
					# lines(c(x, x + 0.005), c(y, y) + 1 / scaleBy)
				
				# } # next sigma2
		
			# } # next sigma1
			
		# } # next variable

		# ### ENM T1 vs ENM T2
		
		# ### create plot with sigma1 and sigma2 as axes, each combination has a torus, left side indicating distribution of omni and right side distribution of the ENM
		# plot(0, 0, col='white', xlim=c(0, 0.55), ylim=c(0, 0.55), xlab=expression(paste('Niche breadth on T1 (', sigma[1], ')')), ylab=expression(paste('Niche breadth on T1 (', sigma[2], ')')))
		# lab <- paste0('c) T1 versus T2 using ', toupper(algo))
		# text(-0.085, 0.6, labels=lab, pos=4, xpd=NA)

		# # by SIGMA1
		# for (sigma1 in round(seq(0.1, 0.5, by=0.1), 1)) {

			# # by SIGMA2
			# for (sigma2 in round(seq(0.1, 0.5, by=0.1), 1)) {
			
				# x <- sigma1
				# y <- sigma2

				# if (sigma2 > sigma1) {
					# thisSigma1 <- sigma2
					# thisSigma2 <- sigma1
					# resp1 <- 'T2'
					# resp2 <- 'T1'
				# } else {
					# thisSigma1 <- sigma1
					# thisSigma2 <- sigma2
					# resp1 <- 'T1'
					# resp2 <- 'T2'
				# }
				
				# # ENM UNPERMUTED T1 (left side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp1)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha('black', 0.4), force0=TRUE)
			
				# # ENM PERMUTED T1 (left side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp1)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha(colDarkT1, 0.5), force0=TRUE)
			
				# # ENM UNPERMUTED T2 (right side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp2)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha('black', 0.4), force0=TRUE)
				
				# # ENM PERMUTED T2 (right side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp2)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(1, 179), col=alpha(colDarkT2, 0.5), force0=TRUE)
				
				# # scale bar
				# lines(c(x, x), c(y, y + 1 / scaleBy))
				# lines(c(x, x + 0.005), c(y, y))
				# lines(c(x, x + 0.005), c(y, y) + 0.5 * 1 / scaleBy)
				# lines(c(x, x + 0.005), c(y, y) + 1 / scaleBy)
			
			# } # next sigma2
	
		# } # next sigma1

	# } # next ENM

	# ### COR multivariate unpermuted vs multivariate permuted
	# ########################################################
	
	# respManip <- 'corPresBgMulti_perm'
	
	# # for (algo in allAlgosSansOmniscient) {
	# for (algo in 'maxent') {
	
		# enm <- readRDS(paste0(thisOutDir, '/evaluations/!Collated Evaluations for ', toupper(algo), '.rds'))
		# enm <- enm[enm$rho == 0 & enm$rotT2 == 90, ]

		# par(mfrow=c(1, 3), pty='s', mgp=c(2.5, 1, 0))
		
		# # by PREDICTOR
		# for (thisVar in c('T1', 'T2')) {
			
			# ### create plot with sigma1 and sigma2 as axes, each combination has a torus, left side indicating distribution of omni and right side distribution of the ENM
			# plot(0, 0, col='white', xlim=c(0, 0.55), ylim=c(0, 0.55), xlab=expression(paste('Niche breadth on T1 (', sigma[1], ')')), ylab=expression(paste('Niche breadth on T1 (', sigma[2], ')')))
			# lab <- if (thisVar == 'T1') { 'a) Permutation test for T1' } else { 'b) Permutation test for T2' }
			# text(-0.085, 0.6, labels=lab, pos=4, xpd=NA)

			# ### OMNISCIENT vs ENM
			
			# # by SIGMA1
			# for (sigma1 in round(seq(0.1, 0.5, by=0.1), 1)) {

				# # by SIGMA2
				# for (sigma2 in round(seq(0.1, 0.5, by=0.1), 1)) {
				
					# x <- sigma1
					# y <- sigma2

					# if (sigma2 > sigma1) {
						# thisSigma1 <- sigma2
						# thisSigma2 <- sigma1
						# resp <- if (thisVar == 'T1') { 'T2' } else { 'T1' }
					# } else {
						# thisSigma1 <- sigma1
						# thisSigma2 <- sigma2
						# resp <- if (thisVar == 'T1') { 'T1' } else { 'T2' }
					# }
					
					# manipCol <- if (thisVar == 'T1') { colDarkT1 } else { colDarkT2 }
					
					# # OMNISCIENT PERMUTED (left side of annulus)
					# vals <- omni[round(omni$sigma1, 1) == round(thisSigma1, 1) & round(omni$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp)]
					# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
					# quants <- quants / scaleBy
					
					# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha(manipCol, 0.5), force0=TRUE)
				
					# # ENM PERMUTED (right side of annulus)
					# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp)]
					# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
					# quants <- quants / scaleBy
					
					# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(0, 180), col=alpha(manipCol, 0.5), force0=TRUE)
					
					# # scale bar
					# lines(c(x, x), c(y, y + 1 / scaleBy))
					# lines(c(x, x + 0.005), c(y, y))
					# lines(c(x, x + 0.005), c(y, y) + 0.5 * 1 / scaleBy)
					# lines(c(x, x + 0.005), c(y, y) + 1 / scaleBy)
				
				# } # next sigma2
		
			# } # next sigma1
			
		# } # next variable

		# ### ENM T1 vs ENM T2
		
		# ### create plot with sigma1 and sigma2 as axes, each combination has a torus, left side indicating distribution of omni and right side distribution of the ENM
		# plot(0, 0, col='white', xlim=c(0, 0.55), ylim=c(0, 0.55), xlab=expression(paste('Niche breadth on T1 (', sigma[1], ')')), ylab=expression(paste('Niche breadth on T1 (', sigma[2], ')')))
		# lab <- paste0('c) T1 versus T2 using ', toupper(algo))
		# text(-0.085, 0.6, labels=lab, pos=4, xpd=NA)

		# # by SIGMA1
		# for (sigma1 in round(seq(0.1, 0.5, by=0.1), 1)) {

			# # by SIGMA2
			# for (sigma2 in round(seq(0.1, 0.5, by=0.1), 1)) {
			
				# x <- sigma1
				# y <- sigma2

				# if (sigma2 > sigma1) {
					# thisSigma1 <- sigma2
					# thisSigma2 <- sigma1
					# resp1 <- 'T2'
					# resp2 <- 'T1'
				# } else {
					# thisSigma1 <- sigma1
					# thisSigma2 <- sigma2
					# resp1 <- 'T1'
					# resp2 <- 'T2'
				# }
				
				# # ENM UNPERMUTED T1 (left side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp1)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha('black', 0.4), force0=TRUE)
			
				# # ENM PERMUTED T1 (left side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp1)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha(colDarkT1, 0.5), force0=TRUE)
			
				# # ENM UNPERMUTED T2 (right side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp2)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(180, 360), col=alpha('black', 0.4), force0=TRUE)
				
				# # ENM PERMUTED T2 (right side of annulus)
				# vals <- enm[round(enm$sigma1, 1) == round(thisSigma1, 1) & round(enm$sigma2, 1) == round(thisSigma2, 1), paste0(respManip, resp2)]
				# quants <- quantile(vals, c(0.025, 0.975), na.rm=TRUE)
				# quants <- quants / scaleBy
				
				# annulus(x=x, y=y, inner=quants[1], outer=quants[2], deg=c(1, 179), col=alpha(colDarkT2, 0.5), force0=TRUE)
				
				# # scale bar
				# lines(c(x, x), c(y, y + 1 / scaleBy))
				# lines(c(x, x + 0.005), c(y, y))
				# lines(c(x, x + 0.005), c(y, y) + 0.5 * 1 / scaleBy)
				# lines(c(x, x + 0.005), c(y, y) + 1 / scaleBy)
			
			# } # next sigma2
	
		# } # next sigma1

	# } # next ENM



		
say('DONE!!!!!', level=1)

