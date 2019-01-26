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

library(compiler); library(sp); library(rgdal); library(raster); library(rJava); options(java.parameters='-Xmx1g' ); library(dismo); library(gbm); library(mgcv); library(MuMIn); library(scales); library(beanplot); library(hier.part); library(omnibus); library(enmSdm)

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
### [simple] ###
### correlation test example ###


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
	
	colDarkT1 <- '#00441b' # greenish
	colDarkF1 <- '#800026' # reddish
	
	allAlgos <- c('omniscient', 'brt', 'gam', 'maxent')
	niceAlgos <- c('Omniscient', 'BRT', 'GAM', 'Maxent')

	algoPch <- c(21, 22, 23, 25)
	
#################
### FUNCTIONS ###
#################

	orderAlgos <- function(x) {
	
		# input is a list of algorithm names (e.g., 'omnicient', 'brt', 'gam', 'maxent')
		# output is a vector of indices corresponding to input that will re-order such that omniscient is first, brt second, gam third, and maxent fourth
		
		x <- c(which(x == 'omniscient'), which(x == 'brt'), which(x == 'gam'), which(x == 'maxent'))
		x
		
	}
	
# say('################')
# say('### [simple] ###')
# say('################')

	# thisOutDir <- './Results/simple'

	# # load results

	# results1 <- readRDS(paste0(thisOutDir, '/evaluations/Evaluations for multivariate univariate BRT.rds'))
	# results2 <- readRDS(paste0(thisOutDir, '/evaluations/Evaluations for multivariate univariate GAM.rds'))
	# results3 <- readRDS(paste0(thisOutDir, '/evaluations/Evaluations for multivariate univariate MAXENT.rds'))
	# results4 <- readRDS(paste0(thisOutDir, '/evaluations/Evaluations for multivariate univariate OMNISCIENT.rds'))
	
	# results <- merge(results1, results2, all=TRUE)
	# results <- merge(results, results3, all=TRUE)
	# results <- merge(results, results4, all=TRUE)

	# results <- results[orderAlgos(results$algo), ]
	
	# # bean plot--truncated at lower 2.5% and upper 2.5%, center bar represents median
	# ### CBI - multivariate vs permuted
	# ##################################
	
		# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=1100, height=1100, res=300)
		
			# main <- 'CBI - Multivariate vs Permuted'
			
			# mainVar <- 'cbiMulti'
			# testVar1 <- 'cbiMulti_permT1'
			# testVar2 <- 'cbiMulti_permF1'
			
			# mainName <- 'unpermuted'
			# testName1 <- 'permuted T1'
			# testName2 <- 'permuted F1'
			
			# minVal <- -0.75
			# maxVal <- 1
			# ylab <- 'CBI\n\U2190 more important    less important\U2192'
			
			# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
				
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
			
			# legend('bottom', inset=-0.35, xpd=NA, fill=c(colMulti, colLightT1, colLightF1), legend=c(mainName, testName1, testName2), ncol=3, cex=0.5)
		
		# dev.off()
		
	# ### CBI - multivariate vs univariate
	# ####################################

		# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Univariate.png'), width=1100, height=1100, res=300)
		
			# main <- 'CBI - Multivariate vs Univariate'
			
			# mainVar <- 'cbiMulti'
			# testVar1 <- 'cbiUni_onlyT1'
			# testVar2 <- 'cbiUni_onlyF1'
			
			# mainName <- 'multivariate'
			# testName1 <- 'univariate T1'
			# testName2 <- 'univariate F1'
			
			# minVal <- -0.75
			# maxVal <- 1
			# ylab <- 'CBI\n\U2190 less important    more important\U2192'
			
			# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
				
			# for (i in seq_along(allAlgos)) {
			
				# thisAlgo <- allAlgos[i]
				# niceAlgo <- niceAlgos[i]
				# colLight <- lights[i]
				# colDark <- darks[i]

				# data <- results[results$algo == thisAlgo, mainVar]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(colMulti, colMulti, colMulti, fg), beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i, maxwidth=0.5, overallline='median')
				
				# data <- results[results$algo == thisAlgo, testVar1]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), alpha(colLightT1, 0.75), colDarkT1), border=colDarkT1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i - 0.19, maxwidth=0.5, overallline='median')
				
				# data <- results[results$algo == thisAlgo, testVar2]
				# beanplot(data, what=c(FALSE, TRUE, TRUE, FALSE), col=c(alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), alpha(colLightF1, 0.75), colDarkF1), border=colDarkF1, beanlines='median', bw='nrd0', add=TRUE, cutmin=quantile(data, 0.025, na.rm=T), cutmax=quantile(data, 0.975, na.rm=T), beanlinewd=1, xaxt='n', at=i + 0.19, maxwidth=0.5, overallline='median')
				
			# }
			
			# legend('bottom', inset=-0.35, xpd=NA, fill=c(colMulti, colLightT1, colLightF1), legend=c(mainName, testName1, testName2), ncol=3, cex=0.5)
		
		# dev.off()
		
	# ### COR - presences & BG
	# ########################
	
		# png(paste0(thisOutDir, '/!Importance - COR - Presences & BG.png'), width=1100, height=1100, res=300)
		
			# main <- 'COR - Presences & BG'
			
			# testVar1 <- 'corPresBgMulti_permT1'
			# testVar2 <- 'corPresBgMulti_permF1'
			
			# testName1 <- 'permuted T1'
			# testName2 <- 'permuted F1'
			
			# minVal <- -0.1
			# maxVal <- 1
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
				
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
			
			# legend('bottom', inset=-0.35, xpd=NA, fill=c(colLightT1, colLightF1), legend=c(testName1, testName2), cex=0.5, ncol=2)
		
		# dev.off()
		
	# ### COR - presences & absences
	# ##############################
	
		# png(paste0(thisOutDir, '/!Importance - COR - Presences & Absences.png'), width=1100, height=1100, res=300)
		
			# main <- 'COR - Presences & Absences'
			
			# testVar1 <- 'corPresAbsMulti_permT1'
			# testVar2 <- 'corPresAbsMulti_permF1'
			
			# testName1 <- 'permuted T1'
			# testName2 <- 'permuted F1'
			
			# minVal <- -0.1
			# maxVal <- 1
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5)
			# boxplot(matrix(c(rep(maxVal, 4), rep(minVal, 4)), ncol=4, byrow=TRUE), col=bg, border=bg, names=niceAlgos, ylab=ylab, cex.axis=0.5, cex.lab=0.65, main=main)
				
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
			
			# legend('bottom', inset=-0.35, xpd=NA, fill=c(colLightT1, colLightF1), legend=c(testName1, testName2), cex=0.5, ncol=2)
		
		# dev.off()

# say('###################')
# say('### sample size ###')
# say('###################')

# say('##################')
# say('### prevalance ###')
# say('##################')

	# thisOutDir <- './Results/prevalance'

	# b11s <- c(-1.74, -0.7, 0, 0.7, 1.74)
	
	# # results <- data.frame()
	# # for (thisAlgo in allAlgos) {
	
		# # for (b11 in b11s) {

			# # thisResults <- readRDS(paste0(thisOutDir, '/evaluations/Evaluations for multivariate univariate ', toupper(thisAlgo), ' b11 = ', sprintf('%.2f', b11), '.rds'))
		
			# # results <- if (nrow(results) == 0) {
				# # thisResults
			# # } else {
				# # merge(results, thisResults, all=TRUE)
			# # }
			
		# # }
		
	# # }
	
	# # results <- results[orderAlgos(results$algo), ]

	# # saveRDS(results, paste0(thisOutDir, '/evaluations/!All Evaluations Collated.rds'))
	
	
	# results <- readRDS(paste0(thisOutDir, '/evaluations/!All Evaluations Collated.rds'))
	
	# ### CBI - multivariate vs permuted
	# ##################################
	
		# offset <- seq(-0.0315, 0.0315, length.out=4)
	
		# png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=2000, height=1100, res=300)
		# par(mfrow=c(1, 2), mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			# # by VARIABLE
			# for (thisVar in c('T1', 'F1')) {
		
				# minVal <- -0.6
				# ylab <- 'CBI\n\U2190 more important    less important\U2192'
				
				# lab <- if (thisVar == 'T1') {
					# 'a) Permutation importance for TRUE variable'
				# } else {
					# 'b) Permutation importance for FALSE variable'
				# }
				
				# plot(1, 1, col='white', xlab='Prevalance', ylab=ylab, ylim=c(minVal, 1), xlim=c(0, 1))
				# text(-0.32, 1.2, xpd=NA, labels=lab, pos=4, cex=0.7)
				
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
					# for (b11 in b11s) {
					
						# set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						# prev <- unique(set$prev)
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(c(prev, prev) + offset[i], c(low, high), lty='solid', col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					# set <- set[order(set$prev), ]
					
					# # lines(set$prev + offset[i], set[ , mainVar], col=colDark)
					# points(set$prev + offset[i], set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				# }
				
				# ### permuted

				# offset <- offset + 0.007
				# mainVar <- paste0('cbiMulti_perm', thisVar)
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- lights[i]
					# colDark <- darks[i]
					# pch <- algoPch[i]

					# # error bars
					# for (b11 in b11s) {
					
						# set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						# prev <- unique(set$prev)
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(c(prev, prev) + offset[i], c(low, high), lty='dotted', col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					# set <- set[order(set$prev), ]
					
					# # lines(set$prev + offset[i], set[ , mainVar], col=colDark, lty='solid')
					# points(set$prev + offset[i], set[ , mainVar], col=colDark, bg=bg, pch=pch)
					
				# }
				
				# legend('bottomleft', inset=0.01, xpd=NA, pch=c(algoPch, algoPch), pt.bg=c(lights, rep(bg, 4)), col=c(darks, darks), ncol=2, cex=0.4, legend=c('Omniscient unpermuted', 'BRT unpermuted', 'GAM unpermuted', 'Maxent unpermuted', 'Omniscient permuted', 'BRT permuted', 'GAM permuted', 'Maxent permuted'), bty='n', pt.cex=0.7)
		
			# }
		
		# dev.off()
			
	# ### COR - presences & BG
	# ########################
	
		# offset <- seq(-0.0315, 0.0315, length.out=4)
	
		# png(paste0(thisOutDir, '/!Importance - COR Presences & BG.png'), width=1000, height=1100, res=300)
		# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			# minVal <- -0.6
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# plot(1, 1, col='white', xlab='Prevalance', ylab=ylab, ylim=c(minVal, 1), xlim=c(0, 1))
			
			# ### permuted

			# for (thisVar in c('T1', 'F1')) {
				
				# mainVar <- paste0('corPresBgMulti_perm', thisVar)
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.005, 0.005)
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- ifelse(thisVar == 'T1', colLightT1, bg)
					# colDark <- ifelse(thisVar == 'T1', colDarkT1, colDarkF1)
					# pch <- algoPch[i]

					# # error bars
					# for (b11 in b11s) {
					
						# set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						# prev <- unique(set$prev)
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(c(prev, prev) + thisOffset, c(low, high), lty=ifelse(thisVar=='T1', 'solid', 'dotted'), col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					# set <- set[order(set$prev), ]
					
					# # lines(set$prev + thisOffset, set[ , mainVar], col=colDark, lty='solid')
					# points(set$prev + thisOffset, set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				# }
				
			# } # next variable
			
			# legend('bottomleft', inset=0.01, xpd=NA, pch=algoPch, pt.bg=rep(c(colLightT1, bg), each=4), col=rep(c(colDarkT1, colDarkF1), each=4), ncol=2, cex=0.4, legend=c(paste(niceAlgos, 'TRUE'), paste(niceAlgos, 'FALSE')), bty='n', pt.cex=0.7)
	
		# dev.off()
			
	# ### COR - stratified BG
	# #######################
	
		# offset <- seq(-0.0315, 0.0315, length.out=4)
	
		# png(paste0(thisOutDir, '/!Importance - COR Stratified BG.png'), width=1000, height=1100, res=300)
		# par(mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			# minVal <- -0.6
			# ylab <- 'COR\n\U2190 more important    less important\U2192'
			
			# plot(1, 1, col='white', xlab='Prevalance', ylab=ylab, ylim=c(minVal, 1), xlim=c(0, 1))
			
			# ### permuted

			# for (thisVar in c('T1', 'F1')) {
				
				# mainVar <- paste0('corStratBgMulti_perm', thisVar)
				
				# # by ALGORITHM
				# for (i in seq_along(allAlgos)) {
			
					# thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.005, 0.005)
					# thisAlgo <- allAlgos[i]
					# niceAlgo <- niceAlgos[i]
					# colLight <- ifelse(thisVar == 'T1', colLightT1, bg)
					# colDark <- ifelse(thisVar == 'T1', colDarkT1, colDarkF1)
					# pch <- algoPch[i]

					# # error bars
					# for (b11 in b11s) {
					
						# set <- results[results$algo == thisAlgo & results$b11 == b11, ]
						# prev <- unique(set$prev)
						# vals <- set[ , mainVar]
						# low <- quantile(vals, 0.025, na.rm=TRUE)
						# high <- quantile(vals, 0.975, na.rm=TRUE)
						# n <- sum(!is.na(vals))
						# lines(c(prev, prev) + thisOffset, c(low, high), lty=ifelse(thisVar=='T1', 'solid', 'dotted'), col=colDark)
					
					# }
					
					# # trendlines
					# set <- results[results$algo == thisAlgo, ]
					# set$algo <- set$circle <- set$response <- NULL
					# set <- aggregate(set, by=list(set$b11), FUN=median, na.rm=TRUE)
					# set <- set[order(set$prev), ]
					
					# # lines(set$prev + thisOffset, set[ , mainVar], col=colDark, lty='solid')
					# points(set$prev + thisOffset, set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				# }
				
			# } # next variable
			
			# legend('bottomleft', inset=0.01, xpd=NA, pch=algoPch, pt.bg=rep(c(colLightT1, bg), each=4), col=rep(c(colDarkT1, colDarkF1), each=4), ncol=2, cex=0.4, legend=c(paste(niceAlgos, 'TRUE'), paste(niceAlgos, 'FALSE')), bty='n', pt.cex=0.7)
	
		# dev.off()
		
say('##############')
say('### extent ###')
say('##############')

	thisOutDir <- './Results/extent'

	# landSize <- data.frame(landSize=c(125, 251, 501, 1001, 2001, 4001), min=-1 * c(0.125, 0.25, 0.5, 1, 2, 4), max=c(0.125, 0.25, 0.5, 1, 2, 4))
	
	# results <- data.frame()
	# for (thisAlgo in allAlgos) {
	
		# for (i in 1:nrow(landSize)) {

			# thisResults <- readRDS(paste0(thisOutDir, '/evaluations/Evaluations for multivariate univariate ', toupper(thisAlgo), ' landscape size = ', prefix(landSize$landSize[i], 4), ' cells.rds'))
		
			# results <- if (nrow(results) == 0) {
				# thisResults
			# } else {
				# merge(results, thisResults, all=TRUE)
			# }
			
		# }
		
	# }
	
	# results <- results[orderAlgos(results$algo), ]

	# # ### in plots define environmental range with respect to amount of change in T1 to produce change from 0.025 to 0.975 probability of occurrence
	
	# # define species
	# b0 <- 0 # intercept
	# b1 <- 2 # slope of P1
	# b2 <- 1 # slope of P2
	# b11 <- 0 # shift parameter... offset of inflection from 0 on landscape relative to T1
	# b12 <- 0 # slope of T1 * T2
	# mu1 <- mu2 <- sigma1 <- sigma2 <- rho <- NA
	
	# # value of x1 that gives 0.025 probability of occurrence
	# base <- 0.025
	# tol <- base * 10E-5
	# x1 <- 1
	# diff <- Inf
	# while (diff > tol) {
	
		# newPrOcc <- logisticShift(x1=x1, x2=0, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12)
		# diff <- abs(base - newPrOcc)
		# x1 <- x1 - 10E-6
	
	# }
	
	# lowX1 <- x1
	
	# # value of x1 that gives 0.975 probability of occurrence
	# base <- 0.975
	# tol <- base * 10E-5
	# x1 <- -1
	# diff <- Inf
	# while (diff > tol) {
	
		# newPrOcc <- logisticShift(x1=x1, x2=0, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12)
		# diff <- abs(base - newPrOcc)
		# x1 <- x1 + 10E-6
	
	# }
	
	# highX1 <- x1
	
	# rangeScale <- highX1 - lowX1
	# write.csv(paste0('Need change of ', rangeScale, ' in TRUE variable to change probability of occurrence from 0.025 to 0.975.'), paste0(thisOutDir, 'Change in TRUE variable needed to observe 95% change in Pr(occ).'), row.names=FALSE)
	
	# results$rangeT1 <- results$maxT1 - results$minT1
	
	# saveRDS(results, paste0(thisOutDir, '/evaluations/!All Evaluations Collated.rds'))
	
	results <- readRDS(paste0(thisOutDir, '/evaluations/!All Evaluations Collated.rds'))
	
	### CBI - multivariate vs permuted
	##################################
	
		offset <- seq(-0.0315, 0.0315, length.out=4)
		
		ranges <- sort(unique(results$rangeT1))
	
		png(paste0(thisOutDir, '/!Importance - CBI Multivariate vs Permuted.png'), width=2000, height=1100, res=300)
		par(mfrow=c(1, 2), mgp=c(1.5, 0.5, 0), tck=-0.03, cex.main=0.5, cex.lab=0.7, cex.axis=0.6, mar=c(5, 3, 4, 1))
	
			# by VARIABLE
			for (thisVar in c('T1', 'F1')) {
		
				xlim <- c(-0.8, 1.1)
				minVal <- -1.17
				ylab <- 'CBI\n\U2190 more important    less important\U2192'
				
				lab <- if (thisVar == 'T1') {
					'a) Permutation importance for TRUE variable'
				} else {
					'b) Permutation importance for FALSE variable'
				}
	
				plot(1, 1, col='white', xlab='Range of TRUE Variable (log10)', ylab=ylab, ylim=c(minVal, 1), xlim=xlim)
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
					for (thisRange in ranges) {
					
						set <- results[results$algo == thisAlgo & results$rangeT1 == thisRange, ]
						vals <- set[ , mainVar]
						low <- quantile(vals, 0.025, na.rm=TRUE)
						high <- quantile(vals, 0.975, na.rm=TRUE)
						n <- sum(!is.na(vals))
						lines(log10(c(thisRange, thisRange)) + offset[i], c(low, high), lty='solid', col=colDark)
					
					}
					
					# trendlines
					set <- results[results$algo == thisAlgo, ]
					set$algo <- set$circle <- set$response <- NULL
					set <- aggregate(set, by=list(set$rangeT1), FUN=median, na.rm=TRUE)
					set <- set[order(set$rangeT1), ]
					
					# lines(log10(set$rangeT1) + offset[i], set[ , mainVar], col=colDark)
					points(log10(set$rangeT1) + offset[i], set[ , mainVar], col=colDark, bg=colLight, pch=pch)
					
				}
				
				### permuted

				offset <- offset + 0.007
				mainVar <- paste0('cbiMulti_perm', thisVar)
				
				# by ALGORITHM
				for (i in seq_along(allAlgos)) {
			
					thisAlgo <- allAlgos[i]
					niceAlgo <- niceAlgos[i]
					colLight <- lights[i]
					colDark <- darks[i]
					pch <- algoPch[i]

					# error bars
					for (thisRange in ranges) {
					
						set <- results[results$algo == thisAlgo & results$rangeT1 == thisRange, ]
						vals <- set[ , mainVar]
						low <- quantile(vals, 0.025, na.rm=TRUE)
						high <- quantile(vals, 0.975, na.rm=TRUE)
						n <- sum(!is.na(vals))
						lines(log10(c(thisRange, thisRange)) + offset[i], c(low, high), lty='dotted', col=colDark)
					
					}
					
					# trendlines
					set <- results[results$algo == thisAlgo, ]
					set$algo <- set$circle <- set$response <- NULL
					set <- aggregate(set, by=list(set$rangeT1), FUN=median, na.rm=TRUE)
					set <- set[order(set$rangeT1), ]
					
					# lines(log10(set$rangeT1) + offset[i], set[ , mainVar], col=colDark, lty='solid')
					points(log10(set$rangeT1) + offset[i], set[ , mainVar], col=colDark, bg=bg, pch=pch)
					
				}
				
				legend('bottomleft', inset=0.01, xpd=NA, pch=c(algoPch, algoPch), pt.bg=c(lights, rep(bg, 4)), col=c(darks, darks), ncol=2, cex=0.4, legend=c('Omniscient unpermuted', 'BRT unpermuted', 'GAM unpermuted', 'Maxent unpermuted', 'Omniscient permuted', 'BRT permuted', 'GAM permuted', 'Maxent permuted'), bty='n', pt.cex=0.7)
		
			}
		
		dev.off()
			
	print(NON)		
			
	### COR - presences & BG
	########################
	
		offset <- seq(-0.0315, 0.0315, length.out=4)
	
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
			
					thisOffset <- offset[i] + ifelse(thisVar == 'T1', -0.005, 0.005)
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
			




# say('#####################################################################')
# say('### bivariate -- NO landscape correlation and NO niche covariance ###')
# say('#####################################################################')






		
say('DONE!!!!!', level=1)

