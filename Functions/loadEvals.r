loadEvals <- function(outDir, algos=c('omniscient', 'brt', 'gam', 'maxent')) {

	# loads evaluation files and compiles them into a single data frame
	
	for (algo in algos) {
		say(algo)
		files <- listFiles(paste0(outDir, '/evaluations'), pattern=toupper(algo))
		algoPerform <- data.frame()
		
		for (file in files) {
			say(file)
			load(file)
			if (!exists('perform', inherits=FALSE)) stop('File does not contain a variable named "perform."')
			if (any(!(perform$algo %in% algo))) stop(paste('Algorithm listed in perform data frame is not the intended algorithm (data frame is', perform$algo[1], 'but intended algorithm is', algo, '.'))
			algoPerform <- rbind(algoPerform, perform)
		}
		
		masterPerform <- if (exists('masterPerform', inherits=FALSE)) {
			merge(masterPerform, algoPerform, all=TRUE)
		} else {
			algoPerform
		}
		
	}

	masterPerform <- masterPerform[orderAlgos(masterPerform$algo), ]
	masterPerform
	
}
