### functions to make and delete temporary directories ###
makeTemp <- function() {

	tempDir <- paste0('C:/ecology/!Scratch/_scratchDir_dontDelete/_temp', round(10^7 * runif(1)))
	dir.create(path=tempDir, showWarnings=F, recursive=T)
	rasterOptions(tmpdir=tempDir)
	tempDir
	
}

