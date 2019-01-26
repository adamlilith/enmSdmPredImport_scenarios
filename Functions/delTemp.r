### functions to make and delete temporary directories ###
delTemp <- function(tempDir) {

	Sys.sleep(1)
	rasterOptions(tmpdir='C:/ecology/!Scratch')
	unlink(tempDir, recursive=TRUE, force=TRUE)

}
