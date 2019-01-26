#' Delete temporary directory
#'
#' Delete temporary directory.
#' @param tempDir Character, directory path to delete.
#' @param defaultTemp Character, default temporary directory (temp dir will be redirected here).
#' @value Nothing (deletes a directory).
#' @export
delTemp <- function(tempDir, defaultTempDir='C:/ecology/!Scratch') {

	Sys.sleep(1)
	rasterOptions(tmpdir=defaultTempDir)
	unlink(tempDir, recursive=TRUE, force=TRUE)

}
