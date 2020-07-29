# enmSdmPredImport_scenarios

The scripts in this repository recreate all of the analyses of Smith, A.B. and Santos, M.J. Accepted. Testing the ability of species distribution models to infer variable importance. Ecography. A pre-print of this manuscript is available for free at https://doi.org/10.1101/715904.

To run these scripts, you will need to have installed the following packages to create scenarios, and run and evaluate models: `sp`, `raster`, `gbm`, `dismo`, and `rJava`.

You will also need to install some other packages from GitHub using the following commands in R:  
`remotes::install_github('adamlilith/omnibus')`  
`remotes::install_github('adamlilith/statisfactory')`  
`remotes::install_github('adamlilith/legendary')`  
`remotes::install_github('adamlilith/enmSdm')`  
`remotes::install_github('adamlilith/enmSdmPredImport')` 

Note that if you have issues installing any of the latter packages (e.g., because of using an earlier/later version of R), you can download the ZIP file of the package from the `zipTarFiles` directory in each of these packages. You can then install manually in R from this file.

The analysis in Smith & Santos used Maxent version 3.3.3k. To recreate the output with fidelity (except for random seeds), you will need to copy a Java file into the "java" directory of the library of your `dismo` installation.  Version 3.3.3k seems not to be available anymore, but can be obtained by contacting Adam Smith (adam DOT smith AT mobot DOT org). You can also use the newer version available from https://biodiversityinformatics.amnh.org/open_source/maxent/.

Additional packages needed for creating figures in Smith & Santos include `RColorBrewer`,  `plotrix`, `fpCompare`, and `scales`.

Last updated 2020-07-29
Adam B. Smith
