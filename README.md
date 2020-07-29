# enmSdmPredImport_scenarios

The scripts in this repository recreate all of the analyses of Smith, A.B. and Santos, M.J. Accepted. Testing the ability of species distribution models to infer variable importance. Ecography. A pre-print of this manuscript is available for free at https://doi.org/10.1101/715904.

To run these scripts, you will need to have installed the following packages:

Creating scenarios and running models: `sp`, `raster`, `gbm`, `dismo`, and `rJava`.

To install from GitHub:
`remotes::install_github('adamlilith/omnibus')` 
`remotes::install_github('adamlilith/statisfactory')` 
`remotes::install_github('adamlilith/legendary')` 
`remotes::install_github('adamlilith/enmSdm')` 
`remotes::install_github('adamlilith/enmSdmPredImport')` 

The analysis in the publication used Maxent version 3.3.3k. To recreate the output with fidelity (except for random seeds), you will need to copy a java file into the "java" directory of the library of your `dismo` installation.  Version 3.3.3k seems not to be available, but can be obtained by contacting Adam Smith (adam DOT smith AT mobot DOT org). You can also use the newer version available from download https://biodiversityinformatics.amnh.org/open_source/maxent/.

Additional packages needed for creating figures: `RColorBrewer`,  `plotrix`, `fpCompare`, and `scales`.

Last updated 2020-07-29
Adam B. Smith
