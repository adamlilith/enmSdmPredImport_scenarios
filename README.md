# enmSdmPredImport_scenarios

The scripts in this repository recreate all of the analyses of Smith, A.B. and Santos, M.J. 2020. Testing the ability of species distribution models to infer variable importance. *Ecography* 43:1801-1813.

To run these scripts, you will need to have installed the following packages to create scenarios, and run and evaluate models: `sp`, `raster`, `gbm`, `dismo`, and `rJava`.

You will also need to install some other packages from GitHub using the following commands in R:  
`remotes::install_github('adamlilith/omnibus')`  
`remotes::install_github('adamlilith/statisfactory')`  
`remotes::install_github('adamlilith/legendary')`  
`remotes::install_github('adamlilith/enmSdm')`  
`remotes::install_github('adamlilith/enmSdmPredImport')` 

Note that if you have issues installing any of the latter packages (e.g., because of using an earlier/later version of R), you can download the ZIP file of the package from the `zipTarFiles` directory in each of these packages. You can then install manually in R from this file.

The analysis in Smith & Santos (2020) used Maxent version 3.3.3k. To recreate the output with fidelity (except for random seeds), you will need to copy a Java file into the "java" directory of the library of your `dismo` installation.  Version 3.3.3k seems not to be available anymore, but can be obtained by contacting Adam Smith (adam DOT smith AT mobot DOT org). You can also use the newer version available from https://biodiversityinformatics.amnh.org/open_source/maxent/.

Additional packages needed for creating figures in Smith & Santos include `RColorBrewer`,  `plotrix`, `fpCompare`, and `scales`.

## Citation ##

This package was first used in [Smith, A.B. and Santos, M.J. 2020.](https://doi.org/10.1111/ecog.05317) Testing the ability of species distribution models to infer variable importance. *Ecography* 43:1801-1813.

*Abstract*: Models of species’ distributions and niches are frequently used to infer the importance of range‐ and niche‐defining variables. However, the degree to which these models can reliably identify important variables and quantify their influence remains unknown. Here we use a series of simulations to explore how well models can 1) discriminate between variables with different influence and 2) calibrate the magnitude of influence relative to an ‘omniscient’ model. To quantify variable importance, we trained generalized additive models (GAMs), Maxent and boosted regression trees (BRTs) on simulated data and tested their sensitivity to permutations in each predictor. Importance was inferred by calculating the correlation between permuted and unpermuted predictions, and by comparing predictive accuracy of permuted and unpermuted predictions using AUC and the continuous Boyce index. In scenarios with one influential and one uninfluential variable, models failed to discriminate reliably between variables when training occurrences were < 8–64, prevalence was > 0.5, spatial extent was small, environmental data had coarse resolution and spatial autocorrelation was low, or when pairwise correlation between environmental variables was |r| > 0.7. When two variables influenced the distribution equally, importance was underestimated when species had narrow or intermediate niche breadth. Interactions between variables in how they shaped the niche did not affect inferences about their importance. When variables acted unequally, the effect of the stronger variable was overestimated. GAMs and Maxent discriminated between variables more reliably than BRTs, but no algorithm was consistently well‐calibrated vis‐à‐vis the omniscient model. Algorithm‐specific measures of importance like Maxent's change‐in‐gain metric were less robust than the permutation test. Overall, high predictive accuracy did not connote robust inferential capacity. As a result, requirements for reliably measuring variable importance are likely more stringent than for creating models with high predictive accuracy.

Last updated 2020-12-02
Adam B. Smith
