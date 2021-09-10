## Test environments
* Ubuntu 18.04.5 LTS (bionic), R 4.1.0
* POP OS 21.04 (hirsute), R 4.0.4
* win-builder (devel and oldrelease)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTES in the Linux-based tested environments.

Some NOTES were obtained by the win-builder. Some of them are related to the execution of two costly examples (`fsi_qw_eval` and `fsr_topological_relationships`) that may consume more than 10s of execution. It does not affect the package functionality.

Another NOTE was obtained by the `devtools::check_win_release`, which relates to this file (`cran-comments`) in the directory of the package. However, this file is included in .Rbuildignore.

## Downstream dependencies
There are currently no downstream dependencies for this package.