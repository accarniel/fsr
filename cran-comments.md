## Test environments
* Ubuntu 18.04.5 LTS (bionic), R 4.1.0
* POP OS 21.04 (hirsute), R 4.0.4
* win-builder (devel and oldrelease)
* R-hub (Platforms:	Windows Server 2008 R2 SP1, R-devel, 32/64 bit; and Ubuntu Linux 20.04.1 LTS, R-release, GCC)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTES in the Linux-based local environments.

Some NOTES were obtained by the win-builder and R-hub when checking examples of `fsi_qw_eval` and/or `fsr_topological_relationships` since they may consume more than 5s (or 10s) of execution. These functions are complex and expensive in terms of processing time. Hence, it does not affect the package functionality.

Another NOTE was obtained by the `devtools::check_win_release`, which relates to this file (`cran-comments`) in the directory of the package. However, this file is included in .Rbuildignore.

## Downstream dependencies
There are currently no downstream dependencies for this package.