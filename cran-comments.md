## Resubmission
This is a resubmission. In this version I have:

* Removed redundant parts from title and description text.

* Added references to research papers in the description field of the DESCRIPTION file.

* Added \value to spa_set_classification.Rd to clearly indicate that this function does not return a value.

* Wrapped code lines in examples in fsi_qw_eval.Rd and fsr_topological_relationships.Rd by using \dontrun (they contain lengthy examples - >5 sec)

## Test environments
* Ubuntu 18.04.5 LTS (bionic), R 4.1.0
* POP OS 21.04 (hirsute), R 4.0.4
* win-builder (devel, release, and oldrelease)
* R-hub (Platforms:	Windows Server 2008 R2 SP1, R-devel, 32/64 bit; and Ubuntu Linux 20.04.1 LTS, R-release, GCC)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

A NOTE was obtained by the `devtools::check_win_devel`, which relates to this file (`cran-comments`) in the directory of the package. However, this file is included in .Rbuildignore.

## Downstream dependencies
There are currently no downstream dependencies for this package.