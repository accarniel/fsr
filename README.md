# _fsr_: Handling Fuzzy Spatial Data in R

[![CRAN](https://www.r-pkg.org/badges/version/fsr)](https://cran.r-project.org/package=fsr)
[![CRAN checks](https://badges.cranchecks.info/worst/fsr.svg)](https://cran.r-project.org/web/checks/check_results_fsr.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/fsr)](https://cran.r-project.org/package=fsr)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/fsr)](https://cran.r-project.org/package=fsr)

## Overview

Many spatial objects (i.e., geographical objects) in reality may have inexact locations, vague boundaries, and/or blurred interiors, and hence cannot be adequately represented by well-known (crisp) spatial objects (e.g., those objects available in spatial libraries like GEOS and GDAL). Fuzzy set theory and fuzzy logic are powerful approaches to deal with spatial fuzziness.

Unfortunately, spatial fuzziness has so far not found its way into *spatial data science projects* due to a lack of complete implementations of software packages that can handle fuzzy spatial objects. Motivated by this lack of support, the _fsr_ package has the following advantages:

- It is an implementation of the [fuzzy spatial data types, operations, and predicates](https://ieeexplore.ieee.org/document/7737976), including [heterogeneous fuzzy spatial data types](https://ieeexplore.ieee.org/document/9177620), based on the [Spatial Plateau Algebra (SPA)](https://ieeexplore.ieee.org/document/8491565).
- It offers the [construction of fuzzy spatial objects as spatial plateau objects](https://ieeexplore.ieee.org/document/8858878) from real spatial datasets.
- It has families of functions that permit users to conduct exploratory (spatial) data analysis by issuing geometric operations and topological predicates on fuzzy spatial objects.
- It allows the design and creation of [fuzzy spatial inference (FSI) models](https://ieeexplore.ieee.org/document/8015707) to discover new findings from fuzzy spatial objects.
- It provides evaluation methods to process different types of [spatial inference queries by using FSI models](https://ieeexplore.ieee.org/document/9882658).

## Citation

To cite _fsr_ in publications use:

- To refer to its second version:
  - [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://doi.org/10.1111/tgis.13044)

- To refer to its first version and the implementation of fuzzy spatial inference models:
  - [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (ACM SIGSPATIAL 2021), pp. 526-535, 2021.](https://doi.org/10.1145/3474717.3484255) 

## Installing

Install the released version of fsr from CRAN with:

```r
install.packages("fsr")
```

Install the development version from GitHub with:

```r
# install.packages("devtools")
library(devtools)
install_github("accarniel/fsr")
```

## Contributing and Contact

The project welcomes contributions. You can contribute to _fsr_ by making Issues or Pull Requests on this GitHub Repository. Alternatively, you can contact Anderson Carniel. When a Pull Request or email is sent, Anderson Carniel will check it as soon as possible.