# _fsr_ Package: Handling fuzzy spatial data in R

Current release: 1.0.0

A research paper describing the first version of _fsr_ has been accepted to publication in [ACM SIGSPATIAL International Conference on Advances in Geographic Information Systems 2021 (ACM SIGSPATIAL 2021)](https://sigspatial2021.sigspatial.org/accepted-papers/).

## About _fsr_

Many spatial objects (i.e., geographical objects) in reality may have inexact locations, vague boundaries, and/or blurred interiors, and hence cannot be adequately represented by well-known (crisp) spatial objects (e.g., those objects available in spatial libraries like GEOS and GDAL). Fuzzy set theory and fuzzy logic are powerful approaches to deal with spatial fuzziness.

Unfortunately, spatial fuzziness has so far not found its way into *spatial data science projects* due to a lack of complete implementations of software packages that can handle fuzzy spatial objects. Motivated by this lack of support, the _fsr_ package has the following advantages:

- It is an implementation of the [fuzzy spatial data types, operations, and predicates](https://ieeexplore.ieee.org/document/7737976) of the [Spatial Plateau Algebra (SPA)](https://ieeexplore.ieee.org/document/8491565).
- It offers the [construction of fuzzy spatial objects as spatial plateau objects](https://ieeexplore.ieee.org/document/8858878) from real point datasets.
- It has families of functions that permit users to conduct exploratory (spatial) data analysis by issuing geometric operations and topological predicates on fuzzy spatial objects.
- It provides methods for designing [fuzzy spatial inference models](https://ieeexplore.ieee.org/document/8015707) to discover new findings from fuzzy spatial objects.
- It deploys algorithms to determine and only evaluate a subset of points that really contributes to finding the final but approximated answer to a user’s query window inference.

## Installing

Install the development version from GitHub with

```r
library(devtools)
install_github("accarniel/fsr")
```

## Contributing and Contact

The project welcomes contributions from three different perspectives. You can contribute to _fsr_ by making Pull Requests on this GitHub Repository. Alternatively, contact Anderson Carniel by sending an email to accarniel@ufscar.br. When a Pull Request or email is sent, Anderson Carniel will check it as soon as possible.
