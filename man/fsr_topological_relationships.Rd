% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzy_topological_relations.R
\name{fsr_topological_relationships}
\alias{fsr_topological_relationships}
\alias{spa_overlap}
\alias{spa_meet}
\alias{spa_disjoint}
\alias{spa_equal}
\alias{spa_inside}
\alias{spa_contains}
\title{Compute fuzzy topological relationships}
\usage{
spa_overlap(pgo1, pgo2, itype = "min", ret = "degree", ...)

spa_meet(pgo1, pgo2, itype = "min", ret = "degree", ...)

spa_disjoint(pgo1, pgo2, itype = "min", ret = "degree", ...)

spa_equal(pgo1, pgo2, utype = "max", ret = "degree", ...)

spa_inside(pgo1, pgo2, utype = "max", ret = "degree", ...)

spa_contains(pgo1, pgo2, utype = "max", ret = "degree", ...)
}
\arguments{
\item{pgo1}{A \code{pregion} object.}

\item{pgo2}{A \code{pregion} object.}

\item{itype}{A character value that indicates the name of a function implementing a t-norm. The default value is \code{"min"}, which is the standard operator of the intersection.}

\item{ret}{A character value that indicates the return type of the fuzzy topological relationship. The default value is \code{"degree"} and other possible values are \code{"list"} and \code{"bool"}.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> If \code{ret = "bool"}, two additional parameters have to be informed, as described below.}

\item{utype}{A character value that indicates the name of a function implementing a t-conorm. The default value is \code{"max"}, which is the standard operator of the union.}
}
\value{
The returning value is determined by the parameter \code{ret}, as described above.
}
\description{
Fuzzy topological relationships are implemented by spatial plateau topological relationships.
A fuzzy topological relationship expresses a particular relative position of two spatial plateau objects.
Such a topological relationship determines the degree to which it holds for any two spatial plateau objects by a real value in the interval [0, 1].
}
\details{
These functions implement the spatial plateau topological relationships between plateau region objects.
The key idea of these relationships is to consider point subsets resulting from the combination of spatial plateau
set operations and spatial plateau metric operations on spatial plateau objects for computing the resulting degree.
The resulting degree can be also interpreted as a linguistic value.

The spatial plateau topological relationships are implemented by the following functions:
\itemize{
\item \code{spa_overlap()} computes the overlapping degree of two plateau region objects.
Since it uses the intersection operation, a t-norm operator can be given by the parameter \code{itype}. Currently, it can assume \code{"min"} (default) or \code{"prod"}.
\item \code{spa_meet()} computes the meeting degree of two plateau region objects.
Similarly to \code{spa_overlap}, a t-norm operator can be given by the parameter \code{itype}.
\item \code{spa_disjoint()} computes the disjointedness degree of two plateau region objects.
Similarly to \code{spa_overlap} and \code{spa_meet}, a t-norm operator can be given by the parameter \code{itype}.
\item \code{spa_equal()} computes how equal are two plateau region objects.
Since it uses the union operation, a t-conorm operator can be given by the parameter \code{utype}. Currently, it can assume \code{"max"} (default).
\item \code{spa_inside()} computes the containment degree of \code{pgo1} in \code{pgo2}.
Similarly to \code{spa_equal()}, a t-conorm operator can be given by the parameter \code{utype}.
\item \code{spa_contains()} changes the order of the operations \code{pgo1} ad \code{pgo2} when invoking \code{spa_inside()}.
}

The parameter \code{ret} determines the returning value of a fuzzy topological relationship.
The default value is \code{"degree"} (default), which indicates that the function will return a value in [0, 1] that represents the degree of truth of a given topological relationship.

For the remainder possible values, the functions make use of a set of linguistic values that characterize the different situations of topological relationships.
Each linguistic value has an associated membership function defined in the domain [0, 1].
The \code{fsr} package has a default set of linguistic values. You can use the function \code{spa_set_classification()} to change this set of linguistic values.

The remainder possible values for the parameter \code{ret} are:
\itemize{
\item \code{ret = "list"} indicates that the function will return a named list containing the membership degree of the result of the predicate for each linguistic value (i.e., it employs the membership functions of the linguistic values).
\item \code{ret = "bool"} indicates that the function will return a Boolean value indicating whether the degree returned by the topological relationship matches a given linguistic value according to an \emph{evaluation mode}.
The evaluation mode and the linguistic values have to be informed by using the parameters \code{eval_mode} and \code{lval}, respectively.
The possible values for \code{eval_mode} are: \code{"soft_eval"}, \code{"strict_eval"}, \code{"alpha_eval"}, and \code{"soft_alpha_eval"}.
They have different behavior in how computing the Boolean value from the membership function of a linguistic value.
See the documentation of the functions \code{soft_eval()}, \code{strict_eval()}, \code{alpha_eval()}, and \code{soft_alpha_eval()} for more details.
Note that the parameter \code{lval} only accept a character value belonging to the set of linguistic values that characterize the different situations of topological relationships.
}
}
\examples{
library(tibble)
library(sf)

set.seed(456)

# Generating some random points to create pgeometry objects by using spa_creator()
tbl = tibble(x = runif(10, min= 0, max = 30), 
             y = runif(10, min = 0, max = 30), 
             z = runif(10, min = 0, max = 50))

# Getting the convex hull on the points to clip plateau region objects during their constructions
pts <- st_as_sf(tbl, coords = c(1, 2))
ch <- st_convex_hull(do.call(c, st_geometry(pts)))

pregions <- spa_creator(tbl, base_poly = ch, fuzz_policy = "fcp", k = 2)

plot(pregions$pgeometry[[1]])
plot(pregions$pgeometry[[2]])

\dontrun{ 
# Showing the different types of returning values
spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]])
spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "bool", 
           eval_mode = "soft_eval", lval = "mostly")

## Examples for evaluating the other fuzzy topological relationships
spa_meet(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
spa_disjoint(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
spa_equal(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
spa_inside(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
spa_contains(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
}
}
\references{
\href{https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044}{Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.}

Underlying concepts and formal definitions of spatial plateau topological relationships and fuzzy topological relationships are respectively introduced in:
\itemize{
\item \href{https://ieeexplore.ieee.org/document/8491565}{Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.}
\item \href{https://ieeexplore.ieee.org/document/7737976}{Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.}
}
}
