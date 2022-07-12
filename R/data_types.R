#' An S4 Class for representing a component of a spatial plateau object
#'
#' @slot obj An `sfg` object.
#' @slot md The membership degree of the component.
#' 
#' @details 
#' 
#' A `component` object is composed of two attributes. The first one is a crisp spatial
#' object and the second one a membership degree in \]0, 1\] of this  `component`.
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' 
#' @import methods sf
#' @export
setClass("component",
         slots = list(
           obj = "XY",
           md = "numeric"
         )
)

#' An S4 Class for representing a spatial plateau object
#'
#' @slot component A list of components.
#' @slot supp An `sfg` object that stores the union of the spatial objects of the components of the spatial plateau object.
#' @slot type The data type of the spatial plateau object.
#' 
#' @details 
#' 
#' A `pgeometry` object is composed of a list of `component` objects, an `sfg` object that represents 
#' the union of all crisp spatial objects of its components (i.e., the support), and its data
#' type, which can be either `PLATEAUPOINT`, `PLATEAULINE`, or `PLATEAUREGION`.
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' 
#' @import methods
#' @export
setClass("pgeometry",
         slots = list(
           component = "list",
           supp = "XY",
           type = "character"
         )
)