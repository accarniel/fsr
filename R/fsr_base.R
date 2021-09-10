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

#' @title The PWKT of a spatial plateau object
#'
#' @description This function gives the Plateau Well-Known Text (PWKT) representation of a `pgeometry` object. 
#'
#' @usage
#'
#' spa_pwkt(pgo)
#'
#' @param pgo A `pgeometry` object of any type.
#' 
#' @name PWKT
#'
#' @details
#'
#' It gives the textual representation for a `pgeometry` object, 
#' combining the Well-Known Text (WKT) representation for crisp vector geometry
#' objects and the formal definitions of the tree spatial plateau data types.
#' (i.e. `PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`).
#'
#' @return
#'
#' A character value with the textual representation of a given `pgeometry` object.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' library(sf)
#'
#' # For a `PLATEAUPOINT` object.
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- component_from_sfg(st_multipoint(pts1), 0.2) 
#' comp2 <- component_from_sfg(st_point(c(1, 5)), 0.8)  
#' 
#' ppoint <- create_pgeometry(list(comp1, comp2), "PLATEAUPOINT")
#' 
#' spa_pwkt(ppoint)
#' 
#' # For a `PLATEAULINE` object.
#' 
#' lpts1 <- rbind(c(0, 0), c(1, 1))
#' lpts2 <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lpts3 <- rbind(c(2, 1), c(1.5, 0.5))
#'
#' comp4 <- component_from_sfg(st_linestring(lpts1), 0.4)
#' comp5 <- component_from_sfg(st_linestring(lpts2), 1)
#' comp6 <- component_from_sfg(st_linestring(lpts3), 0.7)
#'
#' pline <- create_pgeometry(list(comp4, comp5, comp6), "PLATEAULINE")
#' 
#' spa_pwkt(pline)
#'
#' # For a `PLATEAUREGION` object.
#' 
#' p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' pol1 <-st_polygon(list(p1,p2))
#' 
#' comp1 <- component_from_sfg(pol1, 0.2)
#' 
#' pregion <- create_pgeometry(list(comp1), "PLATEAUREGION")
#' 
#' spa_pwkt(pregion)
#' 
#' 
#' @import sf
#' @export
spa_pwkt <- function(pgo) {

  if(fsr_is_empty(pgo)){
    return(paste0(pgo@type, " EMPTY"))
  }

  component_to_text <- function(comp) {
    paste0("(", st_as_text(comp@obj), ", ", comp@md, ")")
  }

  l <- unlist(lapply(pgo@component, component_to_text))

  l <- paste(pgo@type," (", paste(l, collapse = ", "), ")", sep="")
  l
}


#' @name PWKT
#' @param x A `pgeometry` object of any type.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#' @export
format.pgeometry <- function(x, ...) {
  spa_pwkt(x)
}

#' @name PWKT
#' 
#' @param object A `pgeometry` object of any type.
#' @aliases show,pgeometry-method
#' 
#' @import methods
#' @export
setMethod("show", "pgeometry", function(object) {
  print(spa_pwkt(object))
})

#' @name PWKT
#' 
#' @param x A `pgeometry` object of any type.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#' 
#' @aliases as.character,pgeometry-method
#' 
#' @import methods
#' @export
setMethod("as.character", "pgeometry", function(x, ...) {
  spa_pwkt(x)
})


#' @name plot
#' 
#' @param x A `pgeometry` object of any type.
#' @param y Not applicable.
#' 
#' @aliases plot,pgeometry,missing-method
#'   
#' @import methods
#' @export
setMethod("plot", signature(x = "pgeometry", y = "missing"), function(x, y, ...) {
  fsr_plot(x, ...)
})

#' @method as.data.frame pgeometry
#' @name as_tibble.pgeometry
#' @export
as.data.frame.pgeometry <-
  function(x, ...)
  {
    as.data.frame(as_tibble(x))
  }