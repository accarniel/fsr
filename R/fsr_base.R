#' An S4 Class for representing a component of a spatial plateau object
#'
#' @slot obj A sf data type
#' @slot md A membership degree of the component
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
#' @slot component A list of components
#' @slot supp An sfg object that stores the union of spatial objects of the components of the spatial plateau object
#' @slot type The data type of the spatial plateau object
#' 
#' @import methods
#' @export
setClass("pgeom",
         slots = list(
           component = "list",
           supp = "XY",
           type = "character"
         )
)

#' @title pgeom_to_pwkt
#'
#' @description pgeom_to_pwkt gives the Well-Known Text representation of a pgeom
#' object. 
#'
#' @usage
#'
#' pgeom_to_pwkt(pgeom)
#'
#' @param pgeom A `pgeom` object of any type.
#' 
#' @name PWKT
#'
#' @details
#'
#' It gives the textual representation for a `pgeom` object, 
#' combining the Well-Known Text (WKT) representation for crisp vector geometry
#' objects and the formal definitions of the tree spatial plateau data types.
#' (i.e. `PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`).
#'
#' @return
#'
#' A character value with the textual representation of a given `pgeom` object.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' library(sf)
#'
#' # For a `PLATEAUPOINT` pgeom object.
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- component_from_sfg(st_multipoint(pts1), 0.2) 
#' comp2 <- component_from_sfg(st_point(c(1, 5)), 0.8)  
#' 
#' plateau_point_pgeom <- create_pgeom(list(comp1, comp2), "PLATEAUPOINT")
#' 
#' pgeom_to_pwkt(plateau_point_pgeom)
#' 
#' # For a `PLATEAULINE` pgeom object.
#' 
#' lpts1 <- rbind(c(0, 0), c(1, 1))
#' lpts2 <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lpts3 <- rbind(c(2, 1), c(1.5, 0.5))
#'
#' comp4 <- component_from_sfg(st_linestring(lpts1), 0.4)
#' comp5 <- component_from_sfg(st_linestring(lpts2), 1)
#' comp6 <- component_from_sfg(st_linestring(lpts3), 0.7)
#'
#' plateau_line_pgeom<- create_pgeom(list(comp4, comp5, comp6), "PLATEAULINE")
#' 
#' pgeom_to_pwkt(plateau_line_pgeom)
#'
#' # For a `PLATEAUREGION` pgeom object.
#' 
#' p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' pol1 <-st_polygon(list(p1,p2))
#' 
#' comp1 <- component_from_sfg(pol1, 0.2)
#' 
#' plateau_region_pgeom <- create_pgeom(list(comp1), "PLATEAUREGION")
#' 
#' pgeom_to_pwkt(plateau_region_pgeom)
#' 
#' 
#' @import sf
#' @export
pgeom_to_pwkt <- function(pgeom) {

  if(pgeom_is_empty(pgeom)){
    return(paste0(pgeom@type, " EMPTY"))
  }

  component_to_text <- function(comp) {
    paste0("(", st_as_text(comp@obj), ", ", comp@md, ")")
  }

  l <- unlist(lapply(pgeom@component, component_to_text))

  l <- paste(pgeom@type," (", paste(l, collapse = ", "), ")", sep="")
  l
}


#' @name PWKT
#' @param x A `pgeom` object of any type.
#' @param ... unused
#' @export
format.pgeom <- function(x, ...) {
  pgeom_to_pwkt(x)
}

#' @name PWKT
#' 
#' @param object A `pgeom` object of any type.
#' @aliases show,pgeom-method
#' 
#' @import methods
#' @export
setMethod("show", "pgeom", function(object) {
  print(pgeom_to_pwkt(object))
})

#' @name PWKT
#' 
#' @param x A `pgeom` object of any type.
#' @param ... unused
#' 
#' @aliases as.character,pgeom-method
#' 
#' @import methods
#' @export
setMethod("as.character", "pgeom", function(x, ...) {
  pgeom_to_pwkt(x)
})


#' @name pgeom_plot
#' 
#' @param x A `pgeom` object of any type.
#' @param y Not applicable.
#' 
#' @aliases plot,pgeom,missing-method
#'   
#' @import methods
#' @export
setMethod("plot", signature(x = "pgeom", y = "missing"), function(x, y, ...) {
  pgeom_plot(x, ...)
})

#' @export
as.data.frame.pgeom <-
  function(x, row.names=NULL, optional=FALSE, ...)
  {
    as.data.frame(pgeom_as_tibble(x))
  }