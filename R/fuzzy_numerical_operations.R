#' @title Compute fuzzy numerical operations
#'
#' @description Fuzzy numerical operations are implemented by spatial plateau numerical operations, which 
#' extract geometric measurements from spatial plateau objects, 
#' such as the area of a plateau region object and the length of a plateau line object.
#'
#' @usage
#'
#' spa_avg_degree(pgo)
#'
#' @param pgo A `pgeometry` object of any type.
#' 
#' @name fsr_numerical_operations
#'
#' @details
#'
#' These functions calculate numerical properties from spatial plateau objects (i.e., `pgeometry` objects). 
#' Some of them are _type-independent_. This means that the parameter can be a `pgeometry` object of any type. 
#' The type-independent functions are:
#' 
#' - `spa_avg_degree()` calculates the average membership degree of a spatial plateau object.
#' - `spa_ncomp()` returns the number of components of a spatial plateau object.
#' 
#' The remaining functions are _type-dependent_. This means that the parameter have to be of a specific type.
#' The type-dependent functions are:
#' 
#' - `spa_area()` computes the area of a plateau region, plateau composition, or plateau collection object.
#' - `spa_perimeter()` computes the perimeter of a plateau region, plateau composition, or plateau collection.
#' - `spa_length()` computes the length of a plateau line, plateau composition, or plateau collection object.
#' 
#' For the aforementioned functions, if the input has the incorrect data type, it throws a warning message and returns 0.
#' 
#' @return
#'
#' A numerical value.
#'
#' @references
#' 
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' Underlying concepts and formal definitions of some spatial plateau numerical operations are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' # Point components
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' pcp4 <- create_component("MULTIPOINT((1 2), (2 1), (3 2))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0.5), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3.5))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (4 2))", 0.4)
#' # Line components
#' lcp1 <- create_component("LINESTRING(0 0, 1 1.5)", 0.2)
#' lcp2 <- create_component("LINESTRING(1 3, 1 2, 2 0.5)", 0.5)
#' lcp3 <- create_component("LINESTRING(2 1.2, 3 1.6, 4 4)", 0.7)
#' lcp4 <- create_component("LINESTRING(1 1.5, 2 1.2)", 1.0)
#' lcp5 <- create_component("LINESTRING(-1 1, 2 2)", 0.9)
#' # Polygon components
#' rcp1 <- create_component("POLYGON((0 0, 1 4, 2 2, 0 0))", 0.4)
#' rcp2 <- create_component("POLYGON((2 0.5, 4 1, 4 0, 2 0.5))", 0.8)
#' 
#' # Creating spatial plateau objects
#' ppoint <- create_pgeometry(list(pcp1, pcp2, pcp3, pcp4, pcp5), "PLATEAUPOINT")
#' pline <- create_pgeometry(list(lcp1, lcp2, lcp3), "PLATEAULINE")
#' pregion <- create_pgeometry(list(rcp1, rcp2), "PLATEAUREGION")
#' pcomp <- create_pgeometry(list(pcp6, pcp7, lcp4, lcp5), "PLATEAUCOMPOSITION")
#' pcol <- create_pgeometry(list(ppoint, pline, pregion, pcomp), "PLATEAUCOLLECTION")
#' 
#' spa_avg_degree(ppoint)
#' spa_avg_degree(pline)
#' spa_avg_degree(pregion)
#' spa_avg_degree(pcomp)
#' spa_avg_degree(pcol)
#' 
#' spa_ncomp(ppoint)
#' spa_ncomp(pline)
#' spa_ncomp(pregion)
#' spa_ncomp(pcomp)
#' spa_ncomp(pcol)
#' 
#' spa_area(pregion)
#' spa_area(pcomp)
#' spa_area(pcol)
#' 
#' spa_perimeter(pregion)
#' spa_perimeter(pcomp)
#' spa_perimeter(pcol)
#' 
#' spa_length(pline)
#' spa_length(pcomp)
#' spa_length(pcol)
#' @export
spa_avg_degree <- function(pgo) {
  type <- spa_get_type(pgo)
  if(!spa_is_empty(pgo)) {
    if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")) {
      mean(sapply(pgo@component, attr, "md"))
    } else if(type == "PLATEAUCOMPOSITION") {
      mean(sapply(c(pgo@ppoint@component, pgo@pline@component, pgo@pregion@component), attr, "md"))
    } else if(type == "PLATEAUCOLLECTION") {
      mean(sapply(pgo@pgos, spa_avg_degree))
    }
  } else {
    0
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_ncomp(pgo) 
#'  
#' @export
spa_ncomp <- function(pgo) {
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")) {
    length(pgo@component)
  } else if(type == "PLATEAUCOMPOSITION") {
    spa_ncomp(pgo@ppoint) + spa_ncomp(pgo@pline) + spa_ncomp(pgo@pregion)
  } else if(type == "PLATEAUCOLLECTION") {
    if(!spa_is_empty(pgo)) {
      sum(sapply(pgo@pgos, spa_ncomp))
    } else {
      0
    }
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_area(pgo) 
#' 
#' @param pgo A `pgeometry` object of the type `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION`. It throws a warning if a different type is given.
#'  
#' @import sf
#' @export
spa_area <- function(pgo) {
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE")) {
    warning(paste("A", type, "object does not have an area."), call. = FALSE)
    0
  } else if(!spa_is_empty(pgo) && type %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
    if(type == "PLATEAUREGION") {
      area_comp <- function(comp) {
        md_comp <- comp@md
        area_obj <- st_area(comp@obj)
        area_obj * md_comp
      }
      comps_areas <- sapply(pgo@component, area_comp)
      sum(comps_areas)
    } else if(type == "PLATEAUCOMPOSITION") {
      spa_area(pgo@pregion)
    } else if(type == "PLATEAUCOLLECTION") {
      pgos <- get_pgos_from_pcollection(pgo)
      pgos <- pgos[sapply(pgos, spa_get_type) %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")]
      sum(sapply(pgos, spa_area))
    }
  } else {
    0
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_perimeter(pgo) 
#' 
#' @param pgo A `pgeometry` object of the type `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION`. It throws a warning if a different type is given.
#'  
#' @import sf lwgeom
#' @export
spa_perimeter <- function(pgo) {
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE")) {
    warning(paste("A", type, "object does not have a perimeter."), call. = FALSE)
    0
  } else if(!spa_is_empty(pgo) && type %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
    if(type == "PLATEAUREGION") {
      perimeter_comp <- function(comp) {
        md_comp <- comp@md
        temp <- st_sfc(comp@obj)
        st_set_crs(temp, 4326)
        perimeter_obj <- st_perimeter(temp)
        perimeter_obj * md_comp
      }
      comps_perimeter <- sapply(pgo@component, perimeter_comp)
      sum(comps_perimeter)
    } else if(type == "PLATEAUCOMPOSITION") {
      spa_perimeter(pgo@pregion)
    } else if(type == "PLATEAUCOLLECTION") {
      pgos <- get_pgos_from_pcollection(pgo)
      pgos <- pgos[sapply(pgos, spa_get_type) %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")]
      sum(sapply(pgos, spa_perimeter))
    }
  } else {
    0
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_length(pgo) 
#' 
#' @param pgo A `pgeometry` object of the type `PLATEAULINE`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION`. It throws a warning if a different type is given.
#'  
#' @import sf
#' @export
spa_length <- function(pgo) {
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAUREGION")) {
    warning(paste("A", type, "object does not have a length."), call. = FALSE)
    0
  } else if(!spa_is_empty(pgo) && type %in% c("PLATEAULINE", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
    if(type == "PLATEAULINE") {
      length_comp <- function(comp){
        md_comp = comp@md
        length_obj = st_length(comp@obj)
        length_obj * md_comp
      }
      components_lenghts <- sapply(pgo@component, length_comp)
      sum(components_lenghts)
    } else if(type == "PLATEAUCOMPOSITION") {
      spa_length(pgo@pline)
    } else if(type == "PLATEAUCOLLECTION") {
      pgos <- get_pgos_from_pcollection(pgo)
      pgos <- pgos[sapply(pgos, spa_get_type) %in% c("PLATEAULINE", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")]
      sum(sapply(pgos, spa_length))
    }
  } else {
    0
  }
}