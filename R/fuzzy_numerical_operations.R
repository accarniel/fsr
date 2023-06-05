#' @title Fuzzy numerical operations
#'
#' @description Fuzzy numerical operations are given as a family of functions that implements spatial plateau numerical operations.
#' These functions extract metric properties from spatial plateau objects, 
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
#' - `spa_avg_degree` calculates the average membership degree of a spatial plateau object.
#' - `spa_ncomp` returns the number of components of a spatial plateau object.
#' 
#' The remaining functions are _type-dependent_. This means that the parameter have to be of a specific type.
#' The type-dependent functions are:
#' 
#' - `spa_area` computes the area of a plateau region, plateau composition, or plateau collection. Thus, its parameter has to be a `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION` object.
#' - `spa_perimeter` computes the perimeter of a plateau region, plateau composition, or plateau collection. Thus, its parameter has to be a `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION` object.
#' - `spa_length` computes the length of a plateau line, plateau composition, or plateau collection. Thus, its parameter has to be a `PLATEAULINE`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION` object.
#' 
#' @return
#'
#' A numerical value.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' library(sf)
#' library(tibble)
#'
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- create_component(st_multipoint(pts1), 0.2) 
#' comp2 <- create_component(st_point(c(1, 5)), 0.8)  
#' 
#' pp <- create_pgeometry(list(comp1, comp2), "PLATEAUPOINT")
#' 
#' # calculating the average degree and number of components of pp
#' 
#' spa_avg_degree(pp)
#' spa_ncomp(pp)
#' 
#' # calculating the area and perimeter
#' 
#' set.seed(345)
#' 
#' # some random points to create plateau region objects by using the function spa_creator
#' tbl = tibble(x = runif(10, min= 0, max = 20), 
#'              y = runif(10, min = 0, max = 30), 
#'              z = runif(10, min = 0, max = 100))
#' 
#' #getting the convex hull on the points to clip the construction of plateau region objects
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' pregions <- spa_creator(tbl, fuzz_policy = "fcp", k = 2, base_poly = ch)
#' 
#' spa_area(pregions$pgeometry[[1]])
#' spa_area(pregions$pgeometry[[2]])
#' 
#' spa_perimeter(pregions$pgeometry[[1]])
#' spa_perimeter(pregions$pgeometry[[2]])
#' 
#' # calculating the length of a plateau line object
#' 
#' lpts1 <- rbind(c(0, 0), c(1, 1))
#' lpts2 <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lpts3 <- rbind(c(2, 1), c(1.5, 0.5))
#' 
#' cp1 <- create_component(st_linestring(lpts1), 0.4)
#' cp2 <- create_component(st_linestring(lpts2), 1)
#' cp3 <- create_component(st_linestring(lpts3), 0.7)
#' 
#' pline <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAULINE")
#' 
#' spa_length(pline)
#' @export
spa_avg_degree <- function(pgo) {
  type <- spa_get_type(pgo)
  if(!fsr_is_empty(pgo)) {
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
    if(!fsr_is_empty(pgo)) {
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
  } else if(!fsr_is_empty(pgo) && type %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
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
  } else if(!fsr_is_empty(pgo) && type %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
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
  } else if(!fsr_is_empty(pgo) && type %in% c("PLATEAULINE", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
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