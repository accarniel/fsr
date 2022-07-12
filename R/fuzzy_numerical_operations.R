#' @title Fuzzy numerical operations
#'
#' @description Fuzzy numerical operations are given as a family of functions that implements spatial plateau metric operations.
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
#' - `spa_area` computes the area of a plateau region object. Thus, its parameter has to be a `PLATEAUREGION` object.
#' - `spa_perimeter` computes the perimeter of a plateau region object. Thus, its parameter has to be a `PLATEAUREGION` object.
#' - `spa_length` computes the length of a plateau line object. Thus, its parameter has to be a `PLATEAULINE` object.
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
#'
#' library(sf)
#' library(tibble)
#'
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- component_from_sfg(st_multipoint(pts1), 0.2) 
#' comp2 <- component_from_sfg(st_point(c(1, 5)), 0.8)  
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
#' cp1 <- component_from_sfg(st_linestring(lpts1), 0.4)
#' cp2 <- component_from_sfg(st_linestring(lpts2), 1)
#' cp3 <- component_from_sfg(st_linestring(lpts3), 0.7)
#' 
#' pline <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAULINE")
#' 
#' spa_length(pline)
#'
#' @export
spa_avg_degree <- function(pgo){
  get_md <- function(comp){
    comp@md
  }
  if(!fsr_is_empty(pgo)) {
    mds_vec <- sapply(pgo@component, get_md)
    mean(mds_vec)
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
spa_ncomp <- function(pgo){
  length(pgo@component)
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_area(pr) 
#' 
#' @param pr A `pgeometry` object of the type `PLATEAUREGION`. It throws an error if a different type is given.
#'  
#' @import sf
#' @export
spa_area <- function(pr) {
  if(pr@type != "PLATEAUREGION") {
    warning("The input is not a PLATEAUREGION object.", call. = FALSE)
    0
  } else if(!fsr_is_empty(pr)) {
    area_comp <- function(comp){
      md_comp = comp@md
      area_obj = st_area(comp@obj)
      area_obj * md_comp
    }
    
    comps_areas <- sapply(pr@component, area_comp)
    sum(comps_areas)
  } else {
    0
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_perimeter(pr) 
#'  
#' @import sf lwgeom
#' @export
spa_perimeter <- function(pr) {
  if(pr@type != "PLATEAUREGION") {
    warning("The input is not a PLATEAUREGION object.", call. = FALSE)
    0
  } else if(!fsr_is_empty(pr)) {
    perimeter_comp <- function(comp) {
      md_comp = comp@md
      temp <- st_sfc(comp@obj)
      st_set_crs(temp, 4326)
      perimeter_obj = st_perimeter(temp)
      perimeter_obj * md_comp
    }
    
    comps_perimeter <- sapply(pr@component, perimeter_comp)
    sum(comps_perimeter)
  } else {
    0
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_length(pl) 
#' 
#' @param pl A `pgeometry` object of the type `PLATEAULINE`. It throws an error if a different type is given.
#'  
#' @import sf
#' @export
spa_length <- function(pl) {
  if(pl@type != "PLATEAULINE") {
    warning("The input is not a PLATEAULINE object.", call. = FALSE)
    0
  } else if(!fsr_is_empty(pl)) {
    length_comp <- function(comp){
      md_comp = comp@md
      length_obj = st_length(comp@obj)
      length_obj * md_comp
    }
  
    components_lenghts <- sapply(pl@component, length_comp)
    sum(components_lenghts)
  } else {
    0
  }
}