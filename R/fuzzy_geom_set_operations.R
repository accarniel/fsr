# we include the functions `spa_flatten` and `pcollection_to_pcomposition` in this file because they are related to fuzzy geometric set operations on `pcollection` objects.
# TODO we can create another category of functions, e.g., type-specific operations.

#' @title Flatten a fuzzy spatial collection
#'
#' @description This function gathers all the fuzzy spatial objects of a `PLATEAUCOLLECTION` and 
#' reorganizes them into a single flattened fuzzy spatial object containing a quadruple
#' (`PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`, `PLATEAUCOMPOSITION`) that preserves the identity of sub-objects.
#'
#' @usage
#'
#' spa_flatten(pcol)
#'
#' @param pcol A `pcollection` object.
#'
#' @details
#'
#' It gives a single flattened fuzzy spatial object, aggregating all spatial plateau objects by their types.
#' In the case of a two-level hierarchy, i.e., there is a `PLATEAUCOLLECTION` inside the other, 
#' the function is applied recursively in the lower levels until the quadruple is built. 
#' The t-conorm considered in the aggregation is the `max` operator.
#'
#' @return
#'
#' A spatial plateau collection object.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'
#' @examples
#' library(sf)
#' 
#' pts <- rbind(c(1, 2), c(3, 2))
#' pcp <- create_component(st_multipoint(pts), 0.3)
#' 
#' lpts <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lcp <- create_component(st_linestring(lpts), 1)
#' pline <- create_pgeometry(list(lcp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' rpts2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' rcp <- create_component(st_polygon(list(rpts1, rpts2)), 0.7)
#' pregion <- create_pgeometry(list(rcp), "PLATEAUREGION")
#' 
#' pcomposition <- create_pgeometry(list(pcp, rcp), "PLATEAUCOMPOSITION")
#' pcol1 <- create_pgeometry(list(pcomposition), "PLATEAUCOLLECTION")
#' pcol2 <- create_pgeometry(list(pline, pregion, pcol1), "PLATEAUCOLLECTION")
#' pcol2
#' 
#' spa_flatten(pcol2)
#' @import methods
#' @export
spa_flatten <- function(pcol) {
  if(fsr_is_empty(pcol)) {
    return(pcol)
  }
  
  pgo_list <- get_pgos_from_pcollection(pcol)
  types <- lapply(pgo_list, spa_get_type)
  
  ppoints <- pgo_list[types == "PLATEAUPOINT"]
  plines <- pgo_list[types == "PLATEAULINE"]
  pregions <- pgo_list[types == "PLATEAUREGION"]
  pcompositions <- pgo_list[types == "PLATEAUCOMPOSITION"]
  
  ppoint <- create_empty_pgeometry("PLATEAUPOINT")
  if(length(ppoints)) {
    ppoint <- internal_union_list(ppoints[!sapply(ppoints, fsr_is_empty)], "PLATEAUPOINT")
  }
  pline <- create_empty_pgeometry("PLATEAULINE")
  if(length(plines)) {
    pline <- internal_union_list(plines[!sapply(plines, fsr_is_empty)], "PLATEAULINE")
  }
  pregion <- create_empty_pgeometry("PLATEAUREGION")
  if(length(pregions)) {
    pregion <- internal_union_list(pregions[!sapply(pregions, fsr_is_empty)], "PLATEAUREGION")
  }
  pcomposition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
  if(length(pcompositions)) {
    pcomposition <- internal_union_list(pcompositions[!sapply(pcompositions, fsr_is_empty)], "PLATEAUCOMPOSITION")
  }
  
  pgos <- c(ppoint, pline, pregion, pcomposition)
  new("pcollection", supp = pcol@supp, pgos = pgos)
}


#' @title Convert a spatial plateau collection into a spatial plateau composition
#'
#' @description This function gathers all the fuzzy spatial objects of a `PLATEAUCOLLECTION` and 
#' reorganizes them into an equivalent fuzzy spatial composition object.
#'
#' @usage
#'
#' pcollection_to_pcomposition(pcol)
#'
#' @param pcol A `pcollection` object.
#'
#' @details
#'
#' It gives a fuzzy spatial composition object equivalent to the spatial collection object given as input
#' aggregating all spatial plateau objects by type.
#'
#' @return
#'
#' A spatial plateau composition object.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'
#' @examples
#' library(sf)
#' 
#' pts <- rbind(c(1, 2), c(3, 2))
#' pcp <- create_component(st_multipoint(pts), 0.3)
#' 
#' lpts <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lcp <- create_component(st_linestring(lpts), 1)
#' pline <- create_pgeometry(list(lcp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' rpts2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' rcp <- create_component(st_polygon(list(rpts1, rpts2)), 0.7)
#' pregion <- create_pgeometry(list(rcp), "PLATEAUREGION")
#' 
#' pcomposition <- create_pgeometry(list(pcp, rcp), "PLATEAUCOMPOSITION")
#' pcollection <- create_pgeometry(list(pcomposition), "PLATEAUCOLLECTION")
#' 
#' pcollection_to_pcomposition(pcollection)
#' @import sf
#' @export
pcollection_to_pcomposition <- function(pcol) {
  project <- function(pcol) {
    types <- sapply(pcol@pgos, spa_get_type)
    ppoint <- pcol@pgos[types == "PLATEAUPOINT"]
    if(!length(ppoint)) {
      ppoint <- create_empty_pgeometry("PLATEAUPOINT")
    } else {
      ppoint <- ppoint[[1]]
    }
    pline <- pcol@pgos[types == "PLATEAULINE"]
    if(!length(pline)) {
      pline <- create_empty_pgeometry("PLATEAULINE")
    } else {
      pline <- pline[[1]]
    }
    pregion <- pcol@pgos[types == "PLATEAUREGION"]
    if(!length(pregion)) {
      pregion <- create_empty_pgeometry("PLATEAUREGION")
    } else {
      pregion <- pregion[[1]]
    }
    pcomposition <- pcol@pgos[types == "PLATEAUCOMPOSITION"]
    if(!length(pcomposition)) {
      pcomposition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    } else {
      pcomposition <- pcomposition[[1]]
    }
    list(ppoint = ppoint, pline = pline, pregion = pregion, pcomposition = pcomposition)
  }
  
  # Step (i): removes the hierarchy of the operand object and flattens it.
  pcol <- spa_flatten(pcol)
  
  # Step (ii): we form the fuzzy geometric union of all fuzzy point objects, 
  # fuzzy line objects, and fuzzy region objects respectively that can be found 
  # in the flattened fuzzy spatial collection object.
  quad <- project(pcol)
  ppoint <- spa_union(quad$ppoint, quad$pcomposition@ppoint)
  pline <- spa_union(quad$pline, quad$pcomposition@pline)
  pregion <- spa_union(quad$pregion, quad$pcomposition@pregion)
  
  # Step (iii): takes these two objects and transforms each of them into another fuzzy 
  # spatial collection object. Their single fuzzy point, single fuzzy line, and single 
  # fuzzy region sub-objects fulfill the topological constraints of disjointedness or 
  # adjacency required in the definition of the data type fcomposition. Note that the 
  # fuzzy union operation has the effect of only preserving those lower-dimensional 
  # objects that are not located in higher-dimensional objects.
  
  # Spatial plateau point
  pcomposition <- spa_union(ppoint, pline, as_pcomposition = TRUE)
  pcomposition <- spa_union(pcomposition@ppoint, pregion, as_pcomposition = TRUE)
  ppoint <- pcomposition@ppoint
  # Spatial plateau line
  pcomposition <- spa_union(pline, pregion, as_pcomposition = TRUE)
  pline <- pcomposition@pline
  
  # Spatial plateau region
  pregion <- pcomposition@pregion
  
  # TODO check if we really to compute the support here since it would be the same support as the support of pcol
  supp <- st_union(st_sfc(ppoint@supp, pline@supp, pregion@supp))
  
  result <- create_empty_pgeometry("PLATEAUCOMPOSITION")
  result@supp <- supp[[1]]
  result@ppoint <- ppoint
  result@pline <- pline
  result@pregion <- pregion
  result
}

#' @title Fuzzy geometric set operations
#'
#' @description Fuzzy geometric set operations are given as a family of functions that implements spatial plateau set operations.
#' These functions yield a spatial plateau object from a specific combination of other two spatial plateau objects, 
#' such as the intersection of two plateau region objects.
#'
#' @usage
#'
#' spa_intersection(pgo1, pgo2, itype = "min")
#'
#' @param pgo1 A `pgeometry` object of any type.
#' @param pgo2 A `pgeometry` object of the same type of `pgo1`.
#' @param itype A character value that indicates the name of a function implementing a t-norm. The default value is `"min"`, which is the standard operator of the intersection.
#' 
#' @name fsr_geometric_operations
#'
#' @details
#'
#' These functions implement geometric operations of the spatial plateau algebra. 
#' They receive two `pgeometry` objects of the _same type_ together with an operation as inputs and yield another `pgeometry` object as output. The output object has _the same_ type of the inputs.
#' The family of fuzzy geometric set operations consists of the following functions:
#' 
#' - `spa_intersection` computes the geometric intersection of two spatial plateau objects. 
#' The membership degree of common points are calculated by using a t-norm operator given by the parameter `itype`. Currently, it can assume `"min"` (default) or `"prod"`.
#' - `spa_union` computes the geometric union of two spatial plateau objects.
#' The membership degree of common points are calculated by using a t-conorm operator given by the parameter `utype`. Currently, it can assume `"max"` (default).
#' - `spa_difference` computes the geometric difference of two spatial plateau objects.
#' The membership degree of common points are calculated by using a diff operator given by the parameter `dtype`. 
#' Currently, it can assume `"f_diff"` (default fuzzy difference), `"f_bound_diff"` (fuzzy bounded difference), `"f_symm_diff"` (fuzzy symmetric difference), and `"f_abs_diff"` (fuzzy absolute difference).
#' 
#' Another related geometric function is:
#' 
#' - `spa_common_points` which gets the common points of two plateau line objects by using a t-norm to compute their membership degrees. 
#' It is different from the other functions since it gets two plateau line objects as input and yields a plateau point object as output.
#' 
#' Other t-norms, t-conorms, and diff operators can be implemented and given as values for the `"itype"`, `"utype"`, and `"dtype"`, respectively. 
#' For this, the following steps should be performed:
#' 
#' 1 - implement your function that accepts two numeric values as inputs and yields another numeric value as output. All values should be between 0 and 1. Recall that t-norms and t-conorms must have some specific properties according to the fuzzy set theory.
#' 2 - use the name of your function as the character value of the corresponding `"itype"`, `"utype"`, or `"dtype"`.
#' 
#' An example of operator is the source code of `f_bound_diff`:
#' 
#' `f_bound_diff <- function(x, y) { max(0, (x - y)) }`
#' 
#' @return
#'
#' A `pgeometry` object that is the result of the geometric manipulation between two spatial plateau objects.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' library(sf)
#' 
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pts3 <- rbind(c(2, 2), c(3, 3))
#'
#' cp1 <- component_from_sfg(st_multipoint(pts1), 0.3)
#' cp2 <- component_from_sfg(st_multipoint(pts2), 0.6)
#' cp3 <- component_from_sfg(st_multipoint(pts3), 1)
#' 
#' pp1 <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' 
#' pts4 <- rbind(c(0, 0), c(1, 1))
#' pts5 <- rbind(c(2, 3), c(1.2, 1.9), c(2, 1))
#' pts6 <- rbind(c(3, 1), c(1.5, 0.5))
#' 
#' cp4 <- component_from_sfg(st_multipoint(pts4), 0.4)
#' cp5 <- component_from_sfg(st_multipoint(pts5), 1)
#' cp6 <- component_from_sfg(st_multipoint(pts6), 0.7)
#' 
#' pp2 <- create_pgeometry(list(cp4, cp5, cp6), "PLATEAUPOINT")
#' 
#' pp1
#' pp2
#' 
#' spa_intersection(pp1, pp2)
#' spa_intersection(pp1, pp2, itype = "prod") #changing the t-norm
#' spa_union(pp1, pp2)
#' spa_difference(pp1, pp2)
#'
#' @import sf
#' @export
spa_intersection <- function(pgo1, pgo2, itype = "min"){

  if(pgo1@type != pgo2@type){
    stop("Different spatial plateau data types.", call. = FALSE)
  }

  sigma <- match.fun(itype)
  result <- create_empty_pgeometry(pgo1@type)
  lcomps <- vector("list")

  for(comp1 in pgo1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md

    for(comp2 in pgo2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md

      result_md = sigma(md_comp_p1, md_comp_p2)

      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)

      # check geom and pgo
      lcomps <- append_valid_comps(sf_result, result , result_md, lcomps)
      }
  }

  if(length(lcomps) > 0){
    spa_add_component(result, lcomps)
  } else {
    result
  }
}

#' @name fsr_geometric_operations
#' 
#' @usage
#' 
#' spa_union(pgo1, pgo2, utype = "max")
#' 
#' @param utype A character value that refers to a t-conorm. The default value is `"max"`, which is the standard operator of the union.
#' 
#' @import sf
#' @export
spa_union <- function(pgo1, pgo2, utype = "max"){

  if(pgo1@type != pgo2@type){
    stop("Different spatial plateau data types.", call. = FALSE)
  }

  tau <- match.fun(utype)
  result <- create_empty_pgeometry(pgo1@type)
  lcomps <- vector("list")

  supp_intersected <- st_intersection(pgo1@supp, pgo2@supp)

  for(comp1 in pgo1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md

    for(comp2 in pgo2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md

      result_md = tau(md_comp_p1, md_comp_p2)

      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)

      lcomps <- append_valid_comps(sf_result, result , result_md, lcomps)
    }

    sf_diff_1 <- st_difference(obj_comp_p1, supp_intersected)
    # check result type and appends to list if compatible
    lcomps <- append_valid_comps(sf_diff_1, result, md_comp_p1, lcomps)
  }

  for(comp2 in pgo2@component){
    obj_comp_p2 <- comp2@obj
    md_comp_p2 <- comp2@md

    sf_diff_3 <- st_difference(obj_comp_p2, supp_intersected)
    # check result type and appends to list if compatible
    lcomps <- append_valid_comps(sf_diff_3, result, md_comp_p2, lcomps)
  }

  if(length(lcomps) > 0){
    spa_add_component(result, lcomps)
  } else {
    result
  }
}

#' @name fsr_geometric_operations
#' 
#' @usage
#' 
#' spa_difference(pgo1, pgo2, dtype = "f_diff")
#' 
#' @param dtype A character value that indicates the name of a difference operator. The default value is `"f_diff"`, which implements the standard fuzzy difference.
#' 
#' @import sf
#' @export
spa_difference <- function(pgo1, pgo2, dtype = "f_diff"){

  if(pgo1@type != pgo2@type){
    stop("Different spatial plateau data types.", call. = FALSE)
  }

  nu <- match.fun(dtype)
  result <- create_empty_pgeometry(pgo1@type)
  lcomps <- vector("list")

  supp_intersected <- st_intersection(pgo1@supp, pgo2@supp)

  for(comp1 in pgo1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md

    for(comp2 in pgo2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md

      result_md = nu(md_comp_p1, md_comp_p2)

      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)

      lcomps <- append_valid_comps(sf_result, result , result_md, lcomps)
    }

    sf_diff_1 <- st_difference(obj_comp_p1, supp_intersected)

    # check result type and appends to list if compatible
    lcomps <- append_valid_comps(sf_diff_1, result, md_comp_p1, lcomps)
  }

  if(length(lcomps) > 0){
    spa_add_component(result, lcomps)
  } else {
    result
  }
}

#' @name fsr_geometric_operations
#' 
#' @usage
#' 
#' spa_common_points(pline1, pline2, itype = "min")
#' 
#' @param pline1 A `pgeometry` object of the type `PLATEAULINE`.
#' @param pline2 A `pgeometry` object of the type `PLATEAULINE`.
#' 
#' @import sf methods
#' @export
spa_common_points <- function(pline1, pline2, itype = "min"){
  
  if(pline1@type != pline2@type){
    stop("Different Spatial Plateau Types.", call. = FALSE)
  }
  
  sigma <- match.fun(itype)
  result <- create_empty_pgeometry(pline1@type)
  lcomps <- vector("list")
  
  for(comp1 in pline1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md
    
    for(comp2 in pline2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md
      
      result_md = sigma(md_comp_p1, md_comp_p2)
      
      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)
      
      if(!st_is_empty(sf_result) && st_geometry_type(sf_result) %in% c("POINT", "MULTIPOINT")){
        result_comp <- new("component", obj = sf_result, md = result_md)
        lcomps <- append(lcomps, result_comp)
      } else if(!st_is_empty(sf_result) && st_geometry_type(sf_result) == "GEOMETRYCOLLECTION") {
        type_geom = get_counter_ctype(pline1)
        union_obj <- st_union(st_collection_extract(sf_result, type = type_geom))
        if(inherits(union_obj, "sfc")) {
          union_obj <- union_obj[[1]]
        }
        result_comp <- new("component", obj = union_obj, md = result_md)
        lcomps <- append(lcomps, result_comp)
      }
    }
  }
  
  if(length(lcomps) > 0){
    spa_add_component(result, lcomps)
  } else {
    result
  }
}

#' @title Fuzzy difference operators
#'
#' @description Fuzzy difference operations are set operations that generalize Boolean difference operations. 
#' This family of functions implements some operators that help us to define different fuzzy difference operations.
#' These operators receive two numerical values in \[0, 1\] as input and calculates another numerical value in \[0, 1\] as output.
#'
#' @usage
#'
#' f_diff(x, y)
#'
#' @param x A numerical vector whose values are in \[0, 1\].
#' @param y A numerical vector whose values are in \[0, 1\].
#' 
#' @name fsr_diff_operators
#'
#' @details
#'
#' These functions calculate the resulting membership degree of a fuzzy difference operator applied on two numerical values in the interval \[0, 1\]. 
#' The following fuzzy difference operators are available:
#' - `f_diff`: The standard _fuzzy set difference_ operator defined as the intersection of `x` and the complement of `y`, that is, `min(x, 1 - y)`.
#' - `f_bound_diff`: The _fuzzy bounded difference_ operator defined as `x` minus `y` with upper bound equal to 0, that is, `max(0, x - y)`.
#' - `f_symm_diff`: The _fuzzy symmetric difference_ operator defined as the union of the difference of `x` and `y` and the difference of `y` and `x`, that is, `max(f_diff(x, y), f_diff(y, x))`.
#' - `f_abs_diff`: The _fuzzy absolute difference_ operator defined as the absolute difference of `x` and `y`, that is, `abs(x - y)`.
#' 
#' These operators are useful to process the function `spa_difference` since one of them can be informed as a parameter for this function.
#' 
#' @return
#'
#' A numerical vector.
#'
#' @examples
#'
#' x <- c(0.1, 0.3, 0.6, 0.8)
#' y <- c(0.9, 0.7, 0.4, 0.2)
#' 
#' f_diff(x, y)
#' f_bound_diff(x, y)
#' f_symm_diff(x, y)
#' f_abs_diff(x, y)
#'
#' @export
f_diff <- function(x, y){
  pmin(x, (1 - y))
}

#' @name fsr_diff_operators
#' @export
f_bound_diff <- function(x, y){
  pmax(0, (x - y))
}

#' @name fsr_diff_operators
#' @export
f_symm_diff <- function(x, y){
  pmax(f_diff(x, y), f_diff(y, x))
}

#' @name fsr_diff_operators
#' @export
f_abs_diff <- function(x, y){
  abs(x - y)
}