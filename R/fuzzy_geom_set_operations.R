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