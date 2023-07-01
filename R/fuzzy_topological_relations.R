#' @title Check exact equality
#'
#' @description This function checks whether two spatial plateau objects are exactly equal.
#'
#' @usage
#'
#' spa_exact_equal(pgo1, pgo2)
#'
#' @param pgo1 A `pgeometry` object of the types `PLATEAUPOINT`, `PLATEAULINE`, and `PLATEAUREGION`.
#' @param pgo2 A `pgeometry` object of the types `PLATEAUPOINT`, `PLATEAULINE`, and `PLATEAUREGION`.
#' 
#' @details
#'
#' It is a Boolean function that checks _fuzzy equality_ in the spatial plateau context. Two `pgeometry` objects are exactly equal if their components are equal. 
#' Two components are equal if they have the same membership degree and they are (spatially) equal (i.e., their `sfg` objects have the same geometric format - this means that the order of the points can be different).
#'   
#' @return
#'
#' A Boolean value that indicates if two `pgeometry` objects are exactly equal.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' library(sf)
#'
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pts3 <- rbind(c(2, 2), c(3, 3))
#' 
#' cp1 <- create_component(st_multipoint(pts1), 0.3)
#' cp2 <- create_component(st_multipoint(pts2), 0.6)
#' cp3 <- create_component(st_multipoint(pts3), 1.0)
#' 
#' pp1 <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' pp2 <- create_pgeometry(list(cp2, cp1), "PLATEAUPOINT")
#' 
#' spa_exact_equal(pp1, pp2)
#' 
#' spa_exact_equal(pp1, pp1)
#' @import sf
#' @export
spa_exact_equal <- function(pgo1, pgo2) {
  
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION") || type2 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
    stop("This function only deals with PLATEAUPOINT, PLATEAULINE, and PLATEAUREGION.")
  }
  
  # Same type and both empty
  if(type1 == type2 && all(sapply(c(pgo1, pgo2), spa_is_empty))) {
    return(TRUE)
  }
  
  comp_check <- function(comp1, comp2) {
    if(st_equals(comp1@obj, comp2@obj, sparse=FALSE)[1] && (comp1@md == comp2@md)) {
      TRUE
    }
    FALSE
  }
  
  if((type1 != type2) ||
     (spa_ncomp(pgo1) != spa_ncomp(pgo2)) ||
     (!(st_equals(pgo1@supp, pgo2@supp, sparse=FALSE)[1]))) {
    return(FALSE)
  } else {
    for(i in 1:spa_ncomp(pgo1)){
      if(!(comp_check(pgo1@component[[i]], pgo2@component[[i]]))) {
        return(FALSE)
      }
    }
  }
  TRUE
}

#' @title Check exact containment
#'
#' @description This function checks whether a `pgeometry` object is completely inside of another `pgeometry` object.
#'
#' @usage
#'
#' spa_exact_inside(pgo1, pgo2)
#'
#' @param pgo1 A `pgeometry` object of the types `PLATEAUPOINT`, `PLATEAULINE`, and `PLATEAUREGION`.
#' @param pgo2 A `pgeometry` object of the types `PLATEAUPOINT`, `PLATEAULINE`, and `PLATEAUREGION`.
#' 
#' @details
#'  
#' It is a Boolean function that checks _fuzzy containment_ in the spatial plateau context. 
#' 
#' This Boolean function checks whether the components of `pgo1` are contained in the components of `pgo2` 
#' by considering their membership degrees and geographic positions. That is, it is follows the classical definition of fuzzy containment of the fuzzy set theory.
#' 
#' In other words, this function checks if the (standard) intersection of `pgo1` and `pgo2` is exactly equal to `pgo1`. The other of operands affects the result.
#' 
#' @return
#'
#' A Boolean value that indicates if a `pgeometry` is completely and certainly inside `pgo2`.
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
#' cp1 <- create_component(st_multipoint(pts1), 0.3)
#' cp2 <- create_component(st_multipoint(pts2), 0.6)
#' cp3 <- create_component(st_multipoint(pts3), 1.0)
#' 
#' # Creating two spatial plateau objects
#' pp1 <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' pp2 <- create_pgeometry(list(cp2, cp1), "PLATEAUPOINT")
#' 
#' # The other of operands after the result
#' # pp1 is not inside pp2 since it has one point that is not included in pp2
#' spa_exact_inside(pp1, pp2)
#' 
#' # on the other hand, pp2 is inside pp1
#' spa_exact_inside(pp2, pp1)
#'
#' @export
spa_exact_inside <- function(pgo1, pgo2){
  
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION") || type2 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
    stop("This function only deals with PLATEAUPOINT, PLATEAULINE, and PLATEAUREGION.")
  }
  
  intersected <- spa_intersection(pgo1, pgo2, as_pcomposition = FALSE)
  if(spa_get_type(intersected) == "PLATEAUCOMPOSITION") {
    FALSE
  } else {
    # TODO implement the spa_exact_equal for PLATEAUCOMPOSITION
    spa_exact_equal(intersected, pgo1)
  }
}

#' Returns the desired type of result of a fuzzy topological relationship
#' 
#' @noRd
spa_eval_relation <- function(ret, result, ...) {
  
  aux_function <- function(degree) {
    classes <- pkg_env$ftopological_classes
    mfs <- pkg_env$ftopological_mfs
    
    values_set <- list()
    degrees <- lapply(mfs, function(mf) mf(degree))
    names(degrees) <- classes
    degrees
  }
  
  args <- list(...)
  switch(ret,
         degree = return(result),
         list = return(aux_function(result)),
         bool = {
           list_res <- aux_function(result)
           if(!("eval_mode" %in% names(args) && "lval" %in% names(args))){
             stop("args not supplied. 'eval_mode' and 'lval' needed for bool result type", call. = FALSE)
           }
           e_mode <- match.fun(args$eval_mode)
           term <- args$lval
           return(e_mode(list_res[[term]]))
         },
         stop("Return type does not exist.", call. = FALSE))
}

#' Checks if we can evaluate the fuzzy topological relationship
#' 
#' @noRd
check_spa_topological_condition <- function(pgo1, pgo2) {
  
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 != type2) {
    stop("The spatial plateau objects have different types.", call. = FALSE)
  } else if(type1 != "PLATEAUREGION" || type2 != "PLATEAUREGION") {
    stop(paste0("This operator is not implemented to (", type1, " x ", type2, ") yet."), call. = FALSE)
  }
}

#' @title Fuzzy topological relationships
#'
#' @description Fuzzy topological relationships are given as a family of functions that implements spatial plateau topological relationships.
#' A fuzzy topological relationship expresses a particular relative position of two spatial plateau objects.
#' Since the spatial objects are fuzzy, their topological relationships are also fuzzy.
#' Hence, a fuzzy topological relationship determines the degree to which a relation holds for any two spatial plateau objects by a real value in the interval \[0, 1\].
#' The key idea of these relationships is to consider point subsets resulting from the combination of spatial plateau
#' set operations and spatial plateau metric operations on the spatial plateau objects for computing the resulting degree.
#' The resulting degree can be also interpreted as a linguistic value. 
#'
#' @usage
#'
#' spa_overlap(pgo1, pgo2, itype = "min", ret = "degree", ...)
#'
#' @param pgo1 A `pgeometry` object of the type `PLATEAUREGION`.
#' @param pgo2 A `pgeometry` object of the type `PLATEAUREGION`.
#' @param itype A character value that indicates the name of a function implementing a t-norm. The default value is `"min"`, which is the standard operator of the intersection.
#' @param ret A character value that indicates the return type of the fuzzy topological relationship. The default value is `"degree"` and other possible values are `"list"` and `"bool"`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> If `ret = "bool"`, two additional parameters have to be informed, as described below.
#' 
#' @name fsr_topological_relationships
#'
#' @details
#'
#' These functions implement topological relationships of the spatial plateau algebra. 
#' They receive two `pgeometry` objects of the type `PLATEAUREGION` together with some additional parameters (as detailed below).
#' The family of fuzzy topological relationships consists of the following functions:
#' 
#' - `spa_overlap` computes the overlapping degree of two plateau region objects.
#' Since it uses the intersection operation, a t-norm operator can be given by the parameter `itype`. Currently, it can assume `"min"` (default) or `"prod"`.
#' - `spa_meet` computes the meeting degree of two plateau region objects.
#' Similarly to `spa_overlap`, a t-norm operator can be given by the parameter `itype`.
#' - `spa_disjoint` computes the disjointedness degree of two plateau region objects.
#' Similarly to `spa_overlap` and `spa_meet`, a t-norm operator can be given by the parameter `itype`.
#' - `spa_equal` - computes how equal are two plateau region objects.
#' Since it uses the union operation, a t-conorm operator can be given by the parameter `utype`. Currently, it can assume `"max"` (default).
#' - `spa_inside` - computes the containment degree of `pgo1` in `pgo2`.
#' Similarly to `spa_equal`, a t-conorm operator can be given by the parameter `utype`.
#' - `spa_contains` - it is the same of `spa_inside` but changing the order of the operands `pgo1` and `pgo2`.
#' 
#' The parameter `ret` determines the returning value of a fuzzy topological relationship. The default value is the following:
#' 
#' - `"degree"` (default) - it indicates that the function will return a value in \[0, 1\] that represents the degree of truth of a given topological relationships.
#' 
#' For the remainder possible values, the functions make use of a set of linguistic values that characterize the different situations of topological relationships.
#' Each linguistic value has an associated membership function defined in the domain \[0, 1\].
#' The `fsr` package has a default set of linguistic values. You can use the function `spa_set_classification` to change this set of linguistic values.
#' 
#' The remainder possible values for the parameter `ret` are:
#' 
#' - `"list"` - it indicates that the function will return a named list containing how much the result of the predicate belongs to each linguistic value (i.e., it employs the membership functions of the linguistic values).
#' - `"bool"` - it indicates that the function will return a Boolean value indicating whether the degree returned by the topological relationship matches a given linguistic value according to an _evaluation mode_.
#' The evaluation mode and the linguistic values have to be informed by using the parameters `eval_mode` and `lval`, respectively.
#' The possible values for `eval_mode` are: `"soft_eval"`, `"strict_eval"`, `"alpha_eval"`, and `"soft_alpha_eval"`.
#' They have different behavior in how computing the Boolean value from the membership function of a linguistic value. See their documentations for more details.
#' Note that the parameter `lval` only accept a character value belonging to the set of linguistic values that characterize the different situations of topological relationships.
#'   
#' @return
#'
#' The returning value is determined by the parameter `ret`, as described above.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' library(tibble)
#' library(sf)
#' 
#' set.seed(456)
#' 
#' # some random points to create pgeometry objects by using the function spa_creator
#' tbl = tibble(x = runif(10, min= 0, max = 30), 
#'              y = runif(10, min = 0, max = 30), 
#'              z = runif(10, min = 0, max = 50))
#' 
#' #getting the convex hull on the points to clipping the construction of plateau region objects
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' pregions <- spa_creator(tbl, base_poly = ch, fuzz_policy = "fcp", k = 2)
#' 
#' # Showing the different types of returning values
#' spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]])
#' spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#' spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "bool", 
#'            eval_mode = "soft_eval", lval = "mostly")
#'
#' ## Examples for evaluating the other fuzzy topological relationships
#' \dontrun{ 
#' spa_meet(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#' spa_disjoint(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#' spa_equal(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#' spa_inside(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#' spa_contains(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#' }
#' 
#' @import sf
#' @export
spa_overlap <- function(pgo1, pgo2, itype = "min", ret = "degree", ...) {
  
  check_spa_topological_condition(pgo1, pgo2)
  
  r <- spa_intersection(pgo1, pgo2, itype = itype, as_pcomposition = TRUE)
  r <- r@pregion
  
  supp_pgo1 <- pgo1@supp
  supp_pgo2 <- pgo2@supp
  
  result <- NULL
  
  if(spa_ncomp(r) == 1 && !st_is_empty(spa_core(r))){
    result <- 1
  } else if(st_disjoint(supp_pgo1, supp_pgo2, sparse=FALSE)[1] ||
            st_touches(supp_pgo1, supp_pgo2, sparse=FALSE)[1] ||
            spa_exact_inside(pgo1, pgo2) ||
            spa_exact_inside(pgo2, pgo1) ||
            spa_exact_equal(pgo2, pgo1)) {
    result <- 0
  } else {
    result <- spa_area(r)/st_area(st_intersection(supp_pgo1, supp_pgo2))
  }
  
  spa_eval_relation(ret, result, ...)
}

#' @name fsr_topological_relationships
#' 
#' @usage
#' 
#' spa_meet(pgo1, pgo2, itype = "min", ret = "degree", ...) 
#' 
#' @import sf
#' @export
spa_meet <- function(pgo1, pgo2, itype = "min", ret = "degree", ...){
  
  check_spa_topological_condition(pgo1, pgo2)
  
  countour_pgo1 <- spa_contour(pgo1)
  countour_pgo2 <- spa_contour(pgo2)
  
  countour_int <- spa_intersection(countour_pgo1, countour_pgo2, itype = itype, as_pcomposition = TRUE)
  # Common points
  p <- countour_int@ppoint
  # Common border lines
  c <- countour_int@pline
  
  p_ncomp <- spa_ncomp(p)
  c_ncomp <- spa_ncomp(c)
  p_core <- spa_core(p)
  c_core <- spa_core(c)
  
  result <- NULL
  
  if((p_ncomp == 1 && !st_is_empty(p_core)) ||
     (c_ncomp == 1 && !st_is_empty(c_core))) {
    result <- 1
  } else {    
    supp_1 <- pgo1@supp
    supp_2 <- pgo2@supp
    
    pgo1_core <- spa_core(pgo1)
    pgo2_core <- spa_core(pgo2)
    
    if((st_disjoint(supp_1, supp_2, sparse=FALSE)[1]) ||
       !(st_disjoint(pgo1_core, pgo2_core, sparse=FALSE)[1]) ||
       st_touches(pgo1_core, pgo2_core, sparse=FALSE)[1] ||
       spa_exact_inside(pgo1, pgo2) ||
       spa_exact_inside(pgo2, pgo1) ||
       spa_exact_equal(pgo1, pgo2)) {
      result <- 0
    } else if(st_relate(supp_1, supp_2, pattern = "F**0*****", sparse=FALSE)[1] ||
              st_relate(supp_1, supp_2, pattern = "F***0****", sparse=FALSE)[1] ||
              st_relate(supp_1, supp_2, pattern = "F0*******", sparse=FALSE)[1]) {
      result <- spa_avg_degree(p)
    } else if (st_relate(supp_1, supp_2, pattern = "F**1*****", sparse=FALSE)[1] ||
               st_relate(supp_1, supp_2, pattern = "F***1****", sparse=FALSE)[1] ||
               st_relate(supp_1, supp_2, pattern = "F1*******", sparse=FALSE)[1]) {
      pgo1_boundary <- spa_boundary(pgo1)
      pgo1_boundary <- pgo1_boundary@pline
      pgo2_boundary <- spa_boundary(pgo2)
      pgo2_boundary <- pgo2_boundary@pline
      bl <- spa_intersection(pgo1_boundary, pgo2_boundary, itype = itype, as_pcomposition = TRUE)
      bl <- bl@pline
      
      plength <- spa_length(bl)
      length_support <- st_length(bl@supp)
      
      result <- plength/length_support
    } else {
      pgo1_boundary <- spa_boundary(pgo1)
      pgo1_boundary <- pgo1_boundary@pregion
      pgo2_boundary <- spa_boundary(pgo2)
      pgo2_boundary <- pgo2_boundary@pregion
      br <- spa_intersection(pgo1_boundary, pgo2_boundary, itype = itype, as_pcomposition = TRUE)
      br <- br@pregion
      br_area <- spa_area(br)
      area_support <- st_area(br@supp)
      result <- br_area/area_support
    }
  }
  spa_eval_relation(ret, result, ...)
}


#' @name fsr_topological_relationships
#' 
#' @usage
#' 
#' spa_disjoint(pgo1, pgo2, itype = "min", ret = "degree", ...) 
#' 
#' @import sf
#' @export
spa_disjoint <- function(pgo1, pgo2, itype="min", ret = "degree", ...) {

  check_spa_topological_condition(pgo1, pgo2)

  supp_pgo1 <- pgo1@supp
  supp_pgo2 <- pgo2@supp
  
  result <- NULL

  if(st_disjoint(supp_pgo1, supp_pgo2, sparse=FALSE)[1]) {
    result <- 1
  } else {
    r_overlap <- spa_overlap(pgo1, pgo2, itype = itype)
    r_meet <- spa_meet(pgo1, pgo2)
    if(r_overlap == 1 || r_meet == 1 ||
        spa_exact_inside(pgo1, pgo2) ||
        spa_exact_inside(pgo2, pgo1) ||
        spa_exact_equal(pgo2, pgo1)) {
      result <- 0
    } else {
      result <- 1 - max(r_overlap, r_meet)
    }
  }
  spa_eval_relation(ret, result, ...)
}

#' @name fsr_topological_relationships
#' 
#' @usage
#' 
#' spa_equal(pgo1, pgo2, utype = "max", ret = "degree", ...) 
#' 
#' @param utype A character value that indicates the name of a function implementing a t-conorm. The default value is `"max"`, which is the standard operator of the union.
#' 
#' @import sf
#' @export
spa_equal <- function(pgo1, pgo2, utype = "max", ret = "degree", ...) {
  
  check_spa_topological_condition(pgo1, pgo2)
  result <- NULL
  
  if(spa_exact_equal(pgo1, pgo2)) {
    result <- 1
  } else {   
    supp_pgo1 <- pgo1@supp
    supp_pgo2 <- pgo2@supp
    
    if(st_disjoint(supp_pgo1, supp_pgo2, sparse=FALSE)[1] ||
       st_touches(supp_pgo1, supp_pgo2, sparse=FALSE)[1]) {
      result <- 0
    } else {
      r_diff <- spa_difference(pgo1, pgo2, dtype = "f_abs_diff", as_pcomposition = TRUE)
      r_diff <- r_diff@pregion
      r_union <- spa_union(pgo1, pgo2, utype = utype, as_pcomposition = TRUE)
      r_union <- r_union@pregion
      
      r_spa_area <- spa_area(r_diff)
      r_sfg_area <- st_area(r_union@supp)
      
      result <- 1 - (r_spa_area/r_sfg_area)
    }
  }
  spa_eval_relation(ret, result, ...)
}

#' @name fsr_topological_relationships
#' 
#' @usage
#' 
#' spa_inside(pgo1, pgo2, utype = "max", ret = "degree", ...) 
#' 
#' @import sf
#' @export
spa_inside <- function(pgo1, pgo2, utype = "max", ret = "degree", ...) {
  
  check_spa_topological_condition(pgo1, pgo2)
  result <- NULL
  
  if(spa_exact_inside(pgo1, pgo2)) {
    result <- 1
  } else {    
    supp_pgo1 <- pgo1@supp
    supp_pgo2 <- pgo2@supp
    
    if(spa_equal(pgo1, pgo2, utype = utype) == 1 ||
       st_disjoint(supp_pgo1, supp_pgo2, sparse=FALSE)[1] ||
       st_touches(supp_pgo1, supp_pgo2, sparse=FALSE)[1]) {
      result <- 0
    } else {
      r_diff <- spa_difference(pgo1, pgo2, dtype = "f_bound_diff", as_pcomposition = TRUE)
      r_diff <- r_diff@pregion
      
      result <- 1 - (spa_area(r_diff)/st_area(supp_pgo1))
    }
  }
  spa_eval_relation(ret, result, ...)
}

#' @name fsr_topological_relationships
#' 
#' @usage
#' 
#' spa_contains(pgo1, pgo2, utype = "max", ret = "degree", ...) 
#' 
#' @export
spa_contains <- function(pgo1, pgo2, utype = "max", ret = "degree", ...){
  spa_inside(pgo2, pgo1, utype = utype, ret = ret, ...)
}

pkg_env <- new.env()
pkg_env$ftopological_classes <- c("a little bit", "somewhat", "slightly", "averagely", "mostly", "quite")

pkg_env$ftopological_mfs <- c(trap_mf(0, 0, 0.03, 0.08),
                              trap_mf(0.03, 0.08, 0.2, 0.26),
                              trap_mf(0.2, 0.26, 0.39, 0.45),
                              trap_mf(0.39, 0.45, 0.62, 0.69),
                              trap_mf(0.62, 0.69, 0.93, 0.95),
                              trap_mf(0.93, 0.95, 1, 1))

#' @title Setting a new classification for fuzzy topological relationships
#'
#' @description This functions configures a new set of linguistic values and their corresponding membership functions to be used by fuzzy topological relationships.
#'
#' @usage
#'
#' spa_set_classification(classes, mfs)
#'
#' @param classes A character vector containing linguistic values that characterizes different situations of fuzzy topological relationships.
#' @param mfs A vector of membership functions with domain in \[0, 1\].
#'
#' @details
#'
#' This function replaces the default linguistic values employed by fuzzy topological relationships.
#' Each membership function _i_ of the parameter `mfs` represents the class _i_ of the parameter `classes`.
#' The length of these parameters have to be same.
#' 
#' @return 
#' 
#' No return values, called for side effects.
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' library(tibble)
#' library(sf)
#' 
#' set.seed(456)
#' 
#' # some random points to create pgeometry objects by using the function spa_creator
#' tbl = tibble(x = runif(10, min= 0, max = 30), 
#'              y = runif(10, min = 0, max = 30), 
#'              z = runif(10, min = 0, max = 50))
#' 
#' #getting the convex hull on the points to clipping the construction of plateau region objects
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' pregions <- spa_creator(tbl, base_poly = ch, fuzz_policy = "fcp", k = 2)
#' 
#' # Showing the default list of classes
#' spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#'
#' # Changing the default classification
#' 
#' trap_mf <- function(a, b, c, d) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), 1, (d - x)/(d - c), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' classes <- c("small", "medium", "large")
#' small <- trap_mf(0, 0.3, 0.4, 0.6)
#' medium <- trap_mf(0.4, 0.6, 0.8, 1)
#' large <- trap_mf(0.6, 0.8, 1, 1)
#' 
#' spa_set_classification(classes, c(small, medium, large))
#' 
#' spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#'
#' @export
spa_set_classification <- function(classes, mfs) {
  if(!(length(classes) == length(mfs))) {
    stop("Classes and topological_mfs have different lengths.", call. = FALSE)
  } else if(!is.character(classes)) {
    stop("Classes need to be a character vector.", call. = FALSE)
  } else if(any(sapply(mfs, function(x) !(is.function(x))))) {
    stop("The parameter mfs have to be a list of membership functions.", call. = FALSE)
  }
  pkg_env$ftopological_classes <- classes
  pkg_env$ftopological_mfs <- mfs
}

#' @title Evaluation modes
#'
#' @description This family of functions implements evaluation modes 
#' that returns a Boolean value for a given degree in \[0, 1\] obtained from a membership function of a linguistic value.
#'  
#' @usage
#'
#' soft_eval(degree)
#'
#' @param degree A numerical vector whose values are in \[0, 1\].
#' 
#' @name fsr_eval_modes
#'
#' @details
#'
#' These functions yield a Boolean value that express the meaning of a degree returning from an evaluation of a membership function.
#' That is, the parameter `degree` is a value in \[0, 1\] resulting from evaluation a value in a membership degree.
#' Then, an evaluation mode "translate" the meaning of this degree of truth as a Boolean value.
#' 
#' There some different ways to make this kind of translation:
#' - `soft_eval`: It returns `TRUE` if `degree` is greater than 0.
#' - `strict_eval`: It returns `TRUE` if `degree` is equal to 0.
#' - `alpha_eval`: It returns `TRUE` if `degree` is greater than or equal to another value (named `alpha`).
#' - `soft_alpha_eval`: It returns `TRUE` if `degree` is greater than another value (named `alpha`).
#' 
#' These operators are employed to process the evaluation modes of fuzzy topological relationships that are processed as Boolean predicates.
#' 
#' @return
#'
#' A Boolean vector.
#'
#' @examples
#'
#' x <- c(0.1, 0.3, 0.6, 0.8)
#' 
#' soft_eval(x)
#' strict_eval(x)
#' alpha_eval(x, 0.3)
#' soft_alpha_eval(x, 0.3)
#'
#' @export
soft_eval <- function(degree){
  degree > 0
}

#' @name fsr_eval_modes
#' 
#' @usage 
#' 
#' strict_eval(degree)
#' 
#' @export
strict_eval <- function(degree){
  degree == 1
}

#' @name fsr_eval_modes
#' 
#' @usage 
#' 
#' alpha_eval(degree, alpha)
#' 
#' @param alpha A single numeric value in \[0, 1\].
#' 
#' @export
alpha_eval <- function(degree, alpha){
  degree >= alpha
}

#' @name fsr_eval_modes
#' 
#' @usage 
#' 
#' soft_alpha_eval(degree, alpha)
#'  
#' @export
soft_alpha_eval <- function(degree, alpha){
  degree > alpha
}