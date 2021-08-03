#' @import sf
#' @export
spa_add_component <- function(pgeom_obj, components) {
  if(is.null(pgeom_obj)){
    stop("pgeom_obj is null. Please use create_pgeom() to
         create an empty spatial plateau object.", call. = FALSE)
  }

  if(is.null(components)){
    stop("components is null. It should be a single component or a list of components.", call. = FALSE)
  }

  if(class(pgeom_obj) != "pgeom"){
    stop(paste(pgeom_obj, "is not a pgeom object (Spatial Plateau Object.)", sep = ' '), call. = FALSE)
  }

  if(!inherits(components, "list")) {
    components <- list(components)
  }

  #should we check all components or just the first one?
  if(class(components[[1]]) != "component"){
    stop(paste(components, " is not a component object.", sep = ' '), call. = FALSE)
  }


  for(component in components) {
    c <- component@obj
    m <- component@md

    # does nothing, lets check the next component
    if(is.null(c) || st_is_empty(c) || m == 0){
      next
    }

    # 2. if the pgeom_obj is empty and the component is not empty and has a membership greater than 0
    if(pgeom_is_empty(pgeom_obj) && !(is.null(c) || st_is_empty(c)) && m > 0){
      pgeom_obj@component[[1]] <- component
      pgeom_obj@supp <- c

    } else if(!is.null(c) & length(c) >= 1){
      index = search_by_md(pgeom_obj@component, 1, length(pgeom_obj@component), m)

      # 3. if the membership degree exists in the pgeom_obj, we should merge it
      if(index[1] == TRUE){
        pgeom_obj@component[[index[2]]]@obj <- st_union(pgeom_obj@component[[index[2]]]@obj, c)
      } else {
        #otherwise, we simply append into the correct location
        pgeom_obj@component <- append(pgeom_obj@component, component, after=index[2]-1)
      }
      #in both cases we update its support
      pgeom_obj@supp <- st_union(pgeom_obj@supp, c)
    }
  }

  pgeom_obj
}

#' @import sf
#' @export
spa_eval <- function(obj, point = NA){
  if(inherits(obj, "component")){
    obj@md
  } else if(inherits(obj, "pgeom")){
    if(any(is.na(point))){
      stop("point is NA.", call. = FALSE)
    }
    ret <- 0
    if(class(point)[[2]] != "POINT"){
      stop("'point' must be a simple point object.", call. = FALSE)
    }

    if(st_intersects(point, obj@supp, sparse = FALSE)[1]){
      # check in the boundary
      md_comps <- c()
      for(component in obj@component){
        if(st_intersects(point, st_boundary(component@obj), sparse = FALSE)[1]){
          md_comps <- append(md_comps, component@md)
        } #  check in its interior...
        else if(st_intersects(point, component@obj, sparse = FALSE)[1]){
          if(obj@type %in% c("PLATEAUPOINT", "PLATEAUREGION")){
            return(component@md)
          } else{
            md_comps <- append(md_comps, component@md)
          }

        }
      }
      ret <- max(md_comps)
    }
    ret
  }
}

#' @export
spa_avg_degree <- function(pgeom){
  get_md <- function(comp){
    comp@md
  }
  mds_vec <- unlist(lapply(pgeom@component, get_md))
  mean(mds_vec)
}

#' @export
spa_ncomp <- function(pgeom){
  length(pgeom@component)
}

#' @import sf
#' @export
spa_area <- function(pgeom){

  if(pgeom@type!='PLATEAUREGION'){
    stop("This type is not a PLATEAUREGION object.", call. = FALSE)
  }

  area_comp <- function(comp){
    md_comp = comp@md
    area_obj = st_area(comp@obj)
    area_obj * md_comp
  }

  comps_areas <- unlist(lapply(pgeom@component, area_comp))
  sum(comps_areas)
}

#' @import sf
#' @export
spa_length <- function(pgeom){

  if(pgeom@type!='PLATEAULINE'){
    stop("This type is not a PLATEAULINE object.", call. = FALSE)
  }

  length_comp <- function(comp){
    md_comp = comp@md
    length_obj = st_length(comp@obj)
    length_obj * md_comp
  }

  components_lenghts <- unlist(lapply(pgeom@component, length_comp))
  sum(components_lenghts)
}

#' @import sf lwgeom
#' @export
spa_perimeter <- function(pgeom){

  if(pgeom@type!='PLATEAUREGION'){
    stop("Not PLATEAUREGION pgeom.", call. = FALSE)
  }

  perimeter_comp <- function(comp){
    md_comp = comp@md
    temp <- st_sfc(comp@obj)
    st_set_crs(temp, 4326)
    perimeter_obj = st_perimeter(temp)
    perimeter_obj * md_comp
  }

  comps_perimeter <- unlist(lapply(pgeom@component, perimeter_comp))
  sum(comps_perimeter)
}

#' @import sf
#' @export
spa_intersection <- function(pgeom1, pgeom2, itype="min"){

  if(pgeom1@type != pgeom2@type){
    stop("Different spatial plateau data types.", call. = FALSE)
  }

  sigma <- match.fun(itype)
  result <- create_empty_pgeom(pgeom1@type)
  lcomps <- vector("list")

  for(comp1 in pgeom1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md

    for(comp2 in pgeom2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md

      result_md = sigma(md_comp_p1, md_comp_p2)

      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)

      # check geom and pgeom
      lcomps <- check_geom_sfg_pgeom(sf_result, result , result_md, lcomps)
      }
  }

  if(length(lcomps) > 0){
    spa_add_component(result, lcomps)
  } else {
    result
  }
}

#' @import sf
#' @export
spa_union <- function(pgeom1, pgeom2, utype="max"){

  if(pgeom1@type != pgeom2@type){
    stop("Different spatial plateau data types.", call. = FALSE)
  }

  tau <- match.fun(utype)
  result <- create_empty_pgeom(pgeom1@type)
  lcomps <- vector("list")

  supp_intersected <- st_intersection(pgeom1@supp, pgeom2@supp)

  for(comp1 in pgeom1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md

    for(comp2 in pgeom2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md

      result_md = tau(md_comp_p1, md_comp_p2)

      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)

      lcomps <- check_geom_sfg_pgeom(sf_result, result , result_md, lcomps)
    }

    sf_diff_1 <- st_difference(obj_comp_p1, supp_intersected)
    # check result type and appends to list if compatible
    lcomps <- check_geom_sfg_pgeom(sf_diff_1, result, md_comp_p1, lcomps)
  }

  for(comp2 in pgeom2@component){
    obj_comp_p2 <- comp2@obj
    md_comp_p2 <- comp2@md

    sf_diff_3 <- st_difference(obj_comp_p2, supp_intersected)
    # check result type and appends to list if compatible
    lcomps <- check_geom_sfg_pgeom(sf_diff_3, result, md_comp_p2, lcomps)
  }

  if(length(lcomps) > 0){
    spa_add_component(result, lcomps)
  } else {
    result
  }
}

#' @import sf
#' @export
spa_difference <- function(pgeom1, pgeom2, dtype="f_diff"){

  if(pgeom1@type != pgeom2@type){
    stop("Different spatial plateau data types.", call. = FALSE)
  }

  nu <- match.fun(dtype)
  result <- create_empty_pgeom(pgeom1@type)
  lcomps <- vector("list")

  supp_intersected <- st_intersection(pgeom1@supp, pgeom2@supp)

  for(comp1 in pgeom1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md

    for(comp2 in pgeom2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md

      result_md = nu(md_comp_p1, md_comp_p2)

      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)

      lcomps <- check_geom_sfg_pgeom(sf_result, result , result_md, lcomps)
    }

    sf_diff_1 <- st_difference(obj_comp_p1, supp_intersected)

    # check result type and appends to list if compatible
    lcomps <- check_geom_sfg_pgeom(sf_diff_1, result, md_comp_p1, lcomps)
  }

  if(length(lcomps) > 0){
    spa_add_component(result, lcomps)
  } else {
    result
  }
}

#' @title spa_support
#'
#' @description spa_support yields a crisp spatial object (as a `sfg` object) that corresponds to the support of a `pgeom` object given as input
#'
#' @usage
#'
#' spa_support(pgeom)
#'
#' @param pgeom A `pgeom` object (i.e., plateau geometry object) of any type.
#'
#' @details
#'
#' It employs the classical definition of _support_ from the fuzzy set theory in the context of spatial plateau algebra. 
#' The _support_ only comprises the points with membership degree greater than or equal to 1.
#' Hence, this operation returns the `sfg` object that represents the total extent of the `pgeom` given as input. 
#' If the `pgeom` object has no components, then an empty `sfg` object is returned (i.e., a crisp spatial object without points).
#'
#' @return
#'
#' An `sfg` object that represents the support of `pgeom`. It can be an empty object if `pgeom` is empty.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
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
#' cp3 <- component_from_sfg(st_multipoint(pts3), 1.0)
#' 
#' pp <- create_pgeom(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' pp
#' 
#' pp_supp <- spa_support(pp)
#' pp_supp
#' 
#' pp_empty <- create_empty_pgeom("PLATEAUPOINT")
#' pp_empty_supp <- spa_support(pp_empty)
#' pp_empty_supp
#'
#' @export
spa_support <- function(pgeom){
  return(pgeom@supp)
}

#' @title spa_core
#'
#' @description spa_core yields a crisp spatial object (as a `sfg` object) that corresponds to the core of a `pgeom` object given as input
#'
#' @usage
#'
#' spa_core(pgeom)
#'
#' @param pgeom A `pgeom` object (i.e., plateau geometry object) of any type.
#'
#' @details
#'
#' It employs the classical definition of _core_ from the fuzzy set theory in the context of spatial plateau algebra. 
#' The _core_ only comprises the points with membership degree equal to 1.
#' Hence, this operation returns the `sfg` object that represents the component labeled with 
#' membership degree equal to 1 of the `pgeom` given as input. If the `pgeom` object has no core, then an empty `sfg` object is returned (i.e., a crisp spatial object without points).
#'
#' @return
#'
#' An `sfg` object that represents the core of `pgeom`. It can be an empty object if `pgeom` has no a component with membership degree 1.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
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
#' cp3 <- component_from_sfg(st_multipoint(pts3), 1.0)
#' 
#' pp <- create_pgeom(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' pp
#' 
#' pp_core <- spa_core(pp)
#' pp_core
#'
#' #Creating a pgeom object without core
#' pp2 <- create_pgeom(list(cp1, cp2), "PLATEAUPOINT")
#' pp2
#' 
#' spa_core(pp2)
#'
#' @import sf utils
#' @export
spa_core <- function(pgeom){

  last_comp <- tail(pgeom@component, 1)

  if(last_comp[[1]]@md == 1){
    return(last_comp[[1]]@obj)
  }
  sf_type <- get_counter_ctype(pgeom)

  sfg_obj <- switch(sf_type,
                    POINT = st_point(),
                    LINESTRING = st_linestring(),
                    POLYGON = st_polygon())
  sfg_obj
}

#' @import sf
#' @export
spa_exact_equal <- function(pgeom1, pgeom2){

  comp_check <- function(comp1, comp2){
    if(st_equals(comp1@obj, comp2@obj, sparse=FALSE)[1] && (comp1@md == comp2@md)){
      return(TRUE)
    }
    return(FALSE)
  }

  if((pgeom1@type != pgeom2@type) ||
     (spa_ncomp(pgeom1) != spa_ncomp(pgeom2)) ||
     (!(st_equals(pgeom1@supp, pgeom2@supp, sparse=FALSE)[1]))){
    return(FALSE)
  } else {
    for(i in 1:spa_ncomp(pgeom1)){
      if(!(comp_check(pgeom1@component[[i]], pgeom2@component[[i]]))){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#' @export
spa_exact_inside <- function(pgeom1, pgeom2){
  spa_exact_equal(spa_intersection(pgeom1, pgeom2), pgeom1)
}

#' @noRd
spa_eval_relation <- function(ret, result, ...){
  
  aux_function <- function(degree){
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
           if(!("eval_mode" %in% names(args) & "lval" %in% names(args))){
             stop("args not supplied. 'eval_mode' and 'lval' needed for bool result type", call. = FALSE)
           }
           e_mode <- match.fun(args$eval_mode)
           term <- args$lval
           return(e_mode(list_res[[term]]))
         },
         stop("Return type does not exist.", call. = FALSE))
}

#' @import sf
#' @export
spa_overlap <- function(pgeom1, pgeom2, itype = "min", ret = "degree", ...){

  check_spa_topological_condition(pgeom1, pgeom2)

  r = spa_intersection(pgeom1, pgeom2, itype = itype)
  supp_pgeom1 <- pgeom1@supp
  supp_pgeom2 <- pgeom2@supp

  result <- 0

  if(spa_ncomp(r) == 1 && !(st_is_empty(spa_core(r)))){
    result <- 1
  } else if(st_disjoint(supp_pgeom1, supp_pgeom2, sparse=FALSE)[1] ||
            st_touches(supp_pgeom1, supp_pgeom2, sparse=FALSE)[1] ||
            spa_exact_inside(pgeom1, pgeom2) ||
            spa_exact_inside(pgeom2, pgeom1) ||
            spa_exact_equal(pgeom2, pgeom1)) {
    result <- 0
  } else {
    result <- spa_area(r)/st_area(st_intersection(supp_pgeom1, supp_pgeom2))
  }

  spa_eval_relation(ret, result, ...)

}

#' @import sf
#' @export
spa_meet <- function(pgeom1, pgeom2, itype = "min", ret = "degree", ...){

  check_spa_topological_condition(pgeom1, pgeom2)

  countour_pgeom1 <- spa_contour(pgeom1)
  countour_pgeom2 <- spa_contour(pgeom2)

  p <- spa_common_points(countour_pgeom1, countour_pgeom2, itype = itype)
  c <- spa_intersection(countour_pgeom1, countour_pgeom2, itype = itype)

  p_ncomp <- spa_ncomp(p)
  c_ncomp <- spa_ncomp(c)
  p_core <- spa_core(p)
  c_core <- spa_core(c)

  if((p_ncomp == 1) & !(st_is_empty(p_core)) ||
     (c_ncomp == 1) & !(st_is_empty(c_core))){
    result <- 1
  }

  supp_1 <- pgeom1@supp
  supp_2 <- pgeom2@supp

  pgeom1_core <- spa_core(pgeom1)
  pgeom2_core <- spa_core(pgeom2)

  if((st_disjoint(supp_1, supp_2, sparse=FALSE)[1]) ||
    !(st_disjoint(pgeom1_core, pgeom2_core, sparse=FALSE)[1]) ||
    st_touches(pgeom1_core, pgeom2_core, sparse=FALSE)[1] ||
    spa_exact_inside(pgeom1, pgeom2) ||
    spa_exact_inside(pgeom2, pgeom1) ||
    spa_exact_equal(pgeom1, pgeom2)){

    result <- 0
  }

  if(st_relate(supp_1, supp_2, pattern = "F**0*****") ||
     st_relate(supp_1, supp_2, pattern = "F***0****") ||
     st_relate(supp_1, supp_2, pattern = "F0*******")){
    result <- spa_avg_degree(p)
  } else if (st_relate(supp_1, supp_2, pattern = "F**1*****") ||
             st_relate(supp_1, supp_2, pattern = "F***1****") ||
             st_relate(supp_1, supp_2, pattern = "F1*******")){
    pgeom1_boundary <- spa_boundary_pregion(pgeom1, bound_part = 'line')
    pgeom2_boundary <- spa_boundary_pregion(pgeom2, bound_part = 'line')
    bl <- spa_intersection(pgeom1_boundary, pgeom2_boundary, itype = itype)

    plength <- spa_length(bl)
    length_support <- length(bl@supp)

    result <- plength/length_support

  } else {
    pgeom1_boundary <- spa_boundary_pregion(pgeom1, bound_part = 'region')
    pgeom2_boundary <- spa_boundary_pregion(pgeom2, bound_part = 'region')
    br <- spa_intersection(pgeom1_boundary, pgeom2_boundary, itype = itype)
    br_area <- spa_area(br)
    area_support <- st_area(br@supp)

    result <- br_area/area_support

  }
  spa_eval_relation(ret, result, ...)
}

#' @import sf
#' @export
spa_disjoint <- function(pgeom1, pgeom2, itype="min", ret = "degree", ...){

  check_spa_topological_condition(pgeom1, pgeom2)

  supp_pgeom1 <- pgeom1@supp
  supp_pgeom2 <- pgeom2@supp
  result <- 0

  if(st_disjoint(supp_pgeom1, supp_pgeom2, sparse=FALSE)[1]){
    result <- 1
  }

  r_overlap <- spa_overlap(pgeom1, pgeom2, itype = itype)
  r_meet <- spa_meet(pgeom1, pgeom2)

  if((r_overlap == 1) || (r_meet == 1) ||
      spa_exact_inside(pgeom1, pgeom2) ||
      spa_exact_inside(pgeom2, pgeom1) ||
      spa_exact_equal(pgeom2, pgeom1))  {
    result <- 0
  } else {
    result <- 1 - max(r_overlap, r_meet)
  }
  spa_eval_relation(ret, result, ...)
}

#' @import sf
#' @export
spa_equal <- function(pgeom1, pgeom2, utype = "max", ret = 'degree', ...){

  check_spa_topological_condition(pgeom1, pgeom2)

  if(spa_exact_equal(pgeom1, pgeom2)){
    result <- 1
  }

  supp_pgeom1 <- pgeom1@supp
  supp_pgeom2 <- pgeom2@supp

  if(st_disjoint(supp_pgeom1, supp_pgeom2, sparse=FALSE)[1] ||
     st_touches(supp_pgeom1, supp_pgeom2, sparse=FALSE)[1]){
    result <- 0
  } else {
    r_diff <- spa_difference(pgeom1, pgeom2, dtype="f_symm_diff")
    r_union <- spa_union(pgeom1, pgeom2, utype = utype)

    r_spa_area <- spa_area(r_diff)
    r_sfg_area <- st_area(r_union)

    result <- 1 - (r_spa_area/r_sfg_area)
  }
  spa_eval_relation(ret, result, ...)
}

#' @import sf
#' @export
spa_inside <- function(pgeom1, pgeom2, utype = "max", ret = 'degree', ...){

  check_spa_topological_condition(pgeom1, pgeom2)

  if(spa_exact_inside(pgeom1, pgeom2)){
    result <- 1
  }

  supp_pgeom1 <- pgeom1@supp
  supp_pgeom2 <- pgeom2@supp

  if(spa_equal(pgeom1, pgeom2, utype = utype) == 1 ||
     st_disjoint(supp_pgeom1, supp_pgeom2) ||
     st_touches(supp_pgeom1, supp_pgeom2)){
    result <- 0
  } else {

    r_diff <- spa_difference(pgeom1, pgeom2, dtype = "f_bound_diff")

    result <- 1 - (spa_area(r_diff)/st_area(supp_pgeom1))
  }
  spa_eval_relation(ret, result, ...)
}

#' @export
spa_contains <- function(pgeom1, pgeom2, utype = "max", ret = 'degree', ...){
  spa_inside(pgeom2, pgeom1, utype = utype, ret = 'degree', ...)
}

#' @import sf methods
#' @export
spa_common_points <- function(pgeom1, pgeom2, itype = "min"){

  if(pgeom1@type != pgeom2@type){
    stop("Different Spatial Plateau Types.", call. = FALSE)
  }

  sigma <- match.fun(itype)
  result <- create_empty_pgeom(pgeom1@type)
  lcomps <- vector("list")

  for(comp1 in pgeom1@component){
    obj_comp_p1 <- comp1@obj
    md_comp_p1 <- comp1@md

    for(comp2 in pgeom2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md

      result_md = sigma(md_comp_p1, md_comp_p2)

      sf_result <- st_intersection(obj_comp_p1, obj_comp_p2)

      if(!st_is_empty(sf_result) && st_geometry_type(sf_result) %in% c("POINT", "MULTIPOINT")){
        result_comp <- new("component", obj = sf_result, md = result_md)
        lcomps <- append(lcomps, result_comp)
      } else if(!st_is_empty(sf_result) && st_geometry_type(sf_result) == "GEOMETRYCOLLECTION") {
        type_geom = get_counter_ctype(pgeom1)
        result_comp <- new("component", obj = st_union(st_collection_extract(sf_result, type = "POINT"))[[1]], md = result_md)
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

#' @import  sf
#' @export
spa_contour <- function(pregion){
  if(pregion@type != "PLATEAUREGION"){
    stop("pgeom must be a PLATEAUREGION type.", call. = FALSE)
  }

  pregion_tibble <- pgeom_as_tibble(pregion)
  pregion_tibble$boundary <- st_boundary(pregion_tibble$geometry)

  pregion_df <- as.data.frame(pregion_tibble)
  create_pgeom(pregion_df[,c(1,3)], "PLATEAULINE")
}

pkg_env <- new.env()
pkg_env$ftopological_classes <- c("a little bit", "somewhat", "slightly", "averagely", "mostly","quite")

pkg_env$ftopological_mfs <- c(FuzzyR::genmf("trapmf", c(0, 0, 0.3, 0.8)),
                              FuzzyR::genmf("trapmf", c(0.3, 0.8, 1.3, 1.5)),
                              FuzzyR::genmf("trapmf", c(1.1, 1.5, 1.6, 2.0)),
                              FuzzyR::genmf("trapmf", c(1.8, 2.1, 2.5, 2.9)),
                              FuzzyR::genmf("trapmf", c(2.7, 3.1, 3.6, 3.9)),
                              FuzzyR::genmf("trapmf", c(3.8, 4.1, 4.5, 4.5)))

#' @export
spa_set_classification <- function(classes, mfs){

  if(!(length(classes) == length(mfs))){
    stop("Classes and topological_mfs have different lengths.", call. = FALSE)
  } else if(class(classes) != "character"){
    stop("Classes need to be a character vector.", call. = FALSE)
  } else if(any(sapply(mfs, function(x) !(is.function(x))))){
    stop("The parameter mfs have to be a list of functions (generated by FuzzyR::genmf).", call. = FALSE)
  }

  pkg_env$ftopological_classes <- classes
  pkg_env$ftopological_mfs <- mfs
}

#' @noRd
soft_eval <- function(degree){
  degree > 0
}

#' @noRd
strict_eval <- function(degree){
  degree == 1
}

#' @noRd
alpha_eval <- function(degree, alpha){
  degree >= alpha
}

#' @noRd
soft_alpha_eval <- function(degree, alpha){
  degree > alpha
}

#' @import methods utils sf
#' @export
spa_boundary_pregion <- function(pgeom, bound_part = "region"){

  if(pgeom@type != "PLATEAUREGION"){
    stop("pgeom is not a PLATEAUREGION object.", call. = FALSE)
  }

  if(bound_part == "line"){
    bpl <- create_empty_pgeom("PLATEAULINE")
    last_comp <- tail(pgeom@component, 1)
    if(last_comp[[1]]@md == 1){
      boundary_component <- st_boundary(last_comp@obj)
      comp_line <- new("component", obj = boundary_component, md=1)
      bpl <- spa_add_component(bpl, comp_line)
    }
    return(bpl)
  }
  else if(bound_part == "region"){
    bpr <- create_empty_pgeom("PLATEAUREGION")
    last_comp <- tail(pgeom@component, 1)
    components <- pgeom@component
    n_comps <- spa_ncomp(pgeom)
    if(last_comp[[1]]@md == 1 && n_comps > 1){
      bpr <- spa_add_component(bpr, head(components, n=n_comps-1))
    }
    return(bpr)
  }
  else{
    stop("Invalid value for the parameter 'bound_part'.", call. = FALSE)
  }
}
