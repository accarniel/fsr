#' @title spa_add_component
#'
#' @description
#'
#' @usage
#' spa_add_component()
#' @param spo
#' @param component
#'
#' @details Implementation of \eqn{\alpha}
#'
#' @return
#'
#'
#' @references
#' Carniel, A. C.; Schneider, M.
#' Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types.
#' In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems
#' (FUZZ-IEEE 2018), pp. 1-8, 2018. <https://doi.org/10.1109/FUZZ-IEEE.2018.8491565>
#'
#' @seealso
#'
#' @examples
#'
#' ### exemple 1...
#'
#' ### example 2...
#'
#' @export
spa_add_component <- function(pgeom_obj, components) {
  if(is.null(pgeom_obj)){
    stop("pgeom_obj is null. Please use create_pgeom() to
         create an empty spatial plateau object.")
  }

  if(is.null(components)){
    stop("components is null. It should be a single component or a list of components.")
  }

  if(class(pgeom_obj) != "pgeom"){
    stop(paste(pgeom_obj, "is not a pgeom object (Spatial Plateau Object.)", sep = ' '))
  }

  if(!inherits(components, "list")) {
    components <- list(components)
  }

  #should we check all components or just the first one?
  if(class(components[[1]]) != "component"){
    stop(paste(components, " is not a component object.", sep = ' '))
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



#' @title spa_eval
#'
#' @description
#'
#'
#' @param comp
#'
#' @return
#' @examples
#'
#' @export
#'
spa_eval <- function(obj, point = NA){
  if(inherits(obj, "component")){
    obj@md
  } else if(inherits(obj, "pgeom")){
    if(any(is.na(point))){
      stop("point is NA.")
    }
    ret <- 0
    if(class(point)[[2]] != "POINT"){
      stop("'point' must be a simple point object.")
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


#' @title spa_avg_degree
#' @family Spatial Plateau Metric Operations
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
spa_avg_degree <- function(pgeom){

  get_md <- function(comp){
    comp@md
  }
  mds_vec <- unlist(lapply(pgeom@component, get_md))
  mean(mds_vec)
}


#' @title spa_ncomp
#' @family Spatial Plateau Metric Operations
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
spa_ncomp <- function(pgeom){
  length(pgeom@component)
}

#' @title spa_area
#' @family Spatial Plateau Metric Operations
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
spa_area <- function(pgeom){

  if(pgeom@type!='PLATEAUREGION'){
    stop("Not PLATEAUREGION pgeom.")
  }

  area_comp <- function(comp){
    md_comp = comp@md
    area_obj = st_area(comp@obj)
    area_obj * md_comp
  }

  comps_areas <- unlist(lapply(pgeom@component, area_comp))
  sum(comps_areas)
}


#' @title spa_length
#' @family Spatial Plateau Metric Operations
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
spa_length <- function(pgeom){

  if(pgeom@type!='PLATEAULINE'){
    stop("Not PLATEAULINE pgeom.")
  }

  length_comp <- function(comp){
    md_comp = comp@md
    length_obj = st_length(comp@obj)
    length_obj * md_comp
  }

  components_lenghts <- unlist(lapply(spo@component, length_comp))
  sum(components_lenghts)
}

#' @title spa_perimeter
#' @family Spatial Plateau Metric Operations
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
spa_perimeter <- function(pgeom){

  if(pgeom@type!='PLATEAUREGION'){
    stop("Not PLATEAUREGION pgeom.")
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

#' @title spa_intersection
#' @family Spatial Plateau Set Operations
#' @description
#'
#' \deqn{spaintersection(po_1, po_2, \sigma) = \langle \rangle \odot \bigodot \underset ab}
# \underset ab}
# \underset{1 \leq j \leq n_2}}
# = \newline
# \langle\rangle \odot \bigodot\underset{1 \leq j \leq n_2}{1 \leq i \leq n_1} \newline
# (o_1,_i \otimes o_2,_j), \sigma(m_1,_i,m_2,_j)}
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
spa_intersection <- function(pgeom1, pgeom2, itype="min"){

  if(pgeom1@type != pgeom2@type){
    stop("Different Spatial Plateau Types.")
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



#' @title spa_union
#' @family Spatial Plateau Set Operations
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
#'
spa_union <- function(pgeom1, pgeom2, utype="max"){

  if(pgeom1@type != pgeom2@type){
    stop("Different Spatial Plateau Types.")
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

#' @title spa_difference
#' @family Spatial Plateau Set Operations
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
spa_difference <- function(pgeom1, pgeom2, dtype="f_diff"){

  if(pgeom1@type != pgeom2@type){
    stop("Different Spatial Plateau Types.")
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
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
#'
spa_support <- function(pgeom){
  return(pgeom@supp)
}

#' @title spa_core
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
#'
spa_core <- function(pgeom){

  last_comp <- tail(pgeom@component)

  if(last_comp[[1]]@md == 1){
    return(last_comp@obj)
  }
  sf_type <- get_counter_ctype(pgeom)

  sfg_obj <- switch(sf_type,
                    POINT = st_point(),
                    LINESTRING = st_linestring(),
                    POLYGON = st_polygon())
  sfg_obj
}

#' @title spa_exact_equal
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
#'
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
    # to do - improve comparison method
  }
  return(TRUE)
}


#' @title spa_exact_inside
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
#'
spa_exact_inside <- function(pgeom1, pgeom2){
  spa_exact_equal(spa_intersection(pgeom1, pgeom2), pgeom1)
}



#' @title spa_overlap
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
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

  ret_apply(ret, result, ...)

}

#' @title spa_overlap
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#'
#' @noRd
ret_apply <- function(ret, result, ...){
  args <- list(...)
  switch(ret,
         degree = return(result),
         list = return(spa_eval_relation(result)),
         bool = {
           list_res <- spa_eval_relation(result)
           if(!("eval_mode" %in% names(args) & "lval" %in% names(args))){
             stop("args not supplied. 'eval_mode' and 'lval' needed for bool result type")
           }
           e_mode <- match.fun(args$eval_mode)
           term <- args$lval
           return(e_mode(list_res[[term]]))
         },
         stop("Return type does not exist."))
}


#' @title spa_meet
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
# spa_meet <- function(pgeom1, pgeom2){
#
#   check_spa_topological_condition(pgeom1, pgeom2)
#
#   countour_pgeom1 <- spa_contour(pgeom1)
#   countour_pgeom2 <- spa_contour(pgeom2)
#
#   p <- spa_common_points(countour_pgeom1, countour_pgeom2, itype = "min")
#   c <- spa_intersection(countour_pgeom1, countour_pgeom2, itype = "min")
#
#   p_ncomp <- spa_ncomp(p)
#   c_ncomp <- spa_ncomp(c)
#   p_core <- spa_core(p)
#   c_core <- spa_core(c)
#
#   if((p_ncomp == 1) & !(st_is_empty(p_core)) ||
#      (c_ncomp == 1) & !(st_is_empty(c_core))){
#     return(1)
#   }
#
#   supp_1 <- pgeom1@supp
#   supp_2 <- pgeom2@supp
#
#   pgeom1_core <- spa_core(pgeom1)
#   pgeom2_core <- spa_core(pgeom2)
#
#   if((st_disjoint(supp1, supp2, sparse=FALSE)[1]) ||
#     !(st_disjoint(pgeom1_core, pgeom2_core, sparse=FALSE)[1]) ||
#     st_touches(pgeom1_core, pgeom2_core, sparse=FALSE)[1] ||
#     spa_exact_inside(pgeom1, pgeom2) ||
#     spa_exact_inside(pgeom2, pgeom1) ||
#     spa_exact_equal(pgeom1, pgeom2)){
#
#     return(0)
#   }
#
#   meet_c <- st_touches(supp1, supp2)
#
#   if(#meet em ponto){
#     return(spa_avg_degree(p))
# }
#
#   if(# meet ponto e linha){
#
#
#   }
#
#
# }

#' @title spa_disjoint
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
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
  ret_apply(ret, result, ...)
}

#' @title spa_equal
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
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
  ret_apply(ret, result, ...)
}

#' @title spa_inside
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
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
  ret_apply(ret, result, ...)
}

#' @title spa_contains
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
spa_contains <- function(pgeom1, pgeom2, utype = "max", ret = 'degree', ...){
  spa_inside(pgeom2, pgeom1, utype = utype, ret = 'degree', ...)
}

#' @title spa_common_points
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
spa_common_points <- function(pgeom1, pgeom2, itype = "min"){

  if(pgeom1@type != pgeom2@type){
    stop("Different Spatial Plateau Types.")
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
      if(!st_is_empty(sfg) && st_geometry_type(sfg) %in% c("POINT", "MULTIPOINT")){
        result_comp <- new("component", obj = sfg, md = md)
        lcomps <- append(lcomps, result_comp)
      } else if(!st_is_empty(sfg) && st_geometry_type(sfg) == "GEOMETRYCOLLECTION") {
        type_geom = get_counter_ctype(pgeom)
        result_comp <- new("component", obj = st_union(st_collection_extract(sfg, type = "POINT"))[[1]], md = md)
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

#' @title spa_contour
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
spa_contour <- function(pregion){


  if(pregion@type != "PLATEAUREGION"){
    stop("pgeom must be a PLATEAUREGION type.")
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


#' @title spa_set_classification
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
spa_set_classification <- function(classes, mfs){

  if(!(length(classes) == length(mfs))){
    stop("Topological classes and topological_mfs are different length.")

  } else if(class(classes) != "character"){
    stop("Topological_classes needs to be a 'character' type.")

  } else if(any(sapply(mfs, function(x) !(is.function(x))))){
    stop("Topoligcal_mfs needs to be a list of functions.")
  }

  pkg_env$ftopological_classes <- classes
  pkg_env$ftopological_mfs <- mfs
}


#' @title spa_eval_relation
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @noRd
spa_eval_relation <- function(degree){

  classes <- pkg_env$ftopological_classes
  mfs <- pkg_env$ftopological_mfs

  values_set <- list()
  degrees <- lapply(mfs, function(mf) mf(degree))

  names(degrees) <- classes
  degrees
}


#' @title spa_eval_relation
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @noRd
soft_eval <- function(degree){
  degree > 0
}

#' @title spa_eval_relation
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @noRd
strict_eval <- function(degree){
  degree == 1
}

#' @title alpha_eval
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @noRd
alpha_eval <- function(degree, alpha){
  degree >= alpha
}

#' @title spa_eval_relation
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @noRd
soft_alpha_eval <- function(degree, alpha){
  degree > alpha
}

#' @title spa_eval_relation
#' @family Spatial Plateau Topological Relationships
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
spa_boundary_pregion <- function(pgeom, bound_part = "region"){

  if(pgeom@type != "PLATEAUREGION"){
    stop("pgeom is not a PLATEAUREGION type.")
  }


  if(bound_part == "region"){
    new_pgeom <- create_empty_pgeom("PLATEAUREGION")
    last_comp <- tail(pgeom@component)
    if(last_comp@md == 1){
      boundary_component <- st_boundary(last_comp@obj)
      new_pgeom <- spa_add_component(new_pgeom, boundary_component)
    }

  }
  else if(bound_part == "line"){
    new_pgeom <- create_empty_pgeom("PLATEAULINE")

    bpl
  }

  else{
    stop("Bound part invalid.")
  }

}



