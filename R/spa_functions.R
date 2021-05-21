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
spa_eval <- function(obj, point=NA){

  if(inherits(obj, "component")){
    md <- obj@md
    return(md)
  } else if(inherits(obj, "pgeom")){

    if(is.na(point)){
      stop("point is NA.")
    }

    if(class(point)[[2]]!="POINT"){
      stop("'point' must be a simple point object.")
    }

    if(st_intersects(point, obj@supp, sparse=FALSE)[1]){

      ################################
      # check in the boundary
      md_comps <- c()
      for(component in obj@component){
        if(st_intersects(point, st_boundary(component@obj), sparse=FALSE)[1]){
          md_point <- get_md(component)
          md_comps <- append(md_comps, md_point)
        } #  check in its interior...
        else if(st_intersects(point, component@obj, sparse=FALSE)[1]){
          md_point <- get_md(component)
          return(md_point)
        }
      }
      return(max(md_comps))
    }
    return(0)

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
spa_overlap <- function(pgeom1, pgeom2, itype = "min"){

  if(pgeom1@type != pgeom2@type){
    stop("Different Spatial Plateau Types.")
  } else if(pgeom1@type != "PLATEAUREGION"){
    stop(paste("Operator not implemented to", pgeom1@type))
  }

  r = spa_intersection(pgeom1, pgeom2, itype = itype)
  supp_pgeom1 <- pgeom1@supp
  supp_pgeom2 <- pgeom2@supp

  if(spa_ncomp(r) == 1 && !(st_is_empty(spa_core(r)))){
    return(1)
  } else if(st_disjoint(supp_pgeom1, supp_pgeom2, sparse=FALSE)[1] ||
            st_touches(supp_pgeom1, supp_pgeom2, sparse=FALSE)[1] ||
            spa_exact_inside(pgeom1, pgeom2) ||
            spa_exact_inside(pgeom2, pgeom1) ||
            spa_exact_equal(pgeom2, pgeom1)) {
    return(0)
  } else {
    return(spa_area(r)/st_area(st_intersection(supp_pgeom1, supp_pgeom2)))
  }
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
spa_meet <- function(pgeom1, pgeom2){

}

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
spa_disjoint <- function(pgeom1, pgeom2, )
