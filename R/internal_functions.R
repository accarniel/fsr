#' search_by_md finds the index of the component of a plateau plateau object based on a given membership degree
#'
#' This function uses an interactive version of the binary search algorithm.
#'
#' @param components List of components
#' @param low First index of the list to start the binary search
#' @param high Last index of the list to end the binary search
#' @param m Membership degree to be located
#'
#' @return If found, returns a vector with TRUE and the index position.
#' If not found, returns a vector with FALSE and the first position given (low)
#'
#' @import dplyr
#' @noRd
search_by_md <- function(components, low, high, m){
  while(low <= high){
    mid = floor((low + high)/2)
    if(near(m, components[[mid]]@md)){
      return(c(TRUE,mid))
    } else if(m < components[[mid]]@md){
      high = mid - 1
    } else{
      low = mid + 1
    }
  }
  return(c(FALSE, low))
}

#' @import sf
#' @noRd
is_compatible <- function(sfg, ptype) {
  ptype = toupper(ptype)
  ret <- FALSE
  if(class(sfg)[1] == "XY") {
    sfg_type = class(sfg)[2]
    if((sfg_type == "POINT" || sfg_type == "MULTIPOINT") && 
       ptype == "PLATEAUPOINT") {
        ret <- TRUE
    } else if((sfg_type == "LINESTRING" || sfg_type == "MULTILINESTRING") &&
               ptype=="PLATEAULINE") {
        ret <- TRUE
    } else if((sfg_type == "POLYGON" || sfg_type == "MULTIPOLYGON") &&
              ptype=="PLATEAUREGION") {
        ret <- TRUE
    }
  } else{
    stop("Component is not a sfg data type", call. = FALSE)
  }
  ret
}

#' @noRd
is_pgeometry <- function(type) {
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION", 
                 "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
    TRUE
  } else {
    FALSE
  }
}


#' @noRd
get_counter_ctype <- function(pgo){
  ptype <- pgo@type
  
  type <- switch(ptype,
                 PLATEAUPOINT = "POINT",
                 PLATEAULINE = "LINESTRING",
                 PLATEAUREGION = "POLYGON")
  
  type
}

#' @import sf methods
#' @noRd
append_valid_comps <- function(sfg, pgo, md, lcomps){
  if(!st_is_empty(sfg) && is_compatible(sfg, pgo@type)){
    result_comp <- new("component", obj = sfg, md = md)
    lcomps <- append(lcomps, result_comp)
  } else if(!st_is_empty(sfg) && st_geometry_type(sfg) == "GEOMETRYCOLLECTION") {
    type_geom = get_counter_ctype(pgo)
    union_obj <- st_union(st_collection_extract(sfg, type = type_geom))
    if(inherits(union_obj, "sfc")) {
      union_obj <- union_obj[[1]]
    }
    result_comp <- new("component", obj = union_obj, md = md)
    lcomps <- append(lcomps, result_comp)
  }
  lcomps
}

#' @noRd
check_spa_topological_condition <- function(pgo1, pgo2){
  if(pgo1@type != pgo2@type){
    stop("The spatial plateau objects have different types.", call. = FALSE)
  } else if(pgo1@type != "PLATEAUREGION" || pgo2@type != "PLATEAUREGION") {
    stop(paste0("This operator is not implemented to (", pgo1@type, " x ", pgo2@type, ") yet."), call. = FALSE)
  }
}