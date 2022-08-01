#' This function computes the geometric union of a list of components (i.e., support) for a given type of spatial plateau object. 
#' It throws an error if any component is not compatible with the given spatial plateau data type.
#' 
#' @import sf
#' @noRd
compute_support <- function(components, type) {
  
  md_value <- c()
  obj_sf <- list()
  
  for(comp in 1:length(components)) {
    md <- components[[comp]]@md
    obj_comp <- components[[comp]]@obj
    
    if(!is_compatible(obj_comp, type)) {
      stop("Input component type error. Please verify if your component type is correct.", call. = FALSE)
    }
    if(md > 0 && md <= 1) {
      md_value[comp] <- md
    } else {
      stop("There is a component with a invalid membership degree.", call. = FALSE)
    }
    obj_sf[[comp]] <- obj_comp
  }
  order_comps <- order(md_value)
  new_components <- components[order_comps]
  
  supp <- st_union(st_sfc(obj_sf))
  
  return(list(new_components, supp[[1]]))
}

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
  ptype <- toupper(ptype)
  ret <- FALSE
  if(inherits(sfg, "sfg")) {
    if(inherits(sfg, c("POINT", "MULTIPOINT")) && ptype == "PLATEAUPOINT") {
      ret <- TRUE
    } else if(inherits(sfg, c("LINESTRING", "MULTILINESTRING")) && ptype == "PLATEAULINE") {
      ret <- TRUE
    } else if(inherits(sfg, c("POLYGON", "MULTIPOLYGON")) && ptype == "PLATEAUREGION") {
      ret <- TRUE
    } else if(inherits(sfg, c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")) &&
              (ptype == "PLATEAUCOMPOSITION" || ptype == "PLATEAUCOLLECTION")) {
      ret <- TRUE
    }
  } else {
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
is_list_pgos <- function(x) {
  types <- lapply(x, function(pgo){paste0("PLATEAU", 
                                          substr(toupper(is(pgo)[1]), 2, nchar(toupper(is(pgo)[1]))))})
  all(unlist(lapply(types, is_pgeometry)))
}

#' @noRd
is_list_components <- function(x) {
  types <- lapply(lapply(x, is), function(x) x[[1]])
  all(unlist(types) == "component")
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