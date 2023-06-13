#' Checks if an `sfg` object is compatible with a given spatial plateau data type
#'
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

#' Computes the geometric union of a list of components (i.e., support) for a given type of spatial plateau object. 
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
  
  list(new_components, supp[[1]])
}

#' Captures all spatial plateau objects from a `pcollection` object as a list
#' 
#' @noRd
get_pgos_from_pcollection <- function(pcol) {
  pgos <- list()
  for(pgo in pcol@pgos) {
    type <- spa_get_type(pgo)
    if(type == "PLATEAUCOLLECTION"){
      pgos <- append(pgos, get_pgos_from_pcollection(pgo))
    } else {
      pgos <- append(pgos, pgo)
    }
  }
  pgos
}

#' Generates a trapezoidal membership function
#' 
#' @noRd
trap_mf <- function(a, b, c, d, h = 1) {
  function(x) {
    pmax(pmin((x - a)/(b - a), h, (d - x)/(d - c), na.rm = TRUE), 0)
  }
}