#' @title Adding components to a `pgeometry` object
#'
#' @description This function adds components to a spatial plateau object (i.e., `pgeometry` object). 
#' The crisp spatial object of the component must be compatible with the type of the plateau spatial object.
#' For instance, a `pgeometry` object of the type `PLATEAUREGION` accepts only components containing polygons (e.g., `POLYGON` or `MULTIPOLYGON`). 
#' In the case of `PLATEAUCOMPOSITION` object, formed by a triple (`PLATEAUPOINT`, `PLATEAULINE` and `PLATEAUREGION`), any type of component is compatible to be added. 
#' Thus, a component of type `POINT`, for example, will be added to the `PLATEAUPOINT` in which it is composed. 
#' On the other hand, as a `PLATEAUCOLLECTION` object can have multiple spatial objects of the same type, this function is not applicable to it.
#'
#' @usage
#'
#' spa_add_component(pgo, components, is_valid = FALSE)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param components A `component` object or a list of `component` objects.
#' @param is_valid A Boolean value to check if the user wants to validate the updated spatial plateau object at the end. If `is_valid = TRUE`, it calls `validObject` method.
#'
#' @details
#'
#' This function implements the \eqn{\odot}{odot} operator defined by Spatial Plateau Algebra.
#' The goal of this function is to insert a component or a list of components into a `pgeometry` object. 
#' This insertion is based on the membership degree of the component (e.g., created by `component_from_sfg`). Thus, it preserves the properties of a spatial plateau object.
#' However, this function assumes that a component is compatible with the `pgeometry` object and its geometric format is valid (i.e., it does not overlap with existing components).
#'  
#' @return
#'
#' A `pgeometry` object containing the `component` objects.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' library(sf)
#' 
#' # For a `PLATEAUPOINT` object.
#'
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- component_from_sfg(st_multipoint(pts1), 0.2) 
#' comp2 <- component_from_sfg(st_point(c(1, 5)), 0.8)  
#' 
#' # appending these components into an empty pgeometry object
#' 
#' pp <- create_empty_pgeometry("PLATEAUPOINT")
#' pp <- spa_add_component(pp, list(comp1, comp2))
#' pp
#' 
#' # For a `PLATEAUCOMPOSITION` object.
#' 
#' pts <- rbind(c(1, 2), c(3, 2))
#' pcp <- component_from_sfg(st_multipoint(pts), 0.3)
#' pc <- create_pgeometry(list(pcp), "PLATEAUCOMPOSITION")
#' pc
#' 
#' # appending these components into a pgeometry object
#' 
#' lpts <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lcp <- component_from_sfg(st_linestring(lpts), 0.4)
#' pc <- spa_add_component(pc, list(lcp))
#' pc
#'
#' @import  sf
#' @export
spa_add_component <- function(pgo, components, is_valid = FALSE) {
  
  if(is.null(pgo)){
    stop("pgo is null. Please use create_empty_pgeometry() to
         create an empty spatial plateau object.", call. = FALSE)
  }

  if(is.null(components)){
    stop("components is null. It should be a single component or a list of components.", call. = FALSE)
  }
  
  type <- spa_get_type(pgo)
  
  if(type == "PLATEAUCOLLECTION"){
    stop("It is not possible to add components to a spatial plateau collection object.", call. = FALSE)
  }

  if(!inherits(components, "list")) {
    components <- list(components)
  }
  
  types <- sapply(lapply(components, is), function(x) x[[1]])
  components <- components[(types == "component")]

  pgo <- spa_add_internal(pgo, components)
  
  if(is_valid){
    validObject(pgo)
  }
  pgo
}

#' @import  sf
#' @noRd
spa_add_internal <- function(pgo, components) {
  
  type <- spa_get_type(pgo)
  
  for(component in components) {
    
    c <- component@obj
    m <- component@md
    
    # does nothing, lets check the next component
    if(is.null(c) || st_is_empty(c) || m == 0){
      next
    }
    
    if(type == "PLATEAUCOMPOSITION"){
      if(is_compatible(c, "PLATEAUPOINT")){
        spobj  <- pgo@ppoint
      } else if(is_compatible(c, "PLATEAULINE")){
        spobj  <- pgo@pline
      } else if(is_compatible(c, "PLATEAUREGION")){
        spobj <- pgo@pregion
      } else{
        warning(paste(c, " is not compatible with the spatial plateau object; we will skip this component.", sep = ' '), call. = FALSE)
        next
      }
    } else{
      if(!is_compatible(c, type)){
        warning(paste(c, " is not compatible with the spatial plateau object; we will skip this component.", sep = ' '), call. = FALSE)
        next
      } else{
        spobj <- pgo
      }
    }
    
    # 2. if the pgo is empty and the component is not empty and has a membership greater than 0
    if(fsr_is_empty(spobj) && !(is.null(c) || st_is_empty(c)) && m > 0){
      
      spobj@component[[1]] <- component
      spobj@supp <- c 
      
    } else if(!is.null(c) && length(spobj@component) >= 1){
      index = search_by_md(spobj@component, 1, length(spobj@component), m)
      
      # 3. if the membership degree exists in the pgo, we should merge it
      if(index[1] == TRUE){
        spobj@component[[index[2]]]@obj <- st_union(spobj@component[[index[2]]]@obj, c)
      } else {
        #otherwise, we simply append into the correct location
        spobj@component <- append(spobj@component, component, after=index[2]-1)
      }
      #in both cases we update its support
      spobj@supp <- st_union(spobj@supp, c)
    }
    
    if(type == "PLATEAUCOMPOSITION"){
      if(inherits(spobj, c("ppoint"))){
        pgo@ppoint <- spobj
      } else if(inherits(spobj, c("pline"))){
        pgo@pline <- spobj
      } else{
        pgo@pregion <- spobj
      }
      pgo@supp <- st_union(pgo@supp, c)
    } else{
      pgo <- spobj
    }
  }
  pgo
}

#' @title Capturing the membership degree of a point
#'
#' @description This function evaluates the membership degree of a given point in a spatial plateau object of any type.
#' It returns a value in \[0, 1\] that indicates to which extent the point belongs to the `pgeometry` object.
#'
#' @usage
#'
#' spa_eval(pgo, point)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param point An `sfg` object of the type `POINT`.
#'
#' @details
#'
#' The goal of this function is to return the membership degree of a simple point object (i.e., `sfg` object) in a given spatial plateau object (i.e., `pgeometry` object).
#' This evaluation depends on the following basic cases:
#' 
#' - if the simple point object belongs to the interior or boundary of _one_ component of the spatial plateau object, it returns the membership degree of that component.
#' - if the simple point object intersects more components (e.g., boundaries of region components, or different line components), it returns the maximum membership degree of all intersected components.
#' - if the simple point object is disjoint to the support of the spatial plateau object, it returns 0.
#' 
#' @return
#'
#' A numeric value between 0 and 1 that indicates the membership degree of a point (i.e., `sfg` object) in a spatial plateau object (i.e., `pgeometry` object).
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' library(tibble)
#' library(sf)
#' library(FuzzyR)
#' 
#' # some basic examples 
#' 
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pts3 <- rbind(c(2, 2), c(3, 3))
#' 
#' cp1 <- component_from_sfg(st_multipoint(pts1), 0.3)
#' cp2 <- component_from_sfg(st_multipoint(pts2), 0.6)
#' cp3 <- component_from_sfg(st_multipoint(pts3), 1.0)
#' 
#' pp <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' 
#' spa_eval(pp, st_point(c(1, 2)))
#' spa_eval(pp, st_point(c(1, 3)))
#' 
#' # other examples with plateau regions
#' 
#' set.seed(345)
#' 
#' # some random points to create plateau region objects by using the function spa_creator
#' tbl = tibble(x = runif(10, min= 0, max = 20), 
#'              y = runif(10, min = 0, max = 30), 
#'              z = runif(10, min = 0, max = 100))
#' 
#' #getting the convex hull on the points to clipping the construction of plateau region objects
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' pregions <- spa_creator(tbl, fuzz_policy = "fcp", k = 2, base_poly = ch)
#' 
#' # capturing the membership degree of a specific point in each object
#' spa_eval(pregions$pgeometry[[1]], st_point(c(5, 15)))
#' spa_eval(pregions$pgeometry[[2]], st_point(c(5, 15)))
#'
#' @import  sf
#' @export
spa_eval <- function(pgo, point){
  
  if(any(is.na(point))){
    stop("The parameter 'point' is NA.", call. = FALSE)
  }
  
  if(!(st_is(point, "POINT") && inherits(point, "sfg"))){
    stop("The parameter 'point' is not a simple point object of class sfg.", call. = FALSE)
  }
  
  ret <- 0

  if(st_intersects(point, pgo@supp, sparse = FALSE)[1]){
    
    type = spa_get_type(pgo)
    
    if(type == "PLATEAUCOMPOSITION"){
      
      ppoint_ret <- spa_eval(pgo@ppoint, point)
      pline_ret <- spa_eval(pgo@pline, point)
      pregion_ret <- spa_eval(pgo@pregion, point)
      
      ret <- max(c(ppoint_ret, pline_ret, pregion_ret))
      
    } else if(type == "PLATEAUCOLLECTION"){
      
      ret <- max(sapply(pgo@pgos, spa_eval, point)) 

    } else{
      
      # check in the boundary
      md_comps <- c()
      for(component in pgo@component){
        if(st_intersects(point, st_boundary(component@obj), sparse = FALSE)[1]){
          md_comps <- append(md_comps, component@md)
        } #  check in its interior...
        else if(st_intersects(point, component@obj, sparse = FALSE)[1]){
          if(type %in% c("PLATEAUPOINT", "PLATEAUREGION")){
            return(component@md)
          } else{
            md_comps <- append(md_comps, component@md)
          }
        }
      }
      
      ret <- max(md_comps)
      
    }
  }
  ret
}

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
#' - `spa_area` computes the area of a plateau region, plateau composition, or plateau collection. Thus, its parameter has to be a `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION` object.
#' - `spa_perimeter` computes the perimeter of a plateau region, plateau composition, or plateau collection. Thus, its parameter has to be a `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION` object.
#' - `spa_length` computes the length of a plateau line, plateau composition, or plateau collection. Thus, its parameter has to be a `PLATEAULINE`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION` object.
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
  type <- spa_get_type(pgo)
  if(!fsr_is_empty(pgo)){
    if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")){
      mean(sapply(pgo@component, attr, "md"))
    }else if(type == "PLATEAUCOMPOSITION"){
      mean(sapply(c(pgo@ppoint@component, pgo@pline@component, pgo@pregion@component), attr, "md"))
    }else if(type == "PLATEAUCOLLECTION"){
      mean(sapply(pgo@pgos, spa_avg_degree))
    }
  }else{
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
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")){
    length(pgo@component)
  }else if(type == "PLATEAUCOMPOSITION"){
    spa_ncomp(pgo@ppoint) + spa_ncomp(pgo@pline) + spa_ncomp(pgo@pregion)
  }else if(type == "PLATEAUCOLLECTION"){
    if(!fsr_is_empty(pgo)){
      sum(sapply(pgo@pgos, spa_ncomp))
    }else{
      0
    }
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_area(pgo) 
#' 
#' @param pgo A `pgeometry` object of the type `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION`. It throws a warning if a different type is given.
#'  
#' @import sf
#' @export
spa_area <- function(pgo){
  
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE")){
    warning(paste("A", type, "object does not have an area."), call. = FALSE)
    0
  }else if(!fsr_is_empty(pgo) && type %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")){
    
    if(type == "PLATEAUREGION"){
    
      area_comp <- function(comp){
        md_comp <- comp@md
        area_obj <- st_area(comp@obj)
        area_obj * md_comp
      }
      comps_areas <- sapply(pgo@component, area_comp)
      sum(comps_areas)
    }else if(type == "PLATEAUCOMPOSITION"){
      spa_area(pgo@pregion)
    }else if(type == "PLATEAUCOLLECTION"){
      pgos <- get_pgos(pgo)
      pgos <- pgos[sapply(pgos, spa_get_type) %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")]
      sum(sapply(pgos, spa_area))
    }
  }else{
    0
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_perimeter(pgo) 
#' 
#' @param pgo A `pgeometry` object of the type `PLATEAUREGION`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION`. It throws a warning if a different type is given.
#'  
#' @import sf lwgeom
#' @export
spa_perimeter <- function(pgo){
  
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE")){
    warning(paste("A", type, "object does not have a perimeter."), call. = FALSE)
    0
  }else if(!fsr_is_empty(pgo) && type %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")){
    
    if(type == "PLATEAUREGION"){
      perimeter_comp <- function(comp){
        md_comp <- comp@md
        temp <- st_sfc(comp@obj)
        st_set_crs(temp, 4326)
        perimeter_obj <- st_perimeter(temp)
        perimeter_obj * md_comp
      }
      comps_perimeter <- sapply(pgo@component, perimeter_comp)
      sum(comps_perimeter)
    }else if(type == "PLATEAUCOMPOSITION"){
      spa_perimeter(pgo@pregion)
    }else if(type == "PLATEAUCOLLECTION"){
      pgos <- get_pgos(pgo)
      pgos <- pgos[sapply(pgos, spa_get_type) %in% c("PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")]
      sum(sapply(pgos, spa_perimeter))
    }
  }else{
    0
  }
}

#' @name fsr_numerical_operations
#' 
#' @usage
#' 
#' spa_length(pgo) 
#' 
#' @param pgo A `pgeometry` object of the type `PLATEAULINE`, `PLATEAUCOMPOSITION`, or `PLATEAUCOLLECTION`. It throws a warning if a different type is given.
#'  
#' @import sf
#' @export
spa_length <- function(pgo){
  
  type <- spa_get_type(pgo)
  if(type %in% c("PLATEAUPOINT", "PLATEAUREGION")){
    warning(paste("A", type, "object does not have a length."), call. = FALSE)
    0
  }else if(!fsr_is_empty(pgo) && type %in% c("PLATEAULINE", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")){
    
    if(type == "PLATEAULINE"){
      length_comp <- function(comp){
        md_comp = comp@md
        length_obj = st_length(comp@obj)
        length_obj * md_comp
      }
      components_lenghts <- sapply(pgo@component, length_comp)
      sum(components_lenghts)
    }else if(type == "PLATEAUCOMPOSITION"){
      spa_length(pgo@pline)
    }else if(type == "PLATEAUCOLLECTION"){
      pgos <- get_pgos(pgo)
      pgos <- pgos[sapply(pgos, spa_get_type) %in% c("PLATEAULINE", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")]
      sum(sapply(pgos, spa_length))
    }
  }else{
    0
  }
}

#' @import sf
#' @noRd
obj_union <- function(obj, geom_type){
  objs <- suppressWarnings(st_collection_extract(obj, geom_type))
  if(inherits(objs, "sfg")){
    objs
  }else if(inherits(objs, "sfc")){
    st_union(objs)[[1]]
  }
}

#' @noRd
append_comps <- function(obj, md, comps){
  comp <- new("component", obj = obj, md = md)
  append(comps, comp)
}

#' @noRd
simplify <- function(pgo){
  
  if(fsr_is_empty(pgo)){
    return(pgo)
  }
  
  type <- spa_get_type(pgo)
  
  if(type == "PLATEAUCOMPOSITION"){
    
    triple <- c(pgo@ppoint, pgo@pline, pgo@pregion)
    mask_empty_objs <- sapply(triple, fsr_is_empty)
    n_empty_objs <- sum(mask_empty_objs)
    if(n_empty_objs == 2){
      if(!mask_empty_objs[1]){
        pgo@ppoint
      }else if(!mask_empty_objs[2]){
        pgo@pline
      }else if(!mask_empty_objs[3]){
        pgo@pregion
      }
    }else{
      pgo
    }
    
  }else if(type == "PLATEAUCOLLECTION"){
    
    mask_empty_objs <- sapply(pgo@pgos, fsr_is_empty)
    n_non_empty_objs <- length(pgo@pgos) - sum(mask_empty_objs)
    
    if(n_non_empty_objs == 1){
      pgo@pgos[!mask_empty_objs][[1]]
    }else{
      pgo
    }
  
  # Spatial plateau point, spatial plateau line, or spatial plateau region   
  }else{
    pgo
  }
  
}

#' @noRd
heterogeneous_geom_comp <- function(pgo1, pgo2, sigma, beta, as_pcomposition){
  
  if(inherits(pgo1, "pcomposition") && inherits(pgo2, "pcomposition")){
    spgo1 <- simplify(pgo1)
    spgo2 <- simplify(pgo2)
    
    if(inherits(spgo1, c("ppoint", "pline", "pregion")) && inherits(spgo2, c("ppoint", "pline", "pregion"))){
      return(sigma(spgo1, spgo2, beta, as_pcomposition = as_pcomposition))
    }
    cm <- combination_matrix(pgo1, pgo2, sigma, beta, as_pcomposition = TRUE)
    agg_combination_matrix(cm, as_pcomposition)
    
  }else if(inherits(pgo1, c("ppoint", "pline", "pregion")) && inherits(pgo2, "pcomposition")){
    # Due to the recursive nature of this function, we cannot simplify one pgeometry object ONLY. 
    # Otherwise, we would simplify this case that we explicitly want to use a pcomposition with only one non-empty component.
    pgo1 <- create_pgeometry(list(pgo1), "PLATEAUCOMPOSITION", is_valid = FALSE)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
    
  }else if(inherits(pgo1, "pcomposition") && inherits(pgo2, c("ppoint", "pline", "pregion"))){
    pgo2 <- create_pgeometry(list(pgo2), "PLATEAUCOMPOSITION", is_valid = FALSE)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
    
  }else if(inherits(pgo1, "pcollection") && inherits(pgo2, "pcollection")){
    spgo1 <- simplify(pgo1)
    spgo2 <- simplify(pgo2)
    
    if(inherits(spgo1, c("ppoint", "pline", "pregion")) && inherits(spgo2, c("ppoint", "pline", "pregion"))){
      return(sigma(spgo1, spgo2, beta, as_pcomposition = as_pcomposition))
    }
    pgo1 <- pcollection_to_pcomposition(pgo1)
    pgo2 <- pcollection_to_pcomposition(pgo2)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
    
  }else if(inherits(pgo1, "pcollection") && inherits(pgo2, c("ppoint", "pline", "pregion", "pcomposition"))){
    pgo1 <- pcollection_to_pcomposition(pgo1)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
    
  }else if(inherits(pgo1, c("ppoint", "pline", "pregion", "pcomposition")) && inherits(pgo2, "pcollection")){
    pgo2 <- pcollection_to_pcomposition(pgo2)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
  }
  
}

#' @noRd
spa_union_list <- function(pgos, type){
  
  if(length(pgos) == 1){
    pgos[[1]]
  }else if(length(pgos) == 0){
    create_empty_pgeometry(type)
  }else{
    result <- pgos[[1]]
    for(pgo in pgos[2:length(pgos)]){
      result <- spa_union(result, pgo, utype = "max", as_pcomposition = FALSE)
      if(fsr_is_empty(result)){
        result <- create_empty_pgeometry(type)
      }
    }
    result
  }
}

#' @noRd
get_pgos <- function(pcollection){
  
  pgos <- list()
  for(pgo in pcollection@pgos){
    
    type <- spa_get_type(pgo)
    if(type == "PLATEAUCOLLECTION"){
      pgos <- append(pgos, get_pgos(pgo))
    } else{
      pgos <- append(pgos, pgo)
    }
  }
  pgos
}

#' @title Converting a spatial plateau collection to a spatial plateau composition
#'
#' @description This function gathers all the fuzzy spatial objects of a `PLATEAUCOLLECTION` and 
#' reorganizes them into an equivalent fuzzy spatial composition object.
#'
#' @usage
#'
#' pcollection_to_pcomposition(pcollection)
#'
#' @param pcollection A `pcollection` object.
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
#'
#' library(sf)
#' 
#' pts <- rbind(c(1, 2), c(3, 2))
#' pcp <- component_from_sfg(st_multipoint(pts), 0.3)
#' 
#' lpts <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lcp <- component_from_sfg(st_linestring(lpts), 1)
#' pline <- create_pgeometry(list(lcp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' rpts2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' rcp <- component_from_sfg(st_polygon(list(rpts1, rpts2)), 0.7)
#' pregion <- create_pgeometry(list(rcp), "PLATEAUREGION")
#' 
#' pcomposition <- create_pgeometry(list(pcp, rcp), "PLATEAUCOMPOSITION")
#' pcollection <- create_pgeometry(list(pcomposition), "PLATEAUCOLLECTION")
#' 
#' pcollection_to_pcomposition(pcollection)
#' 
#' @import sf
#' @export
pcollection_to_pcomposition <- function(pcollection){
  
  project <- function(pcollection){
    types <- sapply(pcollection@pgos, spa_get_type)
    ppoint <- pcollection@pgos[types == "PLATEAUPOINT"]
    if(!length(ppoint)){
      ppoint <- create_empty_pgeometry("PLATEAUPOINT")
    }else{
      ppoint <- ppoint[[1]]
    }
    pline <- pcollection@pgos[types == "PLATEAULINE"]
    if(!length(pline)){
      pline <- create_empty_pgeometry("PLATEAULINE")
    }else{
      pline <- pline[[1]]
    }
    pregion <- pcollection@pgos[types == "PLATEAUREGION"]
    if(!length(pregion)){
      pregion <- create_empty_pgeometry("PLATEAUREGION")
    }else{
      pregion <- pregion[[1]]
    }
    pcomposition <- pcollection@pgos[types == "PLATEAUCOMPOSITION"]
    if(!length(pcomposition)){
      pcomposition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    }else{
      pcomposition <- pcomposition[[1]]
    }
    list(ppoint = ppoint, pline = pline, pregion = pregion, pcomposition = pcomposition)
  }
  
  # Step (i): removes the hierarchy of the operand object and flattens it.
  pcollection <- spa_flatten(pcollection)
  
  # Step (ii): we form the fuzzy geometric union of all fuzzy point objects, 
  # fuzzy line objects, and fuzzy region objects respectively that can be found 
  # in the flattened fuzzy spatial collection object.
  quad <- project(pcollection)
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
  
  supp <- st_union(st_sfc(ppoint@supp, pline@supp, pregion@supp))

  result <- create_empty_pgeometry("PLATEAUCOMPOSITION")
  result@supp <- supp[[1]]
  result@ppoint <- ppoint
  result@pline <- pline
  result@pregion <- pregion
  result
  
}

#' @noRd
combination_matrix <- function(pgo1, pgo2, sigma, beta, as_pcomposition = TRUE){
  
  triple1 <- c(pgo1@ppoint, pgo1@pline, pgo1@pregion)
  triple2 <- c(pgo2@ppoint, pgo2@pline, pgo2@pregion)
  rep1 <- rep(triple1, each = 3)
  mapply(sigma, rep1, triple2, beta, as_pcomposition)
}

#' @noRd
agg_combination_matrix <- function(cm, as_pcomposition = FALSE){
  
  ppoints <- lapply(cm, attr, "ppoint")
  presult <- spa_union_list(ppoints[!sapply(ppoints, fsr_is_empty)], "PLATEAUPOINT")
  
  plines <- lapply(cm, attr, "pline")
  lresult <- spa_union_list(plines[!sapply(plines, fsr_is_empty)], "PLATEAULINE")
  
  pregions <- lapply(cm, attr, "pregion")
  rresult <- spa_union_list(pregions[!sapply(pregions, fsr_is_empty)], "PLATEAUREGION")
  
  mask_empty_objs <- sapply(c(presult, lresult, rresult), fsr_is_empty)
  n_empty_objs <- sum(mask_empty_objs)
  if(n_empty_objs == 2 && !as_pcomposition){
    non_empty_objs <- c(presult, lresult, rresult)[!mask_empty_objs][[1]]
    non_empty_objs
  }else{
    supp <- st_union(st_sfc(presult@supp, lresult@supp, rresult@supp))[[1]]
    agg_cm <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    agg_cm@supp <- supp
    agg_cm@ppoint <- presult
    agg_cm@pline <- lresult
    agg_cm@pregion <- rresult
    agg_cm
  }
  
}

#' @noRd
final_data_type <- function(type1, type2){

  if(type1 == type2){
    type1
  }else{
    if(any(c(type1, type2) == "PLATEAUPOINT")){
      "PLATEAUPOINT"
    }else{
      # plateau line and plateau region
      "PLATEAULINE" 
    }
  }
}

#' @noRd
pcomposition_builder <- function(comps, as_pcomposition, type){
  # Spatial plateau composition (two or more non-empty lists)
  non_empty <- sum(sapply(list(comps$point, comps$line, comps$region), length) > 0)
  if(non_empty >= 2){
    result <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    spa_add_internal(result, c(comps$point, comps$line, comps$region))
  }else if(non_empty == 1 && !as_pcomposition){
    if(length(comps$point)){
      result <- create_empty_pgeometry("PLATEAUPOINT")
      spa_add_internal(result, comps$point)
    }else if(length(comps$line)){
      result <- create_empty_pgeometry("PLATEAULINE")
      spa_add_internal(result, comps$line)
    }else if(length(comps$region)){
      result <- create_empty_pgeometry("PLATEAUREGION")
      spa_add_internal(result, comps$region)
    }
  }else if(non_empty == 1 && as_pcomposition){
    result <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    if(length(comps$point)){
      spa_add_internal(result, comps$point)
    }else if(length(comps$line)){
      spa_add_internal(result, comps$line)
    }else if(length(comps$region)){
      spa_add_internal(result, comps$region)
    }
  }else if(non_empty == 0 && as_pcomposition){
    create_empty_pgeometry("PLATEAUCOMPOSITION")
  }else if(non_empty == 0 && !as_pcomposition){
    create_empty_pgeometry(type)
  }
}

#' @noRd
result_handler <- function(type1, type2, obj, md, comps, check_compatibility = FALSE){
  
  #TODO Explain how the compatibility works
  if(!st_is_empty(obj) && md > 0 && md <= 1){
    
    geom_type = st_geometry_type(obj)
    
    if(check_compatibility){
      if(type1 == type2 && type1 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION") && is_compatible(obj, type1)){
        if(type1 == "PLATEAUPOINT"){
          comps$point <- append_comps(obj, md, comps$point)
        }else if(type1 == "PLATEAULINE"){
          comps$line <- append_comps(obj, md, comps$line)
        }else if(type1 == "PLATEAUREGION"){
          comps$region <- append_comps(obj, md, comps$region)
        }
      }
    }else{
      if(geom_type == "GEOMETRYCOLLECTION"){
        comps$point <- append_comps(obj_union(obj, "POINT"), md, comps$point)
        comps$line <- append_comps(obj_union(obj, "LINESTRING"), md, comps$line)
        comps$region <- append_comps(obj_union(obj, "POLYGON"), md, comps$region)
      }else if(geom_type %in% c("POINT", "MULTIPOINT")){
        comps$point <- append_comps(obj, md, comps$point)
      }else if(geom_type %in% c("LINESTRING", "MULTILINESTRING")){
        comps$line <- append_comps(obj, md, comps$line)
      }else if(geom_type %in% c("POLYGON", "MULTIPOLYGON")){
        comps$region <- append_comps(obj, md, comps$region)
      }
    }
  }
  comps
}

#' @title The operation flatten
#'
#' @description This function gathers all the fuzzy spatial objects of a `PLATEAUCOLLECTION` and 
#' reorganizes them into a single flattened fuzzy spatial object containing a quadruple
#' (`PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`, `PLATEAUCOMPOSITION`) that preserves the identity of sub-objects.
#'
#' @usage
#'
#' spa_flatten(pcollection)
#'
#' @param pcollection A `pcollection` object.
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
#'
#' library(sf)
#' 
#' pts <- rbind(c(1, 2), c(3, 2))
#' pcp <- component_from_sfg(st_multipoint(pts), 0.3)
#' 
#' lpts <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lcp <- component_from_sfg(st_linestring(lpts), 1)
#' pline <- create_pgeometry(list(lcp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' rpts2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' rcp <- component_from_sfg(st_polygon(list(rpts1, rpts2)), 0.7)
#' pregion <- create_pgeometry(list(rcp), "PLATEAUREGION")
#' 
#' pcomposition <- create_pgeometry(list(pcp, rcp), "PLATEAUCOMPOSITION")
#' pcol1 <- create_pgeometry(list(pcomposition), "PLATEAUCOLLECTION")
#' pcol2 <- create_pgeometry(list(pline, pregion, pcol1), "PLATEAUCOLLECTION")
#' pcol2
#' 
#' spa_flatten(pcol2)
#' 
#' @import methods
#' @export
spa_flatten <- function(pcollection){
  
  if(fsr_is_empty(pcollection)){
    pcollection@pgos <- append(pcollection@pgos, create_empty_pgeometry("PLATEAUPOINT"))
    pcollection@pgos <- append(pcollection@pgos, create_empty_pgeometry("PLATEAULINE"))
    pcollection@pgos <- append(pcollection@pgos, create_empty_pgeometry("PLATEAUREGION"))
    pcollection@pgos <- append(pcollection@pgos, create_empty_pgeometry("PLATEAUCOMPOSITION"))
    return(pcollection)
  }
  
  pgo_list <- get_pgos(pcollection)
  types <- lapply(pgo_list, spa_get_type)
  
  ppoints <- pgo_list[types == "PLATEAUPOINT"]
  plines <- pgo_list[types == "PLATEAULINE"]
  pregions <- pgo_list[types == "PLATEAUREGION"]
  pcompositions <- pgo_list[types == "PLATEAUCOMPOSITION"]
  
  ppoint <- create_empty_pgeometry("PLATEAUPOINT")
  if(length(ppoints)){
    ppoint <- spa_union_list(ppoints[!sapply(ppoints, fsr_is_empty)], "PLATEAUPOINT")
  }
  pline <- create_empty_pgeometry("PLATEAULINE")
  if(length(plines)){
    pline <- spa_union_list(plines[!sapply(plines, fsr_is_empty)], "PLATEAULINE")
  }
  pregion <- create_empty_pgeometry("PLATEAUREGION")
  if(length(pregions)){
    pregion <- spa_union_list(pregions[!sapply(pregions, fsr_is_empty)], "PLATEAUREGION")
  }
  pcomposition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
  if(length(pcompositions)){
    pcomposition <- spa_union_list(pcompositions[!sapply(pcompositions, fsr_is_empty)], "PLATEAUCOMPOSITION")
  }
  
  pgos <- c(ppoint, pline, pregion, pcomposition)
  new("pcollection", supp = pcollection@supp, pgos = pgos)

}

#' @title Fuzzy geometric set operations
#'
#' @description Fuzzy geometric set operations are given as a family of functions that implements spatial plateau set operations.
#' These functions yield a spatial plateau object from a specific combination of other two spatial plateau objects, 
#' such as the intersection of two plateau region objects.
#'
#' @usage
#'
#' spa_intersection(pgo1, pgo2, itype = "min", as_pcomposition = FALSE)
#'
#' @param pgo1 A `pgeometry` object of any type.
#' @param pgo2 A `pgeometry` object of any type.
#' @param itype A character value that indicates the name of a function implementing a t-norm. The default value is `"min"`, which is the standard operator of the intersection.
#' @param as_pcomposition A logical value; if `TRUE`, it returns a spatial plateau composition object.
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
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
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
spa_intersection <- function(pgo1, pgo2, itype = "min", as_pcomposition = FALSE){

  ##TODO should we add a short-circuit?

  beta <- match.fun(itype)
  comps <- list(point = list(), line = list(), region = list())
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)

  if(type1 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION") && type2 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")){

    for(comp1 in pgo1@component){
      for(comp2 in pgo2@component){

        result_md <- beta(comp1@md, comp2@md)
        result_obj <- st_intersection(comp1@obj, comp2@obj)
        comps <- result_handler(type1, type2, result_obj, result_md, comps, FALSE)

      }
    }
    ftype <- final_data_type(type1, type2)
    pcomposition_builder(comps, as_pcomposition, ftype)
    
  }else{
    sigma <- match.fun("spa_intersection")
    heterogeneous_geom_comp(pgo1, pgo2, sigma, beta = itype, as_pcomposition = as_pcomposition)
    
  }
}

#' @noRd
is_special_case <- function(type1, type2){
  
  if(type1 == "PLATEAULINE" && type2 == "PLATEAUREGION"){
    TRUE
  }else if(type1 == "PLATEAUREGION" && type2 == "PLATEAULINE"){
    TRUE
  }else{
    FALSE
  }
    
}

#' @name fsr_geometric_operations
#' 
#' @usage
#' 
#' spa_union(pgo1, pgo2, utype = "max", as_pcomposition = FALSE)
#' 
#' @param utype A character value that refers to a t-conorm. The default value is `"max"`, which is the standard operator of the union.
#' 
#' @import sf
#' @export
spa_union <- function(pgo1, pgo2, utype = "max", as_pcomposition = FALSE){
  
  tau <- match.fun(utype)
  comps <- list(point = list(), line = list(), region = list())
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION") && type2 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")){
    
    if(!is_special_case(type1, type2)){
      supp_intersected <- st_intersection(pgo1@supp, pgo2@supp)
    }
    
    for(comp1 in pgo1@component){
      obj_comp_p1 <- comp1@obj
      md_comp_p1 <- comp1@md
      
      for(comp2 in pgo2@component){
        obj_comp_p2 <- comp2@obj
        md_comp_p2 <- comp2@md
        
        result_md <- tau(md_comp_p1, md_comp_p2)
        result_obj <- st_intersection(obj_comp_p1, obj_comp_p2)
        comps <- result_handler(type1, type2, result_obj, result_md, comps, TRUE)
      }
      
      # Special case between a spatial plateau region and a spatial plateau line
      # TODO Conduct a performance evaluation to compare both strategies (probabily the first case is faster)
      if(is_special_case(type1, type2)){
        sf_diff_1 <- st_difference(obj_comp_p1, pgo2@supp)
      }else{
        sf_diff_1 <- st_difference(obj_comp_p1, supp_intersected)
      }
      comps <- result_handler(type1, type2, sf_diff_1, md_comp_p1, comps, FALSE)
    }
    
    for(comp2 in pgo2@component){
      obj_comp_p2 <- comp2@obj
      md_comp_p2 <- comp2@md
      
      # Special case between a spatial plateau region and a spatial plateau line
      if(is_special_case(type1, type2)){
        sf_diff_2 <- st_difference(obj_comp_p2, pgo1@supp)
      }else{
        sf_diff_2 <- st_difference(obj_comp_p2, supp_intersected)
      }
      comps <- result_handler(type1, type2, sf_diff_2, md_comp_p2, comps, FALSE)
    }
    ftype <- final_data_type(type1, type2)
    pcomposition_builder(comps, as_pcomposition, ftype)
  }else{
    sigma <- match.fun("spa_union")
    heterogeneous_geom_comp(pgo1, pgo2, sigma, beta = utype, as_pcomposition = as_pcomposition)
    
  }
  
}

#' @name fsr_geometric_operations
#' 
#' @usage
#' 
#' spa_difference(pgo1, pgo2, dtype = "f_diff", as_pcomposition = FALSE)
#' 
#' @param dtype A character value that indicates the name of a difference operator. The default value is `"f_diff"`, which implements the standard fuzzy difference.
#' 
#' @import sf
#' @export
spa_difference <- function(pgo1, pgo2, dtype = "f_diff", as_pcomposition = FALSE){

  nu <- match.fun(dtype)
  comps <- list(point = list(), line = list(), region = list())
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION") && type2 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")){
    
    if(!is_special_case(type1, type2)){
      supp_intersected <- st_intersection(pgo1@supp, pgo2@supp)
    }
    
    for(comp1 in pgo1@component){
      obj_comp_p1 <- comp1@obj
      md_comp_p1 <- comp1@md

      for(comp2 in pgo2@component){
        obj_comp_p2 <- comp2@obj
        md_comp_p2 <- comp2@md
        
        result_md = nu(md_comp_p1, md_comp_p2)
        result_obj = st_intersection(obj_comp_p1, obj_comp_p2)
        comps <- result_handler(type1, type2, result_obj, result_md, comps, TRUE)
        
      }
      # Special case between a spatial plateau region and a spatial plateau line
      if(is_special_case(type1, type2)){
        sf_diff_1 <- st_difference(obj_comp_p1, pgo2@supp)
      }else{
        sf_diff_1 <- st_difference(obj_comp_p1, supp_intersected)
      }
      comps <- result_handler(type1, type2, sf_diff_1, md_comp_p1, comps, FALSE)
    }
    ftype <- final_data_type(type1, type2)
    pcomposition_builder(comps, as_pcomposition, ftype)
     
  }else{
    sigma <- match.fun("spa_difference")
    heterogeneous_geom_comp(pgo1, pgo2, sigma, beta = dtype, as_pcomposition = as_pcomposition)
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
  
  .Deprecated("spa_intersection")
  
  type1 <- spa_get_type(pline1)
  type2 <- spa_get_type(pline2)
  
  if(type1 != type2){
    stop("Different Spatial Plateau Types.", call. = FALSE)
  }
  
  result <- spa_intersection(pline1, pline2, itype = itype, as_pcomposition = TRUE)
  
  result@ppoint
  
}

#' @title Capturing the support of a `pgeometry` object
#'
#' @description This function yields a crisp spatial object (as an `sfg` object) that corresponds to the support of a `pgeometry` object given as input.
#'
#' @usage
#'
#' spa_support(pgo)
#'
#' @param pgo A `pgeometry` object of any type.
#'
#' @details
#'
#' It employs the classical definition of _support_ from the fuzzy set theory in the context of spatial plateau algebra. 
#' The _support_ only comprises the points with membership degree greater than or equal to 1.
#' Hence, this operation returns the `sfg` object that represents the total extent of the `pgeometry` given as input. 
#' If the `pgeometry` object has no components, then an empty `sfg` object is returned (i.e., a crisp spatial object without points).
#'
#' @return
#'
#' An `sfg` object that represents the support of `pgeometry`. It can be an empty object if `pgeometry` is empty.
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
#' pp <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' pp
#' 
#' pp_supp <- spa_support(pp)
#' pp_supp
#' 
#' pp_empty <- create_empty_pgeometry("PLATEAUPOINT")
#' pp_empty_supp <- spa_support(pp_empty)
#' pp_empty_supp
#'
#' @export
spa_support <- function(pgo){
  pgo@supp
}

#' @title Capturing the core of a `pgeometry` object
#'
#' @description This function yields a crisp spatial object (as an `sfg` object) that corresponds to the core of a `pgeometry` object given as input.
#'
#' @usage
#'
#' spa_core(pgo)
#'
#' @param pgo A `pgeometry` object of any type.
#'
#' @details
#'
#' It employs the classical definition of _core_ from the fuzzy set theory in the context of spatial plateau algebra. 
#' The _core_ only comprises the points with membership degree equal to 1.
#' Hence, this operation returns the `sfg` object that represents the component labeled with 
#' membership degree equal to 1 of the `pgeometry` object given as input. If the `pgeometry` object has no core, then an empty `sfg` object is returned (i.e., a crisp spatial object without points).
#'
#' @return
#'
#' An `sfg` object that represents the core of `pgo`. It can be an empty object if `pgo` does not have a component with membership degree 1.
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
#' pp <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' pp
#' 
#' pp_core <- spa_core(pp)
#' pp_core
#'
#' #Creating a pgeometry object without core
#' pp2 <- create_pgeometry(list(cp1, cp2), "PLATEAUPOINT")
#' pp2
#' 
#' spa_core(pp2)
#'
#' @import sf utils
#' @export
spa_core <- function(pgo){
  
  type <- spa_get_type(pgo)
  
  if(fsr_is_empty(pgo)){
    sf_type <- get_counter_ctype(pgo)
    
    sfg_obj <- switch(sf_type,
                      POINT = st_point(),
                      LINESTRING = st_linestring(),
                      POLYGON = st_polygon(),
                      GEOMETRYCOLLECTION = st_geometrycollection())
    return(sfg_obj)
  }else{
   
    if(type == "PLATEAUCOMPOSITION"){
      triple_core <- st_sfc(spa_core(pgo@ppoint), spa_core(pgo@pline), spa_core(pgo@pregion))
      return(st_union(triple_core)[[1]])
    }else if(type == "PLATEAUCOLLECTION"){
      core_list <- lapply(pgo@pgos, spa_core)
      return(st_union(st_sfc(core_list))[[1]])
    }else{
      last_comp <- tail(pgo@component, 1)
      if(last_comp[[1]]@md == 1){
        return(last_comp[[1]]@obj)
      }else{
        spa_core(create_empty_pgeometry(type))
      }
    } 
  }
}

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
#' pp1 <- create_pgeometry(list(cp1, cp2, cp3), "PLATEAUPOINT")
#' pp2 <- create_pgeometry(list(cp2, cp1), "PLATEAUPOINT")
#' 
#' spa_exact_equal(pp1, pp2)
#' 
#' spa_exact_equal(pp1, pp1)
#'
#' @import sf
#' @export
spa_exact_equal <- function(pgo1, pgo2){
  
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION") || type2 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")){
    stop("This function only deals with PLATEAUPOINT, PLATEAULINE, and PLATEAUREGION.")
  }
  
  # Same type and both empty
  if(type1 == type2 && all(sapply(c(pgo1, pgo2), fsr_is_empty))){
    return(TRUE)
  }

  comp_check <- function(comp1, comp2){
    if(st_equals(comp1@obj, comp2@obj, sparse=FALSE)[1] && (comp1@md == comp2@md)){
      return(TRUE)
    }
    return(FALSE)
  }

  if((type1 != type2) ||
     (spa_ncomp(pgo1) != spa_ncomp(pgo2)) ||
     (!(st_equals(pgo1@supp, pgo2@supp, sparse=FALSE)[1]))){
    return(FALSE)
  } else {
    for(i in 1:spa_ncomp(pgo1)){
      if(!(comp_check(pgo1@component[[i]], pgo2@component[[i]]))){
        return(FALSE)
      }
    }
  }
  return(TRUE)
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
#' cp1 <- component_from_sfg(st_multipoint(pts1), 0.3)
#' cp2 <- component_from_sfg(st_multipoint(pts2), 0.6)
#' cp3 <- component_from_sfg(st_multipoint(pts3), 1.0)
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
  
  if(type1 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION") || type2 %in% c("PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")){
    stop("This function only deals with PLATEAUPOINT, PLATEAULINE, and PLATEAUREGION.")
  }
  
  intersected <- spa_intersection(pgo1, pgo2, as_pcomposition = FALSE)
  if(spa_get_type(intersected) == 'PLATEAUCOMPOSITION') {
    FALSE
  } else {
    # TODO implement the spa_exact_equal for PLATEAUCOMPOSITION
    spa_exact_equal(intersected, pgo1)
  }
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
           if(!("eval_mode" %in% names(args) && "lval" %in% names(args))){
             stop("args not supplied. 'eval_mode' and 'lval' needed for bool result type", call. = FALSE)
           }
           e_mode <- match.fun(args$eval_mode)
           term <- args$lval
           return(e_mode(list_res[[term]]))
         },
         stop("Return type does not exist.", call. = FALSE))
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
#'
#' library(tibble)
#' library(sf)
#' library(FuzzyR)
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
spa_overlap <- function(pgo1, pgo2, itype = "min", ret = "degree", ...){

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

  # Common points
  countour_int <- spa_intersection(countour_pgo1, countour_pgo2, itype = itype, as_pcomposition = TRUE)
  p <- countour_int@ppoint
  c <- countour_int@pline

  p_ncomp <- spa_ncomp(p)
  c_ncomp <- spa_ncomp(c)
  p_core <- spa_core(p)
  c_core <- spa_core(c)
  
  result <- NULL

  if((p_ncomp == 1 && !st_is_empty(p_core)) ||
     (c_ncomp == 1 && !st_is_empty(c_core))){
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
       spa_exact_equal(pgo1, pgo2)){
      
      result <- 0
    } else if(st_relate(supp_1, supp_2, pattern = "F**0*****", sparse=FALSE)[1] ||
       st_relate(supp_1, supp_2, pattern = "F***0****", sparse=FALSE)[1] ||
       st_relate(supp_1, supp_2, pattern = "F0*******", sparse=FALSE)[1]){
      result <- spa_avg_degree(p)
    } else if (st_relate(supp_1, supp_2, pattern = "F**1*****", sparse=FALSE)[1] ||
               st_relate(supp_1, supp_2, pattern = "F***1****", sparse=FALSE)[1] ||
               st_relate(supp_1, supp_2, pattern = "F1*******", sparse=FALSE)[1]){
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
spa_disjoint <- function(pgo1, pgo2, itype="min", ret = "degree", ...){

  check_spa_topological_condition(pgo1, pgo2)

  supp_pgo1 <- pgo1@supp
  supp_pgo2 <- pgo2@supp
  result <- NULL

  if(st_disjoint(supp_pgo1, supp_pgo2, sparse=FALSE)[1]){
    result <- 1
  }else{
    
    r_overlap <- spa_overlap(pgo1, pgo2, itype = itype)
    r_meet <- spa_meet(pgo1, pgo2)
    
    if((r_overlap == 1) || (r_meet == 1) ||
       spa_exact_inside(pgo1, pgo2) ||
       spa_exact_inside(pgo2, pgo1) ||
       spa_exact_equal(pgo2, pgo1))  {
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
#' spa_equal(pgo1, pgo2, utype = "max", ret = 'degree', ...) 
#' 
#' @param utype A character value that indicates the name of a function implementing a t-conorm. The default value is `"max"`, which is the standard operator of the union.
#' 
#' @import sf
#' @export
spa_equal <- function(pgo1, pgo2, utype = "max", ret = 'degree', ...){

  check_spa_topological_condition(pgo1, pgo2)
  result <- NULL

  if(spa_exact_equal(pgo1, pgo2)){
    result <- 1
  } else {   
    supp_pgo1 <- pgo1@supp
    supp_pgo2 <- pgo2@supp
    
    if(st_disjoint(supp_pgo1, supp_pgo2, sparse=FALSE)[1] ||
       st_touches(supp_pgo1, supp_pgo2, sparse=FALSE)[1]){
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
#' spa_inside(pgo1, pgo2, utype = "max", ret = 'degree', ...) 
#' 
#' @import sf
#' @export
spa_inside <- function(pgo1, pgo2, utype = "max", ret = 'degree', ...){

  check_spa_topological_condition(pgo1, pgo2)
  result <- NULL

  if(spa_exact_inside(pgo1, pgo2)){
    result <- 1
  } else {    
    supp_pgo1 <- pgo1@supp
    supp_pgo2 <- pgo2@supp
    
    if(spa_equal(pgo1, pgo2, utype = utype) == 1 ||
       st_disjoint(supp_pgo1, supp_pgo2, sparse=FALSE)[1] ||
       st_touches(supp_pgo1, supp_pgo2, sparse=FALSE)[1]){
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
#' spa_contains(pgo1, pgo2, utype = "max", ret = 'degree', ...) 
#' 
#' @export
spa_contains <- function(pgo1, pgo2, utype = "max", ret = 'degree', ...){
  spa_inside(pgo2, pgo1, utype = utype, ret = ret, ...)
}

#' @title Capturing the frontier of a plateau region object
#'
#' @description This function extracts the frontier (i.e., linear boundary) of a plateau region object by maintaining its membership degrees.
#'
#' @usage
#'
#' spa_contour(pregion)
#'
#' @param pregion A `pgeometry` object of the type `PLATEAUREGION`. It throws an error if a different type is given.
#'
#' @details
#'
#' It employs the definition of _fuzzy frontier_ of a fuzzy region object in the context of spatial plateau algebra (as defined in the references). 
#' The _fuzzy frontier_ of a fuzzy region object `A` collects all single points of `A`, preserving its membership degrees, that are not in the interior of its support.
#' 
#' IMPORTANT NOTE: Fuzzy frontier is different from fuzzy boundary (see `spa_boundary_region`).
#' 
#' @return
#'
#' A `pgeometry` object of the type `PLATEAULINE` that represents the contour (i.e. frontier) of a plateau region object given as input.
#'
#' @references
#'
#' - [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' library(tibble)
#' library(sf)
#' library(FuzzyR)
#' 
#' set.seed(123)
#' 
#' # some random points to create pgeometry objects by using the function spa_creator
#' tbl = tibble(x = runif(10, min= 0, max = 30), 
#'              y = runif(10, min = 0, max = 50), 
#'              z = runif(10, min = 0, max = 100))
#'
#' classes <- c("category-1", "category-2")
#' mf1 <- genmf("trapmf", c(0, 5, 20, 35))
#' mf2 <- genmf("trimf", c(35, 80, 100))
#' 
#' #getting the convex hull on the points to clipping the construction of plateau region objects
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' pregions <- spa_creator(tbl, classes = classes, mfs = c(mf1, mf2), base_poly = ch)
#' 
#' # capturing and showing the frontier of each pgeometry object previously created
#' frontier_pregion1 <- spa_contour(pregions$pgeometry[[1]]) 
#' frontier_pregion2 <- spa_contour(pregions$pgeometry[[2]])
#' 
#' plot(pregions$pgeometry[[1]])
#' plot(frontier_pregion1)
#' 
#' plot(pregions$pgeometry[[2]])
#' plot(frontier_pregion2)
#'
#' @import sf
#' @export
spa_contour <- function(pregion){
  
  type <- spa_get_type(pregion)
  
  if(type != "PLATEAUREGION"){
    stop("pregion must be a PLATEAUREGION type.", call. = FALSE)
  }
  
  if(fsr_is_empty(pregion)){
    return(create_empty_pgeometry("PLATEAULINE"))
  }

  pregion_tibble <- as_tibble(pregion)
  pregion_tibble$boundary <- st_boundary(pregion_tibble$geometry)
  pregion_df <- as.data.frame(pregion_tibble)
  pline <- create_pgeometry(pregion_df[,c(3,2)], "PLATEAULINE", is_valid = FALSE)
  
  crisp_contour <- create_empty_pgeometry("PLATEAULINE")
  crisp_contour <- spa_add_component(crisp_contour, component_from_sfg(st_boundary(pregion@supp), 1))
  
  result <- spa_intersection(pline, crisp_contour, as_pcomposition = TRUE)
  result@pline
  
}

pkg_env <- new.env()
pkg_env$ftopological_classes <- c("a little bit", "somewhat", "slightly", "averagely", "mostly", "quite")

pkg_env$ftopological_mfs <- c(FuzzyR::genmf("trapmf", c(0, 0, 0.03, 0.08)),
                              FuzzyR::genmf("trapmf", c(0.03, 0.08, 0.2, 0.26)),
                              FuzzyR::genmf("trapmf", c(0.2, 0.26, 0.39, 0.45)),
                              FuzzyR::genmf("trapmf", c(0.39, 0.45, 0.62, 0.69)),
                              FuzzyR::genmf("trapmf", c(0.62, 0.69, 0.93, 0.95)),
                              FuzzyR::genmf("trapmf", c(0.93, 0.95, 1, 1)))

#' @title Setting a new classification for fuzzy topological relationships
#'
#' @description This functions configures a new set of linguistic values and their corresponding membership functions to be used by fuzzy topological relationships.
#'
#' @usage
#'
#' spa_set_classification(classes, mfs)
#'
#' @param classes A character vector containing linguistic values that characterizes different situations of fuzzy topological relationships.
#' @param mfs A vector containing membership functions generated by the function `genmf` of the FuzzyR package. Their domain have to be in \[0, 1\].
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
#' 
#' library(tibble)
#' library(sf)
#' library(FuzzyR)
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
#' classes <- c("small", "medium", "large")
#' small <- genmf("trapmf", c(0, 0.3, 0.4, 0.6))
#' medium <- genmf("trapmf", c(0.4, 0.6, 0.8, 1))
#' large <- genmf("trapmf", c(0.6, 0.8, 1, 1))
#' 
#' spa_set_classification(classes, c(small, medium, large))
#' 
#' spa_overlap(pregions$pgeometry[[1]], pregions$pgeometry[[2]], ret = "list")
#'
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

#' @title Capturing the fuzzy boundary of a plateau region object
#'
#' @description This function yields the fuzzy boundary of a plateau region object.
#'
#' @usage
#'
#' spa_boundary(pgo)
#'
#' @param pgo A `pgeometry` object of type `PLATEAULINE`, or `PLATEAUREGION`.
#'
#' @details
#'
#' It employs the definition of _fuzzy boundary_ of a fuzzy region object in the context of spatial plateau algebra (as defined in the references). 
#' The _fuzzy boundary_ of a fuzzy region object `A` has a heterogeneous nature since it consists of two parts:
#' - a fuzzy line object that corresponds to the boundary of the core of `A`.
#' - a fuzzy region object that comprises all points of `A` with a membership degree greater than 0 and less than 1.
#' 
#' This means that the function `spa_boundary` can yield these two parts of the fuzzy boundary of a plateau region object (the argument `pgeometry`):
#' the boundary plateau line and the boundary plateau region (i.e., returns a `pgeometry` object of the type `PLATEAUCOMPOSITION` with these two plateau objects).
#' 
#' @return
#'
#' A `pcomposition` object that represents a fuzzy boundary of `pgeometry` object given as input.
#'
#' @references
#'
#' - [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' library(tibble)
#' library(FuzzyR)
#' 
#' set.seed(123)
#' 
#' # some random points to create pgeometry objects by using the function spa_creator
#' tbl = tibble(x = runif(10, min= 0, max = 30), 
#'              y = runif(10, min = 0, max = 50), 
#'              z = runif(10, min = 0, max = 100))
#'
#' classes <- c("category-1", "category-2")
#' mf1 <- genmf("trapmf", c(0, 5, 20, 35))
#' mf2 <- genmf("trimf", c(20, 80, 100))
#' 
#' pregions <- spa_creator(tbl, classes = classes, mfs = c(mf1, mf2))
#' pregions$pgeometry[[1]]
#' pregions$pgeometry[[2]]
#' 
#' # capturing and showing the boundary of each pgeometry object previously created
#' (spa_boundary(pregions$pgeometry[[1]])) 
#' (spa_boundary(pregions$pgeometry[[2]]))
#'
#' @import methods utils sf
#' @export
spa_boundary <- function(pgo){
  
  if(fsr_is_empty(pgo)){
    return(create_empty_pgeometry("PLATEAUCOMPOSITION"))
  }
  
  type <- spa_get_type(pgo)
  if(!(type %in% c("PLATEAULINE", "PLATEAUREGION"))){
    stop("pgo is not a PLATEAULINE or PLATEAUREGION object.", call. = FALSE)
  }else{
    comps <- list()
    last_comp <- tail(pgo@component, 1)
    if(last_comp[[1]]@md == 1){
      # point or multipoint sf object (if pgo is a plateau line)
      # linestring or multilinestring sf object (if pgo is a plateau region)
      boundary_component <- st_boundary(last_comp[[1]]@obj)
      comp <- new("component", obj = boundary_component, md = 1)
      comps <- append(comps, comp)
      n_comps <- spa_ncomp(pgo)
      if(n_comps > 1){
        # linestring or multilinestring sf object (if pgo is a plateau line)
        # polygon or multipolygon sf object (if pgo is a plateau region)
        comp <- head(pgo@component, n = n_comps-1)
        comps <- append(comps, comp)
      }
    }else{
      comps <- append(comps, pgo@component)
    }
    create_pgeometry(comps, type = "PLATEAUCOMPOSITION", is_valid = FALSE)
  }
}

#' @title Capturing the fuzzy boundary of a plateau region object
#'
#' @description This function yields a specific part of the fuzzy boundary of a plateau region object.
#'
#' @usage
#'
#' spa_boundary_pregion(pregion, bound_part = "region")
#'
#' @param pregion A `pgeometry` object of the type `PLATEAUREGION`. It throws an error if a different type is given.
#' @param bound_part A character value that indicates the part of the fuzzy boundary to be returned. It can be `"region"` or `"line"`. See below for more details.
#'
#' @details
#'
#' It employs the definition of _fuzzy boundary_ of a fuzzy region object in the context of spatial plateau algebra (as defined in the references). 
#' The _fuzzy boundary_ of a fuzzy region object `A` has a heterogeneous nature since it consists of two parts:
#' - a fuzzy line object that corresponds to the boundary of the core of `A`.
#' - a fuzzy region object that comprises all points of `A` with a membership degree greater than 0 and less than 1.
#' 
#' This means that the function `spa_boundary_pregion` can yield one specific part of the fuzzy boundary of a plateau region object (the argument `pgeometry`).
#' If `boundary = "line"`, then the function returns the boundary plateau line of `pgeometry` (i.e., returns a `pgeometry` object of the type `PLATEAULINE`).
#' Else if `boundary = "region"` (the default value), then the function returns the boundary plateau region of `pgeometry` (i.e., returns a `pgeometry` object of the type `PLATEAUREGION`).
#' 
#' @return
#'
#' A `pgeometry` object that represents a specific part of the fuzzy boundary of `pgeometry` object given as input.
#'
#' @references
#'
#' - [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#'
#' \dontrun{
#' library(tibble)
#' library(FuzzyR)
#' 
#' set.seed(123)
#' 
#' # some random points to create pgeometry objects by using the function spa_creator
#' tbl = tibble(x = runif(10, min= 0, max = 30), 
#'              y = runif(10, min = 0, max = 50), 
#'              z = runif(10, min = 0, max = 100))
#'
#' classes <- c("category-1", "category-2")
#' mf1 <- genmf("trapmf", c(0, 5, 20, 35))
#' mf2 <- genmf("trimf", c(20, 80, 100))
#' 
#' pregions <- spa_creator(tbl, classes = classes, mfs = c(mf1, mf2))
#' pregions$pgeometry[[1]]
#' pregions$pgeometry[[2]]
#' 
#' # these functions are now deprecated, use `spa_boundary`
#' 
#' # capturing and showing the boundary plateau line of each pgeometry object previously created
#' (spa_boundary_pregion(pregions$pgeometry[[1]], bound_part = "line")) 
#' (spa_boundary_pregion(pregions$pgeometry[[2]], bound_part = "line"))
#' # the last boundary is empty because there is no core! 
#' 
#' # capturing and showing the boundary plateau region (this is the default behavior)
#' (spa_boundary_pregion(pregions$pgeometry[[1]]))
#' (spa_boundary_pregion(pregions$pgeometry[[2]]))
#' }
#'
#' @import methods utils sf
#' @export
spa_boundary_pregion <- function(pregion, bound_part = "region"){
  
  .Deprecated("spa_boundary")
  
  type <- spa_get_type(pregion)
  
  if(type != "PLATEAUREGION"){
    stop("pregion is not a PLATEAUREGION object.", call. = FALSE)
  }
  
  if(bound_part == "line"){
    bpl <- create_empty_pgeometry("PLATEAULINE")
    last_comp <- tail(pregion@component, 1)
    if(last_comp[[1]]@md == 1){
      boundary_component <- st_boundary(last_comp[[1]]@obj)
      comp_line <- new("component", obj = boundary_component, md=1)
      bpl <- spa_add_component(bpl, comp_line)
    }
    return(bpl)
  }
  else if(bound_part == "region"){
    last_comp <- tail(pregion@component, 1)
    n_comps <- spa_ncomp(pregion)
    if(last_comp[[1]]@md == 1 && n_comps > 1){
      bpr <- create_empty_pgeometry("PLATEAUREGION")
      bpr <- spa_add_component(bpr, head(pregion@component, n=n_comps-1))
    } else {
      ret <- new("pregion", component = pregion@component, supp = pregion@supp)
      return(ret)
    }
  }
  else{
    stop("Invalid value for the parameter 'bound_part'.", call. = FALSE)
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

#' @noRd
check_spa_topological_condition <- function(pgo1, pgo2){
  
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 != type2){
    stop("The spatial plateau objects have different types.", call. = FALSE)
  } else if(type1 != "PLATEAUREGION" || type2 != "PLATEAUREGION") {
    stop(paste0("This operator is not implemented to (", type1, " x ", type2, ") yet."), call. = FALSE)
  }
}
