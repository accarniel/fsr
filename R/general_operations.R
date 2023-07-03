#' @title Add components to a `pgeometry` object
#'
#' @description `spa_add_component()` inserts components into a spatial plateau object (i.e., `pgeometry` object). 
#'
#' @usage
#'
#' spa_add_component(pgo, components, is_valid = FALSE)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param components A `component` object or a list of `component` objects.
#' @param is_valid A Boolean value to check if the user wants to validate the updated spatial plateau object at the end. If `is_valid = TRUE`, it calls `validObject()` method.
#'
#' @details
#'
#' This function implements the \eqn{\odot}{odot} operator defined by Spatial Plateau Algebra. 
#' The goal of this function is to insert a component or a list of components into a `pgeometry` object. 
#' The crisp spatial object of the component must be compatible with the type of the plateau spatial object.
#' For instance, a `pregion` object accepts only components containing polygons (e.g., `POLYGON` or `MULTIPOLYGON`). 
#' In the case of `pcomposition` object any type of component is compatible to be added. 
#' For instance, a point component is added to the plateau point sub-object of the plateau composition object.
#' On the other hand, as a `pcollection` object can have multiple spatial objects of the same type, this function is not applicable to it.
#' 
#' The insertion is based on the membership degree of the component. Thus, it preserves the properties of a spatial plateau object.
#' However, `spa_add_component()` assumes that the geometric format of the component is valid (i.e., it does not overlap with existing components).
#'  
#' @return
#'
#' A `pgeometry` object containing the `component` objects.
#'
#' @references
#' 
#' The formal definition of the \eqn{\odot}{odot} operator is described in: 
#'
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' comp1 <- create_component("MULTIPOINT(1 1, 2 2)", 0.2) 
#' comp2 <- create_component("POINT(1 5)", 0.8)  
#' 
#' # appending these components into an empty pgeometry object
#' pp <- create_empty_pgeometry("PLATEAUPOINT")
#' pp <- spa_add_component(pp, list(comp1, comp2))
#' pp
#' 
#' # inserting components with existing membership degrees are merged
#' comp3 <- create_component("MULTIPOINT(0 0, 4 4)", 0.2)
#' pp <- spa_add_component(pp, comp3)
#' pp
#' 
#' comp4 <- create_component("MULTIPOINT(0 1, 3 4)", 1)
#' pc <- create_pgeometry(list(comp4), "PLATEAUCOMPOSITION")
#' pc
#' 
#' # appending these components into pc
#' comp5 <- create_component("LINESTRING(-1 1, 2 2)", 0.9)
#' comp6 <- create_component("POLYGON((0 0, 1 4, 2 2, 0 0))", 0.4)
#' pc <- spa_add_component(pc, list(comp5, comp6))
#' pc
#' @import  sf
#' @export
spa_add_component <- function(pgo, components, is_valid = FALSE) {
  
  if(is.null(pgo)) {
    stop("pgo is null. Please use create_empty_pgeometry() to
         create an empty spatial plateau object.", call. = FALSE)
  }
  
  if(is.null(components)) {
    stop("components is null. It should be a single component or a list of components.", call. = FALSE)
  }
  
  type <- spa_get_type(pgo)
  
  if(type == "PLATEAUCOLLECTION") {
    stop("It is not possible to add components to a spatial plateau collection object.", call. = FALSE)
  }
  
  if(!inherits(components, "list")) {
    components <- list(components)
  }
  
  types <- sapply(lapply(components, is), function(x) x[[1]])
  components <- components[(types == "component")]
  
  pgo <- spa_add_internal(pgo, components)
  
  if(is_valid) {
    validObject(pgo)
  }
  pgo
}

#' @import  sf
#' @noRd
spa_add_internal <- function(pgo, components) {
  
  # This internal function finds the index of the component of a plateau plateau object based on a given membership degree.
  # It uses an interactive version of the binary search algorithm.
  # Parameters:
  # components is a list of components
  # low is the first index of the list to start the binary search
  # high is the last index of the list to end the binary search
  # m is the membership degree to be located
  #
  # If found, it returns a vector with TRUE and the index position.
  # If not found, returns a vector with FALSE and the first position given (low)
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
  
  type <- spa_get_type(pgo)
  
  for(component in components) {
    c <- component@obj
    m <- component@md
    
    # does nothing, lets check the next component
    if(is.null(c) || st_is_empty(c) || m == 0) {
      next
    }
    
    if(type == "PLATEAUCOMPOSITION") {
      if(is_compatible(c, "PLATEAUPOINT")) {
        spobj  <- pgo@ppoint
      } else if(is_compatible(c, "PLATEAULINE")) {
        spobj  <- pgo@pline
      } else if(is_compatible(c, "PLATEAUREGION")) {
        spobj <- pgo@pregion
      } else {
        warning(paste(c, " is not compatible with the spatial plateau object; we will skip this component.", sep = ' '), call. = FALSE)
        next
      }
    } else {
      if(!is_compatible(c, type)) {
        warning(paste(c, " is not compatible with the spatial plateau object; we will skip this component.", sep = ' '), call. = FALSE)
        next
      } else {
        spobj <- pgo
      }
    }
    
    # 2. if the pgo is empty and the component is not empty and has a membership greater than 0
    if(spa_is_empty(spobj) && !(is.null(c) || st_is_empty(c)) && m > 0) {
      spobj@component[[1]] <- component
      spobj@supp <- c 
    } else if(!is.null(c) && length(spobj@component) >= 1) {
      index = search_by_md(spobj@component, 1, length(spobj@component), m)
      
      # 3. if the membership degree exists in the pgo, we should merge it
      if(index[1] == TRUE) {
        spobj@component[[index[2]]]@obj <- st_union(spobj@component[[index[2]]]@obj, c)
      } else {
        #otherwise, we simply append into the correct location
        spobj@component <- append(spobj@component, component, after=index[2]-1)
      }
      #in both cases we update its support
      spobj@supp <- st_union(spobj@supp, c)
    }
    
    if(type == "PLATEAUCOMPOSITION") {
      if(inherits(spobj, c("ppoint"))) {
        pgo@ppoint <- spobj
      } else if(inherits(spobj, c("pline"))) {
        pgo@pline <- spobj
      } else {
        pgo@pregion <- spobj
      }
      pgo@supp <- st_union(pgo@supp, c)
    } else {
      pgo <- spobj
    }
  }
  pgo
}

#' @title Evaluate the membership degree of a point in a `pgeometry` object
#'
#' @description `spa_eval()` evaluates the membership degree of a given point in a spatial plateau object of any type.
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
#' The `spa_eval()` returns the membership degree of a simple point object (i.e., `sfg` object) in a given spatial plateau object (i.e., `pgeometry` object). 
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
#' Formal definitions of this function are described in:
#' 
#' - [Carniel, A. C.; Galdino, F.; Schneider, M. Evaluating Region Inference Methods by Using Fuzzy Spatial Inference Models. In Proceedings of the 2022 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2022), pp. 1-8, 2022.](https://ieeexplore.ieee.org/document/9882658)
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' library(sf)
#' 
#' # Point components
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' pcp4 <- create_component("MULTIPOINT((1 2), (2 1), (3 2))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0.5), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3.5))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (4 2))", 0.4)
#' # Line components
#' lcp1 <- create_component("LINESTRING(0 0, 1 1.5)", 0.2)
#' lcp2 <- create_component("LINESTRING(1 3, 1 2, 2 0.5)", 0.5)
#' lcp3 <- create_component("LINESTRING(2 1.2, 3 1.6, 4 4)", 0.7)
#' lcp4 <- create_component("LINESTRING(1 1.5, 2 1.2)", 1.0)
#' lcp5 <- create_component("LINESTRING(-1 1, 2 2)", 0.9)
#' # Polygon components
#' rcp1 <- create_component("POLYGON((0 0, 1 4, 2 2, 0 0))", 0.4)
#' rcp2 <- create_component("POLYGON((2 0.5, 4 1, 4 0, 2 0.5))", 0.8)
#' 
#' # Creating spatial plateau objects
#' ppoint <- create_pgeometry(list(pcp1, pcp2, pcp3, pcp4, pcp5), "PLATEAUPOINT")
#' pline <- create_pgeometry(list(lcp1, lcp2, lcp3), "PLATEAULINE")
#' pregion <- create_pgeometry(list(rcp1, rcp2), "PLATEAUREGION")
#' pcomp <- create_pgeometry(list(pcp6, pcp7, lcp4, lcp5), "PLATEAUCOMPOSITION")
#' pcol <- create_pgeometry(list(ppoint, pline, pregion, pcomp), "PLATEAUCOLLECTION")
#' 
#' point <- st_point(c(0, 0))
#' 
#' spa_eval(ppoint, point)
#' spa_eval(pline, point)
#' spa_eval(pregion, point)
#' spa_eval(pcomp, point)
#' spa_eval(pcol, point)
#' @import  sf
#' @export
spa_eval <- function(pgo, point) {
  
  if(any(is.na(point))) {
    stop("The parameter 'point' is NA.", call. = FALSE)
  }
  
  if(!(st_is(point, "POINT") && inherits(point, "sfg"))) {
    stop("The parameter 'point' is not a simple point object of class sfg.", call. = FALSE)
  }
  
  ret <- 0
  
  if(st_intersects(point, pgo@supp, sparse = FALSE)[1]) {
    type = spa_get_type(pgo)
    if(type == "PLATEAUCOMPOSITION") {
      ppoint_ret <- spa_eval(pgo@ppoint, point)
      pline_ret <- spa_eval(pgo@pline, point)
      pregion_ret <- spa_eval(pgo@pregion, point)
      ret <- max(c(ppoint_ret, pline_ret, pregion_ret))
    } else if(type == "PLATEAUCOLLECTION") {
      ret <- max(sapply(pgo@pgos, spa_eval, point)) 
    } else {
      # check in the boundary
      md_comps <- c()
      for(component in pgo@component) {
        if(st_intersects(point, st_boundary(component@obj), sparse = FALSE)[1]) {
          md_comps <- append(md_comps, component@md)
        } #  check in its interior...
        else if(st_intersects(point, component@obj, sparse = FALSE)[1]) {
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

#' @title Get the support of a `pgeometry` object
#'
#' @description `spa_support()` yields a crisp spatial object (as an `sfg` object) that corresponds to the support of a `pgeometry` object given as input.
#'
#' @usage
#'
#' spa_support(pgo)
#'
#' @param pgo A `pgeometry` object of any type.
#'
#' @details
#'
#' The `spa_support()` function employs the classical definition of _support_ from the fuzzy set theory in the context of Spatial Plateau Algebra. 
#' The _support_ only comprises the points with membership degree greater than or equal to 1.
#' Hence, `spa_support()` returns the `sfg` object that represents the total extent of the `pgeometry` given as input. 
#' If the `pgeometry` is empty, then an empty `sfg` object is returned.
#'
#' @return
#'
#' An `sfg` object that represents the support of `pgeometry`. It can be an empty object, if `pgeometry` is empty.
#'
#' @references
#' 
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' Underlying concepts and formal definitions of Spatial Plateau Algebra are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' pcp4 <- create_component("MULTIPOINT((1 2), (2 1), (3 2))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0.5), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3.5))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (4 2))", 0.4)
#' 
#' # Creating a plateau point object
#' ppoint <- create_pgeometry(list(pcp1, pcp2, pcp3, pcp4, pcp5), "PLATEAUPOINT")
#' ppoint
#' 
#' # Getting its support
#' spa_support(ppoint)
#' 
#' # Getting the support of an empty pgeometry
#' spa_support(create_empty_pgeometry("PLATEAUREGION"))
#' @export
spa_support <- function(pgo){
  pgo@supp
}

#' Gets the crisp spatial data type compatible with the given pgeometry object
#'
#' @noRd
get_counter_ctype <- function(pgo){
  ptype = spa_get_type(pgo)
  switch(ptype,
         PLATEAUPOINT = "POINT",
         PLATEAULINE = "LINESTRING",
         PLATEAUREGION = "POLYGON",
         PLATEAUCOMPOSITION = "GEOMETRYCOLLECTION",
         PLATEAUCOLLECTION = "GEOMETRYCOLLECTION")
}

#' @title Get the core of a `pgeometry` object
#'
#' @description `spa_core()` yields a crisp spatial object (as an `sfg` object) that corresponds to the core of a `pgeometry` object given as input.
#'
#' @usage
#'
#' spa_core(pgo)
#'
#' @param pgo A `pgeometry` object of any type.
#'
#' @details
#'
#' The `spa_core()` function employs the classical definition of _core_ from the fuzzy set theory in the context of Spatial Plateau Algebra. 
#' The _core_ only comprises the points with membership degree equal to 1.
#' Hence, this operation returns the `sfg` object that represents the component labeled with 
#' membership degree equal to 1 of the `pgeometry` object given as input. If the `pgeometry` object has no core, then an empty `sfg` object is returned.
#'
#' @return
#'
#' An `sfg` object that represents the core of `pgo`. It can be an empty object, if `pgo` does not have a component with membership degree 1.
#'
#' @references
#' 
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' Underlying concepts and formal definitions of Spatial Plateau Algebra are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' pcp4 <- create_component("MULTIPOINT((1 2), (2 1), (3 2))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0.5), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3.5))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (4 2))", 0.4)
#' 
#' # Creating a plateau point object
#' ppoint <- create_pgeometry(list(pcp1, pcp2, pcp3, pcp4, pcp5), "PLATEAUPOINT")
#' ppoint
#' 
#' # Getting its core
#' spa_core(ppoint)
#' 
#' # Getting the core of an empty pgeometry
#' spa_core(create_empty_pgeometry("PLATEAUREGION"))
#' @import sf
#' @export
spa_core <- function(pgo) {
  type <- spa_get_type(pgo)
  if(spa_is_empty(pgo)) {
    sf_type <- get_counter_ctype(pgo)
    sfg_obj <- switch(sf_type,
                      POINT = st_point(),
                      LINESTRING = st_linestring(),
                      POLYGON = st_polygon(),
                      GEOMETRYCOLLECTION = st_geometrycollection())
    return(sfg_obj)
  } else {
    if(type == "PLATEAUCOMPOSITION") {
      triple_core <- st_sfc(spa_core(pgo@ppoint), spa_core(pgo@pline), spa_core(pgo@pregion))
      return(st_union(triple_core)[[1]])
    } else if(type == "PLATEAUCOLLECTION") {
      core_list <- lapply(pgo@pgos, spa_core)
      return(st_union(st_sfc(core_list))[[1]])
    } else {
      last_comp <- pgo@component[[length(pgo@component)]]
      if(last_comp@md == 1) {
        return(last_comp@obj)
      } else {
        spa_core(create_empty_pgeometry(type))
      }
    } 
  }
}

#' @title Return a crisp spatial object formed by geometric parts of a `pgeometry` object
#'
#' @description These functions yield a crisp spatial object (as an `sfg` object) formed by the geometric parts of the components of the `pgeometry` given as input that satisfy a filter condition based on their membership degrees.
#'
#' @usage
#'
#' spa_range(pgo, lvalue, rvalue, lside_closed = TRUE, rside_closed = TRUE)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param lvalue A numeric value denoting the left side of an interval in \[0, 1\].
#' @param rvalue A numeric value denoting the right side of an interval in \[0, 1\].
#' @param lside_closed A Boolean value indicating whether the left side is closed or not. The default value is `TRUE`.
#' @param rside_closed A Boolean value indicating whether the right side is closed or not. The default value is `TRUE`.
#'
#' @name fsr_filter_operations
#'
#' @details
#'
#' Given a spatial plateau object as input, these functions return a crisp spatial object formed by the geometric parts of the components of the input that satisfy a filter 
#' condition based on their membership degrees. The filter condition of each function is detailed as follows:
#' 
#' - `spa_alpha_cut()` selects all components that have membership degrees greater than or equal to a given value in \[0, 1\] indicated by the parameter `alpha`. 
#' - `spa_strict_alpha_cut()` picks a subset of components that have membership values greater than the parameter `alpha` (a value in \]0, 1\]). 
#' - `spa_range()` generalizes these two operations and allows one to pick all components that have membership degrees belonging to a given open or closed interval. 
#' The parameters `lside_closed` and `rside_closed`, respectively, determine whether the left and right side (parameters `lvalue` and `rvalue`) of the interval is open (`FALSE`) or closed (`TRUE`). 
#' For example, to represent the right open interval \[0.5, 0.8\[, the following parameter values should be given: `lvalue = 0.5, rvalue = 0.8, lside_closed = TRUE, rside_closed = FALSE`.
#'
#' @return
#'
#' An `sfg` object that represents the geometric union of the components extracted after applying the specific filter condition.
#'
#' @references
#'
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#'
#' @examples
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' pcp4 <- create_component("MULTIPOINT((1 2), (2 1), (3 2))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0.5), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3.5))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (4 2))", 0.4)
#' 
#' # Creating a plateau point object
#' ppoint <- create_pgeometry(list(pcp1, pcp2, pcp3, pcp4, pcp5), "PLATEAUPOINT")
#' ppoint
#' 
#' # Processing the alpha-cut, strict alpha-cut, and range
#' spa_alpha_cut(ppoint, 0.7)
#' spa_strict_alpha_cut(ppoint, 0.7)
#' spa_range(ppoint, 0.4, 0.8)
#' @import sf
#' @export
spa_range <- function(pgo, lvalue, rvalue, lside_closed = TRUE, rside_closed = TRUE) {
  type <- spa_get_type(pgo)
  if(spa_is_empty(pgo)) {
    sf_type <- get_counter_ctype(pgo)
    sfg_obj <- switch(sf_type,
                      POINT = st_point(),
                      LINESTRING = st_linestring(),
                      POLYGON = st_polygon(),
                      GEOMETRYCOLLECTION = st_geometrycollection())
    return(sfg_obj)
  } else {
    if(type == "PLATEAUCOMPOSITION") {
      triple <- st_sfc(spa_range(pgo@ppoint, lvalue, rvalue, lside_closed, rside_closed), 
                       spa_range(pgo@pline, lvalue, rvalue, lside_closed, rside_closed), 
                       spa_range(pgo@pregion, lvalue, rvalue, lside_closed, rside_closed))
      return(st_union(triple)[[1]])
    } else if(type == "PLATEAUCOLLECTION") {
      range_list <- lapply(pgo@pgos, spa_range, lvalue = lvalue, rvalue = rvalue, lside_closed = lside_closed, rside_closed = rside_closed)
      return(st_union(st_sfc(range_list))[[1]])
    } else{
      # filtering out elements that do not satisfy the condition
      filtered_list <- pgo@component[sapply(pgo@component, function(x) {
        md <- x@md
        if (lside_closed) {
          if (rside_closed) {
            md >= lvalue & md <= rvalue
          } else {
            md >= lvalue & md < rvalue
          }
        } else {
          if (rside_closed) {
            md > lvalue & md <= rvalue
          } else {
            md > lvalue & md < rvalue
          }
        }
      })]
      if(length(filtered_list) > 0) {
        objs <- lapply(filtered_list, attr, "obj")
        return(st_union(st_sfc(objs))[[1]])
      } else {
        spa_core(create_empty_pgeometry(type))
      }
    } 
  }
}

#' @name fsr_filter_operations
#' 
#' @usage
#' 
#' spa_alpha_cut(pgo, alpha) 
#' 
#' @param alpha A numeric value. For `spa_alpha_cut()`, it must be in \[0, 1\]. For `spa_strict_alpha_cut()`, it must be in \]0, 1\].
#' 
#' @import sf
#' @export
spa_alpha_cut <- function(pgo, alpha) {
  if(alpha < 0 && alpha > 1) {
    stop("'alpha' must be in [0, 1]", call. = FALSE)
  }
  spa_range(pgo, alpha, 1, TRUE, TRUE)
}

#' @name fsr_filter_operations
#' 
#' @usage
#' 
#' spa_strict_alpha_cut(pgo, alpha) 
#' 
#' @import sf
#' @export
spa_strict_alpha_cut <- function(pgo, alpha) {
  if(alpha <= 0 && alpha > 1) {
    stop("'alpha' must be in ]0, 1]", call. = FALSE)
  }
  spa_range(pgo, alpha, 1, FALSE, TRUE)
}

#' @title Capture the fuzzy boundary of a plateau region object
#'
#' @description `spa_boundary_pregion()` yields a specific part of the fuzzy boundary of a plateau region object. This function is deprecated; use `spa_boundary()`.
#'
#' @usage
#'
#' spa_boundary_pregion(pregion, bound_part = "region")
#'
#' @param pregion A `pregion` object. It throws an error if a different type is given.
#' @param bound_part A character value that indicates the part of the fuzzy boundary to be returned. It can be `"region"` or `"line"`. See below for more details.
#'
#' @details
#'
#' The `spa_boundary_pregion()` function employs the definition of _fuzzy boundary_ of a fuzzy region object in the context of Spatial Plateau Algebra. 
#' The _fuzzy boundary_ of a fuzzy region object `A` has a heterogeneous nature since it consists of two parts:
#' - a fuzzy line object that corresponds to the boundary of the core of `A`.
#' - a fuzzy region object that comprises all points of `A` with a membership degree greater than 0 and less than 1.
#' 
#' This means that `spa_boundary_pregion()` can yield one specific part of the fuzzy boundary of a plateau region object.
#' If `boundary = "line"`, then the function returns the boundary plateau line of `pregion` (i.e., returns a `pline` object).
#' Else if `boundary = "region"` (the default value), then the function returns the boundary plateau region of `pregion` (i.e., returns a `pregion` object).
#' 
#' This function is deprecated; use `spa_boundary()`.
#' 
#' @return
#'
#' A `pgeometry` object that represents a specific part of the fuzzy boundary of `pgeometry` object given as input.
#'
#' @references
#'
#' Concepts of fuzzy boundary of plateau region objects are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' library(sf)
#' library(ggplot2)
#' 
#' # defining two different types of membership functions
#' trap_mf <- function(a, b, c, d) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), 1, (d - x)/(d - c), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' trim_mf <- function(a, b, c) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), (c - x)/(c - b), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' set.seed(7)
#' tbl = tibble(x = runif(10, min = 0, max = 30), 
#'              y = runif(10, min = 0, max = 50), 
#'              z = runif(10, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- trap_mf(0, 10, 20, 35)
#' hot_mf <- trim_mf(35, 50, 100)
#' 
#' # Getting the convex hull on the points to clip plateau region objects during their constructions
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' # Using the standard fuzzification policy based on fuzzy sets
#' pregions <- spa_creator(tbl, classes = classes, mfs = c(cold_mf, hot_mf), base_poly = ch)
#' plot(pregions$pgeometry[[1]]) + ggtitle("Cold")
#' plot(pregions$pgeometry[[2]]) + ggtitle("Hot")
#' 
#' # these functions are now deprecated, use `spa_boundary()`
#' 
#' # capturing and showing the boundary plateau line of each pgeometry object previously created
#' (spa_boundary_pregion(pregions$pgeometry[[1]], bound_part = "line")) 
#' (spa_boundary_pregion(pregions$pgeometry[[2]], bound_part = "line"))
#' # this part of the boundary is empty because there is no core! 
#' # capturing and showing the boundary plateau region (this is the default behavior)
#' (spa_boundary_pregion(pregions$pgeometry[[1]]))
#' (spa_boundary_pregion(pregions$pgeometry[[2]]))
#' }
#' @import methods sf
#' @export
spa_boundary_pregion <- function(pregion, bound_part = "region") {
  .Deprecated("spa_boundary")
  
  type <- spa_get_type(pregion)
  if(type != "PLATEAUREGION") {
    stop("pregion is not a PLATEAUREGION object.", call. = FALSE)
  }
  
  if(bound_part == "line") {
    bpl <- create_empty_pgeometry("PLATEAULINE")
    last_comp <- pregion@component[[length(pregion@component)]]
    if(last_comp@md == 1) {
      boundary_component <- st_boundary(last_comp@obj)
      comp_line <- new("component", obj = boundary_component, md=1)
      bpl <- spa_add_component(bpl, comp_line)
    }
    return(bpl)
  } else if(bound_part == "region") {
    last_comp <- pregion@component[[length(pregion@component)]]
    n_comps <- spa_ncomp(pregion)
    if(last_comp@md == 1 && n_comps > 1) {
      bpr <- create_empty_pgeometry("PLATEAUREGION")
      bpr <- spa_add_component(bpr, pregion@component[1:n_comps-1])
    } else {
      ret <- new("pregion", component = pregion@component, supp = pregion@supp)
      return(ret)
    }
  } else {
    stop("Invalid value for the parameter 'bound_part'.", call. = FALSE)
  }
}

#' @title Capture the fuzzy boundary of a spatial plateau object
#'
#' @description `spa_boundary()` yields the fuzzy boundary of a homogeneous spatial plateau object.
#'
#' @usage
#'
#' spa_boundary(pgo)
#'
#' @param pgo A `pgeometry` object of type `ppoint`, `pline`, or `pregion`.
#'
#' @details
#'
#' The `spa_boundary()` function employs the definition of _fuzzy boundary_ in the context of Spatial Plateau Algebra. 
#' The _fuzzy boundary_ of a fuzzy spatial object has a heterogeneous nature. For instance, the fuzzy boundary of a plateau region object consists of two parts:
#' - a plateau line object that corresponds to the boundary of the core of `A`.
#' - a plateau region object that comprises all points of `A` with a membership degree greater than 0 and less than 1.
#' 
#' This means that `spa_boundary()` returns a `pcomposition` object.
#' 
#' @return
#'
#' A `pcomposition` object that represents a fuzzy boundary of the `pgeometry` object given as input.
#'
#' @references
#' 
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' Concepts and formal definitions of fuzzy boundary are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' library(tibble)
#' library(sf)
#' library(ggplot2)
#' 
#' # defining two different types of membership functions
#' trap_mf <- function(a, b, c, d) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), 1, (d - x)/(d - c), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' set.seed(7)
#' tbl = tibble(x = runif(20, min = 0, max = 30), 
#'              y = runif(20, min = 0, max = 50), 
#'              z = runif(20, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- trap_mf(0, 10, 20, 35)
#' hot_mf <- trap_mf(20, 50, 100, 100)
#' 
#' # Getting the convex hull on the points to clip plateau region objects during their constructions
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' # Using the standard fuzzification policy based on fuzzy sets
#' pregions <- spa_creator(tbl, classes = classes, mfs = c(cold_mf, hot_mf), base_poly = ch)
#' \dontrun{
#' pregions
#' plot(pregions$pgeometry[[1]]) + ggtitle("Cold")
#' plot(pregions$pgeometry[[2]]) + ggtitle("Hot")
#' }
#' # capturing and showing the boundary of each pgeometry object previously created
#' boundary_cold <- spa_boundary(pregions$pgeometry[[1]])
#' boundary_hot <- spa_boundary(pregions$pgeometry[[2]])
#' \dontrun{
#' plot(boundary_cold) + ggtitle("Boundary (Cold)")
#' plot(boundary_hot) + ggtitle("Boundary (Hot)")
#' }
#' @import methods sf
#' @export
spa_boundary <- function(pgo) {
  if(spa_is_empty(pgo)) {
    return(create_empty_pgeometry("PLATEAUCOMPOSITION"))
  }
  
  type <- spa_get_type(pgo)
  if(!(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION"))) {
    stop("pgo is not a PLATEAUPOINT, PLATEAULINE, or PLATEAUREGION object.", call. = FALSE)
  } else {
    if(type == "PLATEAUPOINT") {
      return(create_empty_pgeometry("PLATEAUCOMPOSITION"))
    }
    
    comps <- list()
    last_comp <- pgo@component[[length(pgo@component)]]
    if(last_comp@md == 1) {
      # point or multipoint sf object (if pgo is a plateau line)
      # linestring or multilinestring sf object (if pgo is a plateau region)
      boundary_component <- st_boundary(last_comp@obj)
      comp <- new("component", obj = boundary_component, md = 1)
      comps <- append(comps, comp)
      n_comps <- spa_ncomp(pgo)
      if(n_comps > 1) {
        # linestring or multilinestring sf object (if pgo is a plateau line)
        # polygon or multipolygon sf object (if pgo is a plateau region)
        comp <- pgo@component[1:n_comps-1]
        comps <- append(comps, comp)
      }
    } else {
      comps <- append(comps, pgo@component)
    }
    create_pgeometry(comps, type = "PLATEAUCOMPOSITION", is_valid = FALSE)
  }
}

#' @title Capture the frontier of a plateau region object
#'
#' @description `spa_contour()` extracts the frontier (i.e., linear boundary) of a plateau region object by maintaining its membership degrees.
#'
#' @usage
#'
#' spa_contour(pregion)
#'
#' @param pregion A `pregion` object. It throws an error if a different type is given.
#'
#' @details
#'
#' The `spa_contour()` function implements the definition of _fuzzy frontier_ of a fuzzy region object in the context of Spatial Plateau Algebra. 
#' The _fuzzy frontier_ of a fuzzy region object `A` collects all single points of `A`, preserving its membership degrees, that are not in the interior of its support.
#' 
#' Note that fuzzy frontier is different from fuzzy boundary (see `spa_boundary()`).
#' 
#' @return
#'
#' A `pline` object that represents the contour (i.e. frontier) of a plateau region object given as input.
#'
#' @references
#'
#' - [Carniel, A. C.; Schneider, M. A Conceptual Model of Fuzzy Topological Relationships for Fuzzy Regions. In Proceedings of the 2016 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2016), pp. 2271-2278, 2016.](https://ieeexplore.ieee.org/document/7737976)
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#'
#' @examples
#' library(tibble)
#' library(sf)
#' library(ggplot2)
#' 
#' # defining two different types of membership functions
#' trap_mf <- function(a, b, c, d) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), 1, (d - x)/(d - c), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' set.seed(7)
#' tbl = tibble(x = runif(20, min = 0, max = 30), 
#'              y = runif(20, min = 0, max = 50), 
#'              z = runif(20, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- trap_mf(0, 10, 20, 35)
#' hot_mf <- trap_mf(20, 50, 100, 100)
#' 
#' # Getting the convex hull on the points to clip plateau region objects during their constructions
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' 
#' # Using the standard fuzzification policy based on fuzzy sets
#' pregions <- spa_creator(tbl, classes = classes, mfs = c(cold_mf, hot_mf), base_poly = ch)
#' pregions
#' \dontrun{
#' plot(pregions$pgeometry[[1]]) + ggtitle("Cold")
#' plot(pregions$pgeometry[[2]]) + ggtitle("Hot")
#' }
#' # capturing and showing the frontier of each pgeometry object previously created
#' cold_contour <- spa_contour(pregions$pgeometry[[1]])
#' hot_contour <- spa_contour(pregions$pgeometry[[2]])
#' \dontrun{
#' plot(cold_contour) + ggtitle("Frontier (Cold)")
#' plot(hot_contour) + ggtitle("Frontier (Hot)")
#' }
#' @import sf
#' @export
spa_contour <- function(pregion) {
  type <- spa_get_type(pregion)
  
  if(type != "PLATEAUREGION") {
    stop("pregion must be a PLATEAUREGION type.", call. = FALSE)
  }
  
  if(spa_is_empty(pregion)) {
    return(create_empty_pgeometry("PLATEAULINE"))
  }
  
  pregion_tibble <- as_tibble(pregion)
  pregion_tibble$boundary <- st_boundary(pregion_tibble$geometry)
  pregion_df <- as.data.frame(pregion_tibble)
  pline <- create_pgeometry(pregion_df[,c(3,2)], "PLATEAULINE", is_valid = FALSE)
  
  crisp_contour <- create_empty_pgeometry("PLATEAULINE")
  crisp_contour <- spa_add_component(crisp_contour, create_component(st_boundary(pregion@supp), 1))
  
  result <- spa_intersection(pline, crisp_contour, as_pcomposition = TRUE)
  result@pline
}