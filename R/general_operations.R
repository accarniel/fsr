#' @title Adding components to a `pgeometry` object
#'
#' @description This function adds components to a spatial plateau object (i.e., `pgeometry` object). 
#' The crisp spatial object of the component must be compatible with the type of the plateau spatial object.
#' For instance, a `pgeometry` object of the type `PLATEAUREGION` accepts only components containing polygons (e.g., `POLYGON` or `MULTIPOLYGON`). 
#'
#' @usage
#'
#' spa_add_component(pgo, components)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param components A `component` object or a list of `component` objects.
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
#' @import  sf
#' @export
spa_add_component <- function(pgo, components) {
  if(is.null(pgo)){
    stop("pgo is null. Please use create_pgeometry() to
         create an empty spatial plateau object.", call. = FALSE)
  }

  if(is.null(components)){
    stop("components is null. It should be a single component or a list of components.", call. = FALSE)
  }

  if(!inherits(pgo, "pgeometry")){
    stop(paste(pgo, "is not a pgeometry object.", sep = ' '), call. = FALSE)
  }

  if(!inherits(components, "list")) {
    components <- list(components)
  }

  #should we check all components or just the first one?
  if(!inherits(components[[1]], "component")){
    stop(paste(components, " is not a component object.", sep = ' '), call. = FALSE)
  }


  for(component in components) {
    c <- component@obj
    m <- component@md

    # does nothing, lets check the next component
    if(is.null(c) || st_is_empty(c) || m == 0){
      next
    }

    # 2. if the pgo is empty and the component is not empty and has a membership greater than 0
    if(fsr_is_empty(pgo) && !(is.null(c) || st_is_empty(c)) && m > 0){
      pgo@component[[1]] <- component
      pgo@supp <- c

    } else if(!is.null(c) && length(pgo@component) >= 1){
      index = search_by_md(pgo@component, 1, length(pgo@component), m)

      # 3. if the membership degree exists in the pgo, we should merge it
      if(index[1] == TRUE){
        pgo@component[[index[2]]]@obj <- st_union(pgo@component[[index[2]]]@obj, c)
      } else {
        #otherwise, we simply append into the correct location
        pgo@component <- append(pgo@component, component, after=index[2]-1)
      }
      #in both cases we update its support
      pgo@supp <- st_union(pgo@supp, c)
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
    # check in the boundary
    md_comps <- c()
    for(component in pgo@component){
      if(st_intersects(point, st_boundary(component@obj), sparse = FALSE)[1]){
        md_comps <- append(md_comps, component@md)
      } #  check in its interior...
      else if(st_intersects(point, component@obj, sparse = FALSE)[1]){
        if(pgo@type %in% c("PLATEAUPOINT", "PLATEAUREGION")){
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
  return(pgo@supp)
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

  if(!fsr_is_empty(pgo)) {
    last_comp <- tail(pgo@component, 1)
    
    if(last_comp[[1]]@md == 1){
      return(last_comp[[1]]@obj)
    }
  } 
  
  sf_type <- get_counter_ctype(pgo)

  sfg_obj <- switch(sf_type,
                    POINT = st_point(),
                    LINESTRING = st_linestring(),
                    POLYGON = st_polygon())
  sfg_obj
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
#' # capturing and showing the boundary plateau line of each pgeometry object previously created
#' (spa_boundary_pregion(pregions$pgeometry[[1]], bound_part = "line")) 
#' (spa_boundary_pregion(pregions$pgeometry[[2]], bound_part = "line"))
#' # the last boundary is empty because there is no core! 
#' 
#' # capturing and showing the boundary plateau region (this is the default behavior)
#' (spa_boundary_pregion(pregions$pgeometry[[1]]))
#' (spa_boundary_pregion(pregions$pgeometry[[2]]))
#'
#' @import methods utils sf
#' @export
spa_boundary_pregion <- function(pregion, bound_part = "region"){

  if(pregion@type != "PLATEAUREGION"){
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
      ret <- new("pgeometry", component = pregion@component, supp = pregion@supp, type = pregion@type)
      return(ret)
    }
  }
  else {
    stop("Invalid value for the parameter 'bound_part'.", call. = FALSE)
  }
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
  if(pregion@type != "PLATEAUREGION"){
    stop("pregion must be a PLATEAUREGION type.", call. = FALSE)
  }
  
  pregion_tibble <- as_tibble(pregion)
  pregion_tibble$boundary <- st_boundary(pregion_tibble$geometry)
  pregion_df <- as.data.frame(pregion_tibble)
  pline <- create_pgeometry(pregion_df[,c(3,1)], "PLATEAULINE")
  
  crisp_contour <- create_empty_pgeometry("PLATEAULINE")
  crisp_contour <- spa_add_component(crisp_contour, component_from_sfg(st_boundary(pregion@supp), 1))
  
  spa_intersection(pline, crisp_contour)
}