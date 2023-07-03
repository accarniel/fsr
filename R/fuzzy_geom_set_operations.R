# we include the functions `spa_flatten()` and `pcollection_to_pcomposition()` in this file because they are related to fuzzy geometric set operations on `pcollection` objects.
# TODO we can create another category of functions, e.g., type-specific operations.

#' @title Flatten a plateau collection object
#'
#' @description `spa_flatten()` gathers all the objects of a plateau collection object and 
#' reorganizes them into a single flattened spatial plateau object containing a quadruple
#' (`PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`, `PLATEAUCOMPOSITION`) that preserves the identity of sub-objects.
#'
#' @usage
#'
#' spa_flatten(pcol)
#'
#' @param pcol A `pcollection` object.
#'
#' @details
#'
#' The `spa_flatten()` function yields a single flattened spatial plateau object, aggregating all spatial plateau objects by their types.
#' In the case of a two-level hierarchy, i.e., a plateau collection inside another one, 
#' the function is applied recursively in the lower levels until the quadruple is built. Hence, it simplifies the representation of complex plateau collection objects. 
#' The t-conorm considered in the aggregation is the `max` operator.
#'
#' @return
#'
#' A `pcollection` object consisting of a quadruple (`PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`, `PLATEAUCOMPOSITION`).
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'
#' @examples
#' # Point components
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' pcp4 <- create_component("MULTIPOINT((10 10), (9 8), (7 7))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (2 3))", 0.4)
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
#' # Creating plateau point objects
#' ppoint1 <- create_pgeometry(list(pcp1, pcp2, pcp3), "PLATEAUPOINT")
#' ppoint2 <- create_pgeometry(list(pcp4, pcp5), "PLATEAUPOINT")
#' ppoint3 <- create_pgeometry(list(pcp4, pcp5), "PLATEAUPOINT")
#' ppoint4 <- create_pgeometry(list(pcp6, pcp7), "PLATEAUPOINT")
#' # Creating plateau line objects
#' pline1 <- create_pgeometry(list(lcp1, lcp3), "PLATEAULINE")
#' pline2 <- create_pgeometry(list(lcp2, lcp4), "PLATEAULINE")
#' pline3 <- create_pgeometry(list(lcp5), "PLATEAULINE")
#' # Creating a plateau region objects
#' pregion <- create_pgeometry(list(rcp1, rcp2), "PLATEAUREGION")
#' # Creating a plateau composition object
#' pcomposition <- create_pgeometry(list(ppoint4, pline3), "PLATEAUCOMPOSITION")
#' # Creating plateau collection objects
#' pcol1 <- create_pgeometry(list(ppoint1, ppoint2, ppoint3, pline1), "PLATEAUCOLLECTION")
#' pcol2 <- create_pgeometry(list(pline2, pregion, pcomposition, pcol1), "PLATEAUCOLLECTION")
#' \dontrun{
#' pcol2
#' plot(pcol2)
#' 
#' flatten_col <- spa_flatten(pcol2)
#' flatten_col
#' plot(flatten_col)
#' }
#' @import methods
#' @export
spa_flatten <- function(pcol) {
  if(spa_is_empty(pcol)) {
    return(pcol)
  }
  
  pgo_list <- get_pgos_from_pcollection(pcol)
  types <- lapply(pgo_list, spa_get_type)
  
  ppoints <- pgo_list[types == "PLATEAUPOINT"]
  plines <- pgo_list[types == "PLATEAULINE"]
  pregions <- pgo_list[types == "PLATEAUREGION"]
  pcompositions <- pgo_list[types == "PLATEAUCOMPOSITION"]
  
  ppoint <- create_empty_pgeometry("PLATEAUPOINT")
  if(length(ppoints)) {
    ppoint <- internal_union_list(ppoints[!sapply(ppoints, spa_is_empty)], "PLATEAUPOINT")
  }
  pline <- create_empty_pgeometry("PLATEAULINE")
  if(length(plines)) {
    pline <- internal_union_list(plines[!sapply(plines, spa_is_empty)], "PLATEAULINE")
  }
  pregion <- create_empty_pgeometry("PLATEAUREGION")
  if(length(pregions)) {
    pregion <- internal_union_list(pregions[!sapply(pregions, spa_is_empty)], "PLATEAUREGION")
  }
  pcomposition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
  if(length(pcompositions)) {
    pcomposition <- internal_union_list(pcompositions[!sapply(pcompositions, spa_is_empty)], "PLATEAUCOMPOSITION")
  }
  
  pgos <- c(ppoint, pline, pregion, pcomposition)
  new("pcollection", supp = pcol@supp, pgos = pgos)
}

#' Computes the standard union of a list of spatial plateau objects
#' 
#' @noRd
internal_union_list <- function(pgos, type) {
  if(length(pgos) == 0) {
    create_empty_pgeometry(type)
  } else if(length(pgos) == 1) {
    pgos[[1]]
  } else {
    result <- pgos[[1]]
    for(pgo in pgos[2:length(pgos)]){
      result <- spa_union(result, pgo, utype = "max", as_pcomposition = FALSE)
      if(spa_is_empty(result)){
        result <- create_empty_pgeometry(type)
      }
    }
    result
  }
}

#' @title Convert a plateau collection object into a plateau composition object
#'
#' @description `pcollection_to_pcomposition()` converts a plateau collection object into an equivalent plateau composition object.
#'
#' @usage
#'
#' pcollection_to_pcomposition(pcol)
#'
#' @param pcol A `pcollection` object.
#'
#' @details
#'
#' The `pcollection_to_pcomposition()` function yields a `pcomposition` object that is equivalent to the `pcollection` object given as input by
#' aggregating all spatial plateau objects by type.
#'
#' @return
#'
#' A `pcomposition` object.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'
#' @examples
#' # Point components
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' pcp4 <- create_component("MULTIPOINT((10 10), (9 8), (7 7))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (2 3))", 0.4)
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
#' # Creating plateau point objects
#' ppoint1 <- create_pgeometry(list(pcp1, pcp2, pcp3), "PLATEAUPOINT")
#' ppoint2 <- create_pgeometry(list(pcp4, pcp5), "PLATEAUPOINT")
#' ppoint3 <- create_pgeometry(list(pcp4, pcp5), "PLATEAUPOINT")
#' ppoint4 <- create_pgeometry(list(pcp6, pcp7), "PLATEAUPOINT")
#' # Creating plateau line objects
#' pline1 <- create_pgeometry(list(lcp1, lcp3), "PLATEAULINE")
#' pline2 <- create_pgeometry(list(lcp2, lcp4), "PLATEAULINE")
#' pline3 <- create_pgeometry(list(lcp5), "PLATEAULINE")
#' # Creating a plateau region objects
#' pregion <- create_pgeometry(list(rcp1, rcp2), "PLATEAUREGION")
#' # Creating a plateau composition object
#' pcomposition <- create_pgeometry(list(ppoint4, pline3), "PLATEAUCOMPOSITION")
#' # Creating plateau collection objects
#' pcol1 <- create_pgeometry(list(ppoint1, ppoint2, ppoint3, pline1), "PLATEAUCOLLECTION")
#' pcol2 <- create_pgeometry(list(pline2, pregion, pcomposition, pcol1), "PLATEAUCOLLECTION")
#' pcol2
#' plot(pcol2)
#' \dontrun{
#' converted_pcomp <- pcollection_to_pcomposition(pcol2)
#' converted_pcomp
#' plot(converted_pcomp)
#' }
#' @import sf
#' @export
pcollection_to_pcomposition <- function(pcol) {
  # helper function that returns a quadruple of spatial plateau objects from a pcollection
  project <- function(pcol) {
    types <- sapply(pcol@pgos, spa_get_type)
    ppoint <- pcol@pgos[types == "PLATEAUPOINT"]
    if(!length(ppoint)) {
      ppoint <- create_empty_pgeometry("PLATEAUPOINT")
    } else {
      ppoint <- ppoint[[1]]
    }
    pline <- pcol@pgos[types == "PLATEAULINE"]
    if(!length(pline)) {
      pline <- create_empty_pgeometry("PLATEAULINE")
    } else {
      pline <- pline[[1]]
    }
    pregion <- pcol@pgos[types == "PLATEAUREGION"]
    if(!length(pregion)) {
      pregion <- create_empty_pgeometry("PLATEAUREGION")
    } else {
      pregion <- pregion[[1]]
    }
    pcomposition <- pcol@pgos[types == "PLATEAUCOMPOSITION"]
    if(!length(pcomposition)) {
      pcomposition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    } else {
      pcomposition <- pcomposition[[1]]
    }
    list(ppoint = ppoint, pline = pline, pregion = pregion, pcomposition = pcomposition)
  }
  
  # Step (i): removes the hierarchy of the operand object and flattens it.
  pcol <- spa_flatten(pcol)
  
  # Step (ii): we form the fuzzy geometric union of all fuzzy point objects, 
  # fuzzy line objects, and fuzzy region objects respectively that can be found 
  # in the flattened fuzzy spatial collection object.
  quad <- project(pcol)
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
  
  # TODO check if we really to compute the support here since it would be the same support as the support of pcol
  supp <- st_union(st_sfc(ppoint@supp, pline@supp, pregion@supp))
  
  result <- create_empty_pgeometry("PLATEAUCOMPOSITION")
  result@supp <- supp[[1]]
  result@ppoint <- ppoint
  result@pline <- pline
  result@pregion <- pregion
  result
}

#' Handles the result from crisp intersection and crisp difference operations in spatial plateau set operations
#' 
#' @noRd
result_handler <- function(type1, type2, obj, md, comps, check_compatibility = FALSE) {
  # helper function that adds a new component into a given list
  append_comps <- function(obj, md, comps) {
    comp <- new("component", obj = obj, md = md)
    append(comps, comp)
  }
  
  # helper function that makes the union of all objects of a particular data type inside a geometrycollection
  obj_union <- function(obj, geom_type) {
    objs <- suppressWarnings(st_collection_extract(obj, geom_type))
    if(inherits(objs, "sfg")){
      objs
    } else if(inherits(objs, "sfc")){
      st_union(objs)[[1]]
    }
  }
  
  if(!st_is_empty(obj) && md > 0 && md <= 1) {
    geom_type <- st_geometry_type(obj)
    # the compatibility is only checked during the intersection of components in the first phase of a geometric set operation (union and difference)
    # when the types are equal and the resulting object is of same dimension as the spatial plateau objects, it means that we need to add the resulting object in the result
    # otherwise this means that the resulting object is of lower dimension
    # thus, we can don't add it in the list of components since this part of object will be present in a later point (during the second phase of the geometric set operation)
    if(check_compatibility) {
      if(type1 == type2 && type1 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION") && is_compatible(obj, type1)) {
        if(type1 == "PLATEAUPOINT") {
          comps$point <- append_comps(obj, md, comps$point)
        } else if(type1 == "PLATEAULINE") {
          comps$line <- append_comps(obj, md, comps$line)
        } else if(type1 == "PLATEAUREGION") {
          comps$region <- append_comps(obj, md, comps$region)
        }
      }
    } else {
      if(geom_type == "GEOMETRYCOLLECTION") {
        comps$point <- append_comps(obj_union(obj, "POINT"), md, comps$point)
        comps$line <- append_comps(obj_union(obj, "LINESTRING"), md, comps$line)
        comps$region <- append_comps(obj_union(obj, "POLYGON"), md, comps$region)
      } else if(geom_type %in% c("POINT", "MULTIPOINT")) {
        comps$point <- append_comps(obj, md, comps$point)
      } else if(geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
        comps$line <- append_comps(obj, md, comps$line)
      } else if(geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
        comps$region <- append_comps(obj, md, comps$region)
      }
    }
  }
  comps
}

#' Builds a spatial plateau object corresponding to the result of a spatial plateau set operation between homogeneous data types
#' 
#' @noRd
create_result_set_op <- function(comps, as_pcomposition, type) {
  # we get the number of different spatial plateau data types required to compose the result
  non_empty <- sum(sapply(list(comps$point, comps$line, comps$region), length) > 0)
  if(non_empty >= 2) {
    # in this case, only a plateau composition can be built
    result <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    spa_add_internal(result, c(comps$point, comps$line, comps$region))
  } else if(non_empty == 1 && !as_pcomposition) {
    if(length(comps$point)) {
      result <- create_empty_pgeometry("PLATEAUPOINT")
      spa_add_internal(result, comps$point)
    } else if(length(comps$line)) {
      result <- create_empty_pgeometry("PLATEAULINE")
      spa_add_internal(result, comps$line)
    } else if(length(comps$region)) {
      result <- create_empty_pgeometry("PLATEAUREGION")
      spa_add_internal(result, comps$region)
    }
  } else if(non_empty == 1 && as_pcomposition) {
    result <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    if(length(comps$point)) {
      spa_add_internal(result, comps$point)
    } else if(length(comps$line)) {
      spa_add_internal(result, comps$line)
    } else if(length(comps$region)) {
      spa_add_internal(result, comps$region)
    }
  } else if(non_empty == 0 && as_pcomposition) {
    create_empty_pgeometry("PLATEAUCOMPOSITION")
  } else if(non_empty == 0 && !as_pcomposition) {
    create_empty_pgeometry(type)
  }
}

#' Computes spatial plateau set operation when at least of one operands is a plateau composition or plateau collection (i.e., a heterogeneous data type)
#' 
#' @noRd
heterogeneous_geom_comp <- function(pgo1, pgo2, sigma, beta, as_pcomposition) {
  # helper function that tries to simplify the structure of a spatial plateau object (e.g., if a composition has only one sub-object, and it will be simplified to such sub-object)
  simplify <- function(pgo) {
    if(spa_is_empty(pgo)) {
      return(pgo)
    }
    type <- spa_get_type(pgo)
    if(type == "PLATEAUCOMPOSITION") {
      triple <- c(pgo@ppoint, pgo@pline, pgo@pregion)
      mask_empty_objs <- sapply(triple, spa_is_empty)
      n_empty_objs <- sum(mask_empty_objs)
      if(n_empty_objs == 2) {
        if(!mask_empty_objs[1]) {
          pgo@ppoint
        } else if(!mask_empty_objs[2]) {
          pgo@pline
        } else if(!mask_empty_objs[3]) {
          pgo@pregion
        }
      } else {
        pgo
      }
    } else if(type == "PLATEAUCOLLECTION") {
      mask_empty_objs <- sapply(pgo@pgos, spa_is_empty)
      n_non_empty_objs <- length(pgo@pgos) - sum(mask_empty_objs)
      if(n_non_empty_objs == 1) {
        pgo@pgos[!mask_empty_objs][[1]]
      } else {
        pgo
      }
    } else {
      pgo
    }
  }
  
  if(inherits(pgo1, "pcomposition") && inherits(pgo2, "pcomposition")) {
    spgo1 <- simplify(pgo1)
    spgo2 <- simplify(pgo2)
    
    if(inherits(spgo1, c("ppoint", "pline", "pregion")) && inherits(spgo2, c("ppoint", "pline", "pregion"))){
      # if we successfully simplified BOTH objects, then we can simply call the underlying geometric set operation on such simplified objects
      return(sigma(spgo1, spgo2, beta, as_pcomposition = as_pcomposition))
    }
    # otherwise, we have to compute the combination matrix
    cm <- combination_matrix(pgo1, pgo2, sigma, beta, as_pcomposition = TRUE)
    agg_combination_matrix(cm, as_pcomposition)
  } else if(inherits(pgo1, c("ppoint", "pline", "pregion")) && inherits(pgo2, "pcomposition")) {
    # we treat the other object as a pcomposition object and recursively call this function (it will enter in its first case)
    pgo1 <- create_pgeometry(list(pgo1), "PLATEAUCOMPOSITION", is_valid = FALSE)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
  } else if(inherits(pgo1, "pcomposition") && inherits(pgo2, c("ppoint", "pline", "pregion"))) {
    pgo2 <- create_pgeometry(list(pgo2), "PLATEAUCOMPOSITION", is_valid = FALSE)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
  } else if(inherits(pgo1, "pcollection") && inherits(pgo2, "pcollection")) {
    spgo1 <- simplify(pgo1)
    spgo2 <- simplify(pgo2)
    
    if(inherits(spgo1, c("ppoint", "pline", "pregion")) && inherits(spgo2, c("ppoint", "pline", "pregion"))) {
      # if we successfully simplified BOTH objects, then we can simply call the underlying geometric set operation on such simplified objects
      return(sigma(spgo1, spgo2, beta, as_pcomposition = as_pcomposition))
    }
    # otherwise, we have to transform those object as pcomposition objects and recursively call this function
    pgo1 <- pcollection_to_pcomposition(pgo1)
    pgo2 <- pcollection_to_pcomposition(pgo2)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
  } else if(inherits(pgo1, "pcollection") && inherits(pgo2, c("ppoint", "pline", "pregion", "pcomposition"))) {
    pgo1 <- pcollection_to_pcomposition(pgo1)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
  } else if(inherits(pgo1, c("ppoint", "pline", "pregion", "pcomposition")) && inherits(pgo2, "pcollection")) {
    pgo2 <- pcollection_to_pcomposition(pgo2)
    sigma(pgo1, pgo2, beta, as_pcomposition = as_pcomposition)
  }
}

#' Computes the combination matrix of two plateau composition objects
#' 
#' @noRd
combination_matrix <- function(pgo1, pgo2, sigma, beta, as_pcomposition = TRUE) {
  triple1 <- c(pgo1@ppoint, pgo1@pline, pgo1@pregion)
  triple2 <- c(pgo2@ppoint, pgo2@pline, pgo2@pregion)
  rep1 <- rep(triple1, each = 3)
  mapply(sigma, rep1, triple2, beta, as_pcomposition)
}

#' Aggregates the combination matrix and returns a single spatial plateau object (possibly a plateau composition)
#' 
#' @noRd
agg_combination_matrix <- function(cm, as_pcomposition = FALSE) {
  ppoints <- lapply(cm, attr, "ppoint")
  presult <- internal_union_list(ppoints[!sapply(ppoints, spa_is_empty)], "PLATEAUPOINT")
  
  plines <- lapply(cm, attr, "pline")
  lresult <- internal_union_list(plines[!sapply(plines, spa_is_empty)], "PLATEAULINE")
  
  pregions <- lapply(cm, attr, "pregion")
  rresult <- internal_union_list(pregions[!sapply(pregions, spa_is_empty)], "PLATEAUREGION")
  
  mask_empty_objs <- sapply(c(presult, lresult, rresult), spa_is_empty)
  n_empty_objs <- sum(mask_empty_objs)
  if(n_empty_objs == 2 && !as_pcomposition) {
    non_empty_objs <- c(presult, lresult, rresult)[!mask_empty_objs][[1]]
    non_empty_objs
  } else {
    supp <- st_union(st_sfc(presult@supp, lresult@supp, rresult@supp))[[1]]
    agg_cm <- create_empty_pgeometry("PLATEAUCOMPOSITION")
    agg_cm@supp <- supp
    agg_cm@ppoint <- presult
    agg_cm@pline <- lresult
    agg_cm@pregion <- rresult
    agg_cm
  }
}

#' Determines the final spatial plateau data type of a geometric set operation when dealing with homogeneous objects
#' 
#' @noRd
final_data_type <- function(type1, type2) {
  if(type1 == type2) {
    type1
  } else {
    if(any(c(type1, type2) == "PLATEAUPOINT")) {
      "PLATEAUPOINT"
    } else {
      # plateau line and plateau region
      "PLATEAULINE" 
    }
  }
}

#' @title Compute fuzzy geometric set operations
#'
#' @description The spatial plateau set operations _plateau intersection_, _plateau union_, and _plateau difference_ implement 
#' the respective operations _fuzzy geometric intersection_, _fuzzy geometric union_, and _fuzzy geometric difference_. 
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
#' They receive two `pgeometry` objects of the _any type_ as input and yield another `pgeometry` object as output.
#' The family of fuzzy geometric set operations consists of the following functions:
#' 
#' - `spa_intersection()` computes the geometric intersection of two spatial plateau objects. 
#' The membership degree of common points are calculated by using a t-norm operator given by the parameter `itype`. Currently, it can assume `"min"` (default) or `"prod"`.
#' - `spa_union()` computes the geometric union of two spatial plateau objects.
#' The membership degree of common points are calculated by using a t-conorm operator given by the parameter `utype`. Currently, it can assume `"max"` (default).
#' - `spa_difference()` computes the geometric difference of two spatial plateau objects.
#' The membership degree of common points are calculated by using a difference operator given by the parameter `dtype`. 
#' Currently, it can assume `"f_diff"` (default fuzzy difference), `"f_bound_diff"` (fuzzy bounded difference), `"f_symm_diff"` (fuzzy symmetric difference), or `"f_abs_diff"` (fuzzy absolute difference).
#' 
#' Other t-norms, t-conorms, and difference operators can be implemented and given as values for the parameters `itype`, `utype`, and `dtype`, respectively. 
#' For this, the following steps should be performed:
#' 
#' 1. Implement your function that accepts two numeric values in \[0, 1\] as inputs and yields another numeric value in \[0, 1\] as output. Recall that t-norms and t-conorms must have some specific properties according to the fuzzy set theory.
#' 2. Use the name of your function as the character value of the corresponding parameter `itype`, `utype`, or `dtype`.
#' 
#' An example of operator is the source code of `f_bound_diff()`:
#' 
#' `f_bound_diff <- function(x, y) { max(0, (x - y)) }`
#' 
#' The `spa_common_points()` is deprecated. In the past, it computed the common points of two plateau line objects; now, you can use `spa_intersection()`.
#' 
#' @return
#'
#' A `pgeometry` object that is the result of a fuzzy geometric set operation.
#'
#' @references
#' 
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' Underlying concepts and formal definitions of spatial plateau set operations are explained in detail in:
#' 
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' - [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'
#' @examples
#' library(ggplot2)
#' 
#' # Point components
#' pcp1 <- create_component("POINT(0 0)", 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' 
#' pcp4 <- create_component("MULTIPOINT((2 2), (2 4), (3 2))", 1)
#' pcp5 <- create_component("MULTIPOINT((0 0), (2 3))", 0.7)
#' pcp6 <- create_component("MULTIPOINT((0 1), (3 3))", 0.85)
#' pcp7 <- create_component("MULTIPOINT((1 0), (4 2))", 0.4)
#' # Line components
#' lcp1 <- create_component("LINESTRING(0 0, 1 1.5)", 0.2)
#' lcp2 <- create_component("LINESTRING(1 3, 1 2, 2 0.5)", 0.5)
#' lcp3 <- create_component("LINESTRING(2 1.2, 3 1.6, 4 4)", 0.7)
#' 
#' lcp4 <- create_component("LINESTRING(1 1.5, 2 1.2)", 1.0)
#' lcp5 <- create_component("LINESTRING(-1 1, 2 2)", 0.9)
#' # Polygon components
#' rcp1 <- create_component("POLYGON((0 0, 1 4, 2 2, 0 0))", 0.4)
#' rcp2 <- create_component("POLYGON((2 0.5, 4 1, 4 0, 2 0.5))", 0.8)
#' 
#' # Creating plateau point objects
#' ppoint1 <- create_pgeometry(list(pcp1, pcp2, pcp3), "PLATEAUPOINT")
#' ppoint2 <- create_pgeometry(list(pcp4, pcp5, pcp6, pcp7), "PLATEAUPOINT")
#' # Creating plateau line objects
#' pline1 <- create_pgeometry(list(lcp1, lcp2, lcp3), "PLATEAULINE")
#' pline2 <- create_pgeometry(list(lcp4, lcp5), "PLATEAULINE")
#' # Creating a plateau region objects
#' pregion <- create_pgeometry(list(rcp1, rcp2), "PLATEAUREGION")
#' 
#' # Defining a wrapper to combine plots side by side, for convenience
#' combine_plots <- function(plot1, plot2, plot3) {
#'   # setting the same range of coordinates and removing the legend of plot1 and plot2
#'   plot1 <- plot1 + coord_sf(xlim = c(0, 4), ylim = c(0, 4)) + theme(legend.position = "none")
#'   plot2 <- plot2 + coord_sf(xlim = c(0, 4), ylim = c(0, 4)) + theme(legend.position = "none")
#'   plot3 <- plot3 + coord_sf(xlim = c(0, 4), ylim = c(0, 4))
#'   ggplot() +
#'     annotation_custom(ggplotGrob(plot1), xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1) +
#'     annotation_custom(ggplotGrob(plot2), xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1) +
#'     annotation_custom(ggplotGrob(plot3), xmin = 0, xmax = 1, ymin = 0, ymax = 0.5) +
#'     coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
#'     theme_void()
#' }
#' 
#' plot_ppoint1 <- plot(ppoint1) + ggtitle("Plateau point 1")
#' plot_ppoint2 <- plot(ppoint2) + ggtitle("Plateau point 2")
#' plot_pline1 <- plot(pline1) + ggtitle("Plateau line 1")
#' plot_pline2 <- plot(pline2) + ggtitle("Plateau line 2")
#' plot_pregion <- plot(pregion) + ggtitle("Plateau region")
#' 
#' # Computing the intersection
#' ppoints_intersec <- spa_intersection(ppoint1, ppoint2)
#' plot_inter <- plot(ppoints_intersec) + ggtitle("Intersection")
#' combine_plots(plot_ppoint1, plot_ppoint2, plot_inter)
#' 
#' \dontrun{
#' # varying the t-norm 
#' ppoints_intersec <- spa_intersection(ppoint1, ppoint2, itype = "prod")
#' plot_inter <- plot(ppoints_intersec) + ggtitle("Intersection (prod)")
#' combine_plots(plot_ppoint1, plot_ppoint2, plot_inter)
#' 
#' plines_intersec <- spa_intersection(pline1, pline2)
#' plot_inter <- plot(plines_intersec) + ggtitle("Intersection")
#' combine_plots(plot_pline1, plot_pline2, plot_inter)
#' 
#' pregion_pline_intersec <- spa_intersection(pline1, pregion)
#' plot_inter <- plot(pregion_pline_intersec) + ggtitle("Intersection")
#' combine_plots(plot_pline1, plot_pregion, plot_inter)
#' 
#' # Computing the union
#' ppoints_union <- spa_union(ppoint1, ppoint2)
#' plot_union <- plot(ppoints_union) + ggtitle("Union")
#' combine_plots(plot_ppoint1, plot_ppoint2, plot_union)
#' 
#' plines_union <- spa_union(pline1, pline2)
#' plot_union <- plot(plines_union) + ggtitle("Union")
#' combine_plots(plot_pline1, plot_pline2, plot_union)
#' 
#' pregion_pline_union <- spa_union(pline1, pregion)
#' plot_union <- plot(pregion_pline_union) + ggtitle("Union")
#' combine_plots(plot_pline1, plot_pregion, plot_union)
#' 
#' # Computing the difference
#' ppoints_diff <- spa_difference(ppoint1, ppoint2)
#' plot_diff <- plot(ppoints_diff) + ggtitle("Difference")
#' combine_plots(plot_ppoint1, plot_ppoint2, plot_diff)
#' 
#' plines_diff <- spa_difference(pline1, pline2)
#' plot_diff <- plot(plines_diff) + ggtitle("Difference")
#' combine_plots(plot_pline1, plot_pline2, plot_diff)
#' 
#' pregion_pline_diff <- spa_difference(pline1, pregion)
#' plot_diff <- plot(pregion_pline_diff) + ggtitle("Difference")
#' combine_plots(plot_pline1, plot_pregion, plot_diff)
#' }
#' @import sf
#' @export
spa_intersection <- function(pgo1, pgo2, itype = "min", as_pcomposition = FALSE) {
  # TODO add a short-circuit
  
  beta <- match.fun(itype)
  comps <- list(point = list(), line = list(), region = list())
  type1 <- spa_get_type(pgo1)
  type2 <- spa_get_type(pgo2)
  
  if(type1 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION") && type2 %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")) {
    # computing the intersection between homogeneous spatial plateau objects
    for(comp1 in pgo1@component) {
      for(comp2 in pgo2@component) {
        result_md <- beta(comp1@md, comp2@md)
        result_obj <- st_intersection(comp1@obj, comp2@obj)
        comps <- result_handler(type1, type2, result_obj, result_md, comps, FALSE)
      }
    }
    ftype <- final_data_type(type1, type2)
    create_result_set_op(comps, as_pcomposition, ftype)
  } else {
    sigma <- match.fun("spa_intersection")
    heterogeneous_geom_comp(pgo1, pgo2, sigma, beta = itype, as_pcomposition = as_pcomposition)
  }
}

#' @noRd
is_pline_pregion_case <- function(type1, type2) {
  if(type1 == "PLATEAULINE" && type2 == "PLATEAUREGION") {
    TRUE
  } else if(type1 == "PLATEAUREGION" && type2 == "PLATEAULINE") {
    TRUE
  } else {
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
    
    if(!is_pline_pregion_case(type1, type2)){
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
      if(is_pline_pregion_case(type1, type2)){
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
      if(is_pline_pregion_case(type1, type2)){
        sf_diff_2 <- st_difference(obj_comp_p2, pgo1@supp)
      }else{
        sf_diff_2 <- st_difference(obj_comp_p2, supp_intersected)
      }
      comps <- result_handler(type1, type2, sf_diff_2, md_comp_p2, comps, FALSE)
    }
    ftype <- final_data_type(type1, type2)
    create_result_set_op(comps, as_pcomposition, ftype)
  } else {
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
    
    if(!is_pline_pregion_case(type1, type2)) {
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
      if(is_pline_pregion_case(type1, type2)){
        sf_diff_1 <- st_difference(obj_comp_p1, pgo2@supp)
      }else{
        sf_diff_1 <- st_difference(obj_comp_p1, supp_intersected)
      }
      comps <- result_handler(type1, type2, sf_diff_1, md_comp_p1, comps, FALSE)
    }
    ftype <- final_data_type(type1, type2)
    create_result_set_op(comps, as_pcomposition, ftype)
  } else {
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
    stop("Different spatial plateau data types.", call. = FALSE)
  }
  
  result <- spa_intersection(pline1, pline2, itype = itype, as_pcomposition = TRUE)
  
  result@ppoint
}

#' @title Compute fuzzy difference operators
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
#' - `f_diff()`: The standard _fuzzy set difference_ operator defined as the intersection of `x` and the complement of `y`, that is, `min(x, 1 - y)`.
#' - `f_bound_diff()`: The _fuzzy bounded difference_ operator defined as `x` minus `y` with upper bound equal to 0, that is, `max(0, x - y)`.
#' - `f_symm_diff()`: The _fuzzy symmetric difference_ operator defined as the union of the difference of `x` and `y` and the difference of `y` and `x`, that is, `max(f_diff(x, y), f_diff(y, x))`.
#' - `f_abs_diff()`: The _fuzzy absolute difference_ operator defined as the absolute difference of `x` and `y`, that is, `abs(x - y)`.
#' 
#' The name of these functions can be used in the parameter `dtype` of the `spa_difference()` function.
#' 
#' @return
#'
#' A numerical vector.
#'
#' @examples
#' x <- c(0.1, 0.3, 0.6, 0.8)
#' y <- c(0.9, 0.7, 0.4, 0.2)
#' 
#' f_diff(x, y)
#' f_bound_diff(x, y)
#' f_symm_diff(x, y)
#' f_abs_diff(x, y)
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