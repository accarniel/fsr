#' An S4 Class for representing a component of a spatial plateau object
#'
#' @slot obj An `sfg` object.
#' @slot md The membership degree of the component.
#' 
#' @details 
#' 
#' A `component` object is composed of two attributes. The first one is a crisp spatial
#' object and the second one is the membership degree in \]0, 1\] of this  `component`.
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' 
#' @import methods sf
#' @export
setClass("component",
         slots = list(
           obj = "XY",
           md = "numeric"
         )
)

setValidity("component", function(object){
  if(inherits(object@obj, "sfg") && 
     inherits(object@obj, c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")) &&
     object@md > 0 && object@md <= 1){
    TRUE
  } else {
    "The component must be a pair of an sfg object and a membership degree in ]0, 1]. The sfg object must be it should be either a `POINT`, `MULTIPOINT`, `LINESTRING`, `MULTILINESTRING`, `POLYGON` or `MULTIPOLYGON` object."
  }
})

#' An S4 Class for representing spatial plateau data types
#'
#' @slot supp An `sfg` object that stores the union of all spatial objects of the components of the spatial plateau object.
#' 
#' @details 
#' 
#' It is a superclass for representing spatial plateau data types. A `pgeometry` object stores an `sfg` object that represents the union 
#' of all crisp spatial objects of its components (i.e., the support).
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' 
#' @import methods
#' @export
setClass("pgeometry",
         slots = list(
           supp = "XY"
         )
)

#' @import methods sf
#' @noRd
check_validity <- function(object) {
  if(!spa_is_empty(object)) {
    degrees <- sapply(object@component, attr, "md")
    if(anyDuplicated(degrees)) {
      "The membership degrees of the components must be unique (i.e., duplicated degrees are not allowed)."
    } else if(is.unsorted(degrees)) {
      "The membership degrees of the components must be in ascending order."
    } else {
      type <- spa_get_type(object)
      pgo <- compute_support(object@component, type)
      supp <- pgo[[2]]
      
      if(!st_equals(object@supp, supp, sparse = FALSE)[1]) {
        "The support of the spatial plateau object is not correct."
      } else {
        # If all components in the list of components are valid
        if(all(sapply(object@component, validObject))) {
          crisp_objs <- lapply(object@component, attr, "obj")
          
          disjunction <- st_disjoint(st_sfc(crisp_objs), sparse = FALSE)
          adjacency <- st_touches(st_sfc(crisp_objs), sparse = FALSE)
          
          if(type == "PLATEAULINE") {
            intersection <- st_crosses(st_sfc(crisp_objs), sparse = FALSE)
            topology_matrix <- disjunction[upper.tri(disjunction, diag = FALSE)] |
              adjacency[upper.tri(adjacency, diag = FALSE)] |
              intersection[upper.tri(intersection, diag = FALSE)]
          } else {
            topology_matrix <- disjunction[upper.tri(disjunction, diag = FALSE)] |
              adjacency[upper.tri(adjacency, diag = FALSE)]
          }
          
          # Checking disjointedness, intersection (only for spatial plateau line) and adjacency between all crisp objects in the list of components
          if(all(topology_matrix)) {
            TRUE
          } else {
            "All crisp spatial objects in the list of components must be disjoint or adjacent from each other."
          }
        }
      }
    }
  } else {
    TRUE
  }
}

#' An S4 Class for representing plateau points (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot component A list of components.
#' 
#' @details 
#' 
#' A `ppoint` object is composed of a list of `component` objects and inherits 
#' the attribute `supp` from the class `pgeometry` (i.e., the support).
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' 
#' @import methods
#' @export
setClass("ppoint",
         contains = "pgeometry",
         slots = list(
           component = "list"
         )
)

setValidity("ppoint", function(object){
  check_validity(object)
})

#' An S4 Class for representing plateau lines (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot component A list of components.
#' 
#' @details 
#' 
#' A `pline` object is composed of a list of `component` objects and inherits 
#' the attribute `supp` from the class `pgeometry` (i.e., the support).
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' 
#' @import methods
#' @export
setClass("pline",
         contains = "pgeometry",
         slots = list(
           component = "list"
         )
)

setValidity("pline", function(object){
  check_validity(object)
})

#' An S4 Class for representing plateau regions (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot component A list of components.
#' 
#' @details 
#' 
#' A `pregion` object is composed of a list of `component` objects and inherits 
#' the attribute `supp` from the class `pgeometry` (i.e., the support).
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' 
#' @import methods
#' @export
setClass("pregion",
         contains = "pgeometry",
         slots = list(
           component = "list"
         )
)

setValidity("pregion", function(object){
  check_validity(object)
})

#' An S4 Class for representing plateau compositions (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot ppoint A plateau point object.
#' @slot pline A plateau line object.
#' @slot pregion A plateau region object.
#' 
#' @details 
#' 
#' A `pcomposition` object is composed of a `ppoint` object, `pline` object, `pregion` object and inherits 
#' the attribute `supp` from the class `pgeometry` (i.e., the support).
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#' 
#' @import methods
#' @export
setClass("pcomposition",
         contains = "pgeometry",
         slots = list(
           ppoint = "ppoint",
           pline = "pline",
           pregion = "pregion"
         )
)

setValidity("pcomposition", function(object) {
  if(!spa_is_empty(object)) {
    if(validObject(object@ppoint) && validObject(object@pline) && validObject(object@pregion)) {
      disjunction <- st_disjoint(st_sfc(object@ppoint@supp, object@pline@supp, object@pregion@supp), sparse = FALSE)
      adjacency <- st_touches(st_sfc(object@ppoint@supp, object@pline@supp, object@pregion@supp), sparse = FALSE)
      
      topology_matrix <- disjunction[upper.tri(disjunction, diag = FALSE)] |
        adjacency[upper.tri(adjacency, diag = FALSE)]
      
      if(all(topology_matrix)) {
        supp <- st_union(st_sfc(object@ppoint@supp, object@pline@supp, object@pregion@supp))
        
        if(st_equals(object@supp, supp, sparse = FALSE)[1]) {
          # TODO validate the condition (iii) of a spatial plateau composition defined in the paper
          # [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
          TRUE
        } else {
          "The support of the spatial plateau composition is not correct."
        }
        
      } else {
        "Any pair of supports of the spatial plateau objects contained in the triple have to be adjacent or disjoint."
      }
    }
  } else {
    TRUE
  }
})

#' An S4 Class for representing plateau collections (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot pgos A list of spatial plateau objects.
#' 
#' @details 
#' 
#' A `pcollection` object is composed of a list of spatial plateau objects and inherits 
#' the attribute `supp` from the class `pgeometry` (i.e., the support).
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#' 
#' @import methods
#' @export
setClass("pcollection",
         contains = "pgeometry",
         slots = list(
           pgos = "list"
         )
)

setValidity("pcollection", function(object) {
  if(!spa_is_empty(object)){
    if(all(sapply(object@pgos, validObject))) {
      obj_sf <- list()
      for(pgo in 1:length(object@pgos)){
        object_sf <- object@pgos[[pgo]]@supp
        obj_sf[[pgo]] <- object_sf
      }
      supp <- st_union(st_sfc(obj_sf))
      
      if(st_equals(object@supp, supp, sparse = FALSE)[1]) {
        TRUE
      } else {
        "The support of the spatial plateau collection is not correct."
      }
    }
  } else {
    TRUE
  }
})