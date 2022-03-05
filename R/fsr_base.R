#' An S4 Class for representing a component of a spatial plateau object
#'
#' @slot obj An `sfg` object.
#' @slot md The membership degree of the component.
#' 
#' @details 
#' 
#' A `component` object is composed of two attributes. The first one is a crisp spatial
#' object and the second one a membership degree in \]0, 1\] of this  `component`.
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
  } else{
    "The component must be a pair of an sfg object and the membership degree in ]0,1]"
  }
})

#' An S4 Class for representing a spatial plateau object
#'
#' @slot supp An `sfg` object that stores the union of the spatial objects of the components of the spatial plateau object.
#' 
#' @details 
#' 
#' A `pgeometry` object is composed of an `sfg` object that represents the union 
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

#' @noRd
spa_is_valid <- function(object){
  
  if(!fsr_is_empty(object)){
    
    if(is.unsorted(sapply(object@component, attr, "md"))){
      "The list of components is not in ascending order of membership degree."
    } else{
          
      type <- spa_get_type(object)
      pgo <- compute_support(object@component, type)
      supp <- pgo[[2]]
      
      if(!st_equals(object@supp, supp, sparse = FALSE)[1]){
        "The support of the spatial plateau object is not correct."
      } else{
        
        # If all components in the list of components are valid
        if(all(sapply(object@component, validObject))){
          
          crisp_objs <- lapply(object@component, attr, "obj")
  
          disjunction <- st_disjoint(st_sfc(crisp_objs), sparse = FALSE)
          adjacency <- st_touches(st_sfc(crisp_objs), sparse = FALSE)
          
          if(type == "PLATEAULINE"){
            intersection <- st_crosses(st_sfc(crisp_objs), sparse = FALSE)
            topology_matrix <- disjunction[upper.tri(disjunction, diag = FALSE)] |
                               adjacency[upper.tri(adjacency, diag = FALSE)] |
                               intersection[upper.tri(intersection, diag = FALSE)]
          } else{
            topology_matrix <- disjunction[upper.tri(disjunction, diag = FALSE)] |
                               adjacency[upper.tri(adjacency, diag = FALSE)]
          }
          
          # Checking disjunction, intersection (only for spatial plateau line) and adjacency between all crisp objects in the list of components
          if(all(topology_matrix)){
              TRUE
          } else{
            "All crisp spatial objects in the list of components must be disjoint or adjacent from each other."
          }
        }
      }
    }
  } else{
    TRUE
  }
}

#' An S4 Class for representing a plateau point object (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot component A list of components.
#' 
#' @details 
#' 
#' A `ppoint` object is composed of a list of `component` objects and inherits 
#' all attributes of the class `pgeometry` (i.e., the support).
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
  spa_is_valid(object)
})

#' An S4 Class for representing a plateau line object (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot component A list of components.
#' 
#' @details 
#' 
#' A `pline` object is composed of a list of `component` objects and inherits 
#' all attributes of the class `pgeometry` (i.e., the support).
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
  spa_is_valid(object)
})

#' An S4 Class for representing a plateau region object (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot component A list of components.
#' 
#' @details 
#' 
#' A `pregion` object is composed of a list of `component` objects and inherits 
#' all attributes of the class `pgeometry` (i.e., the support).
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
  spa_is_valid(object)
})

#' An S4 Class for representing a plateau composition object (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot ppoint An S4 Class for representing a plateau point object.
#' @slot pline An S4 Class for representing a plateau line object.
#' @slot pregion An S4 Class for representing a plateau region object.
#' 
#' @details 
#' 
#' A `pcomposition` object is composed of a `ppoint` object, `pline` object, `pregion` object and inherits 
#' all attributes of the class `pgeometry` (i.e., the support).
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

setValidity("pcomposition", function(object){
  
  if(!fsr_is_empty(object)){
    
    if(validObject(object@ppoint) && validObject(object@pline) && validObject(object@pregion)){
      
      disjunction <- st_disjoint(st_sfc(object@ppoint@supp, object@pline@supp, object@pregion@supp), sparse = FALSE)
      adjacency <- st_touches(st_sfc(object@ppoint@supp, object@pline@supp, object@pregion@supp), sparse = FALSE)
      
      topology_matrix <- disjunction[upper.tri(disjunction, diag = FALSE)] |
                         adjacency[upper.tri(adjacency, diag = FALSE)]
      
      if(all(topology_matrix)){
        
        supp <- st_union(st_sfc(object@ppoint@supp, object@pline@supp, object@pregion@supp))
        
        if(st_equals(object@supp, supp, sparse = FALSE)[1]){
          # TODO validate the condition (iii) of a spatial plateau composition defined in the paper
          # [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
          TRUE
        } else{
          "The support of the spatial plateau composition is not correct."
        }
        
      } else{
        "Any pair of supports of the spatial plateau objects contained in the triple have to be adjacent or disjoint."
      }
      
    }
    
  } else{
    TRUE
  }
  
})

#' An S4 Class for representing a plateau collection object (subclass of `pgeometry`)
#'
#' @slot supp It is inherited from `pgeometry`.
#' @slot pgos A list of spatial plateau objects.
#' 
#' @details 
#' 
#' A `pcollection` object is composed of a list of spatial plateau objects and inherits 
#' all attributes of the class `pgeometry` (i.e., the support).
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

setValidity("pcollection", function(object){
  
  if(!fsr_is_empty(object)){
  
    if(all(sapply(object@pgos, validObject))){
      
      obj_sf <- list()
      for(pgo in 1:length(object@pgos)){
        object_sf <- object@pgos[[pgo]]@supp
        obj_sf[[pgo]] <- object_sf
      }
      supp <- st_union(st_sfc(obj_sf))
      
      if(st_equals(object@supp, supp, sparse = FALSE)[1]){
        TRUE
      } else{
        "The support of the spatial plateau collection is not correct."
      }
      
    }
  } else{
    TRUE
  }
  
})

#' @import sf
#' @noRd
component_to_text <- function(comp) {
  paste0("(", st_as_text(comp@obj), ", ", comp@md, ")")
}

#' @title The PWKT of a spatial plateau object
#'
#' @description This function gives the Plateau Well-Known Text (PWKT) representation of a `pgeometry` object. 
#'
#' @usage
#'
#' spa_pwkt(pgo)
#'
#' @param pgo A `pgeometry` object of any type.
#' 
#' @name PWKT
#'
#' @details
#'
#' It gives the textual representation for a `pgeometry` object, 
#' combining the Well-Known Text (WKT) representation for crisp vector geometry
#' objects and the formal definitions of the five spatial plateau data types.
#' (i.e. `PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`, `PLATEAUCOMPOSITION`, `PLATEAUCOLLECTION`).
#'
#' @return
#'
#' A character value with the textual representation of a given `pgeometry` object.
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
#' # For a `PLATEAUPOINT` object.
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- component_from_sfg(st_multipoint(pts1), 0.2) 
#' comp2 <- component_from_sfg(st_point(c(1, 5)), 0.8)  
#' 
#' ppoint <- create_pgeometry(list(comp1, comp2), "PLATEAUPOINT")
#' 
#' spa_pwkt(ppoint)
#' 
#' # For a `PLATEAULINE` object.
#' 
#' lpts1 <- rbind(c(0, 0), c(1, 1))
#' lpts2 <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lpts3 <- rbind(c(2, 1), c(1.5, 0.5))
#'
#' comp4 <- component_from_sfg(st_linestring(lpts1), 0.4)
#' comp5 <- component_from_sfg(st_linestring(lpts2), 1)
#' comp6 <- component_from_sfg(st_linestring(lpts3), 0.7)
#'
#' pline <- create_pgeometry(list(comp4, comp5, comp6), "PLATEAULINE")
#' 
#' spa_pwkt(pline)
#'
#' # For a `PLATEAUREGION` object.
#' 
#' p1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' p2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol1 <-st_polygon(list(p1, p2))
#' 
#' comp1 <- component_from_sfg(pol1, 0.2)
#' 
#' pregion <- create_pgeometry(list(comp1), "PLATEAUREGION")
#' 
#' spa_pwkt(pregion)
#' 
#' # For a `PLATEAUCOMPOSITION` object.
#' 
#' ppts <- rbind(c(1, 2), c(3, 2))
#' pcomp <- component_from_sfg(st_multipoint(ppts), 0.2) 
#' 
#' lpts <- rbind(c(0, 0), c(1, 1))
#' lcomp <- component_from_sfg(st_linestring(lpts), 0.4)
#' 
#' rpts1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' rpts2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol <- st_polygon(list(rpts1, rpts2))
#' rcomp <- component_from_sfg(pol, 0.2)
#' 
#' pcomposition <- create_pgeometry(list(pcomp, lcomp, rcomp), "PLATEAUCOMPOSITION")
#' 
#' spa_pwkt(pcomposition)
#' 
#' # For a `PLATEAUCOLLECTION` object.
#' 
#' lpts <- rbind(c(0, 0), c(1, 1))
#' lcomp <- component_from_sfg(st_linestring(lpts), 0.4)
#' pline <- create_pgeometry(list(lcomp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' rpts2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol <- st_polygon(list(rpts1, rpts2))
#' rcomp <- component_from_sfg(pol, 0.2)
#' pregion <- create_pgeometry(list(rcomp), "PLATEAUREGION")
#' 
#' pcomposition <- create_pgeometry(list(pline, pregion), "PLATEAUCOMPOSITION")
#' 
#' pcollection <- create_pgeometry(list(pline, pregion, pcomposition), "PLATEAUCOLLECTION")
#'  
#' spa_pwkt(pcollection)
#'  
#' @export
spa_pwkt <- function(pgo) {
  
  type <- spa_get_type(pgo)

  if(fsr_is_empty(pgo)){
    return(paste0(type, " EMPTY"))
  }

  if(type == "PLATEAUCOMPOSITION"){
    l <- c(spa_pwkt(pgo@ppoint), spa_pwkt(pgo@pline), spa_pwkt(pgo@pregion))
  }else if(type == "PLATEAUCOLLECTION"){
    l <- unlist(lapply(pgo@pgos, spa_pwkt))
  }else{
    l <- unlist(lapply(pgo@component, component_to_text))
  }
  
  l <- paste(type," (", paste(l, collapse = ", "), ")", sep="")
  l
}


#' @name PWKT
#' @param x A `pgeometry` object of any type.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#' @export
format.pgeometry <- function(x, ...) {
  spa_pwkt(x)
}

#' @name PWKT
#' 
#' @param object A `pgeometry` object of any type.
#' @aliases show,pgeometry-method
#' 
#' @import methods
#' @export
setMethod("show", "pgeometry", function(object) {
  print(spa_pwkt(object))
})

#' @name PWKT
#' 
#' @param x A `pgeometry` object of any type.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#' 
#' @aliases as.character,pgeometry-method
#' 
#' @import methods
#' @export
setMethod("as.character", "pgeometry", function(x, ...) {
  spa_pwkt(x)
})


#' @name plot
#' 
#' @param x A `pgeometry` object of any type.
#' @param y Not applicable.
#' 
#' @aliases plot,pgeometry,missing-method
#'   
#' @import methods
#' @export
setMethod("plot", signature(x = "pgeometry", y = "missing"), function(x, y, ...) {
  fsr_plot(x, ...)
})

#' @method as.data.frame pgeometry
#' @name as_tibble.pgeometry
#' @export
as.data.frame.pgeometry <-
  function(x, ...)
  {
    as.data.frame(as_tibble(x))
  }