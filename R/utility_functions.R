#' @title Creation of a component
#'
#' @description There are two functions that build a component from coordinate pairs or a 
#' single `sfg` object labeled with a membership degree. This component can be added to a spatial plateau object. 
#' A component consists of an `sfg` object and an associated membership degree.
#' A component can be built in two different ways. By using the function `create_component`, the component is formed by 
#' the means of a  numeric vector, list or matrix that represents a pair of coordinates. 
#' By using the function `component_from_sfg`, the component is created from an `sfg`
#' object. 
#'
#' @usage
#'
#' create_component(raw_obj, md, type)
#'
#' @param raw_obj A vector, list or matrix containing the pairs of coordinates  to create the `sfg` object of the component.
#' @param md A numeric value indicating the membership degree of the component. It has to be a value in \eqn{]0, 1]}.
#' @param type A character value that indicates the type of the desired `sfg` object. 
#' It should be either `"POINT"`, `"LINE"`, or `"REGION"`.
#'
#' @name fsr_components
#' 
#' @details
#'
#' These functions create a `component` object, which is a pair of an `sfg` object and a membership degree in \eqn{]0, 1]}.
#'
#' The function `create_component` receives  three parameters: `raw_obj`, `md` and `type`. The use of `raw_obj` is 
#' similar to the parameter of the family of functions of the `sf` package (`st` family) that creates spatial objects 
#' from a numeric vector, matrix or list (e.g., the functions `st_point`, `st_multipoint`, etc.). The spatial data type 
#' (i.e., the type of the `sfg` object) indicated by the parameter `type` represents simple and complex objects.
#' For instance, `"POINT"` may refer to simple or complex point objects (internally, we can create a POINT or MULTIPOINT object). 
#'
#' The `component_from_sfg` builds a `component` object by using the specification of two parameters that directly represents the 
#' pair of an `sfg` object and its corresponding membership degree (i.e.,  `md` value).
#' 
#' @return
#'
#' A `component` object that can be added to a spatial plateau object (i.e., a `pgeometry` object).
#'
#' @examples
#'
#' # Creating two components of the type POINT
#' v1 = rbind(c(1,2), c(3,4))
#' v2 = rbind(c(1,4), c(2,3),c(4,4))
#'
#' md1 = 0.2
#' md2 = 0.1
#'
#' comp1 <- create_component(v1, md1, type="POINT")
#' comp2 <- create_component(v2, md2, type="POINT")
#'
#' # Creating two components of the type LINE
#'
#' md3 = 0.45
#' md4 = 0.32
#'
#' v3 = rbind(c(2,2), c(3,3))
#' v4 = rbind(c(1,1), c(3,2))
#'
#' comp3 <- create_component(v3, md3, type="LINE")
#' comp4 <- create_component(v4, md4, type="LINE")
#'
#' # Creating two components of the type REGION
#'
#' p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' list_pols_1 <- list(p1,p2)
#'
#' p3 <- rbind(c(1,0), c(2,0), c(4,2), c(3,4), c(2,4), c(1,0))
#' p4 <- rbind(c(2,2), c(2,3), c(3,4), c(2,2))
#' list_pols_2 <- list(p3,p4)
#'
#' comp_pol1 <- create_component(list_pols_1, 0.4, "REGION")
#' comp_pol2 <- create_component(list_pols_2, 0.6, "REGION")
#'
#' @import sf methods
#' @export
create_component <- function(raw_obj, md, type){
  
  if(type == "POINT"){
    if(inherits(raw_obj, "numeric")){
      obj_component <- st_point(raw_obj)
    }
    else if(inherits(raw_obj, "matrix")){
      obj_component <- st_multipoint(raw_obj)
    }
  } else if(type == "LINE"){
    if(inherits(raw_obj, "matrix")){
      obj_component <- st_linestring(raw_obj)
    }
    else if(inherits(raw_obj, "list")){
      obj_component <- st_multilinestring(raw_obj)
    }
  } else if(type == "REGION"){
    if(inherits(raw_obj[[1]], "matrix")){
      obj_component <- st_polygon(raw_obj)
    }
    else if(inherits(raw_obj[1], "list")){
      obj_component <- st_multipolygon(raw_obj)
    }
  } else {
    stop("Invalid type for the component creation.", call. = FALSE)
  }
  
  new("component", obj = obj_component, md = md)
}

#' @name fsr_components
#' 
#' @usage
#' component_from_sfg(sfg, md) 
#' 
#' @param sfg An `sfg` object. It should be either `POINT`, `MULTIPOINT`, `LINESTRING`,
#'  `MULTILINESTRING`, `POLYGON` or `MULTIPOLYGON` type. Other types of spatial objects are not allowed.
#'  
#' @examples
#' 
#' # Creating components with an sfg object
#' library(sf)
#' 
#' # POINT
#' md1 <- 0.2
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- component_from_sfg(st_multipoint(pts1), md1) 
#' 
#' # LINE
#' md2 <- 0.1
#' pts2 <- rbind(c(2, 2), c(3, 3))
#' comp2 <- component_from_sfg(st_linestring(pts2), md2) 
#'
#' # REGION
#' md3 <- 0.4
#' matrix_object = matrix(c(1,1,8,1,8,8,1,8,1,1),ncol=2, byrow=TRUE)
#' pts3 = list(matrix_object)
#' comp3 = component_from_sfg(st_polygon(pts3), md3)
#' 
#' @import sf methods
#' @export
component_from_sfg <- function(sfg, md){
  
  possible_geometries = c("sfc_POINT",
                          "sfc_MULTIPOINT",
                          "sfc_LINESTRING",
                          "sfc_MULTILINESTRING",
                          "sfc_POLYGON",
                          "sfc_MULTIPOLYGON")
  
  sfg_type <- class(st_geometry(sfg))[1]
  
  if(sfg_type %in% possible_geometries){
    new("component", obj = sfg, md = md)
  } else {
    stop(paste(sfg_type, "type not allowed to create a component."), call. = FALSE)
  }
}

#' @title Creation of an empty `pgeometry` object
#'
#' @description This function builds an empty `pgeometry` object of a specific type.
#'
#' @usage
#'
#' create_empty_pgeometry(type)
#'
#' @param type A character value indicating the data type of the `pgeometry` object.
#' It can be either `"PLATEAUPOINT"`, `"PLATEAULINE"`, `"PLATEAUREGION"`, `"PLATEAUCOMPOSITION"` or `"PLATEAUCOLLECTION"`.
#'
#' @details
#'
#' The `create_empty_pgeometry` creates a new `pgeometry` object with no components. To add new components to this object, you
#' should use `spa_add_component`. The components added to this object must be of same type of the empty pgeometry object.
#'
#' @return
#'
#' A `pgeometry` object.
#'
#' @examples
#'
#' # Creating an Empty Plateau Point object
#' empty_plateau_point <- create_empty_pgeometry("PLATEAUPOINT")
#'
#' # Creating an Empty Plateau Line object
#' empty_plateau_line <- create_empty_pgeometry("PLATEAULINE")
#'
#' # Creating an Empty Plateau Region object
#' empty_plateau_region <- create_empty_pgeometry("PLATEAUREGION")
#' 
#' # Creating an Empty Plateau Composition object
#' empty_plateau_composition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
#' 
#' # Creating an Empty Plateau Collection object
#' empty_plateau_collection <- create_empty_pgeometry("PLATEAUCOLLECTION")
#'
#' @import sf methods
#' @export
create_empty_pgeometry <- function(type){
  
  type <- toupper(type)
  
  if(is_pgeometry(type)){
    
    supp <- NULL
    
    if(type == "PLATEAUPOINT"){
      supp <- st_multipoint()
      new("ppoint", component = list(), supp = supp)
    } else if(type == "PLATEAULINE"){
      supp <- st_multilinestring()
      new("pline", component = list(), supp = supp)
    } else if(type == "PLATEAUREGION"){
      supp <- st_multipolygon()
      new("pregion", component = list(), supp = supp)
    } else if(type == "PLATEAUCOMPOSITION"){
      supp <- st_geometrycollection()
      new("pcomposition", supp = supp, 
          ppoint = create_empty_pgeometry("PLATEAUPOINT"),
          pline = create_empty_pgeometry("PLATEAULINE"),
          pregion = create_empty_pgeometry("PLATEAUREGION"))
    } else if(type == "PLATEAUCOLLECTION"){
      supp <- st_geometrycollection()
      new("pcollection", supp = supp, pgos = list())
    }
  } else {
    stop("Invalid spatial plateau data type", call. = FALSE)
  }
  
}

#' @title Getting the type of spatial plateau object
#'
#' @description This function gets the type of a spatial plateau object.
#' It can be either `"PLATEAUPOINT"`, `"PLATEAULINE"`, `"PLATEAUREGION"`, `"PLATEAUCOMPOSITION"`, or `"PLATEAUCOLLECTION"`.
#'
#' @usage
#' 
#' spa_get_type(pgo)
#'
#' @param pgo A `pgeometry` object of any type. 
#'
#' @details
#' 
#' The `spa_get_type` function gets the type of the spatial plateau object given as input.
#' For instance, if the `pgo` is a object of the class `ppoint` (subclass of `pgeometry`), the return will be "PLATEAUPOINT".
#' 
#' @return
#' 
#' A type of spatial plateau object.
#' 
#' @examples
#'
#' library(sf)
#' 
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pcomp1 <- component_from_sfg(st_multipoint(pts1), 0.4)
#' pcomp2 <- component_from_sfg(st_multipoint(pts2), 0.3)
#' ppoint <- create_pgeometry(list(pcomp1, pcomp2), "PLATEAUPOINT")
#' 
#' spa_get_type(ppoint)
#' 
#' @export
spa_get_type <- function(pgo){
  type <- toupper(is(pgo)[1])
  type <- paste0("PLATEAU", substr(type, 2, nchar(type)))
  if(is_pgeometry(type)){
    type
  } else{
    stop("Invalid spatial plateau data type", call. = FALSE)
  }
}

#' @noRd
is_list_pgos <- function(x){
  
  types <- lapply(x, function(pgo){paste0("PLATEAU", 
                  substr(toupper(is(pgo)[1]), 2, nchar(toupper(is(pgo)[1]))))})
  all(unlist(lapply(types, is_pgeometry)))
  
}

#' @noRd
is_list_components <- function(x){
  
  types <- lapply(lapply(x, is), function(x) x[[1]])
  all(unlist(types) == "component")
  
}

#' @noRd
get_components <- function(x){
  
  types <- lapply(x, function(pgo){paste0("PLATEAU", 
                  substr(toupper(is(pgo)[1]), 2, nchar(toupper(is(pgo)[1]))))})
  components <- c()

  for(pgo in 1:length(x)){
    
    if(types[[pgo]] == "PLATEAUCOMPOSITION"){
      components[[pgo]] <- c(x[[pgo]]@ppoint@component, x[[pgo]]@pline@component, x[[pgo]]@pregion@component)
    } else if(types[[pgo]] == "PLATEAUCOLLECTION"){
      components[[pgo]] <- get_components(x[[pgo]]@pgos)
    } else{
      components[[pgo]] <- x[[pgo]]@component
    }
  }
  components
}

#' @noRd
compute_support <- function(components, type){
  
  md_value <- c()
  obj_sf <- list()
  
  for(comp in 1:length(components)){
    md <- components[[comp]]@md
    obj_comp <- components[[comp]]@obj
    
    if(!is_compatible(obj_comp, type)){
      stop("Input component type error. Please verify if your component type is correct.", call. = FALSE)
    }
    if(md > 0 && md <= 1){
      md_value[comp] <- md
    } else{
      stop("There is a component with a invalid membership degree.", call. = FALSE)
    }
    
    obj_sf[[comp]] <- obj_comp
  }
  order_comps <- order(md_value)
  new_components <- components[order_comps]
  
  supp <- st_union(st_sfc(obj_sf))
  
  return(list(new_components, supp[[1]]))
  
}

#' @title Creation of a `pgeometry` object with components
#'
#' @description This function creates a `pgeometry` object from a `data.frame`/`tibble`, a list of components, or a list of spatial plateau objects.
#'
#' @usage
#' 
#' create_pgeometry(x, type, is_valid = FALSE)
#'
#' @param x A list of `component` objects, a list of `pgeometry` objects or a `data.frame`/`tibble`. For `PLATEAUPOINT`, `PLATEAULINE` and `PLATEAUREGION`, the type of each component must be the same for all components.
#' @param type A character value that indicates the type of the desired `pgeometry` object. 
#' It should be either `"PLATEAUPOINT"`, `"PLATEAULINE"`, `"PLATEAUREGION"`, `"PLATEAUCOMPOSITION"`, or `"PLATEAUCOLLECTION"`. 
#' It must be compatible with the components given in `x` parameter.
#' @param is_valid A Boolean value to check if the user wants to validate the created spatial plateau object at the end. If `is_valid = TRUE`, it calls `validObject` method.
#'
#' @details
#' 
#' The `create_pgeometry` function creates a `pgeometry` object of a given type. This object is built by using either a 
#' list of `component` objects, a list of `pgeometry` objects or a `data.frame` (or `tibble`). 
#' If a `data.frame` is given, it must have two columns: the first one is a `sfc` object 
#' and second one indicates the membership degree of each respective object of the `sfc` column.
#' 
#' @return
#' 
#' A `pgeometry` object.
#' 
#' @examples
#'
#' library(sf)
#' # Example 1 - Creating an `PLATEAUPOINT` object from a list of components.
#' 
#' # Creating components for the plateau point object
#' v1 <- rbind(c(1,2), c(3,4))
#' v2 <- rbind(c(1,4), c(2,3),c(4,4))
#'
#' md1 <- 0.2
#' md2 <- 0.1
#' md3 <- 0.4
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pts3 <- rbind(c(2, 2), c(3, 3))
#'
#' comp1 <- component_from_sfg(st_multipoint(pts1), md1)
#' comp2 <- component_from_sfg(st_multipoint(pts2), md2)
#' comp3 <- component_from_sfg(st_multipoint(pts3), md3)
#' 
#' # Creating the plateau point object as a pgeometry object with 3 components
#' 
#' plateau_point <- create_pgeometry(list(comp1, comp2, comp3), "PLATEAUPOINT")
#' 
#' # Example 2 - Creating an `PLATEAULINE` object from a list of components.
#' 
#' lpts1 <- rbind(c(0, 0), c(1, 1))
#' lpts2 <- rbind(c(1, 1), c(1.2, 1.9), c(2, 1))
#' lpts3 <- rbind(c(2, 1), c(1.5, 0.5))
#'
#' comp4 <- component_from_sfg(st_linestring(lpts1), 0.4)
#' comp5 <- component_from_sfg(st_linestring(lpts2), 1)
#' comp6 <- component_from_sfg(st_linestring(lpts3), 0.7)
#'
#' plateau_line <- create_pgeometry(list(comp4, comp5, comp6), "PLATEAULINE")
#' 
#' # Example 3 - Creating an `PLATEAUREGION` object from a list of components.
#' 
#' p1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' p2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol1 <-st_polygon(list(p1,p2))
#' 
#' comp1 <- component_from_sfg(pol1, 0.2)
#' 
#' plateau_region <- create_pgeometry(list(comp1), "PLATEAUREGION")
#' 
#' # Example 4 - Creating an `PLATEAUCOMPOSITION` object from a list of components.
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
#' plateau_composition <- create_pgeometry(list(pcomp, lcomp, rcomp), "PLATEAUCOMPOSITION")
#' 
#' # Example 5 - Creating an `PLATEAUCOLLECTION` object from a list of spatial plateau objects.
#' 
#' lpts <- rbind(c(0, 0), c(1, 1))
#' lcomp <- component_from_sfg(st_linestring(lpts), 0.4)
#' plateau_line <- create_pgeometry(list(lcomp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' rpts2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol <- st_polygon(list(rpts1, rpts2))
#' rcomp <- component_from_sfg(pol, 0.2)
#' plateau_region <- create_pgeometry(list(rcomp), "PLATEAUREGION")
#' 
#' plateau_composition <- create_pgeometry(list(plateau_region), "PLATEAUCOMPOSITION")
#' 
#' plateau_collection <- create_pgeometry(list(plateau_line, plateau_composition), "PLATEAUCOLLECTION")
#' 
#' @import sf dplyr
#' @export
create_pgeometry <- function(x, type, is_valid = FALSE){

  # Checking if x is a list
  if(inherits(x, "list")){
    
    # Checking if all elements in the list are components or spatial plateau objects
    if(!is_list_pgos(x) && !is_list_components(x)){
      stop("When x argument is a list, it must be a list of components or a list of spatial plateau objects.", call. = FALSE)
    }
      
    # Type of spatial plateau object that the user wants to create
    type <- toupper(type)
  
    # Validating if the type is a pgeometry object
    if(is_pgeometry(type)){
    
      # If list x is empty, create an empty spatial plateau object
      if(!length(x)){
        create_empty_pgeometry(type)
      } else{
          # If all elements in the list are components
          if(is_list_components(x)){
            # Spatial plateau object is a plateau point, plateau line or plateau region
            if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")){
              # Calculate support and order components according to the membership degree of the components
              pgo <- compute_support(x, type)
              new_components <- pgo[[1]] 
              supp <- pgo[[2]]
              
              if(is_valid){
                # For example, PLATEAUPOINT to ppoint
                type <- paste0("p", substr(tolower(type), 8, nchar(type)))
                new(type, supp = supp, component = new_components)  
              } else {
                spa_obj <- create_empty_pgeometry(type)
                spa_obj@component <- new_components
                spa_obj@supp <- supp
                spa_obj
              }
              
            # Spatial plateau object is a plateau composition
            } else if(type == "PLATEAUCOMPOSITION"){
              
              # Ignoring other possible types of crisp objects
              crisp_objs <- lapply(x, attr, "obj")
              # Aggregating components according to crisp object type
              points <- x[unlist(lapply(crisp_objs, st_is, c("POINT", "MULTIPOINT")))]
              lines <- x[unlist(lapply(crisp_objs, st_is, c("LINESTRING", "MULTILINESTRING")))]
              regions <- x[unlist(lapply(crisp_objs, st_is, c("POLYGON", "MULTIPOLYGON")))]
              
              components <- c(points, lines, regions)
              
              # Calculate support and order components according to the membership degree
              pgo <- compute_support(components, type)
              supp <- pgo[[2]]
              
              if(!length(points)){
                ppoint <- create_empty_pgeometry("PLATEAUPOINT")
              } else{
                ppoint <- create_pgeometry(points, "PLATEAUPOINT")  
              }
              
              if(!length(lines)){
                pline <- create_empty_pgeometry("PLATEAULINE")
              } else{
                pline <- create_pgeometry(lines, "PLATEAULINE")  
              }
              
              if(!length(regions)){
                pregion <- create_empty_pgeometry("PLATEAUREGION")
              } else{
                pregion <- create_pgeometry(regions, "PLATEAUREGION") 
              }
              
              if(is_valid){
                new("pcomposition", supp = supp, ppoint = ppoint, pline = pline, pregion = pregion)
              } else{
                spa_obj <- create_empty_pgeometry(type)
                spa_obj@supp <- supp
                spa_obj@ppoint <- ppoint
                spa_obj@pline <- pline
                spa_obj@pregion <- pregion
                spa_obj
              }
              
            # Spatial plateau object is a plateau collection
            } else if(type == "PLATEAUCOLLECTION"){
              stop("To create a PLATEAUCOLLECTION object, you must provide a list of spatial plateau objects.", call. = FALSE)
            }
          
          # If all elements in the list are spatial plateau objects
          } else if(is_list_pgos(x)){
            
            if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")){
              if(length(x) == 1){
                # The list x at position 1 is the spatial plateau object itself
                x[[1]]
              } else{
                stop("To create a PLATEAUPOINT, PLATEAULINE or PLATEAUREGION object you must pass a list of components or a list with only one spatial plateau object.", call. = FALSE)
              }
              
            } else if (type == "PLATEAUCOMPOSITION"){
              types <- lapply(x, function(pgo){paste0("PLATEAU", 
                       substr(toupper(is(pgo)[1]), 2, nchar(toupper(is(pgo)[1]))))})
              # Checking for spatial plateau objects of the same type in the list
              if(anyDuplicated(types)){
                stop("To create a PLATEAUCOMPOSITION object, you must provide a list of components or a list with different types of spatial plateau objects.", call. = FALSE)
              # Checking if there is no spatial plateau composition or spatial plateau collection in this list
              } else if("PLATEAUCOMPOSITION" %in% types || "PLATEAUCOLLECTION" %in% types){
                if(length(x) == 1){
                  x[[1]]
                } else {
                  stop("A PLATEAUCOMPOSITION object can not contain a PLATEAUCOMPOSITION or a PLATEAUCOLLECTION object.", call. = FALSE)
                }
              # There is only a single spatial plateau point, a single spatial plateau line and a single spatial plateau region
              } else {
                ppoint <- pline <- pregion <- NULL
                components <- c()
                for(pgo in 1:length(x)){
                  # Save components to calculate support
                  components[[pgo]] <- x[[pgo]]@component
                  # Creating the triple
                  if(types[[pgo]] == "PLATEAUPOINT"){
                    ppoint <- create_pgeometry(x[[pgo]]@component, "PLATEAUPOINT")
                  } else if(types[[pgo]] == "PLATEAULINE"){
                    pline <- create_pgeometry(x[[pgo]]@component, "PLATEAULINE")
                  } else if(types[[pgo]] == "PLATEAUREGION"){
                    pregion <- create_pgeometry(x[[pgo]]@component, "PLATEAUREGION")
                  }
                }
                
                # Completing with missing spatial plateau objects
                if(is.null(ppoint)){
                  ppoint <- create_empty_pgeometry("PLATEAUPOINT")
                }
                if(is.null(pline)){
                  pline <- create_empty_pgeometry("PLATEAULINE")
                }
                if(is.null(pregion)){
                  pregion <- create_empty_pgeometry("PLATEAUREGION")
                }
                
                if(!length(unlist(components))){
                  create_empty_pgeometry(type)
                } else {
                  pgo <- compute_support(unlist(components), type)
                  supp <- pgo[[2]]
                  if(is_valid){
                    new("pcomposition", supp = supp, ppoint = ppoint, pline = pline, pregion = pregion) 
                  } else{
                    spa_obj <- create_empty_pgeometry(type)
                    spa_obj@supp <- supp
                    spa_obj@ppoint <- ppoint
                    spa_obj@pline <- pline
                    spa_obj@pregion <-pregion
                    spa_obj
                  }
                }
                
              }
            } else if (type == "PLATEAUCOLLECTION"){
              
              obj_sf <- list()
              for(supp_pgo in 1:length(x)) {
                object_sf <- x[[supp_pgo]]@supp
                obj_sf[[supp_pgo]] <- object_sf
              }
              # Union of the supports of all spatial plateau objects in the spatial plateau collection
              supp <- st_union(st_sfc(obj_sf))
              
              if(is_valid){
                new("pcollection", supp = supp[[1]], pgos = x) 
              } else{
                spa_obj <- create_empty_pgeometry(type)
                spa_obj@supp <- supp[[1]]
                spa_obj@pgos <- x
                spa_obj
              }
            }
          }
          
      }
      
    } else{
      stop("Invalid spatial plateau data type", call. = FALSE)
    }
  # Checking if x is a data frame or a tibble
  } else if(inherits(x, c("data.frame", "tibble"))){
    
    new_components <- vector("list", nrow(x))

    for(i in 1:nrow(x)){
      new_components[[i]] <- new("component", obj = x[[1]][[i]], md = x[[i, 2]])
      obj_comp <- new_components[[i]]@obj
    }

    create_pgeometry(new_components, type, is_valid = is_valid)

  } else{
    stop("The x argument must be of type list, data frame or tibble.", call. = FALSE)
  }
  
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @title Converting a `pgeometry` object into tabular data
#'
#' @description We can convert a `pgeometry` object into tabular data, such as a `tibble` or `data.frame` object, 
#' where the components of the `pgeometry` object compose the rows of the table.
#'
#' @method as_tibble pgeometry
#'
#' @param x A `pgeometry` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @details
#' 
#' This function is an interface for the S3 generic `as_tibble`. 
#' Here, it turns a `pgeometry` object into a tibble, which is a data frame with class `tbl_df`.
#' This allows us to get the internal components of the `pgeometry` object 
#' (i.e., spatial features objects and membership degrees) as a data frame with
#' two separate columns - called `md` (_membership degree_) and `geometry` (an `sfc` object). 
#' 
#' For each component of the `pgeometry` object, `as_tibble` gets the `md` and `geometry` 
#' values and allocates them into a row of the new created tibble, in separated columns.
#' Therefore, each row of this tibble represents a component of the original `pgeometry` object. 
#' 
#' It is also possible to call the S3 method `as.data.frame` to convert a `pgeometry` object into tabular data.
#' 
#' @return
#' 
#' A tibble object of size `n x 2` where `n` is the number of components of 
#' the `pgeometry` object and two columns in the format `(geometry, md)`.
#' 
#' @examples
#' 
#' library(sf)
#' 
#' # Creating components for our plateau point object
#' v1 <- rbind(c(1,2), c(3,4))
#' v2 <- rbind(c(1,4), c(2,3),c(4,4))
#'
#' md1 <- 0.2
#' md2 <- 0.1
#' md3 <- 0.4
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pts3 <- rbind(c(2, 2), c(3, 3))
#'
#' comp1 <- component_from_sfg(st_multipoint(pts1), md1)
#' comp2 <- component_from_sfg(st_multipoint(pts2), md2)
#' comp3 <- component_from_sfg(st_multipoint(pts3), md3)
#' 
#' # Creating the plateau point object as a pgeometry object with 3 components
#' 
#' plateau_point <- create_pgeometry(list(comp1, comp2, comp3), "PLATEAUPOINT")
#' 
#' # Converting the pgeometry object into a tibble object
#' plateau_point_tibble <- as_tibble(plateau_point)
#' 
#' plateau_point_tibble
#' 
#' @import sf tibble
#' @export
as_tibble.pgeometry <- function(x, ...) {
  
  components <- unlist(get_components(list(x)))
  
  md <- sapply(components, attr, "md")
  geometry <- st_sfc(lapply(components, attr, "obj"))
  
  tibble(geometry, md)
  
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
    mid <- floor((low + high)/2)
    if(near(m, components[[mid]]@md)){
      return(c(TRUE,mid))
    } else if(m < components[[mid]]@md){
      high <- mid - 1
    } else{
      low <- mid + 1
    }
  }
  return(c(FALSE, low))
}

#' @title Visualization of `pgeometry` objects
#'
#' @description This function plots a `pgeometry` object.
#'
#' @usage
#' 
#' fsr_plot(pgo, base_poly = NULL, add_base_poly = TRUE, 
#'            low = "white", high = "black", crs = 4326, ...)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param base_poly An `sfg` object of the type `POLYGON` or `MULTIPOLYGON`. It can also be an `sfc` object with only one element of the type `POLYGON` or `MULTIPOLYGON`.
#' @param add_base_poly A Boolean value that indicates whether `base_poly` will added to the visualization.
#' @param low A character value that indicates the color for the lower `md`s limit value (0). Default is `"white"`.
#' @param high A character value that indicates the color for the higher `md`s limit value (1). Default is `"black"`.
#' @param crs A numerical value that denotes the coordinate reference system (i.e., EPSG code) of the visualization. Default is 4326.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Optional parameters. They can be the same as the parameters of `geom_sf` function.
#'
#' @name plot
#'
#' @details
#' 
#' The `fsr_plot` uses a `ggplot` method to construct the plot. It receives a `pgeometry` object (if it is empty, an empty graphics
#'  in obtained). 
#' 
#' The `low` and `high` parameters are the colors for the minimum and maximum limits of the membership degrees. The 
#' default colors are "white" and "black", respectively. Other colors can be given in the same way that colors are informed
#' to visualizations produced by the `ggplot` package.
#' 
#' It is possible to clip the geometric format of the components by using the parameter `base_poly`. The boundaries of this object
#' can also be included in the visualization if the parameter `add_base_poly` is TRUE.
#' 
#' @return
#' 
#' A `ggplot` object.
#' 
#' @examples
#' 
#' library(sf)
#' 
#' ### Example 1
#' 
#' # Creating components for the plateau point object
#' v1 <- rbind(c(1,2), c(3,4))
#' v2 <- rbind(c(1,4), c(2,3),c(4,4))
#'
#' md1 <- 0.2
#' md2 <- 0.1
#' md3 <- 0.4
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pts3 <- rbind(c(2, 2), c(3, 3))
#'
#' comp1 <- component_from_sfg(st_multipoint(pts1), md1)
#' comp2 <- component_from_sfg(st_multipoint(pts2), md2)
#' comp3 <- component_from_sfg(st_multipoint(pts3), md3)
#' 
#' # Creating the plateau point object as a pgeometry object with 3 components
#' 
#' ppoint <- create_pgeometry(list(comp1, comp2, comp3), "PLATEAUPOINT")
#' 
#' fsr_plot(ppoint) # with default colors
#' fsr_plot(ppoint, low="blue",high = "red") # with custom limit colors
#' 
#'# Example 2 - PLATEAULINE PLOT
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
#' fsr_plot(pline) # Default values
#' fsr_plot(pline, low="green", high="blue") # Custom colors ...
#'
#'
#'# Example 3 - PLATEAUREGION PLOT
#'
#' p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' pol1 <-st_polygon(list(p1,p2))
#' p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
#' p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
#' pol2 <- st_polygon(list(p3,p4))
#' pol3 <- st_polygon(list(rbind(c(3,3), c(4,2), c(4,3), c(3,3))))
#' 
#' comp1 <- component_from_sfg(pol1, 0.2)
#' comp2 <- component_from_sfg(pol2, 0.4)
#' comp3 <- component_from_sfg(pol3, 0.7)
#' 
#' pregion <- create_pgeometry(list(comp1, comp2, comp3), "PLATEAUREGION")
#' fsr_plot(pregion)
#' fsr_plot(pregion, low = "blue", high = "red")
#' 
#' @import sf ggplot2
#' @importFrom rlang .data
#' @export
fsr_plot <- function(pgo, base_poly = NULL, add_base_poly = TRUE, low = "white", high = "black", 
                     crs = 4326, ...) {
  
  if(!is.null(base_poly) && !inherits(base_poly, c("sfg", "sfc"))) {
    stop("base_poly has to be an sfg object.", call. = FALSE)
  }
  
  plot <- NULL
  
  if(!is.null(base_poly) && add_base_poly){
    plot <- ggplot() + geom_sf(data = st_as_sf(st_sfc(base_poly, crs = crs)), 
            color = high, size = 0.5, aes(geometry = .data$x), fill = "transparent") + 
            theme_classic()
    if(fsr_is_empty(pgo)){
      return(plot)
    }
  }
  
  pgo_tibble <- as_tibble(pgo)
  
  # TODO improve the management of CRS in pgeometry objects
  # here, we simply add the CRS into the geometry column
  # thus, the visualization and intersection will be valid
  st_crs(pgo_tibble$geometry) <- crs
  
  if(!is.null(base_poly)) {
    # TODO validate if base_poly has the same crs as the geometry column
    # note that base_poly has a crs value only if it is an sfc object
    pgo_tibble$geometry <- st_intersection(pgo_tibble$geometry, base_poly)
  }
  
  points <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTIPOINT", "POINT")))
  lines <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTILINESTRING", "LINESTRING")))
  regions <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTIPOLYGON", "POLYGON")))

  if(nrow(regions) != 0){
    if(!is.null(plot)){
      # lwd = 0 ; color = NA in order to remove the border of the components in the plot
      plot <- plot + geom_sf(data = regions, aes(fill = .data$md, geometry = .data$geometry), ...) + 
        scale_fill_gradient(name = "", limits = c(0, 1), low = low, high = high)
    } else{
      plot <- ggplot() + geom_sf(data = regions, aes(fill = .data$md, geometry = .data$geometry), ...) +
        scale_fill_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    }
  }
  
  if(nrow(lines) != 0){
    if(!is.null(plot)){
      plot <- plot + geom_sf(data = lines, aes(color = .data$md, geometry = .data$geometry), ...) + 
        scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    } else{
      plot <- ggplot() + geom_sf(data = lines, aes(color = .data$md, geometry = .data$geometry), ...) +
              scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
              theme_classic()
    }
  }
  
  if(nrow(points) != 0){
    if(!is.null(plot)){
      plot <- plot + geom_sf(data = points, aes(color = .data$md, geometry = .data$geometry), ...)
      if(nrow(lines) == 0){
        plot <- plot + scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
      }
    } else{
      plot <- ggplot() + geom_sf(data = points, aes(color = .data$md, geometry = .data$geometry), ...) +
        scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    }
  }

  plot
  
}

#' @import sf
#' @noRd
is_compatible <- function(sfg, ptype) {
  ptype <- toupper(ptype)
  ret <- FALSE
  if(class(sfg)[1] == "XY") {
    sfg_type <- class(sfg)[2]
    if((sfg_type == "POINT" || sfg_type == "MULTIPOINT") && 
       ptype == "PLATEAUPOINT") {
        ret <- TRUE
    } else if((sfg_type == "LINESTRING" || sfg_type == "MULTILINESTRING") &&
               ptype == "PLATEAULINE") {
        ret <- TRUE
    } else if((sfg_type == "POLYGON" || sfg_type == "MULTIPOLYGON") &&
              ptype == "PLATEAUREGION") {
        ret <- TRUE
    } else if((sfg_type == "POINT" || sfg_type == "MULTIPOINT" ||
              sfg_type == "LINESTRING" || sfg_type == "MULTILINESTRING" ||
              sfg_type == "POLYGON" || sfg_type == "MULTIPOLYGON") &&
              (ptype == "PLATEAUCOMPOSITION" || ptype == "PLATEAUCOLLECTION")){
        ret <- TRUE
    }
  } else{
    stop("Component is not a sfg data type", call. = FALSE)
  }
  ret
}

#' @noRd
is_pgeometry <- function(type){
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION", "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")){
    TRUE
  } else {
    FALSE
  }
}


#' @title Checking whether a `pgeometry` object is empty
#'
#' @description This function checks whether a `pgeometry` object is empty (i.e., if it does not contain components).
#'
#' @usage
#' 
#' fsr_is_empty(pgo)
#'
#' @param pgo A `pgeometry` object.
#'
#' @details
#' 
#' It checks if a pgeometry object has any component or not. If the number of components of a `pgeometry` object is equal to 0, then 
#' it returns  `TRUE`. Otherwise, it returns `FALSE`. 
#' 
#' @return
#' 
#' A Boolean value that indicates if a `pgeometry` is empty.
#' 
#' @examples
#'
#' # Creating an empty pgeometry object 
#' pgo1 <- create_empty_pgeometry("PLATEAULINE")
#' 
#' # Checking if it is empty
#' fsr_is_empty(pgo1)
#' 
#' # Creating a component to populate the pgeometry object
#' 
#' library(sf)
#' md <- 0.4
#' pts <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' 
#' comp <- component_from_sfg(st_multipoint(pts), md)
#' 
#' # Adding the component to the pgeometry object
#' pgo1 <- spa_add_component(pgo1, comp)
#' 
#' # Checking if it is still empty
#' fsr_is_empty(pgo1) 
#' 
#' @import sf
#' @export
fsr_is_empty <- function(pgo){
  
  if(st_is_empty(pgo@supp)){
    
    type <- toupper(class(pgo)[1])
    type <- paste0("PLATEAU", substr(type, 2, nchar(type)))
    
    if(type == "PLATEAUCOLLECTION"){
       ifelse(!length(pgo@pgos), TRUE, FALSE)
    } else if(type == "PLATEAUCOMPOSITION"){
      ifelse(fsr_is_empty(pgo@ppoint) && fsr_is_empty(pgo@pline) && fsr_is_empty(pgo@pregion), TRUE, FALSE)
    } else{
      ifelse(!length(pgo@component), TRUE, FALSE)
    }
  
  } else {
    FALSE
  }
  
}

#' @noRd
get_counter_ctype <- function(pgo){
  ptype = spa_get_type(pgo)
  type <- switch(ptype,
                 PLATEAUPOINT = "POINT",
                 PLATEAULINE = "LINESTRING",
                 PLATEAUREGION = "POLYGON",
                 PLATEAUCOMPOSITION = "GEOMETRYCOLLECTION",
                 PLATEAUCOLLECTION = "GEOMETRYCOLLECTION")
  type
}
