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
  
  if(type=="POINT"){
    if(inherits(raw_obj, "numeric")){
      obj_component = st_point(raw_obj)
    }
    else if(inherits(raw_obj, "matrix")){
      obj_component = st_multipoint(raw_obj)
    }
  } else if(type=="LINE"){
    if(inherits(raw_obj, "matrix")){
      obj_component = st_linestring(raw_obj)
    }
    else if(inherits(raw_obj, "list")){
      obj_component = st_multilinestring(raw_obj)
    }
  } else if(type=="REGION"){
    if(inherits(raw_obj[[1]], "matrix")){
      obj_component = st_polygon(raw_obj)
    }
    else if(inherits(raw_obj[1], "list")){
      obj_component = st_multipolygon(raw_obj)
    }
  } else {
    stop("Invalid type for the component creation.", call. = FALSE)
  }
  
  new("component", obj = obj_component,md=md)
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
  
  sfg_type = class(st_geometry(sfg))[1]
  
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
#' It can be either `"PLATEAUPOINT"`, `"PLATEAULINE"` or `"PLATEAUREGION"`.
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
#' @import sf methods
#' @export
create_empty_pgeometry <- function(type){
  type = toupper(type)
  if(is_pgeometry(type)){
    supp <- NULL
    if(type == "PLATEAUPOINT"){
      supp <- st_multipoint()
    } else if(type == "PLATEAULINE"){
      supp <- st_multilinestring()
    } else if(type == "PLATEAUREGION"){
      supp = st_multipolygon()
    }
    new("pgeometry", component = list(), supp = supp, type = type)
  } else {
    stop("Invalid data type", call. = FALSE)
  }
  
}

#' @title Creation of a `pgeometry` object with components
#'
#' @description This function creates a `pgeometry` object from a `data.frame` or a list of components.
#'
#' @usage
#' 
#' create_pgeometry(components, type)
#'
#' @param components A list of `component` objects or a `data.frame`. The type of each component must be the same for all components.
#' @param type A character value that indicates the type of the desired `pgeometry` object. 
#' It should be either `"PLATEAUPOINT"`, `"PLATEAULINE"`, or `"PLATEAUREGION"`. It must be compatible with 
#' the components given in `components` parameter.
#'
#' @details
#' 
#' The `create_pgeometry` function creates a `pgeometry` object of a given type. This object is built by using either a 
#' list of `component` objects or a dataframe (or tibble). If a dataframe is given, it must have two columns: the first one is 
#' a `sfc` object and second one indicates the membership degree of each respective object of the `sfc` column.
#' 
#' @return
#' 
#' A `pgeometry` object.
#' 
#' @examples
#'
#' library(sf)
#' # Example 1 - Creating an `PLATEAUPOINT` object.
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
#' plateau_point_pgeom <- create_pgeometry(list(comp1, comp2, comp3), "PLATEAUPOINT")
#' 
#' # Example 2 - Creating an `PLATEAULINE` object.
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
#' 
#' @import sf dplyr
#' @export
create_pgeometry <- function(components, type){
  
  type = toupper(type)
  
  if(is_pgeometry(type)){
    if(inherits(components, "list")){
      md_value = c()
      for(comp in 1:length(components)){
        md = components[[comp]]@md
        obj_comp = components[[comp]]@obj
        
        if(!is_compatible(obj_comp, type)){
          stop("Input Component type error. Please verify if your component type is correct", call. = FALSE)
        }
        md_value[comp] <- md
      }
      order_comps = order(md_value)
      new_components <- components[order_comps]
      
      obj_sf = list()
      for(comp in 1:length(components)){
        
        object_sf = components[[comp]]@obj
        obj_sf[[comp]] <- object_sf
      }
      supp = st_union(st_sfc(obj_sf))
    }
    
    else if(inherits(components, "data.frame") || inherits(components, "tibble")){
      new_df <- arrange(components, components[2])
      new_components = vector("list", nrow(new_df))
      
      for(i in 1:nrow(new_df)){
        new_components[[i]] <- new("component", obj = new_df[i,1][[1]], md = new_df[i, 2])
        obj_comp = new_components[[i]]@obj
        
        if(!is_compatible(obj_comp, type)){
          stop("Input Component type error.Please verify if your component type is correct", call. = FALSE)
        }
      }
      supp = st_union(new_df[,1])
    }
    new("pgeometry", component = new_components, supp = supp[[1]], type = type)
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
#' the `pgeometry` object and two columns in the format `(md, geometry)`.
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
  get_md <- function(comp){
    md <- comp@md
    return(md)
  }
  
  get_obj <- function(comp){
    obj <- comp@obj
    return(obj)
  }
  
  pgo_tibble <- tibble(
    md <- unlist(lapply(x@component, get_md)),
    points <- st_sfc(lapply(x@component, get_obj))
  )
  colnames(pgo_tibble) <- c("md", "geometry")
  return(pgo_tibble)
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

#' @title Visualization of `pgeometry` objects
#'
#' @description This function plots a `pgeometry` object.
#'
#' @usage
#' 
#' fsr_plot(pgo, base_poly = NULL, add_base_poly = TRUE, 
#'            low = "white", high = "black", crs = NA, ...)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param base_poly An `sfg` object of the type `POLYGON` or `MULTIPOLYGON`. It can also be an `sfc` object with only one element of the type `POLYGON` or `MULTIPOLYGON`.
#' @param add_base_poly A Boolean value that indicates whether `base_poly` will added to the visualization.
#' @param low A character value that indicates the color for the lower `md`s limit value (0). Default is `"white"`.
#' @param high A character value that indicates the color for the higher `md`s limit value (1). Default is `"black"`.
#' @param crs A numerical value that denotes the coordinate reference system (i.e., EPSG code) of the visualization. Default is `NA`.
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
                     crs = NA, ...) {
  
  if(!is.null(base_poly) && !inherits(base_poly, c("sfg", "sfc"))) {
    stop("base_poly has to be an sfg object.", call. = FALSE)
  }
  
  if(!is.na(crs) && !is.null(base_poly) && st_crs(crs) != st_crs(base_poly)) {
    stop("The coordinate reference system (CRS) of base_poly is different than the informed CRS.", call. = FALSE)
  }
  
  pgo_tibble <- as_tibble(pgo)
  
  if(!is.null(base_poly) && is.na(crs)) {
    crs <- st_crs(base_poly)
  }
  
  if(!is.na(crs)) {
    st_crs(pgo_tibble$geometry) <- crs
  }
  
  # TODO validate if pgo is empty and base_poly is not empty (then we should only plot base_poly)
  
  # TODO improve the management of CRS in pgeometry objects
  # here, we simply add the CRS into the geometry column
  # thus, the visualization and intersection will be valid

  if(!is.null(base_poly)) {
    # TODO validate if base_poly has the same crs as the geometry column
    # note that base_poly has a crs value only if it is an sfg object
    pgo_tibble$geometry <- st_intersection(pgo_tibble$geometry, base_poly)
  }
  
  if(inherits(pgo_tibble$geometry, "sfc_MULTILINESTRING") ||
     inherits(pgo_tibble$geometry, "sfc_MULTIPOINT") ||
     inherits(pgo_tibble$geometry, "sfc_LINESTRING") ||
     inherits(pgo_tibble$geometry, "sfc_POINT")){
    plot <- ggplot(data = pgo_tibble) +
      geom_sf(aes(color = .data$md, geometry = .data$geometry), ...) +
      scale_colour_gradient(limits = c(0, 1), low = low, high = high)  +
      theme_classic()
  } else {
    # lwd = 0 ; color = NA in order to remove the border of the components in the plot
    plot <-  ggplot(data = pgo_tibble) +
      geom_sf(aes(fill = .data$md, geometry = .data$geometry), ...) +
      scale_fill_gradient(limits = c(0, 1),  low = low, high = high) +
      theme_classic()
  }
  
  if (!is.null(base_poly) && add_base_poly) {
    bl <- NULL
    if(inherits(base_poly, c("sfg"))) {
      if(is.na(crs)) {
        bl <- st_sfc(base_poly)
      } else {
        bl <- st_sfc(base_poly, 
                     crs = crs)
      }
    } else {
      bl <- base_poly
    }
    plot <- plot + geom_sf(data = st_as_sf(bl), color = high, size = 0.5, aes(geometry = .data$x), 
                           fill = "transparent")
  }
  
  plot
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
is_pgeometry <- function(type){
  if(!(type %in% c('PLATEAUPOINT', 'PLATEAULINE', 'PLATEAUREGION'))){
    FALSE
  } else {
    TRUE
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
  if(st_is_empty(pgo@supp) && !length(pgo@component)){
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