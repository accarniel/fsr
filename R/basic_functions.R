#' @title Getting the type of a spatial plateau object
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
#' The `spa_get_type` function gets the type of a spatial plateau object given as input.
#' For instance, if the `pgo` is a object of the class `ppoint` (subclass of `pgeometry`), the return will be "PLATEAUPOINT".
#' 
#' @return
#' 
#' The type of a spatial plateau object as a character object (i.e., a string).
#' 
#' @examples
#' library(sf)
#' 
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' pts2 <- rbind(c(1, 1), c(2, 3), c(2, 1))
#' pcomp1 <- create_component(st_multipoint(pts1), 0.4)
#' pcomp2 <- create_component(st_multipoint(pts2), 0.3)
#' ppoint <- create_pgeometry(list(pcomp1, pcomp2), "PLATEAUPOINT")
#' 
#' spa_get_type(ppoint) 
#' @export
spa_get_type <- function(pgo) {
  type <- toupper(is(pgo)[1])
  type <- paste0("PLATEAU", substr(type, 2, nchar(type)))
  if(is_pgeometry(type)) {
    type
  } else {
    stop("Invalid spatial plateau data type", call. = FALSE)
  }
}

#' Checks if a given string is a valid spatial plateau data type
#'
#' @noRd
is_pgeometry <- function(type) {
  if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION", 
                 "PLATEAUCOMPOSITION", "PLATEAUCOLLECTION")) {
    TRUE
  } else {
    FALSE
  }
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
#' It returns the textual representation for a `pgeometry` object, 
#' which combines the Well-Known Text (WKT) representation for crisp vector geometry
#' objects and the formal definitions of the spatial plateau data types.
#' (i.e. `PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`, `PLATEAUCOMPOSITION`, and `PLATEAUCOLLECTION`).
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
#' library(sf)
#'
#' # For a `PLATEAUPOINT` object.
#' pts1 <- rbind(c(1, 2), c(3, 2))
#' comp1 <- create_component(st_multipoint(pts1), 0.2) 
#' comp2 <- create_component(st_point(c(1, 5)), 0.8)  
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
#' comp4 <- create_component(st_linestring(lpts1), 0.4)
#' comp5 <- create_component(st_linestring(lpts2), 1)
#' comp6 <- create_component(st_linestring(lpts3), 0.7)
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
#' comp1 <- create_component(pol1, 0.2)
#' 
#' pregion <- create_pgeometry(list(comp1), "PLATEAUREGION")
#' 
#' spa_pwkt(pregion)
#' 
#' # For a `PLATEAUCOMPOSITION` object.
#' 
#' ppts <- rbind(c(1, 2), c(3, 2))
#' pcomp <- create_component(st_multipoint(ppts), 0.2) 
#' 
#' lpts <- rbind(c(0, 0), c(1, 1))
#' lcomp <- create_component(st_linestring(lpts), 0.4)
#' 
#' rpts1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' rpts2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol <- st_polygon(list(rpts1, rpts2))
#' rcomp <- create_component(pol, 0.2)
#' 
#' pcomposition <- create_pgeometry(list(pcomp, lcomp, rcomp), "PLATEAUCOMPOSITION")
#' 
#' spa_pwkt(pcomposition)
#' 
#' # For a `PLATEAUCOLLECTION` object.
#' 
#' lpts <- rbind(c(0, 0), c(1, 1))
#' lcomp <- create_component(st_linestring(lpts), 0.4)
#' pline <- create_pgeometry(list(lcomp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' rpts2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol <- st_polygon(list(rpts1, rpts2))
#' rcomp <- create_component(pol, 0.2)
#' pregion <- create_pgeometry(list(rcomp), "PLATEAUREGION")
#' 
#' pcomposition <- create_pgeometry(list(pline, pregion), "PLATEAUCOMPOSITION")
#' 
#' pcollection <- create_pgeometry(list(pline, pregion, pcomposition), "PLATEAUCOLLECTION")
#'  
#' spa_pwkt(pcollection)  
#' @export
spa_pwkt <- function(pgo) {
  type <- spa_get_type(pgo)
  
  if(fsr_is_empty(pgo)) {
    return(paste0(type, " EMPTY"))
  }
  
  component_to_text <- function(comp) {
    paste0("(", st_as_text(comp@obj), ", ", comp@md, ")")
  }
  
  if(type == "PLATEAUCOMPOSITION") {
    pwkt <- c(spa_pwkt(pgo@ppoint), spa_pwkt(pgo@pline), spa_pwkt(pgo@pregion))
  } else if(type == "PLATEAUCOLLECTION") {
    pwkt <- unlist(lapply(pgo@pgos, spa_pwkt))
  } else {
    pwkt <- unlist(lapply(pgo@component, component_to_text))
  }
  
  paste0(type," (", paste0(pwkt, collapse = ", "), ")")
}

#' @name PWKT
#' @param x A `pgeometry` object of any type.
#' @param width An integer value that indicates the number of characters to be printed. If it is 0 `NULL` or `NA`, then it will print everything.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#' 
#' @export
format.pgeometry <- function(x, ..., width = 30) {
  if(is.null(width) || is.na(width)) {
    width <- 0
  }
  pwkt <- spa_pwkt(x)
  if(width > 0 && nchar(pwkt) > width) {
    paste0(substr(pwkt, 1, width - 3), "...")
  } else {
    pwkt
  }
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
  # helper function that returns all components of a pgeometry objects as a single list
  get_components <- function(x) {
    types <- lapply(x, spa_get_type)
    components <- c()
    
    for(pgo in 1:length(x)) {
      if(types[[pgo]] == "PLATEAUCOMPOSITION") {
        components[[pgo]] <- c(x[[pgo]]@ppoint@component, x[[pgo]]@pline@component, x[[pgo]]@pregion@component)
      } else if(types[[pgo]] == "PLATEAUCOLLECTION") {
        components[[pgo]] <- get_components(x[[pgo]]@pgos)
      } else {
        components[[pgo]] <- x[[pgo]]@component
      }
    }
    components
  }
  
  components <- unlist(get_components(list(x)))
  
  md <- sapply(components, attr, "md")
  geometry <- st_sfc(lapply(components, attr, "obj"))
  
  tibble(geometry, md)
}

#' @method as.data.frame pgeometry
#' @name as_tibble.pgeometry
#' @export
as.data.frame.pgeometry <- function(x, ...) {
    as.data.frame(as_tibble(x))
}

#' @title Visualization of `pgeometry` objects
#'
#' @description This function plots a `pgeometry` object.
#'
#' @usage
#' 
#' fsr_plot(pgo, base_poly = NULL, add_base_poly = TRUE, low = "white", high = "black", 
#'          crs = NA, clip = FALSE, line_lwd = 1, region_lwd = 1, ...)
#'
#' @param pgo A `pgeometry` object of any type.
#' @param base_poly An `sfg` object of the type `POLYGON` or `MULTIPOLYGON`. It can also be an `sfc` object with only one element of the type `POLYGON` or `MULTIPOLYGON`.
#' @param add_base_poly A Boolean value that indicates whether `base_poly` will added to the visualization.
#' @param low A character value that indicates the color for the lowest membership degree (i.e., 0). Default is `"white"`.
#' @param high A character value that indicates the color for the highest membership degree (i.e., 1). Default is `"black"`.
#' @param crs A numerical value that denotes the coordinate reference system (i.e., EPSG code) of the visualization. Default is `NA`.
#' @param clip A Boolean value that indicates whether the boundaries of the components should be clipped by the `sfg` object `base_poly` (if it is not `null`).
#' @param line_lwd A numeric value that specifies the line width of linear components.
#' @param region_lwd A numeric value that specifies the line width of the boundaries of polygonal components.
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
#' can also be included in the visualization if the parameter `add_base_poly` is `TRUE`.
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
                     crs = NA, clip = FALSE, line_lwd = 1, region_lwd = 1, ...) {
  
  if(!is.null(base_poly) && !inherits(base_poly, c("sfg", "sfc"))) {
    stop("base_poly has to be an sfg object.", call. = FALSE)
  }
  
  plot <- NULL
  
  pgo_tibble <- as_tibble(pgo)
  
  # TODO improve the management of CRS in pgeometry objects
  # here, we simply add the CRS into the geometry column
  # thus, the visualization and intersection will be valid
  
  if(!is.null(base_poly)) {
    # TODO validate if base_poly has the same crs as the geometry column
    # note that base_poly has a crs value only if it is an sfc object
    if(clip) {
      base_poly_wo_crs <- base_poly
      if(inherits(base_poly, "sfc")) {
        st_crs(base_poly_wo_crs) <- NA
      }
      pgo_tibble$geometry <- st_intersection(pgo_tibble$geometry, base_poly_wo_crs)
    }
    if(inherits(base_poly, "sfc")) {
      st_crs(pgo_tibble$geometry) <- st_crs(base_poly)
    } else {
      if(!is.na(crs) && !is.null(crs)){
        st_crs(pgo_tibble$geometry) <- st_crs(crs)
        base_poly <- st_sfc(base_poly, crs = crs)
      }
    }
  }
  
  if(!is.null(base_poly) && add_base_poly){
    plot <- ggplot() + geom_sf(data = st_as_sf(st_sfc(base_poly)), 
                               color = high, size = 0.5, aes(geometry = .data$x), fill = "transparent") + 
      theme_classic()
    if(fsr_is_empty(pgo)){
      return(plot)
    }
  }
  
  points <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTIPOINT", "POINT")))
  lines <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTILINESTRING", "LINESTRING")))
  regions <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTIPOLYGON", "POLYGON")))
  
  if(nrow(regions) != 0){
    if(!is.null(plot)){
      # lwd = 0 ; color = NA in order to remove the border of the components in the plot
      plot <- plot + geom_sf(data = st_as_sf(regions), aes(fill = .data$md, color = .data$md, geometry = .data$geometry), lwd = line_lwd, ...) + 
        scale_fill_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    } else{
      plot <- ggplot() + geom_sf(data = st_as_sf(regions), aes(fill = .data$md, color = .data$md, geometry = .data$geometry), lwd = line_lwd, ...) +
        scale_fill_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    }
  }
  
  if(nrow(lines) != 0) {
    if(!is.null(plot)) {
      plot <- plot + geom_sf(data = st_as_sf(lines), aes(color = .data$md, geometry = .data$geometry), lwd = region_lwd, lineend = "round", ...) +
        scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    } else {
      plot <- ggplot() + geom_sf(data = st_as_sf(lines), aes(color = .data$md, geometry = .data$geometry), lwd = region_lwd, lineend = "round", ...) +
        scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    }
  }
  
  if(nrow(points) != 0) {
    if(!is.null(plot)){
      plot <- plot + geom_sf(data = st_as_sf(points), aes(color = .data$md, geometry = .data$geometry), ...)
      if(nrow(lines) == 0) {
        plot <- plot + scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) + 
          theme_classic()
      }
    } else {
      plot <- ggplot() + geom_sf(data = st_as_sf(points), aes(color = .data$md, geometry = .data$geometry), ...) +
        scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) +
        theme_classic()
    }
  }
  
  plot
}

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

#' @title Creation of a component
#'
#' @description This function builds an object of class `component`. 
#' A component consists of a crisp spatial object (i.e., `sfg` object) labeled with a membership degree in \eqn{]0, 1]}.
#' It is a flexible function since the crisp spatial object can be provided by using different formats.
#'
#' @usage
#'
#' create_component(obj, md, ...)
#'
#' @param obj A crisp spatial object in a specific format (see details below). 
#' @param md A numeric value indicating the membership degree of the component. It must be a value in \eqn{]0, 1]}.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Different parameters that are used to convert a crisp spatial object from a specific representation (see more in details below).
#'
#' @name fsr_components
#' 
#' @details
#'
#' The function `create_component` creates a `component` object. Internally, it is a pair of an `sfg` object and a membership degree in \eqn{]0, 1]}.
#'
#' For this, `obj` can be either:
#' - an `sfg` object of type, 
#' - a character vector containing the WKT representation of a crisp spatial object, 
#' - a structure of class `"WKB"` with the WKB or EWKB representation of a crisp spatial object (if the EWKB representation is used, then you have to provide the additional parameter `EWKB = TRUE` in `...`),
#' - a vector, list, or matrix containing coordinate pairs to be used when creating the `sfg` object. 
#' This means that in this case it has a similar behavior to the family of functions `st` of the `sf` package (e.g., the functions `st_point`, `st_multipoint`, etc.). 
#' Thus, you have to provide the additional parameter `type` in `...`, which should be either `"POINT"`, `"LINE"`, or `"REGION"`.  
#'
#' It is important to emphasize that the crisp spatial object must be a simple or complex point, line, or region (i.e., polygon) object. 
#' That is, it should be a `POINT`, `MULTIPOINT`, `LINESTRING`, `MULTILINESTRING`, `POLYGON` or `MULTIPOLYGON` object.
#' If other types of crisp spatial objects are given, an error will be thrown.
#'
#' The function `component_from_sfg` is now integrated with the function `create_component`. 
#' Hence, `component_from_sfg` is deprecated.
#' 
#' @return
#'
#' A `component` object that can be added to a spatial plateau object (i.e., a `pgeometry` object).
#'
#' @examples
#'
#' # first way: providing sfg objects
#' library(sf)
#' 
#' pts <- rbind(c(1, 2), c(3, 2))
#' comp1 <- create_component(st_multipoint(pts), 0.2) 
#' 
#' lpts <- rbind(c(2, 2), c(3, 3))
#' comp2 <- create_component(st_linestring(lpts), 0.1) 
#'
#' matrix_obj <- matrix(c(1,1,8,1,8,8,1,8,1,1), ncol = 2, byrow = TRUE)
#' rpts <- list(matrix_obj)
#' comp3 <- create_component(st_polygon(rpts), 0.4)
#' 
#' # second way: providing WKT representations
#' 
#' comp4 <- create_component("POINT(10 35)", 0.5)
#' comp5 <- create_component("MULTILINESTRING((-29 -27,-36 -31,-45 -33), (-45 -33,-46 -32))", 0.9)
#' comp6 <- create_component("POLYGON((75 29, 77 29, 77 29, 75 29))", 1)
#' 
#' # third way: providing WKB representations
#' 
#' wkb = structure(list("0x0101000020e610000000000000000000000000000000000040"), class = "WKB")
#' comp7 <- create_component(wkb, 0.8, EWKB = TRUE)
#' 
#' # fourth way: providing coordinates
#'
#' coords1 = rbind(c(2,2), c(3,3))
#' coords2 = rbind(c(1,1), c(3,2))
#'
#' comp8 <- create_component(coords1, 0.45, type = "LINE")
#' comp9 <- create_component(coords2, 0.32, type = "POINT")
#' 
#' @import sf methods
#' @export
create_component <- function(obj, md, ...) {
  params <- list(...)
  sfg_obj <- NULL
  
  if(inherits(obj, c("sfg"))) {
    # first possibility - obj is an sfg object
    sfg_obj <- obj
  } else if(is.character(obj)) {
    # second possibility: obj is in the WKT representation
    sfg_obj <- st_as_sfc(obj)[[1]]
  } else if(inherits(obj, "WKB")) {
    # third possibility: obj is in the WKB representation
    EWKB <- FALSE
    if(!is.null(params[["EWKB"]])) {
      EWKB <- params$EWKB
    }
    
    sfg_obj <- st_as_sfc(obj, EWKB = EWKB)[[1]]
  } else if (inherits(obj, c("numeric", "matrix", "list"))) {
    # last type of format for obj
    
    if(is.null(params[["type"]])) {
      stop("You should provide the desired type of the crisp spatial object by using the parameter `type`, which can be either `\"POINT\"`, `\"LINE\"`, or `\"REGION\"`.", call. = FALSE)
    }  
    type <- params$type
    
    if(type == "POINT"){
      if(inherits(obj, "numeric")){
        sfg_obj <- st_point(obj)
      }
      else if(inherits(obj, "matrix")){
        sfg_obj <- st_multipoint(obj)
      }
    } else if(type == "LINE"){
      if(inherits(obj, "matrix")){
        sfg_obj <- st_linestring(obj)
      }
      else if(inherits(obj, "list")){
        sfg_obj <- st_multilinestring(obj)
      }
    } else if(type == "REGION"){
      if(inherits(obj[[1]], "matrix")){
        sfg_obj <- st_polygon(obj)
      }
      else if(inherits(obj[1], "list")){
        sfg_obj <- st_multipolygon(obj)
      }
    } else {
      stop("Invalid type for the creation of the crisp spatial object. It must be either `\"POINT\"`, `\"LINE\"`, or `\"REGION\"`.", call. = FALSE)
    }
  } else {
    stop("Invalid type for `obj`. It should be a valid representation of a crisp spatial object.", call. = FALSE)
  }
  
  new("component", obj = sfg_obj, md = md)
}

#' @name fsr_components
#' 
#' @usage
#' component_from_sfg(sfg, md) 
#' 
#' @param sfg An `sfg` object. It should be either of type `POINT`, `MULTIPOINT`, `LINESTRING`,
#'  `MULTILINESTRING`, `POLYGON` or `MULTIPOLYGON`. Other types of spatial objects are not allowed.
#'  
#' @import sf methods
#' @export
component_from_sfg <- function(sfg, md) {
  
  .Deprecated("create_component")
  
  possible_geometries <- c("sfc_POINT",
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
#' # Creating an empty plateau point object
#' empty_plateau_point <- create_empty_pgeometry("PLATEAUPOINT")
#'
#' # Creating an empty plateau line object
#' empty_plateau_line <- create_empty_pgeometry("PLATEAULINE")
#'
#' # Creating an empty plateau region object
#' empty_plateau_region <- create_empty_pgeometry("PLATEAUREGION")
#' 
#' # Creating an empty plateau composition object
#' empty_plateau_composition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
#' 
#' # Creating an empty plateau collection object
#' empty_plateau_collection <- create_empty_pgeometry("PLATEAUCOLLECTION")
#' @import sf methods
#' @export
create_empty_pgeometry <- function(type) {
  type <- toupper(type)
  if(is_pgeometry(type)) {
    supp <- NULL
    if(type == "PLATEAUPOINT") {
      supp <- st_multipoint()
      new("ppoint", component = list(), supp = supp)
    } else if(type == "PLATEAULINE") {
      supp <- st_multilinestring()
      new("pline", component = list(), supp = supp)
    } else if(type == "PLATEAUREGION") {
      supp <- st_multipolygon()
      new("pregion", component = list(), supp = supp)
    } else if(type == "PLATEAUCOMPOSITION") {
      supp <- st_geometrycollection()
      new("pcomposition", supp = supp, 
          ppoint = create_empty_pgeometry("PLATEAUPOINT"),
          pline = create_empty_pgeometry("PLATEAULINE"),
          pregion = create_empty_pgeometry("PLATEAUREGION"))
    } else if(type == "PLATEAUCOLLECTION") {
      supp <- st_geometrycollection()
      new("pcollection", supp = supp, pgos = list())
    }
  } else {
    stop("Invalid spatial plateau data type", call. = FALSE)
  }
}

#' @title Creation of a `pgeometry` object with components
#'
#' @description This function creates a `pgeometry` object from a `data.frame`/`tibble`, a list of components, or a list of spatial plateau objects.
#'
#' @usage
#' 
#' create_pgeometry(x, type, is_valid = TRUE)
#'
#' @param x A list of `component` objects, a list of `pgeometry` objects or a `data.frame`/`tibble`. For `PLATEAUPOINT`, `PLATEAULINE` and `PLATEAUREGION`, the type of each component must be the same for all components.
#' @param type A character value that indicates the type of the desired `pgeometry` object. 
#' It should be either `"PLATEAUPOINT"`, `"PLATEAULINE"`, `"PLATEAUREGION"`, `"PLATEAUCOMPOSITION"`, or `"PLATEAUCOLLECTION"`. 
#' It must be compatible with the components given in `x` parameter.
#' @param is_valid A Boolean value to check if the user wants to validate the created spatial plateau object at the end. If `is_valid = TRUE`, it calls `validObject` method.
#'
#' @details
#' 
#' The function `create_pgeometry` creates a `pgeometry` object. 
#' This object is built by using either a list of `component` objects, a list of `pgeometry` objects or a `data.frame` (or `tibble`). 
#' If a `data.frame` is given, it must have two columns: the first one is a `sfc` object 
#' and second one indicates the membership degree of each respective object of the `sfc` column.
#' 
#' By default, this function checks if the resulting spatial plateau object is valid. 
#' That is, it checks whether all constraints defined by the Spatial Plateau Algebra are satisfied. 
#' For instance, the components of a plateau point, plateau line, or plateau region must be adjacent or disjoint from each other and have to be unique membership degrees.
#' 
#' If you are sure that the component objects provided to this function satisfy all the constraints, then you can use `is_valid = FALSE` to improve the performance of this function.
#' 
#' @return
#' 
#' A `pgeometry` object.
#' 
#' @references
#'
#' [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'  
#' @examples
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
#' comp1 <- create_component(st_multipoint(pts1), md1)
#' comp2 <- create_component(st_multipoint(pts2), md2)
#' comp3 <- create_component(st_multipoint(pts3), md3)
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
#' comp4 <- create_component(st_linestring(lpts1), 0.4)
#' comp5 <- create_component(st_linestring(lpts2), 1)
#' comp6 <- create_component(st_linestring(lpts3), 0.7)
#'
#' plateau_line <- create_pgeometry(list(comp4, comp5, comp6), "PLATEAULINE")
#' 
#' # Example 3 - Creating an `PLATEAUREGION` object from a list of components.
#' 
#' p1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' p2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol1 <-st_polygon(list(p1,p2))
#' 
#' comp1 <- create_component(pol1, 0.2)
#' 
#' plateau_region <- create_pgeometry(list(comp1), "PLATEAUREGION")
#' 
#' # Example 4 - Creating an `PLATEAUCOMPOSITION` object from a list of components.
#' 
#' ppts <- rbind(c(1, 2), c(3, 2))
#' pcomp <- create_component(st_multipoint(ppts), 0.2) 
#' 
#' lpts <- rbind(c(0, 0), c(1, 1))
#' lcomp <- create_component(st_linestring(lpts), 0.4)
#' 
#' rpts1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' rpts2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol <- st_polygon(list(rpts1, rpts2))
#' rcomp <- create_component(pol, 0.2)
#' 
#' plateau_composition <- create_pgeometry(list(pcomp, lcomp, rcomp), "PLATEAUCOMPOSITION")
#' 
#' # Example 5 - Creating an `PLATEAUCOLLECTION` object from a list of spatial plateau objects.
#' 
#' lpts <- rbind(c(0, 0), c(1, 1))
#' lcomp <- create_component(st_linestring(lpts), 0.4)
#' plateau_line <- create_pgeometry(list(lcomp), "PLATEAULINE")
#' 
#' rpts1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#' rpts2 <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#' pol <- st_polygon(list(rpts1, rpts2))
#' rcomp <- create_component(pol, 0.2)
#' plateau_region <- create_pgeometry(list(rcomp), "PLATEAUREGION")
#' 
#' plateau_composition <- create_pgeometry(list(plateau_region), "PLATEAUCOMPOSITION")
#' 
#' plateau_collection <- create_pgeometry(list(plateau_line, plateau_composition), "PLATEAUCOLLECTION") 
#' @import sf dplyr
#' @export
create_pgeometry <- function(x, type, is_valid = TRUE) {
  # some helper functions to check the type of x
  is_list_pgos <- function(x) {
    types <- lapply(x, spa_get_type)
    all(unlist(lapply(types, is_pgeometry)))
  }
  
  is_list_components <- function(x) {
    types <- lapply(lapply(x, is), function(x) x[[1]])
    all(unlist(types) == "component")
  }
  
  # Checking if x is a list
  if(inherits(x, "list")) {
    
    # Checking if all elements in the list are components or spatial plateau objects
    if(!is_list_pgos(x) && !is_list_components(x)) {
      stop("If x argument is a list, it must be a list of components or a list of spatial plateau objects.", call. = FALSE)
    }
    
    # Type of spatial plateau object that the user wants to create
    type <- toupper(type)
    
    # Validating if the type is a pgeometry object
    if(is_pgeometry(type)) {
      
      # If list x is empty, create an empty spatial plateau object
      if(!length(x)) {
        create_empty_pgeometry(type)
      } else {
        # If all elements in the list are components
        if(is_list_components(x)) {
          # Spatial plateau object is a plateau point, plateau line or plateau region
          if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")) {
            # Calculate support and order components according to the membership degree of the components
            pgo <- compute_support(x, type)
            new_components <- pgo[[1]] 
            supp <- pgo[[2]]
            
            if(is_valid) {
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
          } else if(type == "PLATEAUCOMPOSITION") {
            
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
            
            if(!length(points)) {
              ppoint <- create_empty_pgeometry("PLATEAUPOINT")
            } else {
              ppoint <- create_pgeometry(points, "PLATEAUPOINT", is_valid = FALSE)  
            }
            
            if(!length(lines)) {
              pline <- create_empty_pgeometry("PLATEAULINE")
            } else {
              pline <- create_pgeometry(lines, "PLATEAULINE", is_valid = FALSE)  
            }
            
            if(!length(regions)) {
              pregion <- create_empty_pgeometry("PLATEAUREGION")
            } else {
              pregion <- create_pgeometry(regions, "PLATEAUREGION", is_valid = FALSE) 
            }
            
            if(is_valid) {
              new("pcomposition", supp = supp, ppoint = ppoint, pline = pline, pregion = pregion)
            } else {
              spa_obj <- create_empty_pgeometry(type)
              spa_obj@supp <- supp
              spa_obj@ppoint <- ppoint
              spa_obj@pline <- pline
              spa_obj@pregion <- pregion
              spa_obj
            }
            # Spatial plateau object is a plateau collection
          } else if(type == "PLATEAUCOLLECTION") {
            stop("To create a PLATEAUCOLLECTION object, you must provide a list of spatial plateau objects.", call. = FALSE)
          }
          # If all elements in the list are spatial plateau objects
        } else if(is_list_pgos(x)) {
          if(type %in% c("PLATEAUPOINT", "PLATEAULINE", "PLATEAUREGION")) {
            if(length(x) == 1) {
              # The list x at position 1 is the spatial plateau object itself
              x[[1]]
            } else {
              stop("To create a PLATEAUPOINT, PLATEAULINE or PLATEAUREGION object you must pass a list of components or a list with only one spatial plateau object.", call. = FALSE)
            }
          } else if (type == "PLATEAUCOMPOSITION") {
            types <- lapply(x, spa_get_type)
            # Checking for spatial plateau objects of the same type in the list
            if(anyDuplicated(types)) {
              stop("To create a PLATEAUCOMPOSITION object, you must provide a list of components or a list with different types of spatial plateau objects.", call. = FALSE)
              # Checking if there is no spatial plateau composition or spatial plateau collection in this list
            } else if("PLATEAUCOMPOSITION" %in% types || "PLATEAUCOLLECTION" %in% types) {
              if(length(x) == 1) {
                x[[1]]
              } else {
                stop("A PLATEAUCOMPOSITION object can not contain a PLATEAUCOMPOSITION or a PLATEAUCOLLECTION object.", call. = FALSE)
              }
              # There is only a single spatial plateau point, a single spatial plateau line and a single spatial plateau region
            } else {
              ppoint <- pline <- pregion <- NULL
              components <- c()
              for(pgo in 1:length(x)) {
                # Save components to calculate support
                components[[pgo]] <- x[[pgo]]@component
                # Creating the triple
                if(types[[pgo]] == "PLATEAUPOINT") {
                  ppoint <- create_pgeometry(x[[pgo]]@component, "PLATEAUPOINT", is_valid = FALSE)
                } else if(types[[pgo]] == "PLATEAULINE") {
                  pline <- create_pgeometry(x[[pgo]]@component, "PLATEAULINE", is_valid = FALSE)
                } else if(types[[pgo]] == "PLATEAUREGION") {
                  pregion <- create_pgeometry(x[[pgo]]@component, "PLATEAUREGION", is_valid = FALSE)
                }
              }
              
              # Completing with missing spatial plateau objects
              if(is.null(ppoint)) {
                ppoint <- create_empty_pgeometry("PLATEAUPOINT")
              }
              if(is.null(pline)) {
                pline <- create_empty_pgeometry("PLATEAULINE")
              }
              if(is.null(pregion)) {
                pregion <- create_empty_pgeometry("PLATEAUREGION")
              }
              
              if(!length(unlist(components))) {
                create_empty_pgeometry(type)
              } else {
                pgo <- compute_support(unlist(components), type)
                supp <- pgo[[2]]
                if(is_valid) {
                  new("pcomposition", supp = supp, ppoint = ppoint, pline = pline, pregion = pregion) 
                } else {
                  spa_obj <- create_empty_pgeometry(type)
                  spa_obj@supp <- supp
                  spa_obj@ppoint <- ppoint
                  spa_obj@pline <- pline
                  spa_obj@pregion <-pregion
                  spa_obj
                }
              }
            }
          } else if (type == "PLATEAUCOLLECTION") {
            obj_sf <- list()
            for(supp_pgo in 1:length(x)) {
              object_sf <- x[[supp_pgo]]@supp
              obj_sf[[supp_pgo]] <- object_sf
            }
            # Union of the supports of all spatial plateau objects in the spatial plateau collection
            supp <- st_union(st_sfc(obj_sf))
            
            if(is_valid) {
              new("pcollection", supp = supp[[1]], pgos = x) 
            } else {
              spa_obj <- create_empty_pgeometry(type)
              spa_obj@supp <- supp[[1]]
              spa_obj@pgos <- x
              spa_obj
            }
          }
        }
      }
    } else {
      stop("Invalid spatial plateau data type", call. = FALSE)
    }
  } else if(inherits(x, c("data.frame", "tibble"))) {
    # x is a data frame or a tibble
    new_components <- vector("list", nrow(x))
    
    for(i in 1:nrow(x)) {
      new_components[[i]] <- new("component", obj = x[[1]][[i]], md = x[[i, 2]])
    }
    
    create_pgeometry(new_components, type, is_valid = is_valid)
  } else {
    stop("The x argument must be of type list, data frame, or tibble.", call. = FALSE)
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
#' comp <- create_component(st_multipoint(pts), md)
#' 
#' # Adding the component to the pgeometry object
#' pgo1 <- spa_add_component(pgo1, comp)
#' 
#' # Checking if it is still empty
#' fsr_is_empty(pgo1)  
#' @import sf
#' @export
fsr_is_empty <- function(pgo) {
  if(st_is_empty(pgo@supp)) {
    type <- spa_get_type(pgo)
    if(type == "PLATEAUCOLLECTION") {
      if(length(pgo@pgos) == 0) {
        TRUE
      } else {
        FALSE
      }
    } else if(type == "PLATEAUCOMPOSITION") {
      if(fsr_is_empty(pgo@ppoint) && fsr_is_empty(pgo@pline) && fsr_is_empty(pgo@pregion)) {
        TRUE
      } else {
        FALSE
      }
    } else {
      if(length(pgo@component) == 0) {
        TRUE
      } else {
        FALSE
      }
    }
  } else {
    FALSE
  }
}

