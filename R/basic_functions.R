#' @title Get the type of a spatial plateau object
#'
#' @description `spa_get_type()` returns the type of a spatial plateau object.
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
#' The `spa_get_type()` function yields the type of a spatial plateau object given as input.
#' For instance, if the `pgo` is a object of the class `ppoint` (subclass of `pgeometry`), it returns `"PLATEAUPOINT"`.
#' 
#' @return
#' 
#' The type of a spatial plateau object as a character object (i.e., a string).
#' 
#' @examples
#' pcomp1 <- create_component("MULTIPOINT(1 2, 3 2)", 0.4)
#' pcomp2 <- create_component("POINT(2 1)", 0.3)
#' ppoint <- create_pgeometry(list(pcomp1, pcomp2), "PLATEAUPOINT")
#' 
#' spa_get_type(ppoint) 
#' 
#' lcomp1 <- create_component("LINESTRING(1 2, 3 3, 3 4)", 1)
#' lcomp2 <- create_component("LINESTRING(0 0, 5 5)", 0.5)
#' pline <- create_pgeometry(list(lcomp1, lcomp2), "PLATEAULINE")
#' 
#' spa_get_type(pline)
#' 
#' pcomposition <- create_pgeometry(list(ppoint, pline), "PLATEAUCOMPOSITION")
#' 
#' spa_get_type(pcomposition)
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

#' @title Return PWKT representation of a spatial plateau object
#'
#' @description These functions give the Plateau Well-Known Text (PWKT) representation of a `pgeometry` object. 
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
#' These functions return the textual representation of a `pgeometry` object, 
#' which combines the Well-Known Text (WKT) representation for crisp vector geometry
#' objects and the formal definitions of spatial plateau data types.
#' (i.e. `PLATEAUPOINT`, `PLATEAULINE`, `PLATEAUREGION`, `PLATEAUCOMPOSITION`, and `PLATEAUCOLLECTION`).
#'
#' @return
#'
#' A character object (i.e., string) with the textual representation of a given `pgeometry` object.
#'
#' @references
#' 
#' The formal definition of PWKT is given in:
#' 
#' - [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' Underlying concepts and formal definitions of spatial plateau data types are explained in detail in:
#' 
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' - [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'
#' @examples
#' pcomp1 <- create_component("MULTIPOINT(1 2, 3 2)", 0.4)
#' pcomp2 <- create_component("POINT(2 1)", 0.3)
#' ppoint <- create_pgeometry(list(pcomp1, pcomp2), "PLATEAUPOINT")
#' 
#' # using spa_pwkt()
#' spa_pwkt(ppoint) 
#' # using show() to display the content of ppoint
#' ppoint
#' # using format with width = 30 (default value)
#' format(ppoint)
#' 
#' lcomp1 <- create_component("LINESTRING(1 2, 3 3, 3 4)", 1)
#' lcomp2 <- create_component("LINESTRING(0 0, 5 5)", 0.5)
#' pline <- create_pgeometry(list(lcomp1, lcomp2), "PLATEAULINE")
#' 
#' spa_pwkt(pline)
#' 
#' rcomp1 <- create_component("POLYGON((40 40, 20 48, 48 35, 40 40))", 0.8)
#' rcomp2 <- create_component("POLYGON((10 0, 40 18, 10 20, 5 18, 10 0))", 0.2)
#' pregion <- create_pgeometry(list(rcomp1, rcomp2), "PLATEAUREGION")
#' 
#' spa_pwkt(pregion)
#' 
#' pcomposition <- create_pgeometry(list(ppoint, pline, pregion), "PLATEAUCOMPOSITION")
#' 
#' spa_pwkt(pcomposition)
#' 
#' pcomp3 <- create_component("POINT(10 15)", 0.3)
#' ppoint2 <- create_pgeometry(list(pcomp3), "PLATEAUPOINT")
#' 
#' pcollection <- create_pgeometry(list(pcomposition, ppoint2), "PLATEAUCOLLECTION")
#' 
#' spa_pwkt(pcollection)
#' @export
spa_pwkt <- function(pgo) {
  type <- spa_get_type(pgo)
  
  if(spa_is_empty(pgo)) {
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

#' @title Convert a `pgeometry` object into tabular data (`data.frame` or `tibble`)
#'
#' @description These functions convert a `pgeometry` object into a tabular format, such as a `tibble` or `data.frame` object, 
#' where the components of the `pgeometry` object compose the rows of the table.
#'
#' @method as_tibble pgeometry
#'
#' @param x A `pgeometry` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @details
#' 
#' These functions are S3 methods for `pgeometry`. 
#' The `as_tibble()` function converts a `pgeometry` object into a `tibble`, which is a data frame with class `tbl_df`.
#' This allows us to get the internal components of the `pgeometry` object 
#' (i.e., spatial features objects and membership degrees) as a data frame with
#' two separate columns: (i) `geometry` (an `sfc` object) and (ii) `md` (_membership degree_). 
#' Therefore, each row of this tibble represents a component of the original `pgeometry` object. 
#' 
#' It is also possible to call the S3 method `as.data.frame()` to convert a `pgeometry` object into a `data.frame` object.
#' 
#' @return
#' 
#' A tabular object (`data.frame` or `tibble`) with the number of rows corresponding to the number of components of 
#' the `pgeometry` object given as input and two columns in the format `(geometry, md)`.
#' 
#' @examples
#' pcomp1 <- create_component("MULTIPOINT(1 2, 3 2)", 0.4)
#' pcomp2 <- create_component("POINT(2 1)", 0.3)
#' pcomp3 <- create_component("MULTIPOINT(5 1, 0 0)", 1)
#' ppoint <- create_pgeometry(list(pcomp1, pcomp2, pcomp3), "PLATEAUPOINT")
#' 
#' # Converting the pgeometry object into a tibble object
#' ppoint_tibble <- as_tibble(ppoint)
#' ppoint_tibble
#' 
#' # Converting it into data.frame
#' ppoint_df <- as.data.frame(ppoint)
#' ppoint_df
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

#' @title Graphically visualize `pgeometry` objects
#'
#' @description The `fsr_plot()` function (and the S4 method `plot()`) plots a `pgeometry` object.
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
#' @param clip A Boolean value that indicates whether the boundaries of the components must be clipped by the `sfg` object `base_poly` (if it is not `null`).
#' @param line_lwd A numeric value that specifies the line width of linear components.
#' @param region_lwd A numeric value that specifies the line width of the boundaries of polygonal components.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Optional parameters. They can be the same as the parameters of `geom_sf()` function from `ggplot2`.
#'
#' @name plot
#'
#' @details
#' 
#' The `fsr_plot()` function uses a `ggplot2` package to built the resulting plot. It receives a `pgeometry` object as input (if it is empty, an empty graphics
#'  in obtained). 
#' 
#' The `low` and `high` parameters are the colors for the minimum and maximum limits of the membership degrees. The 
#' default colors are `"white"` and `"black"`, respectively. Other colors can be given in the same way that colors are informed
#' to visualizations produced by the `ggplot2` package.
#' 
#' It is possible to clip the geometric format of the components by using the parameter `base_poly`. The boundaries of this object
#' can also be included in the visualization if the parameter `add_base_poly` is `TRUE`.
#' 
#' Since the returned value is a `ggplot` object, it can be further be customized (see examples below).
#' 
#' @return
#' 
#' A `ggplot` object.
#' 
#' @references
#' 
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' @examples
#' library(sf)
#' 
#' pts <- rbind(c(0, 2), c(4, 2))
#' # Point components
#' pcp1 <- create_component(st_multipoint(pts), 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' # Line components
#' lcp1 <- create_component("LINESTRING(0 0, 1 1.5)", 0.2)
#' lcp2 <- create_component("LINESTRING(1 3, 1 2, 2 0.5)", 0.5)
#' lcp3 <- create_component("LINESTRING(2 1.2, 3 1.6, 4 4)", 0.7)
#' lcp4 <- create_component("LINESTRING(1 1.5, 2 1.2)", 1.0)
#' # Polygon components
#' rcp1 <- create_component("POLYGON((0 0, 1 4, 2 2, 0 0))", 0.4)
#' rcp2 <- create_component("POLYGON((2 0.5, 4 1, 4 0, 2 0.5))", 0.8)
#' # Creating spatial plateau objects
#' pp <- create_pgeometry(list(pcp1, pcp2, pcp3), "PLATEAUPOINT")
#' pl <- create_pgeometry(list(lcp1, lcp3, lcp4), "PLATEAULINE")
#' pr <- create_pgeometry(list(rcp1, rcp2), "PLATEAUREGION")
#' pcm <- create_pgeometry(list(pcp1, pcp2, lcp1, lcp2, lcp3, rcp2), "PLATEAUCOMPOSITION")
#' pcl <- create_pgeometry(list(pp, pr, pcm), "PLATEAUCOLLECTION")
#' 
#' # Displaying their textual representations
#' pp
#' pl
#' pr
#' pcm
#' pcl
#' 
#' # Plotting them
#' plot(pp)
#' plot(pl)
#' plot(pr)
#' plot(pcm)
#' plot(pcl)
#' \dontrun{
#' # Custom colors
#' fsr_plot(pr, low = "green", high = "blue")
#' 
#' # Changing the line width of line components
#' fsr_plot(pl, line_lwd = 2)
#' 
#' # Changing the line width of boundary lines of region components
#' fsr_plot(pr, region_lwd = 2)
#' 
#' # Changing the line width of boundary lines of region components and its color
#' fsr_plot(pr, region_lwd = 2, color = "blue")
#' 
#' # You can customize the whole visualization using ggplot
#' library(ggplot2)
#' 
#' fsr_plot(pp, size = 5) +   
#'   theme(legend.position = "none") +
#'   theme(text=element_text(size=20, family = "serif", color = "black"),
#'         axis.text=element_text(color="black")) +
#'   scale_x_continuous(breaks = c(0, 1, 2, 3, 4)) +
#'   scale_y_continuous(breaks = c(0, 1, 2, 3, 4))
#' }
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
                               color = high, size = 0.5, aes(geometry = .data$x), fill = "transparent")
    if(spa_is_empty(pgo)){
      plot <- plot + theme_classic()
      return(plot)
    }
  }
  
  points <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTIPOINT", "POINT")))
  lines <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTILINESTRING", "LINESTRING")))
  regions <- subset(pgo_tibble, sapply(pgo_tibble$geometry, st_is, c("MULTIPOLYGON", "POLYGON")))
  
  if(nrow(regions) != 0) {
    if(!is.null(plot)) {
      # lwd = 0 ; color = NA in order to remove the border of the components in the plot
      plot <- plot + geom_sf(data = st_as_sf(regions), aes(fill = .data$md, color = .data$md, geometry = .data$geometry), lwd = region_lwd, ...)
    } else {
      plot <- ggplot() + geom_sf(data = st_as_sf(regions), aes(fill = .data$md, color = .data$md, geometry = .data$geometry), lwd = region_lwd, ...)
    }
  }
  
  if(nrow(lines) != 0) {
    if(!is.null(plot)) {
      plot <- plot + geom_sf(data = st_as_sf(lines), aes(color = .data$md, geometry = .data$geometry), lwd = line_lwd, lineend = "round", ...) 
    } else {
      plot <- ggplot() + geom_sf(data = st_as_sf(lines), aes(color = .data$md, geometry = .data$geometry), lwd = line_lwd, lineend = "round", ...)
    }
  }
  
  if(nrow(points) != 0) {
    if(!is.null(plot)) {
      plot <- plot + geom_sf(data = st_as_sf(points), aes(color = .data$md, geometry = .data$geometry), ...)
    } else {
      plot <- ggplot() + geom_sf(data = st_as_sf(points), aes(color = .data$md, geometry = .data$geometry), ...) 
    }
  }
  
  # deciding how to put scale_*_gradient
  if(nrow(regions) != 0) {
    plot <- plot + scale_fill_gradient(name = "", limits = c(0, 1), low = low, high = high)
  }
  
  plot + scale_colour_gradient(name = "", limits = c(0, 1), low = low, high = high) + 
    theme_classic()
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

#' @title Create a component
#'
#' @description `create_component()` builds an object of class `component`. 
#' A component consists of a crisp spatial object (`sfg` object) labeled with a membership degree in \]0, 1\].
#' It is a flexible function since the crisp spatial object can be provided by using different formats.
#'
#' @usage
#'
#' create_component(obj, md, ...)
#'
#' @param obj A crisp spatial object in a specific format (see details below). 
#' @param md A numeric value indicating the membership degree of the component. It must be a value in \]0, 1\].
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Different parameters that are used to convert a crisp spatial object from a specific representation (see more in details below).
#'
#' @name fsr_components
#' 
#' @details
#'
#' The `create_component()` function creates a `component` object. Internally, it is a pair of an `sfg` object and a membership degree in \]0, 1\].
#'
#' `obj` can be either (see restrictions regarding its data type below):
#' - an `sfg` object.
#' - a character vector containing the WKT representation of a crisp spatial object.
#' - a structure of class `"WKB"` with the WKB or EWKB representation of a crisp spatial object. If the EWKB representation is used, then you have to provide the additional parameter `EWKB = TRUE` in `...`.
#' - a vector, list, or matrix containing coordinate pairs to be used when creating the `sfg` object. 
#' This means that it has a similar behavior to the family of functions `st` of the `sf` package (e.g., `st_point()`, `st_multipoint()`, etc.). 
#' Thus, you have to provide the additional parameter `type` in `...`, which should be either `"POINT"`, `"LINE"`, or `"REGION"`.  
#'
#' It is important to emphasize that the crisp spatial object must be a simple or complex point, line, or region (i.e., polygon) object. 
#' That is, it should be a `POINT`, `MULTIPOINT`, `LINESTRING`, `MULTILINESTRING`, `POLYGON` or `MULTIPOLYGON` object.
#' If other types of crisp spatial objects are given, an error will be thrown.
#'
#' The `component_from_sfg()` function is deprecated.
#' 
#' @return
#'
#' A `component` object that can be added to a spatial plateau object (i.e., a `pgeometry` object).
#'
#' @references
#' 
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' @examples
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
#' comp4 <- create_component("POINT(10 35)", 0.5)
#' comp5 <- create_component("MULTILINESTRING((-29 -27, -36 -31, -45 -33), (-45 -33, -46 -32))", 0.9)
#' comp6 <- create_component("POLYGON((75 29, 77 29, 77 29, 75 29))", 1)
#' 
#' # third way: providing WKB representations
#' wkb = structure(list("0x0101000020e610000000000000000000000000000000000040"), class = "WKB")
#' comp7 <- create_component(wkb, 0.8, EWKB = TRUE)
#' 
#' # fourth way: providing coordinate pairs
#' coords1 = rbind(c(2,2), c(3,3))
#' coords2 = rbind(c(1,1), c(3,2))
#'
#' comp8 <- create_component(coords1, 0.45, type = "LINE")
#' comp9 <- create_component(coords2, 0.32, type = "POINT")
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

#' @title Create an empty `pgeometry` object
#'
#' @description `create_empty_pgeometry()` builds an empty `pgeometry` object of a specific type.
#'
#' @usage
#'
#' create_empty_pgeometry(type)
#'
#' @param type A character value indicating the spatial plateau data type of the `pgeometry` object.
#' It can be either `"PLATEAUPOINT"`, `"PLATEAULINE"`, `"PLATEAUREGION"`, `"PLATEAUCOMPOSITION"` or `"PLATEAUCOLLECTION"`.
#'
#' @details
#'
#' The `create_empty_pgeometry()` function creates a new `pgeometry` object with no components. To add new components to this object, you
#' should use `spa_add_component()`. The components added to this object must be compatible with the type of the empty `pgeometry` object.
#'
#' @return
#'
#' An empty `pgeometry` object.
#'
#' @examples
#' # Creating an empty plateau point object
#' empty_plateau_point <- create_empty_pgeometry("PLATEAUPOINT")
#' empty_plateau_point
#'
#' # Creating an empty plateau line object
#' empty_plateau_line <- create_empty_pgeometry("PLATEAULINE")
#' empty_plateau_line
#'
#' # Creating an empty plateau region object
#' empty_plateau_region <- create_empty_pgeometry("PLATEAUREGION")
#' empty_plateau_region
#' 
#' # Creating an empty plateau composition object
#' empty_plateau_composition <- create_empty_pgeometry("PLATEAUCOMPOSITION")
#' empty_plateau_composition
#' 
#' # Creating an empty plateau collection object
#' empty_plateau_collection <- create_empty_pgeometry("PLATEAUCOLLECTION")
#' empty_plateau_collection
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

#' @title Create a `pgeometry` object with components
#'
#' @description `create_pgeometry()` creates a `pgeometry` object from a `data.frame` or `tibble` object, a list of components, or a list of spatial plateau objects.
#'
#' @usage
#' 
#' create_pgeometry(x, type, is_valid = TRUE)
#'
#' @param x A list of `component` objects, a list of `pgeometry` objects or a `data.frame`/`tibble` object. For `PLATEAUPOINT`, `PLATEAULINE` and `PLATEAUREGION`, the type of each component must be the same for all components.
#' @param type A character value that indicates the type of the desired `pgeometry` object. 
#' It should be either `"PLATEAUPOINT"`, `"PLATEAULINE"`, `"PLATEAUREGION"`, `"PLATEAUCOMPOSITION"`, or `"PLATEAUCOLLECTION"`. 
#' It must be compatible with the components given in `x` parameter.
#' @param is_valid A Boolean value to check whether the user wants to validate the created spatial plateau object at the end. If `is_valid = TRUE`, it calls `validObject()` method.
#'
#' @details
#' 
#' `create_pgeometry()` is a flexible function that creates a `pgeometry` object by using the values given in `x`. 
#' This object is built by using either a list of `component` objects, a list of `pgeometry` objects or a `data.frame` (or `tibble`) object. 
#' If a `data.frame` or `tibble` object is given as input, its columns must have the following format: (i) first column is an `sfc` object, and
#' (ii) the second columns consists of the membership degree of each respective object of the `sfc` column.
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
#' [Carniel, A. C.; Venâncio, P. V. A. B; Schneider, M. fsr: An R package for fuzzy spatial data handling. Transactions in GIS, vol. 27, no. 3, pp. 900-927, 2023.](https://onlinelibrary.wiley.com/doi/10.1111/tgis.13044)
#' 
#' Underlying concepts and formal definitions of spatial plateau data types are explained in detail in:
#' 
#' - [Carniel, A. C.; Schneider, M. Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types. In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2018), pp. 1-8, 2018.](https://ieeexplore.ieee.org/document/8491565)
#' - [Carniel, A. C.; Schneider, M. Spatial Data Types for Heterogeneously Structured Fuzzy Spatial Collections and Compositions. In Proceedings of the 2020 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2020), pp. 1-8, 2020.](https://ieeexplore.ieee.org/document/9177620)
#'
#' @examples
#' library(sf)
#' 
#' # Creating some components
#' pts <- rbind(c(0, 2), c(4, 2))
#' # Point components
#' pcp1 <- create_component(st_multipoint(pts), 0.3)
#' pcp2 <- create_component("MULTIPOINT((2 2), (2 4), (2 0))", 0.5)
#' pcp3 <- create_component("MULTIPOINT((1 1), (3 1), (1 3), (3 3))", 0.9)
#' # Line components
#' lcp1 <- create_component("LINESTRING(0 0, 1 1.5)", 0.2)
#' lcp2 <- create_component("LINESTRING(1 3, 1 2, 2 0.5)", 0.5)
#' lcp3 <- create_component("LINESTRING(2 1.2, 3 1.6, 4 4)", 0.7)
#' lcp4 <- create_component("LINESTRING(1 1.5, 2 1.2)", 1.0)
#' # Polygon components
#' rcp1 <- create_component("POLYGON((0 0, 1 4, 2 2, 0 0))", 0.4)
#' rcp2 <- create_component("POLYGON((2 0.5, 4 1, 4 0, 2 0.5))", 0.8)
#' 
#' # Creating spatial plateau objects from lists of components
#' pp <- create_pgeometry(list(pcp1, pcp2, pcp3), "PLATEAUPOINT")
#' pl <- create_pgeometry(list(lcp1, lcp3, lcp4), "PLATEAULINE")
#' pr <- create_pgeometry(list(rcp1, rcp2), "PLATEAUREGION")
#' pcm <- create_pgeometry(list(pcp1, pcp2, lcp1, lcp2, lcp3, rcp2), "PLATEAUCOMPOSITION")
#' 
#' # Creating a spatial plateau objects from a list of spatial plateau objects
#' pcl <- create_pgeometry(list(pp, pr, pcm), "PLATEAUCOLLECTION")
#' 
#' # Converting pp into a tibble
#' pp
#' tibble_pp <- as_tibble(pp)
#' tibble_pp
#' 
#' # Creating a spatial plateau point from the previous tibble
#' equivalent_pp <- create_pgeometry(tibble_pp, "PLATEAUPOINT")
#' equivalent_pp
#' @import sf dplyr
#' @export
create_pgeometry <- function(x, type, is_valid = TRUE) {
  # some helper functions to check the type of x
  is_list_pgos <- function(x) {
    # we don't use spa_get_type here because it throws an error if an invalid object is found
    types <- lapply(x, function(pgo){paste0("PLATEAU", 
                                            substr(toupper(is(pgo)[1]), 2, nchar(toupper(is(pgo)[1]))))})
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
            # we don't use spa_get_type here because it throws an error if an invalid object is found
            types <- lapply(x, function(pgo){paste0("PLATEAU", 
                                                    substr(toupper(is(pgo)[1]), 2, nchar(toupper(is(pgo)[1]))))})
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

#' @title Check if a `pgeometry` object is empty
#'
#' @description `spa_is_empty()` checks whether a given `pgeometry` object is empty (i.e., if it does not contain components).
#'
#' @usage
#' 
#' spa_is_empty(pgo)
#'
#' @param pgo A `pgeometry` object.
#'
#' @details
#' 
#' The `spa_is_empty()` function checks if a pgeometry object has any component or not. If the number of components of a `pgeometry` object is equal to 0, then 
#' it returns `TRUE`. Otherwise, it returns `FALSE`. 
#' 
#' @return
#' 
#' A Boolean value that indicates if a `pgeometry` is empty.
#' 
#' @examples
#' # Creating an empty plateau line object 
#' pgo1 <- create_empty_pgeometry("PLATEAULINE")
#' 
#' # Checking if it is empty
#' spa_is_empty(pgo1)
#' 
#' # Adding a component to it and checking if it still empty
#' comp <- create_component("LINESTRING(1 1, 2 2, 2 3)", 0.5)
#' pgo1 <- spa_add_component(pgo1, comp)
#' spa_is_empty(pgo1)  
#' @import sf
#' @export
spa_is_empty <- function(pgo) {
  if(st_is_empty(pgo@supp)) {
    type <- spa_get_type(pgo)
    if(type == "PLATEAUCOLLECTION") {
      if(length(pgo@pgos) == 0) {
        TRUE
      } else {
        FALSE
      }
    } else if(type == "PLATEAUCOMPOSITION") {
      if(spa_is_empty(pgo@ppoint) && spa_is_empty(pgo@pline) && spa_is_empty(pgo@pregion)) {
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