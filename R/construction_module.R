#' Fuzzy set policy for the fuzzification stage, as described in the following paper
#'
#' Carniel, A. C.; Schneider, M.
#' A Systematic Approach to Creating Fuzzy Region Objects from Real Spatial Data Sets.
#' In Proceedings of the 2019 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2019), pp. 1-6, 2019.
#' <https://doi.org/10.1109/FUZZ-IEEE.2019.8858878>
#'
#' @param tbl A data.frame or tibble with the following format: (x, y, z).
#' @param classes A character vector containing the class names.
#' @param mfs A vector of membership functions generated from FuzzyR (function `genmf`). Each membership function `i` corresponds to the class `i`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @return a tibble containing `n` new attributes, where `n` corresponds to `length(classes)` (and `length(mfs)`)
#'
#' @examples
#' 
#' library(rlang)
#' library(tibble)
#' library(FuzzyR)
#' library(dplyr)
#' set.seed(7)
#' tbl = tibble(x = runif(10, min= 0, max = 30), y = runif(10, min = 0, max = 50), z = runif(10, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- genmf("trapmf", c(0, 10, 20, 35))
#' hot_mf <- genmf("trimf", c(35, 50, 100))
#' fsp <- fuzzy_set_policy(tbl, classes, mfs = c(cold_mf, hot_mf))
#' fsp
#' 
#' @import tibble dplyr
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @noRd
fuzzy_set_policy <- function(tbl, classes, mfs, ...) {
  if(length(classes) != length(mfs)) {
    stop("The length of classes and mfs have to be equal.", call. = FALSE)
  }

  result <- tibble(x = as.numeric(tbl[[1]]),
                   y = as.numeric(tbl[[2]]),
                   z = as.numeric(tbl[[3]]))

  #adding the new columns
  for(i in 1:length(classes)) {
    result <- result %>% 
      dplyr::mutate(!!classes[i] := as.numeric(evalmf(.data$z, mfs[[i]])))
  }

  result
}

#' Fuzzy clustering policy, based on the C-means, for the fuzzification stage, as described in the following paper
#'
#' Carniel, A. C.; Schneider, M.
#' A Systematic Approach to Creating Fuzzy Region Objects from Real Spatial Data Sets.
#' In Proceedings of the 2019 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2019), pp. 1-6, 2019.
#' <https://doi.org/10.1109/FUZZ-IEEE.2019.8858878>
#'
#' @param tbl A data.frame or tibble with the following format: (x, y, z)
#' @param k A numeric value that refers to the number of groups to be created
#' @param method A fuzzy clustering method of the package `e1071`, which can be either `"cmeans"` (default) or `"cshell"`
#' @param use_coords A Boolean value to indicate whether the columns (x, y) should be used in the clustering algorithm (default is FALSE)
#' @param iter A numeric indicating the number of maximum iterations of the clustering algorithm (default is 100)
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @return a tibble containing `n` new attributes, where `n` corresponds to the `k` groups.
#'
#' @examples
#'
#' library(e1071)
#' set.seed(7)
#' tbl = tibble(x = runif(10, min= 0, max = 30), y = runif(10, min = 0, max = 50), z = runif(10, min = 0, max = 100))
#' fcp <- fuzzy_clustering_policy(tbl, 3)
#' fcp
#'
#' @importFrom e1071 cmeans
#' @importFrom e1071 cshell
#' @importFrom rlang :=
#' @import tibble dplyr
#' @noRd
fuzzy_clustering_policy <- function(tbl, k, method = "cmeans", use_coords = FALSE, iter = 100, ...) {
  #the needed package (in the future, it is better to implement our own fuzzy clustering algorithms)
  if(k <= 1) {
    stop("The value of k should be greater than 1", call. = FALSE)
  }

  method <- tolower(method)

  result <- tibble(x = as.numeric(tbl[[1]]),
                   y = as.numeric(tbl[[2]]),
                   z = as.numeric(tbl[[3]]))
  #the result of the clustering algorithm
  cm <- NULL

  input <- as.matrix(tbl)

  if(method == "cmeans") {
    if(use_coords) {
      cm <- cmeans(input, k, iter)
    } else {
      input2 <- input[, 3]
      cm <- cmeans(input2, k, iter)
    }
  } else if(method == "cshell") {
    if(use_coords) {
      cm <- cshell(input, k, iter)
    } else {
      input2 <- input[, 3]
      cm <- cshell(input2, k, iter)
    }
  } else {
    stop(paste0("The method '", method,"' is not a supported fuzzy clustering algorithm. The options are either 'cmeans' or 'cshell'"), call. = FALSE)
  }

  #adding the new columns, where each column is a group created by the clustering algorithm
  for(i in 1:k){
    col_name <- paste0("group", i)
    result <- result %>% mutate(!!col_name := cm$membership[, i])
  }

  result
}

#' Auxiliary function to process and return a list containing either Voronoi cells or triangles from the Delaunay triangulation
#'
#' @param sf An `sf` object containing point objects. This sf object should be created from the tibble resulted from the fuzzification stage
#' @param op The name of function that should be evaluated: either `st_voronoi` (default) or `st_triangulate`
#' @param base_poly An `sfg` object that will be used to clip the generated polygons (optional argument)
#' @param d_tolerance A numeric value to be used in the parameter `dTolerance`
#'
#' @return an `sfc` object with the generated polygons (voronoi cells or triangles)
#'
#' @examples
#'
#' set.seed(7)
#' tbl = tibble(x = runif(10, min= 0, max = 30), y = runif(10, min = 0, max = 50), z = runif(10, min = 0, max = 100))
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#'
#' voro <- voronoi_delaunay_prep(pts)
#' voro
#'
#' #getting the CH as base_poly
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#' voro2 <- voronoi_delaunay_prep(pts, base_poly = ch)
#' voro2
#'
#' del <- voronoi_delaunay_prep(pts, op = "st_triangulate")
#' del
#'
#' del2 <- voronoi_delaunay_prep(pts, op = "st_triangulate", base_poly = ch)
#' del2
#'
#' @import sf
#' @noRd
voronoi_delaunay_prep <- function(sf, op = "st_voronoi", base_poly = NULL, d_tolerance = 0) {
  # it follows the example in https://r-spatial.github.io/sf/reference/geos_unary.html
  desired_op <- match.fun(op)

  # computing the desired operation provided by the param op
  pols <- st_collection_extract(desired_op(do.call(c, st_geometry(sf)), dTolerance = d_tolerance))

  pols
}

#' @import sf
#' @noRd
clip_op <- function(objs, base_poly) {
  # first, we collect the indices of the objects that are not contained in the base_poly
  # and another list of indices of the objects that are disjoint to base_poly
  not_contained_objs <- unlist(!st_contains(base_poly, objs))
  disjoint_objs <- unlist(st_disjoint(base_poly, objs))
  # we capture those elements that need to be effectively clipped
  to_clip <- not_contained_objs[!(not_contained_objs %in% disjoint_objs)]
  
  # for those disjoint objects, we simply set an empty polygon
  if(length(disjoint_objs) > 0) {
    objs[disjoint_objs] <- st_polygon()
  }
  # for those objects to_clip we perform the intersection
  if(length(to_clip) > 0) {
    objs[to_clip] <- st_intersection(objs[to_clip], base_poly)
  }
  
  objs
}

#' Voronoi diagram policy for the construction stage, as described in the following paper
#'
#' Carniel, A. C.; Schneider, M.
#' A Systematic Approach to Creating Fuzzy Region Objects from Real Spatial Data Sets.
#' In Proceedings of the 2019 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2019), pp. 1-6, 2019.
#' <https://doi.org/10.1109/FUZZ-IEEE.2019.8858878>
#'
#' @param lp A data.frame or tibble with the labeled points in the format: (x, y, z, ...) where ... are attributes added by the fuzzification step
#' @param base_poly An `sfg` object that will be used to clip the generated polygons (optional argument)
#' @param d_tolerance A numeric parameter that is applied to the `st_voronoi`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @return a tibble in the format (class, pgeometry)
#'
#' @examples
#'
#' set.seed(7)
#' tbl = tibble(x = runif(10, min= 0, max = 30), y = runif(10, min = 0, max = 50), z = runif(10, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- genmf("trapmf", c(0, 10, 20, 35))
#' hot_mf <- genmf("trimf", c(35, 50, 100))
#' fsp <- fuzzy_set_policy(tbl, classes, mfs = c(cold_mf, hot_mf))
#'
#' voronoi_diagram_policy(fsp)
#'
#' #getting the CH as base_poly
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#'
#' voronoi_diagram_policy(fsp, base_poly = ch)
#'
#' @import sf tibble
#' @noRd
voronoi_diagram_policy <- function(lp, base_poly = NULL, d_tolerance = 0, ...) {
  pts <- st_as_sf(lp, coords = c(1, 2))

  cls <- colnames(lp)[-c(1:3)]
  pgo <- vector("list")

  cells <- voronoi_delaunay_prep(pts, base_poly = base_poly, d_tolerance = d_tolerance)
  pts$cells <- cells[unlist(st_intersects(pts, cells))]
  
  # lets make a clipping to our base_poly, if it is provided
  if(!is.null(base_poly) && any(class(base_poly) %in% c("POLYGON", "MULTIPOLYGON"))) {
    # note that empty objects are not considered in the creation of a pgeometry object
    pts$cells <- clip_op(pts$cells, base_poly)
  }

  #producing the result: we have a plateau spatial object for each class
  for(class in cls){
    # we create list of components for each class
    lcomps <- apply(pts[, c(class, "cells")], MARGIN = 1, FUN = function(x) new("component", obj = x[[2]], md = x[[1]]))

    pgo <- append(pgo, spa_add_component(create_empty_pgeometry("PLATEAUREGION"), lcomps))
  }

  tibble(class = cls, pgeometry = pgo)
}

#' Delaunay triangulation policy for the construction stage, as described in the following paper
#'
#'  Carniel, A. C.; Schneider, M.
#' A Systematic Approach to Creating Fuzzy Region Objects from Real Spatial Data Sets.
#' In Proceedings of the 2019 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2019), pp. 1-6, 2019.
#' <https://doi.org/10.1109/FUZZ-IEEE.2019.8858878>
#'
#' @param lp A data.frame or tibble with the labeled points in the format: (x, y, z, ...) where ... are attributes added by the fuzzification step
#' @param tnorm A t-norm used to calculate the membership degree of the triangle. It should be the name of a vector function (e.g., "prod", "min").
#' @param base_poly An `sfg` object that will be used to clip the generated polygons (optional argument)
#' @param d_tolerance A numeric parameter that is applied to the `st_triangulate`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @return a tibble in the format (class, pgeometry)
#'
#' @details Note that it is possible to use its own t-norms. A t-norm should has the following signature:
#' FUN(x) where x is a numeric vector. Such a function should return a single numeric value.
#'
#' @examples
#'
#' set.seed(7)
#' tbl = tibble(x = runif(10, min= 0, max = 30), y = runif(10, min = 0, max = 50), z = runif(10, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- genmf("trapmf", c(0, 10, 20, 35))
#' hot_mf <- genmf("trimf", c(35, 50, 100))
#' fsp <- fuzzy_set_policy(tbl, classes, mfs = c(cold_mf, hot_mf))
#'
#' delaunay_triangulation_policy(fsp)
#'
#' #getting the CH as base_poly
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#'
#' delaunay_triangulation_policy(fsp, base_poly = ch)
#'
#' @import sf methods tibble
#' @noRd
delaunay_triangulation_policy <- function(lp, tnorm = "min", base_poly = NULL, d_tolerance = 0, ...) {
  #should we validate the possible acceptable functions?
  sigma <- match.fun(tnorm)

  pts <- st_as_sf(lp, coords = c(1, 2))

  cls <- colnames(lp)[-c(1:3)]
  pgo <- vector("list")

  triangs <- voronoi_delaunay_prep(pts, op = "st_triangulate", base_poly = base_poly, d_tolerance = d_tolerance)
  # getting the indexes of the points of each triangle as a sparse geometry binary predicate list
  triangs_p_int <- st_intersects(triangs, pts)
  
  # lets make a clipping to our base_poly, if it is provided
  if(!is.null(base_poly) && any(class(base_poly) %in% c("POLYGON", "MULTIPOLYGON"))) {
    # note that empty objects are not considered in the creation of a pgeometry object
    triangs <- clip_op(triangs, base_poly)
  }

  #producing the result: we have a plateau spatial object for each class
  for(class in cls){
    # we create list of components for each class
    lcomps <- lapply(seq_along(triangs_p_int), function(index) new("component", obj = triangs[[index]], md = sigma( pts[triangs_p_int[[index]], class][[1]] )))

    pgo <- append(pgo, spa_add_component(create_empty_pgeometry("PLATEAUREGION"), lcomps))
  }

  tibble(class = cls, pgeometry = pgo)
}

#' Convex hull policy for the construction stage, as described in the following paper
#'
#' Carniel, A. C.; Schneider, M.
#' A Systematic Approach to Creating Fuzzy Region Objects from Real Spatial Data Sets.
#' In Proceedings of the 2019 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2019), pp. 1-6, 2019.
#' <https://doi.org/10.1109/FUZZ-IEEE.2019.8858878>
#'
#' @param lp A data.frame or tibble with the labeled points in the format: (x, y, z, ...) where ... are attributes added by the fuzzification step
#' @param M A numeric vector containing the membership degrees that will be used to create the components.
#' @param d A numeric value representing the tolerance distance to compute the membership degree between the elements of M and the membership degrees of the points.
#' @param base_poly An `sfg` object that will be used to clip the generated polygons (optional argument). In fact, this kind of argument for this policy is useless since the plateau regions will be based on the "sub" convex hulls of the dataset.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @return a tibble in the format (class, pgeometry)
#'
#' @examples
#'
#' library(tibble)
#' library(FuzzyR)
#' library(sf)
#' library(dplyr)
#' 
#' set.seed(7)
#' tbl = tibble(x = runif(20, min= 0, max = 30), 
#'   y = runif(20, min = 0, max = 50), 
#'   z = runif(20, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- genmf("trapmf", c(0, 10, 20, 35))
#' hot_mf <- genmf("trimf", c(35, 50, 100))
#' fsp <- fuzzy_set_policy(tbl, classes, mfs = c(cold_mf, hot_mf))
#'
#' chp <- convex_hull_policy(fsp, seq(0.1, 1, by = 0.1), 0.05)
#' chp
#'
#' #getting the CH as base_poly
#' pts <- st_as_sf(tbl, coords = c(1, 2))
#' ch <- st_convex_hull(do.call(c, st_geometry(pts)))
#'
#' chp2 <- convex_hull_policy(fsp, seq(0.1, 1, by = 0.1), 0.05, base_poly = ch)
#'
#' @import sf tibble dplyr
#' @noRd
convex_hull_policy <- function(lp, M = seq(0.05, 1, by = 0.05), d = 0.05, base_poly = NULL, ...) {
  # we create a plateau region based on the convex hull for each class
  result_classes <- list(ncol(lp) - 3)
  cls <- colnames(lp)[-c(1:3)]
  
  for(k in 4:ncol(lp)) {
    result_classes[[k-3]] <- create_empty_pgeometry("PLATEAUREGION")

    for (level in M){
      res <- lp %>% filter((!!as.symbol(cls[k-3])) > 0 & (abs((!!as.symbol(cls[k-3])) - level) <= d))
      
      #if we have at least three points, we are able to produce a polygon
      if(nrow(res) > 2) {
        pts <- st_as_sf(res, coords = c(1, 2))
        ch <- st_convex_hull(do.call(c, st_geometry(pts)))
        
        if(inherits(ch, c("POLYGON", "MULTIPOLYGON"))){
          
          # lets make a clipping to our base_poly, if it is provided
          if(!is.null(base_poly) && inherits(base_poly, c("POLYGON", "MULTIPOLYGON"))) {
            ch <- st_intersection(ch, base_poly)
          }
          
          comp <- component_from_sfg(ch, level)
          pregion <- create_pgeometry(list(comp), "PLATEAUREGION")
          result_classes[[k-3]] <- spa_union(result_classes[[k-3]], pregion)
        }
      }
    }
  }
  
  tibble(class = cls, pgeometry = result_classes)
}

#' @title Building `pgeometry` objects from a point dataset
#'
#' @description This function builds a set of spatial plateau objects from a given point dataset assigned with domain-specific numerical values.
#'
#' @usage
#'
#' spa_creator(tbl, fuzz_policy = "fsp", const_policy = "voronoi", ...)
#'
#' @param tbl A data.frame or tibble with the following format: (x, y, z).
#' @param fuzz_policy The fuzzification policy to be employed by the algorithm. See details below.
#' @param const_policy The construction policy to be used by the algorithm. See details below.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Parameters for the chosen policies. See details below.
#'
#' @details
#'
#' It follows the two-stage construction method described in the research paper of reference.
#'
#' The input `tbl` is a point dataset where each point represents the location of a phenomenon treated by the application.
#' Further, each point is annotated with numerical data that describe its meaning in the application.
#' Therefore, `tbl` must have three columns: (_x_, _y_, _z_). The columns _x_, _y_ are the pair of coordinates, and _z_ is the column containing domain-specific numeric values.
#' 
#' `fuzz_policy` refers to the method used by the **fuzzification stage**.
#' This stage aims to assign membership degrees to each point of the dataset.
#' It accepts three possible values only: `"fsp"` (default), or `"fcp"`.
#'
#' `"fsp"` stands for _fuzzy set policy_ and requires two parameters that should be informed in `...`:
#' - `classes`: A character vector containing the name of classes
#' - `mfs`: A vector of membership functions generated by the function `genmf` of `FuzzyR` package. Each membership function _i_ represents the class _i_, where _i_ in `length(classes)`
#'
#' `"fcp"` stands for _fuzzy clustering policy_ and requires the `e1071` package. Its possible parameters, informed in `...`, are:
#' - `k`: A numeric value that refers to the number of groups to be created
#' - `method`: A fuzzy clustering method of the package `e1071`, which can be either `"cmeans"` (default) or `"cshell"`
#' - `use_coords`: A Boolean value to indicate whether the columns (x, y) should be used in the clustering algorithm (default is `FALSE`)
#' - `iter`: A numeric indicating the number of maximum iterations of the clustering algorithm (default is 100)
#'
#' An optional and common parameter for both fuzzification stages is the `"digits"`. 
#' This is an integer value that indicates the number of decimal digits of the membership degrees calculated by the fuzzification stage.
#' That is, it is used to **round** membership degrees to the specified number of decimal places.
#' Be careful with this optional parameter! If you specify a low value for `"digits"` some membership degrees could be rounded to 0 and thus, some components would not be created.
#'
#' `const_policy` refers to the method used by the **construction stage**.
#' This stage aims to create polygons from the labeled point dataset and use them to build spatial plateau objects.
#' It accepts two possible values only: either `"voronoi"` (default) or "`delaunay"`.
#'
#' `"voronoi"` stands for _Voronoi diagram policy_ and has one optional parameter that can be provided in `...`:
#' - `base_poly`: An `sfg` object that will be used to clip the generated polygons (optional argument). If this parameter is not provided, the Voronoi is created by using a bounding box (standard behavior of `sf`).
#' - `d_tolerance`: It refers to the parameter `dTolerance` employed by the function `st_voronoi` of the package `sf`. 
#'
#' `"delaunay"` stands for _Delaunay triangulation policy_, which accepts the following parameters in `...`:
#' - `base_poly`: An `sfg` object that will be used to clip the generated triangles (optional argument).
#' - `tnorm`: A t-norm used to calculate the membership degree of the triangle. It should be the name of a vector function.
#' Possible values are `"min"` (default), and `"prod"`. 
#' Note that it is possible to use your own t-norms. A t-norm should has the following signature: `FUN(x)` where _x_ is a numeric vector. Such a function should return a single numeric value.
#' - `d_tolerance`: It refers to the parameter `dTolerance` employed by the function `st_triangulate` of the package `sf`. 
#' 
#' `"convex_hull"` stands for _Convex hull policy_, which accepts the following parameters in `...`:
#' - `M`: A numeric vector containing the membership degrees that will be used to create the components. The default is defined by `seq(0.05, 1, by = 0.05)`.
#' - `d`: A numeric value representing the tolerance distance to compute the membership degree between the elements of `M` and the membership degrees of the points. The default is `0.05`.
#' - `base_poly`: An `sfg` object that will be used to clip the generated polygons (optional argument).
#'
#' @return
#'
#' A tibble in the format `(class, pgeometry)`, where `class` is a character column and `pgeometry` is a list of `pgeometry` objects.
#' This means that a spatial plateau object is created for representing a specific class of the point dataset.
#'
#' @references
#'
#' [Carniel, A. C.; Schneider, M. A Systematic Approach to Creating Fuzzy Region Objects from Real Spatial Data Sets. In Proceedings of the 2019 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2019), pp. 1-6, 2019.](https://ieeexplore.ieee.org/document/8858878/)
#'
#' @examples
#'
#' library(tibble)
#' library(FuzzyR)
#' 
#' set.seed(7)
#' tbl = tibble(x = runif(10, min= 0, max = 30), 
#'              y = runif(10, min = 0, max = 50), 
#'              z = runif(10, min = 0, max = 100))
#' classes <- c("cold", "hot")
#' cold_mf <- genmf("trapmf", c(0, 10, 20, 35))
#' hot_mf <- genmf("trimf", c(35, 50, 100))
#' 
#' spa_creator(tbl, classes = classes, mfs = c(cold_mf, hot_mf))
#'
#' spa_creator(tbl, fuzz_policy = "fcp", k = 4)
#' 
#' spa_creator(tbl, fuzz_policy = "fcp", k = 4, digits = 2)
#'
#' spa_creator(tbl, fuzz_policy = "fcp", k = 3, const_policy = "delaunay")
#'
#' spa_creator(tbl, fuzz_policy = "fcp", const_policy = "delaunay", k = 3, tnorm = "prod")
#' 
#' spa_creator(tbl, fuzz_policy = "fcp", k = 2, digits = 2, 
#'             M = seq(0.1, 1, by = 0.1), d = 0.05, const_policy = "convex_hull")
#'             
#' spa_creator(tbl, classes = classes, mfs = c(cold_mf, hot_mf), 
#'             digits = 2, const_policy = "convex_hull")
#'
#' @import methods
#' @export
spa_creator <- function(tbl, fuzz_policy = "fsp", const_policy = "voronoi", ...) {
  # should we validate the params here instead of validating them in the policies?
  params <- list(...)

  # first step is to apply the fuzzification step
  fuzz_stage <- switch(fuzz_policy,
                       fsp = do.call(fuzzy_set_policy, c(list(tbl = tbl), params)),
                       fcp = do.call(fuzzy_clustering_policy, c(list(tbl = tbl), params)),
                       stop(paste0("The fuzzification policy '", fuzz_policy, "' is not a supported policy.
                                   The values are 'fsp', 'fsc', 'mesma'."), call. = FALSE)
                       )

  # from https://stat.ethz.ch/R-manual/R-devel/library/base/html/integer.html
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  if(hasArg("digits")) {
    if(is.wholenumber(params$digits)) {
      fuzz_stage[ , 4:ncol(fuzz_stage)] <- round(fuzz_stage[ , 4:ncol(fuzz_stage)], params$digits)
    } else {
      stop("The argument 'digits' has to be an integer value.", call. = FALSE)
    }
  }
  
  # a short validation for base_poly, which should be an sfg object
  if(hasArg("base_poly")) {
    if(!is.null(params$base_poly)) {
      if(!inherits(params$base_poly, c("sfg", "sfc"))) {
        stop("The argument 'base_poly' should be an sfg object.", call. = FALSE)
      } 
      if(inherits(params$base_poly, "sfc")) {
        warning("The argument 'base_poly' is an sfc. We will take only its first element without considering its CRS.", call. = FALSE)
        params$base_poly <- params$base_poly[[1]]
      }
    } 
  }
  
  # second step is to apply the construction step
  result <- switch(const_policy,
    voronoi = do.call(voronoi_diagram_policy, c(list(lp = fuzz_stage), params)),
    delaunay = do.call(delaunay_triangulation_policy, c(list(lp = fuzz_stage), params)),
    convex_hull = do.call(convex_hull_policy, c(list(lp = fuzz_stage), params)),
    stop(paste0("The construction policy '", const_policy, "' is not a supported policy.
                                   The possible values are 'voronoi' and 'delaunay'."), call. = FALSE)
    )

  # then, we return the result
  result
}
