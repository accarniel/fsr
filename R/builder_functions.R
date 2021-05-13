#' @title fuzzysetpolicy
#'
#' @description
#'
#'
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'
# the format of data is a data frame with at least three columns (x, y, z)
# we will apply the fuzzy sets in fuzzysets on the z values and return another data frame with the resulting values
fuzzysetpolicy <- function(data, classes, fuzzysets) {
  require(FuzzyR)
  if (.hasSlot(data, 'coords')) {
    input <- data@coords
  } else input <- data

  result <- data.frame(x = numeric(), y = numeric())
  #adding the new columns
  for(i in 1:length(classes)){
    result[, ncol(result) + 1] <- numeric()
    names(result)[ncol(result)] <- classes[i]
  }

  for(i in 1:nrow(input)) {
    row <- input[i,]
    x <- as.numeric(row[1])
    y <- as.numeric(row[2])
    z <- as.numeric(row[3])

    result[i,1] <- x
    result[i,2] <- y
    #evaluating the membership degree for each class
    for(j in 1:length(fuzzysets)) {
      #result[i, j+2] <- evaluate(fuzzysets[[j]], z)
      result[i, j+2] <- evalmf(z, fuzzysets[[j]])
    }
  }

  result
}


#' @title fuzzyclusteringpolicy
#'
#' @description
#'
#'
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'
fuzzyclusteringpolicy <- function(data, method, sclus, k, iter=100) {
  require(e1071)
  if (.hasSlot(data, 'coords')) {
    input <- data@coords
  } else input <- data

  result <- data.frame(x = as.numeric(input[,1]), y = as.numeric(input[,2]))
  #adding the new columns, each column is a group here.
  for(i in 1:k){
    result[, ncol(result) + 1] <- numeric()
    names(result)[ncol(result)] <- paste0("group_", i)
  }

  cat("Method =", method, "\n", "Considering the points? ", sclus, "\n")
  cat("Number of clusters =", k, "\n", "Max. iterations =", iter, "\n")

  cm <- NULL

  if(method == "cmeans") {
    if(sclus) {
      cm <- cmeans(input, k, iter)
    } else {
      input2 <- input[, 3]
      cm <- cmeans(input2, k, iter)
    }
  } else if(method == "cshell") {
    if(sclus) {
      cm <- cshell(input, k, iter)
    } else {
      input2 <- input[, 3]
      cm <- cshell(input2, k, iter)
    }
  }

  memb <- cm$membership
  for(i in 1:k){
    result[, i + 2] <- memb[, i]
  }

  result
}



#' @title fuzzyclusteringpolicy
#'
#' @description
#'
#'
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'
generatepolygons <- function(input, basepoly, method, m, refin=TRUE) {
  require(deldir)

  #get the bbox and the rectangle encompassing the base polygon
  bb = bbox(basepoly)
  rw = as.numeric(t(bb))

  if(method == "voronoi") {
    cat("Executing the Voronoi diagram...\n")
  } else if(method == "delaunay") {
    cat("Executing the Delaunay triangulation...\n")
  }

  #generating the Voronoi diagram/Delaunay triangulation
  res <- deldir(input, rw = rw, z = input[,m])

  #now we will transform the voronoi cells or the triangles into polygons
  l <- NULL
  if(method == "voronoi") {
    l <- tile.list(res)
  } else if(method == "delaunay") {
    l <- triang.list(res)
  }
  #polys <- vector(mode='list', length=length(l))
  polys <- list()
  xyz <- data.frame(x = numeric(), y = numeric(), z = numeric())
  index <- 1

  require(sp)
  require(rgeos)
  for (i in 1:length(l)) {
    pcrds <- cbind(l[[i]]$x, l[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    pol <- Polygons(list(Polygon(pcrds)), ID=as.character(index))
    t <- SpatialPolygons(list(pol))
    proj4string(t) <- proj4string(basepoly)
    if(gContains(basepoly, t)) {
      polys[[index]] <- pol
      if(method == "voronoi") {
        xyz[index,] <- list(l[[i]]$pt["x"], l[[i]]$pt["y"], l[[i]]$z)
      } else if(method == "delaunay") {
        centr <- gCentroid(t)
        xyz[index,] <- list(as.numeric(centr@coords[,1]), as.numeric(centr@coords[,2]), as.numeric(min(l[[i]]$z)))
      }
      index <- index + 1
    } else if(refin & gIntersects(basepoly, t)) {
      inter <- gIntersection(basepoly, t)
      if(inherits(inter@polygons[[1]], "Polygons")) {
        polys[[index]] <- inter@polygons[[1]]
        polys[[index]]@ID <- as.character(index)
        if(method == "voronoi") {
          xyz[index,] <- list(l[[i]]$pt["x"], l[[i]]$pt["y"], l[[i]]$z)
        } else if(method == "delaunay") {
          centr <- gCentroid(t)
          xyz[index,] <- list(as.numeric(centr@coords[,1]), as.numeric(centr@coords[,2]), as.numeric(min(l[[i]]$z)))
        }
        index <- index + 1
      }
    }
  }
  SP <- SpatialPolygons(polys)

  cat("Returning the polygons. They are ", index, " \n")

  SpatialPolygonsDataFrame(
    SP, data.frame(x=xyz[,1], y=xyz[,2], z=xyz[,3], ID=1:length(SP),
                   row.names=sapply(slot(SP, 'polygons'),
                                    function(x) slot(x, 'ID'))))
}



#' @title fuzzyclusteringpolicy
#'
#' @description
#'
#'
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'
voronoidiagrampolicy <- function(lp, c, basepoly, refin=TRUE) {
  require(sp)
  require(rgeos)

  res <- vector(mode='list', length=length(c))
  for (i in 1:length(c)) {
    cat("Processing the class = ", c[i], "\n")

    ## ESSE TIPO DE FILTRO NÃO PODE ACONTECER, POIS ESSAS REGIOES COM 0 SÃO OS BURACOS DA CAMADA
    ## ELAS DEVEM SER REMOVIDAS SOH LÁ NA FRENTE!
    # p <- filter(lp, lp[i+2] > 0)
    #p <- subset.data.frame(lp, (get(c[i])) > 0)
    p <- lp



    pts <- SpatialPoints(subset.data.frame(p, select=c("x", "y")))
    proj4string(pts) <- proj4string(basepoly)
    cv <- gConvexHull(pts)
    proj4string(cv) <- proj4string(basepoly)
    inter <- gIntersection(cv, basepoly)
    proj4string(inter) <- proj4string(basepoly)

    spdf <- generatepolygons(p, inter, "voronoi", m=i+2, refin)
    proj4string(spdf) <- proj4string(basepoly)

    # INTEGRATION WITH THE CODE OF THE FELIPPE
    #res[[i]] <- spdf

    cat("Creating the plateau region object...\n")

    df_aux <- as.data.frame(as(spdf, "sf"))
    df_aux <- select(df_aux, z, geometry) %>% filter(z > 0)

    res[[i]] <- create_spatial_plateau(components = df_aux,  type = "plateau_region")
  }

  cat("Forming the final result PRO...\n")

  #pro <- data.frame(class = c, pgeom = res)
  pro <- cbind(c, res)
  pro <- as.data.frame(pro)
  colnames(pro) <- c("class", "pgeom")

  pro
}



#' @title fuzzyclusteringpolicy
#'
#' @description
#'
#'
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'
delaunaydiagrampolicy <- function(lp, c, basepoly, refin=TRUE) {
  require(sp)
  require(rgeos)

  res <- vector(mode='list', length=length(c))
  for (i in 1:length(c)) {
    cat("Processing the class = ", c[i], "\n")

    p <- subset.data.frame(lp, (get(c[i])) > 0)

    pts <- SpatialPoints(subset.data.frame(p, select=c("x", "y")), proj4string = crs(basepoly))
    cv <- gConvexHull(pts)
    proj4string(cv) <- proj4string(basepoly)
    inter <- gIntersection(cv, basepoly)
    proj4string(inter) <- proj4string(basepoly)

    spdf <- generatepolygons(p, inter, "delaunay", m=i+2, refin)
    proj4string(spdf) <- proj4string(basepoly)
    res[[i]] <- spdf
  }

  cat("Forming the final result PRO...\n")
  pro <- cbind(c, res)

  #TODO include the PWKT representation of the plateau region object - to be extracted from the PolygonsDataFrame

  pro
}






