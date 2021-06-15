# @title
# @description
# @usage
# @param (todos os parâmetros...)
# @details
# @return
# @references (para aquelas funções que foram definidas em algum artigo)
# @seelaso (apontando para funções similares)
# @examples
# @export -> para deixar a função visivel para os usuários


#' @title Create a Component of Spatial Plateau Geometry
#'
#' @description
#' Create an object composed by a Spatial Feature Geometry (sfg) and a Membership Degree.
#'
#' @param raw_obj Vector, list or matrix containing the points to create a sfg object.
#' @param md Component Membership Degree.
#' @param type Spatial Feature Geometric to create.
#'
#' @details
#' A component class is a object composed by 2 slots: (1) a Crisp spatial object (sfg)
#' and a Membership Degree associated to this object, in the interval ]0,1].
#'
#' The types "POINT", "LINE" and "REGION" are related to Spatial Plateau Point,
#' Spatial Plateau Line and Spatial Plateau Region. For each input type, we have the
#' following sfg objects created:
#' \itemize{
#' \item{"POINT"}{ If raw_obj is a vector of single points, }
#' \item{"LINE"}{ If raw_obj is a vector of single points, }
#' \item{"REGION"}{ If raw_obj is a vector of single points, }
#' }
#'
#'
#' @return A object of class "component".
#' @examples
#'
#' # Creating a POINT component type
#' v1 = rbind(c(1,2), c(3,4))
#' v2 = rbind(c(1,4), c(2,3),c(4,4))
#'
#' md1 = 0.2
#' md2 = 0.1
#'
#' comp1 <- create_component(v1, md1, type="POINT")
#' comp2 <- create_component(v2, md2, type="POINT")
#'
#' # Creating a LINE component type
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
#' # Creating a REGION component type
#'
#' p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' list_pols_1 <- list(p1,p2)
#'
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
    # Single Point or MultiPoint
    if(inherits(raw_obj, "numeric")){
      obj_component = st_point(raw_obj)
    }
    else if(inherits(raw_obj, "matrix")){
      obj_component = st_multipoint(raw_obj)
    }
  }

  else if(type=="LINE"){
    if(inherits(raw_obj, "matrix")){
      obj_component = st_linestring(raw_obj)
    }
    else if(inherits(raw_obj, "list")){
      obj_component = st_multilinestring(raw_obj)
    }
  }

  else if(type=="REGION"){
    if(inherits(raw_obj[[1]], "matrix")){
      obj_component = st_polygon(raw_obj)
    }
    else if(inherits(raw_obj[1], "list")){
      obj_component = st_multipolygon(raw_obj)
    }
  }

  new("component", obj = obj_component,md=md)
}


#' @title Create an empty Plateau Geometry object
#'
#' @description
#'
#'
#' @param type Type of Plateau Geometry to be created..
#'
#' @return
#' A pgeom class object.
#' @examples
#'
#' pgeom_point <- create_empty_pgeom("PLATEAUPOINT")
#' pgeom_line <- create_empty_pgeom("PLATEAULINE")
#' pgeom_region <- create_empty_pgeom("PLATEAUREGION")
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
    new("component", obj = sfg,md=md)
  } else {
    stop(paste(sfg_type, "type not allowed to create a component."), call. = FALSE)
  }
}

#' @title Create an empty Plateau Geometry object
#'
#' @description
#'
#'
#' @param type Type of Plateau Geometry to be created..
#'
#' @return
#' A pgeom class object.
#' @examples
#'
#' pgeom_point <- create_empty_pgeom("PLATEAUPOINT")
#' pgeom_line <- create_empty_pgeom("PLATEAULINE")
#' pgeom_region <- create_empty_pgeom("PLATEAUREGION")
#' @import sf methods
#' @export
create_empty_pgeom <- function(type){

  type = toupper(type)
  if(is_pgeom(type)){

    if(type == "PLATEAUPOINT"){
      new("pgeom", component = list(), supp = st_multipoint(), type = type)
    }
    else if(type == "PLATEAULINE"){
      new("pgeom", component = list(), supp = st_multilinestring(), type = type)
    }
    else if(type == "PLATEAUREGION"){
      new("pgeom", component = list(), supp = st_multipolygon(), type = type)
    }
  } else {
    stop("Invalid data type", call. = FALSE)
  }

}

#' @title create_pgeom
#'
#' @description
#' Create an object of class pgeom (Plateau Geomtry)
#'
#' @param components A list, tibble or dataframe of componenets.
#' @param type The type of Spatial Plateau Geometry Object to be created.
#'
#' @return
#' An pgeom class object.
#' @examples
#'
#' @seealso \code{\link{create_component}}
#' @import sf tidyverse
#' @export
create_pgeom <- function(components, type){

  type = toupper(type)

  if(is_pgeom(type)){
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
      new_df <- arrange(components, components[1])
      new_components = vector("list", nrow(new_df))

      for(i in 1:nrow(new_df)){
        new_components[[i]] <- new("component", obj = new_df[i,2][[1]], md = new_df[i, 1])
        obj_comp = new_components[[i]]@obj

        if(!is_compatible(obj_comp, type)){
          stop("Input Component type error.Please verify if your component type is correct", call. = FALSE)
        }
      }
      supp = st_union(new_df[,2])
    }
    new("pgeom", component = new_components, supp = supp[[1]], type = type)
  }
}

#' @title Transforms a pgeom object into a tibble object
#'
#' @description
#'
#'
#' @param pgeom pgeom object
#'
#' @return
#' A tibble object with two columns: md and geometry
#' @examples
#'
#' @import sf tidyverse
#' @export
pgeom_as_tibble <- function(pgeom){

  get_md <- function(comp){
    md <- comp@md
    return(md)
  }

  get_obj <- function(comp){
    obj <- comp@obj
    return(obj)
  }

  pgeom_tibble <- tibble(
    md <- unlist(lapply(pgeom@component, get_md)),
    points <- st_sfc(lapply(pgeom@component, get_obj))
  )
  colnames(pgeom_tibble) <- c('md','geometry')
  return(pgeom_tibble)
}


#' @title search_by_md
#'
#' @description Searches by a given membership degree in a list of components and their respectives md, returning either TRUE
#' and the index of the component that has that md or FALSE if not found.
#'
#' This function use Bynary Search to search by the md.
#'
#' @param components List with components
#' @param low First index of the list to start the binary search
#' @param high Last element of the list to end the binary search
#' @param m Membership Degree searched
#'
#' @return If found, returns a vector with TRUE and the index position.
#' If not found, returns a vector with FALSE and the first position given (low)
#'
#' @examples
#'
#'
#' @import tidyverse
#' @export
search_by_md <- function(components, low, high, m){


  while(low <= high){

    mid = floor((low + high)/2)

    if(near(m, components[[mid]]@md)){
      return(c(TRUE,mid))
    }

    else if(m < components[[mid]]@md){
      high = mid - 1
    }

    else{
      low = mid + 1
    }
  }
  return(c(FALSE, low))

}


#' @title pgeom_plot
#'
#' @description
#'
#'
#' @param spo
#' @param low
#' @param high
#'
#' @return
#' @examples
#'
#' @import sf tidyverse
#' @export
pgeom_plot <- function(pgeom,  base_poly = NULL, low = "white", high = "black", palette = "Greys", m = 10, ...){

  pgeom_tibble <- pgeom_as_tibble(pgeom)

  if(!is.null(base_poly)) {
    pgeom_tibble$geometry <- st_intersection(pgeom_tibble$geometry, base_poly)
  }

  if(inherits(pgeom_tibble$geometry, "sfc_MULTILINESTRING")||
     inherits(pgeom_tibble$geometry, "sfc_MULTIPOINT")||
     inherits(pgeom_tibble$geometry, "sfc_LINESTRING")||
     inherits(pgeom_tibble$geometry, "sfc_POINT")){

    if(nrow(pgeom_tibble) > m){

      plot <-  ggplot(pgeom_tibble) +
        geom_sf(aes(color = md, geometry=geometry), ...) +
        scale_colour_gradient(name="", limits = c(0, 1),  low = low, high = high)  +
        theme_classic()

    } else {

      pgeom_tibble$md <- as.factor(pgeom_tibble$md)
      pgeom_tibble$md <- factor(pgeom_tibble$md, levels=c(levels(pgeom_tibble$md), "0"))

      plot <- ggplot(pgeom_tibble) +
        geom_sf(aes(color = md, geometry=geometry), ...) +
        scale_colour_brewer(name="", palette = palette) +
        theme_classic() +
        theme(legend.position = "none")
    }

  }

  else{

    if(nrow(pgeom_tibble) > m){
      # lwd = 0 ; color = NA
      plot <-  ggplot(pgeom_tibble) +
        geom_sf(aes(fill = md, geometry=geometry), ...) +
        scale_fill_gradient(name="", limits = c(0, 1),  low = low, high = high) +
        theme_classic()

    } else {

      pgeom_tibble$md <- as.factor(pgeom_tibble$md)
      pgeom_tibble$md <- factor(pgeom_tibble$md, levels=c(levels(pgeom_tibble$md), "0", "1"))

      plot <- ggplot(pgeom_tibble) +
        geom_sf(aes(fill = md, geometry=geometry), ...) +
        scale_fill_brewer(name="", palette = palette) +
        theme_classic()
    }


  }
  return(plot)
}


#' @title Check compatibility of sfg object
#'
#' @description
#'
#'
#' @param sfg
#' @param type
#'
#' @return
#' @examples
#'
#' @import sf
#' @export
#'
is_compatible <- function(sfg, ptype){

  ptype = toupper(ptype)

  if(class(sfg)[1] == "XY"){
    sfg_type = class(sfg)[2]
    if(sfg_type == "POINT" || sfg_type == "MULTIPOINT"){
      if(ptype=="PLATEAUPOINT"){
        TRUE
      } else{
        FALSE
      }
    }
    else if(sfg_type == "LINESTRING" || sfg_type == "MULTILINESTRING"){
      if(ptype=="PLATEAULINE"){
        TRUE
      } else{
        FALSE
      }
    }
    else if(sfg_type == "POLYGON" || sfg_type == "MULTIPOLYGON"){
      if(ptype=="PLATEAUREGION"){
        TRUE
      } else{
        FALSE
      }
    }} else{
      stop("Component is not a sfg data type", call. = FALSE)
    }
}



#' @title Check validity of
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
is_pgeom <- function(type){
  if(!(type %in% c('PLATEAUPOINT', 'PLATEAULINE', 'PLATEAUREGION'))){
    FALSE
  } else{
    TRUE
  }
}


#' @title pgeom_is_empty
#'
#' @description
#'
#'
#' @param spo
#'
#' @return
#' @examples
#'
#' @import sf
#' @export
#'
pgeom_is_empty <- function(pgeom){
  if(st_is_empty(pgeom@supp) && !length(pgeom@component)){
    TRUE
  }
  else{
    FALSE
  }
}

#' @title get_counter_ctype
#'
#' @description
#' Get the equivalent crisp spatial data type
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @noRd
get_counter_ctype <- function(pgeom){
  ptype <- pgeom@type

  type <- switch(ptype,
                 PLATEAUPOINT = "POINT",
                 PLATEAULINE = "LINESTRING",
                 PLATEAUREGION = "POLYGON")

  type
}


#' @title check_geom_sfg_pgeom
#'
#' @description
#'
#'
#' @param spo
#'
#' @return
#' @examples
#'
#' @import sf methods
#' @noRd
check_geom_sfg_pgeom <- function(sfg, pgeom, md, lcomps){
  if(!st_is_empty(sfg) && is_compatible(sfg, pgeom@type)){
    result_comp <- new("component", obj = sfg, md = md)
    lcomps <- append(lcomps, result_comp)
  } else if(!st_is_empty(sfg) && st_geometry_type(sfg) == "GEOMETRYCOLLECTION") {
    type_geom = get_counter_ctype(pgeom)
    result_comp <- new("component", obj = st_union(st_collection_extract(sfg, type = type_geom))[[1]], md = md)
    lcomps <- append(lcomps, result_comp)
  }
  lcomps
}


#' @title f_diff
#' @family Fuzzy Difference Operators
#' @description
#'
#'
#' @param spo
#'
#' @return
#' @examples
#' @export
#'
fdiff <- function(x, y){
  min(x, (1 - y))
}

#' @title f_bound_diff
#' @family Fuzzy Difference Operators
#' @description
#'
#'
#' @param x
#' @param y
#'
#' @return
#' @examples
#'
#' @export
f_bound_diff <- function(x, y){
  max(0, (x - y))
}

#' @title f_symm_diff
#' @family Fuzzy Difference Operators
#' @description
#'
#'
#' @param spo
#'
#' @return
#' @examples
#' @export
f_symm_diff <- function(x, y){
  abs(x - y)
}

#' @title
#' @family Fuzzy Difference Operators
#' @description
#'
#'
#' @param spo
#'
#' @return
#' @examples
#' @noRd
check_spa_topological_condition <- function(pgeom1, pgeom2){
  if(pgeom1@type != pgeom2@type){
    stop("Different Spatial Plateau Types.", call. = FALSE)
  } else if(pgeom1@type != "PLATEAUREGION"){
    stop(paste("Operator not implemented to", pgeom1@type), call. = FALSE)
  }
}
