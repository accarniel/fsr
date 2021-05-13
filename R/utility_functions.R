#' @title create_component
#'
#' @description
#'
#'
#' @param raw_obj
#' @param md
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'

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

  component <- new("component", obj = obj_component,md=md)
  component
}


################################################################################


#' @title create_empty_pgeom
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
create_empty_pgeom <- function(type){

  type = tolower(type)
  if(is_pgeom(type)){

    if(type=='plateau_point'){
      new("pgeom", component = list(), supp = st_multipoint(), type = type)
    }
    else if(type=='plateau_line'){
      new("pgeom", component = list(), supp = st_multilinestring(), type = type)
    }
    else if(type=='plateau_region'){
      new("pgeom", component = list(), supp = st_multipolygon(), type = type)
    }
  }

}

#' @title create_pgeom
#'
#' @description
#'
#'
#' @param components
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'
create_pgeom <- function(components, type){

  type = tolower(type)

  if(is_pgeom(type)){


    if(inherits(components, "list")){


      md_value = c()
      for(comp in 1:length(components)){
        md = components[[comp]]@md
        obj_comp = components[[comp]]@obj

        if(!is_compatible(obj_comp, type)){
          stop("Input Component type error. Please verify if your component type is correct")
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

    else if(inherits(components, "data.frame") || inherits(df, "tibble")){

      new_df <- arrange(components, components[1])

      new_components = vector("list", nrow(new_df))


      for(i in 1:nrow(new_df)){

        new_components[[i]] <- new("component", obj = new_df[i,2][[1]], md = new_df[i, 1])

        obj_comp = new_components[[i]]@obj

        if(!is_compatible(obj_comp, type)){
          stop("Input Component type error.Please verify if your component type is correct")
        }

      }

      supp = st_union(new_df[,2])

    }

    new("pgeom", component = new_components, supp = supp[[1]], type = type)
  }

}



#' @title pgeom_as_tibble
#'
#' @description
#'
#'
#' @param pgeom
#'
#' @return
#' @examples
#'
#' @export
#'
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
    md <- unlist(lapply(spo@component, get_md)),
    points <- st_sfc(lapply(spo@component, get_obj))
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
#' md_index <- search_by_md(components= toydata, low = 1, high = 10, md=0.4)
#'
#'
#'
#' @export
#'
search_by_md <- function(components, low, high, m){


  while(low <= high){

    mid = floor((low + high)/2)

    if(dplyr::near(m, components[[mid]]@md)){
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
#' @export
#'
pgeom_plot <- function(pgeom, low = "white", high = "black"){

  pgeom_tibble <- pgeom_as_tibble(pgeom)

  if(inherits(pgeom_tibble$geometry, "sfc_MULTILINESTRING")||
     inherits(pgeom_tibble$geometry, "sfc_MULTIPOINT")||
     inherits(pgeom_tibble$geometry, "sfc_LINESTRING")||
     inherits(pgeom_tibble$geometry, "sfc_POINT")){

    plot <- ggplot(pgeom_tibble) +
      geom_sf(aes(colour = md, geometry=geometry), size = 0) + theme_classic() +
      scale_colour_gradient(name="", limits = c(0, 1), low = low, high = high)
  }

  else{
    plot <-  ggplot(pgeom_tibble) +
      geom_sf(aes(fill = md, geometry=geometry), size = 0) + theme_classic() +
      scale_fill_gradient(name="", limits = c(0, 1),  low = low, high = high)
  }
  return(plot)
}



#' @title is_compatible
#'
#' @description
#'
#'
#' @param component
#' @param type
#'
#' @return
#' @examples
#'
#' @export
#'
is_compatible <- function(component, type){

  type = tolower(type)
  #type to lower
  if(class(component)[1] == "XY"){
    component_type = class(component)[2]
    if(component_type == "POINT" || component_type == "MULTIPOINT"){
      if(type=="plateau_point"){
        TRUE
      } else{
        FALSE
      }
    }
    else if(component_type == "LINESTRING" || component_type == "MULTILINE"){
      if(type=="plateau_line"){
        TRUE
      } else{
        FALSE
      }
    }
    else if(component_type == "POLYGON" || component_type == "MULTIPOLYGON"){
      if(type=="plateau_region"){
        TRUE
      } else{
        FALSE
      }
    }} else{
      stop("Component is not a sf data type")
    }
}



#' @title is_pgeom
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
  if(!(type %in% c('plateau_point', 'plateau_line', 'plateau_region'))){
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


#
########################################################################



