# Classes for Spatial Fuzzy R Package


library('sf')
library('tibble')
library('dplyr')

# Component Class 

setClass("Component",
         slots = list(
           obj = "XY",
           md = "numeric"
         )
)


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
  
  component <- new("Component", obj = obj_component,md=md)
}


# SpatialPlateau Class (Super Class)

setClass("SpatialPlateau",
         slots = list(
           component = "list",
           supp = "XY",
           type = "character"
         )
)



create_spatial_plateau <- function(components, type){
  
  # Repensar no estilo de Helper 
  # create_ ? ...
  
  # Checar se é lista ou dataframe 
  
  if(inherits(components, "list")){
    # Caso LISTA
    ordered_comp <- function(components){
      md_value = c()
      for(comp in 1:length(components)){
        md = components[[comp]]@md
        md_value[comp] <- md
      }
      order_comps = order(md_value)
      new_components <- components[order_comps]
      return(new_components)
    }
    new_list <- ordered_comp(components)
    
      create_supp <- function(components){
        obj_sf = c()
      for(comp in 1:length(components)){
        object_sf = components[[comp]]@obj
        obj_sf[comp] <- object_sf
      }
        supp = st_union(st_sfc(obj_sf))
      return(supp)
      }
      supp = create_supp(components)
  }
  
  else if(inherits(components, "data.frame") || inherits(df, "tibble")){
    
    new_df <- arrange(components, components[1])
    
    new_list = vector("list", nrow(new_df))
    
    for(i in 1:nrow(new_df)){
      #new_list[[i]] <- row
      new_list[[i]] <- new("Component", obj = new_df[i,2][[1]], md = new_df[i, 1])
    }
    
    
    supp = st_union(new_df[,2])
    # REPETIR PARA IF LISTA 
  }
  # CEHCAR OS TIPOS ( POINT, LINE , REGION)
  
  # 1 CHECAGEM - VERIFICAR TIPOS INPUT USUARIO
  # TYPES:
  # plateau_point
  # plateau_line
  # plateau_region
  
  new("SpatialPlateau", component = new_list, supp = supp[[1]], type = type)
  
}













setClass("PlateauLine",
         contains = "SpatialPlateau"
         )


setClass("PlateauRegion",
         contains = "SpatialPlateau"
         )



