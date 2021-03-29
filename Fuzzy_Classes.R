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





  
  
  
check_type_correspondence <- function(component, type){
  
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




check_spatial_plateau_type <- function(type){
  if(!(type %in% c('plateau_point', 'plateau_line', 'plateau_region'))){
    stop(paste("The type '", type,"' is not a valid Spatial Plateau Type.
               The type must be either 'plateau_point', 'plateau_line' 
               or 'plateau_region'.", sep=''))
  } else{
    TRUE
  }
}


create_empty_spatial_plateau <- function(type){
  
  type = tolower(type)
  if(check_spatial_plateau_type(type)){
    
    if(type=='plateau_point'){
      new("SpatialPlateau", component = list(), supp = st_multipoint(), type = type)
    }
    else if(type=='plateau_line'){
      new("SpatialPlateau", component = list(), supp = st_multilinestring(), type = type)
    }
    else if(type=='plateau_region'){
      new("SpatialPlateau", component = list(), supp = st_multipolygon(), type = type)
    }
  }  
  
}


# spatial_plateau_builder(DEFINICAO FORMAL (TODAS))

# spt_plateau_builder(obj_spatial_plateau, component)

#### Spatial Plateau Builder 



# functions <- checar se SPO é vazio

spo_is_empty <- function(spo){
  if(st_is_empty(spo@supp) && !length(spo@component)){
    TRUE
}
  else{
    FALSE
  }
}


# functions <- binary_search.

md_binary_search <- function(components, low, high, m){
  
  mid = (low + high)/2
  
  if(low <= high){
    if(m== components[[mid]]@md){
      mid
    }
    
    else if(m < components[[mid]]@md){
      md_binary_search(components, low, mid - 1, m)
    }
    
    else{
      md_binary_search(components, mid + 1, high, m)
    }
  }
  else{
    ceiling(mid)
  }
}




md_binary_search <- function(components, low, high, m){
  
  mid = (low + high)/2
  
  if(low <= high){
    if(dplyr::near(m, components[[mid]]@md)){
      return(c(TRUE,mid))
    }
    
    else if(m < components[[mid]]@md){
      md_binary_search(components, low, mid - 1, m)
    }
    
    else{
      md_binary_search(components, mid + 1, high, m)
    }
  }
  else{
    return(c(FALSE,ceiling(mid)))
  }
}



# ITERATIVE MODE 
md_binary_search_iter <- function(components, low, high, m){
  
  
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
  # mid = floor((low + high)/2)
  # if(mid <= 0){
  #   mid = 1
  # }
  # if(m <= components[[mid]]@md){
  #   return(c(FALSE, mid))
  # }
  # 
  # return(c(FALSE,mid+1))
  }
  


#return(c(FALSE, low))

#mid = floor((low + high)/2)
#if(mid <= 0){
#  mid = 1
#}
#if(m <= components[mid]){
#  return(c(FALSE, mid))
#}

#return(c(FALSE,mid+1))



spa_plateau_builder <- function(spo, component){
  
  # VALIDAR AS DUAS ENTRADAS:
  # Não permitir que sejam nulas.
  
  if(is.null(spo)){
    stop("spo is null. Please use create_spatial_plateau() to 
         create an empty spatial plateau object.")
  }
  
  if(is.null(component)){
    stop("component is null. Please create a component.")
  }
  
  # Checar os tipos e classes...

  if(class(spo)!="SpatialPlateau"){
    stop(paste(spo, " is not a SpatialPlateau Object.", sep = ' '))
  }
  
  if(class(component)!="Component"){
    stop(paste(component, " is not a Component Object.", sep = ' '))
  }

  #
  
  c <- component@obj
  m <- component@md
  
  
  
  # 1. spo ------> if c = ∅ ∨m = 0
  # spo <- Se c é diferente de vazio  OU m = 0
  # FEITO
  
  if(is.null(c) || st_is_empty(c) || m == 0){
    return(spo)
  }
  
  
  # 2. if po = �� ∧ c �= ∅ ∧m > 0
  # Se plateau_object E c diferente de vazio E m > 0
  # FEITO
  else if(spo_is_empty(spo) && !(is.null(c) || st_is_empty(c)) && m > 0){
    spo@component[[1]] <- component
    spo@supp <- c
    return(spo)
  }
  
  
  

  else if(!is.null(c) & length(c) >= 1){
    index = md_binary_search_iter(spo@component, 1, length(spo@component), m)

# 3. if c �= ∅ ∧ n ≥ 1 ∧ ∃i ∈ {1, ..., n} : mi = m
    if(index[1]==TRUE){
      spo@component[[index[2]]]@obj <- st_union(spo@component[[index[2]]]@obj, c)
      spo@supp <- st_union(spo@supp, c)

    }

# 4. if c �= ∅ ∧ n ≥ 2 ∧ ∃i ∈ {1, ..., n − 1} : mi < m < mi+1
# 5. if c �= ∅ ∧ n ≥ 1 ∧ 0 < m < mi
# 6. if c �= ∅ ∧ n ≥ 1 ∧m > mn
    else{
      spo@component <- append(spo@component, component, after=index[2]-1)
      spo@supp <- st_union(spo@supp,c)

    }
    return(spo)    
  }
}
















# type  = plateau_...*
create_spatial_plateau <- function(components, type){
  
  type = tolower(type)

  if(check_spatial_plateau_type(type)){


  if(inherits(components, "list")){


      md_value = c()
      for(comp in 1:length(components)){
        md = components[[comp]]@md
        obj_comp = components[[comp]]@obj
      
        if(!check_type_correspondence(obj_comp, type)){
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
      #new_list[[i]] <- row
      ##### INSERIR CHECAGEM DO TIPO DE CADA COMPONETE ####
      new_components[[i]] <- new("Component", obj = new_df[i,2][[1]], md = new_df[i, 1])
      
      obj_comp = new_components[[i]]@obj
      
      if(!check_type_correspondence(obj_comp, type)){
        stop("Input Component type error.Please verify if your component type is correct")
      }

    }
    
    supp = st_union(new_df[,2])

  }
 
  new("SpatialPlateau", component = new_components, supp = supp[[1]], type = type)
  }
  
}






##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################



