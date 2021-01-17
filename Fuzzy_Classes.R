# Classes for Spatial Fuzzy R Package


library('sf')
library('tibble')


# Component Class 

setClass("Component",
         slots = list(
           obj = "XY",
           md = "numeric"
         )
)


# SpatialPlateau Class (Super Class)

setClass("SpatialPlateau",
         slots = list(
           component = "list",
           supp = "XY"
         )
)



# Point Class 

setClass("PlateauPoint",
        contains = "SpatialPlateau"
        )



SpatialPlateau <- function(components){
  
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
  
  new("SpatialPlateau", component = new_list, supp = supp[[1]])
  
  ### CHECAR neste HELPER!
  # 1. Aceitar apenas MULTIPOINT ou POINT 
}













setClass("PlateauLine",
         contains = "SpatialPlateau"
         )


setClass("PlateauRegion",
         contains = "SpatialPlateau"
         )



