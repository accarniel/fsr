




### Check if a value is MULTIPOINT or POINT 


# For a unique Value (Component)

### RECEBER COMPONENTE E O TIPO DE DADO DE INPUT DO USUARIO ( QUE ELE DESEJA)
### VALIDAR SE O INPUT é CORRETO.
# 1. Checar na lista, item a item, o tipo de dado (criar a função)
check_plateau <- function(component, type){
  #type tolower
  
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
      if(type=="LINE"){
        TRUE
      } else{
        FALSE
      }
    }
    else if(component_type == "POLYGON" || component_type == "MULTIPOLYGON"){
      if(type=="POLYGON"){
        TRUE
      } else{
        FALSE
      }
    }} else{
      stop("Component is not a sf data type")
  }     
}









is_point <- function(component){
  if ((class(component)[2] == "POINT" || class(component)[2] == "MULTIPOINT")){
    TRUE
  }
  else{
    FALSE
  }
}



#####################
#####################
#### LINE ##########
####################
####################



is_line <- function(component){
  if ((class(component)[2]  == "MULTILINE") || (class(component)[2] =="LINESTRING")){
    TRUE
  }
  else{
    FALSE
  }
}


is_point <- function(component){
  if (class(component)[2] == "MULTIPOINT" || "LINE"){
    TRUE
  }
  else{
    FALSE
  }
}




#####################
#####################
#### REGION 
###################
####################



is_region <- function(component){
  if ((class(component)[2]  == "POLYGON") || (class(component)[2] =="MULTIPOLYGON")){
    TRUE
  }
  else{
    FALSE
  }
}




##########


spo_is_empty <- function(spo){
  
}









