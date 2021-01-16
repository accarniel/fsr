




### Check if a value is MULTIPOINT or POINT 


# For a unique Value (Component)
is_point <- function(component){
  if ((class(component)[2] == "POINT") || (class(component)[2] == "MULTIPOINT")){
    TRUE
  }
  else{
    FALSE
  }
}


# For a list of componenents 

is_point <- function(list_components){
  
}

#####################
#####################
#### LINE 
###################
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



