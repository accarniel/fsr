#' @title spa_add_component
#'
#' @description
#'
#' @usage
#' spa_add_component()
#' @param spo
#' @param component
#'
#' @details Implementation of \eqn{\alpha}
#'
#' @return
#'
#'
#' @references
#' Carniel, A. C.; Schneider, M.
#' Spatial Plateau Algebra: An Executable Type System for Fuzzy Spatial Data Types.
#' In Proceedings of the 2018 IEEE International Conference on Fuzzy Systems
#' (FUZZ-IEEE 2018), pp. 1-8, 2018. <https://doi.org/10.1109/FUZZ-IEEE.2018.8491565>
#'
#' @seealso
#'
#' @examples
#'
#' ### exemple 1...
#'
#' ### example 2...
#'
#' @export
#' @importFrom dplyr "%>%"

spa_add_component <- function(spo, component){

  if(is.null(spo)){
    stop("spo is null. Please use create_pgeom() to
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

  if(is.null(c) || st_is_empty(c) || m == 0){
    return(spo)
  }


  # 2. if po = �� ∧ c �= ∅ ∧m > 0
  # Se plateau_object E c diferente de vazio E m > 0
  else if(pgeom_is_empty(spo) && !(is.null(c) || st_is_empty(c)) && m > 0){
    spo@component[[1]] <- component
    spo@supp <- c
    return(spo)
  }




  else if(!is.null(c) & length(c) >= 1){
    index = search_by_md(spo@component, 1, length(spo@component), m)

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



#' @title spa_eval
#'
#' @description
#'
#'
#' @param comp
#'
#' @return
#' @examples
#'
#' @export
#'
spa_eval <- function(obj, point=NA){

  if(inherits(obj, "component")){
    md <- obj@md
    return(md)
  } else if(inherits(obj, "pgeom")){

    if(is.na(point)){
      stop("point is NA.")
    }

    if(class(point)[[2]]!="POINT"){
      stop("'point' must be a simple point object.")
    }

    if(st_intersects(point, obj@supp, sparse=FALSE)[1]){

      ################################
      # in_boundary ?
      md_comps <- c()
      for(component in obj@component){
        if(st_intersects(point, st_boundary(component@obj), sparse=FALSE)[1]){
          md_point <- get_md(component)
          md_comps <- append(md_comps, md_point)
        } # Se não estiver na borda, checar está no interior
        else if(st_intersects(point, component@obj, sparse=FALSE)[1]){
          md_point <- get_md(component)
          return(md_point)
        }
      }
      return(max(md_comps))
    }
    return(0)

  }


}





