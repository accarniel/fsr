#' An S4 Class for Spatial Plateau Component
#'
#' @slot obj A sf data type
#' @slot md A membership degree of the component
#' @import sf
setClass("component",
         slots = list(
           obj = "XY",
           md = "numeric"
         )
)


#' An S4 Class for a Spatial Plateau data type
#'
#' @slot component A length-one numeric vector
#' @slot supp A sfc object storing the union of all components
#' @slot type Plateau Geometry Type
setClass("pgeom",
         slots = list(
           component = "list",
           supp = "XY",
           type = "character"
         )
)


##### Funções Modificadoras (redefinem funções do R)




#' @title pgeom_to_pwkt
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
pgeom_to_pwkt <- function(pgeom) {

  if(pgeom_is_empty(pgeom)){
    return(paste0(pgeom@type, " EMPTY"))
  }

  component_to_text <- function(comp) {
    paste0("(", st_as_text(comp@obj), ", ", comp@md, ")")
  }

  l <- unlist(lapply(pgeom@component, component_to_text))

  l <- paste(pgeom@type," (", paste(l, collapse = ", "), ")", sep="")
  l
}



format.pgeom <- function(x, ...) {
  pgeom_to_pwkt(x)
}

setMethod("show", "pgeom", function(object) {
  print(pgeom_to_pwkt(object))
})

setMethod("as.character", "pgeom", function(x, ...) {
  pgeom_to_pwkt(x)
})


setMethod("plot", "pgeom", function(x, ...) {
  pgeom_plot(x, ...)
})


as.data.frame.pgeom <-
  function(x, row.names=NULL, optional=FALSE, ...)
  {
    as.data.frame(pgeom_as_tibble(x))
  }











