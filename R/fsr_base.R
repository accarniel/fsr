#' An S4 Class for representing a component of a spatial plateau object
#'
#' @slot obj A sf data type
#' @slot md A membership degree of the component
#' 
#' @import methods sf
setClass("component",
         slots = list(
           obj = "XY",
           md = "numeric"
         )
)

#' An S4 Class for representing a spatial plateau object
#'
#' @slot component A list of components
#' @slot supp An sfg object that stores the union of spatial objects of the components of the spatial plateau object
#' @slot type The data type of the spatial plateau object
#' 
#' @import methods
setClass("pgeom",
         slots = list(
           component = "list",
           supp = "XY",
           type = "character"
         )
)

#' @import sf
#' @export
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

#' @export
format.pgeom <- function(x, ...) {
  pgeom_to_pwkt(x)
}

#' @import methods
#' @export
setMethod("show", "pgeom", function(object) {
  print(pgeom_to_pwkt(object))
})

#' @import methods
#' @export
setMethod("as.character", "pgeom", function(x, ...) {
  pgeom_to_pwkt(x)
})

#' @import methods
#' @export
setMethod("plot", signature(x = "pgeom", y = "missing"), function(x, y, ...) {
  pgeom_plot(x, ...)
})

#' @export
as.data.frame.pgeom <-
  function(x, row.names=NULL, optional=FALSE, ...)
  {
    as.data.frame(pgeom_as_tibble(x))
  }