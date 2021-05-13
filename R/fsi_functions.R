
#' @title new_fsi
#'
#' @description
#'
#'
#' @param name
#' @param fsi_type
#' @param and_method
#' @param or_method
#' @imp_method
#' @agg_method
#' @defuzz_method
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
new_fsi <- function(name, fsi_type = "mamdani", and_method = "min",
                    or_method = "max", imp_method = "min", agg_method = "max",
                    defuzz_method = "centroid") {
  fsi <- list(name = name, type = fsi_type,
              and_method = and_method, or_method = or_method, imp_method = imp_method,
              agg_method = agg_method, defuzz_method = defuzz_method,
              fsa_names = NULL, cs_names = NULL,
              input = NULL, output = NULL, rule = NULL)
  fsi
}



#' @title add_fsa
#'
#' @description
#'
#'
#' @param fsi
#' @param p
#'
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
add_fsa <- function(fsi, lvar, lvar_domain) {
  # fsi$input <- append(fsi$input, list(list(name = lvar,
  #                                          domain = lvar_domain, layer = NULL)))
  fsi$input <- append(fsi$input, list(list(name = lvar, domain = lvar_domain,
                                           layer = tibble(lval = character(), spo = list()))))
  fsi$fsa_names <- append(fsi$fsa_names, lvar)
  fsi
}




#' @title add_cs
#'
#' @description
#'
#'
#' @param fsi
#' @param p
#'
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
add_cs <- function(fsi, lvar, lvar_domain, bounds) {
  fsi$output <- append(fsi$output, list(list(name = lvar, domain = lvar_domain,
                                             range = bounds, mf = NULL, defuzzification.method = NULL)))
  fsi$cs_names <- append(fsi$cs_names, lvar)
  fsi
}



#' @title add_pgeom_layer
#'
#' @description
#'
#'
#' @param fsi
#' @param p
#'
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
add_pgeom_layer <- function(fsi, lvar, lval, spo) {

  lvar_index <- match(lvar, fsi$fsa_names)

  if(length(fsi$input[[lvar_index]]$domain[fsi$input[[lvar_index]]$domain == lval]) < 1) {
    warning(paste0("lval (", lval, ") does not belong to the domain of the linguistic variable (", lvar, ")"))
    fsi
  }

  lval_index <- match(lval, fsi$input[[lvar_index]]$domain)


  #fsi$input[[lvar_index]]$layer <- append(fsi$input[[lvar_index]]$layer,
  #                                   list(list(spo = spo, lval = lval)))

  fsi$input[[lvar_index]]$layer <- add_row(fsi$input[[lvar_index]]$layer, lval = lval, spo = list(spo))

  fsi
}




#' @title add_cs_mf
#'
#' @description
#'
#'
#' @param fsi
#' @param p
#'
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
add_cs_mf <- function(fsi, lvar, lval, mf_type, mf_params) {
  lvar_index <- match(lvar, fsi$cs_names)

  if(length(fsi$output[[lvar_index]]$domain[fsi$output[[lvar_index]]$domain == lval]) < 1) {
    warning(paste0("lval ('", lval, "') does not belong to the domain of the linguistic variable ('", lvar, "')"))
    fsi
  }

  lval_index <- match(lval, fsi$output[[lvar_index]]$domain)

  fsi$output[[lvar_index]]$mf <- append(fsi$output[[lvar_index]]$mf,
                                        list(list(name = lval, type = mf_type, params = mf_params,
                                                  perturbation = NULL)))

  fsi
}




#' @title add_rule
#'
#' @description
#'
#'
#' @param fsi
#' @param p
#'
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
add_rule <- function(fsi, antecedents, consequents, weights = 1, logical_op_ant = "AND", logical_op_conseq = "AND") {
  #TODO depois do PI - validar se o usuário passou valores válidos (ou seja, que existem) - Juliana
  fsi$rule <- append(fsi$rule,
                     list(list(ants = antecedents, cons = consequents, w = weights, loa = logical_op_ant,
                               loc = logical_op_conseq)))
  fsi
}



#' @title fsi_eval
#'
#' @description
#'
#'
#' @param fsi
#' @param p
#'
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
fsi_eval <- function(fsi, p) {
 # pegar do PI
}


#' @title fsi_eval_pso
#'
#' @description
#'
#'
#' @param fsi
#' @param bbox
#' @param bbox
#'
#' @return
#' @examples
#'
#' @export
#' @importFrom
#'
fsi_eval_pso <- function(fsi, bbox, n_quads, ...){
  # O código do PI
}


