
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
  # 1 - The first step of the algorithm is to find the fuzzy regions
  # that contain the point P with some membership degree greater
  # than 0

  # this is a list of lists
  # Linguistic Variable [..., ..., ...]
  # texture [um tibble de objetos que contém o ponto p com algum grau de pertinência]
  all_spos <- list()

  for(input in fsi$input) {
    #print(input)

    #aqui seria a OTIMIZAÇÃO 1, ao invés de armazenar o spo, armazenar apenas seus graus de pertinência
    r <- tibble(lval = character(), spo = list())

    #layer é uma tabela!
    layer <- input$layer

    #aqui seria a OTIMIZAÇÃO 2 - fazer o tratamento adequado dependento do tipo de layer definido

    # print(layer)
    # print(nrow(layer))

    if(nrow(layer) > 0) {

      for(i in 1:nrow(layer)) {
        row <- layer[i, ]

        # print(row$lval)
        # print(row$spo[[1]])


        # ARRUMAR ISSO NA REUNIÃO!!!!!!!!!!!!!!
        # AQUI NA VERDADE NÃO PRECISA SABER O GRAU DE PERTINÊNCIA, MAS SIM SABER SE ELE SERÁ MAIOR QUE 0 OU NÃO
        # ASSIM, PODE DEIXAR ESSA PARTE MAIS OTIMIZADA
        if(check_md(row$spo[[1]], p) > 0) {
          r <- add_row(r, lval = row$lval, spo = list(row$spo[[1]]))
        }
      }
    }

    all_spos <- append(all_spos,
                       list(list(lvar = input$name, objects = r)))

    #TODO como usar o filter com funções que não são vetorizadas
    #r <- filter(i$layer, check_md(spo, p) > 0)
  }

  # The next step computes the Cartesian product on the values
  # of the arrays stored in N to find all combinations of the
  # linguistic values of the stored fuzzy regions
  # this results in a matrix called K

  # Each row of the K indicates a fuzzy rule to be executed by
  # the fuzzy inference method IM

  #aqui seria a OTIMIZAÇÃO 3: evitar a varredura sequencial de todas as regras

  # Compute the degree of fulfillment of each rule (fire rule)
  fire_rules <- list()

  all_rules <- fsi$rule

  for(pos_r in 1:length(all_rules)) {
    #checando se as variaveis linguisticas e valores linguisticas do all_spos
    #tem um match com os antecedentes de cada regra

    antecedents <- all_rules[[pos_r]]$ants

    # cat(paste0("checando a regra com antecedentes...", antecedents, "\n"))

    membership_degrees_antecedents <- numeric()

    for(ant in antecedents) {
      lvar_ant <- ant[1]
      lval_ant <- ant[2]
      entrou <- FALSE
      # TODO Juliana - melhorar pós PI
      for(layer in all_spos) {
        # cat(paste0("a camada sendo percorrida eh...", layer$lvar, "\n"))

        position <- match(lval_ant, layer$objects$lval)

        # cat(paste0("comparando... ", lvar_ant, " [", lval_ant, "] com ", layer$objects$lval[position], "\n"))

        #cat(paste0("evaluating the membership degree for ", layer$objects$lval[position], "\n"))

        if(layer$lvar == lvar_ant && !is.na(position) && position >= 1) {
          membership_degrees_antecedents <- append(membership_degrees_antecedents,
                                                   check_md(layer$objects$spo[position][[1]], p))

          # cat(paste0("Graus de pertinencia adicionando esse antecedente fica: ", membership_degrees_antecedents, "\n"))
          entrou <- TRUE
          break
        } #else {
        #  membership_degrees_antecedents <- append(membership_degrees_antecedents,
        #                                           0)
        #}


      }
      if(!entrou) {
        membership_degrees_antecedents <- append(membership_degrees_antecedents, 0)

      }
    }

    # cat("os graus de pertinencia dos antecedents sao...\n")
    # print(membership_degrees_antecedents)

    #loa = logical operator do antecedent
    if(all_rules[[pos_r]]$loa == 'AND') {
      and_method <- match.fun(fsi$and_method)
      # fire_rules <- append(fire_rules, list(list(pos_rule = pos_r, fire_rule = and_method(membership_degrees_antecedents))))
      fire_rules <- append(fire_rules, list(list(pos_rule = pos_r, fire_rule = max(membership_degrees_antecedents))))
    } else {
      or_method <- match.fun(fsi$or_method)
      fire_rules <- append(fire_rules, list(list(pos_rule = pos_r, fire_rule = or_method(membership_degrees_antecedents))))
    }
  }

  # cat("os disparos das regras sao:\n")
  # print(fire_rules)

  # Compute the implication
  imp_method <- match.fun(fsi$imp_method)

  results_imp <- list()

  min_conseq <- fsi$output[[1]]$range[1]
  max_conseq <- fsi$output[[1]]$range[2]

  # cat(paste0(min_conseq, "-", max_conseq, " como range do consequente\n"))

  for(fr in fire_rules) {
    # cat("a regra sendo percorrida eh:\n")
    # print(fr)

    fire_rule <- fr$fire_rule

    if(fire_rule > 0) {
      which_rule <- fr$pos_rule
      consequent <- fsi$rule[[which_rule]]$cons

      # cat(paste0(fsi$rule[[which_rule]], " com grau de disparo ", fire_rule, "\n"))

      mf_pos <- match(consequent[[1]][2], fsi$output[[1]]$domain)

      if(!is.na(mf_pos) && mf_pos >= 1) {
        mf_conseq <- fsi$output[[1]]$mf[[mf_pos]]

        #### resultado da implicacao
        mf_fuzzyr <- genmf(mf_conseq$type, mf_conseq$params)

        mf_cut <- genmf("trapmf", c(min_conseq, min_conseq, max_conseq, max_conseq, fire_rule))

        res_imp <- fuzzy.tnorm(imp_method, mf_fuzzyr, mf_cut)
        results_imp <- append(results_imp, res_imp)
      }
    }
  }

  ## 2 - agregacao de diversos conjuntos fuzzy
  agg_method <- match.fun(fsi$agg_method)
  if(length(results_imp) == 0) {
    return(0) # mudar depois para NA
  }
  result_fsi <- evalmf(seq(min_conseq, max_conseq, by=1), results_imp[[1]])
  if(length(results_imp) >= 2) {
    for(i in 2:length(results_imp)) {
      result_fsi <- pmax(evalmf(seq(min_conseq, max_conseq, by=1), results_imp[[i]]), result_fsi)
    }

    ## TODO improve latter (JULIANA)
  }


  # result_fsi <- results_imp[[1]]
  # if(length(results_imp) > 1) {
  #
  #   ## TODO improve latter (JULIANA)
  #
  #   result_fsi <- fuzzy.tconorm(agg_method, results_imp[[1]], results_imp[[2]])
  #
  #   for(i in 3:length(results_imp)) {
  #     result_fsi <- fuzzy.tconorm(agg_method, result_fsi, results_imp[[i]])
  #   }
  # }
  # cat("defuzificando... \n")
  ## 3 - defuzzificacao
  # crisp_result <- defuzz(seq(min_conseq, max_conseq, by=1), evalmf(seq(min_conseq, max_conseq, by=1), result_fsi), fsi$defuzz_method)
  crisp_result <- defuzz(seq(min_conseq, max_conseq, by=1), result_fsi, fsi$defuzz_method)

  crisp_result
  #result_fsi
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


