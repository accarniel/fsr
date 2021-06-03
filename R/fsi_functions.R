
#' @title fsi_create
#'
#' @description
#'
#'
#' @param name
#' @param fsi_type
#' @param and_method
#' @param or_method
#' @param default_conseq optional. If...MUST be a fuzzy set (something returned by genmf)
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
fsi_create <- function(name, fsi_type = "mamdani", and_method = "min",
                    or_method = "max", imp_method = "min", agg_method = "max",
                    defuzz_method = "centroid", default_conseq=NULL) {
  fsi <- list(name = name, type = fsi_type,
              and_method = and_method, or_method = or_method, imp_method = imp_method,
              agg_method = agg_method, defuzz_method = defuzz_method,
              fsa_names = NULL, cs_names = NULL,
              input = NULL, output = NULL, rule = NULL, default_conseq=default_conseq)
  fsi
}



#' @title fsi_add_fsa
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
fsi_add_fsa <- function(fsi, lvar, lvar_domain) {
  # fsi$input <- append(fsi$input, list(list(name = lvar,
  #                                          domain = lvar_domain, layer = NULL)))
  fsi$input <- append(fsi$input, list(list(name = lvar, domain = lvar_domain,
                                           layer = tibble(lval = character(), spo = list()))))
  fsi$fsa_names <- append(fsi$fsa_names, lvar)
  fsi
}




#' @title fsi_add_cs
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
fsi_add_cs <- function(fsi, lvar, lvar_domain, bounds) {
  fsi$output <- append(fsi$output, list(list(name = lvar, domain = lvar_domain,
                                             range = bounds, mf = NULL, defuzzification.method = NULL)))
  fsi$cs_names <- append(fsi$cs_names, lvar)
  fsi
}



#' @title fsi_add_pgeom_layers
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
fsi_add_pgeom_layers <- function(fsi, lvar, tbl) {

  lvar_index <- match(lvar, fsi$fsa_names)

  # TODO validar a var linguistica: verificar se o valor linguistico faz parte do domain do lvar.
  
  fsi$input[[lvar_index]]$layer <- tbl[c(1,2)]
  colnames(fsi$input[[lvar_index]]$layer) <- c("lval", "pgeom")

  fsi
}



#' @title fsi_add_pgeom_layer
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
fsi_add_pgeom_layer <- function(fsi, lvar, lval, spo) {
  
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



#' @title fsi_add_cs_mf
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
fsi_add_cs_mf <- function(fsi, lvar, lval, mf_type, mf_params) {
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



#' Get antecedent(s) from an input rule specified by the user.
#'
#' This function extracts the linguistic variable and its value from an input rule.
#' The rule must be in the following pattern:
#' IF linguistic variable is linguistic value LOGICAL OPERATOR linguistic variable is linguistic value THEN linguistic variable is linguistic value
#' Example:
#' IF hotel is affordable AND attraction is free THEN visiting is accessible
#' Pay attention that there is no punctuation in the rule.
#' The rule can use only one type of logical operator at a time (e.g. antecedents connected only by AND or only by OR).
#' A rule can have one or more antecedents.
#'
#' @param user_rule Input rule specified by the user
#' @param logical_op_ant Logical operator for antecedents. Default value is NULL (there is only one antecedent)
#' @return A list of linguistic variable and its value from the antecedent(s) of the rule
#' # GNEMF FONTE DA FUZZYR (ADD_SMF)
#' # doc do fsi_eval em details colocar os passos
#' @noRd
#' 
#'
get_antecedents <- function(user_rule) {
  us_rule <- str_to_lower(user_rule)
  antecedents <- str_extract(us_rule, "(?<=if )(.*\n?)(?=then)")
  
  if (str_count(antecedents, pattern = " and ") > 0) {
    lant <- "AND"
    
    qtd_and <- str_count(antecedents, pattern = " and ") + 1
    ant <- vector("list", length = qtd_and)
    tmpres <- str_split_fixed(antecedents, " and ", qtd_and)
    
    for (i in 1:qtd_and) {
      tmp_res <- str_split_fixed(tmpres[i], " is ", 2)
      a1 <- c(tmp_res[1][1], tmp_res[2][1])
      ant[[i]] <- a1
    }
  } else if (str_count(antecedents, pattern = " or ") > 0) {
    lant <- "OR"
    qtd_or <- str_count(antecedents, pattern = " or ") + 1
    ant <- vector("list", length = qtd_or)
    tmpres <- str_split_fixed(antecedents, " or ", qtd_or)
    
    for (i in 1:qtd_or) {
      tmp_res <- str_split_fixed(tmpres[i], " is ", 2)
      a1 <- c(tmp_res[1][1], tmp_res[2][1])
      ant[[i]] <- a1
    }
  } else {
    lant <- "OR"
    ant <- vector("list", length = 1)
    tmp_res <- str_split_fixed(antecedents, " is ", 2)
    a1 <- c(tmp_res[1][1], tmp_res[2][1])
    ant[[1]] <- a1
  }
  list(ants=ant, op=lant)
}


# add documentation
get_consequent <- function(user_rule) {
  us_rule <- str_to_lower(user_rule)
  consequent <- str_extract(us_rule, "(?<=then )(.*\n?)")
  conseq <- vector("list", length = 1)
  tmp_res <- str_split_fixed(consequent, " is ", 2)
  cons1 <- c(tmp_res[1][1], tmp_res[2][1])
  conseq[[1]] <- cons1
  return(conseq)
}



#' @title fsi_add_rules
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

fsi_add_rules <- function(fsi, user_rules, weights = rep(1, length(user_rules))) {
  #TODO depois do PI - validar se o usuário passou valores válidos (ou seja, que existem) - Juliana
  #TODO: improve performance
  if (length(user_rules) != length(weights)) {
    stop("The lengths of parameters for user_rules and weights are different")
  }
  i <- 1
  for(ur in user_rules) {
    antecedents <- get_antecedents(ur) #, logical_op_ant)
    consequents <- get_consequent(ur)
    fsi$rule <- append(fsi$rule, 
                       list(list(ants = antecedents$ants, cons = consequents, w = weights[i], loa = antecedents$op)))
    i <- i + 1
  }
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
fsi_eval <- function(fsi, p, discret_by = 0.5, discret_length = NULL) {
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
        # TODO change spo name at line 342: obj
        if(spa_eval(row$pgeom[[1]], p) > 0) {
          r <- add_row(r, lval = row$lval, spo = list(row$pgeom[[1]]))
        }
      }
    }
    
    all_spos <- append(all_spos,
                       list(list(lvar = input$name, objects = r)))
    
    #TODO como usar o filter com funções que não são vetorizadas
    #r <- filter(i$layer, spa_eval(spo, p) > 0)
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
      flag <- FALSE
      # TODO Juliana - melhorar pós PI
      for(layer in all_spos) {
        # cat(paste0("a camada sendo percorrida eh...", layer$lvar, "\n"))
        
        position <- match(lval_ant, layer$objects$lval)
        
        # cat(paste0("comparando... ", lvar_ant, " [", lval_ant, "] com ", layer$objects$lval[position], "\n"))
        
        #cat(paste0("evaluating the membership degree for ", layer$objects$lval[position], "\n"))
        
        if(layer$lvar == lvar_ant && !is.na(position) && position >= 1) {
          membership_degrees_antecedents <- append(membership_degrees_antecedents,
                                                   spa_eval(layer$objects$spo[position][[1]], p))
          
          # cat(paste0("Graus de pertinencia adicionando esse antecedente fica: ", membership_degrees_antecedents, "\n"))
          flag <- TRUE
          break
        } #else {
        #  membership_degrees_antecedents <- append(membership_degrees_antecedents,
        #                                           0)
        #}
        
        
      }
      if(!flag) {
        membership_degrees_antecedents <- append(membership_degrees_antecedents, 0)
        
      }
    }
    
    # cat("os graus de pertinencia dos antecedents sao...\n")
    # print(membership_degrees_antecedents)
    
    #loa = logical operator do antecedent
    if(all_rules[[pos_r]]$loa == 'AND') {
      and_method <- match.fun(fsi$and_method)
      fire_rules <- append(fire_rules, list(list(pos_rule = pos_r, fire_rule = and_method(membership_degrees_antecedents))))
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
  
  conseq_values <- NULL
  if(!is.null(discret_by)) {
    conseq_values <- seq(min_conseq, max_conseq, by = discret_by)
  } else if(!is.null(discret_length)) {
    conseq_values <- seq(min_conseq, max_conseq, length.out = discret_length)
  } else {
    conseq_values <- seq(min_conseq, max_conseq)
  }
  
  agg_method <- match.fun(fsi$agg_method)
  if(length(results_imp) == 0) {
    db <- fsi$default_conseq
    if(is.null(db)) {
      warning("No default rule defined")
      return(NA)
    }
    result_fsi <- evalmf(conseq_values, db)
  } else {
    result_fsi <- evalmf(conseq_values, results_imp[[1]])
    if(length(results_imp) >= 2) {
      for(i in 2:length(results_imp)) {
        result_fsi <- pmax(evalmf(conseq_values, results_imp[[i]]), result_fsi)
      }
      ## TODO improve latter (JULIANA)
    }
  }
  
  crisp_result <- defuzz(conseq_values, result_fsi, fsi$defuzz_method)
  
  crisp_result
}

#' @noRd
fsi_qwi_discretization <- function(fsi, qw, k, n_col = NULL, n_row = NULL) {
  if(!(is.null(n_col) && is.null(n_row))) {
    regular_grid_points <- st_make_grid(qw, n = c(n_row, n_col), what = "centers")
  } else {
    #TODO check if sqrt(k ) is an integer value
    regular_grid_points <- st_make_grid(qw, n = c(sqrt(k), sqrt(k)), what = "centers")
  }
  qw_inference_grid_output <- numeric(length = length(regular_grid_points))
  
  i <- 1
  for(point in regular_grid_points) {
    qw_inference_grid_output[i] <- fsi_eval(fsi, point)
    i <- i + 1
  }
  
  tibble(points = regular_grid_points, inferred_values = qw_inference_grid_output)
}


#' @export
fsi_qw_eval <- function(fsi, qw, target_lval, approach = "discretization", ...) {
  params <- list(...)
  result_qwi <- switch(approach,
                       discretization = do.call(fsi_qwi_discretization, c(list(fsi, qw), params)),
                       #include the PSO approach...
                       stop("This query window inference approach is not valid.")
  )
  # target_lval defines what should be returned
  # all, or a specific character vector of lvals from the consequent part
  # target_lval = "great".. 
  #   it should return the collection of points with their corresponding result that have some membership degree in the set "great"
  target_mf <- NULL
  for(part in fsi$output[[1]]$mf) {
    if(part$name == target_lval) {
      target_mf <- genmf(part$type, part$params)
    }
  }
  
  filter(result_qwi, target_mf(inferred_values) > 0)
}



