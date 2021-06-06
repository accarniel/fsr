
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
fsi_create <- function(name, fsi_type = "mamdani", and_method = "min",
                    or_method = "max", imp_method = "min", agg_method = "max",
                    defuzz_method = "centroid", default_conseq=NULL) {
  fsi <- list(name = name, type = fsi_type,
              and_method = and_method, or_method = or_method, imp_method = imp_method,
              agg_method = agg_method, defuzz_method = defuzz_method,
              fsa = NULL, cs = NULL, rule = NULL, default_conseq = default_conseq)
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
fsi_add_fsa <- function(fsi, lvar, tbl) {
  if(nrow(tbl) <= 0) {
    stop("The tibble with spatial plateau objects should contain at least one line", call. = FALSE)
  }
  
  # TODO Juliana - validate the format of tbl
  pgeom_layer <- tbl[c(1, 2)]
  colnames(pgeom_layer) <- c("lval", "pgeom")
  
  fsi$fsa <- append(fsi$fsa, list(list(name = lvar, layer = pgeom_layer)))
  
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
fsi_add_cs <- function(fsi, lvar, lvals, mfs, bounds) {
  if(length(lvals) != length(mfs)) {
    stop("The length of the linguistic values (lvals) and membership functions (mfs) should be equal", call. = FALSE)
  }
  fsi$cs <- append(fsi$cs, list(list(name = lvar, lvals = lvals, range = bounds, mfs = mfs)))
  
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
#' @noRd
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
      ant[[i]] <- str_trim(a1)
    }
  } else if (str_count(antecedents, pattern = " or ") > 0) {
    lant <- "OR"
    qtd_or <- str_count(antecedents, pattern = " or ") + 1
    ant <- vector("list", length = qtd_or)
    tmpres <- str_split_fixed(antecedents, " or ", qtd_or)
    
    for (i in 1:qtd_or) {
      tmp_res <- str_split_fixed(tmpres[i], " is ", 2)
      a1 <- c(tmp_res[1][1], tmp_res[2][1])
      ant[[i]] <- str_trim(a1)
    }
  } else {
    lant <- "OR"
    ant <- vector("list", length = 1)
    tmp_res <- str_split_fixed(antecedents, " is ", 2)
    a1 <- c(tmp_res[1][1], tmp_res[2][1])
    ant[[1]] <- str_trim(a1)
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
  conseq[[1]] <- str_trim(cons1)
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
fsi_add_rules <- function(fsi, user_rules, weights = rep(1, length(user_rules))) {
  #TODO Juliana - check if the rules use valid linguistic variables and linguistic values only
  if (length(user_rules) != length(weights)) {
    stop("The lengths of parameters for user_rules and weights should be equal")
  }
  i <- 1
  for(ur in user_rules) {
    #TODO Juliana - improve performance
    antecedents <- get_antecedents(ur)
    consequents <- get_consequent(ur)
    fsi$rule <- append(fsi$rule, 
                       list(list(ant = antecedents$ants, cons = consequents, w = weights[i], connective = antecedents$op)))
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
fsi_eval <- function(fsi, point, discret_by = 0.5, discret_length = NULL) {
  
  if(any(is.na(point))){
    stop("'point' should not be NA", call. = FALSE)
  }
  if(class(point)[[2]] != "POINT"){
    stop("'point' must be a simple point object", call. = FALSE)
  }
  
  # First step: store the degree to which point belongs to each part of the FSA
  tbl_eval <- tibble(lvar = character(), lval = character(), degree = numeric())
  for(input in fsi$fsa) {
    input_layer <- input$layer
    for(i in 1:nrow(input_layer)) {
      row <- input_layer[i, ]
      tbl_eval <- add_row(tbl_eval, lvar = input$name, lval = row$lval, degree = spa_eval(row$pgeom[[1]], point))
    }
  }
  
  #show(tbl_eval)
  
  fire_rules <- tibble(rule_index = integer(), degree = numeric(), consequent = list())
  
  # Second step: for each rule, we compute its firing rule strength
  i <- 1
  for(rule in fsi$rule) {
    degrees <- numeric()
    
    for(ant_part in rule$ant) {
      #the first position in ant_part is the lvar, the second position is its lval
      degrees <- append(degrees, 
                        tbl_eval[tbl_eval[, 1] == ant_part[1] & tbl_eval[, 2] == ant_part[2], ]$degree)
    }
    
    #show(degrees)
    
    if(rule$connective == 'AND') {
      connective_method <- match.fun(fsi$and_method)
    } else {
      connective_method <- match.fun(fsi$or_method)
    }
    fire_rules <- add_row(fire_rules, rule_index = i, degree = (connective_method(degrees) * rule$w), consequent = rule$cons)
    
    i <- i + 1
  }
  
  #show(fire_rules)
  
  # Third step: compute the implication of each fired rule
  imp_method <- match.fun(fsi$imp_method)
  
  results_imp <- list()
  
  min_conseq <- fsi$cs[[1]]$range[1]
  max_conseq <- fsi$cs[[1]]$range[2]
  
  for(j in 1:nrow(fire_rules)) {
    row <- fire_rules[j, ]
    
    if(row$degree > 0) {
      consequent <- row$consequent[[1]]
      mf_pos <- match(consequent[2], fsi$cs[[1]]$lvals)
      
      if(!is.na(mf_pos) && mf_pos >= 1) {
        mf_conseq <- fsi$cs[[1]]$mfs[[mf_pos]]
        
        #implication here
        # TODO Juliana - improve a little bit, we can discretize the values here too, instead of calling the tnorm
        mf_cut <- genmf("trapmf", c(min_conseq, min_conseq, max_conseq, max_conseq, row$degree))
        res_imp <- fuzzy.tnorm(imp_method, mf_conseq, mf_cut)
        results_imp <- append(results_imp, res_imp)
      }
    }
  }
  
  # Fourth step: compute the aggregation
  
  conseq_values <- NULL
  if(!is.null(discret_by)) {
    conseq_values <- seq(min_conseq, max_conseq, by = discret_by)
  } else if(!is.null(discret_length)) {
    conseq_values <- seq(min_conseq, max_conseq, length.out = discret_length)
  } else {
    conseq_values <- seq(min_conseq, max_conseq)
  }
  
  agg_method <- NULL
  if(fsi$agg_method == "max") {
    agg_method <- match.fun("pmax")
  } else {
    agg_method <- match.fun(fsi$agg_method)
  }
  
  
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
        result_fsi <- agg_method(evalmf(conseq_values, results_imp[[i]]), result_fsi)
      }
      ## TODO Juliana - improve this evaluation
    }
  }
  
  defuzz(conseq_values, result_fsi, fsi$defuzz_method)
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



