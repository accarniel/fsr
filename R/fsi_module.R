#' @title Create an empty fuzzy spatial inference model
#'
#' @description `fsi_create()` builds a fuzzy spatial inference (FSI) model without elements of the data source component (i.e., spatial plateau objects, fuzzy rules set, and fuzzy sets).
#'
#' @usage
#'
#' fsi_create(name, and_method = "min", or_method = "max",
#'            imp_method = "min", agg_method = "max", 
#'            defuzz_method = "centroid", default_conseq = NULL)
#'
#' @param name A character value that specifies the name of the FSI model.
#' @param and_method A character value that defines the operator for the logical connective AND. Default value is `"min"`.
#' @param or_method A character value that defines the operator for the logical connective OR. Default value is `"max"`.
#' @param imp_method A character value that defines the implication operator. Default value is `"min"`.
#' @param agg_method A character value that defines the aggregation operator. Default value is `"max"`.
#' @param defuzz_method A character value that determines the defuzzification technique. Default value is the centroid technique.
#' @param default_conseq A function object that corresponds to a membership function of the consequent.
#' 
#' @details
#' 
#' The `fsi_create()` function creates an empty FSI model and its default parameter values will implement a model using Mamdani's method.
#' 
#' The possible values for the parameters `and_method` and `imp_method` are: `"min"`, `"prod"`. The name of a user-defined t-norm function can also be informed here.
#' The possible value for the parameters `or_method` and `agg_method` is: `"max"`.  The name of a user-defined t-conorm function can also be informed here.
#' The possible values for the parameter `defuzz_method` are `"centroid"` (default value), `"bisector"`, `"mom"`, `"som"`, and `"lom"`.
#' The parameter `default_conseq` defines the default behavior of the FSI model when there is no fuzzy rule with a degree of fulfillment greater than 0 returned by the FSI model.
#' 
#' After creating an empty FSI model, you have to call the functions `fsi_add_fsa()`, `fsi_add_cs()`, and `fsi_add_rules()` to fulfill the FSI model with the needed information before performing inferences.
#' 
#' @return
#'
#' An empty named FSI model that is ready to be populated with data source component (i.e., spatial plateau objects, fuzzy rules set, and fuzzy sets).
#'
#' @references 
#' 
#' [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (AM SIGSPATIAL 2021), pp. 526-535, 2021.](https://dl.acm.org/doi/10.1145/3474717.3484255)
#' 
#' Underlying concepts and formal definitions of FSI models are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. Fuzzy inference on fuzzy spatial objects (FIFUS) for spatial decision support systems. In Proceedings of the 2017 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2017), pp. 1-6, 2017.](https://ieeexplore.ieee.org/document/8015707)
#'
#' @examples
#' trim_mf <- function(a, b, c) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), (c - x)/(c - b), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' # Creating the FSI model
#' fsi <- fsi_create("To visit or not to visit, that is the question", 
#'                   default_conseq = trim_mf(10, 30, 60)) 
#' @export
fsi_create <- function(name, and_method = "min",
                    or_method = "max", imp_method = "min", agg_method = "max",
                    defuzz_method = "centroid", default_conseq = NULL) {
  fsi <- list(name = name, type = "mamdani",
              and_method = and_method, or_method = or_method, imp_method = imp_method,
              agg_method = agg_method, defuzz_method = defuzz_method,
              fsa = NULL, cs = NULL, rule = NULL, default_conseq = default_conseq)
  fsi
}

#' @title Add an antecedent to an FSI model
#' 
#' @description `fsi_add_fsa()` adds a fuzzy spatial antecedent to a fuzzy spatial inference (FSI) model. 
#' A fuzzy spatial antecedent corresponds to a layer of fuzzy spatial objects (i.e., spatial plateau objects) that describe the different characteristics of the problem.
#' The antecedent has a linguistic variable and its fuzzy spatial objects have linguistic values so that they are used in the IF part of fuzzy rules.
#' 
#' @usage 
#' 
#' fsi_add_fsa(fsi, lvar, tbl)
#' 
#' @param fsi The FSI model instantiated with the `fsi_create()` function.
#' @param lvar A character value that represents a linguistic variable of the antecedent.
#' @param tbl A tibble with spatial plateau objects annotated with linguistic values of the linguistic variable specified by the above `lvar` parameter.
#' 
#' @details 
#' 
#' The `fsi_add_fsa()` function adds a fuzzy spatial antecedent composed of a linguistic variable and its corresponding `pgeometry` objects annotated by linguistic values. 
#' The format of `tbl` is the same as the output of the function `spa_creator()`, allowing users to directly provide plateau region objects as input when designing FSI models.
#' 
#' @returns 
#' 
#' An FSI model populated with a fuzzy spatial antecedent.
#' 
#' @references 
#' 
#' [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (AM SIGSPATIAL 2021), pp. 526-535, 2021.](https://dl.acm.org/doi/10.1145/3474717.3484255)
#' 
#' Underlying concepts and formal definitions of FSI models are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. Fuzzy inference on fuzzy spatial objects (FIFUS) for spatial decision support systems. In Proceedings of the 2017 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2017), pp. 1-6, 2017.](https://ieeexplore.ieee.org/document/8015707)
#'
#' @examples
#' library(tibble)
#' 
#' trap_mf <- function(a, b, c, d) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), 1, (d - x)/(d - c), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' trim_mf <- function(a, b, c) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), (c - x)/(c - b), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' # Creating spatial plateau objects for the linguistic variable "accommodation price"
#' lvals_accom_price <- c("cut-rate", "affordable", "expensive")
#' cut_rate_mf <- trap_mf(0, 0, 10, 48)
#' affordable_mf <- trap_mf(10, 48, 80, 115)
#' expensive_mf <- trap_mf(80, 115, 10000, 10000)
#' 
#' # Example of point dataset
#' accom_price <- tibble(longitude = c(-74.0, -74.0, -74.0), 
#'                       latitude = c(40.8, 40.75, 40.7),
#'                       price = c(150, 76, 60))
#'  
#' accom_price_layer <- spa_creator(accom_price, classes = lvals_accom_price, 
#'                          mfs = c(cut_rate_mf, affordable_mf, expensive_mf))
#'                          
#' # Creating the FSI model
#' fsi <- fsi_create("To visit or not to visit, that is the question", 
#'                   default_conseq = trim_mf(10, 30, 60))
#' 
#' # Adding the fuzzy spatial antecedent to the FSI model
#' fsi <- fsi_add_fsa(fsi, "accommodation price", accom_price_layer) 
#' @export
fsi_add_fsa <- function(fsi, lvar, tbl) {
  if(nrow(tbl) <= 0) {
    stop("The tibble with spatial plateau objects should contain at least one line", call. = FALSE)
  }

  # TODO validate the format of tbl
  pgeometry_layer <- tbl[c(1, 2)]
  colnames(pgeometry_layer) <- c("lval", "pgeometry")

  fsi$fsa <- append(fsi$fsa, list(list(name = lvar, layer = pgeometry_layer)))

  fsi
}

#' @title Add the consequent to an FSI model
#' 
#' @description `fsi_add_cs()` adds the consequent to a fuzzy spatial inference (FSI) model. It consists of a set of membership functions labeled with linguistic values.
#' 
#' @usage 
#' 
#' fsi_add_cs(fsi, lvar, lvals, mfs, bounds)
#' 
#' @param fsi The FSI model instantiated with the `fsi_create()` function.
#' @param lvar A character value that represents a linguistic variable of the consequent.
#' @param lvals A character vector that contains linguistic values of the linguistic variable of the consequent.
#' @param mfs A vector of membership functions (see examples below).
#' @param bounds A numeric vector that represents the lower and upper bounds of the consequent domain. 
#' 
#' @details 
#' 
#' The `fsi_add_cs()` function adds the consequent to an FSI model. 
#' Each linguistic value defined in `lvals` has a corresponding membership function defined in `mfs`.
#' Thus, these two parameters must have the same length.
#' For instance, the first value of `lvals` defines the linguistic value of the first membership function in `mfs`.
#' In `bounds`, the lower and upper values correspond to the first and second parameter, respectively.
#' 
#' @returns 
#' 
#' An FSI model populated with a consequent.
#' 
#' @references 
#' 
#' [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (AM SIGSPATIAL 2021), pp. 526-535, 2021.](https://dl.acm.org/doi/10.1145/3474717.3484255)
#' 
#' Underlying concepts and formal definitions of FSI models are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. Fuzzy inference on fuzzy spatial objects (FIFUS) for spatial decision support systems. In Proceedings of the 2017 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2017), pp. 1-6, 2017.](https://ieeexplore.ieee.org/document/8015707)
#'
#' @examples 
#' # Defining two different types of membership functions
#' trap_mf <- function(a, b, c, d) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), 1, (d - x)/(d - c), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' trim_mf <- function(a, b, c) {
#'   function(x) {
#'     pmax(pmin((x - a)/(b - a), (c - x)/(c - b), na.rm = TRUE), 0)
#'   }
#' }
#' 
#' # Creating the FSI model
#' fsi <- fsi_create("To visit or not to visit, that is the question", 
#'                    default_conseq = trim_mf(10, 30, 60))
#' 
#' # Creating the vector with the linguistic values of the linguistic variable "visiting experience"
#' lvals_visiting_exp <- c("awful", "average", "great")
#' 
#' # Defining the membership function for each linguistic value
#' awful_mf <- trim_mf(0, 0, 20)
#' average_mf <- trim_mf(10, 30, 60)
#' great_mf <- trap_mf(40, 80, 100, 100)
#' 
#' # Adding the consequent to the FSI model
#' fsi <- fsi_add_cs(fsi, "visiting experience", lvals_visiting_exp,
#'                   c(awful_mf, average_mf, great_mf), c(0, 100))
#' @export
fsi_add_cs <- function(fsi, lvar, lvals, mfs, bounds) {
  if(length(lvals) != length(mfs)) {
    stop("The length of the linguistic values (lvals) and membership functions (mfs) should be equal", call. = FALSE)
  }
  fsi$cs <- append(fsi$cs, list(list(name = lvar, lvals = lvals, range = bounds, mfs = mfs)))

  fsi
}

#' Gets each part of the antecedent from a fuzzy rule specified by the user.
#'
#' This function extracts the linguistic variable and its value from an input rule.
#' The rule must be in the following pattern:
#' IF linguistic variable is linguistic value LOGICAL OPERATOR linguistic variable is linguistic value THEN linguistic variable is linguistic value
#' Example:
#' IF hotel is affordable AND attraction is free THEN visiting is accessible
#' Pay attention that there is no punctuation in the rule.
#' The rule can use only one type of logical operator at a time (e.g. parts of the antecedents connected either by AND or by OR).
#' A rule can have one or more parts of antecedents.
#'
#' @param user_rule Fuzzy rule specified by the user
#' @param logical_op_ant Logical operator of the antecedent. Default value is NULL (there is only one antecedent)
#' @return A list of linguistic variables and their values from the antecedent of the rule
#' 
#' @import stringr
#' @noRd
get_antecedents <- function(user_rule) {
  us_rule <- str_to_lower(user_rule)
  us_rule <- str_replace_all(us_rule, "[\r\n]" , " ")
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

#' @import stringr
#' @noRd
get_consequent <- function(user_rule) {
  us_rule <- str_to_lower(user_rule)
  us_rule <- str_replace_all(us_rule, "[\r\n]" , " ")
  consequent <- str_extract(us_rule, "(?<=then )(.*\n?)")
  conseq <- vector("list", length = 1)
  tmp_res <- str_split_fixed(consequent, " is ", 2)
  cons1 <- c(tmp_res[1][1], tmp_res[2][1])
  conseq[[1]] <- str_trim(cons1)
  return(conseq)
}

#' @title Add fuzzy rules to an FSI model
#' 
#' @description `fsi_add_rules()` adds the fuzzy rules set to a fuzzy spatial inference (FSI) model. 
#' A fuzzy rule must contain only linguistic variables and values included in the antecedent parts and consequent.
#' 
#' @usage 
#' 
#' fsi_add_rules(fsi, rules, weights = rep(1, length(rules)))
#' 
#' @param fsi An FSI model instantiated with the `fsi_create()` function.
#' @param rules A character vector containing the rules defined by the user. It follows a specific format, as detailed below.
#' @param weights A numeric vector of weight values for each rule. Default values are 1.
#' 
#' @details 
#' 
#' The `fsi_add_rules()` function adds fuzzy rules to an FSI model. 
#' The definition of a fuzzy rule is user-friendly since users can write it by using the _linguistic variables_ and _linguistic values_ previously defined and added to the FSI model (via `fsi_add_fsa()` and `fsi_add_cs()`). 
#' 
#' A fuzzy rule has the format `IF A THEN B`, where `A` is called the antecedent and `B` the consequent of the rule such that `A` implies `B`. 
#' Further, `A` and `B` are statements that combine fuzzy propositions by using logical connectives like `AND` or `OR`. 
#' Each fuzzy proposition has the format `LVar is LVal` where `LVal` is a linguistic value in the scope of the linguistic variable `LVar`.
#' 
#' To avoid possible contradictions keep in mind the following items when specifying the rules:
#' - the order of the statements in the antecedent is not relevant.
#' - each linguistic variable has to appear at most one time in each fuzzy rule.
#' - only one kind of logical connective (i.e., `AND` or `OR`) must be used in the statements of the antecedent.
#' 
#' @return 
#' 
#' An FSI model populated with a fuzzy rules set.
#' 
#' @references 
#' 
#' [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (AM SIGSPATIAL 2021), pp. 526-535, 2021.](https://dl.acm.org/doi/10.1145/3474717.3484255)
#' 
#' Underlying concepts and formal definitions of FSI models are introduced in:
#' 
#' - [Carniel, A. C.; Schneider, M. Fuzzy inference on fuzzy spatial objects (FIFUS) for spatial decision support systems. In Proceedings of the 2017 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2017), pp. 1-6, 2017.](https://ieeexplore.ieee.org/document/8015707)
#'
#' @examples 
#' # Creating the FSI model from an example
#' fsi <- visitation()
#'
#' # Creating a vector of fuzzy rules
#' ## note that we make use of the linguistic variables and linguistic values previously defined
#' rules <- c(
#'  "IF accommodation review is reasonable AND 
#'     food safety is low 
#'   THEN visiting experience is awful",
#'  "IF accommodation price is expensive AND 
#'     accommodation review is reasonable 
#'   THEN visiting experience is awful",
#'  "IF accommodation price is affordable AND 
#'     accommodation review is good AND 
#'     food safety is medium 
#'   THEN visiting experience is average",
#'  "IF accommodation price is affordable AND 
#'     accommodation review is excellent AND 
#'     food safety is high 
#'   THEN visiting experience is great",
#'  "IF accommodation price is cut-rate AND 
#'     accommodation review is excellent AND 
#'     food safety is high 
#'   THEN visiting experience is great")
#' 
#' # Adding these rules to the FSI model previously instantiated
#' fsi <- fsi_add_rules(fsi, rules)
#' @export
fsi_add_rules <- function(fsi, rules, weights = rep(1, length(rules))) {
  if (length(rules) != length(weights)) {
    stop("The length of parameters for rules and weights have to be equal", call. = FALSE)
  }
  i <- 1
  for(ur in rules) {
    #TODO improve performance
    antecedents <- get_antecedents(ur)
    consequents <- get_consequent(ur)
    fsi$rule <- append(fsi$rule,
                       list(list(ant = antecedents$ants, cons = consequents, w = weights[i], connective = antecedents$op)))
    i <- i + 1
  }
  fsi
}

#' @title Evaluate a point inference query
#' 
#' @description `fsi_eval()` evaluates a point inference query. 
#' Considering an FSI model, it answers the following question: what is the inferred value for a given single point location? 
#' 
#' @usage 
#' 
#' fsi_eval(fsi, point, ...)
#' 
#' @param fsi An FSI model built with the `fsi_create()` and populated by `fsi_add_fsa()`, `fsi_add_cs()`, and `fsi_add_rules()`.
#' @param point An `sfg` object of the type `POINT`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Informs the `fsi_eval` how the elements of the resulting fuzzy set should be discretized if the user does not want the default configuration (see below). Default values: `discret_by` is 0.5 and `discret_length` is `NULL`.
#'
#' @details 
#' 
#' The `fsi_eval()` function evaluates a point inference query by using an FSI model populated with its fuzzy spatial antecedent, consequent, and fuzzy rules set. 
#' This evaluation is based on the algorithm specified by the references below. 
#'
#' The default behavior of `fsi_eval()` in the parameter `...` is to consider a discrete interval of values with an increment of 0.5 between lower and upper values for the consequent domain (i.e., defined by `fsi_add_cs()` with the parameter `bounds`).
#' 
#' The user can modify the default behavior by using one of the following two ways:
#' - define a value for the parameter `discret_by` by changing the incremental value.
#' - define a desired length for the sequence of values domain of the consequent by using the parameter `discret_length`.
#’ This means that it has the same behavior as the function `seq()`.
#' 
#' @return 
#' 
#' A numeric value that belongs to the domain of the consequent of the FSI model and represents the result of a point inference query
#' 
#' @references
#' 
#' [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (AM SIGSPATIAL 2021), pp. 526-535, 2021.](https://dl.acm.org/doi/10.1145/3474717.3484255)
#' 
#' Underlying concepts and definitions on the evaluation of point inference queries are introduced in:
#' 
#' - [Carniel, A. C.; Galdino, F.; Schneider, M. Evaluating Region Inference Methods by Using Fuzzy Spatial Inference Models. In Proceedings of the 2022 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2022), pp. 1-8, 2022.](https://ieeexplore.ieee.org/document/9882658)
#' - [Carniel, A. C.; Schneider, M. Fuzzy inference on fuzzy spatial objects (FIFUS) for spatial decision support systems. In Proceedings of the 2017 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2017), pp. 1-6, 2017.](https://ieeexplore.ieee.org/document/8015707)
#'
#' @examples 
#' library(sf)
#' 
#' # Creating the FSI model from an example
#' fsi <- visitation()
#'
#' # Creating a vector of fuzzy rules
#' ## note that we make use of the linguistic variables and linguistic values previously defined
#' rules <- c(
#'  "IF accommodation review is reasonable AND 
#'     food safety is low 
#'   THEN visiting experience is awful",
#'  "IF accommodation price is expensive AND 
#'     accommodation review is reasonable 
#'   THEN visiting experience is awful",
#'  "IF accommodation price is affordable AND 
#'     accommodation review is good AND 
#'     food safety is medium 
#'   THEN visiting experience is average",
#'  "IF accommodation price is affordable AND 
#'     accommodation review is excellent AND 
#'     food safety is high 
#'   THEN visiting experience is great",
#'  "IF accommodation price is cut-rate AND 
#'     accommodation review is excellent AND 
#'     food safety is high 
#'   THEN visiting experience is great")
#' 
#' # Adding these rules to the FSI model previously instantiated
#' fsi <- fsi_add_rules(fsi, rules)
#' 
#' # Evaluating a point inference query
#' fsi_eval(fsi, st_point(c(-74.0, 40.7)))
#' \dontrun{
#' # Changing the default discretization
#' fsi_eval(fsi, st_point(c(-74.0, 40.7)), discret_by = 0.8)
#' fsi_eval(fsi, st_point(c(-74.0, 40.7)), discret_length = 200)
#' }
#' @import tibble
#' @export
fsi_eval <- function(fsi, point, ...) {
  discret_by <- 0.5
  discret_length <- NULL

  if(any(is.na(point))){
    stop("'point' should not be NA", call. = FALSE)
  }
  if(class(point)[[2]] != "POINT"){
    stop("'point' must be a simple point object", call. = FALSE)
  }

  args_function <- list(...)
  if(length(args_function) > 0){
    if(!is.null(args_function[["discret_by"]])){
      discret_by <- args_function[["discret_by"]]
    }
    if(!is.null(args_function[["discret_length"]])){
      discret_length <- args_function[["discret_length"]]
    }
  }

  # First step: store the degree to which point belongs to each part of the FSA
  tbl_eval <- tibble(lvar = character(), lval = character(), degree = numeric())
  for(input in fsi$fsa) {
    input_layer <- input$layer
    for(i in 1:nrow(input_layer)) {
      row <- input_layer[i, ]
      tbl_eval <- add_row(tbl_eval, lvar = input$name, lval = row$lval, degree = spa_eval(row$pgeometry[[1]], point))
    }
  }

  fire_rules <- tibble(rule_index = integer(), degree = numeric(), consequent = list())

  # Second step: for each rule, we compute its firing rule strength
  i <- 1
  for(rule in fsi$rule) {
    degrees <- numeric()

    for(ant_part in rule$ant) {
      #the first position in ant_part is the lvar, the second position is its lval
      degrees <- append(degrees,
                        tbl_eval[tbl_eval$lvar == ant_part[1] & tbl_eval$lval == ant_part[2], ]$degree)
    }
    
    if(length(degrees) == 0) {
      degrees <- 0  
    }

    if(rule$connective == "AND") {
      connective_method <- match.fun(fsi$and_method)
    } else {
      connective_method <- match.fun(fsi$or_method)
    }
    fire_rules <- add_row(fire_rules, rule_index = i, 
                          degree = (connective_method(degrees) * rule$w), 
                          consequent = rule$cons)
    i <- i + 1
  }
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
        # TODO improve a little bit, we can discretize the values here too, instead of calling the tnorm
        mf_cut <- trap_mf(min_conseq, min_conseq, max_conseq, max_conseq, h = row$degree)
        # now we concatenate the previous mf with the mf_conseq
        res_imp <- function(x) {
          mfx <- sapply(list(mf_conseq, mf_cut), function(mf) {
            mf(x)
          })
          mfx <- matrix(mfx, nrow = length(x))
          apply(mfx, 1, imp_method)
        }
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
    result_fsi <- db(conseq_values)
  } else {
    result_fsi <- results_imp[[1]](conseq_values)
    if(length(results_imp) >= 2) {
      for(i in 2:length(results_imp)) {
        result_fsi <- agg_method(results_imp[[i]](conseq_values), result_fsi)
      }
      ## TODO improve this evaluation in terms of performance
    }
  }

  defuzz_method(conseq_values, result_fsi, fsi$defuzz_method)
}

#' Implements some defuzzification methods (based on https://www.mathworks.com/help/fuzzy/defuzzification-methods.html and FuzzyR package)
#' @noRd
defuzz_method <- function(x, mf, type) {
  if (type == "centroid") {
    if (sum(mf, na.rm = TRUE) != 0) {
      sum(mf * x, na.rm = TRUE)/sum(mf, na.rm = TRUE)
    } else {
      # we dont have membership degrees, so the centroid is the mean
      mean(x, na.rm = TRUE)
    }
  } else if (type == "bisector") {
    cs = cumsum(mf)
    a2 = sum(mf)/2
    xs = match(TRUE, cs > a2)
    x[xs - 1] + (a2 - cs[xs - 1])/mf[xs] + (x[xs] - x[xs - 1])/2
  } else if (type == "mom") {
    mean(x[which(mf == max(mf))])
  } else if (type == "som") {
    x[min(which(mf == max(mf)))]
  } else if (type == "lom") {
    x[max(which(mf == max(mf)))]
  } else {
    warning("Invalid defuzzification method, returning NA.", call. = FALSE)
    NA
  }
}

#' @import sf tibble
#' @noRd
fsi_qwi_discretization <- function(fsi, qw, k, n_col = NULL, n_row = NULL, ...) {
  if(!(is.null(n_col) && is.null(n_row))) {
    regular_grid_points <- st_make_grid(qw, n = c(n_row, n_col), what = "centers")
  } else {
    n <- as.integer(sqrt(k))
    regular_grid_points <- st_make_grid(qw, n = c(n, n), what = "centers")
  }
  qw_inference_grid_output <- numeric(length = length(regular_grid_points))

  i <- 1
  for(point in regular_grid_points) {
    qw_inference_grid_output[i] <- fsi_eval(fsi, point)
    i <- i + 1
  }

  tibble(points = regular_grid_points, inferred_values = qw_inference_grid_output)
}

#' @import sf pso tibble
#' @noRd
fsi_qwi_pso <- function(fsi, qw, target_mf, max_depth = 2, maxit = 50, population = 10, convergence = Inf,
                        what = "max", stats_output = FALSE) {
  pso.env <- new.env()
  pso.env$count_fitness_function <- 0
  pso.env$count_iteration <- 0

  fitness <- function(p){
    x <- p[1]
    y <- p[2]
    fsi_eval(fsi, st_point(c(x, y)))
  }

  fsi_quadrants_pso <- function(fsi, quadrant, maxit, population, convergence, target_mf,
                                current_depth, max_depth, result, what = "max") {
    if(max_depth == current_depth) {
      return(result)
    }

    subquadrants <- st_make_grid(quadrant, n = c(2, 2))

    if(what == "max") {
      scale = -1
    } else {
      scale = 1
    }

    for(q in subquadrants) {
      bbox <- st_bbox(q)

      pso_result <- psoptim(rep(NA,2), fn = fitness, lower = c(bbox$xmin, bbox$ymin), upper = c(bbox$xmax, bbox$ymax),
                            control = list(maxit = maxit, s = population, fnscale = scale, abstol = convergence * scale))
      pso.env$count_fitness_function <- pso.env$count_fitness_function + as.numeric(pso_result$counts[1])
      pso.env$count_iteration <- pso.env$count_iteration + as.numeric(pso_result$counts[2])

      if(target_mf(pso_result$value * scale) > 0) {
        result <- add_row(result, points = list(st_point(pso_result$par)), inferred_values = pso_result$value * scale)

        result <- fsi_quadrants_pso(fsi, q, maxit, population, convergence, target_mf,
                                    current_depth + 1, max_depth, result, what)
      }
    }
    result
  }

  pso_result_tbl <- tibble(points = list(), inferred_values = numeric())

  pso_result_tbl <- fsi_quadrants_pso(fsi, qw, maxit, population, convergence, target_mf,
                                      0, max_depth, pso_result_tbl, what = "max")
  pso_result_tbl$points <- st_as_sfc(pso_result_tbl$points)

  if(stats_output) {
    list(iterations = pso.env$count_iteration, fitness_calls = pso.env$count_fitness_function, result = pso_result_tbl)
  } else {
    pso_result_tbl
  }
}

#' @title Evaluate region inference methods
#' 
#' @description `fsi_qw_eval()` implements two methods for evaluating region inference (RI) queries: (i) Linguistic value-based RI query, and (ii) Optimal RI query.
#' The objective of these queries is to capture all points that intersect a search object (e.g., a query window) and 
#' whose inferred values fulfill some specific user requirements (e.g., the points with the maximum or minimum inferred values).
#' 
#' @usage
#'
#' fsi_qw_eval(fsi, qw, approach = "discretization", ...) 
#'
#' @param fsi An FSI model built with the `fsi_create()` function and populated by the functions `fsi_add_fsa()`, `fsi_add_cs()`, and `fsi_add_rules()`.
#' @param qw An `sfg` object representing the search object (e.g., a query window). It has to be an axis-aligned rectangle represented by a simple polygon object of 5 points (since the last coordinate pair closes the external ring of the rectangle).
#' @param approach Defines which approach is employed to perform the region inference: `"discretization"` or `"pso"`. Default value is `"discretization"`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Different set of parameters required depending on the chosen approach (see more in details below).
#'
#' @details 
#' 
#' The `fsi_qw_eval()` function evaluates two types of RI queries:
#' - _Linguistic value-based RI query_, which answers the following type of question: what are the points that intersect a given search object and have inferred values that belong to a target linguistic value?
#' - _Optimal RI query_, which answers the following type of question: what are the points that intersect a given search object and have the maximum (or minimum) inferred values?
#' 
#' `fsi_qw_eval()` offers two different methods to answer these questions: (i) _discretization_ method, and (ii) _optimization_ method. 
#' Comparative analyses (see reference below) indicate that the discretization method should be employed to process linguistic value-based RI queries, while 
#' the optimization method is more adequate for processing optimal RI queries. The details below describe how to use these methods.
#'
#' For the _discretization_ method, two additional parameters are needed and must be informed by using the three-dots parameter `...`: 
#' - `target_lval`: A character value that indicates the target linguistic value from the linguistic variable of the consequent.
#' - `k`: A numeric value that defines the number of points that will be captured from the query window and evaluated by `fsi_eval()`. 
#' Its square root has to an integer value. 
#' Alternatively, you can inform the number of columns and rows of the regular grid to be created on the query window by informing numeric values for `n_col` and `n_row`, respectively. 
#' Thus, these parameters can be given instead of the number `k`.
#' 
#' The _optimization_ method employs the particle swarm optimization (PSO) algorithm. Thus, the parameter `approach = "pso"` must be set together with the following parameters:
#' - `what`: A character value that defines the user's goal, which can be either **maximize** or **minimize** inferred values. 
#' Thus, this parameter can be either `"max"` or `"min"`. The default value is `"max"`.
#' - `max_depth`: A numeric value that refers to the number of times that the query window is divided into subquadrants. 
#' The default value is equal to 2. For instance, a `max_depth = 2` means that the query window will be split into four subquadrants, where the PSO will be applied to each one as its search space. 
#' 
#' In addition, the PSO algorithm has its own set of parameters:
#' - `maxit`: A numeric value that defines the maximum number of iterations. Default value is 50.
#' - `population`: A numeric value that defines the number of particles. Default value is 10.
#'
#' @return
#'
#' A tibble in the format `(points, inferred_values)`, where `points` is an `sfc` object and `inferred_values` are inferred values in the domain of the consequent of the FSI model.
#' 
#' @references
#' 
#' [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (AM SIGSPATIAL 2021), pp. 526-535, 2021.](https://dl.acm.org/doi/10.1145/3474717.3484255)
#' 
#' Underlying concepts and definitions on the evaluation of region inference methods are explained in:
#' 
#' - [Carniel, A. C.; Galdino, F.; Schneider, M. Evaluating Region Inference Methods by Using Fuzzy Spatial Inference Models. In Proceedings of the 2022 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2022), pp. 1-8, 2022.](https://ieeexplore.ieee.org/document/9882658)
#'
#' @examples 
#' library(sf)
#' 
#' # Creating the FSI model from an example
#' fsi <- visitation()
#'
#' # Creating a vector of fuzzy rules
#' ## note that we make use of the linguistic variables and linguistic values previously defined
#' rules <- c(
#'  "IF accommodation review is reasonable AND 
#'     food safety is low 
#'   THEN visiting experience is awful",
#'  "IF accommodation price is expensive AND 
#'     accommodation review is reasonable 
#'   THEN visiting experience is awful",
#'  "IF accommodation price is affordable AND 
#'     accommodation review is good AND 
#'     food safety is medium 
#'   THEN visiting experience is average",
#'  "IF accommodation price is affordable AND 
#'     accommodation review is excellent AND 
#'     food safety is high 
#'   THEN visiting experience is great",
#'  "IF accommodation price is cut-rate AND 
#'     accommodation review is excellent AND 
#'     food safety is high 
#'   THEN visiting experience is great")
#' 
#' # Adding these rules to the FSI model previously instantiated
#' fsi <- fsi_add_rules(fsi, rules)
#' 
#' # Defining the query window
#' pts_qw1 <- rbind(c(-73.92, 40.68527), c(-73.75, 40.68527), 
#'                  c(-73.75, 40.75), c(-73.92, 40.75), c(-73.92, 40.68527))
#' qw1 <- st_polygon(list(pts_qw1))
#' 
#' # Recall that our running example is based on a small set of point datasets
#' # This means that inferred values will likely be the same
#' 
#' \dontrun{
#' # Example using the discretization method
#' fsi_qw_eval(fsi, qw1, approach = "discretization", target_lval = "great", k = 25)
#'
#' # Example using the optimization method
#' fsi_qw_eval(fsi, qw1, approach = "pso", max_depth = 2)
#' }
#' @import dplyr
#' @importFrom rlang .data
#' @export
fsi_qw_eval <- function(fsi, qw, approach = "discretization", ...) {
  params <- list(...)
  result_qwi <- switch(approach,
                       discretization = {
                         qwi_discret <- do.call(fsi_qwi_discretization, c(list(fsi, qw), params))
                         target_lval <- params$target_lval
                         target_mf <- NULL
                         mf_pos <- match(target_lval, fsi$cs[[1]]$lvals)
                         if(!is.na(mf_pos) && mf_pos >= 1) {
                           target_mf <- fsi$cs[[1]]$mfs[[mf_pos]]
                         } else {
                           stop("Invalid target linguistic value.", call. = FALSE)
                         }
                         qwi_discret %>% filter(target_mf(.data$inferred_values) > 0)
                       },
                       pso = {
                         target <- params$what
                         target_mf <- NULL
                         if(is.null(target) || target == "max"){
                           last_index <- length(fsi$cs[[1]]$mfs)
                           target_mf <- fsi$cs[[1]]$mfs[[last_index]]
                         } else if (target == "min") {
                           target_mf <- fsi$cs[[1]]$mfs[[1]]
                         } else {
                           stop("Invalid value for the what parameter.", call. = FALSE)
                         }
                         do.call(fsi_qwi_pso, c(list(fsi, qw, target_mf), params))
                       },
                       stop("This query window inference approach is not valid.", call. = FALSE)
  )

  result_qwi
}


#' @title Visitation: An example of FSI model
#' 
#' @description `visitation()` provides an example, without rules, of a fuzzy spatial inference (FSI) model.
#' 
#' @usage 
#' 
#' visitation()
#'
#' @details 
#' 
#' The `visitation()` function provides a hypothetical FSI model that estimates the visiting experience based on prices and overall ratings of accommodations as well as sanitary conditions of restaurants. 
#' The output of such a model infers a value between 0 and 100 that indicates how attractive it is to visit a specific location. 
#' For this, the experience can be classified as _awful_, _average_, and _great_. 
#' 
#' The linguistic variables and their linguistic values of this FSI model are listed below:
#' - _accommodation price_ with _cut-rate_, _affordable_, and _expensive_ as linguistic values.
#' - _accommodation review_ with _bad_, _good_, and _excellent_ as linguistic values.
#' - _food safety_ with _low_, _medium_, and _high_ as linguistic values, which represent levels of sanitary conditions.
#' 
#' Note that this is just a small running example, containing a small set of points to represent the locations of accommodations and restaurants.
#' 
#' The usage of FSI models is subdivided into a _preparation phase_ and an _evaluation phase_. 
#' The preparation phase is responsible for instantiating a new FSI model with the elements of the data source component of FIFUS. 
#' For this, the `fsr` package provides the following functions: `fsi_create()`, `fsi_add_fsa()`, and `fsi_add_cs()`. 
#' These functions are employed by `visitation()` so that users can add their own fuzzy set rules (by using `fsi_add_rules()`) and perform the evaluation phase (by using the functions `fsi_eval()` and/or `fsi_qw_eval()`).
#' 
#' In this sense, `visitation()` performs the following internal actions to return an FSI model:
#' 1. specify the linguistic variables and their corresponding linguistic values, which are in turn represented by membership functions. These items are specified according to the context of the running example.
#' 2. define small point datasets that represent each linguistic variable. Such datasets are `tibble` objects.
#' 3. build spatial plateau objects by using `spa_creator()` on the datasets. As a result, we get spatial plateau objects that represent each linguistic value.
#' 4. create an FSI model with `fsi_create()` function.
#' 5. add fuzzy spatial antecedents with `fsi_add_fsa()`. Recall that the antecedents are spatial plateau objects previously built.
#' 6. define the linguistic variable and its linguistic values with membership functions for the consequent.
#' 7. add the consequent to the FSI model by using `fsi_add_cs()`.
#'
#' @return
#' 
#' An FSI model without fuzzy rules set.
#' 
#' @references
#' 
#' This function is based on the running example introduced in:
#' 
#' - [Carniel, A. C.; Galdino, F.; Philippsen, J. S.; Schneider, M. Handling Fuzzy Spatial Data in R Using the fsr Package. In Proceedings of the 29th International Conference on Advances in Geographic Information Systems (AM SIGSPATIAL 2021), pp. 526-535, 2021.](https://dl.acm.org/doi/10.1145/3474717.3484255)
#' 
#' Underlying concepts and formal definitions of FIFUS are discussed in:
#' 
#' - [Carniel, A. C.; Schneider, M. Fuzzy inference on fuzzy spatial objects (FIFUS) for spatial decision support systems. In Proceedings of the 2017 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE 2017), pp. 1-6, 2017.](https://ieeexplore.ieee.org/document/8015707)
#'
#' @examples 
#'
#' fsi <- visitation()
#'
#' @import sf tibble
#' @export
visitation <- function() {
  trim_mf <- function(a, b, c) {
    function(x) {
       pmax(pmin((x - a)/(b - a), (c - x)/(c - b), na.rm = TRUE), 0)
     }
  }
  
  # Define linguistic values and membership functions for each linguistic variable
  lvals_accom_price <- c("cut-rate", "affordable", "expensive")
  cut_rate_mf <- trap_mf(0, 0, 10, 48)
  affordable_mf <- trap_mf(10, 48, 80, 115)
  expensive_mf <- trap_mf(80, 115, 10000, 10000)
  
  lvals_accom_review <- c("cut-rate", "affordable", "expensive")
  reasonable_mf <- trap_mf(0, 0, 40, 65)
  good_mf <- trap_mf(40, 65, 80, 85)
  excellent_mf <- trap_mf(80, 85, 100, 100)
  
  lvals_food_safety <- c("cut-rate", "affordable", "expensive")
  low_mf <- trap_mf(24, 28, 115, 115)
  medium_mf <- trap_mf(10, 14, 24, 28)
  high_mf <- trap_mf(0, 0, 10, 14)
  
  # Example of datasets
  accom_price <- tibble(
    `longitude` = c(-74.0, -74.0, -74.0, -74.0, -74.0, -74.0, -74.0, -74.0, -73.9, -74.0), 
    `latitude` = c(40.8, 40.7, 40.7, 40.7, 40.8, 40.8, 40.7, 40.7, 40.8, 40.7),
    `price` = c(150, 76, 60, 175, 79, 75, 92, 109, 62, 196)
  )
  
  accom_review <- tibble(
    `longitude` = c(-74.0, -74.0, -74.0, -74.0, -74.0, -74.0, -74.0, -74.0, -74.0, -74.0), 
    `latitude` = c(40.8, 40.7, 40.7, 40.7, 40.8, 40.8, 40.7, 40.7, 40.8, 40.7),
    `review_scores_rating` = c(94, 89, 90, 97, 84, 98, 94, 91, 98, 97)
  )
  
  food_safety <- tibble(
    `longitude` = c( -74.01291, -73.99912, -73.84201, -73.98959, -73.84348, -73.98704, -73.93084, -73.91315, -73.94999, -73.96237), 
    `latitude` = c(40.70626, 40.72316, 40.75804, 40.76259, 40.84978, 40.75079, 40.67171, 40.70068, 40.77965, 40.77600),
    `score` = c(4, 9, 7, 9, 9, 24, 11, 11, 11, 13)
  )
  
  # Building the spatial plateau objects by using the previous datasets
  accom_price_layer <- spa_creator(accom_price, classes = lvals_accom_price, 
                                   mfs = c(cut_rate_mf, affordable_mf, expensive_mf))
  accom_review_layer <- spa_creator(accom_review, classes = lvals_accom_review, 
                                    mfs = c(reasonable_mf, good_mf, excellent_mf))
  food_safety_layer <- spa_creator(food_safety, classes = lvals_food_safety, 
                                   mfs = c(low_mf, medium_mf, high_mf))
  
  # Creating the FSI model with a name and a default consequent:
  fsi <- fsi_create("To visit or not to visit, that is the question", default_conseq = trim_mf(10, 30, 60))
  
  # Adding fuzzy spatial antecedents:
  fsi <- fsi_add_fsa(fsi, "accommodation price", accom_price_layer)
  fsi <- fsi_add_fsa(fsi, "accommodation review", accom_review_layer)
  fsi <- fsi_add_fsa(fsi, "food safety", food_safety_layer)
  
  # Defining the linguistic values for the linguistic variable `"visiting experience"`
  lvals_visiting_exp <- c("awful", "average", "great")
  
  # Specifying the membership functions for the linguistic values created on the previous step:
  awful_mf <- trim_mf(0, 0, 20)
  average_mf <- trim_mf(10, 30, 60)
  great_mf <- trap_mf(40, 80, 100, 100)
  
  # Adding the consequent:
  fsi <- fsi_add_cs(fsi, "visiting experience", lvals_visiting_exp, c(awful_mf, average_mf, great_mf), c(0, 100))
  
  fsi
}