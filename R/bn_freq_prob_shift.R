#' bn_freq_prob_shift
#' @description bn_freq_prob_shift
#' @param freq named count table
#' @export
bn_freq_prob_shift <- function(
    freq = NULL,
    type = c("exponential", "linear", "quadratic"),
    lift = 0.1,
    return_actual_lift = FALSE,
    p_orig = NULL, values = NULL
){


  type <- match.arg(type)


  if(!is.null(freq)){
    values <- freq %>% names() %>% as.numeric() %>% setNames(NULL)
    counts <- freq %>% as.numeric() %>% setNames(NULL)
  }


  # Original probabilities
  p_orig <- counts / sum(counts)


  # Original and target means
  orig_mean <- sum(values * p_orig)
  target_mean <- orig_mean * (1 + lift)


  ##########################
  # Differt shift engines
  ##########################

  normalize <- function(x){x / sum(x)}

  quadratic_shift_type <- function(p_orig, values, target_mean){
    # Quadratic programming setup
    Dmat <- diag(length(p_orig))  # Minimize sum((p_new - p_orig)^2)
    dvec <- p_orig

    # Equality constraints: sum(p_new) = 1 and sum(v * p_new) = target_mean
    Amat <- cbind(rep(1,length(p_orig)), values)  # Each column is a constraint
    bvec <- c(1, target_mean)

    # Inequality constraints p_i >= 0 (quadprog requires Amat %*% x >= bvec)
    # To fit into quadprog, we need to reformulate constraints:
    # Solve using a more flexible package: CVXR (simpler for inequality)

    p_new <- CVXR::Variable(length(p_orig))
    objective <- CVXR::sum_squares(p_new - p_orig)
    constraints <- list(
      sum(p_new) == 1,
      sum(values * p_new) == target_mean,
      p_new >= 1e-6
    )

    prob <- CVXR::Problem(CVXR::Minimize(objective), constraints)
    result <- CVXR::solve(prob)

    p_new_val <- result$getValue(p_new) %>% as.numeric() %>% setNames(values)

    p_new_val
  }


  exponential_shift_type <- function(p_orig, values, target_mean){

    # Function to compute mean after tilting
    tilted_mean <- function(lambda) {
      w <- p_orig * exp(lambda * values)
      w <- w / sum(w)
      sum(values * w)
    }

    # Solve for lambda
    lambda <- uniroot(
      function(l) tilted_mean(l) - target_mean,
      interval = c(-10, 10)
    )$root

    # New probabilities
    p_new <- p_orig * exp(lambda * values)
    p_new <- p_new %>% normalize()

    p_new
  }



  linear_shift_type <- function(p_orig, values, target_mean){

    # Solve for alpha
    num <- (sum(values * p_orig) - target_mean)
    den <- (target_mean * sum(values * p_orig) - sum(values^2 * p_orig))
    alpha <- num / den

    # New probabilities
    p_new <- p_orig * (1 + alpha * values)
    p_new <- p_new %>% normalize()

    p_new
  }



  if(type == "quadratic"){
    p_new_val <- quadratic_shift_type(p_orig = p_orig, values = values, target_mean = target_mean)
  }else if(type == "exponential" || type == "log" || type == "logistic"){
    p_new_val <- exponential_shift_type(p_orig = p_orig, values = values, target_mean = target_mean)
  }else if(type == "linear"){
    p_new_val <- linear_shift_type(p_orig = p_orig, values = values, target_mean = target_mean)
  }


  if(head(p_new_val, 1) < 1e-6){
    p_new_val <- p_new_val + abs(head(p_new_val, 1)) + 1e-6
  }

  p_new <- p_new %>% normalize()

  new_mean <- sum(values * p_new_val)

  ##########################
  # Return
  ##########################

  if(return_actual_lift){

    return(
      list(
        p_new_val = p_new_val,
        p_orig_val = p_orig,
        mean_orig = orig_mean,
        mean_new = new_mean,
        mean_target = target_mean,
        mean_shift = new_mean - orig_mean,
        lift_target = lift,
        lift_actual = (new_mean / orig_mean) - 1
      )
    )

  }else{

    return(p_new_val)

  }

}


# freq %>% bn_freq_shift_quadratic(return_actual_lift = F)
# freq %>% bn_freq_shift_quadratic(return_actual_lift = F, type = "exponential")
# freq %>% bn_freq_shift_quadratic(return_actual_lift = F, type = "linear")
#
# bn_freq_shift_quadratic(freq)

