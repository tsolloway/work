#' Bootstrap Bayesian Network Strength with Directional Alignment
#'
#' @description
#' Performs bootstrap estimation of Bayesian network arc strengths using
#' `bnlearn::boot.strength()`, with optional directional alignment
#' (flipping arcs to match majority direction), thresholding, and filtering
#' of specific arcs or their reverses.
#'
#' @param df A data frame containing the variables to fit the network.
#' @param bootstrap_reps Integer. Number of bootstrap replicates (default = 100).
#' @param algorithm Character. Structure learning algorithm to use: `"tabu"`, `"hc"`, or `"tree.bayes"`.
#' @param score Character. Scoring function for structure learning: `"bic"` or `"aic"`.
#' @param maxp Integer. Maximum number of parents per node (used by `"hc"` and `"tabu"` algorithms).
#' @param black_list Optional data frame of arcs to blacklist (exclude) during structure learning.
#' @param white_list Optional data frame of arcs to whitelist (force include) during structure learning.
#' @param set_dif_arcs_df Optional data frame of arcs (and their reverses) to exclude from the final bootstrap output.
#' @param evaluate_only_arcs_df Optional data frame of arcs (and their reverses) to specifically evaluate from the bootstrap results.
#' @param align_direction Logical; if `TRUE`, arcs are oriented to match the majority-supported direction.
#' @param threshold Numeric or `"auto"`. Minimum strength to retain arcs, or `"auto"` to select top arcs based on `auto_threshold_ratio`.
#' @param auto_threshold_ratio Numeric. Proportion of arcs to retain if `threshold = "auto"` (default = 1/4).
#' @param strength_min Numeric. Minimum strength below which arcs are removed (default = 0.01).
#' @param return_direction Logical. If `TRUE`, includes the `direction` column in the returned data.
#' @param seed Optional integer for reproducibility. If NULL, no seed is set.
#'
#' @return A data frame of arcs with columns:
#' - `from`: source node
#' - `to`: target node
#' - `strength`: bootstrap-estimated strength of the arc
#' - `direction`: proportion of times the arc points in this direction (if `return_direction = TRUE`)
#'
#' @examples
#' \dontrun{
#' bn_boot_strength(df = my_data, bootstrap_reps = 200, algorithm = "tabu",
#'                  threshold = 0.6, align_direction = TRUE)
#' }
#'
#' @export
bn_boot_strength <- function(
    df,
    bootstrap_reps = 100,
    algorithm = c("tabu", "hc", "tree.bayes"),
    score = c("bic", "aic"),
    maxp = 1,
    black_list = NULL,
    white_list = NULL,
    set_dif_arcs_df = NULL,
    evaluate_only_arcs_df = NULL,
    align_direction = TRUE,
    threshold = NULL,
    auto_threshold_ratio = 1/4,
    strength_min = .01,
    return_direction = TRUE,
    seed = 1
) {

  algorithm <- match.arg(algorithm)
  score <- match.arg(score)


  arg_list <- list(
    blacklist = black_list,
    whitelist = white_list
  )


  if (algorithm != "tree.bayes") {
    arg_list[["score"]] <- score
    arg_list[["maxp"]] <- maxp
  }



  # Run bootstrap strength
  if (!is.null(seed)) set.seed(seed)
  bn_boot <- bnlearn::boot.strength(
    data = df,
    R = bootstrap_reps,
    algorithm = algorithm,
    algorithm.args = arg_list
  ) %>%
    dplyr::filter(.data[["direction"]] > .5)



  if(!is.null(evaluate_only_arcs_df)){

    bn_boot <- evaluate_only_arcs_df %>%
      dplyr::left_join(
        bn_boot,
        by = dplyr::join_by(from == from, to == to)
      ) %>%
      dplyr::left_join(
        bn_boot,
        by = dplyr::join_by(from == to, to == from)
      ) %>%
      dplyr::mutate(
        direction.y = 1 - direction.y,
        # pick strength/direction from whichever is non-NA
        strength = dplyr::coalesce(strength.x, strength.y),
        direction = dplyr::coalesce(direction.x, direction.y)
      ) %>%
      dplyr::select(from, to, strength, direction) %>%
      dplyr::filter(!is.na(.data[["strength"]]))

  }


  bn_boot <- bn_boot %>%
    dplyr::arrange(
      dplyr::desc(.data[["strength"]])
    ) %>%
    dplyr::filter(.data[["strength"]] > strength_min)



  # Optionally filter arcs (and reverses) before alignment
  if (!is.null(set_dif_arcs_df)) {

    all_excluded <- dplyr::bind_rows(
      set_dif_arcs_df,
      bn_reverse_arc_df(set_dif_arcs_df)
    )

    bn_boot <- bn_boot %>%
      dplyr::select("from", "to") %>%
      dplyr::setdiff(all_excluded) %>%
      dplyr::left_join(bn_boot, by = c("from", "to"))

  }



  # Align directions if requested
  if (align_direction) {

    bn_boot <- bn_boot %>%
      dplyr::mutate(
        from_final = ifelse(.data[["direction"]] >= 0.5, .data[["from"]], .data[["to"]]),
        to_final   = ifelse(.data[["direction"]] >= 0.5, .data[["to"]], .data[["from"]]),
        direction_final = ifelse(.data[["direction"]] >= 0.5, .data[["direction"]], 1 - .data[["direction"]])
      ) %>%
      dplyr::select(
        from = "from_final",
        to = "to_final",
        strength = "strength",
        direction = "direction_final"
      ) %>%
      dplyr::arrange(
        dplyr::desc(.data[["strength"]]),
        dplyr::desc(.data[["direction"]])
      )

  }


  if(!is.null(threshold)){

    if(threshold == "auto"){


      if (auto_threshold_ratio >= 1 || auto_threshold_ratio <= 0) {
        stop("auto_threshold_ratio must be between 0 and 1")
      }


      temp_threshold <- df %>%
        ncol() %>%
        multiply_by(auto_threshold_ratio) %>%
        round(0)


      threshold <- bn_boot %>%
        dplyr::arrange(dplyr::desc(.data[["strength"]])) %>%
        dplyr::slice(temp_threshold) %>%
        dplyr::pull(.data[["strength"]])
    }


    if (threshold > 1 || threshold < 0) {
      stop("threshold must be between 0 and 1")
    }


    bn_boot <- bn_boot %>%
      dplyr::filter(strength >= threshold)

  }



  if(!return_direction){
    bn_boot <- bn_boot %>% dplyr::select(-direction)
  }



  bn_boot <- bn_boot %>% as.data.frame()


  return(bn_boot)
}
