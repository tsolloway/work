#' seg_write_solutions
#'
#' @description Writes solution workbooks (Excel files) for all or selected
#'   segmentation solutions. Each solution gets its own `.xlsx` file containing
#'   the shell table — segment means, significance flags, and hit highlighting
#'   — produced by [seg_write_shell_parallel()].
#'
#'   This is the main "export" step after clustering: it takes the fitted
#'   solutions stored in `seg$solutions$summary_table` and renders each one as
#'   a formatted Excel workbook suitable for review. Solutions are organised
#'   into sub-folders by `solution_name` inside the output directory.
#'
#'   By default all methods and solution families are written. Use the `do_*`
#'   flags or `solution` / `only_opt` arguments to narrow the set — for
#'   example, writing only the LDA-optimised solutions or only k-means results.
#'
#' @param seg A seg object with solutions already computed (via
#'   [seg_cluster_input_sheet()], [seg_cluster_with_profiles()], etc.).
#' @param solution Character or `NULL`. Solution family name (e.g. `"A"`) to
#'   write. `NULL` writes all families.
#' @param where Character or `NULL`. Output directory. `NULL` uses the path
#'   stored in `seg$paths$folders$solution`; falls back to `getwd()` if that
#'   is also `NULL`.
#' @param only_opt Logical. If `TRUE`, write only the LDA-optimised solutions
#'   (those whose `lda_name` starts with `"LDA_opt_"`). Default: `FALSE`.
#' @param do_kmeans Logical. Include k-means solutions (default: `TRUE`).
#' @param do_medoid Logical. Include PAM/medoid solutions (default: `TRUE`).
#' @param do_gaus_mix Logical. Include Gaussian mixture solutions
#'   (default: `TRUE`).
#' @param do_hierarchical Logical. Include hierarchical solutions
#'   (default: `TRUE`).
#' @param do_spectral Logical. Include spectral solutions (default: `TRUE`).
#' @param do_iterative Logical. Include iterative swap solutions
#'   (default: `TRUE`).
#' @param do_consensus Logical. Include consensus solutions (default: `TRUE`).
#' @param do_optimized Logical. Include `clust_optimized` solutions
#'   (default: `TRUE`).
#' @param strategy Character. Parallelisation strategy passed to
#'   [seg_write_shell_parallel()]: `"multisession"` (default), `"multicore"`,
#'   `"sequential"`, or `"cluster"`.
#' @param workers Integer. Number of parallel workers
#'   (default: `future::availableCores(omit = 1)`).
#' @param add_key Logical. Add a colour key sheet to each workbook
#'   (default: `TRUE`).
#' @param label_width Integer. Maximum character width for variable labels
#'   before wrapping (default: `75`).
#' @param hide_pvalue Logical. If `TRUE`, hide the p-value column in the
#'   output (default: `FALSE`).
#' @param truncate Character. Whether to truncate non-significant rows:
#'   `"no"` (default), `"yes"`, or `"both"` (writes both versions).
#' @param truncate_polar_threshold Numeric. Polar hit threshold for truncation
#'   (default: `0.15`).
#' @param truncate_profile_threshold Numeric. Profile hit threshold for
#'   truncation (default: `0.10`).
#' @param version Character. Output version: `"traditional"` (default) or
#'   `"both"`.
#' @param do_seg_bw Logical. Include a black-and-white formatted sheet
#'   (default: `TRUE`).
#' @param do_italic Logical. Italicise sub-threshold rows (default: `TRUE`).
#' @param switched_polars Logical. If `TRUE`, flag reversed-polarity polars
#'   (default: `FALSE`).
#' @param setting_polar_threshold Numeric. Polar significance threshold
#'   (default: `0.20`).
#' @param setting_profile_threshold Numeric. Profile significance threshold
#'   (default: `0.15`).
#' @param setting_tolerance Numeric. Tolerance band around threshold
#'   (default: `0.05`).
#' @param setting_pvalue Numeric. P-value cut-off for significance
#'   (default: `0.10`).
#' @param setting_diff Numeric. Minimum mean-difference for significance
#'   (default: `0.10`).
#' @param setting_type Character. Significance method: `"diff"` (default) or
#'   `"pvalue"`.
#' @param setting_color Character. Output colour scheme: `"bw"` (default) or
#'   `"color"`.
#' @param verbose Logical. Print progress messages (default: `FALSE`).
#'
#' @return Invisibly returns the result of [seg_write_shell_parallel()].
#'   Called for its side effect of writing Excel files to disk.
#'
#' @export
seg_write_solutions <- function(
    seg, solution = NULL, where = NULL,
    only_opt = FALSE,
    do_kmeans = TRUE,
    do_medoid = TRUE,
    do_gaus_mix = TRUE,
    do_hierarchical = TRUE,
    do_spectral = TRUE,
    do_iterative = TRUE,
    do_consensus = TRUE,
    do_optimized = TRUE,
    strategy = c("multisession", "multicore", "sequential", 'cluster'),
    workers = future::availableCores(omit = 1),
    add_key = TRUE,
    label_width = 75,
    hide_pvalue = FALSE,
    truncate = c("no", "yes", "both"),
    truncate_polar_threshold = .15,
    truncate_profile_threshold = .1,
    version = c("traditional", "both"),
    do_seg_bw = TRUE,
    do_italic = TRUE,
    switched_polars = FALSE,
    setting_polar_threshold = .2,
    setting_profile_threshold = .15,
    setting_tolerance = .05,
    setting_pvalue = .1,
    setting_diff = .1,
    setting_type = c("diff", "pvalue"),
    setting_color = c("bw", "color"),
    verbose = FALSE
){

  strategy <- match.arg(strategy)
  truncate <- match.arg(truncate)
  version <- match.arg(version)
  setting_type <- match.arg(setting_type)
  setting_color <- match.arg(setting_color)


  if(is.null(where)){
    where <- seg[["paths"]][["folders"]][["solution"]]
  }

  if(is.null(where) || is.na(where)){
    where <- getwd()
  }


  solution_summary_table <- seg[["solutions"]][["summary_table"]]


  if(!is.null(solution)){
    solution_summary_table <- solution_summary_table %>% dplyr::filter(solution_name == solution)
  }

  if(only_opt){
    solution_summary_table <- solution_summary_table %>% dplyr::filter(grepl("^LDA_opt_", lda_name))
  }

  # filter out methods set to FALSE
  exclude_patterns <- character(0)
  if (!do_kmeans)       exclude_patterns <- c(exclude_patterns, "kmeans_")
  if (!do_medoid)       exclude_patterns <- c(exclude_patterns, "medoid_")
  if (!do_gaus_mix)     exclude_patterns <- c(exclude_patterns, "gaus_mix_")
  if (!do_hierarchical) exclude_patterns <- c(exclude_patterns, "hierarchical_")
  if (!do_spectral)     exclude_patterns <- c(exclude_patterns, "spectral_")
  if (!do_iterative)    exclude_patterns <- c(exclude_patterns, "iter_")
  if (!do_consensus)    exclude_patterns <- c(exclude_patterns, "consensus_")

  if (length(exclude_patterns) > 0) {
    pat <- paste(exclude_patterns, collapse = "|")
    solution_summary_table <- solution_summary_table %>% dplyr::filter(!grepl(pat, lda_name))
  }

  if (!do_optimized) {
    solution_summary_table <- solution_summary_table %>% dplyr::filter(!grepl("^clust_optimized", solution_name))
  }


  solution_summary_table <- solution_summary_table %>%
    dplyr::mutate(
      location = glue::glue("{where}/{solution_name}")
    )


  solution_vars <- solution_summary_table %>%
    dplyr::select(lda_name) %>%
    unlist() %>%
    setNames(NULL)

  solution_locations <- solution_summary_table %>%
    dplyr::select(location) %>%
    unlist() %>%
    setNames(NULL)


  purrr::walk(
    solution_locations %>% unique(),
    ~dir.create(.x, showWarnings = FALSE)
  )


  invisible(seg_write_shell_parallel(
    seg = seg,
    solution_var = solution_vars,
    where = solution_locations,
    strategy = strategy,
    workers = workers,
    add_key = add_key,
    truncate = truncate,
    truncate_polar_threshold = truncate_polar_threshold,
    truncate_profile_threshold = truncate_profile_threshold,
    version = version,
    do_seg_bw = do_seg_bw,
    do_italic = do_italic,
    label_width = label_width,
    hide_pvalue = hide_pvalue,
    switched_polars = switched_polars,
    setting_polar_threshold = setting_polar_threshold,
    setting_profile_threshold = setting_profile_threshold,
    setting_tolerance = setting_tolerance,
    setting_pvalue = setting_pvalue,
    setting_diff = setting_diff,
    setting_type = setting_type,
    setting_color = setting_color,
    verbose = verbose
  ))

}

