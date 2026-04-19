#' Visualize Bayesian Networks with Multiple Layout Modes
#'
#' @description
#' Creates an interactive or static visualization of a Bayesian Network
#' using \pkg{visNetwork}. Supports multiple layout paradigms:
#' - `"none"`: fixed coordinates or static layout without physics.
#' - `"charge"`: Fruchterman–Reingold charge-based layout (via `igraph`).
#' - `"gravity"`: Barnes–Hut gravity-based physics simulation (dynamic).
#' - `"hierarchy"`: directed or layered layout (hierarchical).
#'
#' Designed for deliverable-quality visuals or exploratory interactivity.
#' Integrates seamlessly with Bayesian network outputs from `bnlearn`
#' and visualization prep outputs from custom BN pipelines.
#'
#' @param obj A Bayesian network visualization object or list containing
#'   at least `nodes` and `edges` data frames. Can also be a higher-level
#'   object from `bn_engine()` or similar that includes `viz_prep`.
#' @param nodes Optional data frame of nodes to override those in `obj`.
#'   Must include columns: `id`, `label`, and optionally `group`, `color`, etc.
#' @param edges Optional data frame of edges to override those in `obj`.
#'   Must include columns: `from`, `to`.
#' @param type Character. Layout type: one of
#'   `"none"`, `"gravity"`, `"charge"`, or `"hierarchy"`. Default `"none"`.
#' @param vs_height Character. Height of the visualization container
#'   (e.g., `"100vh"`). Default `"100vh"`.
#' @param vs_width Character. Width of the visualization container
#'   (e.g., `"100%"`). Default `"100%"`.
#' @param gravity_constant Numeric. Gravitational constant controlling
#'   repulsion strength in Barnes–Hut layout (more negative = stronger repulsion).
#'   Default `-9000`.
#' @param central_gravity Numeric. Pull of all nodes toward the center.
#'   Default `0.2`.
#' @param charge_layout Character. Name of an \pkg{igraph} layout function to use
#'   when `type = "charge"`. Controls how nodes are spatially arranged using
#'   force-directed, geometric, or hierarchical algorithms.
#'
#'   Any valid layout name recognized by \pkg{igraph} may be used (passed
#'   to `igraph::layout_*(graph)` internally). Common and extended options include:
#'
#'   **Force-directed and physics-based layouts**
#'   - `"layout_with_fr"` — Fruchterman–Reingold (default); balances attractive
#'     and repulsive forces for a visually even distribution.
#'   - `"layout_with_kk"` — Kamada–Kawai spring layout; ideal for symmetric networks.
#'   - `"layout_with_drl"` — Distributed Recursive Layout; scalable variant of FR
#'     suitable for large graphs.
#'   - `"layout_with_gem"` — GEM (Graph Embedder) layout; force-directed with adaptive cooling.
#'
#'   **Deterministic geometric layouts**
#'   - `"layout_in_circle"` — Places all nodes evenly spaced on a circle.
#'   - `"layout_in_grid"` — Arranges nodes on a rectangular grid.
#'   - `"layout_randomly"` — Uniform random positions within a bounding box.
#'   - `"layout_norm"` — Normalizes coordinates of any layout to the unit square.
#'
#'   **Hierarchical and structural layouts**
#'   - `"layout_as_tree"` — Rooted tree layout; useful for DAGs or BN structures.
#'   - `"layout_as_star"` — Star layout centered on a single focal node.
#'   - `"layout_as_bipartite"` — Bipartite graph layout with two distinct node sets.
#'
#'   **Dimensional-reduction layouts**
#'   - `"layout_with_mds"` — Multidimensional scaling (MDS) layout; positions nodes
#'     based on pairwise graph distances.
#'   - `"layout_with_lgl"` — Large Graph Layout; optimized for very large networks.
#'   - `"layout_with_stress"` — Stress majorization layout (newer, flexible alternative
#'     to Kamada–Kawai).
#'
#' @param do_community Logical. If `TRUE`, visualize community-level network
#'   instead of node-level attributes (requires community prep).
#' @param add_key Logical. If `TRUE`, adds a legend for community or group colors.
#'   Default `TRUE`.
#' @param key_width Numeric. Width of the legend as a proportion of the widget
#'   (0 to 1). Default `0.1` (10%).
#' @param node_positions Optional. Pre-saved node positions to restore a
#'   previous layout. Accepts:
#'   - A file path to a JSON file exported by the "Save Layout" button.
#'   - A data frame with columns `id`, `x`, `y`.
#'   When provided, node coordinates are set before rendering and physics
#'   is disabled to preserve the layout.
#' @param interactive Logical. If `TRUE`, adds enhanced interactivity (zoom,
#'   drag, font slider, PNG/SVG export) via
#'   `work::bn_visNetwork_deliverable_interactivity()`.
#' @param save_visuals Logical. If `TRUE`, saves the visualization as a
#'   standalone HTML file.
#' @param save_to_png Logical. If `TRUE`, captures the visualization as a
#'   PNG image using \code{webshot2}. Requires the \code{webshot2} package.
#'   Default `FALSE`.
#' @param save_file_name Character. File name (without extension) used for
#'   saved HTML or PNG files. Default `"Network Visual"`.
#' @param png_width Numeric. PNG capture width in pixels. Default 1200.
#' @param png_height Numeric. PNG capture height in pixels. Default 900.
#' @param png_delay Numeric. Seconds to wait for physics stabilization before
#'   capturing PNG. Default 3. Bump if you see unsettled/unstable layouts in
#'   the rendered image on larger networks.
#' @param font_size Numeric or NULL. Override the node-label font size (in
#'   pixels) by applying \code{visNetwork::visNodes(font = list(size = ...))}.
#'   Default NULL keeps vis.js's built-in 14 px. In interactive mode the
#'   in-iframe font slider can still adjust this afterwards.
#' @param seed Numeric. Random seed for reproducible layout placement.
#'
#' @details
#' The function supports four conceptual layout paradigms:
#'
#' | Type | Layout Engine | Physics | Conceptual Model |
#' |------|----------------|----------|------------------|
#' | `"none"` | None | N | Static snapshot (fixed positions) |
#' | `"charge"` | Fruchterman–Reingold | N | Charge repulsion (balanced electrostatic field) |
#' | `"gravity"` | Barnes–Hut | Y. | Gravity simulation (mass clustering) |
#' | `"hierarchy"` | vis.js hierarchical | N | Layered top-down or directional layout |
#'
#' @return
#' A `visNetwork` object representing the network visualization.
#' When `save_visuals = TRUE`, an `.html` file is also written to disk.
#'
#' @examples
#' \dontrun{
#' library(bnlearn)
#' library(visNetwork)
#'
#' # Example 1: Basic static visualization (no physics)
#' bn <- bnlearn::hc(iris)
#' net <- bnlearn::graphviz.plot(bn) # for reference
#'
#' nodes <- data.frame(id = nodes(bn), label = nodes(bn))
#' edges <- bnlearn::arcs(bn) %>% as.data.frame()
#'
#' bn_visual(
#'   obj = list(nodes = nodes, edges = edges),
#'   type = "none"
#' )
#'
#' # Example 2: Charge-based layout (Fruchterman–Reingold)
#' bn_visual(
#'   obj = list(nodes = nodes, edges = edges),
#'   type = "charge"
#' )
#'
#' # Example 3: Gravity layout (Barnes–Hut physics)
#' bn_visual(
#'   obj = list(nodes = nodes, edges = edges),
#'   type = "gravity",
#'   gravity_constant = -7000
#' )
#'
#' # Example 4: Hierarchical layout
#' bn_visual(
#'   obj = list(nodes = nodes, edges = edges),
#'   type = "hierarchy"
#' )
#' }
#'
#' @export
bn_visual <- function(
    obj,
    type = c("none", "gravity", "charge", "hierarchy"),
    nodes = NULL,
    edges = NULL,
    vs_height = "100vh",
    vs_width = "100%",
    gravity_constant = -9000,
    central_gravity = .2,
    charge_layout = "layout_with_fr",
    do_community = FALSE,
    add_key = TRUE,
    key_width = 0.1,
    node_positions = NULL,
    interactive = TRUE,
    physics = TRUE,
    panel_ns = NULL,
    download_prefix = "network",
    save_visuals = FALSE,
    save_to_png = FALSE,
    save_file_name = "Network Visual",
    png_width = 1200,
    png_height = 900,
    png_delay = 3,
    font_size = NULL,
    seed = 1
){

  type <- match.arg(type)
  do_bn_obj_viz <- FALSE

  ########################
  # object unpacking logic
  ########################

  if(!do_community) viz_obj <- obj %>% work::find_recursive(x_name = "attribute_viz_prep")

  if(do_community) viz_obj <- obj %>% work::find_recursive(x_name = "community_viz_prep")


  if(is.null(viz_obj)){
    if(all(c("nodes", "edges") %in% names(obj))){

      viz_obj <- obj

    }else if(work::find_recursive(x = obj, x_class = "bn", return_logical = TRUE)){

      do_bn_obj_viz <- TRUE
      viz_obj <- work::find_recursive(x = obj, x_class = "bn")

    }else if(!is.null(nodes) && !is.null(edges)){

      viz_obj <- list()

    }
  }


  if(do_bn_obj_viz){
    return(
      work::bn_obj_viz(viz_obj)
    )
  }


  ########################
  # validate and finalize viz_obj
  ########################
  if (is.null(viz_obj)) {
    stop("Unable to locate visualization data or bnlearn::bn object in `obj`.")
  }

  # override nodes/edges if provided
  if (!is.null(nodes)) viz_obj[["nodes"]] <- nodes
  if (!is.null(edges)) viz_obj[["edges"]] <- edges

  if (is.null(viz_obj[["nodes"]]) || is.null(viz_obj[["edges"]])) {
    stop("`viz_obj` must contain valid `nodes` and `edges` data frames.")
  }

  viz_obj[["nodes"]] <- viz_obj[["nodes"]] %>% dplyr::arrange(id)


  ########################
  # apply saved node positions
  ########################
  use_saved_positions <- FALSE

  if (!is.null(node_positions)) {

    # read from JSON file if path provided
    if (is.character(node_positions) && length(node_positions) == 1 && file.exists(node_positions)) {
      pos_json <- jsonlite::fromJSON(node_positions)
      node_positions <- tibble::as_tibble(pos_json)
    }

    # merge x/y into nodes
    if (is.data.frame(node_positions) && all(c("id", "x", "y") %in% names(node_positions))) {
      pos_lookup <- node_positions %>% dplyr::select(id, x, y)

      # drop existing x/y if present to avoid suffix collision
      viz_obj[["nodes"]] <- viz_obj[["nodes"]] %>%
        dplyr::select(-dplyr::any_of(c("x", "y"))) %>%
        dplyr::left_join(pos_lookup, by = "id")

      use_saved_positions <- TRUE
    }
  }


  ########################
  # base visual no layout
  ########################
  viz <- visNetwork::visNetwork(
    nodes = viz_obj[["nodes"]],
    edges = viz_obj[["edges"]],
    height = vs_height,
    width = vs_width
  )

  # Optional node-label font size override. Default (NULL) leaves vis.js's
  # built-in 14 px in place; the iframe font slider can still adjust it
  # afterwards in interactive mode.
  if (!is.null(font_size)) {
    viz <- viz %>% visNetwork::visNodes(font = list(size = font_size))
  }

  # ensure reproducibility
  if (!is.null(seed)) viz <- viz %>% visNetwork::visLayout(randomSeed = seed)


  # if saved positions provided, disable physics and skip layout engines
  if (use_saved_positions) {

    viz <- viz %>% visNetwork::visPhysics(enabled = FALSE)

  } else if(type == "gravity"){

    viz <- viz %>%
      visNetwork::visPhysics(
        solver = "barnesHut",
        stabilization = TRUE,
        barnesHut = list(
          gravitationalConstant = gravity_constant,
          centralGravity = central_gravity,
          avoidOverlap = 1
        )
      )

  }else if(type == "charge"){

    viz <- viz %>% visNetwork::visIgraphLayout(layout = charge_layout)

  }else if(type == "hierarchy"){

    viz <- viz %>% visNetwork::visHierarchicalLayout()

    viz[["x"]][["nodes"]][["label"]] <- viz[["x"]][["nodes"]][["label"]] %>%
      strsplit(" - ") %>%
      purrr::map_chr(head(1))

  }



  ########################
  # legend (community key)
  #######################

  if(add_key){

    df_key <- viz[["x"]][["nodes"]] %>%
      dplyr::arrange(group) %>%
      dplyr::select(community_name, color) %>%
      dplyr::distinct() %>%
      dplyr::rename(label = community_name)

    key_json <- jsonlite::toJSON(df_key, auto_unbox = TRUE)
  } else {
    key_json <- NULL
  }


  ########################
  # interactivity
  ########################

  if(interactive) viz <- viz %>% bn_visNetwork_deliverable_interactivity(
    physics = physics, type = type,
    key_json = key_json, key_width = key_width,
    panel_ns = panel_ns, download_prefix = download_prefix
  )


  ########################
  # save
  ########################
  if(save_visuals){
    visNetwork::visSave(
      viz,
      file = glue::glue("{save_file_name}.html"),
      selfcontained = TRUE
    )
  }

  if(save_to_png){
    if (!requireNamespace("webshot2", quietly = TRUE)) {
      warning("webshot2 package required for save_to_png. Install with install.packages('webshot2')")
      return(viz)
    }

    tmp_dir <- tempfile("bn_png_")
    dir.create(tmp_dir, recursive = TRUE)
    tmp_html <- file.path(tmp_dir, "temp_network.html")
    png_path <- paste0(save_file_name, ".png")

    htmlwidgets::saveWidget(viz, file = normalizePath(tmp_html, mustWork = FALSE),
      selfcontained = TRUE)

    webshot2::webshot(
      url = normalizePath(tmp_html),
      file = png_path,
      vwidth = png_width,
      vheight = png_height,
      delay = png_delay
    )

    unlink(tmp_dir, recursive = TRUE)

    if (file.exists(png_path)) {
      cli::cli_alert_success("Saved {png_path}")
    } else {
      warning("PNG capture failed")
    }
  }

  return(viz)
}
