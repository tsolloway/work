#' bn_name_groups
#'
#' @description Use Claude to generate descriptive names for community groups
#'   in a Bayesian Network result. Replaces default "Group 1", "Group 2", etc.
#'   with short thematic labels based on the node labels within each group.
#'   Each engine result is named independently since different network types
#'   may produce different community structures.
#'
#' @param results A BN results object from [bn_initial_networks()] or
#'   [bn_engine()]. Can be a single engine result, unsupervised
#'   (`bn_initial_networks`), or supervised (`bn_initial_networks` with DVs).
#' @param model Character. Claude model ID (default `"claude-sonnet-4-20250514"`).
#' @param max_words Integer. Maximum words per group name (default 3).
#' @param min_words Integer. Minimum words per group name (default 1).
#' @param api_key Character or NULL. Anthropic API key. If NULL, reads from
#'   `ANTHROPIC_API_KEY` environment variable.
#' @param verbose Logical. Print group naming results to console (default TRUE).
#'
#' @return The same `results` object with `community_name` updated in all
#'   `viz_prep$attribute_viz_prep$nodes` data frames, and `community_viz_prep`
#'   regenerated to reflect the new names.
#'
#' @export
bn_name_groups <- function(results,
                           model = "claude-sonnet-4-20250514",
                           max_words = 3,
                           min_words = 1,
                           api_key = NULL,
                           verbose = TRUE) {

  if (is.null(api_key)) {
    api_key <- get_environment_key("ANTHROPIC_API_KEY")
  }

  # --- detect structure and collect all engine results ---
  engine_paths <- .bn_name_find_engines(results)
  if (length(engine_paths) == 0) {
    cli::cli_warn("No engine results with viz_prep found. Returning unchanged.")
    return(results)
  }

  # --- deduplicate: group engines by identical group membership ---
  # (avoids redundant API calls for engines with same communities)
  signatures <- list()
  for (path in engine_paths) {
    engine <- .bn_name_get_engine(results, path)
    nodes <- engine$viz_prep$attribute_viz_prep$nodes
    # signature = sorted id:group pairs
    sig <- paste(sort(paste(nodes$id, nodes$group, sep = "=")), collapse = "|")
    signatures <- c(signatures, list(sig))
  }

  unique_sigs <- unique(unlist(signatures))
  sig_name_maps <- list()

  for (sig in unique_sigs) {
    # find first engine with this signature
    idx <- which(unlist(signatures) == sig)[1]
    engine <- .bn_name_get_engine(results, engine_paths[[idx]])
    nodes <- engine$viz_prep$attribute_viz_prep$nodes
    group_map <- nodes |>
      dplyr::group_by(group, community_name, color) |>
      dplyr::summarise(labels = list(label), .groups = "drop") |>
      dplyr::arrange(group)

    path_label <- paste(engine_paths[[idx]], collapse = "$")
    if (verbose) {
      cli::cli_h2("Naming {nrow(group_map)} groups ({path_label})")
      for (i in seq_len(nrow(group_map))) {
        cli::cli_bullets(c("*" = "Group {group_map$group[i]}: {paste(group_map$labels[[i]], collapse = ', ')}"))
      }
    }

    prompt <- .bn_name_build_prompt(group_map, min_words, max_words)
    response <- .bn_name_call_claude(prompt, model, api_key)
    name_map <- .bn_name_parse_response(response, group_map$group)

    if (verbose) {
      cli::cli_h3("Results")
      for (g in names(name_map)) {
        cli::cli_bullets(c("v" = "Group {g} -> {.val {name_map[[g]]}}"))
      }
    }

    sig_name_maps[[sig]] <- name_map
  }

  # --- apply names to all engine results ---
  for (i in seq_along(engine_paths)) {
    name_map <- sig_name_maps[[unlist(signatures)[[i]]]]
    results <- .bn_name_apply(results, engine_paths[[i]], name_map)
  }

  # report how many API calls were saved
  if (verbose && length(engine_paths) > length(unique_sigs)) {
    cli::cli_alert_info(
      "{length(engine_paths)} engines, {length(unique_sigs)} unique grouping{?s} ({length(engine_paths) - length(unique_sigs)} API call{?s} saved)"
    )
  }

  results
}


# ---- internal helpers ----

#' Find all engine result paths in a BN results object
#' @noRd
.bn_name_find_engines <- function(results) {
  # single engine result
  if (!is.null(results[["viz_prep"]])) {
    return(list(list()))
  }

  paths <- list()

  for (nm in names(results)) {
    child <- results[[nm]]
    if (is.null(child) || !is.list(child)) next

    # direct engine (e.g. bare engine passed to bn_name_groups directly)
    if (!is.null(child[["viz_prep"]])) {
      paths <- c(paths, list(list(nm)))
      next
    }

    # nested engine (results$cb_direct$Species or results$cb$Unsupervised)
    for (nm2 in names(child)) {
      grandchild <- child[[nm2]]
      if (is.list(grandchild) && !is.null(grandchild[["viz_prep"]])) {
        paths <- c(paths, list(list(nm, nm2)))
      }
    }
  }

  paths
}

#' Get an engine result by path
#' @noRd
.bn_name_get_engine <- function(results, path) {
  obj <- results
  for (key in path) obj <- obj[[key]]
  obj
}

#' Build the Claude prompt
#' @noRd
.bn_name_build_prompt <- function(group_map, min_words, max_words) {
  group_lines <- purrr::map_chr(seq_len(nrow(group_map)), function(i) {
    labels <- paste(group_map$labels[[i]], collapse = ", ")
    glue::glue("Group {group_map$group[i]}: {labels}")
  })

  glue::glue(
    "You are naming groups of survey attributes that were clustered together ",
    "in a Bayesian Network community detection analysis.\n\n",
    "For each group below, provide a short descriptive thematic name ",
    "({min_words} to {max_words} words). The name should capture the common ",
    "theme of the attributes in that group.\n\n",
    "{paste(group_lines, collapse = '\n')}\n\n",
    "Respond with ONLY the group names, one per line, in the format:\n",
    "Group N: Name\n\n",
    "Do not include any other text."
  )
}

#' Call the Claude API
#' @noRd
.bn_name_call_claude <- function(prompt, model, api_key) {
  body <- list(
    model = model,
    max_tokens = 1024,
    messages = list(
      list(role = "user", content = prompt)
    )
  )

  resp <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw"
  )

  if (httr::status_code(resp) != 200) {
    err_body <- httr::content(resp, as = "text", encoding = "UTF-8")
    cli::cli_abort("Claude API error ({httr::status_code(resp)}): {err_body}")
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  parsed$content[[1]]$text
}

#' Parse Claude's response into a named list
#' @noRd
.bn_name_parse_response <- function(response, group_numbers) {
  lines <- trimws(strsplit(response, "\n")[[1]])
  lines <- lines[nchar(lines) > 0]

  name_map <- list()
  for (line in lines) {
    m <- regmatches(line, regexec("Group\\s+(\\d+)\\s*:\\s*(.+)", line))[[1]]
    if (length(m) == 3) {
      name_map[[m[2]]] <- trimws(m[3])
    }
  }

  # verify all groups got names
  missing <- setdiff(as.character(group_numbers), names(name_map))
  if (length(missing) > 0) {
    cli::cli_warn("Could not parse names for group{?s} {missing}. Keeping defaults.")
  }

  name_map
}

#' Apply group names to a single engine result within the results object
#' @noRd
.bn_name_apply <- function(results, path, name_map) {
  # navigate to the engine
  obj <- results
  for (key in path) obj <- obj[[key]]

  # update attribute nodes
  nodes <- obj$viz_prep$attribute_viz_prep$nodes
  for (g in names(name_map)) {
    nodes$community_name[nodes$group == as.integer(g)] <- name_map[[g]]
  }
  obj$viz_prep$attribute_viz_prep$nodes <- nodes

  # regenerate community viz_prep
  obj$viz_prep$community_viz_prep <- bn_to_netviz_prep_for_communities(
    attribute_viz_prep = obj$viz_prep$attribute_viz_prep,
    community_edge_by = "sum"
  )

  # write back into results
  if (length(path) == 0) {
    return(obj)
  } else if (length(path) == 1) {
    results[[path[[1]]]] <- obj
  } else {
    results[[path[[1]]]][[path[[2]]]] <- obj
  }

  results
}
