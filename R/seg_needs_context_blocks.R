#' seg_needs_context_blocks
#'
#' @description Writes the per-context profile blocks for a needs spec from the
#'   stack map, so they are not hand-written for each project:
#'
#'   \describe{
#'     \item{which}{one row per context - which context this grid is}
#'     \item{nets}{context groupings, from `nets`}
#'     \item{frequency}{how often the respondent does this context, from a
#'       per-context variable carried by [seg_needs_stack()], plus any 0/1
#'       per-context flags}
#'   }
#'
#'   All three are grid-level: they read `context` and the `context_vars`
#'   columns, which differ down a respondent's rows. [seg_needs_write_shell()]
#'   detects that and keeps them off the person shell.
#'
#'   Unlike the other `seg_needs_*` functions this returns blocks, not the seg
#'   object - it feeds [seg_generate_spec()], which runs before the spec
#'   exists.
#'
#' @param seg A seg object after [seg_needs_stack()].
#' @param nets Named list, net label -> contexts (indices or context labels),
#'   e.g. `list("Any exercise" = c(7, 8))`. `NULL` skips the nets block.
#' @param freq_var Character. The per-context frequency column, as named in
#'   `context_vars` at stack time. Defaults to the only one when there is only
#'   one.
#' @param freq_cuts Named character, row label -> value in spec grammar, e.g.
#'   `c("Does this weekly or more" = ">= 4")`. When `NULL`, one row per coded
#'   value is written from the column's value labels, if it has them.
#' @param flags Named character, per-context 0/1 column -> row label, e.g.
#'   `c(context_is_top = "One of the respondent's most frequent")`. Added to
#'   the frequency block.
#' @param noun Character. What a context is called in block names
#'   (default `"context"`; `"occasion"` for an occasions loop).
#' @param prefixes Named character: `which`, `nets`, `freq`.
#' @param block_names Named character overriding any of the default names.
#'
#' @return A list of profile blocks, as from [seg_create_profile_block()].
#'
#' @export
seg_needs_context_blocks <- function(
    seg,
    nets        = NULL,
    freq_var    = NULL,
    freq_cuts   = NULL,
    flags       = NULL,
    noun        = "context",
    prefixes    = c(which = "OA", nets = "OG", freq = "OQ"),
    block_names = NULL
){

  loop    <- seg[["needs"]][["loop"]]
  stacked <- seg[["data"]][["stacked"]]
  ctx     <- loop[["context_labels"]]
  n_ctx   <- loop[["n_contexts"]]

  if(!is.data.frame(stacked)){
    stop("No stacked frame. Run seg_needs_stack() first.", call. = FALSE)
  }

  Noun <- paste0(toupper(substr(noun, 1, 1)), substring(noun, 2))
  bn <- c(which = paste(Noun, "of This Grid"),
          nets  = paste(Noun, "Type (net)"),
          freq  = paste(Noun, "Frequency"))
  bn[names(block_names)] <- block_names

  resolve_ctx <- function(x){
    idx <- if(is.numeric(x)) as.integer(x) else match(x, ctx)
    if(anyNA(idx) || any(idx < 1 | idx > n_ctx)){
      stop("Cannot match context(s): ", paste(x[is.na(idx) | idx < 1 | idx > n_ctx], collapse = ", "),
           ". Use indices 1-", n_ctx, " or the context labels.", call. = FALSE)
    }
    idx
  }

  # the spec grammar: a single code as-is, several as c(...)
  as_value <- function(idx) if(length(idx) == 1) as.character(idx) else
    paste0("c(", paste(idx, collapse = ", "), ")")

  blocks <- list(
    seg_create_profile_block(
      prefix     = prefixes[["which"]],
      name       = bn[["which"]],
      label      = ctx,
      source_var = rep("context", n_ctx),
      value      = as.character(seq_len(n_ctx))
    )
  )


  # ---- nets -------------------------------------------------------------------
  if(!is.null(nets)){
    if(is.null(names(nets)) || any(names(nets) == "")){
      stop("nets must be a named list: net label -> contexts.", call. = FALSE)
    }
    blocks[[length(blocks) + 1]] <- seg_create_profile_block(
      prefix     = prefixes[["nets"]],
      name       = bn[["nets"]],
      label      = names(nets),
      source_var = rep("context", length(nets)),
      value      = vapply(nets, function(x) as_value(sort(resolve_ctx(x))), character(1), USE.NAMES = FALSE)
    )
  }


  # ---- frequency and flags ----------------------------------------------------
  cvars <- names(loop[["context_vars"]])
  if(is.null(freq_var) && length(cvars) == 1 && is.null(flags)) freq_var <- cvars

  rows <- list(label = character(0), source_var = character(0), value = character(0))

  if(!is.null(freq_var)){
    if(!freq_var %in% names(stacked)){
      stop("freq_var '", freq_var, "' is not on the stacked frame. Carry it with ",
           "seg_needs_stack(context_vars = ).", call. = FALSE)
    }

    if(is.null(freq_cuts)){
      labs <- attr(stacked[[freq_var]], "labels")
      if(is.null(labs)){
        stop("'", freq_var, "' has no value labels to build rows from - supply freq_cuts, e.g. ",
             'c("Weekly or more" = ">= 4").', call. = FALSE)
      }
      labs <- labs[labs %in% stacked[[freq_var]]]
      freq_cuts <- rlang::set_names(as.character(unname(labs)), names(labs))
    }
    if(is.null(names(freq_cuts)) || any(names(freq_cuts) == "")){
      stop("freq_cuts must be named: row label -> value.", call. = FALSE)
    }

    rows$label      <- c(rows$label, names(freq_cuts))
    rows$source_var <- c(rows$source_var, rep(freq_var, length(freq_cuts)))
    rows$value      <- c(rows$value, unname(freq_cuts))
  }

  if(!is.null(flags)){
    missing_flags <- setdiff(names(flags), names(stacked))
    if(length(missing_flags)){
      stop("flag column(s) not on the stacked frame: ", paste(missing_flags, collapse = ", "), call. = FALSE)
    }
    rows$label      <- c(rows$label, unname(flags))
    rows$source_var <- c(rows$source_var, names(flags))
    rows$value      <- c(rows$value, rep("1", length(flags)))
  }

  if(length(rows$label)){
    blocks[[length(blocks) + 1]] <- seg_create_profile_block(
      prefix     = prefixes[["freq"]],
      name       = bn[["freq"]],
      label      = rows$label,
      source_var = rows$source_var,
      value      = rows$value
    )
  }

  message(
    "Context blocks: ",
    paste0(vapply(blocks, `[[`, character(1), "prefix"), " (",
           vapply(blocks, function(b) nrow(b$items), integer(1)), " rows)", collapse = ", "),
    ". All grid-level - they stay off the person shell."
  )

  blocks
}
