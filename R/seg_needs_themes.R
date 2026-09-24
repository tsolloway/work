#' seg_needs_themes
#'
#' @description Assigns each need to a theme - the unit [seg_needs_type()]
#'   scores every grid on. Each item goes to the factor it loads on most
#'   strongly, and the table comes back for review before anything is typed.
#'
#'   Three sources, in order of precedence:
#'
#'   \describe{
#'     \item{`manual`}{a named list, theme name -> items. Replaces the loadings
#'       outright.}
#'     \item{`winner`}{the sheet of the rotation workbook written by
#'       [seg_needs_structure()] - the solution that was actually reviewed. This
#'       is the normal route.}
#'     \item{`nfactors`}{fit the structure here. Equamax has local optima, so a
#'       fresh fit can move a marginal item relative to the workbook; prefer
#'       `winner` once a solution has been picked.}
#'   }
#'
#'   `move` then reassigns individual items on top of whichever source was
#'   used, so a solution that is right bar one item does not have to be
#'   rewritten in full.
#'
#'   Two things are flagged rather than silently resolved. An item whose best
#'   loading is below `min_loading` loads on no theme; it is left out of the
#'   theme scores, because forcing it in makes the theme mean partly noise. An
#'   item whose runner-up loading is within `cross_gap` of its best is a
#'   cross-loader; it stays assigned, but its theme is less clean than the table
#'   makes it look.
#'
#' @param seg A seg object after [seg_needs_prepare()].
#' @param winner Integer. Sheet of the rotation workbook to read, as picked on
#'   review. Uses `seg$paths$files$pca` unless `file_location` is given.
#' @param nfactors Integer. Fit the structure here instead of reading a
#'   workbook. Ignored when `winner` or `manual` is supplied.
#' @param manual Named list, theme name -> items. Items may be given as index,
#'   variable name (`need01`, `pneed01`, `needs01`) or label.
#' @param move Named character, item -> theme name (or `NA` to leave the item
#'   out of every theme). Applied after the source.
#' @param theme_names Character, one per theme in factor order. Defaults to
#'   `"Theme 1"`, `"Theme 2"`, ...
#' @param min_loading Numeric. Below this an item loads on no theme
#'   (default `0.40`).
#' @param cross_gap Numeric. A runner-up loading within this of the best marks
#'   a cross-loader (default `0.10`).
#' @param level,model,rotation Used only with `nfactors`; as in
#'   [seg_needs_structure()].
#' @param file_location Character. Rotation workbook to read with `winner`.
#' @param row_header Integer. Header row of the workbook sheet (default `4`).
#'
#' @return The seg object with the assignment table in
#'   `seg[["needs"]][["themes"]]`. Prints the table and a verdict.
#'
#' @export
seg_needs_themes <- function(
    seg,
    winner        = NULL,
    nfactors      = NULL,
    manual        = NULL,
    move          = NULL,
    theme_names   = NULL,
    min_loading   = 0.40,
    cross_gap     = 0.10,
    level         = c("person", "grid"),
    model         = c("pca", "fa"),
    rotation      = "equamax",
    file_location = NULL,
    row_header    = 4
){

  level <- match.arg(level)
  model <- match.arg(model)

  if(!inherits(seg, "analytic_needs")){
    stop("Run seg_needs_init() first.", call. = FALSE)
  }

  labs  <- seg[["needs"]][["loop"]][["item_labels"]]
  items <- seg[["needs"]][["vars"]][["raw"]]
  n     <- length(labs)

  if(anyNA(items)){
    stop("No item columns recorded. Run seg_needs_stack() and seg_needs_prepare() first.",
         call. = FALSE)
  }


  # ---- loadings: items x factors, in item order -------------------------
  if(!is.null(manual)){

    source_desc <- "manual assignment"
    if(is.null(names(manual)) || any(names(manual) == "")){
      stop("manual must be a named list: theme name -> items.", call. = FALSE)
    }

    L <- matrix(0, n, length(manual))
    for(k in seq_along(manual)){
      L[needs_resolve_items(seg, manual[[k]]), k] <- 1
    }
    if(any(rowSums(L) > 1)){
      stop("An item is listed under more than one theme: ",
           paste(labs[rowSums(L) > 1], collapse = ", "), call. = FALSE)
    }
    if(is.null(theme_names)) theme_names <- names(manual)

  } else if(!is.null(winner)){

    if(is.null(file_location)) file_location <- seg[["paths"]][["files"]][["pca"]]
    if(is.null(file_location) || is.na(file_location) || !file.exists(file_location)){
      stop("No rotation workbook found. Run seg_needs_structure() or pass file_location.",
           call. = FALSE)
    }
    source_desc <- paste0("workbook sheet ", winner, " (", basename(file_location), ")")

    wb <- openxlsx::read.xlsx(file_location, sheet = as.character(winner), startRow = row_header)
    fcols <- grep("^F[0-9]+$", names(wb), value = TRUE)
    wb <- wb[!is.na(wb[["Variable"]]), , drop = FALSE]

    # The workbook blanks loadings under its clean_max, so a blank is a small
    # loading, not a missing one. Reading it as 0 makes the runner-up gap
    # conservative for exactly the items that are cleanest.
    Lw <- as.matrix(wb[fcols])
    Lw[is.na(Lw)] <- 0

    L <- matrix(0, n, length(fcols))
    L[needs_resolve_items(seg, wb[["Variable"]]), ] <- Lw

  } else if(!is.null(nfactors)){

    source_desc <- paste0(toupper(model), " ", rotation, ", ", nfactors, " factors, ", level, " level")

    df <- seg[["data"]][["stacked"]]
    if(level == "person") df <- dplyr::distinct(df, .data$person_id, .keep_all = TRUE)
    vars <- if(level == "person") seg[["needs"]][["vars"]][["person"]] else items
    X <- as.matrix(df[vars])

    fit <- if(model == "pca"){
      psych::principal(X, nfactors = nfactors, rotate = rotation)
    } else {
      psych::fa(X, nfactors = nfactors, rotate = rotation, fm = "minres")
    }
    L <- unclass(fit[["loadings"]])

  } else {
    stop("Supply winner (the reviewed workbook sheet), nfactors, or manual.", call. = FALSE)
  }

  K <- ncol(L)
  if(is.null(theme_names)) theme_names <- paste("Theme", seq_len(K))
  if(length(theme_names) != K){
    stop("theme_names has ", length(theme_names), " names for ", K, " themes.", call. = FALSE)
  }


  # ---- assign by max absolute loading ------------------------------------
  A     <- abs(L)
  best  <- max.col(A, ties.method = "first")
  first <- A[cbind(seq_len(n), best)]
  A2    <- A
  A2[cbind(seq_len(n), best)] <- -Inf
  second_k <- if(K > 1) max.col(A2, ties.method = "first") else rep(NA_integer_, n)
  second   <- if(K > 1) A2[cbind(seq_len(n), second_k)] else rep(0, n)

  tbl <- dplyr::tibble(
    item           = seq_len(n),
    var            = items,
    label          = labs,
    theme          = best,
    loading        = round(L[cbind(seq_len(n), best)], 3),
    second_theme   = second_k,
    second_loading = round(second, 3),
    status         = "assigned"
  )

  if(is.null(manual)){
    tbl$status[second > 0 & first - second < cross_gap] <- "cross-loads"
    tbl$status[first < min_loading]                     <- "loads on no theme"
    tbl$theme[first < min_loading]                      <- NA_integer_
  } else {
    tbl$status <- ifelse(rowSums(L) > 0, "manual", "unassigned")
    tbl$theme[rowSums(L) == 0] <- NA_integer_
    tbl$second_theme   <- NA_integer_
    tbl$second_loading <- NA_real_
  }

  if(!is.null(move)){
    idx <- needs_resolve_items(seg, names(move))
    to  <- match(unname(move), theme_names)
    if(any(!is.na(move) & is.na(to))){
      stop("move names a theme that does not exist: ",
           paste(unique(move[!is.na(move) & is.na(to)]), collapse = ", "),
           ". Themes are: ", paste(theme_names, collapse = ", "), call. = FALSE)
    }
    tbl$theme[idx]  <- to
    tbl$status[idx] <- ifelse(is.na(to), "moved out", "moved")
  }

  # a negative best loading means the item scores the theme in reverse; the
  # typing honours the sign rather than treating it as a positive indicator
  tbl$sign <- ifelse(!is.na(tbl$loading) & tbl$loading < 0, -1L, 1L)
  tbl$theme_name <- theme_names[tbl$theme]

  tbl <- tbl[, c("item", "var", "label", "theme", "theme_name", "loading",
                 "second_theme", "second_loading", "sign", "status")]


  # ---- report -------------------------------------------------------------
  cat("\n=== Needs themes (", source_desc, ") ===\n\n", sep = "")

  for(k in seq_len(K)){
    rows <- tbl[!is.na(tbl$theme) & tbl$theme == k, ]
    rows <- rows[order(-abs(rows$loading)), ]
    cat("  ", theme_names[k], " (", nrow(rows), " items)\n", sep = "")
    for(i in seq_len(nrow(rows))){
      cat(sprintf("      %-45s %6.2f%s\n", rows$label[i], rows$loading[i],
                  if(rows$status[i] %in% c("assigned", "manual")) "" else paste0("   <- ", rows$status[i])))
    }
  }

  out <- tbl[is.na(tbl$theme), ]
  if(nrow(out) > 0){
    cat("  (in no theme)\n")
    for(i in seq_len(nrow(out))){
      cat(sprintf("      %-45s %6.2f   <- %s\n", out$label[i], out$loading[i], out$status[i]))
    }
  }

  sizes <- tabulate(stats::na.omit(tbl$theme), nbins = K)
  n_cross <- sum(tbl$status == "cross-loads")

  cat("\n=== Verdict ===\n")
  if(any(sizes == 0)){
    cat("  FAIL - ", paste(theme_names[sizes == 0], collapse = ", "),
        " has no items. Drop a factor or reassign.\n", sep = "")
  } else if(any(sizes < 2)){
    cat("  CHECK - ", paste(theme_names[sizes < 2], collapse = ", "),
        " rests on a single item, so typing on it is typing on one answer.\n", sep = "")
  } else {
    cat("  ", K, " themes of ", paste(sizes, collapse = "/"), " items; ",
        sum(!is.na(tbl$theme)), " of ", n, " needs assigned.\n", sep = "")
  }
  if(nrow(out) > 0){
    cat("  ", nrow(out), " need(s) sit in no theme and are left out of the typing - ",
        "report them on their own rather than folding them in.\n", sep = "")
  }
  if(n_cross > 0){
    cat("  ", n_cross, " cross-loader(s): assigned, but their theme is less clean than it looks.\n",
        sep = "")
  }
  if(is.null(manual) && identical(theme_names, paste("Theme", seq_len(K)))){
    cat("  Name the themes with theme_names = before typing.\n")
  }
  cat("\n")

  seg[["needs"]][["themes"]] <- tbl
  seg[["needs"]][["theme_names"]] <- theme_names

  # a new assignment invalidates the typing and the cut built on the old one
  seg[["needs"]][["typing"]] <- NA
  seg[["needs"]][["cut"]]    <- NA

  seg
}
