# Internal helpers shared by the seg_needs_* stages.


# Resolve items given as index, variable name (any of the item families) or
# label into item positions. Stops on anything it cannot place, naming it -
# a silently dropped item would change a theme without anyone noticing.
needs_resolve_items <- function(seg, x){

  if(is.numeric(x)) return(as.integer(x))

  v    <- seg[["needs"]][["vars"]]
  labs <- seg[["needs"]][["loop"]][["item_labels"]]
  fams <- Filter(function(f) is.character(f) && length(f) == length(labs),
                 v[c("raw", "centred", "scaled", "person", "person_top")])

  idx <- vapply(x, function(one){
    for(f in fams){
      hit <- match(one, f)
      if(!is.na(hit)) return(hit)
    }
    hit <- match(one, labs)
    # c("Improve my mood", 6) arrives as c("Improve my mood", "6")
    if(is.na(hit) && grepl("^[0-9]+$", one) && as.integer(one) <= length(labs)) hit <- as.integer(one)
    hit
  }, integer(1))

  if(anyNA(idx)){
    stop("Cannot match item(s) to the loop: ", paste(x[is.na(idx)], collapse = ", "),
         ". Use an index, a need variable name or an item label.", call. = FALSE)
  }
  unname(idx)
}


# Columns that differ down a respondent's rows. Such a column has no single
# value for a person, so it can neither be collapsed to a person row nor read
# as a person attribute. Detected rather than listed so it stays right if the
# blocks change.
needs_grid_cols <- function(df, id = "person_id"){
  n_person <- dplyr::n_distinct(df[[id]])
  vars <- setdiff(names(df), id)
  varying <- vapply(vars, function(v) dplyr::n_distinct(df[[id]], df[[v]]) > n_person,
                    logical(1))
  vars[varying]
}


# The executed spec at person level: one row per respondent, grid-level columns
# removed. Every person-level read after the spec goes through this so the
# definition of "person-level" is the same one seg_needs_write_shell() uses.
needs_person_frame <- function(seg){
  df <- seg[["data"]][["with_shell"]]
  if(!is.data.frame(df)){
    stop("No executed spec found. Run seg_get_spec() first.", call. = FALSE)
  }
  if(!"person_id" %in% names(df)){
    stop("person_id is not on the executed frame - was the stacked file loaded?", call. = FALSE)
  }
  df <- df[setdiff(names(df), needs_grid_cols(df))]
  dplyr::distinct(df, .data$person_id, .keep_all = TRUE)
}


# The typed grids a person-level stage works from. decisive_only drops the
# near-ties; ties = "exclude" has already set their theme to NA, so they drop
# either way.
needs_typed_grids <- function(seg, decisive_only = FALSE){

  typing <- seg[["needs"]][["typing"]]
  if(!is.list(typing) || !is.data.frame(typing[["grids"]])){
    stop("No typing found. Run seg_needs_type() first.", call. = FALSE)
  }

  if(decisive_only && identical(typing[["ties"]], "first")){
    stop("The typing was run with ties = 'first', which resolves near-ties as if ",
         "they were decisive. Re-type with ties = 'flag' to use decisive_only.",
         call. = FALSE)
  }

  g <- typing[["grids"]]
  keep <- !is.na(g$theme)
  if(decisive_only) keep <- keep & !g$near_tie
  g[keep, , drop = FALSE]
}


# Distinct themes per person, fast enough to sit inside a permutation loop.
# pid must be an integer code 1..P.
needs_n_distinct <- function(pid, theme, n_person){
  first <- !duplicated(cbind(pid, theme))
  tabulate(pid[first], nbins = n_person)
}


# Spec variables built from the needs loop - the loop columns, the item
# columns or the derived bases. Profiling a needs typing against these only
# restates it.
needs_basis_vars <- function(seg){

  v    <- seg[["needs"]][["vars"]]
  stem <- seg[["needs"]][["loop"]][["stem"]]
  own  <- unlist(Filter(is.character, v))

  specs <- dplyr::bind_rows(
    tidyr::unnest(seg[["spec"]][["polars"]],   cols = "vars"),
    tidyr::unnest(seg[["spec"]][["profiles"]], cols = "vars")
  )
  if(!"source_var" %in% names(specs)) return(character(0))

  from_loop <- vapply(strsplit(specs$source_var, ",\\s*"), function(src){
    src <- trimws(src)
    any(src %in% own) || (!is.na(stem) && any(startsWith(src, stem)))
  }, logical(1))

  specs$var[from_loop]
}


# Join a grid typing onto every frame that holds grids, replacing any earlier
# typing. The stacked frame always; seg$data$original and with_shell only when
# the grid is the unit, since only then are they grid frames.
needs_attach_typing <- function(seg, grids){

  cols <- setdiff(names(grids), c("person_id", "context"))

  attach <- function(df){
    if(!is.data.frame(df) || !all(c("person_id", "context") %in% names(df))) return(df)
    df <- df[setdiff(names(df), c(cols, grep("^need_theme", names(df), value = TRUE)))]
    dplyr::left_join(df, grids, by = c("person_id", "context"))
  }

  seg[["data"]][["stacked"]] <- attach(seg[["data"]][["stacked"]])
  if(identical(seg[["meta"]][["unit"]], "grid")){
    seg[["data"]][["original"]]   <- attach(seg[["data"]][["original"]])
    seg[["data"]][["with_shell"]] <- attach(seg[["data"]][["with_shell"]])
  }

  # a new typing invalidates any cut built on the old one
  seg[["needs"]][["cut"]] <- NA
  seg
}


# The typing as the person-layer stages read it: short names, logical flags.
needs_typing_table <- function(grids){
  grids %>%
    dplyr::rename(theme = "need_theme", top = "need_theme_top", second = "need_theme_second",
                  margin = "need_theme_margin", near_tie = "need_theme_near_tie",
                  flat = "need_theme_flat") %>%
    dplyr::mutate(near_tie = .data$near_tie == 1L, flat = .data$flat == 1L)
}


# Adjusted Rand index between two partitions of the same grids.
needs_ari <- function(a, b){
  ok <- !is.na(a) & !is.na(b)
  t  <- table(a[ok], b[ok])
  n  <- sum(t)
  s  <- sum(choose(t, 2))
  sa <- sum(choose(rowSums(t), 2))
  sb <- sum(choose(colSums(t), 2))
  e  <- sa * sb / choose(n, 2)
  (s - e) / ((sa + sb) / 2 - e)
}


# What a typed grid is called in output - "theme" or "state" - so the person
# layer reads right whichever typing it was built on.
needs_unit <- function(seg){
  if(identical(seg[["needs"]][["typing"]][["source"]], "states")) "state" else "theme"
}


# The near-tie rule, shared by the theme typing and the need states so a
# "close call" means the same thing in both. `m` is the margins of the
# differentiated grids only. A NULL tie_margin takes the `share` percentile
# (10th by default), so the closest-called share are flagged; grids ON that
# percentile are flagged too, because a block of identical margins cannot be
# split. A set value keeps its strict "below" meaning.
needs_tie_cutoff <- function(m, tie_margin = NULL, share = 0.10, rule = NULL){
  derived <- is.null(tie_margin)
  cutoff  <- if(derived) stats::quantile(m, share, names = FALSE) else tie_margin
  q <- stats::quantile(m, c(.25, .5, .75), names = FALSE)
  if(is.null(rule)){
    rule <- if(derived) paste0("closest ", round(100 * share, 1), "% of differentiated grids") else "set"
  }
  list(
    cutoff  = cutoff,
    rule    = rule,
    derived = derived,
    margins = c(q25 = q[1], median = q[2], q75 = q[3])
  )
}

needs_is_near <- function(m, tc){
  if(tc$derived) m <= tc$cutoff + 1e-9 else m < tc$cutoff
}
