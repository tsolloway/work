#' seg_needs_init
#'
#' @description Extends a segmentation project onto the needs path. A needs
#'   segmentation carries TWO units of analysis: the person (as in a standard
#'   seg) and the person-context grid produced by an importance loop. This
#'   function adds the `needs` slot that holds the loop definition, the derived
#'   basis column names and the theme assignment, and tags the object so
#'   downstream `seg_needs_*` functions can find them.
#'
#'   Call it on a seg that has already been through [seg_init()]. It creates no
#'   folders and writes no files - the standard three-folder layout is reused.
#'
#' @param seg A seg object from [seg_init()].
#' @param item_labels Character vector of need labels, in item order. Length
#'   sets how many items the loop is expected to carry.
#' @param context_labels Character vector of context labels, in context order
#'   (for a hearables study, the occasions).
#'
#' @return The seg object with a populated `needs` slot and class
#'   `analytic_needs` prepended.
#'
#' @export
seg_needs_init <- function(seg, item_labels, context_labels){

  if(!inherits(seg, "analytic_segmentation")){
    stop("seg_needs_init() expects a seg object from seg_init().", call. = FALSE)
  }

  if(length(item_labels) < 2)    stop("item_labels needs at least 2 items.", call. = FALSE)
  if(length(context_labels) < 2) stop("context_labels needs at least 2 contexts.", call. = FALSE)


  seg[["needs"]] <- list(

    # filled by seg_needs_stack()
    "loop" = list(
      "stem"           = NA_character_,
      "id_var"         = NA_character_,
      "item_labels"    = item_labels,
      "context_labels" = context_labels,
      "n_items"        = length(item_labels),
      "n_contexts"     = length(context_labels)
    ),

    # column names of the derived bases, filled by seg_needs_prepare()
    "vars" = list(
      "raw"       = NA,   # as answered, per grid
      "centred"   = NA,   # grid-centred
      "scaled"    = NA,   # item-scaled across grids  <- the typing basis
      "person"    = NA,   # person mean per item, carried onto every grid row
      "intensity" = NA    # grid mean across items - a real variable, not a nuisance
    ),

    "themes"   = NA,      # seg_needs_themes()
    "typing"   = NA,      # seg_needs_type()
    "reports"  = list()   # stack / variance / structure reports
  )

  # the person frame is seg$data$original, as always; the grid frame lands here
  seg[["data"]][["stacked"]] <- NA

  class(seg) <- unique(c("analytic_needs", class(seg)))

  message(
    "Needs path initialised: ", length(item_labels), " items x ",
    length(context_labels), " contexts."
  )

  seg
}
