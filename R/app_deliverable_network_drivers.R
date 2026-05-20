# =============================================================================
# Network Drivers module — returns module definition for app_deliverable()
# =============================================================================

#' app_deliverable_network_drivers
#'
#' @description Creates a Network Drivers analysis module for use with
#'   \code{app_deliverable()}. Mirrors the content of \code{bn_report()}
#'   (network visualizations, community membership, optional impact and
#'   prioritization dashboards) but returns a module definition list shaped
#'   like \code{app_deliverable_add_turf()} so the result can be rendered
#'   inside an app deliverable alongside other modules.
#'
#'   Each named entry in \code{results} becomes its own \code{nav_panel}.
#'   Within each panel, the sidebar exposes a Layout dropdown (Dynamic /
#'   Gravity / Charge / Hierarchy, depending on \code{types}) and a View
#'   selector (Attribute / Community / Membership, plus Impacts and
#'   Prioritization when \code{add_additional_results = TRUE}).
#'
#' @param results A named list of \code{bn_finalize_network()} outputs, or a
#'   single such output. If a single unnamed result is passed it is wrapped
#'   in a one-element list named "Network".
#' @param types Character vector. Layout types to expose. One or more of
#'   \code{"none"} (Dynamic), \code{"gravity"}, \code{"charge"},
#'   \code{"hierarchy"}.
#' @param do_community Logical vector. Which views to render. \code{c(TRUE, FALSE)}
#'   exposes Attribute, Community, and Membership tabs; passing only \code{TRUE}
#'   or only \code{FALSE} restricts to that single view.
#' @param default_type Character. Layout type to select on first render.
#'   Defaults to \code{"gravity"}.
#' @param gravity_constant,central_gravity,charge_layout,add_key,physics,seed
#'   Forwarded to \code{bn_visual()}.
#' @param add_additional_results Logical. When \code{TRUE}, adds Attribute
#'   Impacts, Community Impacts, and Prioritization tabs to the View selector
#'   for any result that carries the corresponding data. Defaults to
#'   \code{FALSE}.
#' @param qc_mode Logical. Forwarded to the impact dashboard helper.
#' @param impact_outcome_display Character or \code{NULL}. \code{"Point Change"}
#'   or \code{"\% Change"}; \code{NULL} auto-detects from the DV type.
#' @param shift_type Character. Initial value for the impact dashboard's
#'   Shift Type dropdown. One of \code{"absolute"}, \code{"proportional"},
#'   \code{"headroom"}, \code{"range"}.
#' @param add_prioritization_pvalue Logical. Show the p-value column on the
#'   prioritization dashboard. Defaults to \code{FALSE}.
#' @param prioritize_display Character or \code{NULL}. Initial value for the
#'   prioritization dashboard's Display dropdown.
#' @param sig_threshold,marginal_threshold Numeric. Significance thresholds
#'   forwarded to the prioritization dashboard.
#' @param title Character or \code{NULL}. Title shown on the navbar tab for
#'   this module. \code{NULL} (default) uses the single result's name when
#'   one result is supplied, or \code{"Network Drivers"} when multiple.
#' @param id Character or \code{NULL}. Module id; auto-generated if NULL.
#'
#' @return A module definition list with elements \code{id}, \code{tabs},
#'   \code{server}, \code{css}, and \code{head_tags}. Pass to
#'   \code{app_deliverable(modules = list(...))}.
#'
#' @examples
#' \dontrun{
#' # Single result
#' net <- bn_finalize_network(bn_results)
#' mod <- app_deliverable_network_drivers(
#'   results = list(Network = net),
#'   add_additional_results = TRUE
#' )
#' app_deliverable(title = "Driver Analysis", modules = list(mod))
#'
#' # Comparison between subgroups
#' app_deliverable_network_drivers(
#'   results = list(Experimental = net_exp, Control = net_ctrl),
#'   types = c("gravity", "charge"),
#'   default_type = "gravity"
#' )
#' }
#'
#' @export
app_deliverable_network_drivers <- function(
    results,
    types = c("none", "gravity", "charge", "hierarchy"),
    do_community = c(TRUE, FALSE),
    default_type = "gravity",
    gravity_constant = -9000,
    central_gravity = 0.2,
    charge_layout = "layout_with_fr",
    add_key = TRUE,
    physics = FALSE,
    add_additional_results = FALSE,
    qc_mode = FALSE,
    impact_outcome_display = NULL,
    shift_type = c("absolute", "proportional", "headroom", "range"),
    add_prioritization_pvalue = FALSE,
    prioritize_display = NULL,
    sig_threshold = 0.05,
    marginal_threshold = 0.10,
    seed = 1,
    title = NULL,
    id = NULL
) {

  # ---- Generate unique module ID ----
  if (is.null(id)) {
    id <- paste0("network_drivers_", substr(uuid::UUIDgenerate(), 1, 8))
  }
  ns <- shiny::NS(id)

  # ---- Validate / normalize args ----
  shift_type <- match.arg(shift_type)
  outcome_display <- if (!is.null(impact_outcome_display)) {
    impact_outcome_display <- match.arg(impact_outcome_display,
      c("Point Change", "% Change"))
    if (impact_outcome_display == "Point Change") "absolute" else "proportional"
  } else NULL

  if (is.null(default_type)) default_type <- types[1]
  default_type <- match.arg(default_type, types)

  # Allow callers to pass a single bn_finalize_network() result; wrap.
  results <- .network_drivers_normalize_results(results)

  type_labels <- vapply(types, function(t) {
    switch(t, none = "Dynamic", gravity = "Gravity",
           charge = "Charge", hierarchy = "Hierarchy", t)
  }, character(1))
  type_choices <- stats::setNames(types, type_labels)
  default_type_label <- unname(type_labels[match(default_type, types)])

  has_attr <- isTRUE(FALSE %in% do_community) || identical(do_community, FALSE)
  has_comm <- isTRUE(TRUE  %in% do_community) || identical(do_community, TRUE)
  # Membership is a community-grouping view — only meaningful when both
  # community-detection and the standard attribute view are present.
  has_memb <- has_attr && has_comm

  # ---- Build per-result nav_panels ----
  result_names <- names(results)
  tabs <- purrr::map(result_names, function(rname) {
    .network_drivers_panel(
      ns = ns, result_name = rname, result = results[[rname]],
      type_choices = type_choices, default_type = default_type,
      default_type_label = default_type_label,
      has_attr = has_attr, has_comm = has_comm, has_memb = has_memb,
      add_additional_results = add_additional_results,
      add_prioritization_pvalue = add_prioritization_pvalue
    )
  })

  # ---- Pre-render dashboards (impacts + prioritization) ----
  # bn_report's helpers emit static HTML + inline JS that handles dropdowns
  # and table updates client-side. Render once at module-construction time
  # and embed via htmltools::HTML in the View-switcher output.
  dashboards <- list()
  for (rname in result_names) {
    rd <- list()
    if (isTRUE(add_additional_results)) {
      res <- results[[rname]]
      impacts_res <- res[["impacts"]]
      priort_res  <- res[["prioritizations"]]
      dash_id <- paste0(id, "-", .network_drivers_safe_id(rname))

      # Stage 2 (option e): for impact tabs we no longer render bn_report's
      # HTML dashboard. Instead store metadata; the module renders Shiny
      # inputs in the sidebar + a reactive DT::datatable in the tab body.
      if (has_attr && !is.null(impacts_res) &&
          !is.null(impacts_res[["table_attribute"]])) {
        rd$impacts_attr_meta <- .bn_report_impacts_metadata(
          impacts_res, is_community = FALSE
        )
      }
      if (has_comm && !is.null(impacts_res) &&
          !is.null(impacts_res[["table_community"]])) {
        rd$impacts_comm_meta <- .bn_report_impacts_metadata(
          impacts_res, is_community = TRUE
        )
      }
      if (!is.null(priort_res)) {
        rd$prio_meta <- .bn_report_prio_metadata(
          priort_res,
          add_prioritization_pvalue = add_prioritization_pvalue
        )
      }
    }
    dashboards[[rname]] <- rd
  }

  # ---- CSS ----
  # bn_report CSS for the embedded impact / prioritization / membership
  # dashboards, plus a small set of overrides to harmonize the embedded
  # tables with the surrounding bslib chrome.
  css <- paste(
    .bn_report_css(),
    .network_drivers_module_css(id),
    sep = "\n"
  )

  # ---- head_tags: bn_report JS for embedded dashboards + parent-side
  # listener that forwards iframe postMessage layouts to a Shiny input.
  # `shinyjs::useShinyjs()` registers the shinyjs JS bridge so server-side
  # `shinyjs::disable()` / `enable()` calls reach the browser.
  head_tags <- list(
    shinyjs::useShinyjs(),
    shiny::tags$script(shiny::HTML(.bn_report_js(save_name = NULL))),
    shiny::tags$script(shiny::HTML(.network_drivers_parent_listener_js(id)))
  )

  # ---- Server function ----
  server_fn <- function(input, output, session, dark_mode) {
    .network_drivers_module_server(
      input, output, session,
      dark_mode = dark_mode,
      results = results, dashboards = dashboards,
      types = types, type_labels = type_labels,
      default_type = default_type,
      has_attr = has_attr, has_comm = has_comm, has_memb = has_memb,
      add_additional_results = add_additional_results,
      gravity_constant = gravity_constant,
      central_gravity = central_gravity,
      charge_layout = charge_layout,
      add_key = add_key, physics = physics, seed = seed
    )
  }

  list(
    id        = id,
    tabs      = tabs,
    server    = server_fn,
    css       = css,
    head_tags = head_tags
  )
}


# =============================================================================
# UI: per-result nav_panel
# =============================================================================

#' @noRd
.network_drivers_panel <- function(ns, result_name, result, type_choices,
                                   default_type, default_type_label,
                                   has_attr, has_comm, has_memb,
                                   add_additional_results,
                                   add_prioritization_pvalue = FALSE) {

  rid <- .network_drivers_safe_id(result_name)
  layout_id <- paste0(rid, "_layout")
  view_id   <- paste0(rid, "_view")

  # Which view tabs are available depends on the result's contents.
  impacts_res <- result[["impacts"]]
  priort_res  <- result[["prioritizations"]]
  has_impacts_attr <- isTRUE(add_additional_results) && has_attr &&
    !is.null(impacts_res) && !is.null(impacts_res[["table_attribute"]])
  has_impacts_comm <- isTRUE(add_additional_results) && has_comm &&
    !is.null(impacts_res) && !is.null(impacts_res[["table_community"]])
  has_prio <- isTRUE(add_additional_results) && !is.null(priort_res)

  # Pre-compute metadata at panel-build time so the sidebar can be sized to
  # the actual choice lists. (Server also recomputes when needed.)
  ia_meta <- if (has_impacts_attr) {
    .bn_report_impacts_metadata(impacts_res, is_community = FALSE)
  } else NULL
  ic_meta <- if (has_impacts_comm) {
    .bn_report_impacts_metadata(impacts_res, is_community = TRUE)
  } else NULL
  pm_meta <- if (has_prio) {
    .bn_report_prio_metadata(priort_res,
                             add_prioritization_pvalue = add_prioritization_pvalue)
  } else NULL

  # Default view = first enabled tab.
  default_view <-
    if (has_attr) "attribute"
    else if (has_comm) "community"
    else if (has_memb) "membership"
    else if (has_impacts_attr) "impacts_attr"
    else if (has_impacts_comm) "impacts_comm"
    else if (has_prio) "prioritization"
    else "attribute"

  # Sidebar: Layout (network tabs only) + impact controls (impact tabs).
  view_input_js <- sprintf("input['%s']", ns(view_id))
  layout_panel <- shiny::conditionalPanel(
    condition = sprintf("%s === 'attribute' || %s === 'community'",
                        view_input_js, view_input_js),
    shiny::selectInput(
      ns(layout_id), "Layout:",
      choices = type_choices, selected = default_type
    )
  )
  # Shared impact controls (Assess, Analysis, Focus, Outcome, Shift,
  # Weight) drive BOTH attribute and community impact tabs — show the
  # same panel when either tab is active. Index By stays attribute-only
  # because community has no batteries.
  shared_meta <- ia_meta %||% ic_meta
  shared_views <- c(if (has_impacts_attr) "impacts_attr",
                    if (has_impacts_comm) "impacts_comm")
  imp_sidebar <- if (length(shared_views) > 0) {
    .network_drivers_impacts_sidebar(
      ns, prefix = paste0(rid, "_imp"), metadata = shared_meta,
      view_value = shared_views, view_input_id = ns(view_id),
      include_indexby = FALSE
    )
  } else NULL
  ia_indexby <- if (has_impacts_attr) {
    .network_drivers_impacts_indexby_input(
      ns, prefix = paste0(rid, "_ia"), metadata = ia_meta,
      view_value = "impacts_attr", view_input_id = ns(view_id)
    )
  } else NULL
  pm_sidebar <- if (has_prio) {
    .network_drivers_prio_sidebar(
      ns, prefix = paste0(rid, "_pm"), metadata = pm_meta,
      view_value = "prioritization", view_input_id = ns(view_id)
    )
  } else NULL
  sidebar <- bslib::sidebar(
    width = 240,
    layout_panel,
    imp_sidebar,
    ia_indexby,
    pm_sidebar,
    # Divider + workbook download — pinned to the BOTTOM of the sidebar.
    # bslib::sidebar's content area is a flex column, so `margin-top:
    # auto` on this wrapper consumes all remaining vertical space and
    # pushes itself (with the hr + download button) to the bottom.
    # Controls above stay at the top with their natural heights.
    shiny::div(
      style = "margin-top: auto;",
      htmltools::hr(style = "margin: 12px 0;"),
      shiny::downloadButton(
        ns(paste0(rid, "_download_workbook")),
        label = "Download Workbook",
        icon  = NULL,
        # btn-rdx — brand-shared button class (resondex_css). width:100%
        # is a per-instance layout choice (sidebar fill) and stays here.
        class = "btn-rdx",
        style = "width: 100%;"
      )
    )
  )

  # Build nav_panels in the order Attribute / Community / Membership /
  # Impacts (attr) / Impacts (comm) / Prioritization. Each network view
  # panel is a uiOutput the server fills with all layout slots upfront.
  panels <- list()
  if (has_attr) {
    panels <- c(panels, list(bslib::nav_panel(
      title = "Attribute", value = "attribute",
      shiny::uiOutput(ns(paste0(rid, "_attr_content")),
                      style = "min-height: 70vh;")
    )))
  }
  if (has_comm) {
    panels <- c(panels, list(bslib::nav_panel(
      title = "Community", value = "community",
      shiny::uiOutput(ns(paste0(rid, "_comm_content")),
                      style = "min-height: 70vh;")
    )))
  }
  if (has_memb) {
    # Mirror bn_report's two-view membership panel: a card grid (default)
    # and a table view, toggled by a single button. Both views are rendered
    # into the DOM; JS toggles display so the existing MutationObserver
    # sync layer (which re-applies user label edits to .community-label
    # [data-color] and [data-node-id]) keeps working across both views.
    panels <- c(panels, list(bslib::nav_panel(
      title = "Membership", value = "membership",
      shiny::div(
        class      = "network-drivers-membership",
        `data-rid` = rid,
        style      = "padding: 12px;",
        shiny::div(
          class = "membership-wrap",
          shiny::div(
            class = "membership-toolbar",
            shiny::tags$button(
              # btn-rdx — brand-shared button class defined in
              # resondex_css(). Surfaces, padding, font-size, radius, hover,
              # and focus all flow from there so this button always tracks
              # the brand without per-callsite styling.
              class   = "btn-rdx",
              type    = "button",
              onclick = "toggleMembershipView(this)",
              title   = "Switch view",
              shiny::HTML("&#9776; Toggle View")
            )
          ),
          shiny::div(
            class = "membership-view membership-card-view",
            shiny::uiOutput(ns(paste0(rid, "_membership_cards")))
          ),
          shiny::div(
            class = "membership-view membership-table-view",
            style = "display:none;",
            reactable::reactableOutput(
              ns(paste0(rid, "_membership_table"))
            )
          )
        )
      )
    )))
  }
  # Flex column on card_body: reactable area grows to fill the space
  # left after the footer takes its natural height. Eliminates the
  # different-cap-vs-different-footer mismatch (impacts has a small
  # footer, prio has a large glossary) — both cards end up the same
  # height, both internally scroll their tables to fit.
  flex_card_body_style <- paste(
    "display: flex; flex-direction: column;",
    "height: 100%; min-height: 0;"
  )
  if (has_impacts_attr) {
    panels <- c(panels, list(bslib::nav_panel(
      title = "Attribute Impacts", value = "impacts_attr",
      # Wrap in layout_columns so the card sits in a *fillable* cell with a
      # definite height. Without it, nav_panel's content area falls back to
      # auto height → the flex chain (card → card_body → wrapper →
      # reactable height:100%) resolves to auto and reactable renders all
      # rows. Mirrors the prio table's enable_chart=TRUE path.
      bslib::layout_columns(
        col_widths = 12,
        bslib::card(
          full_screen = TRUE,
          max_height  = "80vh",
          bslib::card_header(
            shiny::uiOutput(ns(paste0(rid, "_ia_title")), inline = TRUE),
            class = "fw-semibold"
          ),
          bslib::card_body(
            padding = 0,
            style = flex_card_body_style,
            shiny::div(
              style = "flex: 1 1 auto; min-height: 0; overflow: hidden;",
              reactable::reactableOutput(ns(paste0(rid, "_ia_dt")),
                                         width = "100%", height = "100%")
            ),
            shiny::div(
              style = "flex: 0 0 auto;",
              shiny::uiOutput(ns(paste0(rid, "_ia_footer")))
            )
          )
        )
      )
    )))
  }
  if (has_impacts_comm) {
    panels <- c(panels, list(bslib::nav_panel(
      title = "Community Impacts", value = "impacts_comm",
      # Same fillable-cell wrapper as Attribute Impacts — see comment there.
      bslib::layout_columns(
        col_widths = 12,
        bslib::card(
          full_screen = TRUE,
          max_height  = "80vh",
          bslib::card_header(
            shiny::uiOutput(ns(paste0(rid, "_ic_title")), inline = TRUE),
            class = "fw-semibold"
          ),
          bslib::card_body(
            padding = 0,
            style = flex_card_body_style,
            shiny::div(
              style = "flex: 1 1 auto; min-height: 0; overflow: hidden;",
              reactable::reactableOutput(ns(paste0(rid, "_ic_dt")),
                                         width = "100%", height = "100%")
            ),
            shiny::div(
              style = "flex: 0 0 auto;",
              shiny::uiOutput(ns(paste0(rid, "_ic_footer")))
            )
          )
        )
      )
    )))
  }
  if (has_prio) {
    # Side-by-side table + chart layout via layout_columns. Flip to
    # FALSE to debug the table card in isolation.
    enable_chart <- TRUE

    table_card <- bslib::card(
      full_screen = TRUE,
      max_height  = "80vh",
      bslib::card_header(
        shiny::uiOutput(ns(paste0(rid, "_pm_title")), inline = TRUE),
        class = "fw-semibold"
      ),
      bslib::card_body(
        padding = 0,
        style = flex_card_body_style,
        shiny::div(
          style = "flex: 1 1 auto; min-height: 0; overflow: hidden;",
          reactable::reactableOutput(ns(paste0(rid, "_pm_dt")),
                                     width = "100%", height = "100%")
        ),
        shiny::div(
          style = "flex: 0 0 auto;",
          shiny::uiOutput(ns(paste0(rid, "_pm_footer")))
        )
      )
    )

    chart_card <- bslib::card(
      full_screen = TRUE,
      max_height  = "80vh",
      bslib::card_header("Prioritization Chart", class = "fw-semibold"),
      bslib::card_body(
        padding = 0,
        plotly::plotlyOutput(ns(paste0(rid, "_pm_chart")),
                             width = "100%", height = "500px")
      )
    )

    prio_body <- if (isTRUE(enable_chart)) {
      bslib::layout_columns(
        col_widths = bslib::breakpoints(
          md  = c(12, 12),
          xxl = c(7, 5)
        ),
        table_card, chart_card
      )
    } else {
      table_card
    }

    panels <- c(panels, list(bslib::nav_panel(
      title = "Prioritization", value = "prioritization",
      prio_body
    )))
  }

  bslib::nav_panel(
    title = result_name,
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar,
      do.call(bslib::navset_underline, c(
        list(id = ns(view_id), selected = default_view),
        panels
      ))
    )
  )
}


# =============================================================================
# Server
# =============================================================================

#' @noRd
.network_drivers_module_server <- function(
    input, output, session, dark_mode,
    results, dashboards, types, type_labels, default_type,
    has_attr, has_comm, has_memb, add_additional_results,
    gravity_constant, central_gravity, charge_layout,
    add_key, physics, seed
) {

  ns <- session$ns
  result_names <- names(results)

  # Per-module temp dir served via addResourcePath. All visNetwork widgets
  # are saved here once per session and embedded via <iframe src=...>.
  module_id <- sub("-$", "", ns(""))
  widget_dir <- file.path(tempdir(), paste0("netdrv_", module_id))
  if (!dir.exists(widget_dir)) dir.create(widget_dir, recursive = TRUE)
  resource_prefix <- paste0("netdrv_", module_id)
  shiny::addResourcePath(resource_prefix, widget_dir)
  shiny::onSessionEnded(function() {
    try(unlink(widget_dir, recursive = TRUE), silent = TRUE)
  })

  # ---- Layout / position state (cross-iframe, cross-session) ----
  # `positions` is a list keyed by "<result>|<layout>|<view>", each value a
  # named list of {x, y} per node id. Updated whenever an iframe pushes a
  # new snapshot (via dragEnd / stabilized → snapshotPush postMessage).
  # Iframes are MOUNT-ONCE — they never get re-rendered for layout/view
  # switches. Loading saved positions into iframes happens via the
  # `applyReportLoad` postMessage pipe (handled below), not by passing
  # node_positions to bn_visual on render.
  positions      <- shiny::reactiveVal(list())
  reload_trigger <- shiny::reactiveVal(0L)

  positions_to_snapshot <- function(pos) {
    nodes <- lapply(names(pos), function(id) {
      list(id = id,
           x  = as.numeric(pos[[id]]$x),
           y  = as.numeric(pos[[id]]$y))
    })
    list(nodes = nodes)
  }

  # Observer: incoming snapshotPush from any iframe → store in positions().
  shiny::observeEvent(input$positions_in, {
    payload <- input$positions_in
    if (!is.list(payload) || is.null(payload$nsKey) ||
        is.null(payload$data) || is.null(payload$data$nodes)) return()
    nodes <- payload$data$nodes
    pos <- list()
    for (n in nodes) {
      if (is.null(n$id)) next
      x <- n$x; y <- n$y
      if (is.null(x) || is.null(y)) next
      pos[[as.character(n$id)]] <- list(x = as.numeric(x), y = as.numeric(y))
    }
    if (length(pos) == 0) return()
    cur <- positions()
    cur[[payload$nsKey]] <- pos
    positions(cur)
  }, ignoreInit = TRUE)

  # Observer: iframe announces it's ready → if we have saved positions for
  # that key, push them via applyReportLoad. Handles the initial-mount
  # case where state was restored before iframes finished loading.
  shiny::observeEvent(input$iframe_ready, {
    nsKey <- input$iframe_ready
    if (!is.character(nsKey) || !nzchar(nsKey)) return()
    pos_all <- positions()
    if (is.null(pos_all[[nsKey]])) return()
    session$sendCustomMessage("netdrv_apply_to_key", list(
      key = nsKey,
      snapshot = positions_to_snapshot(pos_all[[nsKey]])
    ))
  }, ignoreInit = TRUE)

  # Observer: explicit reload (set_state / reset) → broadcast all stored
  # positions to their matching iframes. Iframes are already mounted by
  # this point so contentWindow / message listeners are live.
  shiny::observeEvent(reload_trigger(), {
    pos_all <- positions()
    if (length(pos_all) == 0) return()
    snapshots <- lapply(pos_all, positions_to_snapshot)
    session$sendCustomMessage("netdrv_apply_all", list(snapshots = snapshots))
  }, ignoreInit = TRUE)

  # Observer: dark/light mode change → broadcast 'setMode' to every
  # network iframe so each one flips its data-bs-theme. CSS-var-driven
  # surfaces inside the iframe (legend, toolbar buttons, Select-by-ID,
  # body bg) adapt automatically via the brand layer's dark overrides.
  # Stage 1 only — the visNetwork canvas itself stays as authored.
  # Runs on initial value too (no ignoreInit) so iframes that mount in
  # an already-dark app catch up on first paint.
  shiny::observeEvent(dark_mode(), {
    mode <- if (isTRUE(dark_mode())) "dark" else "light"
    session$sendCustomMessage("netdrv_set_mode", mode)
  })

  # When an iframe announces ready, push the current mode immediately
  # so a freshly-mounted iframe in dark mode flips before the user sees
  # any light-mode flash. Independent of the observer above (which only
  # fires on dark_mode CHANGES — a fresh iframe needs the CURRENT mode).
  shiny::observeEvent(input$iframe_ready, {
    mode <- if (isTRUE(dark_mode())) "dark" else "light"
    session$sendCustomMessage("netdrv_set_mode", mode)
  })

  # Build per-result content (all layout iframes for attribute & community
  # tabs, plus membership / dashboards). View tab switching is handled
  # natively by bslib::navset_card_tab. Within attribute & community tabs,
  # all layout slots mount upfront and the layout selectInput drives a JS
  # toggle to flip display.
  for (rname in result_names) {
    local({
      result_name <- rname
      result      <- results[[rname]]
      dash        <- dashboards[[rname]]
      rid         <- .network_drivers_safe_id(result_name)
      layout_id   <- paste0(rid, "_layout")
      view_id     <- paste0(rid, "_view")
      attr_content_id <- paste0(rid, "_attr_content")
      comm_content_id <- paste0(rid, "_comm_content")
      memb_out_id     <- paste0(rid, "_membership_table")
      memb_cards_id   <- paste0(rid, "_membership_cards")
      ia_content_id   <- paste0(rid, "_impacts_attr_content")
      ic_content_id   <- paste0(rid, "_impacts_comm_content")
      prio_content_id <- paste0(rid, "_prio_content")

      initial_layout <- default_type

      # ---- Build network iframe slot for one (layout, view) ----
      build_net_slot <- function(layout_type, view_type) {
        key <- paste(result_name, layout_type, view_type, sep = "|")
        do_comm <- identical(view_type, "community")
        is_default_slot <- (layout_type == initial_layout)
        slot_style <- paste0(
          "height: 100%; display: ",
          if (is_default_slot) "block" else "none", ";"
        )

        viz <- tryCatch(
          bn_visual(
            obj              = result,
            type             = layout_type,
            do_community     = do_comm,
            vs_height        = "95vh",
            vs_width         = "100%",
            interactive      = TRUE,
            physics          = TRUE,
            gravity_constant = gravity_constant,
            central_gravity  = central_gravity,
            charge_layout    = charge_layout,
            add_key          = add_key && !do_comm,
            panel_ns         = key,
            save_visuals     = FALSE,
            seed             = seed
          ),
          error = function(e) {
            warning("network_drivers: bn_visual failed for [",
                    key, "]: ", conditionMessage(e))
            NULL
          }
        )

        if (is.null(viz)) {
          return(shiny::div(
            class = "netdrv-slot netdrv-network-slot network-drivers-empty",
            `data-rid` = rid,
            `data-layout` = layout_type,
            `data-view` = view_type,
            style = slot_style,
            "Could not render this view."
          ))
        }

        widget_name <- paste0(rid, "__", layout_type, "__",
                              view_type, ".html")
        widget_path <- file.path(widget_dir, widget_name)
        htmlwidgets::saveWidget(viz, file = widget_path,
                                selfcontained = FALSE)

        widget_html <- paste(readLines(widget_path, warn = FALSE),
                             collapse = "\n")
        injected <- paste0(
          "<head><style>",
          # Brand layer first (Inter @import must lead): the iframe is an
          # isolated document, so resondex_css() carries the --ndr-* tokens
          # + Inter + shared classes into it, matching the rest of the app.
          resondex_css(include_import = TRUE),
          # Body bg reads --ndr-card-bg so the iframe paints its own
          # surface (matches the rest of the app). Synchronous data-bs-
          # theme sync (inline script below in inject_head) prevents the
          # light-mode flash that used to happen before the postMessage
          # setMode arrived.
          "body,html{margin:0!important;padding:0!important;",
          "height:100%!important;overflow:hidden!important;",
          "background-color:var(--ndr-card-bg)!important;color:var(--ndr-text);}",
          " .htmlwidget{height:100%!important;background-color:transparent!important;}",
          " .vis-network,.vis-network canvas{background-color:transparent!important;}",
          " #pngButton,#svgButton,#fontButton,#physicsButton{width:130px!important;height:30px!important;}",
          "</style>",
          if (!isTRUE(physics)) {
            "<script>window.__disablePhysicsAfterStabilize=true;</script>"
          } else ""
        )
        widget_html <- sub("<head>", injected, widget_html, fixed = TRUE)
        writeLines(widget_html, widget_path)

        shiny::div(
          class = "netdrv-slot netdrv-network-slot",
          `data-rid` = rid,
          `data-layout` = layout_type,
          `data-view` = view_type,
          style = slot_style,
          shiny::tags$iframe(
            src = paste0(resource_prefix, "/", widget_name),
            `data-key`    = key,
            `data-rid`    = rid,
            `data-view`   = view_type,
            `data-layout` = layout_type,
            style = "width: 100%; height: 80vh; border: none; display: block;",
            sandbox = "allow-scripts allow-downloads",
            allowfullscreen = NA
          )
        )
      }

      # ---- Render attribute / community uiOutputs ----
      if (has_attr) {
        attr_slots <- lapply(types, build_net_slot, view_type = "attribute")
        attr_tag <- do.call(shiny::tagList, attr_slots)
        output[[attr_content_id]] <- shiny::renderUI({ shiny::isolate(attr_tag) })
      }
      if (has_comm) {
        comm_slots <- lapply(types, build_net_slot, view_type = "community")
        comm_tag <- do.call(shiny::tagList, comm_slots)
        output[[comm_content_id]] <- shiny::renderUI({ shiny::isolate(comm_tag) })
      }

      # ---- Membership card view ----
      if (has_memb) {
        output[[memb_cards_id]] <- shiny::renderUI({
          html <- .network_drivers_membership_cards_html(result)
          if (is.null(html)) {
            return(shiny::div(class = "extra-empty",
                              "No community membership available."))
          }
          shiny::HTML(html)
        })
      }

      # ---- Membership reactable (table view) ----
      if (has_memb) {
        output[[memb_out_id]] <- reactable::renderReactable({
          df <- .network_drivers_membership_df(result)
          if (is.null(df) || nrow(df) == 0) {
            return(reactable::reactable(
              data.frame(Message = "No community membership available."),
              sortable    = FALSE,
              pagination  = FALSE,
              highlight   = FALSE,
              borderless  = TRUE
            ))
          }
          reactable::reactable(
            df,
            pagination     = FALSE,
            sortable       = TRUE,
            highlight      = TRUE,
            compact        = FALSE,
            defaultColDef  = reactable::colDef(
              headerStyle = list(
                backgroundColor = "var(--ndr-card-bg)",
                color           = "var(--ndr-text)",
                fontWeight      = 600,
                borderBottom    = "1px solid var(--ndr-border)"
              )
            ),
            columns = list(
              Community = reactable::colDef(
                html       = TRUE,
                minWidth   = 180,
                align      = "left",
                headerStyle = list(
                  textAlign       = "left",
                  backgroundColor = "var(--ndr-card-bg)",
                  color           = "var(--ndr-text)",
                  fontWeight      = 600,
                  borderBottom    = "1px solid var(--ndr-border)"
                )
              ),
              Attributes = reactable::colDef(
                html       = TRUE,
                minWidth   = 400,
                align      = "left",
                headerStyle = list(
                  textAlign       = "left",
                  backgroundColor = "var(--ndr-card-bg)",
                  color           = "var(--ndr-text)",
                  fontWeight      = 600,
                  borderBottom    = "1px solid var(--ndr-border)"
                )
              ),
              Count = reactable::colDef(
                align    = "center",
                maxWidth = 90,
                headerStyle = list(
                  textAlign       = "center",
                  backgroundColor = "var(--ndr-card-bg)",
                  color           = "var(--ndr-text)",
                  fontWeight      = 600,
                  borderBottom    = "1px solid var(--ndr-border)"
                )
              )
            ),
            theme = reactable::reactableTheme(
              color           = "var(--ndr-text)",
              backgroundColor = "var(--ndr-card-bg)",
              borderColor     = "var(--ndr-border)",
              borderWidth     = "1px",
              stripedColor    = "var(--ndr-secondary-bg)",
              highlightColor  = "var(--ndr-secondary-bg)",
              cellPadding     = "8px 12px",
              style           = list(fontSize = "13px"),
              headerStyle     = list(
                backgroundColor = "var(--ndr-card-bg)",
                color           = "var(--ndr-text)",
                borderColor     = "var(--ndr-border)",
                fontWeight      = 600
              )
            )
          )
        })
        # The table view starts with display:none (card view is the
        # default). Without this, Shiny suspends the renderReactable
        # because the output container isn't visible — so clicking
        # "Toggle View" reveals an empty div. Force it to render
        # regardless of visibility.
        shiny::outputOptions(output, memb_out_id, suspendWhenHidden = FALSE)
      }

      # ---- Impact dashboards: native sidebar inputs + reactable ----
      # Shared control inputs (`_imp_*`) drive BOTH attribute and community
      # impact dashboards. The attribute table additionally reads
      # `_ia_indexby` (community has no Index By since it has no batteries).
      shared_prefix <- paste0(rid, "_imp")
      bind_impacts_dt <- function(meta, dt_out_id, footer_out_id,
                                  indexby_input_id = NULL) {
        if (is.null(meta)) return(invisible(NULL))

        display_reactive <- shiny::reactive({
          metric_key   <- input[[paste0(shared_prefix, "_metric")]]
          focus        <- input[[paste0(shared_prefix, "_focus")]]   %||% "Market"
          outcome_disp <- input[[paste0(shared_prefix, "_display")]] %||% "propdisplay"
          shift_t      <- input[[paste0(shared_prefix, "_shift")]]   %||% "propshift"
          weight_v     <- input[[paste0(shared_prefix, "_weight")]]  %||% "Unweighted"
          index_by     <- if (!is.null(indexby_input_id)) {
            input[[indexby_input_id]] %||% "All"
          } else "All"
          if (is.null(metric_key)) return(NULL)
          .network_drivers_impacts_data(
            meta,
            weight = weight_v, focus = focus,
            metric_key = metric_key,
            outcome_display = outcome_disp,
            shift_type = shift_t,
            index_by = index_by
          )
        })

        # Full re-render on every input change. Reactable handles this
        # without the visible flicker DT had — the React-based diffing
        # only patches changed rows, and the colDef-level style/footer
        # functions naturally recompute. No need for proxies / custom
        # message handlers.
        output[[dt_out_id]] <- reactable::renderReactable({
          disp <- display_reactive()
          .network_drivers_impacts_reactable(disp, meta)
        })

        # Footer notes: index-meaning paragraph + static formatting legend.
        # Re-renders on metric/shift change so the index-meaning sentence
        # always describes what the visible numbers mean.
        output[[footer_out_id]] <- shiny::renderUI({
          metric_key <- input[[paste0(shared_prefix, "_metric")]]
          shift_t    <- input[[paste0(shared_prefix, "_shift")]] %||% "propshift"
          if (is.null(metric_key)) return(NULL)
          .network_drivers_impacts_footer_notes(meta, metric_key, shift_t)
        })
      }
      # Attribute table: shared inputs + attr-only Index By
      bind_impacts_dt(
        dash$impacts_attr_meta,
        dt_out_id        = paste0(rid, "_ia_dt"),
        footer_out_id    = paste0(rid, "_ia_footer"),
        indexby_input_id = paste0(rid, "_ia_indexby")
      )
      # Community table: shared inputs only (no Index By)
      bind_impacts_dt(
        dash$impacts_comm_meta,
        dt_out_id     = paste0(rid, "_ic_dt"),
        footer_out_id = paste0(rid, "_ic_footer")
      )

      # Sidebar feedback: dynamic warnings/questions that mirror
      # bn_report's per-control hints. Registered once per result against
      # the shared `_imp_*` inputs. Each warning is a tiny renderUI that
      # reads the relevant inputs + metadata and emits text (or "" to hide).
      shared_meta_warn <- dash$impacts_attr_meta %||% dash$impacts_comm_meta
      if (!is.null(shared_meta_warn)) {
        # Card titles: "{Assess value}: <em>{Question}</em>" for impact tables.
        # Falls back to bare Assess value (e.g., "Custom Impact") when no question.
        make_title <- function(out_id) {
          output[[out_id]] <- shiny::renderUI({
            v <- input[[paste0(shared_prefix, "_assess")]] %||% ""
            q <- .network_drivers_impacts_assess_question(shared_meta_warn, v)
            # Explicit brand text colour so the card-header stays legible
            # in both light AND dark modes (Bootstrap card-header default
            # inherits a dim colour in dark).
            wrap <- function(...) htmltools::tags$span(
              style = "color: var(--ndr-text); font-weight: 600;", ...)
            if (nzchar(v) && nzchar(q)) {
              wrap(
                htmltools::HTML(paste0(htmltools::htmlEscape(v), ": ")),
                htmltools::tags$em(q)
              )
            } else {
              wrap(htmltools::HTML(htmltools::htmlEscape(v)))
            }
          })
        }
        if (!is.null(dash$impacts_attr_meta)) make_title(paste0(rid, "_ia_title"))
        if (!is.null(dash$impacts_comm_meta)) make_title(paste0(rid, "_ic_title"))

        # Disable inert controls — when a control's value would have no
        # effect on the displayed metric, grey it out via shinyjs.
        # Conditions mirror the advisory rules from bn_report:
        #   - fixed-step shift (Fixed Step / % of Range) + lift metric
        #     → focus/weight inert (the shift is a constant increment
        #       independent of the distribution)
        #   - metric = mi/maxVmin → shift inert; weight inert
        #   - metric = mi → display inert
        shiny::observe({
          metric_key <- input[[paste0(shared_prefix, "_metric")]]
          shift_t    <- input[[paste0(shared_prefix, "_shift")]]   %||% "propshift"
          outcome_d  <- input[[paste0(shared_prefix, "_display")]] %||% "propdisplay"
          if (is.null(metric_key)) return()
          is_lift <- !metric_key %in% c("maxVmin", "mi")
          # Focus/weight are inert ONLY when the shift is fixed-step
          # (absshift / rangeshift) AND a lift metric AND Outcome =
          # Point Change. Under % Change the figure is divided by the
          # focus/weight-specific baseline, so focus/weight always matter.
          shift_abs_lift <- shift_t %in% c("absshift", "rangeshift") &&
            is_lift && identical(outcome_d, "absdisplay")

          set_disabled <- function(dim, disabled) {
            id <- paste0(shared_prefix, "_", dim)
            if (isTRUE(disabled)) shinyjs::disable(id) else shinyjs::enable(id)
          }
          set_disabled("focus",
                       metric_key %in% c("maxVmin", "mi") || shift_abs_lift)
          set_disabled("display", identical(metric_key, "mi"))
          set_disabled("shift",   metric_key %in% c("maxVmin", "mi"))
          set_disabled("weight",
                       metric_key %in% c("maxVmin", "mi") || shift_abs_lift)
        })
      }

      # Assess preset auto-flip — registered ONCE per result against the
      # shared `_imp_*` inputs (Assess + metric + shift live in the
      # shared sidebar block now). Use whichever metadata is available
      # for the preset_map lookup; both attribute and community share
      # the same preset definitions.
      shared_meta_srv <- dash$impacts_attr_meta %||% dash$impacts_comm_meta
      if (!is.null(shared_meta_srv) &&
          length(shared_meta_srv$preset_map) > 0) {
        assess_id <- paste0(shared_prefix, "_assess")
        metric_in <- paste0(shared_prefix, "_metric")
        shift_in  <- paste0(shared_prefix, "_shift")
        shiny::observeEvent(input[[assess_id]], {
          preset_name <- input[[assess_id]]
          if (is.null(preset_name) || preset_name == "Custom Impact") return()
          preset <- shared_meta_srv$preset_map[[preset_name]]
          if (is.null(preset)) return()
          if (!is.null(preset$metric) &&
              !identical(input[[metric_in]], preset$metric)) {
            shiny::updateSelectInput(session, metric_in,
                                     selected = preset$metric)
          }
          if (shared_meta_srv$has_shift_type && !is.null(preset$shift) &&
              !identical(input[[shift_in]], preset$shift)) {
            shiny::updateSelectInput(session, shift_in,
                                     selected = preset$shift)
          }
        }, ignoreInit = TRUE)
        shiny::observeEvent({
          list(input[[metric_in]],
               if (shared_meta_srv$has_shift_type) input[[shift_in]] else NULL)
        }, {
          # If user is on Custom Impact, leave them there even if their
          # current metric/shift happen to match a preset. They went into
          # Custom intentionally; auto-flipping back yanks the Analysis
          # control out from under them.
          if (identical(input[[assess_id]], "Custom Impact")) return()
          cur_m <- input[[metric_in]]
          cur_s <- if (shared_meta_srv$has_shift_type) input[[shift_in]] else NULL
          matched <- NULL
          for (nm in names(shared_meta_srv$preset_map)) {
            p <- shared_meta_srv$preset_map[[nm]]
            metric_ok <- is.null(p$metric) || identical(cur_m, p$metric)
            shift_ok  <- is.null(p$shift)  || !shared_meta_srv$has_shift_type ||
                         identical(cur_s, p$shift)
            if (metric_ok && shift_ok) { matched <- nm; break }
          }
          new_assess <- matched %||% "Custom Impact"
          if (!identical(input[[assess_id]], new_assess)) {
            shiny::updateSelectInput(session, assess_id,
                                     selected = new_assess)
          }
        }, ignoreInit = TRUE)
      }

      # ---- Prioritization: native sidebar inputs + reactable -----------
      if (!is.null(dash$prio_meta)) {
        pm <- dash$prio_meta
        pm_prefix <- paste0(rid, "_pm")
        pm_dt_id  <- paste0(rid, "_pm_dt")

        # Outcome Estimate visibility — toggled by an actionButton.
        # Default FALSE = hidden (matches bn_write + bn_report).
        show_estimate_rv <- shiny::reactiveVal(FALSE)
        show_estimate_id <- paste0(pm_prefix, "_show_estimate")
        shiny::observeEvent(input[[show_estimate_id]], {
          new_val <- !show_estimate_rv()
          show_estimate_rv(new_val)
          shiny::updateActionButton(
            session, show_estimate_id,
            label = if (new_val) "Hide" else "Show"
          )
        }, ignoreInit = TRUE)

        prio_display_reactive <- shiny::reactive({
          .network_drivers_prio_data(
            pm,
            strategy = input[[paste0(pm_prefix, "_strategy")]],
            search   = input[[paste0(pm_prefix, "_search")]],
            subgroup = input[[paste0(pm_prefix, "_subgroup")]],
            focus    = input[[paste0(pm_prefix, "_focus")]],
            weight   = input[[paste0(pm_prefix, "_weight")]],
            outcome  = input[[paste0(pm_prefix, "_outcome")]]
          )
        })

        output[[pm_dt_id]] <- reactable::renderReactable({
          disp <- prio_display_reactive()
          .network_drivers_prio_reactable(
            disp, pm,
            sig_threshold = sig_threshold,
            marginal_threshold = marginal_threshold,
            show_estimate = show_estimate_rv()
          )
        })

        # Footer notes: Base + p-value legend + glossary (mirrors bn_report).
        # Glossary items show only when their column is visible in the
        # table OR (for strategy items) when the user's Analysis selection
        # matches that strategy.
        output[[paste0(rid, "_pm_footer")]] <- shiny::renderUI({
          .network_drivers_prio_footer_notes(
            prio_display_reactive(),
            metadata        = pm,
            show_estimate   = show_estimate_rv(),
            current_strategy = input[[paste0(pm_prefix, "_strategy")]],
            sig_threshold    = sig_threshold,
            marginal_threshold = marginal_threshold
          )
        })

        # Prioritization chart: stacked-bar waterfall (Previous + Incremental)
        # plus a cumulative line. Reuses the SAME display data so it stays
        # in sync with the Outcome (% Change / Point Change) toggle.
        # Modebar matches turf's: download PNG named for the chart, no
        # plotly logo, custom button set via plotly_modebar().
        output[[paste0(rid, "_pm_chart")]] <- plotly::renderPlotly({
          theme_v <- input[[paste0(pm_prefix, "_chart_theme")]] %||% "Default"
          # Path A: server-side dark-mode pairing. Subscribe to dark_mode()
          # so the chart re-renders the moment the user toggles light/dark.
          # No client-side relayout, no flicker — the chart paints directly
          # with the right surface tokens.
          if (isTRUE(dark_mode())) {
            theme_v <- .prio_dark_pair(theme_v)
          }
          .network_drivers_prio_chart(
            prio_display_reactive(), pm,
            theme = theme_v
          ) %>%
            plotly::config(
              displaylogo = FALSE,
              modeBarButtons = plotly_modebar("Prioritization Chart")
            )
        })

        # Dynamic card title: "Prioritization: <em>{analysis}</em>".
        # Reads the strategy dim (which our UI labels "Analysis").
        # Falls back to bare "Prioritization" when strategy is unset
        # or there's only one option (no meaningful selection to show).
        output[[paste0(rid, "_pm_title")]] <- shiny::renderUI({
          strat <- input[[paste0(pm_prefix, "_strategy")]] %||% ""
          # Same explicit brand text colour as the impact titles so the
          # card-header stays legible in dark mode.
          wrap <- function(...) htmltools::tags$span(
            style = "color: var(--ndr-text); font-weight: 600;", ...)
          if (!nzchar(strat)) {
            return(wrap("Prioritization"))
          }
          pretty <- gsub("_", " ", strat, fixed = TRUE)
          wrap(
            htmltools::HTML("Prioritization: "),
            htmltools::tags$em(pretty)
          )
        })
      }

      # ---- Workbook download ------------------------------------------
      # bn_write writes the BN result to an .xlsx in the working
      # directory and returns the saved path (S3-classed list whose
      # as.character() yields the path). For the download handler we
      # run bn_write inside a tempdir so the file lands somewhere we
      # can then copy to Shiny's download target.
      output[[paste0(rid, "_download_workbook")]] <- shiny::downloadHandler(
        filename = function() {
          paste0(result_name, " - Network Drivers.xlsx")
        },
        content = function(file) {
          tmp <- tempfile(pattern = paste0(rid, "_wb_"))
          dir.create(tmp)
          old_wd <- setwd(tmp)
          on.exit({ setwd(old_wd); unlink(tmp, recursive = TRUE) }, add = TRUE)
          out <- bn_write(result, file_name = result_name)
          saved <- as.character(out)[1]
          if (!file.exists(saved)) {
            saved <- file.path(tmp, basename(saved))
          }
          file.copy(saved, file, overwrite = TRUE)
        }
      )

      # Layout change → JS toggles which network slot is visible (within
      # both Attribute and Community tab panels — only the active tab
      # shows them). View changes are handled natively by bslib's tabs.
      shiny::observeEvent(input[[layout_id]], {
        layout_val <- input[[layout_id]]
        if (is.null(layout_val)) return()
        session$sendCustomMessage("netdrv_toggle_layout", list(
          rid    = rid,
          layout = layout_val
        ))
      }, ignoreInit = TRUE)

      # View change → re-measure DT columns on the now-visible table.
      # DT can't measure columns while a table is hidden (display:none
      # on inactive tab); without this the header/footer columns stay
      # misaligned from the body until something (any input change)
      # triggers a redraw.
      shiny::observeEvent(input[[view_id]], {
        view_val <- input[[view_id]]
        if (is.null(view_val)) return()
        dt_id <- switch(view_val,
          impacts_attr   = paste0(rid, "_ia_dt"),
          impacts_comm   = paste0(rid, "_ic_dt"),
          prioritization = paste0(rid, "_pm_dt"),
          membership     = paste0(rid, "_membership_table"),
          NULL
        )
        if (!is.null(dt_id)) {
          session$sendCustomMessage("netdrv_dt_adjust",
                                    list(id = ns(dt_id)))
        }
      }, ignoreInit = TRUE)
    })
  }

  # ---- State handlers — registered by app_deliverable when save_restore=TRUE
  list(
    get_state = function() {
      list(positions = positions())
    },
    set_state = function(state) {
      if (is.list(state) && is.list(state$positions)) {
        positions(state$positions)
        reload_trigger(reload_trigger() + 1L)
      }
    },
    get_fingerprint = function() {
      # Result names + node-id sets per result. If a saved state was produced
      # against a different model (different result names or different node
      # rosters), the fingerprint will differ and app_deliverable will skip
      # restoration — preventing position keys from being applied to the
      # wrong network.
      fp <- list(result_names = sort(names(results)))
      for (rn in names(results)) {
        nodes_df <- tryCatch(
          work::find_recursive(results[[rn]],
                               x_name = "attribute_viz_prep")$nodes,
          error = function(e) NULL
        )
        fp[[rn]] <- if (!is.null(nodes_df) && "id" %in% names(nodes_df)) {
          sort(as.character(nodes_df$id))
        } else NA_character_
      }
      digest::digest(fp)
    },
    reset = function() {
      positions(list())
      reload_trigger(reload_trigger() + 1L)
    }
  )
}

# ---- Internal helpers ------------------------------------------------------

#' @noRd
.network_drivers_parent_listener_js <- function(module_id) {
  # Parent-side wiring — runs once per page load. Two responsibilities:
  #
  # 1. window.addEventListener('message', ...): receives postMessage events
  #    from iframes' bn_visNetwork_deliverable_interactivity layer:
  #    - `snapshotPush` (on dragEnd / stabilized / node update) → forward
  #      to input$positions_in so the server can persist the layout
  #    - `iframeReady` (when the interactivity layer finishes init) →
  #      forward to input$iframe_ready so the server can push any
  #      already-stored positions for that key
  #
  # 2. Shiny.addCustomMessageHandler registrations:
  #    - `netdrv_toggle`: flip slot visibility on layout/view change
  #    - `netdrv_apply_to_key`: push a saved snapshot to a specific iframe
  #      via applyReportLoad postMessage
  #    - `netdrv_apply_all`: same, broadcast to every iframe with stored
  #      positions
  flag <- paste0("__netdrv_listener_", gsub("[^A-Za-z0-9]", "_", module_id))
  paste0(
    "(function() {",
    "  if (window['", flag, "']) return;",
    "  window['", flag, "'] = true;",
    # Window-scoped edit cache. Populated by every legendUpdate /
    # nodeUpdate / nodeLabelUpdate; replayed onto (a) iframes via
    # syncEdits when iframeReady fires, and (b) membership DT cells via
    # MutationObserver whenever the DOM subtree changes (DT renders
    # asynchronously, so the cells may not exist when the first edit
    # arrives).
    "  window.__netdrv_legendEdits = window.__netdrv_legendEdits || {};",
    "  window.__netdrv_nodeLabelEdits = window.__netdrv_nodeLabelEdits || {};",
    "  function fanOut(source, view, msg) {",
    "    var sel = 'iframe[data-view=\"' + view + '\"]';",
    "    document.querySelectorAll(sel).forEach(function(f) {",
    "      if (f.contentWindow !== source && f.contentWindow) {",
    "        try { f.contentWindow.postMessage(msg, '*'); } catch(err) {}",
    "      }",
    "    });",
    "  }",
    # No-op writes (setting textContent to its current value) still fire
    # the MutationObserver below, so the equality guards prevent an
    # observer-triggered apply -> apply loop that would freeze the page.
    "  function applyMembershipLegend(edits) {",
    "    document.querySelectorAll('.network-drivers-membership .community-label').forEach(function(el) {",
    "      var c = el.getAttribute('data-color');",
    "      if (c && edits[c] !== undefined && el.textContent !== edits[c]) {",
    "        el.textContent = edits[c];",
    "      }",
    "    });",
    "  }",
    "  function applyMembershipNodeLabel(nodeId, label) {",
    "    var sel = '.network-drivers-membership [data-node-id=\"' + nodeId + '\"]';",
    "    document.querySelectorAll(sel).forEach(function(el) {",
    "      if (el.textContent !== label) el.textContent = label;",
    "    });",
    "  }",
    "  function applyAllStoredToMembership() {",
    "    applyMembershipLegend(window.__netdrv_legendEdits);",
    "    Object.keys(window.__netdrv_nodeLabelEdits).forEach(function(nid) {",
    "      applyMembershipNodeLabel(nid, window.__netdrv_nodeLabelEdits[nid]);",
    "    });",
    "  }",
    # Watch each membership div for subtree mutations (DT initial render
    # and any future updates) and re-apply stored edits.
    "  function watchMembershipDivs() {",
    "    document.querySelectorAll('.network-drivers-membership').forEach(function(div) {",
    "      if (div.__netdrv_observed) return;",
    "      div.__netdrv_observed = true;",
    "      var obs = new MutationObserver(function() { applyAllStoredToMembership(); });",
    "      obs.observe(div, { childList: true, subtree: true });",
    "    });",
    "  }",
    "  if (document.readyState === 'loading') {",
    "    document.addEventListener('DOMContentLoaded', watchMembershipDivs);",
    "  } else { watchMembershipDivs(); }",
    "  setInterval(watchMembershipDivs, 1000);",
    # Toggle between table view and card view. Same JS as bn_report's
    # toggleMembershipView so the onclick attribute on the toolbar button
    # works identically in both contexts. Exposed on window so the inline
    # onclick can reach it across the module's IIFE.
    "  window.toggleMembershipView = function(btn) {",
    # Button may live in the panel toolbar (legacy) OR in the sidebar
    # (current). If sidebar, walk up to the containing layout_sidebar and
    # locate the .membership-wrap inside its content area.
    "    var wrap = btn.closest('.membership-wrap');",
    "    if (!wrap) {",
    "      var root = btn.closest('.bslib-sidebar-layout') || document;",
    "      wrap = root.querySelector('.membership-wrap');",
    "    }",
    "    if (!wrap) return;",
    "    var tbl = wrap.querySelector('.membership-table-view');",
    "    var crd = wrap.querySelector('.membership-card-view');",
    "    if (!tbl || !crd) return;",
    "    if (tbl.style.display === 'none') {",
    "      tbl.style.display = ''; crd.style.display = 'none';",
    "    } else {",
    "      tbl.style.display = 'none'; crd.style.display = '';",
    "    }",
    "  };",
    # bn_report's impact / prioritization helpers append an inline
    # <script>initImpactDashboard(id)</script> at the end of each
    # dashboard's HTML. When Shiny inserts that HTML via innerHTML, the
    # script does NOT execute (HTML5 spec). So we scan for un-inited
    # dashboards and invoke the init functions ourselves.
    "  function initEmbeddedDashboards() {",
    "    document.querySelectorAll('.impact-dashboard:not([data-netdrv-inited])').forEach(function(d) {",
    "      var id = d.getAttribute('data-dashboard-id');",
    "      if (!id || id === '_payload_only') return;",
    "      d.setAttribute('data-netdrv-inited', '1');",
    "      if (typeof initImpactDashboard === 'function') {",
    "        try { initImpactDashboard(id); } catch(err) { console.error('initImpactDashboard failed:', err); }",
    "      }",
    "    });",
    "    document.querySelectorAll('.priort-dashboard:not([data-netdrv-inited])').forEach(function(d) {",
    "      var id = d.getAttribute('data-dashboard-id');",
    "      if (!id || id === '_payload_only') return;",
    "      d.setAttribute('data-netdrv-inited', '1');",
    "      if (typeof initPriortDashboard === 'function') {",
    "        try { initPriortDashboard(id); } catch(err) { console.error('initPriortDashboard failed:', err); }",
    "      }",
    "    });",
    "  }",
    "  if (document.readyState === 'loading') {",
    "    document.addEventListener('DOMContentLoaded', initEmbeddedDashboards);",
    "  } else { initEmbeddedDashboards(); }",
    "  setInterval(initEmbeddedDashboards, 500);",
    "  window.addEventListener('message', function(e) {",
    "    var d = e.data;",
    "    if (!d) return;",
    # Shiny-dependent handlers (gated)
    "    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {",
    "      if (d.type === 'snapshotPush' && d.nsKey) {",
    "        Shiny.setInputValue('", module_id, "-positions_in', d, ",
    "          { priority: 'event' });",
    "      }",
    "      if (d.type === 'iframeReady' && d.nsKey) {",
    "        Shiny.setInputValue('", module_id, "-iframe_ready', d.nsKey, ",
    "          { priority: 'event' });",
    "      }",
    "    }",
    # On iframeReady, replay any stored edits so a late-loading iframe
    # picks them up. Doesn't depend on Shiny.
    "    if (d.type === 'iframeReady' && d.nsKey && e.source) {",
    "      var hasEdits = Object.keys(window.__netdrv_legendEdits).length > 0 ||",
    "                     Object.keys(window.__netdrv_nodeLabelEdits).length > 0;",
    "      if (hasEdits) {",
    "        try { e.source.postMessage({",
    "          type: 'syncEdits',",
    "          legend: window.__netdrv_legendEdits,",
    "          nodeLabels: window.__netdrv_nodeLabelEdits",
    "        }, '*'); } catch(err) {}",
    "      }",
    "    }",
    # Community renamed via legend's "Edit Community Names" modal in an
    # attribute iframe. Source sends `keyData` array; transform to
    # { color: label } edits, store, and forward.
    "    if (d.type === 'legendUpdate' && Array.isArray(d.keyData)) {",
    "      var edits = {};",
    "      d.keyData.forEach(function(item) {",
    "        if (item.color) edits[item.color] = item.label;",
    "      });",
    "      Object.keys(edits).forEach(function(c) {",
    "        window.__netdrv_legendEdits[c] = edits[c];",
    "      });",
    "      fanOut(e.source, 'attribute', { type: 'nodeUpdate', edits: edits });",
    "      fanOut(e.source, 'community', { type: 'legendUpdate', edits: edits });",
    "      applyMembershipLegend(edits);",
    "    }",
    # Community node renamed via right-click on a community-view iframe.
    "    if (d.type === 'nodeUpdate' && d.edits) {",
    "      Object.keys(d.edits).forEach(function(c) {",
    "        window.__netdrv_legendEdits[c] = d.edits[c];",
    "      });",
    "      fanOut(e.source, 'attribute', { type: 'nodeUpdate', edits: d.edits });",
    "      fanOut(e.source, 'community', { type: 'legendUpdate', edits: d.edits });",
    "      applyMembershipLegend(d.edits);",
    "    }",
    # Single attribute node renamed.
    "    if (d.type === 'nodeLabelUpdate' && d.nodeId && d.label) {",
    "      window.__netdrv_nodeLabelEdits[d.nodeId] = d.label;",
    "      fanOut(e.source, 'attribute', { type: 'nodeLabelUpdate', nodeId: d.nodeId, label: d.label });",
    "      applyMembershipNodeLabel(d.nodeId, d.label);",
    "    }",
    "  });",
    "  if (Shiny && Shiny.addCustomMessageHandler) {",
    # netdrv_toggle_layout: scope to network slots only (so it doesn't
    # affect membership / dashboard slots in other tabs). Hide all
    # network slots for this rid, then show the ones matching the new
    # layout (one each in the Attribute and Community tab panels).
    # bslib's tab system handles which view tab is visible.
    "    Shiny.addCustomMessageHandler('netdrv_toggle_layout', function(msg) {",
    "      var sel = '.netdrv-slot.netdrv-network-slot[data-rid=\"' + msg.rid + '\"]';",
    "      document.querySelectorAll(sel).forEach(function(el) {",
    "        el.style.display = 'none';",
    "      });",
    "      document.querySelectorAll(sel + '[data-layout=\"' + msg.layout + '\"]')",
    "        .forEach(function(el) { el.style.display = 'block'; });",
    "    });",
    # netdrv_apply_to_key: postMessage applyReportLoad to ONE iframe.
    "    Shiny.addCustomMessageHandler('netdrv_apply_to_key', function(msg) {",
    "      var iframe = document.querySelector(",
    "        'iframe[data-key=\"' + msg.key + '\"]');",
    "      if (iframe && iframe.contentWindow) {",
    "        iframe.contentWindow.postMessage(",
    "          { type: 'applyReportLoad', snapshot: msg.snapshot }, '*');",
    "      }",
    "    });",
    # netdrv_apply_all: postMessage applyReportLoad to every iframe with
    # a snapshot in `msg.snapshots`.
    "    Shiny.addCustomMessageHandler('netdrv_apply_all', function(msg) {",
    "      Object.keys(msg.snapshots).forEach(function(key) {",
    "        var iframe = document.querySelector(",
    "          'iframe[data-key=\"' + key + '\"]');",
    "        if (iframe && iframe.contentWindow) {",
    "          iframe.contentWindow.postMessage(",
    "            { type: 'applyReportLoad', snapshot: msg.snapshots[key] },",
    "            '*');",
    "        }",
    "      });",
    "    });",
    # netdrv_set_mode: broadcast a dark/light mode flip to EVERY network
    # iframe. Each iframe's listener toggles data-bs-theme on its own
    # <html>, which cascades to all brand-token-driven surfaces (legend,
    # toolbar buttons, Select-by-ID, body bg) via the --ndr-* dark
    # overrides. Stage 1 of dark-mode for network maps; visNetwork canvas
    # contents (label/edge colors) stay as authored.
    "    Shiny.addCustomMessageHandler('netdrv_set_mode', function(mode) {",
    "      document.querySelectorAll('iframe[data-key]').forEach(function(f) {",
    "        if (f.contentWindow) {",
    "          try {",
    "            f.contentWindow.postMessage(",
    "              { type: 'setMode', mode: mode }, '*');",
    "          } catch (err) {}",
    "        }",
    "      });",
    "    });",
    # netdrv_dt_adjust: force a column-width re-measure on the named DT
    # output. Needed because DT can't size columns while a table is
    # hidden (inactive tab) — when the tab becomes visible the header
    # and footer can be misaligned until something triggers adjust().
    "    Shiny.addCustomMessageHandler('netdrv_dt_adjust', function(msg) {",
    "      setTimeout(function() {",
    "        var $tbl = $('#' + msg.id + ' table.dataTable');",
    "        if ($tbl.length && $.fn.dataTable && $.fn.dataTable.isDataTable($tbl)) {",
    "          var dt = $tbl.DataTable();",
    "          try { dt.columns.adjust(); } catch(e) {}",
    "        }",
    "      }, 50);",
    "    });",
    # netdrv_dt_footer: update the Total Impact + Base cells in the
    # tfoot of the named DT output. In scroll mode DT clones the tfoot
    # into .dataTables_scrollFoot, so this targets BOTH the body table's
    # tfoot and the cloned visible one. Idempotent — only writes if the
    # text actually changed (avoids needless DOM mutation).
    "    Shiny.addCustomMessageHandler('netdrv_dt_footer', function(msg) {",
    "      var $root = $('#' + msg.id);",
    "      function setText($el, val) {",
    "        var s = (val === null || val === undefined) ? '' : String(val);",
    "        $el.each(function(){ if (this.textContent !== s) this.textContent = s; });",
    "      }",
    "      var ti = msg.total_impact || {};",
    "      Object.keys(ti).forEach(function(sg) {",
    "        setText($root.find('tfoot th.ti-cell[data-sg=\"' + sg + '\"]'), ti[sg]);",
    "      });",
    "      var bs = msg.base || {};",
    "      Object.keys(bs).forEach(function(sg) {",
    "        setText($root.find('tfoot th.base-cell[data-sg=\"' + sg + '\"]'), bs[sg]);",
    "      });",
    "    });",
    "  }",
    "})();"
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' @noRd
.network_drivers_normalize_results <- function(results) {
  # Single bn_finalize_network output (has $bn / $impacts / $prioritizations).
  is_single <- is.list(results) &&
    any(c("bn", "impacts", "prioritizations") %in% names(results))
  if (is_single) {
    return(list(Network = results))
  }
  if (!is.list(results) || length(results) == 0) {
    cli::cli_abort("{.arg results} must be a non-empty list.")
  }
  if (is.null(names(results)) || any(!nzchar(names(results)))) {
    names(results) <- paste0("Result_", seq_along(results))
  }
  results
}

#' @noRd
.network_drivers_safe_id <- function(x) {
  s <- gsub("[^A-Za-z0-9]+", "_", x)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) s <- "x"
  if (grepl("^[0-9]", s)) s <- paste0("r_", s)
  tolower(s)
}

#' @noRd
.network_drivers_dashboard_html <- function(spec) {
  # Accepts either a bare HTML string OR a list with $id and $html.
  # When `id` is provided the wrapping div carries it so that
  # initImpactDashboard / initPriortDashboard's `getElementById(id)`
  # finds the dashboard root (matches bn_report's structure).
  if (is.null(spec) || identical(spec, "")) {
    return(shiny::div(class = "extra-empty",
                      "No data available for this view."))
  }
  id   <- if (is.list(spec)) spec$id else NULL
  html <- if (is.list(spec)) spec$html else spec
  if (is.null(html) || identical(html, "")) {
    return(shiny::div(class = "extra-empty",
                      "No data available for this view."))
  }
  shiny::div(
    id = id,
    class = "network-drivers-dashboard",
    htmltools::HTML(html)
  )
}

#' @noRd
.network_drivers_membership_df <- function(result) {
  nodes_df <- tryCatch(
    work::find_recursive(result, x_name = "attribute_viz_prep")$nodes,
    error = function(e) NULL
  )
  if (is.null(nodes_df)) return(NULL)
  if (!all(c("community_name", "color", "id", "label") %in% names(nodes_df))) {
    return(NULL)
  }
  groups <- nodes_df %>%
    dplyr::arrange(.data$group) %>%
    dplyr::group_by(.data$community_name, .data$color) %>%
    dplyr::summarise(
      Attributes = paste(
        sprintf('<span class="node-pill" data-node-id="%s">%s</span>',
                htmltools::htmlEscape(.data$id),
                htmltools::htmlEscape(.data$label)),
        collapse = " "
      ),
      Count = dplyr::n(),
      .groups = "drop"
    )
  tibble::tibble(
    # `.community-label[data-color]` selector matches bn_report's DOM
    # update path so the parent JS can swap labels on legendUpdate.
    Community = sprintf(
      paste0('<span class="membership-dot" style="background:%s;"></span>',
             '<span class="community-label" data-color="%s">%s</span>'),
      groups$color, groups$color, htmltools::htmlEscape(groups$community_name)
    ),
    Attributes = groups$Attributes,
    Count = groups$Count
  )
}

#' @noRd
#' Build the card-grid HTML for the membership panel.
#' Mirrors bn_report's `render_membership` card view: one card per community,
#' colored left border, header with dot + community-label + count, then the
#' attribute pills. Class names match the report so:
#'   - .community-label[data-color] catches the JS legend-edit sync
#'   - [data-node-id] catches the JS node-label-edit sync
.network_drivers_membership_cards_html <- function(result) {
  nodes_df <- tryCatch(
    work::find_recursive(result, x_name = "attribute_viz_prep")$nodes,
    error = function(e) NULL
  )
  if (is.null(nodes_df)) return(NULL)
  if (!all(c("community_name", "color", "id", "label") %in% names(nodes_df))) {
    return(NULL)
  }
  groups <- nodes_df %>%
    dplyr::arrange(.data$group) %>%
    dplyr::group_by(.data$community_name, .data$color) %>%
    dplyr::summarise(
      ids    = list(as.character(.data$id)),
      labels = list(as.character(.data$label)),
      .groups = "drop"
    )
  cards <- purrr::pmap_chr(groups, function(community_name, color, ids, labels) {
    pills <- paste(
      sprintf('<span class="node-pill" data-node-id="%s">%s</span>',
              htmltools::htmlEscape(ids),
              htmltools::htmlEscape(labels)),
      collapse = ""
    )
    sprintf(
      paste0(
        '<div class="membership-card" style="border-left: 4px solid %s;">',
        '<div class="mc-header">',
        '<span class="membership-dot" style="background: %s;"></span>',
        '<span class="community-label" data-color="%s">%s</span>',
        '<span class="card-count">%d</span>',
        '</div>',
        '<div class="card-nodes">%s</div>',
        '</div>'
      ),
      color, color, color,
      htmltools::htmlEscape(community_name),
      length(ids),
      pills
    )
  })
  paste0('<div class="membership-cards">', paste(cards, collapse = ""), '</div>')
}

#' @noRd
.network_drivers_module_css <- function(id) {
  # Scoped wrapper styles. The embedded bn_report dashboards bring their own
  # CSS via .bn_report_css(); here we add a thin layer to:
  #   - **neutralise the bn_report `body` margin leak** — .bn_report_css()
  #     restyle layer sets `body { margin: 18px 28px !important; ... }` for
  #     the standalone HTML report. When that CSS injects into the Shiny app
  #     (this module appends it to all_css), the 18px top margin pushed the
  #     page ~3px past viewport on standard displays — a hairline window
  #     scroll. The bslib page handles its own chrome; body needs no margin
  #     in the app context. Override here (loaded AFTER bn_report css so
  #     !important wins), without affecting bn_report standalone.
  #   - constrain dashboard padding when embedded inside a bslib::card body
  #   - tighten the membership DT to feel like the rest of the app
  paste0(
    "body { margin: 0 !important; }\n",
    # Sidebar bottom-pinned block (margin-top:auto wrapper around hr +
    # Download Workbook) needs two things:
    #   1. .sidebar-content as a flex column with a definite height
    #      → so `margin-top: auto` has space to consume.
    #   2. .sidebar itself with a definite height that places its bottom
    #      at the same y as the impact card's max_height bottom.
    #
    # Card top is offset DOWN from sidebar top by the navset_underline
    # tab bar (~45px) + a small nav_panel/layout_columns padding (~5px).
    # So sidebar must be 80vh PLUS that ~50px offset for the bottoms to
    # line up. If the offset is off, dial the +50px piece.
    ".bslib-sidebar-layout > .sidebar {\n",
    "  height: calc(80vh + 50px) !important;\n",
    "}\n",
    ".bslib-sidebar-layout > .sidebar > .sidebar-content {\n",
    "  display: flex !important;\n",
    "  flex-direction: column !important;\n",
    "  height: 100% !important;\n",
    "  overflow-y: auto !important;\n",
    "  gap: 0 !important;\n",
    "}\n",
    "#", id, " .network-drivers-dashboard { padding: 16px; }\n",
    "#", id, " .network-drivers-membership { padding: 8px 12px; }\n",
    # Sidebar control spacing: every selectInput is wrapped in a
    # .netdrv-ctrl-wrap div. Zero out the inner .form-group's bottom
    # margin so the wrapper alone controls vertical rhythm. UNSCOPED
    # via #<id> won't work because layout_sidebar can render the sidebar
    # outside the module's id'd container; .netdrv-ctrl-wrap is unique
    # enough to be safe page-wide.
    ".netdrv-ctrl-wrap { margin-bottom: 12px; }\n",
    ".netdrv-ctrl-wrap .form-group { margin-bottom: 0; }\n",
    # ---- Membership: app-specific layout overrides ----
    # The canonical membership styling (`.membership-card`, `.mc-header`,
    # `.card-count`, `.card-nodes`, `.node-pill`, `.membership-dot`) is
    # tokenized in bn_helpers.R and reaches the app via .bn_report_css().
    # Only app-specific layout rules live here:
    #
    #   - .membership-wrap / .membership-toolbar / .membership-view —
    #     flex shell that doesn't exist in the report
    #   - .membership-cards — APP wants the grid to scroll within the
    #     panel (overflow-y:auto), REPORT wants the page to scroll. The
    #     `.network-drivers-membership` ancestor is the app-only marker
    #     so this rule beats bn_helpers' bare `.membership-cards` by
    #     selector specificity (2 classes vs 1).
    #
    # We DON'T use `#<id>` scoping here because app_deliverable doesn't
    # wrap the module in a container with that id, so `#<id> .foo` would
    # match nothing. `.network-drivers-membership` is the actual scope.
    ".network-drivers-membership .membership-wrap {\n",
    "  display: flex; flex-direction: column; height: 100%;\n",
    "}\n",
    ".network-drivers-membership .membership-toolbar {\n",
    "  display: flex; justify-content: flex-end; margin-bottom: 12px;\n",
    "}\n",
    ".network-drivers-membership .membership-view {\n",
    "  width: 100%;\n",
    "}\n",
    ".network-drivers-membership .membership-cards {\n",
    "  overflow-y: auto;\n",
    "  align-content: start;\n",
    "  max-height: 100%;\n",
    "}\n",
    # DT-in-card fill: makes the DT wrapper flex-grow inside the
    # navset_card_underline's card_body so the table body fills available
    # height without a card-level scrollbar. Overrides the inline
    # max-height DT sets from `scrollY`. UNSCOPED because `app_deliverable`
    # doesn't wrap the module in an element with id=<module_id>, so any
    # `#<id> .foo` selector would match nothing. The .dataTables_* class
    # names are DT-internal and specific enough that page-wide scoping
    # is safe (no other component uses those classes).
    # `display: contents` on `.dataTables_scroll` makes its children
    # (scrollHead, scrollBody, scrollFoot) behave as direct children of
    # the wrapper, so they all participate in the wrapper's flex column.
    # That lets head/body/foot stack at the top with body content-sized
    # (or capped at wrapper height for long tables). Few rows: foot
    # sits right under data. Many rows: body scrolls internally; foot
    # stays right below it.
    #
    # `.dataTables_wrapper` also gets a border + radius + bg so it looks
    # like it's inside a bslib::card without the actual card element
    # (saves a layer in the DOM). Uses theme CSS variables so it tracks
    # dark mode automatically.
    # Fill propagation for navset_underline → tab-pane → card →
    # reactable. Without this, the inner panels don't have a defined
    # height, so reactable's `height = "100%"` resolves against an
    # undefined parent and the table falls back to content size (which
    # then doesn't respond to viewport resize).
    ".nav-underline + .tab-content {",
    "  flex: 1 1 auto;",
    "  min-height: 0;",
    "  display: flex;",
    "  flex-direction: column;",
    # Containing block for the absolutely-positioned inactive panes
    # below (prerender trick).
    "  position: relative;",
    "}\n",
    ".nav-underline + .tab-content > .tab-pane.active {",
    "  flex: 1 1 auto;",
    "  min-height: 0;",
    "  display: flex;",
    "  flex-direction: column;",
    "}\n",
    # ---- Prerender inactive tab-panes ----
    # bslib defaults inactive .tab-pane to display:none, which gives the
    # iframe inside zero dimensions. vis-network can't render to a 0×0
    # canvas; first activation triggers a fresh render → brief flash.
    # Override: keep inactive panes in DOM at full size, hidden via
    # visibility + position:absolute (out of layout flow). vis-network
    # renders fully on init; activation is a visibility flip.
    # Scoped to `.nav-underline + .tab-content` so only the network-
    # drivers inner tabs are affected, NOT the outer page_navbar tabs.
    ".nav-underline + .tab-content > .tab-pane:not(.active) {",
    "  display: flex !important;",
    "  flex-direction: column;",
    "  visibility: hidden;",
    "  position: absolute;",
    "  top: 0; left: 0; right: 0; bottom: 0;",
    "  pointer-events: none;",
    "}\n",
    ".dataTables_wrapper {",
    "  height: calc(100% - 1rem);",
    "  max-height: 85vh;",
    "  margin-bottom: 1rem;",
    "  display: flex;",
    "  flex-direction: column;",
    "  overflow: hidden;",
    "  border: 1px solid var(--bs-card-border-color, #dee2e6);",
    "  border-radius: var(--bs-card-border-radius, 0.375rem);",
    "  background: var(--bs-card-bg, #fff);",
    "  box-shadow: var(--bs-card-box-shadow, 0 1px 2px rgba(0,0,0,.05));",
    "}\n",
    ".dataTables_scroll { display: contents; }\n",
    ".dataTables_scrollHead { flex: 0 0 auto; }\n",
    ".dataTables_scrollBody {",
    "  flex: 1 1 auto !important;",
    "  min-height: 0 !important;",
    "  max-height: none !important;",
    "  height: auto !important;",
    "  overflow-y: auto !important;",
    "}\n",
    ".dataTables_scrollFoot { flex: 0 0 auto; }\n"
  )
}

# Local %||% — avoids depending on rlang at module load.
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
