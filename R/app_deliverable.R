#' app_deliverable
#'
#' @description Launch a modular Shiny deliverable app that hosts one or more
#'   analysis modules (e.g., TURF, MaxDiff) as tabs in a single navbar. Each
#'   module contributes its own tabs, server logic, CSS, and head tags.
#'
#' @param title Character. Navbar title displayed at the top of the app.
#'   Defaults to \code{"Project Name (#XXXXXXX)"}.
#' @param modules List of module definition lists, each created by an
#'   \code{app_deliverable_add_*()} function (e.g.,
#'   \code{app_deliverable_add_turf()}). Each module must contain:
#'   \describe{
#'     \item{id}{Character. Unique namespace identifier.}
#'     \item{tabs}{List of \code{bslib::nav_panel} objects.}
#'     \item{server}{Function with signature
#'       \code{function(input, output, session, dark_mode)}.}
#'     \item{css}{Character string of CSS or \code{NULL}.}
#'     \item{head_tags}{List of additional \code{<head>} tags or \code{NULL}.}
#'   }
#' @param nested Logical. If \code{FALSE} (default), all module tabs are
#'   flattened into the top-level navbar. If \code{TRUE}, each module gets
#'   its own top-level nav panel (title derived from the list name, with
#'   underscores replaced by spaces), and the module's tabs become nested
#'   card tabs inside that panel.
#' @param save_restore Logical. If \code{TRUE} (default), adds state
#'   persistence: auto-save on input changes, auto-restore on launch,
#'   plus Save As / Load / Reset buttons in the navbar. Modules must return
#'   state handler functions to participate.
#' @param state_extension Character. File extension (without dot) for saved
#'   state files. Defaults to \code{"proj"}.
#' @param theme Character (Bootswatch theme name) or a \code{bslib::bs_theme}
#'   object. Default \code{NULL} uses \code{"flatly"}. See
#'   \url{https://bootswatch.com/} for available themes.
#'
#' @return A \code{shiny::shinyApp} object.
#'
#' @examples
#' \dontrun{
#' mod_turf <- app_deliverable_add_turf(
#'   best_combo_results = turf_results,
#'   raw       = example_data_ice_cream,
#'   vars      = example_data_ice_cream_dictionary$variable,
#'   subgroups = c("Total", "Gen_Z", "Millennials", "Gen_X"),
#'   weight    = "weight",
#'   labels    = example_data_ice_cream_dictionary
#' )
#'
#' app_deliverable(
#'   title   = "Ice Cream TURF (#1234567)",
#'   modules = list(mod_turf)
#' )
#'
#' # Nested layout — each module gets its own top-level tab
#' app_deliverable(
#'   title   = "Multi-Study (#1234567)",
#'   modules = list(Brand_Health = mod1, Ad_Tracker = mod2),
#'   nested  = TRUE
#' )
#'
#' # With state persistence using custom extension
#' app_deliverable(
#'   title   = "My Project (#1234567)",
#'   modules = list(mod_turf),
#'   save_restore    = TRUE,
#'   state_extension = "kadra-state"
#' )
#' }
#'
#' @export
app_deliverable <- function(
    title = "Project Name (#XXXXXXX)",
    modules = list(),
    nested = FALSE,
    save_restore = TRUE,
    state_extension = "proj",
    theme = NULL
) {

  if (is.null(theme)) theme <- "flatly"
  if (is.character(theme)) {
    theme <- bslib::bs_theme(version = 5, bootswatch = theme)
  }

  # ---- Collect UI pieces from modules ----
  all_tabs <- list()
  all_css <- character(0)
  all_head_tags <- list()
  mod_names <- names(modules)

  for (i in seq_along(modules)) {
    mod <- modules[[i]]
    if (!is.null(mod$css)) all_css <- c(all_css, mod$css)
    if (!is.null(mod$head_tags)) all_head_tags <- c(all_head_tags, mod$head_tags)

    if (nested) {
      # Derive panel title from list name (replace _ with space)
      panel_title <- if (!is.null(mod_names) && nchar(mod_names[i]) > 0) {
        gsub("_", " ", mod_names[i])
      } else {
        mod$id
      }
      # Wrap module tabs inside a top-level nav_panel with nested card tabs
      nested_panel <- bslib::nav_panel(
        panel_title,
        do.call(bslib::navset_card_tab, mod$tabs)
      )
      all_tabs <- c(all_tabs, list(nested_panel))
    } else {
      all_tabs <- c(all_tabs, mod$tabs)
    }
  }

  # ---- Build header ----
  header_parts <- list()
  if (length(all_css) > 0) {
    header_parts <- c(header_parts, list(
      shiny::tags$style(shiny::HTML(paste(all_css, collapse = "\n")))
    ))
  }
  header_parts <- c(header_parts, all_head_tags)

  # State management CSS + JS
  if (save_restore) {
    header_parts <- c(header_parts, list(
      shiny::tags$style(shiny::HTML(
        ".state-dropdown .dropdown-menu { min-width: 220px; padding: 0.75rem; }
         .state-dropdown .btn-state { font-size: 0.8rem; padding: 0.25rem 0.6rem; }
         .state-dropdown .btn { font-size: 0.8rem; padding: 0.25rem 0.6rem; width: 100%; }"
      )),
      shiny::tags$script(shiny::HTML("
        $(function() {
          var inIframe = window.self !== window.top;
          console.log('[state] JS init — inIframe:', inIframe);

          window.addEventListener('message', function(ev) {
            if (!ev.data || !ev.data.type) return;
            console.log('[state] Received message:', ev.data.type, ev.data);
            if (ev.data.type === 'save-path-result' && ev.data.filePath) {
              Shiny.setInputValue('state_save_path', ev.data.filePath, {priority: 'event'});
            }
            if (ev.data.type === 'load-path-result' && ev.data.filePath) {
              Shiny.setInputValue('state_load_path', ev.data.filePath, {priority: 'event'});
            }
          });

          Shiny.addCustomMessageHandler('request_save_path', function(msg) {
            console.log('[state] request_save_path handler fired, inIframe:', inIframe, msg);
            if (inIframe) {
              window.parent.postMessage({
                type: 'request-save-path',
                defaultPath: msg.defaultPath,
                filters: msg.filters
              }, '*');
              console.log('[state] postMessage sent to parent');
            } else {
              Shiny.setInputValue('state_save_show_modal', Math.random(), {priority: 'event'});
            }
          });

          Shiny.addCustomMessageHandler('request_load_path', function(msg) {
            console.log('[state] request_load_path handler fired, inIframe:', inIframe, msg);
            if (inIframe) {
              window.parent.postMessage({
                type: 'request-load-path',
                filters: msg.filters
              }, '*');
              console.log('[state] postMessage sent to parent');
            } else {
              Shiny.setInputValue('state_load_show_modal', Math.random(), {priority: 'event'});
            }
          });
        });
      "))
    ))
  }

  header <- if (length(header_parts) > 0) {
    shiny::tags$head(header_parts)
  } else {
    NULL
  }

  # ---- State management UI ----
  state_nav_items <- list()
  if (save_restore) {
    state_nav_items <- list(
      bslib::nav_item(
        shiny::tags$div(
          class = "dropdown state-dropdown",
          shiny::tags$button(
            class = "btn btn-sm btn-outline-secondary dropdown-toggle btn-state",
            type = "button",
            `data-bs-toggle` = "dropdown",
            `data-bs-auto-close` = "outside",
            shiny::icon("floppy-disk"), " State"
          ),
          shiny::tags$div(
            class = "dropdown-menu dropdown-menu-end p-3",
            shiny::tags$h6("Save & Load", class = "dropdown-header px-0"),
            shiny::actionButton(
              "state_save_btn", "Save As...",
              class = "btn btn-sm btn-outline-primary w-100 mb-2",
              icon = shiny::icon("download")
            ),
            shiny::actionButton(
              "state_load_btn", "Load...",
              class = "btn btn-sm btn-outline-secondary w-100 mb-2",
              icon = shiny::icon("folder-open")
            ),
            shiny::tags$hr(class = "my-2"),
            shiny::actionButton(
              "state_reset", "Reset to Defaults",
              class = "btn btn-sm btn-outline-danger w-100",
              icon = shiny::icon("arrow-rotate-left")
            )
          )
        )
      )
    )
  }

  # ---- Assemble UI ----
  ui <- do.call(
    bslib::page_navbar,
    c(
      list(title = title, theme = theme, header = header),
      all_tabs,
      list(bslib::nav_spacer()),
      state_nav_items,
      list(bslib::nav_item(bslib::input_dark_mode(id = "dark_mode")))
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {
    # Shared dark mode reactive — passed to each module
    dark_mode <- shiny::reactive({
      isTRUE(input$dark_mode == "dark")
    })

    # Store module state handlers keyed by module ID
    module_state_handlers <- list()

    for (mod in modules) {
      local({
        m <- mod
        result <- shiny::moduleServer(m$id, function(input, output, session) {
          m$server(input, output, session, dark_mode = dark_mode)
        })
        # If module returns state handlers, register them
        if (is.list(result) &&
            is.function(result$get_state) &&
            is.function(result$set_state)) {
          module_state_handlers[[m$id]] <<- result
        }
      })
    }

    if (save_restore) {

      # ---- Fingerprint ----
      compute_fingerprint <- function() {
        mod_ids <- sort(names(module_state_handlers))
        fp_data <- list(title = title, module_ids = mod_ids)
        for (mid in mod_ids) {
          if (is.function(module_state_handlers[[mid]]$get_fingerprint)) {
            fp_data[[mid]] <- module_state_handlers[[mid]]$get_fingerprint()
          }
        }
        digest::digest(fp_data)
      }

      app_fingerprint <- compute_fingerprint()

      # ---- Collect / restore all module state ----
      get_all_state <- function() {
        state <- list(
          .version     = 1L,
          .timestamp   = Sys.time(),
          .app_title   = title,
          .fingerprint = app_fingerprint
        )
        for (mod_id in names(module_state_handlers)) {
          state[[mod_id]] <- module_state_handlers[[mod_id]]$get_state()
        }
        state
      }

      set_all_state <- function(state) {
        for (mod_id in names(module_state_handlers)) {
          if (!is.null(state[[mod_id]])) {
            module_state_handlers[[mod_id]]$set_state(state[[mod_id]])
          }
        }
      }

      # ---- Auto-restore on startup ----
      if (length(module_state_handlers) > 0) {
        session$onFlushed(function() {
          if (file.exists("state.rds")) {
            tryCatch({
              state <- readRDS("state.rds")
              if (is.list(state) && !is.null(state$.version)) {
                if (!is.null(state$.fingerprint) &&
                    state$.fingerprint != app_fingerprint) {
                  shiny::showNotification(
                    paste0("Saved state is from a different app configuration. Skipping restore."),
                    type = "warning", duration = 5
                  )
                } else {
                  set_all_state(state)
                  message("[state] Restored from state.rds")
                }
              }
            }, error = function(e) {
              message("[state] Auto-restore failed: ", e$message)
            })
          }
        }, once = TRUE)

        # ---- Auto-save (debounced) ----
        combined_state <- shiny::reactive({
          get_all_state()
        })

        combined_state_debounced <- shiny::debounce(combined_state, 2000)

        shiny::observe({
          state <- combined_state_debounced()
          tryCatch(
            saveRDS(state, "state.rds"),
            error = function(e) message("[state] Auto-save failed: ", e$message)
          )
        })
      }

      # ---- Save As ----
      default_save_name <- paste0("state_", format(Sys.Date(), "%Y%m%d"),
                                  ".", state_extension)
      save_filters <- list(list(
        name = "State files",
        extensions = list(state_extension)
      ))

      shiny::observeEvent(input$state_save_btn, {
        session$sendCustomMessage("request_save_path", list(
          defaultPath = file.path(
            fs::path_home("Desktop"), default_save_name
          ),
          filters = save_filters
        ))
      })

      # Electron returns a file path
      shiny::observeEvent(input$state_save_path, {
        out_file <- input$state_save_path
        state <- get_all_state()
        tryCatch({
          saveRDS(state, out_file)
          shiny::showNotification(
            paste("State saved to", basename(out_file)),
            type = "message", duration = 3
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Save failed:", e$message),
            type = "error", duration = 5
          )
        })
      })

      # Fallback modal when not in Electron
      shiny::observeEvent(input$state_save_show_modal, {
        save_dirs <- c(
          Desktop   = as.character(fs::path_home("Desktop")),
          Documents = as.character(fs::path_home("Documents")),
          Downloads = as.character(fs::path_home("Downloads"))
        )
        shiny::showModal(shiny::modalDialog(
          title = "Save State",
          shiny::textInput(
            "state_save_name", "File name",
            value = paste0("state_", format(Sys.Date(), "%Y%m%d"))
          ),
          shiny::selectInput("state_save_dir", "Location", choices = save_dirs),
          size = "s", easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("state_save_confirm", "Save",
                                class = "btn btn-primary",
                                icon = shiny::icon("download"))
          )
        ))
      })

      shiny::observeEvent(input$state_save_confirm, {
        name <- trimws(input$state_save_name)
        if (nchar(name) == 0) {
          shiny::showNotification("Please enter a file name", type = "warning")
          return()
        }
        if (!grepl(paste0("\\.", state_extension, "$"), name)) {
          name <- paste0(name, ".", state_extension)
        }
        out_file <- file.path(input$state_save_dir, name)
        state <- get_all_state()
        tryCatch({
          saveRDS(state, out_file)
          shiny::removeModal()
          shiny::showNotification(
            paste("State saved to", out_file),
            type = "message", duration = 3
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Save failed:", e$message), type = "error", duration = 5
          )
        })
      })

      # ---- Load ----
      load_filters <- list(list(
        name = "State files",
        extensions = list(state_extension)
      ))

      shiny::observeEvent(input$state_load_btn, {
        session$sendCustomMessage("request_load_path", list(
          filters = load_filters
        ))
      })

      # Electron returns a file path
      shiny::observeEvent(input$state_load_path, {
        load_file <- input$state_load_path
        tryCatch({
          state <- readRDS(load_file)
          if (!is.list(state) || is.null(state$.version)) {
            shiny::showNotification("Invalid state file", type = "error",
                                   duration = 5)
            return()
          }
          if (!is.null(state$.fingerprint) &&
              state$.fingerprint != app_fingerprint) {
            saved_title <- state$.app_title %||% "unknown"
            shiny::showNotification(
              paste0("This state was saved from '", saved_title,
                     "'. It may not be compatible with this app."),
              type = "warning", duration = 8
            )
          }
          set_all_state(state)
          saveRDS(state, "state.rds")
          shiny::showNotification(
            paste("State loaded from", basename(load_file)),
            type = "message", duration = 3
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Load failed:", e$message), type = "error", duration = 5
          )
        })
      })

      # Fallback modal when not in Electron
      shiny::observeEvent(input$state_load_show_modal, {
        shiny::showModal(shiny::modalDialog(
          title = "Load State",
          shiny::fileInput(
            "state_load_file", NULL,
            accept = paste0(".", state_extension),
            buttonLabel = "Browse...",
            placeholder = paste0("Select a .", state_extension, " file")
          ),
          size = "s", easyClose = TRUE,
          footer = shiny::modalButton("Cancel")
        ))
      })

      shiny::observeEvent(input$state_load_file, {
        shiny::req(input$state_load_file)
        load_file <- input$state_load_file$datapath
        tryCatch({
          state <- readRDS(load_file)
          if (!is.list(state) || is.null(state$.version)) {
            shiny::showNotification("Invalid state file", type = "error",
                                   duration = 5)
            return()
          }
          if (!is.null(state$.fingerprint) &&
              state$.fingerprint != app_fingerprint) {
            saved_title <- state$.app_title %||% "unknown"
            shiny::showNotification(
              paste0("This state was saved from '", saved_title,
                     "'. It may not be compatible with this app."),
              type = "warning", duration = 8
            )
          }
          set_all_state(state)
          saveRDS(state, "state.rds")
          shiny::removeModal()
          shiny::showNotification(
            paste("State loaded from", input$state_load_file$name),
            type = "message", duration = 3
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Load failed:", e$message), type = "error", duration = 5
          )
        })
      })

      # ---- Reset ----
      shiny::observeEvent(input$state_reset, {
        if (file.exists("state.rds")) file.remove("state.rds")
        session$reload()
      })
    }
  }

  shiny::shinyApp(ui, server)
}
