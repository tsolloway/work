# Internal: name every factor of every solution in a pca_analysis() object,
# in one Claude call, for pca_write(). Returns a list keyed by solution
# ("4", "5", ...) of character vectors keyed by factor number, or NULL when
# naming could not be done - the workbook is written either way.


#' @keywords internal
.pca_name_factors <- function(pca_tables, min_words = 1, max_words = 3,
                              model = "claude-haiku-4-5", api_key = NULL){

  if(is.null(api_key)) api_key <- get_environment_key("ANTHROPIC_API_KEY")

  # evidence per factor: the variables assigned to it, strongest first, with
  # the signed loading so the name leans on what drives the factor
  blocks <- purrr::imap_chr(pca_tables, function(tbl, sol){
    lab <- if("label" %in% names(tbl)) tbl[["label"]] else tbl[["variable"]]
    by_factor <- split(seq_len(nrow(tbl)), tbl[["factor"]])
    factors <- vapply(names(by_factor), function(f){
      i <- by_factor[[f]]
      i <- i[order(-abs(tbl[["max"]][i]))]
      paste0("  Factor ", f, ":\n",
             paste0("    - ", lab[i], " (", sprintf("%+.2f", tbl[["max"]][i]), ")", collapse = "\n"))
    }, character(1))
    paste0("Solution ", sol, " (", length(by_factor), " factors):\n", paste(factors, collapse = "\n"))
  })

  prompt <- paste0(
    "These are principal components solutions from a survey. Each solution splits the same ",
    "items into a different number of factors. Under each factor are the items assigned to it, ",
    "strongest first, with the signed loading in brackets.\n\n",
    "Give every factor a name of ", min_words, " to ", max_words, " words that captures what its ",
    "strongest items have in common. The names label the factor groups in the review workbook, ",
    "so write them for a researcher scanning the sheet: plain, specific, sentence case.\n\n",
    "Within a solution every name must be different - if two factors share a theme, qualify ",
    "them (\"Functional trust\" / \"Emotional trust\"). Across solutions, give a factor that is ",
    "essentially the same group of items the same name, so the solutions read as a sequence.\n\n",
    paste(blocks, collapse = "\n\n")
  )

  schema <- list(
    type = "object",
    properties = list(
      solutions = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            solution = list(type = "integer"),
            factors  = list(
              type = "array",
              items = list(
                type = "object",
                properties = list(
                  factor = list(type = "integer"),
                  name   = list(type = "string")
                ),
                required = list("factor", "name"),
                additionalProperties = FALSE
              )
            )
          ),
          required = list("solution", "factors"),
          additionalProperties = FALSE
        )
      )
    ),
    required = list("solutions"),
    additionalProperties = FALSE
  )

  body <- list(
    model      = model,
    max_tokens = 16000,
    # a server-side rerun on another model if the request is declined, rather
    # than a refusal the workbook has to fall back from
    fallbacks  = "default",
    output_config = list(
      effort = "low",
      format = list(type = "json_schema", schema = schema)
    ),
    messages = list(list(role = "user", content = prompt))
  )

  # Haiku 4.5 rejects effort (400) and has no server-side fallback, so both
  # are sent only to the models that take them
  haiku <- grepl("haiku", model)
  if(haiku){
    body$output_config$effort <- NULL
    body$fallbacks <- NULL
  }

  resp <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(.headers = c(
      `x-api-key`         = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type`      = "application/json",
      if(!haiku) c(`anthropic-beta` = "server-side-fallback-2026-07-01")
    )),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(300)
  )

  # A naming failure must not cost the analysis, so it warns and the sheets
  # keep "Factor N" rather than the whole write stopping.
  if(httr::status_code(resp) != 200){
    warning("Factor naming skipped - Claude API returned ", httr::status_code(resp), ": ",
            substr(httr::content(resp, as = "text", encoding = "UTF-8"), 1, 300), call. = FALSE)
    return(NULL)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

  if(!identical(parsed[["stop_reason"]], "end_turn")){
    warning("Factor naming skipped - the response stopped with '", parsed[["stop_reason"]], "'.",
            call. = FALSE)
    return(NULL)
  }

  # thinking comes back as its own block ahead of the answer, so read the
  # text blocks rather than the first block
  text <- paste(unlist(lapply(parsed[["content"]], function(b)
    if(identical(b[["type"]], "text")) b[["text"]])), collapse = "")
  out <- jsonlite::fromJSON(text, simplifyVector = FALSE)[["solutions"]]

  names_by_sol <- lapply(out, function(s){
    nm <- vapply(s[["factors"]], function(f) trimws(f[["name"]]), character(1))
    stats::setNames(nm, vapply(s[["factors"]], function(f) as.character(f[["factor"]]), character(1)))
  })
  names(names_by_sol) <- vapply(out, function(s) as.character(s[["solution"]]), character(1))

  # The Name column is what seg_get_fa_winner() keys factors by, so a name must
  # be unique within its solution. Suffix any repeat the model let through.
  lapply(names_by_sol, function(nm){
    dup <- duplicated(nm)
    if(any(dup)) nm[dup] <- paste0(nm[dup], " (F", names(nm)[dup], ")")
    nm
  })
}
