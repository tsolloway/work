#' Set a parallel future plan with automatic worker handling
#'
#' @description
#' Sets a `future` plan for parallel computation with sensible defaults.
#' Automatically determines the number of workers based on available cores and the number of tasks.
#' If `multicore` is unsupported on the system, it falls back to `multisession`.
#'
#' @param strategy Character; one of `"multisession"`, `"multicore"`, `"sequential"`, or `"cluster"`.
#' @param workers Optional integer; number of workers to use. Defaults to `availableCores() - 1`.
#' @param ntasks Optional integer; if fewer than `workers`, the number of workers is reduced accordingly.
#'
#' @return Invisibly returns the result of `future::plan()`.
#'
#' @examples
#' # Sequential plan
#' future_plan("sequential")
#'
#' # Multisession plan with default workers
#' future_plan("multisession")
#'
#' # Multisession plan with 4 workers
#' future_plan("multisession", workers = 4)
#'
#' @export
future_plan <- function(
    strategy = c("multisession", "multicore", "sequential", "cluster"),
    workers = NULL,
    ntasks = NULL
) {

  load_or_stop("furrr")
  load_or_stop("future")

  strategy <- match.arg(strategy)

  # Fallback if multicore is unsupported
  if(strategy == "multicore" && !future::supportsMulticore()){
    strategy <- "multisession"
  }


  # Determine if no workers are needed (sequential)
  no_workers <- strategy == "sequential"


  # Map string to future strategy
  strategy_fun <- switch(
    strategy,
    sequential = future::sequential,
    multisession = future::multisession,
    multicore = future::multicore,
    cluster = future::cluster
  )


  # Set default workers
  if(is.null(workers)){
    workers <- future::availableCores(omit = 1)
  }


  # Adjust workers if ntasks is smaller
  if(!is.null(ntasks)){
    workers <- min(workers, ntasks)
  }


  # Never exceed available cores
  workers <- min(workers, future::availableCores(omit = 1))


  # Set the future plan
  if(no_workers){
    invisible(future::plan(strategy_fun))
  } else {
    invisible(future::plan(strategy_fun, workers = workers))
  }

}
