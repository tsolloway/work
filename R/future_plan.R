#' future_plan
#' @description future_plan
#' @export
future_plan <- function(
    strategy = c("multisession", "multicore", "sequential", 'cluster'),
    workers = NULL,
    ntasks = NULL
){

  require(furrr)
  require(future)

  strategy <- match.arg(strategy)

  if(strategy == "multicore" && !supportsMulticore()){
    strategy <- "multisession"
  }


  strategy <- switch(
    strategy,
    sequential = future::sequential,
    multisession = future::multisession,
    multicore = future::multicore,
    cluster = future::cluster
  )


  if(is.null(workers)){
    workers <- availableCores(omit = 1)
  }


  if(!is.null(ntasks)){
    if(workers > ntasks){
      workers <- ntasks
    }
  }


  if(workers > availableCores(omit = 1)){
    workers <- availableCores(omit = 1)
  }


  return(
    plan(strategy = strategy, workers = workers)
  )

}
