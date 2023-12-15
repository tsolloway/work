#' set_snippets
#' @description setup the rstudio snippet file
#' @export
set_snippets <- function(path = NULL, on_exit_restart = TRUE){

  if(on_exit_restart) on.exit(work::restart(obs_keep = TRUE))

  if( is.null(path) ){
    path <- work:::get_path_r("r_snippets")

  }else if( !is.null(path) ){
    path <- path %>% normalizePath(mustWork = TRUE)
  }


  init_lines <- '
snippet start
	library(magrittr)
	library(work)


snippet pipe
	library(magrittr)


snippet if
	if( cond ){

	}


snippet ie
	if( cond ){

	}else{

	}


snippet ifs
	if( cond ){

	}else if( cond ){

	}


snippet for
	for( i in vct ){

	};rm(i)


snippet walk
	purrr::walk(.x, ~.x)


snippet map
	purrr::map(.x, ~.x)


snippet fun
	name <- function(){

	}


snippet while
	while( cond ){

	}


snippet restart
	work::restart()


snippet winstall
	work::install_pkg("work")


snippet mehh
	fortunes::fortune()


snippet moo
	dadjokes::tell_joke()


snippet init
	###############################
	# Get work
	###############################
	install.packages("pak")
	pak::pkg_install("rstudioapi")
	Sys.setenv("GITHUB_PAT" = rstudioapi::askForPassword())
	pak::pkg_install("Material-Dev/material-r-analytics", ask = FALSE, upgrade = FALSE, dependencies = TRUE)
	.rs.restartR()


	###############################
	# Option 1: Use Wrapper
	###############################
	analytics::install_r() # check if R is up to date and install proper version if necessary
	analytics::init_install() # init wrapper function that does the steps below
	#restart Rstudio


	###############################
	# Option 2: Step through setup
	###############################
	analytics::install_r() # check if R is up to date and install proper version if necessary
	analytics::install_initial_recommended_r_pkgs() # installs a bunch of common R packages
	analytics::install_material(type = "all") # installs the material packages
	analytics:::set_r_environ("init") # sets your r environment
	analytics:::set_r_profile("init") # sets your r to load magrittr on startup
	analytics:::set_rstudio_prefs() # activates good productivity Rstudio preferences
	analytics:::set_snippets() # sets productivity snippets
	analytics::restart()
	#restart Rstudio
'

  writeLines(init_lines, path)
}

