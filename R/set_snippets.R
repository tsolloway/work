#' set_snippets
#' @description setup the rstudio snippet file
#' @export
set_snippets <- function(path = NULL, on_exit_restart = TRUE){

  if(on_exit_restart) on.exit(work::restart(keep = TRUE))


  if( is.null(path) ){

    path <- work::get_path("snippets_r")

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


snippet install
	work::install_pkg("foo")


snippet winstall
	work::install_pkg_local("work")


snippet mehh
	fortunes::fortune()


snippet moo
	dadjokes::tell_joke()


snippet init
	install.packages("pak")
	pak::pkg_install("rstudioapi")
	Sys.setenv("GITHUB_PAT" = rstudioapi::askForPassword())
	.rs.restartR()
	pak::pkg_install("tsolloway/work", ask = FALSE, upgrade = FALSE, dependencies = TRUE)
	.rs.restartR()

	library(work)
	set_r_environment("git_local_dir", "~/Documents/GitHub/")
	set_r_profile("my")
	set_rstudio_prefs()
	set_snippets()
  install_init_pkgs()
	restart()
'

  writeLines(init_lines, path)
}

