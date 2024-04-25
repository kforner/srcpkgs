execute_plan <- function(plan, quiet = FALSE) {
  if (!length(plan)) return()
  
  for (i in seq_len(nrow(plan))) {
    row <- plan[i, , drop = TRUE]
    if (!quiet)     
      cli::cli_text('executing {.strong {row$action} on {.pkg {row$package}}}')
    switch(row$action, 
      unload = execute_action_unload(row$package, quiet = quiet),
      load = execute_action_load(row$package, quiet = quiet),
      stop_if(TRUE, 'unknown action: "%s"', row$action)
    ) 
  }
}

execute_action_unload <- function(pkg_name, quiet) {
  pkgload::unregister(pkg_name)
  # check since sometimes it may fail (e.g. for rlang)
  if (pkg_is_loaded(pkg_name)) {
    if (!quiet) 
      cli::cli_text('failed to {.emph unregister} {.pkg pkg_name}, retrying with {.fn unload}')
    pkgload::unload(pkg_name)

    if (pkg_is_loaded(pkg_name)) {
      if (!quiet) 
        cli::cli_text('failed to {.emph unload} {.pkg pkg_name}, giving up...')
    }
  }
}

execute_action_load <- function(pkg_name, quiet) {
  browser()
}