execute_plan <- function(plan, src_pkgs, quiet = FALSE, ...) {
  if (!length(plan)) return()
  logger <- get_text_logger(quiet)
  for (i in seq_len(nrow(plan))) {
    row <- plan[i, , drop = TRUE]
    params <- row$params[[1]]
    logger('executing {.strong {row$action} on {.pkg {row$package}}}')
    switch(row$action, 
      unload = execute_action_unload(row$package, quiet = quiet, ...),
      load = do.call(execute_action_load, 
        c(list(as_srcpkg(row$package, src_pkgs), quiet = quiet, ...), params)),
      stop_if(TRUE, 'unknown action: "%s"', row$action)
    )
  }
}

execute_action_unload <- function(pkg_name, quiet, ...) {
  pkgload::unregister(pkg_name)
  # N.B: shoud not happen with source packages
  # # check since sometimes it may fail (e.g. for rlang)
  # if (pkg_is_loaded(pkg_name)) {
  #   if (!quiet) 
  #     cli::cli_text('failed to {.emph unregister} {.pkg pkg_name}, retrying with {.fn unload}')
  #   pkgload::unload(pkg_name)

  #   if (pkg_is_loaded(pkg_name)) {
  #     if (!quiet) 
  #       cli::cli_text('failed to {.emph unload} {.pkg pkg_name}, giving up...')
  #   }
  # }
}

execute_action_load <- function(pkg, quiet, attach = FALSE, roxygen = FALSE, ...) {
  pkg_load_wrapper(pkg, attach = attach, roxygen = roxygen, quiet = quiet, ...)
}

