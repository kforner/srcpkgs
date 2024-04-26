execute_plan <- function(plan, src_pkgs, quiet = FALSE, ...) {
  if (!length(plan)) return()
  logger <- get_text_logger(quiet)
  for (i in seq_len(nrow(plan))) {
    row <- plan[i, , drop = TRUE]
    pkg <- as_srcpkg(row$package, src_pkgs)
    logger('executing {.strong {row$action} on {.pkg {row$package}}}')
    switch(row$action, 
      unload = execute_action_unload(pkg, quiet = quiet, ...),
      load = execute_action_load(pkg, quiet = quiet, ...),
      doc_and_load = execute_action_doc_and_load(pkg, quiet = quiet, ...),
      stop_if(TRUE, 'unknown action: "%s"', row$action)
    ) 
  }
}

execute_action_unload <- function(pkg, quiet, ...) {
  pkg_name <- pkg$package
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

execute_action_load <- function(pkg, quiet, ...) {
  pkg_load_wrapper(pkg, roxygen = FALSE, quiet = quiet, ...)
}

execute_action_doc_and_load <- function(pkg, quiet, ...) {
  pkg_load_wrapper(pkg, roxygen = TRUE, quiet = quiet, ...)
}