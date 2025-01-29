# implements my view on when a condition is true, e.g. length(1) is true


# wrapper of sprintf that automatically collapses vectors
enhanced_sprintf <- function(fmt, ..., collapse = ',') {
  if (length(fmt) != 1) stop('Arg "format" must be a scalar (length 1)')

  dots <- list(...)
  if (!length(dots)) return(fmt)

  .collapse_args <- function(x) {
    if (length(x) > 1) {
      paste0(x, collapse = collapse) 
    } else {
      if (length(x) == 0) '' else x
    }
  }
  fixed_dots <- lapply(dots, .collapse_args)

  do.call(sprintf, c(list(fmt), fixed_dots))
}

# for building error messages, we must not crash on bad input
safe_enhanced_sprintf <- function(fmt, ...) {
  msg <- try(enhanced_sprintf(fmt, ...))
  if (is_error(msg)) {
    msg <- paste0('could not build error message: "', extract_error_message(msg), '"')
  }

  msg
}

extract_error_message <- function(err) {
  conditionMessage(attr(err, 'condition'))
}

is_condition_true <- function(cond) {
  if (length(cond) != 1) stop('condition must be a scalar')
  if (is.na(cond)) stop('condiction must not be missing (NA)')

  as.logical(cond)
}

is_error <- function(x) inherits(x, "try-error")

stop_unless <- function(cond, format, ...) {
	if (missing(format)) {
		stop("Argument 'format' is missing in die_unless()")
	}
  if (!is_condition_true(cond)) {
    msg <- safe_enhanced_sprintf(format, ...)
    # N.B: we do not want the call to be part of the message
    stop(msg, call. = FALSE)
  }

  invisible()
}

stop_if <- function(cond, ...) {
  stop_unless(!cond, ...)
}

mute <- function(..., messages = TRUE, warnings = TRUE, output = TRUE) {
  msg <- if (messages) suppressMessages else identity
  warn <- if (warnings) suppressWarnings else identity
  out <- identity
  if (output) {
    out <- function(...) {
        utils::capture.output(msg <- utils::capture.output(...), type = 'message')
        msg
    }
  } 

  invisible(out(warn(msg(...))))
}

# sorting strings depend on locales, and as such may not be reproducible across platforms and settings
reproducible_sort <- function(...) {
  old_ctype <- Sys.getlocale('LC_CTYPE')
  old_collate <- Sys.getlocale('LC_COLLATE')
  on.exit({
    Sys.setlocale('LC_CTYPE', old_ctype)
    Sys.setlocale('LC_COLLATE', old_collate)
  }, add = TRUE)

  Sys.setlocale('LC_CTYPE', 'C')
  Sys.setlocale('LC_COLLATE', 'C')

  sort(...)
}

get_elements <- function(lst, idx) {
  lapply(lst, getElement, idx)
}

"%||%" <- function(a, b) {
  if (length(a) == 0) b else a
}

file_size <- function(path) {
  file.info(path, extra_cols = FALSE)$size
}

fast_unlist <- function(x, recursive = FALSE, use.names = FALSE) {
  unlist(x, recursive, use.names)
}

get_text_logger <- function(quiet = FALSE) { if (quiet) function(...) {} else cli::cli_text }

get_env <- function(name, default = '') {
  value <- Sys.getenv(name)
  if (length(value) && !is.na(value) && nzchar(value)) value else default
}

set_env <- function(var, value) {
  args <- list(value)
  names(args) <- var
  do.call(Sys.setenv, args)
}


set_option <- function(option, value) {
  args <- list(value)
  names(args) <- option
  do.call(options, args)
}
