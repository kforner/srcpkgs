# implements my view on when a condition is true, e.g. length(1) is true


#' wrapper of sprintf that automatically collapses vectors
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
  if (inherits(msg, 'try-error')) {
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