.CONFIG <- new.env(parent = emptyenv())

# return the stored config or NULL
get_config <- function(key) {
  get0(key, .CONFIG)
}

set_config <- function(key, value) {
  old <- get_config(key)
  assign(key, value, envir = .CONFIG)
  invisible(old)
}

delete_config <- function(key) {
  old <- get_config(key)
  suppressWarnings(rm(list = key, envir = .CONFIG))
  invisible(old)
}