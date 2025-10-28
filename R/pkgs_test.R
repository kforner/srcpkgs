

#' tests a list of source packages
#'
#' @inheritParams params
#' @param filter  filter out the packages to test using this pattern
#' @param ...   passed to `pkg_test`
#' @return the results as a `pkgs_test` object
#' @export
#' @examples
#' ## create a dummy collection of srcpkgs by replicating the dummy srcpkg
#'  pkg <- setup_and_get_dummy_srcpkg()
#'  pkgs <- srcpkgs(list(pkg, pkg))
#' 
#'  res <- pkgs_test(pkgs, error_on = "never")
#'  print(res)
pkgs_test <- function(pkgids = names(filter_srcpkgs(src_pkgs, filter)), src_pkgs = get_srcpkgs(), filter = NULL, quiet = TRUE, ...)
{
  force(src_pkgs)
  if (!length(pkgids)) stop('No package to test')
  pkgs <- as_srcpkgs(pkgids, src_pkgs)

  .test_pkg <- function(pkg) {
    try(pkg_test(pkg, src_pkgs = src_pkgs, quiet = quiet, ...), silent = quiet)
  }
  lst <- lapply(pkgs, .test_pkg)
  class(lst) <- 'pkgs_test'
  invisible(lst)
}

#' @export
as.data.frame.pkgs_test <- function(x, ...) {
  .row <- function(test_res) {
    if (!length(test_res)) {
      return(data.frame(nb = 0L, failed = 0L, passed = 0L, skipped = 0L, error = 0L, warning = 0L, time = 0))
    }

    if (is_error(test_res)) {
      # trick to create an empty data frame like test_results

      dummy_res <- testthat::ListReporter$new()$get_results()
      row <- as.data.frame(dummy_res)
      row[1,] <- NA

      row <- fortify_pkg_test(row)
      row$error <- 1
      row$file <- row$test <- NULL
      return(row)
    }
 
    summary(test_res, col = NULL)
  }
  rows <- lapply(x, .row)
  df <- do.call(rbind, rows)

  ## add package column
  df <- cbind(package = names(x), df, stringsAsFactors = FALSE)

  df
}

#' @export
as.logical.pkgs_test <- function(x, ...) {
   df <- as.data.frame(x)
   all(df$error == 0) && all(df$failed == 0)
}

#' @export
summary.pkgs_test <- function(object, ...) {
  df <- as.data.frame(object)
  .sum_rm_na <- function(...) sum(..., na.rm = TRUE)
  df$package <- 1 # N.B: package column is converted to counts
  sdf <- stats::aggregate(df, by = list(.dummy = rep(TRUE, nrow(df))), .sum_rm_na)
  sdf$.dummy <- NULL
 
  sdf
}

#' @export
print.pkgs_test <- function(x, ...) {
  df <- as.data.frame(x)

  # remove NAs to avoid warning in print_text_table(): replace them by 0
  df$time[is.na(df$time)] <- 0

  # only keep ms
  df$time <- format(df$time, digits = 3)


  ### by package
  bad <- which(df$failed > 0 | df$error > 0)

  print_text_table(df, 
    title = "Test results by package",
    hilite_rows = bad, heatmap_columns = 'time')

  ### overview
  sdf <- summary(x)
  sdf$time <- format(sdf$time, digits = 3)

  print_text_table(sdf, title = "Test results overview")

  ### overall result (parsable)
  cat('\n')
  ok <- as.logical(x)
  if (ok) cat("SUCCESS\n") else cat("FAILED\n")

  invisible()
}