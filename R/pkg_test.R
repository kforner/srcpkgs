#' tests a package - runs its unit tests
#'
#' This function will test a source package using `testthat`, 
#' making sure the package and its source package dependencies are up-to-date and loaded 
#'
#' @inheritParams params
#' @param filter      filter in the tests to run. cf `testthat::test_dir()`
#' @param export_all  passed to [pkg_load()]. Enables the test functions to easily access to non-exported
#'                    functions. Caveat: If the pkg is already loaded and up-to-date with export_all=FALSE, it will not work.
#' @param ...   passed to `testthat::test_dir()`
#' @return the results as a `pkg_test` object, which is an empty listL if no tests were found
#' @importFrom testthat   test_dir
#' @export
#' @examples
#' \dontrun{
#'  pkg_test("mypkg")
#' }
pkg_test <- function(pkgid, filter = NULL, src_pkgs = get_srcpkgs(), export_all = TRUE, quiet = TRUE, ...)
{
  if (is_loaders_hack_enabled()) {
    unhack_r_loaders()
    on.exit(hack_r_loaders(), add = TRUE)
  }

  force(src_pkgs)
  pkg <- as_srcpkg(pkgid, src_pkgs)
  pkg_name <- pkg$package
  stop_unless(pkg_name %in% names(src_pkgs), 
    'package "%s" is not managed by srcpkgs, check `get_srcpkgs()`', pkg_name)

  # load and document if needed
  pkg_load(pkg, src_pkgs, quiet = quiet, export_all = export_all)

  test_path <- file.path(pkg$path, "tests/testthat")

  if (!dir.exists(test_path) || length(dir(test_path)) == 0) {
    # no tests found, return an empty  pkg_test
    return(invisible(new_pkg_test(pkg)))
  }

  res <- testthat::test_dir(test_path, filter = filter, stop_on_failure = FALSE, ...)

  invisible(new_pkg_test(pkg, res))
}

new_pkg_test <- function(pkg_name, test_results = list()) {
  attr(test_results, 'pkg') <- pkg_name
  class(test_results) <- c('pkg_test', class(test_results))
  test_results
}

# tells if the test is successful
# N.B: this is not a roxygen comment ON PURPOSE
#' @export
as.logical.pkg_test <- function(x, ...) {
  df <- as.data.frame(x)
  !any(df$error) && all(df$failed == 0)
}

# reformat the testthat_results data frame 
fortify_pkg_test <- function(df) {
 # remove context (which is deprecated)
  df$context <- NULL

  # hide user and system (not really informative)
  df$user <- df$system <- NULL

  # hide results
  df$result <- NULL

  # shorten file
  df$file <- sub("^test-", "", df$file, ignore.case = TRUE)
  df$file <- sub("\\.R", "", df$file, ignore.case = TRUE)

  # rename real into time
  names(df)[names(df) == "real"] <- "time"

  # reorder cols --> put "passed" after failed
  cols <- names(df)
  cols <- setdiff(cols, "passed")
  cols <- append(cols, "passed", after = match("failed", cols))
  df <- df[cols]

  df
}

#' @export
as.data.frame.pkg_test <- function(x, ...) {
  if (!length(x)) {
    return(data.frame(
      file = character(0), 
      test = character(0), 
      nb = integer(0), 
      failed = integer(0), 
      passed = integer(0), 
      skipped = logical(0), 
      error = logical(0), 
      warning = integer(0), 
      time = numeric(0))
    )
  }
  fortify_pkg_test(NextMethod())
}

#' @export
print.pkg_test <- function(x, ...) {
  pkg <- attr(x, 'pkg')

  if (!length(x)) {
    cli::cli_h1(paste0("package ", pkg$package, " has no tests"))
    return(invisible())
  }

  df <- as.data.frame(x)
  results <- df$result

  ### by test
  bad <- which(df$failed > 0 | df$error)
  print_text_table(df, 
    title = paste0("Test results by test for package ", pkg$package),
    hilite_rows = bad, heatmap_columns = 'time')

  ### by file
  sdf <- summary(x)
  bad <- which(sdf$failed > 0 | sdf$error)
  print_text_table(sdf, 
    title = paste0("Test results by file for package ", pkg$package),
    hilite_rows = bad, heatmap_columns = 'time')

  ### overview
  sdf <- summary(x, col = NULL)
  final_message <- if (as.logical(x)) "Tests successful :) !" else "Test failed :( ..."
  print_text_table(sdf, title = paste0("Test results overview for package ", pkg$package), footnote = final_message)
  
  invisible()
}

# summarizes by test source file. Use by=NA to summarize all rows to get an overview
# N.B: this is not a roxygen comment ON PURPOSE
#' @export
summary.pkg_test <- function(object, col = 'file', ...) {
  if (!length(object)) {
    return(data.frame(
      file = character(0), 
      nb = integer(0), 
      failed = integer(0), 
      passed = integer(0), 
      skipped = logical(0), 
      error = logical(0), 
      warning = integer(0), 
      time = numeric(0))
    )
  }

  df <- as.data.frame(object)

  stop_unless(length(col) <= 1, "col must be a column name or NULL")
  stop_if(length(col) == 1 && !col %in% names(df), "bad column name")

  # special case for columns "skipped" and "error", which are logical
  # for correct aggregation, must be integer
  df$skipped <- as.integer(df$skipped)
  df$error <- as.integer(df$error)

  ### select non-character columns: summary of numeric columns (sums)
  data_cols <- which(!vapply(df, is.character, TRUE))

  # aggregation grouping elements
  by <- list(rows = rep(TRUE, nrow(df)))
  if (length(col)) {  
    by <- list(df[[col]])
    names(by) <- col
  }

  # aggregation function
  .col_stats <- function(x) { if (is.logical(x)) any(x) else sum(x) }
  sdf <- stats::aggregate(df[, data_cols], by = by, .col_stats)
  if (length(col)) sdf <- sdf[order(sdf[[col]]), , drop = FALSE] else sdf$rows <- NULL
  rownames(sdf) <- NULL

  sdf
}
