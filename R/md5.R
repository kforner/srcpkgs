# compute a digest of md5sum of files in a directory
# @return a hash of the md5sum of the package source files , or '' if the list of files is empty
md5sum_dir <- function(dir, ...) {
  stop_unless(dir.exists(dir), 'directory does not exist: "%s"', dir)

  # make sure the paths are sorted and ouput in a reproducible way, because the digest will depend
  # on that order
  paths <- reproducible_sort(list.files(path = dir, full.names = TRUE, ...))
  if (!length(paths)) return('')

  hashes <- tools::md5sum(paths)

  digest::digest(hashes)
}