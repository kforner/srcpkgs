
examples_srcpkgs_basic <- function() {
  pkg_create('.', 'AA', imports = 'BB')
  pkg_create('.', 'BB', suggests = 'stats')

  find_srcpkgs('.')
}

examples_srcpkgs_star <- function() {
  pkg_create('.', 'AA', suggests = 'roxygen2')
  pkg_create('.', 'BB', suggests = 'stats')
  pkg_create('.', 'CC')
  pkg_create('.', 'DD')
  pkg_create('.', 'EE', 
    imports = c('AA', 'BB'), 
    depends = c('BB', 'DD'), 
    suggests = c('CC', 'roxygen2'))

  find_srcpkgs('.')
}

examples_srcpkgs_complex_imports <- function() {
  # C-->B-->A, F-->D-->B, E-->A, Z
  # N.B: we use namespace = TRUE because for unloading, only the ns-imports are considered
  pkg_create('.', 'AA', imports = 'stats', namespace = TRUE)
  pkg_create('.', 'BB', imports = 'AA', namespace = TRUE)
  pkg_create('.', 'CC', imports = 'BB', namespace = TRUE)
  pkg_create('.', 'DD', imports = 'BB', namespace = TRUE)
  pkg_create('.', 'FF', imports = 'DD', namespace = TRUE)
  pkg_create('.', 'EE', imports = 'AA', namespace = TRUE)
  pkg_create('.', 'ZZ')

  find_srcpkgs('.')
}

examples_srcpkgs_complex_deps <- function() {
  # A->B->C->D, B->D->E, Z
  # N.B: we use namespace = TRUE because for unloading, only the ns-imports are considered
  pkg_create('.', 'AA', imports = c('stats', 'BB'), depends = 'CC')
  pkg_create('.', 'BB', imports = c('CC', 'DD'))
  pkg_create('.', 'CC', imports = 'DD', suggests = 'EE')
  pkg_create('.', 'DD', depends = 'EE')
  pkg_create('.', 'EE')
  pkg_create('.', 'ZZ')

  find_srcpkgs('.')
}
 