

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
