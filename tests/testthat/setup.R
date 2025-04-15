PREVIOUS_SETTINGS <- settings()
withr::defer(restore_init(PREVIOUS_SETTINGS), teardown_env())

if (is_loaders_hack_enabled()) {
  unhack_r_loaders()
  withr::defer(hack_r_loaders(), teardown_env())
}

