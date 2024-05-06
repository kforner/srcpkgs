PREVIOUS_SETTINGS <- settings()
withr::defer({
  # print(PREVIOUS_SETTINGS)
  restore_init(PREVIOUS_SETTINGS)}, teardown_env())
