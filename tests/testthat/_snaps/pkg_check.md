# pkg_check

    Code
      print(chk)
    Output
      [36m── R CMD check results ───────────────────────────────────────────── BB 1.0 ────[39m
      Duration: 1.5s
      
      [31m❯ checking tests ...[39m
        See below...
      
      [36m── Test failures ───────────────────────────────────────────────── testthat ────[39m
      
      in path /dummy/setup.R:1:2
       15. └─base::.handleSimpleError(...)
       16.   └─testthat (local) h(simpleError(msg, call))
       17.     └─rlang::abort(...)
      Execution halted
      
        package errors warnings notes time
      1      BB      1        0     0  1.5

