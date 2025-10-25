# pkg_test_s3_methods

    Code
      print(fix_test_result_timings(res))
    Message
      
      [36m──[39m [1mTest results by test for package AA[22m [36m─────────────────────────────────────────[39m
    Output
      ╒═══════╤═══════╤══╤══════╤══════╤═══════╤═════╤═══════╤════╕
      [1m│ file  │ test  │nb│failed│passed│skipped│error│warning│time│[22m
      ╞═══════╪═══════╪══╪══════╪══════╪═══════╪═════╪═══════╪════╡
      [41m│ errors│ errors│ 1│ 0    │ 1    │ FALSE │ TRUE│ 0     │ [48;5;100m0[41m  │[49m
      [41m│failure│failure│ 1│ 1    │ 0    │ FALSE │FALSE│ 0     │ [48;5;100m0[41m  │[49m
      [41m│ misc  │ misc1 │ 2│ 1    │ 1    │ FALSE │FALSE│ 0     │ [48;5;100m0[41m  │[49m
      [41m│ misc  │ misc2 │ 2│ 1    │ 0    │ TRUE  │FALSE│ 0     │ [48;5;100m0[41m  │[49m
      │ misc  │ misc3 │ 2│ 0    │ 2    │ FALSE │FALSE│ 0     │ [48;5;100m0[49m  │
      [41m│ misc  │ misc4 │ 2│ 0    │ 1    │ FALSE │ TRUE│ 1     │ [48;5;100m0[41m  │[49m
      [41m│ mixed │ mixed │ 2│ 1    │ 1    │ FALSE │FALSE│ 0     │ [48;5;100m0[41m  │[49m
      │ skip  │ skip  │ 1│ 0    │ 0    │ TRUE  │FALSE│ 0     │ [48;5;100m0[49m  │
      │success│success│ 1│ 0    │ 1    │ FALSE │FALSE│ 0     │ [48;5;100m0[49m  │
      [41m│warning│warning│ 3│ 2    │ 0    │ FALSE │FALSE│ 1     │ [48;5;100m0[41m  │[49m
      ╘═══════╧═══════╧══╧══════╧══════╧═══════╧═════╧═══════╧════╛
    Message
      
      [36m──[39m [1mTest results by file for package AA[22m [36m─────────────────────────────────────────[39m
    Output
      ╒═══════╤══╤══════╤══════╤═══════╤═════╤═══════╤════╕
      [1m│ file  │nb│failed│passed│skipped│error│warning│time│[22m
      ╞═══════╪══╪══════╪══════╪═══════╪═════╪═══════╪════╡
      [41m│ errors│ 1│ 0    │ 1    │ 0     │ 1   │ 0     │ [48;5;100m0[41m  │[49m
      [41m│failure│ 1│ 1    │ 0    │ 0     │ 0   │ 0     │ [48;5;100m0[41m  │[49m
      [41m│ misc  │ 8│ 2    │ 4    │ 1     │ 1   │ 1     │ [48;5;100m0[41m  │[49m
      [41m│ mixed │ 2│ 1    │ 1    │ 0     │ 0   │ 0     │ [48;5;100m0[41m  │[49m
      │ skip  │ 1│ 0    │ 0    │ 1     │ 0   │ 0     │ [48;5;100m0[49m  │
      │success│ 1│ 0    │ 1    │ 0     │ 0   │ 0     │ [48;5;100m0[49m  │
      [41m│warning│ 3│ 2    │ 0    │ 0     │ 0   │ 1     │ [48;5;100m0[41m  │[49m
      ╘═══════╧══╧══════╧══════╧═══════╧═════╧═══════╧════╛
    Message
      
      [36m──[39m [1mTest results overview for package AA[22m [36m────────────────────────────────────────[39m
    Output
      ╒══╤══════╤══════╤═══════╤═════╤═══════╤════╕
      [1m│nb│failed│passed│skipped│error│warning│time│[22m
      ╞══╪══════╪══════╪═══════╪═════╪═══════╪════╡
      │17│ 6    │ 7    │ 2     │ 2   │ 2     │ 0  │
      ╘══╧══════╧══════╧═══════╧═════╧═══════╧════╛

