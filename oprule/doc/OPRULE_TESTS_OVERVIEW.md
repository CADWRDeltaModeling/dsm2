# oprule tests: coverage overview

A high-level map of what the tests cover, so conceptual gaps are easy to spot. Detail lives in the tests themselves (names say what they check) and in [OPRULE_TEST_PLAN.md](OPRULE_TEST_PLAN.md); how the rule system works is in [OPRULE_REFERENCE.md](OPRULE_REFERENCE.md); build and run instructions are in [DEVELOPER_NOTES.md](DEVELOPER_NOTES.md).

## 1. The three test programs

| Program | Source | Cases | What it exercises | Needs |
|---|---|---|---|---|
| `oprule_smoke_tests` | `test/smoke/SmokeTests.cpp` | 19 | Quick sanity pass: parse, run, defer, ramp, data source, one end-to-end rule. | oprule libs |
| `oprule_core_tests` | `test/core/CoreTests.cpp` | 77 | The generic library: grammar, lexer, expression nodes, rule structure, activation, ramps, conflicts, the rule log, pinned defects. Mock model only. | oprule libs |
| `oprule_dsm2_tests` | `test/dsm2/Dsm2BindingTests.cpp` | 77 | The **real** DSM2 C++ binding (`dsm2/src/oprule_interface`) linked against a mock of the Fortran model, driven by the rules in the study input files; also the log set-up and interface descriptions. | oprule libs + Loki headers |
| `test_model_interface` (Fortran, test-drive) | `dsm2/tests/model_interface/test_model_interface.f90` | 16 | The **real** Fortran routines of `model_interface.f90` that the C++ calls: name lookups, flows, gates and devices, data sources, time, the log level. Checks the mock's description of them. | the full DSM2 build (see DEVELOPER_NOTES.md) |

Run all three after a build: `for t in smoke core dsm2; do ./oprule/test/oprule_${t}_tests; done` (see DEVELOPER_NOTES.md for the build). Setting `OPRULE_CORPUS_DIR=/scratch/psandhu/dsm2_studies/common_input` makes the DSM2 program also parse the complete real `oprule_*.inp` files.

## 2. Test seams (what is real and what is mocked)

```
 rule text ──► [flex/bison parser]  real
                    │ model names        ┌──────────────────────────────────────────┐
                    ▼                    │ core / smoke: mock lookup + mock variables│
              [rule objects]  real       │ dsm2: real DSM2HydroNamedValueLookup,    │
                    │                    │       real factories and interfaces       │
              [OperationManager] real    └──────────────────────────────────────────┘
                    │ set / eval / data sources
                    ▼
        Fortran model (gates, flows, time, data sources)   MOCKED (support/Dsm2FortranMock.*)
```

The mock Fortran layer documents the contract the C++ depends on: every mocked function carries a comment naming the Fortran routine it mirrors, what it does, and any quirk (marked `FORTRAN QUIRK`). Behaviour that would be undefined in Fortran (array index out of range, `exit(n)`) throws `FortranContractViolation` / `FortranExit` so a test fails loudly instead of reading garbage. Assumptions that were not verified against a running model are marked `ASSUMPTION` (array layout, lower-case storage of names, Julian-minute epoch). The suite `fortran_mock_contract` keeps the mock consistent with its own documentation.

## 3. Conceptual coverage

Legend: **C** core tests, **D** DSM2 tests, **S** smoke tests. "pinned" means the test records a believed defect.

| Concept | Where | Notes |
|---|---|---|
| Arithmetic, precedence, unary minus, power | C `grammar_numeric`, C `pinned` | `-2^2`, left-associative `^` pinned |
| Math functions, min/max, `ifelse`, `lookup` | C `grammar_numeric` | lookup error cases included |
| Booleans, comparisons, `AND/OR/NOT`, `STARTUP` | C `grammar_boolean`, D `features_not_in_the_studies` | type errors checked |
| Named numeric / boolean expressions, live evaluation | C `grammar_names_and_time` | redefinition error recorded |
| Time terms, date and season literals | C (recording factory), D `time_nodes` | seasons do not wrap the year |
| Lexer: whitespace, number formats, case, reserved words, quoting | C `lexer` | |
| Stateful nodes: `accumulate`, `predict`, `pid`, `ipid` | C `stateful_nodes` | `accumulate` ignores `dt`, `predict` needs `init()` (pinned) |
| Lagged expressions `name(t-1)` | C, S (pinned) | not implemented |
| Parser state: restart after error, redefinition, duplicate rule names | S, C | |
| `THEN` / `WHILE` structure and precedence | C `rule_structure`, `top_level_chains` | checked through timing |
| Edge-triggered activation, re-arming, `STARTUP` | S, C `runtime`, D | missed edge while active pinned |
| Activation timing relative to the time loop | S, C, D | trigger at end of step *n*, effect in step *n+1* |
| Ramps: linear, `dt` longer than ramp, zero length, re-evaluated target | C `runtime`, D `study_rule_behaviour` | |
| Static vs time-dependent targets (snapshot vs re-read, permanent data source) | C `runtime`, D `model_interfaces` | |
| Conflict policy: deferral, pool order, priority values | S, C `runtime`, D `study_rule_behaviour` | |
| Alternative conflict policies (replace / ignore) | C `alternative_conflict_policies` | code paths exist but nothing uses them |
| What counts as overlapping in DSM2 | D `resolver_overlap` | device-level, install vs device |
| DSM2 model names: registry, read/write, arguments, error types | D `name_registry`, `factory_arguments` | case rules per lookup |
| DSM2 interfaces: set/eval, data sources, install/free | D `model_interfaces`, `fortran_mock_contract` | |
| Read-only nodes: channel interpolation, reservoirs, time series | D `read_only_nodes` | |
| Reading the rule input tables (`#`, `^`, quotes, sort order) | D `study_inputs` | `${VAR}` and layering not covered |
| Real study rules: parse and behave | D `study_inputs`, `study_rule_behaviour` | see section 4 |
| Functions Fortran calls (`parse_rule_`, `advance…`) | D `fortran_entry_points` | one test, process-wide manager |
| Crashes and `exit()` paths | C `pinned_runtime` (child process) | |
| Rule log: events, levels, deferral policy, no extra evaluation | C `rule_log`, D `rule_log_binding` | design in OPRULE_REFERENCE.md B10; helpers in `support/LogCapture.h` |
| Interface descriptions used by the log | D `rule_log_interfaces` | indices, not names |
| Real Fortran behind the mock | Fortran `test_model_interface` | see section 8 |

## 4. Study input patterns covered

Fixtures in `test/data/*.inp` are subsets of `dsm2_studies/common_input/oprule_*.inp`; every row is parsed by `study_inputs/every_rule_and_expression_parses`, and these patterns are also run over time:

| Pattern in the inputs | Example rule | Test (DSM2 program) |
|---|---|---|
| Device property from a time series, `WHEN TRUE` | `clfct_gate_op`, `*_height`, `*_nduplicate`, `glc_barrier_elev/width` | `gate_op_from_a_time_series_becomes_a_permanent_source`, `height_elevation_width_and_nduplicate_…` |
| Static `gate_coef` from a computed expression | `clfct_gate_cf_from` | `gate_coef_from_an_expression_is_applied_only_once` |
| Two actions joined by `WHILE` | `dcc_gate_op` | `while_pair_sets_both_directions` |
| Constant set at a calendar date | `morrow_c_change_ndup`, `sandmound_*` | `datetime_trigger_sets_a_constant` |
| Stage / velocity triggers, named boolean expressions, `AND/OR/NOT` | `mscs_*` | `directions_of_one_device_are_ramped_one_after_the_other` |
| Ramped static `gate_coef` with `DATETIME` triggers on opposite sides | `decker_is_north_weir_*` | `restoration_weir_ramps_down_then_up` |
| External flow switched by stage hysteresis | `dicu_div_151_off/on` | `external_flow_switches_with_stage` |
| `gate_install` driven by a series | `glc_install_in/out` | `gate_is_installed_and_removed_by_a_series` |
| `SEASON` triggers from named expressions, `OPEN/CLOSE/INSTALL/REMOVE`, `IFELSE` target, `NOT expr` | planning barrier rules | `seasonal_rules_start_on_the_right_step_and_queue_per_device` |
| Trigger that reads a writable name (`gate_install(...) == INSTALL`) | `vamp_8500_remove` | `trigger_reads_the_gate_install_state` |
| Rows disabled with `^`, mixed-case names, `@` in gate names | `^retired_rule`, `FalseBarrier_*`, `old_r@tracy_barrier` | `reader_follows_…`, `statements_come_in_model_parse_order` |

## 5. Language and binding features the studies do not use

Covered so they keep working when someone starts using them: `res_stage`, `res_flow`, `chan_flow`, `transfer_flow` (writable), `STARTUP`, calendar terms (`MONTH`, …) as triggers, `lookup` on a series, `accumulate` in a trigger, top-level `THEN` chains (with the manager bypassed, see pinned defects), `ifelse`/`min3`/`max3` (core), `predict` / `pid` / `ipid` (core, node level), alternative conflict policies, `gate_coef` with `direction=both` (pinned).

## 6. Pinned defects and limitations

Each is asserted in its current (undesirable) form, with a comment naming the intended behaviour. When one of these tests fails after a change, check whether the defect was fixed and update the test on purpose. IDs refer to OPRULE_TEST_PLAN.md / OPRULE_REFERENCE.md B9.

| ID | Behaviour | Test |
|---|---|---|
| D-01 | `SEASON == 01JAN 12:30` ignores the hour | C `pinned`, D `time_nodes` |
| D-02 | copies of `chan_stage` / `chan_flow` truncate a fractional `dist` | D `read_only_nodes` |
| D-03 | device interface `operator==` compares the wrong fields | D `read_only_nodes` |
| D-04 | `gate_op(..., to_from_node)` reads only the from-node value | D `model_interfaces`, `fortran_mock_contract` |
| D-07 | a chain nested in `WHILE` is invisible to conflict detection | S, C `pinned_runtime` |
| D-11 | lagged expressions throw (a pointer) | S, C `pinned` |
| D-13 | `gate_coef ... direction=both` ends the run (Fortran `exit(3)`) | D `features_not_in_the_studies`, `fortran_mock_contract` |
| D-14 | `accumulate` does not scale by `dt` | C `pinned` |
| D-16 | a rising edge while a rule is active is missed | C `pinned_runtime` |
| D-17 | a top-level `THEN` chain crashes `addRule` | C `pinned_runtime` (child process) |
| D-21 | lower-case month names evaluate to 0 | C `pinned` |
| D-22 | `WHILE` of unequal-length actions asserts (Debug) | C `pinned_runtime` (child process) |
| D-23 | `PREDICT` in a trigger asserts: `init()` is never called | C `pinned_runtime` (child process) |
| new | unknown channel number is not validated (reads `chan_geom(0)`) | D `factory_arguments` |
| new | unknown reservoir: `res_stage` returns an empty node, `res_flow` indexes `res_geom(-901)` | D `factory_arguments` |
| new | `gate_nduplicate` from a series: the model sees a non-integer after the first step | D `study_rule_behaviour` |
| new | any two actions on one gate device overlap, so opposing-direction rules are serialized (e.g. `mscs_close_from` then `mscs_close_to`; `glc_barrier_elev` then `glc_barrier_in`) | D `resolver_overlap`, `study_rule_behaviour` |

## 7. Not covered (known gaps)

- The C/Fortran ABI mismatch of the `*_datasource` boolean (D-05): a mock cannot reproduce it.
- Uninitialised `_active` in `ActionSet` / `ActionChain` (D-18): reading it is undefined, so it is not asserted.
- `${VAR}` substitution, input layering and duplicate names across layers (input_storage is not under test).
- Fortran-side input processing (`process_input_oprule`, `get_inp_data`) and DSS time series: `ts` values are supplied by the test.
- Real Fortran routines: `test_model_interface` checks `model_interface.f90` (section 8), but only the routines in that file. Other Fortran the binding depends on (`ext2int`, `CompPointAtDist`, `get_surf_elev`, `process_*`, `get_inp_data`, `store_values`) is still mirrored from reading the source.
- Restart / warm-start behaviour of rule state.
- Logging in a complete model run (LOG-01 bitwise comparison, LOG-12 overhead): needs a golden study run (INT-09). Named expression values are not logged (LOG-14).
- Performance with many rules.

## 8. Fortran test of `model_interface.f90`

`dsm2/tests/model_interface/test_model_interface.f90` (test-drive, registered in `dsm2/tests/CMakeLists.txt`) calls the real Fortran routines the C++ binding uses and asserts what the mock in `support/Dsm2FortranMock.cpp` assumes. Build and run: DEVELOPER_NOTES.md section 6.

| Fortran test | Backs (mock behaviour) |
|---|---|
| `direction_constants` | `direct_to_node` = 1, `direct_from_node` = -1, `direct_to_from_node` = 0 |
| `name_lookups` | gate / device / reservoir arguments lower-cased; `qext_index`, `transfer_index`, `ts_index` exact match; absent `ts_index` is -1, others `miss_val_i`; reservoir connection takes the internal node number |
| `gate_name_must_be_stored_lower_case` | a gate stored with capitals is never found |
| `external_and_transfer_flows`, `flows_are_single_precision` | flows are `real*4`: values are read back rounded to single precision (this corrected the mock) |
| `gate_install` | installed by default; `set_gate_install(0)` removes and zeroes device flows; any non-zero value installs |
| `device_op_coefficients`, `to_from_op_coefficient_ignores_the_to_node` | per-direction get/set; unknown direction gives -901 on get and no change on set; `to_from_node` get averages the from-node value with itself (D-04) |
| `device_properties`, `nduplicate_is_rounded_by_the_setter`, `flow_coefficients` | height, width (`maxWidth`), elevation (`baseElev`), rounded `nduplicate`, `gate_coef` to/from |
| `data_source_types`, `fetch_data` | `timedep` selects expression or constant data; constant and DSS sources; any other type gives `miss_val_r` |
| `time_functions`, `reference_minute_of_year` | `julmin` epoch (01JAN1900 00:00 = minute 1440), 0-based day of year, leap years in `get_reference_minute_of_year` |
| `oprule_log_level` | the scalar wins; otherwise `print_level` 4, 5, 6 give 1, 2, 3; 3 or lower or unset gives 0; capped at 3 |

Not testable there: `gate_coef` with `direction=both` (the routine calls `exit(3)`), the C++ side of the `bool` argument (D-05), and routines outside `model_interface.f90`.
