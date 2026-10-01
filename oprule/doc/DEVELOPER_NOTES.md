# oprule / DSM2: developer notes and gotchas

Notes for anyone (or any assistant) picking up work on the operating-rule code. They record what was learned the hard way so it is not rediscovered. Facts were checked against the source or by running the tests unless marked *(unverified)*.

Companion documents in this directory:

- [OPRULE_REFERENCE.md](OPRULE_REFERENCE.md): how rules work end to end, plus suspected defects (B9).
- [OPRULE_USER_GUIDE.md](OPRULE_USER_GUIDE.md): how to write operating rules (for users).
- [OPRULE_LOG_USER_GUIDE.md](OPRULE_LOG_USER_GUIDE.md): how to switch the rule log on and read the HDF5 file.
- [OPRULE_TEST_PLAN.md](OPRULE_TEST_PLAN.md): every open question and suspected defect as a test, with observed results.

---

## 1. Build and environment (HPC login node `hn`)

| Item | Value |
|---|---|
| Toolchain | `module purge; module load intel/2024.0 cmake/3.28.3` gives `icx`, `icpx`, `ifx` (this is what `build_hpc5.sh` loads). `build_hpc4.sh` uses `intel/2025.1` and `cmake/3.29.3`. |
| flex / bison | `/usr/bin/flex` 2.6.1, `/usr/bin/bison` 3.0.4. Needed to generate the parser at build time. |
| Boost | Bundled in `deps/boost_1_83_0` (includes Boost.Test headers and libs). |
| Loki | `deps/loki-0.1.7/include/loki` (the DSM2 resolver uses `MultiMethods.h`). |
| C++ standard | 14 (`linux_options.cmake`). The code uses `std::binary_function` / `bind2nd`, so do not go to 17+ without fixing that. |
| Branch used for this work | `oprule_enhance` |

### Do not run `build_hpc5.sh` casually
It starts with `cmake -E remove_directory build`, which deletes the whole `build/` tree (including fetched dependencies under `build/_deps`). Use a separate build directory for experiments. `build_hpc4.sh` keeps `build/`.

### The existing `build/` directory
Its `CMakeCache.txt` has `Boost_INCLUDE_DIR` pointing at another user's path (`/home/knam/...`). Do not trust it for new work; configure a fresh directory.

### Building and running the oprule tests (no HDF5, no Fortran stdlib, no network)
```bash
module purge && module load intel/2024.0 cmake/3.28.3
cmake -S oprule/test/standalone -B /scratch/psandhu/dsm2_oprule_build -G "Unix Makefiles" \
  -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
  -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_CXX_STANDARD=14 -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
  -DBOOST_ROOT=$PWD/deps/boost_1_83_0
cmake --build /scratch/psandhu/dsm2_oprule_build -j 4        # builds smoke, core and dsm2 test programs
(cd /scratch/psandhu/dsm2_oprule_build && ctest --output-on-failure)
# or run them directly:
/scratch/psandhu/dsm2_oprule_build/oprule/test/oprule_smoke_tests     # 19 test cases
/scratch/psandhu/dsm2_oprule_build/oprule/test/oprule_core_tests      # 100 test cases
/scratch/psandhu/dsm2_oprule_build/oprule/test/oprule_dsm2_tests      # 92 test cases
# also test the HDF5 sink of the rule log: add -DOPRULE_HDF5_ROOT=$PWD/deps/hdf5-1.14.2 to the cmake line above
/scratch/psandhu/dsm2_oprule_build/oprule/test/oprule_hdf5_tests      # 11 test cases (only with OPRULE_HDF5_ROOT)
# optional: also parse the complete real study files
OPRULE_CORPUS_DIR=/scratch/psandhu/dsm2_studies/common_input /scratch/psandhu/dsm2_oprule_build/oprule/test/oprule_dsm2_tests --run_test=study_inputs
```
Notes:

- Do not use `-C linux_options.cmake` with the standalone project: it builds paths from `CMAKE_SOURCE_DIR`, which is `oprule/test/standalone` there. Pass the compilers and `BOOST_ROOT` explicitly.
- CMake prints a harmless warning that `BOOST_ROOT` is ignored (policy CMP0144); `Boost` is still found.
- Compiling prints many warnings (MSVC `#pragma warning`, deprecated `binary_function`). Filter with `grep -E " error|Built target"`.
- The standalone project still enables Fortran (the `oprule/CMakeLists.txt` does), so `ifx` must be loadable.
- In the full build, add `-DOPRULE_BUILD_TESTS=ON` to get the same targets.
- `oprule_dsm2_tests` compiles the real `dsm2/src/oprule_interface/*.cpp` and needs Loki's `MultiMethods.h` (default `deps/loki-0.1.7/include/loki`, override with `-DOPRULE_LOKI_DIR=...`). If Loki or those sources are missing the target is skipped with a status message.
- Test executables use header-only Boost.Test (`boost/test/included/unit_test.hpp`) so there is no library to link or `DYN_LINK` to define. That header may be included in exactly one translation unit per executable.
- Without `OPRULE_HDF5_ROOT` the HDF5 sink is not compiled and the model's default log has no sink (see `oprule_log_text`). With it, the dsm2 tests write `dsm2_oprule_log_entry_points_test.h5` in the working directory.
- The Python tools (`oprule/tools/oprule_log.py`) need `h5py`: `python -m pip install --user h5py`.

### Generated files
`op_rule.cpp`, `op_rule_tab.cpp`, `op_rule.tab.h`, `op_rule.output` are produced in the build directory. They are git-ignored (also `build*/`). `op_rule.output` lists the grammar conflicts.

---

## 2. Where things are

| What | Where |
|---|---|
| Lexer / grammar | `oprule/lib/parser/op_rule.l`, `op_rule.y` |
| Parser global state | `oprule/lib/parser/ParseSymbolManagement.cpp` (header in `oprule/oprule/parser/`) |
| Rule runtime | `oprule/lib/rule/*.cpp`, headers in `oprule/oprule/rule/` (many classes are header-only templates) |
| Expression nodes | `oprule/oprule/expression/*.h` |
| DSM2 C++ binding | `dsm2/src/oprule_interface/*.cpp` (name lookup, factories, model interfaces, resolver, entry points called from Fortran) |
| DSM2 Fortran side | `dsm2/src/common/model_interface.f90`, `gates.f90`, `gates_data.f90`, `grid_data.f90`, `hydrolib/netbnd.f90`, `hydrolib/update_network.f90`, `hydro/fourpt.f90` |
| Rule input handling | `dsm2/src/fixed/process_oprule.f90`, `process_text_hydro_input.f90`, `input_storage/src/*` |
| Real rule inputs | `/scratch/psandhu/dsm2_studies/common_input/oprule_*.inp` (outside this repo) |
| Rule log | `oprule/oprule/rule/RuleLog.h`, `oprule/lib/rule/RuleLog.cpp`, structured records in `LogTypes.h`, HDF5 sink `Hdf5LogSink.h` and `oprule/lib/hdf5/Hdf5LogSink.cpp`; device sampler `dsm2/src/oprule_interface/dsm2_device_sampler.*`; state reporting in `oprule/oprule/expression/NamedExpressionNode.h` and the `collectState`/`describe` members of the expression nodes; design: `oprule/doc/OPRULE_LOG_HDF5_PLAN.md`; reader and checks: `oprule/tools/oprule_log.py` |
| Gate state in the tide file | `common_tide.f90`, `hydrolib/tidefile.f90` (`AccumulateGateState`), `hdf_tidefile/hdf5_init.f90` (`init_gate_state_hdf5`), `hdf5_write.f90` (`write_gate_state_to_hdf5`) |
| System test scripts | `/scratch/psandhu/dsm2_oprule_system_test/` (outside the repositories): `run_system_test.sh`, `make_run.sh`, `run_hydro.sh`, `compare_runs.sh`, `check_log.py` |
| Test fixtures derived from them | `oprule/test/data/*.inp` (subsets; one test-only rule is marked) |
| Tests | `oprule/test/{smoke,core,support,data,standalone}` |

Time-loop order per step (`UpdateNetwork`): load data and copy every data source into the model, advance active rules, solve, step all rules' expressions, test triggers. `julmin` during step *n* is the time at the END of that step.

---

## 3. Behaviour that surprises people (verified by tests unless noted)

These are the facts most likely to waste time. Tests that pin them are in `oprule/test/core/CoreTests.cpp` (suites `pinned*`).

### Parser
- **Restart the flex scanner before every parse** (`op_rulerestart(NULL)`). After a failed parse the scanner stays mid-buffer and the next parse fails too. The model never hits this because a parse failure ends the run with `exit(-3)`. Test helpers must restart it.
- Parser, name lookup and rule tables are file-level globals: not reentrant, and no single reset function exists. A fixture needs `init_expression()`, `init_rule_names()`, `clear_temp_expr()`, `clear_arg_map()`, `get_lagged_vals().clear()` and a scanner restart.
- `init_parser_f_()` (DSM2) resets named expressions but not parsed rules or the global `dsm2_op_manager`, which has no clear/remove method. Several runs in one process accumulate rules. The planned DSM2 tests use their own `OperationManager` per test for this reason.
- Reserved words cannot be argument values or names, and the failure is a plain syntax error. `to` is the one most likely to bite (`name=to`). Also `min`, `dt`, `day`, `month`, `year`, `hour`, `season`, `date`, `t`, month names, and the keywords. Names that merely start with these (`tom_paine`, `minimum`) are fine.
- Model names are case sensitive. `MOCK_VAR(...)` fails with the misleading message "unexpected NAME, expecting 't'".
- **Lower-case month names evaluate to 0** (`jan`, `Mar`; `SEASON > 16may` gets month 0). The lexer accepts any case but looks the name up in an upper-case table. Real inputs use upper case.
- `SEASON == 01JAN 12:30` drops the hour (hour is always 0; the minute is right).
- An exponent needs a decimal point (`1.0e5` ok, `1e5` is a syntax error). `RAMP` accepts only minutes.
- `PARSE_ERROR` is declared and checked in `parse_rule` but never set; failure is detected only through the return code of `op_ruleparse()`.
- Lagged expressions `name(t-1)` parse, but evaluating them throws a *pointer* (`throw new std::logic_error`). Catch with `catch (std::logic_error*)`.

### Runtime
- A rule fires only on a false-to-true trigger edge. The trigger is **not** tested while the rule is active, so a rising edge during the action is missed.
- An action takes effect one step after its trigger became true (rules are tested at the end of a step, advanced at the start of the next).
- A linear ramp of 60 minutes at 15-minute steps completes on the 4th advance (`elapsed == duration` is not `< duration`).
- A static target (`gate_install`, `gate_coef`) ramps from its value at activation; a time-dependent target (everything else writable) is re-read each step and, on completion, the expression becomes its permanent data source.
- **A rule whose top-level action is a `THEN` chain crashes `OperationManager::addRule`** (`ActionChain::setActive(false)` dereferences an unset iterator, SIGSEGV). So top-level `THEN` is unusable in the model. A chain nested inside a `WHILE` group loads fine.
- **A `WHILE` of actions with different ramp lengths aborts in a Debug build** (`ActionSet::advance` re-advances a finished child, `ModelAction::advance` asserts it is active). `build_hpc5.sh` builds Debug. Equal durations (as in the real inputs) are fine.
- Chain actions are left out of a rule's action list, so a chain nested in a group never conflicts with another rule. Conflicts are always resolved by deferral (the new rule retries each step).
- `LookupNode`: levels must have one more entry than values. An argument equal to the last level reads one element past the end of the values array *(from reading; not exercised)*.
- `accumulate` adds the expression value each step without multiplying by `dt`.
- **Nothing ever calls `init()` on expression nodes**, but `PREDICT` asserts it was called: a trigger using `predict(...)` aborts in a Debug build at its first test. `PID`/`IPID` work around it inside `step()`.
- Any two actions on the **same gate device** overlap (whatever the property or direction), and gate-level `gate_install` overlaps every device of that gate. Rules on one device are therefore serialized: e.g. `mscs_close_from` then `mscs_close_to` (the second starts only after the first ramp finishes), or `glc_barrier_elev` then `glc_barrier_in`.
- A time series driving `gate_nduplicate` makes the model see a non-integer after the first step (the setter rounds, the data source does not).
- An unknown channel number is not validated (reads `chan_geom(0)`); an unknown reservoir gives an empty node for `res_stage` and an out-of-range index for `res_flow`.

### DSM2 Fortran contracts the C++ depends on *(read from source; the mock reproduces them)*
- `ext2int` returns 0, not `miss_val_i`, for an unknown channel; `channel_length(0)` then indexes out of bounds. An unknown channel in a rule is not reported cleanly.
- `ts_index` returns -1 (not `miss_val_i`) and searches all input paths, not only oprule ones.
- Gate, device and reservoir names are lower-cased by the Fortran lookups; external-flow, transfer and time-series names are compared exactly.
- `get/set_device_flow_coef` call `exit(3)` for any direction other than `to_node` / `from_node`, so `gate_coef(..., direction=both)` ends the run. `get_device_op_coef` for `to_from_node` averages the from-node value with itself.
- `set_gate_install` frees the gate only for exactly `0.0`.
- Time-series data sources: the C++ passes a `const bool&` while Fortran declares a 4-byte `logical`, and `set_device_nduplicate_datasource` declares its flag by value. Suspected ABI mismatch, not reproducible with a mock.

---

## 4. Test infrastructure

| Piece | Purpose |
|---|---|
| `support/MockModel.{h,cpp}` | Named-double mock model; `VarInterface` (writable, static or time dependent), read-only variable, `MockLookup` (`mock_var`, `mock_tvar`, `mock_ro`), `SameVarResolver`, `FlagTrigger`, `RecordingTimeFactory`, `RuntimeFixture`, `ParserFixture`, `run_in_child`. |
| `support/InpReader.{h,cpp}` | Reads the OPERATING_RULE / OPRULE_EXPRESSION / OPRULE_TIME_SERIES tables the way `input_storage` does and returns statements in model parse order (expressions first, rules next, each sorted by name). |
| `support/Dsm2FortranMock.{h,cpp}`, `Dsm2Harness.{h,cpp}` | A C++ stand-in for the Fortran model (every `extern "C"` routine the binding calls, each commented with the Fortran routine it mirrors and its quirks) and a harness that drives the real binding in time-loop order (`Harness::step` = `UpdateNetwork`). Use `Sim` (in the DSM2 test file) to feed named time series. |
| `support/LogCapture.h` | Header-only helpers for log tests: `LogGuard` (string-stream sink, fixed time label, level; restores defaults), `parse_log`, `sequence`, `count_of`, `event_level`. |
| `data/*.inp` | Representative subsets of the real study inputs. |

Conventions:

- Test names say what they check. Tests that pin a believed defect live in `pinned*` suites and carry a comment naming the reference/plan entry. When one fails after a change, decide whether the defect was fixed and update it deliberately.
- Characterization output uses the prefix `[CHAR]` on stdout.
- Crashes and `exit()` paths are tested in a forked child (`run_in_child`). It resets signal handlers to default first, otherwise Boost.Test's handlers resume the whole test run inside the child (symptom: repeated "failures detected" output and exit status 201).
- Time step in runtime tests is `DT = 900` s. "N advances" means N calls to `advanceActions` after the activation step.

---

## 5. Tool and workflow pitfalls

- `create_file` refuses to overwrite an existing file; edit it instead. Do not delete files without asking the user.
- When editing a file the user may also have changed, re-read it first.
- A multi-replace call with a stray character before a string value is rejected with "must be array"; check the JSON.
- Boost.Test `BOOST_CHECK_CLOSE` takes a percentage; use `BOOST_CHECK_SMALL` for expected zeros and `BOOST_CHECK_CLOSE_FRACTION` for fractions.
- Do not commit or push unless asked. Do not run `sleep` to wait on builds.
- Waiting for a background job: block on its process id (`tail --pid=<pid> -f /dev/null`), not `sleep`. A shell `wait` with no arguments also waits for every other background job started from that terminal (it blocked for the 10-year runs); start parallel runs from a script and wait inside it.
- Long runs: the historical study takes about four minutes for four months in a Debug build (about 2 hours for ten years) and writes about 12 GB of tide file for ten years; delete run directories only with the owner's agreement.
- Search tool: includePattern globs for this multi-root workspace work best as absolute paths (`/scratch/psandhu/dsm2/dsm2/src/**`); relative `dsm2/src/**` returned nothing.

---

## 6. State of the work (update as it changes)

Done and passing (4 programs; coverage map in [OPRULE_TESTS_OVERVIEW.md](OPRULE_TESTS_OVERVIEW.md)):

- `oprule_smoke_tests` (19) and `oprule_core_tests` (100): parser, expression nodes, THEN/WHILE structure by timing, activation / ramp / static-vs-time-dependent / conflict behaviour, alternative conflict policies, the rule log (text and structured: stages, intervals, episodes, write notes), pinned defects.
- `oprule_dsm2_tests` (92): the real DSM2 binding against the mock Fortran: mock contract, name registry, factory arguments, interfaces, read-only nodes, time nodes, resolver overlap matrix, the study input files (fixtures and, optionally, the complete real files), end-to-end behaviour of the study rule patterns, features the studies do not use, Fortran entry points, log set-up, interface descriptions, the state in the log records, and the device sampler (`device_sampler`, 12 cases).
- `oprule_hdf5_tests` (11, needs `OPRULE_HDF5_ROOT`): the HDF5 sink read back with the HDF5 C API, equality with the in-memory sink, device intervals, exit and kill in a child process.
- `test_model_interface` (Fortran, 21 tests): the real `model_interface.f90` routines, including the log options and the gate accessors. The first 16 passed on the first run, which confirmed the mock's time assumptions (01JAN1900 00:00 = minute 1440, 0-based day of year), the name-lookup case rules and D-04. It found one mock error, now fixed: external and transfer flows are `real*4` in the model, so the mock stores them as `float`.
- Logging is implemented, with state reporting (design, format and a worked example: OPRULE_REFERENCE.md B10), and since 2026-10-01 an HDF5 log with gate and device transitions plus gate state series in the tide file (OPRULE_REFERENCE.md B10.1, OPRULE_LOG_HDF5_PLAN.md section 11).
- **System test with the real model** passes (OPRULE_TEST_PLAN.md section 13): `/scratch/psandhu/dsm2_oprule_system_test/run_system_test.sh --clean` (seven runs).

Not done:

- The dense input trace and a timeline plot of the HDF5 log (`oprule_log_trace_interval` is read and ignored).
- A reader that refuses an unknown `format_version`.
- The system test enhancements ST-F1 to ST-F10 (OPRULE_TEST_PLAN.md section 13), in particular moving the scripts into the repository.
- Fortran routines outside `model_interface.f90` that the binding depends on (`ext2int`, `CompPointAtDist`, `get_surf_elev`, `process_*`, `get_inp_data`, `store_values`) are still only mirrored from source.
- `gate_coef` with `direction=both` (D-13) cannot be tested in the Fortran test (it calls `exit(3)`).
- Fixing any of the pinned defects.

### Building and running the Fortran test and the hydro model

Separate full-build directory (do not use `build/`, and do not run `build_hpc5.sh`):

```bash
module purge && module load intel/2024.0 cmake/3.28.3
mkdir -p /scratch/psandhu/dsm2_full_build && cd /scratch/psandhu/dsm2_full_build
cmake -C /scratch/psandhu/dsm2/linux_options.cmake -S /scratch/psandhu/dsm2 -B . -G "Unix Makefiles" \
  -DCMAKE_BUILD_TYPE:STRING=Debug -DFETCHCONTENT_FULLY_DISCONNECTED=ON \
  -DFETCHCONTENT_SOURCE_DIR_FORTRAN_STDLIB=/scratch/psandhu/dsm2/build/_deps/fortran_stdlib-src \
  -DFETCHCONTENT_SOURCE_DIR_TEST-DRIVE=/scratch/psandhu/dsm2/build/_deps/test-drive-src \
  -DFETCHCONTENT_SOURCE_DIR_FYPP=/scratch/psandhu/dsm2/build/_deps/fypp-src
cmake --build . --target test_model_interface -j 8      # the first build compiles fortran_stdlib and takes a long time
./dsm2/tests/model_interface/test_model_interface
cmake --build . --target hydro -j 16                    # the executable the system test runs: dsm2/src/hydro_driver/hydro
```

The stdlib and test-drive sources come from the existing `build/_deps` so no network is needed. If `build/` is ever recreated, fetch them again or point these variables elsewhere.

### Logging: decisions (2026-09-30), implemented

Full design table, record formats and a worked example: OPRULE_REFERENCE.md B10. Acceptance tests: OPRULE_TEST_PLAN.md section 9. System test: section 13 of the same file.

Decisions, in the order they were made:

- Dedicated oprule log file (`oprule_log.txt`); level from an oprule scalar with `print_level` as fallback.
- The logger must never re-evaluate triggers or stateful nodes. `OperatingRule` keeps the value of the latest trigger test and how it changed; the manager logs from that.
- **Only changes are logged.** The first version had a level 3 with a record per rule per step: 212 MB for four months of the historical study. It was removed; levels are now 0, 1 (events) and 2 (adds `ACTION` per advance), and the same four months are 0.43 MB and 0.65 MB.
- Each trigger change carries its inputs (`trigger_inputs=[...]`): model variables, the value of every named expression, and the internal state of stateful nodes. Implemented with `ExpressionNode::collectState`; leaves are labelled by `describe()`; references to named expressions are wrapped in `NamedExpressionNode` by the lexer so their values can be shown; repeated items are listed once.
- `ACTIVATED` and `ACTION` carry the action's state: static snapshot or live start value, duration, elapsed, fraction, base, target, value and what the target reads.
- Rule text is logged at load.
- An HDF5 form of the same log is planned for the next step (OPRULE_LOG_HDF5_PLAN.md).
- Level 0 must stay the default so existing output does not change.

Order of work (all done): `RuleLog` and the stored trigger value; manager events; `ModelInterface::describe()` and `ACTION`; core tests with a string-stream sink; DSM2 binding (load records, time source, scalar, labels) and its tests; state reporting and trigger-change logging; system test.

Things to remember when changing this code:

- `collectState` may call `eval()` only on labelled leaves and on `NamedExpressionNode`; a new stateful node must report its fields and ask its children, and must not call `step()` or anything that changes state.
- A new model variable node should override `describe()` or it will be invisible in the log.
- The lexer (`op_rule.l`) wraps named-expression references; parser tests that compare node values still pass because the wrapper delegates.
- Test fixtures reset `g_vars` when constructed: set variables after creating a `LogBench`/`RuleBench`.
- `name=t` is a reserved word in the rule language (use another name in tests).

### HDF5 log and gate state: decisions (2026-10-01), implemented

Full record: OPRULE_LOG_HDF5_PLAN.md (sections 0, 10, 11). Things to remember when changing the code:

- `RuleLog` is the one place that numbers events and follows stages, intervals and episodes; call sites pass structured data (`RuleLog::event`, `action`, `sourceAttached`), and text is made from it by the text sink. Do not build log strings at call sites.
- A new writable gate property: give its `ModelInterface` a `deviceProperties()` override and add the property to `get_device_property` / `get_device_source` (Fortran), the mock and `DeviceSampler`; add a `DeviceProperty` value.
- The device sampler runs at the end of `advanceopruleactions_`: after data sources refreshed the properties and the rules wrote theirs, before the solve. It only reads.
- An `atexit` hook registered with every sink calls `RuleLog::finish()`; it is registered each time a sink is added so that it runs before HDF5 shuts down (handlers run last in first out; a hook registered before the HDF5 library started ran after it and every write failed). `finish()` is idempotent. The model also calls it explicitly before it closes the tide file.
- `Hdf5LogSink` turns itself off on the first failed HDF5 call and never stops the run. It silences the HDF5 error stack only when it creates the file.
- The scalar reader stores values in 32 characters, so file name options must be short.
- The tide file gate state uses the same `output_inst` switch as `inst device flow`. Padded `(gate, MAX_DEV, time)` layout; unused device slots hold -901. The existing chunk size of `inst device flow` takes its time chunk from `MAX_DEV` (looks like a slip, left alone); the new datasets chunk on time.
- `h5py` is not installed on the host by default: `python -m pip install --user h5py`.

### Fortran-level test of `model_interface.f90`: decisions (2026-09-30), implemented

- Add `dsm2/tests/model_interface/test_model_interface.f90` using `test-drive`, registered like `test_hydro` (`addtest(model_interface)` in `dsm2/tests/CMakeLists.txt`). Goal: assert the real Fortran behaviour that `support/Dsm2FortranMock.cpp` assumes (name lookup case rules, `ext2int` returns 0, `get_device_op_coef` for `to_from_node`, `exit(3)` for `gate_coef` with `both` (needs care in a test-drive run), `nduplicate` rounding, `ts_index` search over all paths, `qext_index` / `transfer_index` case handling, time functions, `set_*_datasource` `timedep` handling).
- Build in a **separate** full-build directory (for example `/scratch/psandhu/dsm2_full_build`), configured like the existing `build/` (HDF5 at `deps/hdf5-1.14.2/cmake`, Intel 2024.0) and reusing the sources already fetched under `build/_deps` (`-DFETCHCONTENT_SOURCE_DIR_FORTRAN_STDLIB=...`, `-DFETCHCONTENT_SOURCE_DIR_TEST-DRIVE=...`). Leave `build/` untouched (`build_hpc5.sh` deletes it; do not run that script).
- Any mismatch found between the real Fortran and the mock is fixed in the mock (and its `FORTRAN QUIRK` / `ASSUMPTION` comments) and noted in OPRULE_TESTS_OVERVIEW.md.
- Do not search the whole filesystem (`find /`) for dependencies; it is far too slow on the shared host.
