# oprule / DSM2: developer notes and gotchas

Notes for anyone (or any assistant) picking up work on the operating-rule code. They record what was learned the hard way so it is not rediscovered. Facts were checked against the source or by running the tests unless marked *(unverified)*.

Companion documents in this directory:

- [OPRULE_REFERENCE.md](OPRULE_REFERENCE.md): how rules work end to end, plus suspected defects (B9).
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
/scratch/psandhu/dsm2_oprule_build/oprule/test/oprule_core_tests      # 64 test cases
/scratch/psandhu/dsm2_oprule_build/oprule/test/oprule_dsm2_tests      # 70 test cases
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
- Search tool: includePattern globs for this multi-root workspace work best as absolute paths (`/scratch/psandhu/dsm2/dsm2/src/**`); relative `dsm2/src/**` returned nothing.

---

## 6. State of the work (update as it changes)

Done and passing (3 programs, 153 test cases; coverage map in [OPRULE_TESTS_OVERVIEW.md](OPRULE_TESTS_OVERVIEW.md)):

- `oprule_smoke_tests` (19) and `oprule_core_tests` (64): parser, expression nodes, THEN/WHILE structure by timing, activation / ramp / static-vs-time-dependent / conflict behaviour, alternative conflict policies, pinned defects.
- `oprule_dsm2_tests` (70): the real DSM2 binding against the mock Fortran: mock contract, name registry, factory arguments, interfaces, read-only nodes, time nodes, resolver overlap matrix, the study input files (fixtures and, optionally, the complete real files), end-to-end behaviour of the study rule patterns, features the studies do not use, Fortran entry points.

Not done:

- A Fortran-level test (`test-drive`) of `model_interface.f90` to check the mock's contract against the real routines.
- Update `OPRULE_TEST_PLAN.md` observed results for every new test (partly done) and keep B9 in `OPRULE_REFERENCE.md` in step with the pinned defects.
- Logging for rule trigger/activation/evaluation (see reference B10).
- Fixing any of the pinned defects.
