# DSM2 Operating Rules: Test Plan

Sister document to [OPRULE_REFERENCE.md](OPRULE_REFERENCE.md). It turns every open question, suspected defect and gotcha in that document into a concrete test, and lists the tests needed before adding new features (logging in particular).

Nothing here has been run. All "current behaviour" expectations are predictions from reading the source, marked `?` where the prediction is uncertain. The first job of each test is to find out what actually happens.

---

## 0. How to use this document

### Test kinds

| Kind | Meaning | What to assert |
|---|---|---|
| **C** | Characterize | Run it, record what happens in the *Observed* column. The test only pins the behaviour; it answers a question. |
| **R** | Requirement / regression | Assert the intended behaviour. Should pass on the current code. |
| **X** | Expected failure | Assert the *intended* behaviour of a suspected defect. Mark as expected-fail (xfail) until the defect is fixed, then promote to **R**. |

Workflow for a suspected defect: write the **C** test, record the result, then either drop it (not a defect), keep it as **R** (intended), or add the **X** test and file the fix.

### Layers

| Layer | Where it runs | Needs |
|---|---|---|
| **U1** | `oprule` C++ unit tests (Boost.Test), mock model and mock time factory | `oprule`, `oprule_parser`; no DSM2 model |
| **U2** | DSM2 C++ binding tests (`oprule_interface_cpp`) with stub definitions of the Fortran `extern "C"` functions | stub library (S1 below) |
| **F** | Fortran tests (test-drive) for `model_interface.f90` and the data-source code | minimal `grid_data` / `gates_data` setup |
| **I** | Integration: real `hydro` run on a tiny network, inspect outputs and logs | fixture network (I1 below) |
| **S** | Static / script checks over source and input files | Python or shell |

### Priority

**P0** blocks the logging work or guards data correctness. **P1** should exist before the next feature. **P2** is nice to have.

### ID scheme

`<area>-<nn>`. Areas: `LEX` lexer, `GRM` grammar, `PSM` parse state, `RUN` activation and runtime, `TRN` actions and transitions, `CFL` conflicts, `DS` data sources, `LK` name lookup, `TIM` time nodes, `TS` time series, `FOR` Fortran routines, `ABI` C/Fortran interface, `INP` input reading, `CASE` case sensitivity, `CORP` real input corpus, `INT` integration, `LOG` logging feature, `STAT` static checks.

---

## 1. Test infrastructure needed

Existing state (from reading the repo):

- `oprule/test/parser` and `oprule/test/rule` hold Boost.Test suites and fixtures (`ParserTestFixture.h` has `TestModelInterface`, `TestModelInterface2/3`, a test lookup and time factory). The CMake blocks that build them are commented out in `oprule/CMakeLists.txt` and reference old target names (`OperatingRule`, `OpRuleParser`). Whether they still compile or pass is unknown.
- `dsm2/tests` uses Fortran **test-drive** through the `ADDTEST(name)` macro (needs `test_<name>.f90`). The only test, `hydro`, calls `prepare_hydro()`.
- `dsm2_tests/tests/*` is a Python (nose) integration harness with `hydro.inp`/`channel.inp` fixtures. Whether it still runs was not checked.

| ID | Item | Notes |
|---|---|---|
| **S0** | Re-enable the `oprule` unit tests | Update the commented CMake blocks to the current targets (`oprule`, `oprule_parser`), add Boost unit-test-framework, get the existing suites to compile and pass, record which fail. Do this first: it is the baseline for all U1 tests. |
| **S1** | Fortran stub library for U2 | C++ file defining every `extern "C"` function declared in `dsm2_interface_fortran.h` and `dsm2_time_interface_fortran.h`, plus `value_from_inputpath` and `fetch_data` if needed. Stubs record their arguments and return configurable values. Lets U2 tests link without the model. |
| **S2** | Parser state reset helper | Calls `init_expression()`, `init_rule_names()`, `clear_temp_expr()`, `clear_arg_map()`, `clear_array_vectors()`, `clear_string_list()`, `get_lagged_vals().clear()`, and restarts the flex scanner (`op_rulerestart`). There is no single reset API today; see PSM-01..03. |
| **S3** | `OperationManager` reset | The DSM2 binding uses a file-level `dsm2_op_manager` with no clear/remove-rule method. U2/F tests that parse more than once per process need a reset hook (see PSM-03). |
| **S4** | Recording mock time factory | A `ModelTimeNodeFactory` that records the arguments of each call (for GRM/TIM tests such as `getReferenceSeasonNode(mon, day, hour, min)`). |
| **S5** | Subprocess test support | Some code paths call `exit()` (`process_oprule`, `get/set_device_flow_coef`). Run those in a child process and assert exit code and stderr (CTest `PASS_REGULAR_EXPRESSION` / `WILL_FAIL`, or a small driver). |
| **I1** | Integration fixture | A minimal channel network with: two external flows, one gate with two devices, one reservoir, one transfer, and a short list of `OPRULE_TIME_SERIES` entries (constant and DSS). Start from `dsm2_tests/tests/test_boundary_conditions` if it still runs. |
| **I2** | Golden-output harness | Runs `hydro` on a fixture or a real study twice and compares results bitwise (HDF5/DSS output, text output). Needed for LOG-* and for fixes that must not change results. |
| **S6** | Corpus lint script | See CORP-00 and STAT-*. |

---

## 2. Open questions, each as a test

### 2.1 Case sensitivity (you answered "not sure")

| ID | L | K | P | Check | Predicted current behaviour (?) | Observed |
|---|---|---|---|---|---|---|
| CASE-01 | U1 | C | P0 | Model name in three cases in a trigger: `chan_stage(...)`, `CHAN_STAGE(...)`, `Chan_Stage(...)` | Lower case works. Upper/mixed become a plain `NAME` (exact-match lookup) and fail to parse. | **Confirmed (smoke).** `MOCK_VAR(name=x)` fails with the misleading message "syntax error, unexpected NAME, expecting 't'" (the grammar tries the lagged form `NAME(t-N)`). |
| CASE-02 | U1 | C | P0 | Constants `OPEN open Open`, `INSTALL install`, `CLOSE`, `REMOVE` as `TO` targets | Registered in upper case only; lower/mixed fail. | — |
| CASE-03 | U1 | R | P1 | Keywords in any case: `set SET Set`, `to`, `when`, `while`, `then`, `ramp 60min`, `true True TRUE`, `and And`, `datetime`, `season`, `ifelse` | All accepted (lexer is case-insensitive). | Partly confirmed (smoke): `set ... to ... when true` accepted. |
| CASE-04 | U2/F | C | P0 | Argument values in mixed/upper case: `gate=FalseBarrier`, `device=Pipes`, `res=`, `direction=To_Node`, `dist=LENGTH` | Gate/device/reservoir are lower-cased by the Fortran lookups and work. `direction` is compared exactly and fails. `dist=LENGTH` works (`lowerCase` helper). | — |
| CASE-05 | U2/F | C | P0 | Mixed-case `ext_flow(name=...)`, `transfer_flow(transfer=...)`, `ts(name=...)` where the stored name is lower case | `qext_index`, `transfer_index`, `ts_index` do no case folding, so mixed case is rejected. Check what case the stored names actually have. | — |
| CASE-06 | I | C | P0 | Expression defined as `Sjr_Flow`, referenced in a trigger as `sjr_flow` and as `Sjr_Flow` | Definition name is lower-cased by `process_oprule_expression`; the reference is not lower-cased, so only the lower-case reference resolves. | — |
| CASE-07 | I | C | P1 | Two rules whose names differ only in case (`Abc`, `abc`) in one layer | Both survive the reader (different identifiers), then collapse after `locase`; second is rejected with "Operating rule name ... used more than once". | — |

### 2.2 Activation semantics

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| RUN-01 | U1 | R | P0 | `WHEN TRUE`, abrupt action, call `stepExpressions; manageActivation; advanceActions` repeatedly | Activates on the first `manageActivation`, first advance writes the value, rule completes in that advance, never re-fires. | **Confirmed** (mock model). |
| RUN-02 | U1 | R | P0 | Same with `STARTUP` | Identical to `TRUE` (lexer maps both to `TRUE`). | — |
| RUN-03 | U1 | R | P0 | Trigger `false, true, true, false, true` over five steps | Fires at steps 2 and 5 only (edge triggered). | **Confirmed** (false, true, still-true, false, true sequence). |
| RUN-04 | U1 | R | P0 | Timing: trigger becomes true during step *n* | First value written during step *n+1* (advance happens before the solve of the next step); never in step *n*. Use the real order: `advance -> [solve] -> step -> manageActivation`. | **Confirmed** (mock model). |
| RUN-05 | I | C | P0 | `DATETIME >= <t0>` trigger, abrupt `ext_flow` change. Find the first output step with the new value | Step after the first step whose end time is at or after `t0`. | — |
| RUN-06 | U1 | C | P0 | **Missed edge while active.** Rule active; trigger goes false then true while it is running | `testNewlyTriggered` is not called while active, so `_prevTriggerValue` is stale `true`. A rising edge that occurs during the active period (or on the exact step the rule completes) is missed. | — |
| RUN-07 | U1 | R | P1 | After completion with trigger still true, then trigger false for one step, then true | Fires again. | — |
| RUN-08 | U1 | C | P1 | State-based trigger true at simulation start (as in `DATETIME >= 20SEP2018 12:00` when starting later) with `RAMP 120MIN` | Fires after the first step, ramping from the initial value for 120 minutes. | — |
| RUN-09 | U1 | C | P1 | Rule with a stateful trigger (`accumulate`) while inactive | `stepExpressions` steps every rule, active or not, so the accumulator advances each step. | — |
| RUN-10 | U1 | C | P1 | Order within a step: trigger expression sees state after `stepExpressions` of the same step | Confirm by `accumulate` count at trigger time. | — |

### 2.3 `THEN` / `WHILE` structure

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| GRM-01 | U1 | R | P0 | `A(60) WHILE B(30) THEN C(30)` (durations in minutes) | Parsed as `A WHILE (B THEN C)`: B done at 30, C done at 60, rule done at 60. The alternative `(A WHILE B) THEN C` would finish at 90. | — |
| GRM-02 | U1 | R | P1 | `A THEN B WHILE C` | `(A THEN B) WHILE C` (THEN binds tighter). | — |
| GRM-03 | U1 | R | P1 | Parenthesized `(A WHILE B) THEN C` | C starts only after both A and B complete. | — |
| TRN-05 | U1 | R | P0 | Chain of three abrupt actions | All complete within a single `advanceActions` call (each child is activated and advanced by 0 on `childComplete`); values set in order. | — |
| TRN-06 | U1 | R | P1 | Set whose children have different ramp lengths | Completes when the last child completes. | — |

### 2.4 Conflict handling (you answered "not sure, document as-is")

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| CFL-01 | U1 | R | P0 | Rule A (ramp) active; rule B overlaps A and its trigger goes true | B is not activated; B's trigger memory is reset so it is retried each step while the trigger stays true; B activates at the first `manageActivation` after A becomes inactive. | **Confirmed** (mock model). |
| CFL-02 | U1 | R | P0 | A and B overlap, both trigger in the same step | The one earlier in the pool wins (it is activated immediately inside the loop); the other is deferred. Verify it follows insertion order. | **Confirmed** (first added wins; unrelated rule activates). |
| CFL-03 | U1 | R | P1 | A and B do not overlap | Both activate in the same step. | — |
| CFL-04 | U1 | C | P1 | `checkActionPriority` return values over many scenarios | Only `DEFER_NEW_RULE` or `RULES_COMPATIBLE` ever returned. | — |
| CFL-05 | U1 | X | P0 | **THEN chain bypass.** Rule `SET a TO 1 THEN SET b TO 2` and active leaf rule `SET a TO 3` | Today: `chainRule.getActionList().size() == 0`, so no conflict is detected (C). Intended: conflict, B deferred (X). | **Confirmed** (smoke): chain list size 0; `ActionSet` list size 2. |
| CFL-06 | U1 | C | P1 | `ActionSet` containing an `ActionChain` | Chain's leaf actions are missing from the flattened list (base `appendSubActionsToList` stub). | — |
| CFL-07 | U2 | C | P0 | Resolver matrix: for every pair in `{ext_flow(i), ext_flow(j), transfer(i), gate_install(g), gate_op(g,d,dir), gate_height(g,d), gate_elev(g,d), gate_width, gate_nduplicate, gate_coef}` | Record the overlap table; expected per reference B6: same ext flow, same transfer, same gate install, install vs any device of the same gate, same device (any property or direction). Test both argument orders (symmetry). | — |
| CFL-08 | U2 | C | P0 | `gate_elev` (device d) vs `gate_op` (device d) | Overlap, even though they are different properties. | — |
| CFL-09 | U2 | C | P1 | `gate_coef` (`DeviceFlowCoefInterface`) vs other device interfaces | Confirms the Loki dispatch matches subclasses through `DeviceInterface`. | — |
| CFL-10 | U2 | C | P1 | `ExternalFlowInterface` vs `DeviceInterface` | Mixed pair: returns false with no message. A pair outside the typelist prints "Default: assume no conflict ..." (`OnError`). | — |
| CFL-11 | U1 | R | P2 | Future feature: custom resolver returning `REPLACE_OLD_RULE` and `IGNORE_NEW_RULE` | Replace deactivates the old rule and activates the new one; ignore leaves the old rule and does not re-arm the new trigger. The code paths exist and have never been exercised. | — |
| CFL-12 | I | C | P1 | Real pair `glc_install_in`/`glc_install_out` (same gate, abrupt) | They never overlap in practice (abrupt actions complete in one advance). Confirm no deferral occurs. | — |

### 2.5 Time-dependent targets and data sources (you answered "not sure")

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| DS-01 | U2 | R | P0 | `SET ext_flow(name=q) TO ts(name=x)`; run to completion | `set_external_flow_datasource` called once with `timedep = true`; `register_express_for_data_source` returned index stored. | — |
| DS-02 | U2 | R | P0 | `SET ext_flow(...) TO 0.0` (static expression) | Datasource called with `timedep = false` and value 0.0 (constant source). | — |
| DS-03 | F | R | P0 | `fetch_data` for each source type | `const_data` -> value; `dss_data` -> `pathinput(ptr)%value`; `expression_data` -> `get_expression_data(ptr)`; unknown -> `miss_val_r`. | — |
| DS-04 | I | C | P0 | Override sequence: rule 1 sets `q` to `ts`, rule 2 later sets `q` to 0.0, rule 1 fires again | Final source is `ts`; each completion replaces the previous source. The original DSS input for `q` is never restored. | — |
| DS-05 | I | C | P1 | `ext_flow` with a DSS input and a `RAMP 60MIN` rule | Ramp base is re-read each step (value loaded from the DSS data), so the ramp tracks a moving baseline: `v = base(t)*(1-f) + target*f`. | — |
| DS-06 | U1 | R | P0 | Static interface (`isTimeDependent == false`): base is the value at activation time, not at the first advance | Change the model value between `manageActivation` and the first `advanceActions`; ramp uses the activation-time value. | — |
| DS-07 | U2 | C | P1 | `register_express_for_data_source` with the same pointer twice / equal but distinct expressions | Same pointer -> same index; distinct nodes -> new entries; entries are never removed. | — |
| DS-08 | I | R | P0 | `gate_coef` (static) with `TO <time-varying expr> WHEN TRUE` | Applied once at activation; later model values do not follow the expression (reference A5.6). Also covers real rules `clfct_gate_cf_*`. | — |
| DS-09 | U2 | C | P1 | `gate_install`: `set_gate_install(0.0)` frees the gate; any non-zero installs; `INSTALL` = 1, `REMOVE` = 0; value 0.5 | 0.5 treated as install (only exactly 0.0 frees). | — |

### 2.6 Input reading and ordering

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| INP-01 | I | C | P0 | Pool order: record the order in which rules reach `parse_rule` for names `b_x`, `A_x`, `a_x`, `B_x` | Sorted lexicographically by the stored name, so upper case sorts before lower case (ASCII). Confirm whether the identifier is lower-cased before sorting. | — |
| INP-02 | I | R | P0 | Layers: same `NAME` in two layers | Higher layer wins; same name twice in one layer is a fatal error. | — |
| INP-03 | I | R | P1 | `^` prefix on a data line | Row is ignored (real file has `^vamp_8500_remove`). | — |
| INP-04 | I | C | P0 | Expression referencing an expression that sorts later | Unknown name at parse time (falls back to `NAME`), leading to a parse error. | — |
| INP-05 | I | C | P0 | Combined `name + action + trigger` over 1024 characters | Fortran internal write into `character*1024`; expect a runtime error or truncation. Record which. | — |
| INP-06 | I | C | P1 | Field length limits: name 33 chars; action or trigger 513 chars | Truncated or rejected by the reader. Record which. | — |
| INP-07 | I | C | P1 | `${VAR}` inside a rule/expression text; unset variable | Environment substitution is applied to the whole line; unset behaviour to be recorded. | — |
| INP-08 | I | C | P1 | Quoted field with embedded commas and spaces, and a trailing `;` in the text | Commas/spaces preserved inside quotes. A trailing `;` yields `;;` in the assembled text; record whether it parses. | — |
| INP-09 | I | R | P1 | Parse failure | Output contains the rule text; exit code is -3 (253). | — |
| INP-10 | I | C | P2 | Rule name equal to an existing expression name | Lexer returns the name as a named value, so the statement becomes an error or reassignment path. Record whether it is fatal, silent, or drops the rule. | — |

### 2.7 Unclear semantics of stateful nodes

| ID | L | K | P | Check | Observed |
|---|---|---|---|---|---|
| NODE-01 | U1 | C | P1 | `accumulate(x, 0)` with `x = 1` over 5 steps of `dt = 900`: value after each step (is it 5 or 4500?). Reset case: on the reset step is the expression added after the reset? | — |
| NODE-02 | U1 | C | P1 | `accumulate(x, init)` with a model-dependent `init`: is `init` evaluated once at parse time (constructor) or at reset only? | — |
| NODE-03 | U1 | C | P1 | `predict(x, linear, 30MIN)` and `quad`: result for a ramp input, sensitivity to `dt` (source notes "needs real dt" for quad) | — |
| NODE-04 | U1 | C | P1 | `pid(...)`: evaluate before the first `step()` (asserts?), constants evaluated at parse time, response to a step input; repeat for `ipid` | — |
| NODE-05 | U1 | C | P2 | `lookup(x, [..], [..])`: below range, above range, exact knots, non-monotonic x, mismatched array lengths | — |
| NODE-06 | U1 | C | P2 | Multiple `lookup` expressions in one rule: array vectors are retained per call (`add_array_vector` grows forever) | — |

---

## 3. Suspected defects (reference B9)

For each: the **C** test first, then the **X** test if confirmed.

| ID | Ref | L | P | C test (confirm) | X test (intended behaviour) |
|---|---|---|---|---|---|
| D-01 | B9.1 | U1 (S4) | P1 | Parse `SEASON == 01JAN 12:30;` with a recording time factory. Record `(mon, day, hour, min)` passed. Predicted hour = 0. Also `01JAN 00:05`, `01JAN 23:59`, `01JAN 10:00`. | `hour == 12, min == 30`. |
| D-02 | B9.2 | U2 | P1 | `ChannelFlowNode(chan, 100.5)` then `copy()`; observe the `distance` passed to the `chan_comp_point` stub for each construction. Repeat for `ChannelWSNode`; `ChannelVelocityNode` as control. Also `dist=length` with a non-integer channel length. | Copy uses `100.5`. |
| D-03 | B9.3 | U2 | P2 | `DeviceOpInterface(1,2,dir) == DeviceOpInterface(1,2,dir)` and `(1,1,dir) == (1,2,dir)` for each device subclass and `DeviceInterface`. Predicted: first false, second true. | First true, second false. |
| D-04 | B9.4 | F | P1 | Gate with `opCoefToNode = 0.2`, `opCoefFromNode = 0.6`; `get_device_op_coef(..., to_from)`. Predicted 0.6. | 0.4. Also check the effect on a `RAMP` with `direction=to_from_node`. |
| D-05 | B9.5 | U2 (S1) | P0 | Call every `set_*_datasource` through the C++ declarations with `timedep` true and false; read back `source_type` in the real Fortran. Repeat with the bool variable at several addresses (stack depths, heap). `set_device_nduplicate_datasource` (by-value in Fortran) is the suspect. Also the 4-byte `logical` read of a 1-byte `bool` for the others. | `source_type` tracks `timedep` exactly, for every address. |
| D-06 | B9.6 | I | P1 | Same as DS-08. | (Design decision) either document as intended or add `setDataExpression` to `DeviceFlowCoefInterface`. |
| D-07 | B9.7 | U1 | P0 | Same as CFL-05. | Conflict detected for chains. |
| D-08 | B9.8 | U2 | P2 | Same as CFL-08. | (Design decision) per-property overlap. |
| D-09 | B9.9 | U1 | P2 | See PSM-01..07. | Reentrant or at least resettable parser. |
| D-10 | B9.10 | U2 | P2 | Direction keywords: `gate_op` accepts `to_node from_node to_from_node bidir`, rejects `both`; `gate_coef` accepts `to_node from_node both`, rejects `to_from_node bidir`. | (Design decision) align keywords. |
| D-11 | B9.11 | U1 | P0 | `name(t-1)` parse succeeds; `eval()`, `step()`, `copy()` each throw a *pointer* (`catch(std::exception&)` does not catch it; use `catch(std::logic_error*)`). Include the existing `testParseLagged`. **Confirmed (smoke):** parse succeeds, `eval()` throws `std::logic_error*`. | Either implemented (value from the previous step), or rejected cleanly at parse time with a clear error. |
| D-12 | B9.11 | U1 | P1 | `apply_lagged_values` after defining two names with lagged references in both; inspect which expression each lagged node holds. `laggedvals` growth across parses. | Each lagged node bound to its own named expression only; list cleared per definition. |
| D-13 | B9.12 | F + S5 | P0 | `get_device_flow_coef` / `set_device_flow_coef` with `direct_to_from_node()` runs in a subprocess; also `SET gate_coef(..., direction=both) TO x WHEN TRUE` end to end. Predicted exit code 3 and "Flow direction not recognized". | `both` handled like `set_device_op_coef` (sets both sides), or rejected at parse time. |
| D-14 | B9.13 | U1 | P1 | See NODE-01/02. | Decide whether `accumulate` should integrate over `dt`. |
| D-15 | B9.14 | U1 | P1 | See NODE-04. | PID initialised before first evaluation. |
| D-16 | new | U1 | P0 | See RUN-06 (missed edge while active). | Edge memory updated every step, including while active. |
| D-17 | new | U1 (S5) | P0 | `OperationManager::addRule` with a chain rule. `addRule` calls `setActive(false)`, which reaches `ActionChain::setActive(false)`, which dereferences `actionIterator` before it was ever set. Predicted undefined behaviour or crash; run in a subprocess. Real inputs use no `THEN`, so this is latent. | No crash; chain rules can be added. |
| D-18 | new | U1 | P2 | `ActionSet` and `ActionChain` `_active` are uninitialised in the constructors; call `isActive()` right after construction (before `addRule`). | `false`. |
| D-19 | new | U2 | P2 | Throwing pointers: `ModelInterface::setDataExpression` default `throw new std::logic_error`. Calling it on a static interface | Exception type that can be caught as `std::exception`. |
| D-20 | new | U1 | P2 | Quick numeric checks for grammar-level surprises: `2^3^2`, `-2^2`, `NOT false AND false`, `1+2 < 4 AND 2*2==4` | Record; promote as R. |
| D-21 | new | U1 | P1 | Lower-case month names in a date/season literal (`01jan2020`, `jan`). **Confirmed (core `pinned`):** evaluate to 0. | Case-insensitive month names. |
| D-22 | new | U1 (S5) | P1 | `WHILE` of two actions with unequal `RAMP` durations. **Confirmed (core, child process):** aborts (SIGABRT) in Debug. | Either action may finish first. |
| D-23 | new | U1 (S5) | P1 | `predict(...)` in a trigger: nothing calls `init()`. **Confirmed (core, child process):** aborts (SIGABRT) at the first test. | `init()` called when the node is built or first stepped. |
| D-24 | new | U2 | P1 | Unknown channel number / reservoir name in `chan_*`, `res_stage`, `res_flow`. **Confirmed (dsm2 `factory_arguments`):** not validated (`chan_geom(0)`; empty node; `res_geom(-901)`). | Parse-time error naming the bad argument. |
| D-25 | new | U2 | P1 | Time series driving `gate_nduplicate`. **Confirmed (dsm2 `study_rule_behaviour`):** the model sees a non-integer after the first step. | Rounded like the setter, or rejected. |
| D-26 | new | U2 | P2 | Any two actions on one gate device overlap (extends D-08). **Confirmed (dsm2 `resolver_overlap`):** opposing-direction rules (`mscs_*`) and different properties (`glc_*`) are serialized. | (Design decision) per-property / per-direction overlap. |
| D-27 | new | U2 | P2 | `ts(...)` series lookup uses the first name match across all paths. **Recorded in the mock contract (dsm2 `fortran_mock_contract`, from reading the Fortran), not verified against the real model.** | (Design decision) require the path. |
| D-28 | new | U1 | P2 | `PARSE_ERROR` is never set (see 9b). **Found by reading; not asserted by a test yet (GRM-10).** | Set by the parser on failure. |

Confirmed by tests as predicted: D-01, D-02, D-03, D-04, D-10, D-11, D-13, D-14, D-16, D-17 (SIGSEGV, child process); D-07 and D-08 are covered through the conflict tests (CFL-05, CFL-08). Not testable with a mock: D-05. Not asserted: D-06, D-09, D-12, D-15, D-18, D-19, D-20 (partly), D-28.
---

## 4. Lexer and grammar checks

| ID | L | K | P | Check | Predicted | Observed |
|---|---|---|---|---|---|---|
| LEX-01 | U1 | R | P0 | Whitespace: `ts(name = x)`, spaces around operators and commas | Accepted (spaces discarded). | — |
| LEX-02 | U1 | C | P1 | Tab characters inside a rule | `[\t];` only matches a tab followed by `;`. A tab followed by anything else matches the catch-all and prints `Unmatched: ...`, but parsing continues. | — |
| LEX-03 | U1 | C | P1 | Stray characters (`$ & ! % ~`) | Dropped with an "Unmatched" message. Check that `a & b` is not silently parsed as something else. | — |
| LEX-04 | U1 | C | P1 | Numbers: `1e5`, `1.0e5`, `1.e5`, `.5`, `5.`, `1.5E-3`, `.` | Exponent only after a decimal point, so `1e5` -> `1` then name `e5`. | — |
| LEX-05 | U1 | C | P0 | Reserved words as argument values and as expression names (full list in reference A6), month names `jan..dec`, `t`, `tt`, `t1`, `tom_paine` | Reserved words fail as names; `t` fails; `tt`, `t1`, `tom_paine` are ok (longest match). | — |
| LEX-06 | U1 | C | P1 | Names with `@` and digits (`old_r@tracy_barrier`), and with `-` (`old-r`) | `@` ok; `-` splits into subtraction. | — |
| LEX-07 | U1 | C | P1 | Date literals: `01JAN2004`, `1JAN2004`, `01jan2004`, `31FEB2001`, `01JAN0999`, `01JAN1000`; times `00:00`, `24:00`, `29:00`, `2:30` | Two-digit day/month-day required; `31FEB` lexes and is validated later (record); `29:00` lexes. | — |
| LEX-08 | U1 | R | P1 | Month names as numbers: `MONTH <= APR`, `MONTH == DEC` | Numeric 1..12. | — |
| LEX-09 | U1 | C | P1 | RAMP forms: `RAMP 60MIN`, `RAMP 60 MIN`, `RAMP 0MIN`, `RAMP 0.5MIN`, `RAMP 1HOUR`, `RAMP 60` | First two ok; `0MIN` behaves abruptly (no division by zero); `1HOUR` and `60` are parse errors. | — |
| LEX-10 | U1 | C | P2 | `=` vs `==`, `<>` vs `!=`, `:` alone, `===` | `=` is the argument `DEFINE` token, so `a = b` in a trigger is a syntax error; `!=` unsupported. | — |
| LEX-11 | U1 | C | P2 | Quoted strings: `'abc def'`, `"x"`, `'1abc'`, unbalanced quote | Must start with a letter; `-`, `_`, `@`, space allowed inside. | — |
| GRM-04 | U1 | R | P1 | Model name used without required arguments (`chan_stage` alone) in an expression | Syntax error. | — |
| GRM-05 | U1 | C | P1 | Missing/empty ACTION or TRIGGER; missing `WHEN`; text already ending with `;` | Parse error or `;;` behaviour; record. | — |
| GRM-06 | U1 | C | P1 | Boolean used in a numeric context (`mscs_calc + 1`) and numeric in boolean context (`IFELSE(1, 2, 3)`) | Syntax error. | — |
| GRM-07 | U1 | C | P1 | `A := 1; A := 2;` | "reassignment of variable" message; record whether `yyparse` returns 0 and what `get_parsed_type()` is (DSM2 `parse_rule` treats only `PARSE_ERROR` as failure). | — |
| GRM-08 | U1 | R | P1 | Duplicate rule name in one process | "used more than once" and parse abort. | **Confirmed** (smoke). |
| GRM-09 | U1 | R | P1 | `lookup` with out-of-range and malformed arrays | Record; ensure no crash. | — |
| STAT-01 | S | R | P1 | Every `%token` declared in `op_rule.y` is returned by `op_rule.l` (or intentionally unused) | Currently fails for `MINDAY`, `STEP` is returned but unused by the grammar. | — |
| STAT-02 | S | C | P1 | Baseline the bison conflict report (`op_rule.output`): number of shift/reduce and reduce/reduce conflicts | Record; fail CI on an increase. | — |

### 4.1 Parser state (PSM)

| ID | L | K | P | Check | Predicted | Observed |
|---|---|---|---|---|---|---|
| PSM-01 | U1 | C | P1 | Parse a failing statement, then a valid one; size of the temp symbol vector before and after | Failure paths do not call `clear_temp_expr`, so entries accumulate until the next success; the next parse still works. | — |
| PSM-02 | U1 | C | P1 | After a syntax error in the middle of a string, parse another string | Scanner may resume mid-buffer if not restarted; record. Check `op_rulerestart`. | **Confirmed** (smoke): after a failed parse the next parse fails too (rc=1) unless `op_rulerestart(NULL)` is called first. Test helpers must restart the scanner before each parse. Not hit in the model because `parse_rule` failure exits. |
| PSM-03 | U2 | C | P0 | Call `init_parser_f` twice in one process and parse the same rule names | `init_parser_f` clears named expressions only (not `rulenames`, not `dsm2_op_manager`); duplicates fail with "used more than once" and old rules stay in the pool. Matters for running several hydro tests in one test executable. | — |
| PSM-04 | U2 | C | P1 | Parse before `init_parser_f` (lookup pointer null) | Crash or null dereference; record (low priority, `parse_rule` is only called after init). | — |
| PSM-05 | U1 | C | P2 | `lexer_init()` called multiple times | Month map refilled harmlessly. | — |
| PSM-06 | U1 | C | P2 | Unnamed rule auto-naming (`OpRuleN`) across parses | Static counter; never reset. | — |

---

## 5. Actions and transitions

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| TRN-01 | U1 | R | P0 | `dt = 900 s`, `RAMP 60MIN`, static interface, target 10, initial 0 | Values after each advance: 2.5, 5, 7.5, then 10 with completion on the **fourth** advance (`elapsed == duration` is not `< duration`). | **Confirmed** (mock model). |
| TRN-02 | U1 | R | P1 | `dt` longer than the ramp (`dt = 3600`, ramp 1800) | One advance, full target, leftover time ignored. | — |
| TRN-03 | U1 | R | P1 | Abrupt action | One advance, completes immediately; `RAMP 0MIN` equivalent. | — |
| TRN-04 | U1 | R | P1 | Target expression changes during the ramp | Re-evaluated each advance; `value = base*(1-f) + target(t)*f`. | — |
| TRN-07 | U1 | R | P1 | Re-arming: after completion, trigger false then true | Restarts from the snapshot at the new activation, not the old one. | — |
| TRN-08 | U1 | R | P1 | `ActionSet` `setActive(true/false)` only toggles children in the opposite state | No double activation. | — |
| TRN-09 | U1 | C | P2 | `SmoothStepTransition` | Not reachable from the grammar; unit-test the class only. | — |
| TRN-10 | U1 | C | P2 | `isApplicable()` for compound actions | Base returns `true`; `group_actions` asserts it on the first action. | — |

---

## 6. DSM2 binding

### 6.1 Name lookup and factories (U2)

| ID | Check | Expected | Observed |
|---|---|---|---|
| LK-01 | Every registered name (reference A4): `isModelName`, `readWriteType`, `takesArguments` | Matches the table: read-only vs read/write, arguments vs none. | — |
| LK-02 | `chan_*` without `channel`; without `dist`; `dist` negative; `dist` larger than channel length; `dist=length`; unknown channel | `MissingIdentifier` / `InvalidIdentifier` with the coded messages. Unknown channel: record (`ext2int` result). | — |
| LK-03 | `res_stage` without `res`; `res_flow` without `node`; using `connect=` instead of `node=` (registration label says `connect`); unknown node | Missing/Invalid messages; `connect=` is treated as missing `node`. | — |
| LK-04 | `gate_*` without `gate`; unknown gate; unknown device; `gate_op` without `direction`; illegal direction | Missing/Invalid messages. Device omitted for a `gate_height` type: message is "Gate or device not found". | — |
| LK-05 | `gate_install(gate=g, device=d)` (extra arg) | Extra arg ignored. | — |
| LK-06 | `ext_flow`, `transfer_flow`, `ts` with unknown names; name `ndx > 0` test vs `miss_val_i` value | `InvalidIdentifier`. Check the actual value of `miss_val_i` against the `> 0` test. | — |
| LK-07 | Direction keywords matrix (D-10) | As stated. | — |
| LK-08 | Duplicate registration | Throws `domain_error`. | — |

### 6.2 Time nodes (TIM)

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| TIM-01 | U2 (S1) | R | P1 | `YEAR MONTH DAY HOUR MIN DT DATETIME SEASON` at known `julmin` values (including midnight, end of month, end of year, leap day) | Match the Fortran definitions; `DAY` is day of month; `MIN` is minute of hour; `DT` in seconds. | — |
| TIM-02 | F | R | P1 | `get_reference_minute_of_year` vs `getModelMinuteOfYear` for the same calendar instant, in a leap and non-leap year | Equal. | — |
| TIM-03 | F | C | P2 | `29FEB` seasonal literal in a non-leap year; `31APR` | Record (`iymdjl` behaviour). | — |
| TIM-04 | U2 | R | P1 | Absolute literal `01JAN2004 00:00` vs `DATETIME` at that model time | Equal; `24:00` and `00:00` next day equivalence recorded. | — |
| TIM-05 | I | C | P1 | `SEASON > 16MAY AND SEASON < 01JUN` over a year boundary run (e.g. December to February) | Seasonal comparisons do not wrap around year end (plain minute-of-year comparison). Record how rules like `SEASON>01DEC OR SEASON<15APR` behave. | — |

### 6.3 Time series (TS)

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| TS-01 | I | C | P0 | `ts(name=x)` value seen by a trigger in step *n* | Equal to the value loaded at the start of step *n* (`get_inp_data` runs before the rules are tested). | — |
| TS-02 | I | C | P1 | `OPRULE_TIME_SERIES` with `FILE constant` and value `1.0` (as `clfct_op` in `oprule_historical_gate.inp`) | `ts(name=...)` returns `1.0`. Check whether `pathinput%value` is filled from `constant_value`. | — |
| TS-03 | I | C | P1 | Missing or fill-in data (`last`, gaps) | Record the value `ts` returns at a gap; comparison `>= 1.0` behaviour. | — |
| TS-04 | F | C | P1 | `ts_index` when a name is shared with a non-oprule input path | First match wins over all `pathinput` entries (not only oprule ones). | — |

### 6.4 Fortran routines (F)

| ID | Check | Expected | Observed |
|---|---|---|---|
| FOR-01 | `gate_index`, `device_index`, `reservoir_index`, `reservoir_connect_index`: case folding, unknown -> `miss_val_i`, trailing spaces | Lower-cased lookups; `miss_val_i` for unknown. | — |
| FOR-02 | `qext_index`, `transfer_index`: case folding | No folding. | — |
| FOR-03 | `chan_comp_point` at `dist = 0`, fractional, and equal to `length` | Weights sum to 1; endpoints map to the end points. | — |
| FOR-04 | `set_device_op_coef` with each direction and an invalid direction | Invalid direction silently sets nothing; `to_from_node` sets both. | — |
| FOR-05 | `set_device_nduplicate` | `nint` rounding (0.5, negative, large). | — |
| FOR-06 | `get_external_flow`/`set_external_flow`, transfer equivalents | Direct read/write of `qext%flow` / `obj2obj%flow`. | — |
| FOR-07 | `store_values` with all source types populated | Each property fetched from its own source; `install_datasource` is fetched but `setFree` is commented out. | — |

---

## 7. Real input corpus (CORP)

Files: `dsm2_studies/common_input/oprule_{historical_gate,hist_restoration,hist_temp_barriers,montezuma_planning_gate,temp_barriers_planning}.inp`.

| ID | L | K | P | Check | Expected | Observed |
|---|---|---|---|---|---|---|
| CORP-00 | S | R | P0 | Corpus parse test: read the five files with the project's own reader (or a small Python reader that honours quotes, `#`, `^`, sections, `${}`), build the assembled `name := ... WHEN ...;` texts, and parse them all with a **permissive stub lookup** (accepts all model names with dummy nodes and all argument values) | All rules and expressions parse. Anything that fails is either a defect in the input or a lexer/grammar gap. | — |
| CORP-01 | S | C | P1 | Lint: duplicate names across files (for example `glc_barrier_elev` appears in more than one file), names differing only by case (`FalseBarrier_*`), names over 32 characters, assembled text over 1024 characters | Report list. | — |
| CORP-02 | S | C | P1 | Lint: references to expressions or `ts` names not defined in the same file or any other included file | Report list. | — |
| CORP-03 | S | C | P1 | Lint: rule pairs on the same target whose triggers can overlap (for example `glc_install_in/out`, `orhrb_fall_install_*` vs `orhrb_fish_install_*`) | Report list, flagged where ramps are involved. | — |
| CORP-04 | S | C | P2 | Lint: usage of `gate_coef ... TO <time-dependent expression>`, `direction=both`, `bidir`, `to_from_node` with `RAMP`, `THEN` chains, lagged expressions | Report list (maps to D-04, D-06, D-10, D-13, D-17, D-11). | — |
| CORP-05 | I | C | P2 | `oprule_montezuma_planning_gate.inp`: rules named `mscs_open_from` / `mscs_open_to` set the **opposite** directions (`to_node` / `from_node`) | Confirm with the modeller that it is intentional. Same for `mscs_close_*` ordering. | — |

---

## 8. Integration scenarios (I1 fixture)

| ID | P | Scenario | Expected |
|---|---|---|---|
| INT-01 | P0 | `SET ext_flow(name=q1) TO 0.0 WHEN TRUE` | `q1 = 0` from step 2 onward, constant source. |
| INT-02 | P0 | `SET gate_op(...) TO 0.0 RAMP 120MIN WHEN DATETIME >= t0` | Linear ramp in the gate output, starting one step after the trigger step. |
| INT-03 | P0 | Stage hysteresis pair as in `dicu_div_151_off/on` (`chan_stage <= 2.0`, `> 2.2`, `ext_flow` set to `0` and to `-1*ts(...)`) | Each edge fires exactly once; the source toggles between constant and expression. |
| INT-04 | P1 | Gate install/remove pair on a DSS trigger (`ts >= 1.0` / `<= 0.0`) | Gate freed/installed at the right steps; ramp conflicts deferred per CFL-01. |
| INT-05 | P1 | Parse-error exit path | Exit -3 and message (INP-09). |
| INT-06 | P1 | Coupled variant `UpdateNetworkPrepare` / `UpdateNetworkWrapup` (hydro-GTM) | Same hook order and call counts as `UpdateNetwork`. |
| INT-07 | P1 | `check_input_data` mode | Rules are not stepped (only `SetBoundaryValuesFromData` runs). |
| INT-08 | P2 | Restart / warm start mid-period | Oprule state (active rules, edge memory, attached sources, accumulators) is not saved; record the behaviour after restart, and what happens to state-based triggers and ramps. |
| INT-09 | P0 | Golden run of a real study (for example one using `oprule_historical_gate.inp`): record outputs before any logging or fix change | Baseline for bitwise comparison (I2). |

---

## 9. Logging feature: acceptance tests (LOG)

Format, destination and control are undecided (reference B10). These tests are written independent of format. Each log event must carry: rule name, model time, event type, and event-specific fields.

| ID | L | P | Check |
|---|---|---|---|
| LOG-01 | I2 | P0 | With logging off (default), stdout/stderr and all result files are byte-identical to the baseline (INT-09). |
| LOG-02 | I2 | P0 | With logging on at every level, result files are still bitwise identical. Guards against logging calling `testNewlyTriggered` a second time (it consumes edge state) or evaluating stateful nodes twice. |
| LOG-03 | I | P0 | Parse-time event: one line per rule and per expression, in pool order, with the name. Count equals the `process_oprule` counts already printed. |
| LOG-04 | I | P0 | Trigger-edge event appears exactly once per false-to-true edge, with model time. Verify against RUN-03 scenario. |
| LOG-05 | I | P0 | Activation, deferral (with the name of the blocking rule), and completion events appear in the right order (CFL-01 scenario). |
| LOG-06 | I | P1 | Deferred rule: decide whether the event is logged every retry or only when the state changes; test that the chosen policy does not flood the log (duration of deferral vs line count). |
| LOG-07 | I | P1 | Action value events (ramp progress, fraction, target, value written, interface description) match the numbers in the model output for the same step (TRN-01 scenario). |
| LOG-08 | I | P1 | Per-step trigger/expression values (verbose level): one record per rule per step; size stays bounded at the lower levels. |
| LOG-09 | I | P1 | Levels: each level emits a strict superset of the events of the level below; unknown level values behave sanely. |
| LOG-10 | I | P1 | Log destination ordering: in the same stream, Fortran lines and C++ lines appear in the correct chronological order around the step (no buffering inversion). |
| LOG-11 | U2 | P1 | Interface descriptions: each writable interface (`ExternalFlow`, `TransferFlow`, `GateInstall`, `Device*`) produces a readable description including gate/device/direction or names. Needs a `describe()` or a name stored at creation. |
| LOG-12 | I | P2 | Overhead: wall time at level 0 within noise of the baseline; at the verbose level, measure and record. |
| LOG-13 | I | P1 | Coupled (`UpdateNetworkPrepare/Wrapup`) and standalone paths log the same events. |
| LOG-14 | I | P2 | Named expressions: if their values are logged, the wrapper does not change their evaluation count or order. |

---

## 9b. New finding from the smoke run

`parse_type` has a `PARSE_ERROR` value and `parse_rule` (`dsm2_oprule_management.cpp`) tests for it, but nothing ever calls `set_parsed_type(PARSE_ERROR)`. Failure is detected only through the `op_ruleparse()` return code. Add a check (GRM-10, P2): every failing parse returns non-zero, and `get_parsed_type()` after a failed parse is recorded.

---

## 10. Suggested order of work

1. **S0, S2, S4**: get the existing `oprule` unit tests compiling, add the reset helper and recording time factory. Record which existing tests pass.
2. P0 **C** tests: CASE-01..06, RUN-06, CFL-05, CFL-07/08, D-05, D-11, D-13, D-17, PSM-03, INP-01, INP-04, INP-05.
3. **CORP-00..04** and **STAT-01/02**: cheap, no model needed, and they tell us what the real inputs depend on.
4. **INT-09, I2**, then the P0 **LOG** tests, then implement logging against them.
5. Promote/convert **C** results into **R** and **X** tests; fix defects one by one, each with an **X** test turning green.

## 11. Traceability (reference document to tests)

| Reference item | Tests |
|---|---|
| A2 grammar, THEN/WHILE precedence | GRM-01..03, TRN-05/06 |
| A3 expressions, time literals | LEX-04/07/08, NODE-01..06, TIM-01..05, D-01, D-20 |
| A3 lagged | D-11, D-12 |
| A4 model names | LK-01..08, CASE-01..05 |
| A5.2 edge triggering, STARTUP | RUN-01..03, RUN-07 |
| A5.3 completion | RUN-07, TRN-01..03 |
| A5.4 ramp | TRN-01..04, DS-05, DS-06 |
| A5.5 permanent data source | DS-01..05, DS-07 |
| A5.6 static targets | DS-06, DS-08, DS-09, D-06 |
| A5.7 and B6 conflicts | CFL-01..12, D-07, D-08 |
| A6 case | CASE-01..07 |
| A6 reserved words, numbers | LEX-04/05, LEX-06 |
| A6 parse order, length limits | INP-01, INP-04..06 |
| B3 parse state | PSM-01..06 |
| B5 start-up and loop | INT-01..08, TS-01, RUN-04/05 |
| B9.1..B9.24 | D-01..D-28 (B9.16..24 map to D-17, D-07, D-16, D-21, D-22, D-23, D-24, D-25, D-26) |
| B10 logging | LOG-01..14, INT-09 |

---

## 12. Smoke tests: status and how to run

Implemented in `oprule/test/smoke/SmokeTests.cpp` (header-only Boost.Test, mock model and mock lookup, no DSM2 model needed). Covers: numeric/boolean/named/time expressions, syntax errors, rule parsing and registration, duplicate rule name, edge-triggered activation, one-step delay, linear ramp, deferral, pool-order conflict, data-source attachment, an end-to-end text -> parser -> manager -> mock-model run, and characterization of CASE-01/03, the `THEN` chain gap, unimplemented lagged expressions and scanner restart. All tests pass on the current code.

Build (standalone, from the repo root). `build_hpc5.sh` deletes and recreates `build/`, so use a separate build directory:

```bash
module purge && module load intel/2024.0 cmake/3.28.3
cmake -S oprule/test/standalone -B /scratch/psandhu/dsm2_oprule_build -G "Unix Makefiles" \
  -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
  -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_CXX_STANDARD=14 -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
  -DBOOST_ROOT=$PWD/deps/boost_1_83_0
cmake --build /scratch/psandhu/dsm2_oprule_build --target oprule_smoke_tests -j 4
(cd /scratch/psandhu/dsm2_oprule_build && ctest --output-on-failure)
```

Inside the full build, configure with `-DOPRULE_BUILD_TESTS=ON` to get the same target. The standalone project needs only flex, bison, a C++14 compiler and the bundled Boost; it still enables Fortran because `oprule/CMakeLists.txt` does.

Test helper notes: call `op_rulerestart(NULL)` before every parse (PSM-02); the parser, lookup and manager use global state, so each test case constructs a fresh fixture.
