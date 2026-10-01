# DSM2 Operating Rules: Reference

Two audiences, two parts:

- **Part A** is for rule authors (people writing `OPERATING_RULE` input tables).
- **Part B** is for maintainers (people changing the lexer/grammar, the rule runtime, or the DSM2 hydro binding).

Everything below was derived from reading the source. Items that were inferred
but not run are marked *(unverified)*. Paths are relative to the repo root (`dsm2/`).
The companion [OPRULE_TEST_PLAN.md](OPRULE_TEST_PLAN.md) turns the open questions and suspected defects into tests.

---

# Part A: Writing rules

## A1. Input tables

Rules are read from three layered input tables (defined in `dsm2/src/input_storage/generate.py`):

| Table | Fields | Max length |
|---|---|---|
| `OPERATING_RULE` | `NAME`, `ACTION`, `TRIGGER` | 32, 512, 512 |
| `OPRULE_EXPRESSION` | `NAME`, `DEFINITION` | 32, 512 |
| `OPRULE_TIME_SERIES` | `NAME`, `FILLIN`, `FILE`, `PATH` | 16, 8, 32, DSS path |

Example files: `dsm2_studies/common_input/oprule_*.inp`.

Reader behaviour (from `input_storage/src`):

- `#` starts a comment.
- A leading `^` on a data line marks the item as unused. It is dropped after layer resolution.
- Text in double quotes is one field; `${VAR}` is substituted.
- Items are layered. For the same `NAME`, a later layer overrides an earlier layer. The same `NAME` twice in one layer is a fatal error.
- After layering, the buffer is sorted lexicographically by `NAME`. This sorted order is the order in which expressions, then rules, are parsed and added to the rule pool. See A6.

Rule names are lower-cased by `process_oprule`. Two names that differ only in case (for example `FalseBarrier_in` and `falsebarrier_in`) collapse to the same name. The second then fails with "Operating rule name ... used more than once".

## A2. Rule grammar

Each row becomes the text `name := <ACTION> WHEN <TRIGGER>;` and is parsed as one statement.

```
rule     := action WHEN boolexpr
action   := SET <target> TO <expr> [RAMP <n> MIN]
          | action THEN action          # sequential
          | action WHILE action         # parallel
          | ( action )
```

- `THEN` binds tighter than `WHILE`. `A WHILE B THEN C` means `A WHILE (B THEN C)`.
- `RAMP` accepts only minutes (`RAMP 60MIN`). Without `RAMP` the change is abrupt (one step).
- `<target>` is a writable model name (A4) with arguments, for example `gate_op(gate=x,device=y,direction=to_node)`.
- Arguments are `key=value` pairs separated by `,` or `;`. A value is a number, a quoted string, or a bare name. Whitespace is ignored.
- Bare names are case sensitive (see A6).

An `OPRULE_EXPRESSION` row becomes `name := <DEFINITION>;`. The result is a named numeric or boolean expression usable in later expressions, triggers and targets.

## A3. Expressions

| Category | Forms |
|---|---|
| Arithmetic | `+ - * / ^`, unary `-`, parentheses |
| Comparison | `< <= > >= == <>` |
| Logic | `AND OR NOT`, `TRUE`, `FALSE`, `STARTUP` (same as `TRUE`) |
| Math | `sqrt abs exp log` (log10) `ln` |
| Min/max | `min2(a,b) max2(a,b) min3(a,b,c) max3(a,b,c)` |
| Conditional | `ifelse(cond, a, b)` |
| Table | `lookup(x, [x1,x2,...], [y1,y2,...])` |
| Stateful | `accumulate(expr, init [, resetCond])`, `predict(expr, linear\|quad, N MIN)`, `pid(9 args)`, `ipid(9 args)` |
| Lagged | `name(t-N)` parses, but **evaluation is not implemented** (`eval()`, `step()`, `copy()` throw; see B9). Do not use. |
| Time | `DATETIME` (or `DATE`), `SEASON`, `YEAR MONTH DAY HOUR MIN DT` |
| Time literals | `ddMONyyyy [hh:mm]` (absolute), `ddMON [hh:mm]` (seasonal), month names `JAN`..`DEC` as numbers 1..12 |

Time semantics:

- `DATETIME` is model Julian minutes. An absolute literal is converted to Julian minutes, so `DATETIME >= 28SEP1992 00:00` works.
- `SEASON` is minute-of-year. A seasonal literal is converted using the model's current year, so `SEASON > 16MAY` works across years.
- `DT` is the time step in seconds.
- `RAMP n MIN` is converted to seconds.

## A4. Model names

Registered in `dsm2/src/oprule_interface/dsm2_hydro_named_value_lookup.cpp`.

| Name | Args | Access | Kind |
|---|---|---|---|
| `chan_stage`, `chan_flow`, `chan_vel` | `channel`, `dist` (number or `length`) | read | expression |
| `res_stage` | `res` | read | expression |
| `res_flow` | `res`, `node` | read | expression |
| `ts` | `name` (an `OPRULE_TIME_SERIES` name) | read | expression |
| `INSTALL`, `OPEN` = 1; `REMOVE`, `CLOSE` = 0 | none | read | constant |
| `ext_flow` | `name` | read/write | time-dependent |
| `transfer_flow` | `transfer` | read/write | time-dependent |
| `gate_install` | `gate` | read/write | static |
| `gate_op` | `gate`, `device`, `direction` = `to_node` \| `from_node` \| `to_from_node` \| `bidir` | read/write | time-dependent |
| `gate_height`, `gate_elev`, `gate_width`, `gate_nduplicate` | `gate`, `device` | read/write | time-dependent |
| `gate_coef` | `gate`, `device`, `direction` = `to_node` \| `from_node` \| `both` | read/write | **static** |

Notes:

- `channel` is the external channel number. `node` is the external node number. `gate`, `device`, `res` are looked up in lower case.
- "Time-dependent" vs "static" decides what happens when an action completes (A5).
- The direction keywords differ between `gate_op` (`to_from_node`, `bidir`) and `gate_coef` (`both`), as coded.
- `gate_coef` with `direction=both` is accepted by the parser, but the Fortran `get/set_device_flow_coef` routines only handle `to_node`/`from_node` and call `exit(3)` otherwise (B9). Use `to_node`/`from_node` only.

## A5. Runtime semantics

1. Each time step: stored data is loaded, active actions are advanced, the network is solved, then all rules' expressions are stepped and all inactive rules' triggers are tested (see B5).
2. A rule is activated only when its trigger goes **false to true** (edge triggered). `WHEN TRUE` and `WHEN STARTUP` fire once, on the first test after the first step. The action starts advancing at the start of the next step.
3. A rule's action runs to completion (one step if abrupt, the ramp length otherwise) and the rule becomes inactive. It can fire again only after its trigger has gone false and then true again.
4. While ramping, the target expression is re-evaluated each step:
   `value = base*(1-f) + target*f`, where `f` is the ramp fraction (0 to 1).
5. When a time-dependent action completes, the rule's target expression is attached as the variable's **permanent data source**:
   - If the expression is time dependent (for example it contains `ts(...)`, `chan_stage(...)`, or a time term), it is evaluated every step from then on. This overrides DSS/boundary input.
   - If the expression is constant, it is stored as a constant value.
   - This lasts until another rule's completion replaces it.
6. A static target (`gate_install`, `gate_coef`) is set once, at the value the expression had during the activation step, and is **not** updated afterwards. `SET gate_coef(...) TO <time-varying expr> WHEN TRUE` is therefore evaluated once.
7. Conflict handling: a newly triggered rule whose action overlaps an *active* rule's action is **deferred**. Its trigger memory is reset, so it is retried on the following steps while the trigger stays true. There is no "replace" or "ignore" behaviour in the DSM2 binding (see B6).

## A6. Gotchas

- **Case**: keywords are case insensitive, but model names and argument values are matched exactly. Real files use lower case for names and upper case for constants (`OPEN`, `INSTALL`). Gate and reservoir names are lower-cased by the Fortran lookup, but `ext_flow` names, `transfer` names and `ts` names are not.
- **Reserved words** cannot be used as bare argument values or names: `abs sqrt exp log ln max2 min2 max3 min3 lookup false true startup ifelse or and not set to when while then ramp step accumulate predict linear quad pid ipid date datetime season year month day hour min dt` and the month names `jan`..`dec`.
- **Parse order**: expressions are parsed before rules, each in sorted-name order. An expression that refers to another expression must sort after it, otherwise the referenced name is unknown at that point.
- **Rule order matters for conflicts**: rules are tested in sorted-name order, so a lower name wins a conflict in the same step.
- **Overlapping device actions**: any two actions on the same gate *device* (any property or direction) are considered overlapping (B6). A long `RAMP` on one property defers every other rule touching that device until it completes.
- **Length limits**: the Fortran buffer for the combined rule text is 1024 characters (`process_oprule.f90`). `NAME` is limited to 32 characters.
- **Numbers**: an exponent is accepted only after a decimal point (`1.0e5`, not `1e5`).
- **Parse failure**: any parse error prints the rule to the error unit and stops the model with exit code -3.

---

# Part B: Maintainer reference

## B1. File map

| Area | Files |
|---|---|
| Lexer | `oprule/lib/parser/op_rule.l` |
| Grammar | `oprule/lib/parser/op_rule.y` |
| Parse state | `oprule/oprule/parser/ParseSymbolManagement.h`, `oprule/lib/parser/ParseSymbolManagement.cpp` |
| Symbols and interfaces | `oprule/oprule/parser/{Symbol,NamedValueLookup,NamedValueLookupImpl,ModelTimeNodeFactory,ModelNameParseError}.h` |
| Expression nodes | `oprule/oprule/expression/*.h`, `oprule/lib/expression/*.cpp` |
| Rule runtime | `oprule/oprule/rule/*.h`, `oprule/lib/rule/*.cpp` |
| DSM2 binding (C++) | `dsm2/src/oprule_interface/*.cpp, *.h` |
| DSM2 binding (Fortran) | `dsm2/src/oprule_interface/oprule_management.f90`, `dsm2/src/common/model_interface.f90` |
| Input handling | `dsm2/src/fixed/process_oprule.f90`, `dsm2/src/fixed/process_text_hydro_input.f90`, `dsm2/src/input_storage/generate.py` |
| Time loop | `dsm2/src/hydro/fourpt.f90`, `dsm2/src/hydrolib/update_network.f90`, `dsm2/src/hydrolib/netbnd.f90` |
| Unit tests (not built) | `oprule/test/parser`, `oprule/test/rule` |

Libraries: `oprule` (runtime), `oprule_parser` (lexer, grammar, symbol management), `oprule_interface` (Fortran) and `oprule_interface_cpp` (DSM2 C++ binding).

## B2. Build

`oprule/CMakeLists.txt` generates the parser with custom commands. `flex` and `bison` must be on `PATH`.

```
flex  -L -P op_rule -o op_rule.cpp   lib/parser/op_rule.l
bison -l -v -p op_rule -d            lib/parser/op_rule.y    # then rename op_rule.tab.c -> op_rule_tab.cpp
```

- `-P`/`-p op_rule` prefixes all symbols, so the entry points are `op_rulelex()` and `op_ruleparse()`, and `yylval` is `op_rulelval`.
- Outputs land in the build directory (`build/oprule/`). `op_rule.output` (from `-v`) holds the state table and any grammar conflicts. Check it after grammar edits.
- `lib/parser/FlexBisonBuildSettings.txt` is a legacy MSVC note and is not used.
- The lexer includes `op_rule.tab.h`. The grammar has `%define parse.error verbose`.

## B3. Lexer and grammar

**Lexer (`op_rule.l`)**

- Case insensitive. Input comes from a string via `YY_INPUT` (`yaccstring`), set with `set_input_string()`.
- Tokens: single characters `- + * / : ( ) [ ] , . ^ t ;`. `t` exists for `name(t-1)`.
- Identifier resolution, in order:
  1. `name_lookup()` (earlier named expressions): `NAMEDVAL` or `BOOLNAMEDVAL`.
  2. `get_lookup()` (model names): `IFNAME`, `IFPARAM` (writable) or `EXPRESS`, `EXPRESSPARAM` (read-only), depending on read/write type and whether the name takes arguments.
  3. Otherwise `NAME`.
- Date tokens: `REFDATE` (`ddMONyyyy`), `REFTIME` (`hh:mm`), `REFSEASON` (`ddMON`). A bare month name returns `NUMBER` (1..12).
- Neutral notes on the current lexer:
  - The whitespace rule is `[\t];` (a tab followed by `;`). Spaces are swallowed by the catch-all rule, which prints "Unmatched" for any other character.
  - The grammar declares `MINDAY`, which the lexer never returns.
  - The exponent in the number pattern attaches only to the decimal-point alternative.
  - `RAMP` uses only the `MIN` token (minutes).

**Grammar (`op_rule.y`)**

- Start symbol `line`. Alternatives: named boolean/numeric expression (`NAME ASSIGN ...;`), bare expression (stored as `eval__`), named rule, unnamed rule (auto-named `OpRuleN`), a `reassignment` error case, and `error ';'` (aborts).
- Semantic values in the `%union` are all `int` indices into a temporary symbol vector (`get_temp_symbol(i)`), not pointers.
- Rule assembly: `modelaction` builds a `ModelAction<double>` with an `AbruptTransition` or `LinearTransition`. `action THEN action` calls `chain_actions()` (an `ActionChain`). `action WHILE action` calls `group_actions()` (an `ActionSet`). `oprule` builds `OperatingRule(action, ExpressionTrigger(bool->copy()))`.
- Model names: `interface` and `namedval` call `get_lookup()->getModelExpression(name, argmap)`. `MissingIdentifier`, `InvalidIdentifier`, `ModelNameNotFound` are caught and turned into `YYERROR`.
- Precedence: comparison, `OR`, `AND`, `NOT`, `+ -`, `* /`, `^`, unary minus; `WHILE` then `THEN` (so `THEN` binds tighter).

**Parse state** (`ParseSymbolManagement.cpp`): file-level globals hold named expressions, rules, the temp symbol vector, the argument map, lagged-value list, lookup pointer, time factory pointer, and last parse type. The parser is **not reentrant**. `parse_rule` does not clear any state between calls except what the grammar actions clear (`clear_temp_expr`, `clear_string_list`, `clear_arg_map`).

## B4. Rule runtime (`oprule/rule`)

| Class | Role |
|---|---|
| `OperatingRule` | action + trigger + name. `testNewlyTriggered()` is the false-to-true edge detector and must be called once per step. `deferActivation()` clears the edge memory. `getActionList()` flattens the action tree for conflict checks. |
| `OperationAction` | Base class. Has a weak parent pointer and `childComplete()` for compound actions. |
| `ModelAction<T>` | Leaf action: interface + target expression + `Transition`. `setActive(true)` snapshots the model value when the interface is static. `advance(dt)` blends and calls `set()`. On completion it calls `setDataExpression()` for time-dependent interfaces. |
| `ActionChain` | Serial. `childComplete()` activates the next child and advances it by 0. |
| `ActionSet` | Parallel. Completes when no child is active. |
| `Transition` | `AbruptTransition` (f = 1), `LinearTransition` (f = t/T), `SmoothStepTransition` (unused by the grammar). |
| `ExpressionTrigger` | Holds a bool expression. `test()` evaluates it, `step()` steps it. |
| `OperationManager` | Pool of rules; `addRule` (inactive), `manageActivation`, `advanceActions`, `stepExpressions`. |
| `ActionResolver` | `overlap(a1, a2)` and `resolve()`. `ModelInterfaceActionResolver` implements `overlap` through a Loki double dispatcher; `resolve()` always returns `DEFER_NEW_RULE`. |

`manageActivation()`: for each **inactive** rule whose trigger just became true and whose action is applicable, compare with every **active** rule via `checkActionPriority` (`actionsOverlap` returns `DEFER_NEW_RULE` or `RULES_COMPATIBLE`). If any active rule overlaps, the new rule is deferred. Otherwise it is activated. The enum also has `REPLACE_OLD_RULE`, `IGNORE_NEW_RULE`, `RECONCILE_RULES`, which the current manager never returns. The code handling them is still present.

`stepExpressions(dt)` steps **every** rule (active or not), so stateful nodes (lagged, accumulate, PID, extrapolation) advance each step.

## B5. DSM2 hydro binding

**Start-up** (`fourpt_init` in `hydro/fourpt.f90`):

1. `InitOpRules` (`oprule_management.f90`) calls C `init_parser_f`, which runs `lexer_init()`, `init_expression()`, `init_lookup(new DSM2HydroNamedValueLookup())` and `init_model_time_factory(new DSM2HydroTimeNodeFactory())`.
2. Channels, reservoirs and gates are initialised, then `SetBoundaryValuesFromData()`.
3. `process_text_oprule_input()` runs. It cannot run earlier because parsing looks up channel/gate/reservoir indices. It processes expressions, then rules. `OPRULE_TIME_SERIES` rows are handled by a separate routine in the same file (`process_input_oprule`); its position relative to these steps was not traced.
4. Each row goes through `process_oprule` or `process_oprule_expression`, which format the text and call C `parse_rule_`. On failure the Fortran caller prints the text and calls `exit(-3)`. On success with parse type `OP_RULE`, `parse_rule` fetches `getOperatingRule()` and calls `dsm2_op_manager.addRule`.

**Time loop** (`hydrolib/update_network.f90`, `UpdateNetwork`; `UpdateNetworkPrepare` and `UpdateNetworkWrapup` are the split variants for coupled runs):

```
SetBoundaryValuesFromData     # store_values: every datasource -> model values (fetch_data)
AdvanceOpRuleActions(dt)      # active rules write values (set)
ApplyBoundaryValues
... Newton iteration: gates, channels, reservoirs, solve ...
CloseNetworkIteration
StepOpRuleExpressions(dt)     # step all rules' expressions
TestOpRuleActivation(dt)      # test triggers, resolve conflicts, activate
```

The C entry points are `advanceopruleactions_`, `stepopruleexpressions_`, `testopruleactivation_` (`dsm2_oprule_management.cpp`). `TestOpRuleActivation` ignores its time argument.

**Name lookup**: `DSM2HydroNamedValueLookup` (a `NamedValueLookupImpl`) registers names with `ADD_EXPRESS_nARG(info, name, READONLY|READWRITE, factory, args...)`. Factories (`dsm2_named_value_factories.cpp`) convert external IDs with Fortran callbacks (`ext2int`, `ext2intnode`, `gate_index`, `device_index`, `reservoir_index`, `reservoir_connect_index`, `qext_index`, `transfer_index`, `ts_index`, `channel_length`) and throw `MissingIdentifier`/`InvalidIdentifier` on bad arguments. The argument list of each registration is used only for "takes arguments?".

**Node classes**: read-only nodes (`ChannelFlowNode`, `ChannelWSNode`, `ChannelVelocityNode`, `ReservoirFlowNode`, `ReservoirWSNode`, `DSM2TimeSeriesNode`, time nodes in `dsm2_time_interface.h`) implement `eval()`, `copy()`, `isTimeDependent()`. Writable nodes (`ExternalFlowInterface`, `TransferFlowInterface`, `GateInstallInterface`, `Device*Interface`) also implement `set()` and, if time-dependent, `setDataExpression()`.

**Data sources** (`datasource_t` in `dsm2_defs/type_defs.f90`): `{value, source_type, indx_ptr}`.

- `setDataExpression(expr)` calls `register_express_for_data_source(expr)`, which stores the node in a global vector (`dsm2_expressions.cpp`) and returns its index. It then calls a Fortran `set_*_datasource`, which calls `set_datasource`: `expression_data` if `expr->isTimeDependent()`, else `const_data` with the value.
- `fetch_data(source)` (`model_interface.f90`) returns the constant, the DSS path value (`pathinput(i)%value`), or `get_expression_data(i)`, which evaluates the registered node.
- `store_values` in `netbnd.f90` calls `fetch_data` every step for transfers, stage boundaries, external flows and all gate device properties.

**Time series**: `ts(name=...)` uses `ts_index` to find the `pathinput` entry created by `process_input_oprule`, and `DSM2TimeSeriesNode::eval()` reads `pathinput(i)%value` through `value_from_inputpath`. The value is refreshed by the normal input reading (`get_inp_data`).

## B6. Conflict detection (DSM2 resolver)

`dsm2_model_interface_resolver.h` defines `DSM2ModelInterfaceResolver` and a `Loki::StaticDispatcher` over four types: `ExternalFlowInterface`, `TransferFlowInterface`, `GateInstallInterface`, `DeviceInterface`.

| Pair | Overlaps when |
|---|---|
| ext flow / ext flow | same index |
| transfer / transfer | same index |
| gate install / gate install | same gate |
| gate install / device | same gate |
| device / device | same gate and same device |
| anything else | no (`OnError` prints "Default: assume no conflict between actions" and returns false) |

- All `Device*` interfaces derive from `DeviceInterface`, so they match the `DeviceInterface` entry regardless of property or direction *(unverified; relies on the dispatcher's dynamic-cast matching)*.
- `OperatingRule::getActionList()` flattens the action. `ActionSet` supplies its members. `ActionChain` defines `appendToActionList()` but not the virtual `appendSubActionsToList()` that `getActionList()` calls, and the base version is an empty stub. A rule whose top-level action is a `THEN` chain therefore has an empty action list and never overlaps anything (**known limitation**).
- Deferral causes repeated trigger re-tests: `deferActivation()` resets the edge memory, so the rule is retried each step.

## B7. Extension recipes

**New read-only model value** (for example a new channel quantity)
1. Add a Fortran getter in `common/model_interface.f90` with `bind(C, name=...)`; declare it `extern "C"` in `dsm2_interface_fortran.h`.
2. Add an `ExpressionNode<double>` subclass in `dsm2_model_interface.h/.cpp` with `eval()`, `copy()`, `isTimeDependent()`.
3. Add a factory in `dsm2_named_value_factories.cpp` and declare it in the `.h` (`EXPRESSION_FACTORY`).
4. Register it in `DSM2HydroNamedValueLookup` with `ADD_EXPRESS_nARG(..., READONLY, ...)`.

**New writable variable**
1. As above, plus a Fortran `set_*` and (if time dependent) `set_*_datasource` calling `set_datasource`.
2. Add a `datasource_t` member to the relevant Fortran type and a `fetch_data` assignment in `store_values` (`netbnd.f90`).
3. Implement `ModelInterface<double>` (`set`, `eval`, `isTimeDependent`, `setDataExpression`).
4. Register with `READWRITE`.
5. Add it to `DSM2ModelInterfaceResolver` (a new `Fire` overload) and to the `LOKI_TYPELIST_n` in the `DSM2Resolver` typedef. The type list length macro changes with the number of types.

**New function or operator**: add tokens in `op_rule.l`, the `%token`/`%type` and production in `op_rule.y`, and a node class under `oprule/oprule/expression/`. Check `op_rule.output` for new conflicts. Add a test in `oprule/test/parser/TestParser.cpp`.

**New time term**: add a method to `ModelTimeNodeFactory`, implement it in `DSM2HydroTimeNodeFactory`, and add a node class in `dsm2_time_interface.h` (the `TIMECLASS` macro) plus its Fortran getter.

**Change activation or conflict policy**: `OperationManager::manageActivation` and `checkActionPriority`, and `ActionResolver::resolve()`. The unused enum values already reserve the extension points.

## B8. Tests

- `oprule/test/parser/TestParser.cpp` and `TestExpression.cpp` exercise the grammar (numeric, boolean, date, assignment, lagged, rules with `WHILE`/`THEN`).
- `oprule/test/rule/*` exercise `ModelAction`, `OperationManager` and the state-action classes.
- The test executables are commented out in `oprule/CMakeLists.txt`, so none of these tests are built or run by the current build. `TestStateAction` appears stale.
- `dsm2/tests/hydro/CMakeLists.txt` links the oprule libraries into `test_hydro`; its content was not reviewed here.

## B9. Suspected defects and observations

Not verified by running the code. Listed so they can be confirmed or dismissed.

1. **REFSEASON + REFTIME hour is always 0** (`op_rule.y`, `date` rule). The hour is taken from `substr(2,3)` of `"HH:MM"`, which is `":MM"`. The minute (`substr(3,2)`) is correct. Real inputs use date-only seasonal literals, so this has not been hit.
2. **`ChannelFlowNode` / `ChannelWSNode` store `distance` as `int`.** `copy()` re-creates the node from the truncated value, so a fractional `dist` is lost in copies (triggers and action targets are copied by the grammar). `ChannelVelocityNode` stores a double.
3. **`DeviceInterface::operator==`** and the device subclass comparisons compare `devndx == rhs.ndx` (should be `rhs.devndx`). The resolver uses its own `Fire` overloads, so it is not affected.
4. **`get_device_op_coef` for `to_from_node`** averages `opCoefFromNode` with itself instead of with `opCoefToNode`. It affects the starting value of a ramp on `direction=to_from_node` when the two directions differ.
5. **Fortran/C++ ABI mismatches for `*_datasource` calls.**
   - C++ declares the `timedep` argument as `const bool&` (one byte, by reference) everywhere (`dsm2_interface_fortran.h`).
   - Most Fortran routines declare `logical timedep` (4 bytes, by reference).
   - `set_device_nduplicate_datasource` declares `logical(c_bool), value` (by value) while the caller passes a pointer, so it tests a pointer-derived byte.
6. **`gate_coef` is static** (`isTimeDependent() == false`, no `setDataExpression`). A rule such as `SET gate_coef(...) TO MIN2(ts(...)/(chan_stage(...)+13.2),1)*0.8+0.75 WHEN TRUE` (as in `oprule_historical_gate.inp`) is applied once, not continuously.
7. **`THEN` chains never conflict** (B6). Stated as a known limitation.
8. **All device properties overlap each other** for the same device (B6), so a long `RAMP` on one property defers every other rule on that device.
9. **`ParseSymbolManagement` globals** make the parser non-reentrant.
10. Direction keywords differ between `gate_op` (`bidir`, `to_from_node`) and `gate_coef` (`both`).
11. **Lagged expressions are unfinished.** `LaggedExpressionNode::eval()`, `step()` and `copy()` all `throw new std::logic_error("NOT IMPLEMENTED!")`. This throws a *pointer*, which `catch (std::exception&)` does not catch. `oprule/doc/todo.txt` lists "Complete lagged expressions". `apply_lagged_values` also looks wrong: for every lagged node whose name differs from the newly defined name it sets that node's expression to the new definition. The lagged list (`laggedvals`) is never cleared.
12. **`gate_coef` with `direction=both`** maps to `direct_to_from_node()`, which `get_device_flow_coef`/`set_device_flow_coef` do not handle; they print "Flow direction not recognized" and call `exit(3)`. `get_device_flow_coef` runs at rule activation (the interface is static), so the model would stop at that point.
13. **`accumulate`** adds the expression value each step without multiplying by `dt` (source comment: "todo: urgent decide this"). The initial value is evaluated once at construction (parse time). On a reset step the initializer is applied and the expression is then added in the same step.
14. **`pid`/`ipid`**: arguments 3 to 9 are evaluated once at parse time (`->eval()`), so they must be constants. `PIDNode::eval()` asserts `_yold != HUGE_VAL` and `init()` is not called by the parser (source comment: "horrible workaround"), so `eval()` before the first `step()` is unsafe.
15. `ModelInterface::setDataExpression` default, and the `LaggedExpressionNode` methods, use `throw new` (pointer throw).

## B10. Logging: where to hook in

Goal: log rule triggers, activation/deferral/completion, per-step trigger values, values written by actions, expression evaluations, and parse-time rule loading. Where it writes and how it is controlled is undecided; options are listed at the end.

**What exists today**
- Fortran side: `process_oprule.f90` prints the rule text when `print_level >= 3`, and prints an error plus `exit(-3)` on failure. `op_ruleerror` prints to `cerr`.
- C++ side: `OperationManager::manageActivation` and `OperatingRule` contain commented-out `cout` debug lines (for example "triggered", "deferred by rule", "Activating"). `OperatingRule` has `getName()`. The model classes have no names.
- Model time is available from C++ via the Fortran-bound `get_model_time` (Julian minutes).

| Event | Hook | Notes |
|---|---|---|
| Rule loaded (parse time) | `parse_rule` in `dsm2_oprule_management.cpp`, after `addRule`; or `process_oprule` in Fortran | Rule name via `getOperatingRule()->getName()`. The Fortran side already has the text. |
| Trigger went true (edge) | `OperatingRule::testNewlyTriggered` | Already has the current and previous value. Called once per step, so do not add extra calls to it. |
| Activated / deferred / blocked / replaced | `OperationManager::manageActivation` | Existing commented lines mark the points. |
| Completed | `ModelAction::onCompletion` / `OperatingRule::isActive` transitions | Completion is detected inside the action, with no rule name there. The manager can log when a rule is seen to go inactive in `advanceActions`. |
| Per-step trigger value (verbose) | `OperatingRule::testTrigger` (or inside `manageActivation` for every inactive rule) | Verbose: rules x time steps. Gate behind a level. |
| Values written by actions | `ModelAction::advance` (`_elapsed`, `_transFraction`, `_currentState`) | Needs a description of the interface. `ModelInterface` has no name today, so add a `describe()` virtual or pass a label from the parser. |
| Named-expression values | No hook today | Expressions are nodes evaluated lazily inside triggers/targets. Options: a decorator node created in `add_symbol`, or evaluate-and-log in the manager for named expressions. |
| Step boundary | `advance/step/test` wrappers in `dsm2_oprule_management.cpp` | Good place for a once-per-step header with model time. |

**Options for the channel and control** (not chosen)
1. Gate by the existing Fortran `print_level` and write through a small Fortran-bound C logging shim, so output goes to `unit_output`/`unit_screen` (consistent with the rest of the model, avoids interleaving with buffered Fortran output).
2. A dedicated oprule log file opened at `InitOpRules`.
3. Plain C++ `std::cout` controlled by an environment variable or a flag passed from Fortran (simplest, but can interleave with Fortran output).
