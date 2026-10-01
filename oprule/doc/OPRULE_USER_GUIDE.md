# DSM2 operating rules: user's guide

This guide is for people who write or review DSM2 operating rules. It covers how rules work, how to write them, the complete list of model variables and functions, worked examples from the Delta studies, and a troubleshooting list. It replaces the shorter [Operating Rule Guide](https://cadwrdeltamodeling.github.io/dsm2/manual/reference/Operating_Rule_Guide/) on the DSM2 website and corrects a few places where that page differs from what the model does (section 13).

Related documents in this directory:

- [OPRULE_LOG_USER_GUIDE.md](OPRULE_LOG_USER_GUIDE.md): how to see what your rules did, after a run.
- [OPRULE_REFERENCE.md](OPRULE_REFERENCE.md): the maintainer's reference, with the list of known defects (B9).

---

## 1. What an operating rule is

An operating rule changes something in the model during a run, in response to what is happening in the model. Typical uses:

- close a gate when the flow reverses, open it when the head difference is right;
- install or remove a temporary barrier on a date;
- follow a gate schedule from a time series;
- set a boundary flow or export to a new value when the season or the stage changes.

A rule has two parts:

| Part | Question it answers | Example |
|---|---|---|
| **Trigger** | When does this rule apply? | `chan_vel(channel=484, dist=5750) < 0` |
| **Action** | What does it do? | `SET gate_op(gate=montezuma_salinity_control, device=radial_gates, direction=from_node) TO CLOSE` |

The two are written together as one statement:

```
close_radial := SET gate_op(gate=montezuma_salinity_control, device=radial_gates, direction=from_node) TO CLOSE
                WHEN chan_vel(channel=484, dist=5750) < 0;
```

Read it as: "the rule named `close_radial` sets the radial gate's from-node coefficient to closed when the velocity in channel 484 goes negative."

**The one idea to hold on to:** a trigger is not "while this is true". The action starts at the moment the trigger **changes from false to true**, once. Nothing happens while it stays true, and nothing happens when it goes false again (section 6).

---

## 2. Where rules are written

Rules live in three input tables, normally in a file such as `oprule_<study>.inp`, and are listed in the study's configuration like any other input file.

| Table | Columns | What it holds |
|---|---|---|
| `OPERATING_RULE` | `NAME`, `ACTION`, `TRIGGER` | The rules |
| `OPRULE_EXPRESSION` | `NAME`, `DEFINITION` | Named expressions that rules can use |
| `OPRULE_TIME_SERIES` | `NAME`, `FILLIN`, `FILE`, `PATH` | Time series (DSS) that rules can read with `ts(name=...)` |

Example (the names and numbers are illustrative, apart from the gate rule, which is taken from the historical study inputs):

```
OPRULE_EXPRESSION
NAME            DEFINITION
mscs_calc       "ts(name=mscs_op) < 0"
mscs_velclose   "chan_vel(channel=512,dist=5750) < -0.1"
END

OPERATING_RULE
NAME            ACTION                                                                                          TRIGGER
mscs_close      "SET gate_op(gate=montezuma_salinity_control,device=radial_gates,direction=from_node) TO 0.0"   "mscs_velclose AND mscs_calc"
END

OPRULE_TIME_SERIES
NAME            FILLIN    FILE                      PATH
mscs_op         last      ${TSINPUTDIR}/mscs.dss    /HIST/MSCS/OP//15MIN/DWR/
END
```

Table conventions (the same as other DSM2 input tables):

- `#` starts a comment.
- A leading `^` on a line switches that row off.
- Put a text value in double quotes if it contains spaces. `${NAME}` is replaced by the environment variable.
- Tables are layered. A row with the same `NAME` in a later layer replaces the earlier one; the same `NAME` twice in one layer is an error.
- The trigger cannot be left blank: the statement `name := action WHEN ;` is a syntax error and stops the model. Write `TRUE` for a rule that should start once at the beginning (section 7).

When the model starts, each row is turned into a single statement of the form `name := ACTION WHEN TRIGGER;` and parsed. This is the form used in the rest of this guide (the website guide writes `WHERE`; the keyword is `WHEN`).

Limits: `NAME` is at most 32 characters; the action and trigger are at most 512 characters each, and the whole statement at most 1024. Names are converted to lower case, so two names that differ only in case are the same name and the second one is an error.

---

## 3. Expressions

An expression is anything that has a value: a model variable, a number, a time term, a series, or a combination.

### Named expressions

A named expression gives an expression a name so that it can be reused:

```
ebb := chan_flow(channel=132, dist=1000) > 0.01
vamp := (MONTH == APR) OR (MONTH == MAY)
critical_stage := chan_stage(channel=132, dist=1000) < (ts(name=tide_level) - 1.0)
ebbmagnitude := LOG(chan_flow(channel=132, dist=1000))
```

`ebb` and `vamp` are logical (true or false) and are normally used in triggers; `ebbmagnitude` is numeric and can be used in an action's target. A named expression is evaluated again at each time step and always shows the current value.

Rules for named expressions:

- Expressions are read before rules, in the order their names sort (capital letters before small ones). An expression that uses another expression must therefore have a name that sorts **after** the one it uses; otherwise the model stops with an unknown-name error. `a_total := b_part + 1` fails because `a_total` is read before `b_part` exists; `z_total := b_part + 1` works.
- A name cannot be one of the reserved words (section 13).

### Model variables

A model variable is a name followed by identifiers in parentheses, written as `key=value` pairs separated by commas:

```
chan_flow(channel=132, dist=1000)          numbers
gate_op(gate=middle_river_barrier, device=weir, direction=to_node)    names
```

Names (gate, device, reservoir) are matched in lower case; write them as they appear in the input. A value with spaces or special characters goes in single or double quotes. The complete list is in section 11.

### Time series

`ts(name=tide_level)` is the current value of a time series listed in `OPRULE_TIME_SERIES`. Use the series name, not the DSS path.

### Time

`YEAR`, `MONTH`, `DAY`, `HOUR`, `MIN`, `DATE`, `DATETIME`, `SEASON`, `DT`: see section 11.4. Compare months by their three-letter **upper-case** names:

```
(MONTH == APR) OR (MONTH == MAY)
DATETIME >= 04FEB1990 00:00
SEASON > 15APR AND SEASON < 01MAY
```

### Math and logic

`+ - * / ^`, parentheses, `< <= > >= == <>`, `AND OR NOT`, `TRUE FALSE`, and the functions in sections 11.5 to 11.7.

---

## 4. Writing a rule step by step

Take the rule "during the VAMP season (April and May), when the flow in channel 132 is ebbing, set the weir operating coefficient from a time series".

1. **Define what you test for** as named expressions:

   ```
   ebb  := chan_flow(channel=132, dist=1000) > 0.01
   vamp := (MONTH == APR) OR (MONTH == MAY)
   ```

2. **Write the trigger** from them: `vamp AND ebb`.
3. **Write the action**: `SET gate_op(gate=middle_river_barrier, device=weir, direction=to_node) TO ts(name=new_time_series)`.
4. **Put them together** as one row of `OPERATING_RULE`:

   ```
   middle_vamp_ebb := SET gate_op(gate=middle_river_barrier, device=weir, direction=to_node) TO ts(name=new_time_series) WHEN vamp AND ebb;
   ```

   In the table, `NAME` is `middle_vamp_ebb`, `ACTION` is the part after `SET ...`, `TRIGGER` is `vamp AND ebb`.

The rule waits until the first time step at which `vamp AND ebb` becomes true. At that moment the action starts and from then on the weir follows `new_time_series`.

---

## 5. Actions

```
SET <model variable> TO <expression> [RAMP <n> MIN]
```

### SET ... TO

The target on the left must be a writable model variable (section 11.1 and 11.2). The expression on the right can be a number, a time series, a named expression or any combination:

```
SET ext_flow(name=sjr) TO IFELSE(vamp, ts(name=ts1), ts(name=ts2))
SET gate_op(gate=g1, device=d1, direction=to_node) TO 0.5 * ts(name=schedule)
SET gate_install(gate=old_river_barrier) TO REMOVE
```

`OPEN` and `INSTALL` mean 1; `CLOSE` and `REMOVE` mean 0.

### RAMP

`RAMP 60MIN` changes the value gradually over the given number of minutes instead of in one step. Only minutes are accepted. At each step the value written is

```
value = start * (1 - f) + target * f
```

where `f` goes from 0 to 1 over the ramp. The target is read again at every step, so if it follows a time series the ramp follows it. A ramp shorter than the model time step finishes in one step. Ramping only makes sense for quantities that can change gradually; for installing or removing a gate it has no practical meaning.

### Two actions in one rule

```
action1 WHILE action2        both at the same time
action1 THEN action2         one after the other
```

`THEN` binds more tightly than `WHILE`: `A WHILE B THEN C` means `A WHILE (B THEN C)`. Example from the study inputs, which sets both directions of a gate at once:

```
SET gate_op(gate=dcc,device=gates,direction=from_node) TO ts(name=dcc_op) WHILE SET gate_op(gate=dcc,device=gates,direction=to_node) TO ts(name=dcc_op)
```

Cautions: a rule whose whole action is a `THEN` chain stops the model when it is read (a known defect), so do not use a top-level `THEN`; write two rules instead. Actions joined by `WHILE` should have the same `RAMP` length: with different lengths a debug build of the model stops. Using the same length is what the existing study inputs do.

### What happens when the action finishes

This depends on the kind of variable being set (sections 11.1 and 11.2).

- **Dynamic control variables** (gate operating coefficient, gate height, elevation, width, number of duplicates, external flow, transfer flow). When the action completes, the target expression becomes the variable's permanent source of data. If the expression is a number, the variable keeps that value. If it is a time series or any expression that changes with time, the variable follows it **from then on, replacing whatever input series it had before**, until another rule's action finishes on the same variable. The old time series is not consulted again.
- **Static control variables** (`gate_install`, `gate_coef`). They are set once, with the value the expression had when the action started, and then left alone. If the target is a time series the model does not complain, but only the value at activation is used.

---

## 6. Triggers

A trigger is a logical expression. It is evaluated at the end of every time step for every rule that is not running.

### Triggers are one-time and one-way

A rule starts when its trigger changes from **false to true**. That is all that counts:

- If the trigger stays true, nothing further happens.
- If the trigger goes false, nothing happens: the rule does not undo itself.
- It can start again only after going false and then true again.
- While a rule's action is running its trigger is not tested. If the trigger goes false and true again during that time, the second rise is missed.

If you want something to be undone when conditions reverse, write a second rule (section 8).

### When an action takes effect

The trigger is tested at the end of step n, after the solution. The action's first value is used in step n+1. A trigger that becomes true at 07:25 therefore changes the gate for the step ending 07:30.

### Dates written with AND misfire

Consider `(YEAR >= 1990 AND MONTH >= APR AND DAY >= 14)`. It becomes true on 14APR1990, false on 01MAY, true again on 14MAY, and so on on the 14th of every later month: the rule fires again each month. What matters is every false-to-true change. Use the date functions instead:

```
DATE >= 14APR1990              true from the start of 14APR1990 on, and stays true
SEASON > 15APR AND SEASON < 01MAY        true once a year, from 15APR to the end of 30APR
DATETIME > 04FEB1990 00:00     date and time
```

`DATE` and `DATETIME` are the current model date and time, and a date literal without a time means 00:00 of that day. `SEASON` is the same without the year. `SEASON <= 30APR` is therefore true only until 00:00 on 30APR, not through the whole day; use `SEASON < 01MAY`.

### Anticipation with PREDICT

`PREDICT(expression, LINEAR, 30MIN) < 0` tests the value expected in 30 minutes by extrapolating the trend (`QUAD` for quadratic extrapolation, better over periods under an hour). It lets you write the limit you care about (stage below 0) instead of a safety buffer (stage below 1) that fires whatever the trend. Note: in the current version `PREDICT` in a **trigger** stops a debug build of the model at its first test (known defect), so test a rule that uses it before relying on it.

---

## 7. The default trigger: TRUE and STARTUP

Write `TRUE` or `STARTUP` as the trigger and the rule starts once, at the beginning of the run. (A blank trigger is not accepted; section 2.) This is right for rules that simply replace a gate's schedule or a boundary flow for the whole run:

```
falseleakage_op := SET gate_op(gate=FalseBarrier,device=leakage,direction=from_node) TO ts(name=fb_pipeop) WHEN TRUE;
```

`TRUE` means "at startup", not "always". Its only false-to-true change happens once at the start. If another rule's action overlaps it and it has to wait, it still runs when the other finishes (section 9). A trigger that is always false never does anything.

A common mistake with the default: to remove a gate for a whole simulation, writing the trigger as "gate in use" and the action as `INSTALL` does nothing useful (the gate is installed by default). Either use `TRUE` with `SET gate_install(...) TO use_gate`, or trigger on the non-default case (`remove_gate` with `REMOVE`).

---

## 8. Complementary rules and continuous rules

### A second rule for the opposite case

The complement of a rule is usually not just `NOT trigger`. Open and close often depend on different conditions; at the Montezuma Salinity Control Structure the gate is closed on velocity and opened on head difference. So write two rules:

```
middle_vamp_ebb   := SET gate_op(gate=middle_r_barrier, device=weir, direction=to_node) TO ts(name=new_time_series)   WHEN vamp AND ebb;
middle_vamp_flood := SET gate_op(gate=middle_r_barrier, device=weir, direction=to_node) TO ts(name=old_time_series)   WHEN vamp AND flood;
```

where `flood := chan_flow(channel=132, dist=1000) < -0.01`. Both rules write the same variable, so they conflict if they overlap in time; section 9 explains how that is resolved.

### A condition that is checked continuously

If you want a value to follow a condition at every step without writing paired rules, use `IFELSE` in the action and `TRUE` as the trigger:

```
sjr_flow := SET ext_flow(name=sjr) TO IFELSE(vamp, ts(name=ts1), ts(name=ts2)) WHEN TRUE;
```

This sets the boundary flow to `ts1` whenever `vamp` is true and to `ts2` otherwise.

### Hysteresis

To avoid a gate chattering when a stage hovers near a limit, use two different limits in the two triggers:

```
gate_off := SET gate_op(...) TO CLOSE WHEN chan_stage(channel=185, dist=0) <= 2.0;
gate_on  := SET gate_op(...) TO OPEN  WHEN chan_stage(channel=185, dist=0) >  2.2;
```

---

## 9. Conflicts and deferral

Two actions **overlap** when they write the same model variable. In DSM2:

| Pair | Overlap when |
|---|---|
| external flow and external flow | same flow |
| transfer flow and transfer flow | same transfer |
| `gate_install` and `gate_install` | same gate |
| `gate_install` and any device property | same gate |
| any device property and any device property | same gate and same **device** (whatever the property or direction) |

The last row matters most: a rule that ramps `gate_height` on a device blocks a rule that sets `gate_op` on that device until it has finished, and the two directions of one device (`from_node` and `to_node`) block each other.

How a conflict is resolved:

1. A rule that is triggered while an overlapping rule is **running** is **deferred**. It does not start.
2. A deferred rule is treated as if its trigger had been false, so it can make the false-to-true change again at the next step. It is retried every step, and starts when the other rule has finished, provided its trigger is still true.
3. If the trigger goes false while it waits, it never starts.
4. If two rules are triggered in the same step and overlap, rules are examined in the order their names sort (capital letters before small ones, so `Zeta` comes before `alpha`) and the first one starts. The result should not be relied on: write rules so that this does not happen.

Because of this order, name the rules so that the ones that should win come first. If a long `RAMP` is delaying other rules on the same device, shorten it or put the rules on different devices.

---

## 10. Checking your rules

- **The model parses every rule at startup.** Any error prints the rule and the reason and stops the model with exit code -3. Fix the first error and run again.
- **To see what the rules did during a run**, switch the rule log on (`oprule_log_level` in the SCALAR table). It records every rule trigger with the values that caused it, every action, and every gate device change with the rule responsible. See [OPRULE_LOG_USER_GUIDE.md](OPRULE_LOG_USER_GUIDE.md).
- **Quick checks.** After a run, look for rules that never triggered (a trigger that is always false, or a series name that is wrong), rules that fire far more often than intended (a trigger that flickers; section 6), and rules that are deferred for a long time (section 9).

---

## 11. Reference

### 11.1 Dynamic control variables

These can be set by a rule. When the action finishes, the variable keeps following the expression (section 5).

| Variable | Identifiers | Meaning |
|---|---|---|
| `gate_op` | `gate`, `device`, `direction` = `to_node`, `from_node`, `to_from_node` (or `bidir`) | Operating coefficient of a gate device in the given direction, 0 (closed) to 1 (open). `to_from_node` sets both directions and is write-only in effect: reading it returns the from-node value |
| `gate_height` | `gate`, `device` | Height of the gate device (ft) |
| `gate_elev` | `gate`, `device` | Crest or invert elevation of the device (ft) |
| `gate_width` | `gate`, `device` | Width or radius of the device (ft) |
| `gate_nduplicate` | `gate`, `device` | Number of identical structures treated as one device. Set it to a whole number: the model rounds a value set by an action, but a time series that drives it reaches the model unrounded |
| `ext_flow` | `name` | External flow (boundary flow, source or sink) |
| `transfer_flow` | `transfer` | Flow in an object-to-object transfer |

### 11.2 Static control variables

These are set once, with the value the expression has when the action starts.

| Variable | Identifiers | Meaning |
|---|---|---|
| `gate_install` | `gate` | Whether the gate is installed. `REMOVE` (or 0) takes the gate out and restores an equal-stage condition at the channel junction; `INSTALL` (or 1) puts it back |
| `gate_coef` | `gate`, `device`, `direction` = `to_node`, `from_node` | The physical flow coefficient of a device in that direction: the roughness or efficiency of the structure. It is not an operating control (use `gate_op`). Do not use `direction=both`: it stops the model |

### 11.3 Observable variables (read only)

| Variable | Identifiers | Meaning |
|---|---|---|
| `chan_flow` | `channel`, `dist` | Flow in the channel at distance `dist` from the upstream end (a number, in ft, or `length` for the downstream end) |
| `chan_vel` | `channel`, `dist` | Velocity there |
| `chan_stage` | `channel`, `dist` | Water surface elevation there |
| `res_stage` | `res` | Water surface elevation in a reservoir |
| `res_flow` | `res`, `node` | Flow from the reservoir to that external node |
| `ts` | `name` | The current value of an `OPRULE_TIME_SERIES` series |

`channel` and `node` are the external (input file) numbers. You can also read a control variable in an expression, for example `gate_install(gate=x) == INSTALL`.

### 11.4 Model time

| Term | Meaning |
|---|---|
| `YEAR`, `MONTH`, `DAY`, `HOUR`, `MIN` | The year, month (1 to 12), day, hour (0 to 23) and minute of the current model time, as numbers. Compare months by name: `MONTH == APR` |
| `DATE`, `DATETIME` | The current model date and time. Compare with a literal: `DATE >= 11OCT1992`, `DATETIME > 04FEB1990 00:00` |
| `SEASON` | The current model time within the year, without the year: `SEASON > 15APR AND SEASON < 01MAY` works in every year |
| `DT` | The model time step in seconds; multiply an `ACCUMULATE` by it to integrate |
| `ddMONyyyy [hh:mm]` | A date or date and time literal, for example `28SEP1992` or `28SEP1992 06:00` |
| `ddMON [hh:mm]` | A seasonal literal without a year, for example `15APR` |

Month names are `JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC` **in upper case**: a month name written in lower or mixed case is read as 0.

### 11.5 Numerical operations

| Form | Meaning |
|---|---|
| `+ - * /` | Arithmetic with the usual precedence; use parentheses to be sure |
| `x^y` | Power. Use parentheses around anything beyond a simple case: `-2^2` and chains of `^` do not follow the usual rules |
| `MIN2(x,y)`, `MAX2(x,y)`, `MIN3(x,y,z)`, `MAX3(x,y,z)` | Minimum and maximum |
| `SQRT(x)`, `ABS(x)`, `EXP(x)` | Square root, absolute value, e to the x |
| `LN(x)`, `LOG(x)` | Natural logarithm, base 10 logarithm |

### 11.6 Logical operations

| Form | Meaning |
|---|---|
| `x == y`, `x <> y` | Equal, not equal |
| `x < y`, `x > y`, `x <= y`, `x >= y` | Comparisons |
| `TRUE`, `FALSE`, `STARTUP` | Constants (`STARTUP` is `TRUE`) |
| `NOT expr`, `expr1 AND expr2`, `expr1 OR expr2` | Logic |

### 11.7 Special functions

| Function | Meaning |
|---|---|
| `IFELSE(cond, a, b)` | `a` if `cond` is true, else `b` |
| `LOOKUP(x, [x1,x2,x3], [y1,y2])` | Table lookup. The first list holds the limits and the second the values, one fewer. Returns the `y` for the highest limit that is `<= x`. `x` above the last limit is an error, and `x` equal to the last limit reads past the end of the values and gives an undefined result, so keep `x` below the last limit. The limits and values must be numbers, not expressions |
| `ACCUMULATE(expr, init [, reset])` | A running total of `expr` added at each step, starting at `init` and set back to `init` whenever `reset` is true. It does not multiply by the step length: write `ACCUMULATE(expr * DT, 0)` to integrate |
| `PREDICT(expr, LINEAR\|QUAD, nMIN)` | `expr` extrapolated n minutes ahead (see the caution in section 6) |
| `PID(...)` | A proportional-integral-derivative controller that steers `expr` toward a target: `PID(expression, target, low, high, K, Ti, Td, Tt, b)`. `low` and `high` bound the output, `K` scales (expression - target) to the control, `Ti` and `Td` are the integral and derivative time constants, `Tt` is the anti-windup time and `b` the set-point weighting (use 1.0 if unsure). All arguments after `target` are evaluated once, when the rule is read, so write them as numbers. `IPID` is the incremental form. Test carefully before use |

---

## 12. Worked examples

Illustrative rules; gate, channel and series names are examples.

### 12.1 A gate that follows a schedule all run

```
falseleakage_op := SET gate_op(gate=FalseBarrier,device=leakage,direction=from_node) TO ts(name=fb_pipeop) WHEN TRUE;
```

Trigger `TRUE`: starts once at the beginning. The gate then follows the series `fb_pipeop` for the rest of the run, overriding any other input for that gate coefficient.

### 12.2 A gate closed by a flow reversal (from the Montezuma Slough gates)

```
mscs_velclose := chan_vel(channel=512,dist=5750) < -0.1               (named expression)
mscs_calc     := ts(name=mscs_op) < 0                                  (named expression)
mscs_g3close  := (ts(name=mscs_op)) < 0.0001 AND (ts(name=mscs_op)) > -0.0001   (named expression)

mscs_close := SET gate_op(gate=montezuma_salinity_control,device=radial_gates,direction=from_node) TO 0.0
              WHEN (mscs_velclose AND mscs_calc) OR (mscs_g3close);
```

Each reversal of the velocity is a new false-to-true change, so the rule fires every tide cycle. A paired rule (`mscs_open`) opens the gate again on its own condition. The opposite direction of the same device is a separate rule (`mscs_close_to`), which waits for this one to finish because both write the same device (section 9).

### 12.3 A barrier installed in a season

```
barrier_in  := SET gate_install(gate=old_river_barrier) TO INSTALL WHEN SEASON > 15APR;
barrier_out := SET gate_install(gate=old_river_barrier) TO REMOVE  WHEN SEASON > 16MAY;
```

Seasonal literals work in every year. Each rule fires once a year, at the day the condition becomes true.

### 12.4 A boundary flow that depends on the season

```
vamp := (MONTH == APR) OR (MONTH == MAY)
sjr_vamp := SET ext_flow(name=sjr) TO IFELSE(vamp, ts(name=vamp_flow), ts(name=normal_flow)) WHEN TRUE;
```

One rule, no pair, no misfire.

### 12.5 A slow change of a gate dimension

```
weir_raise := SET gate_elev(gate=barrier,device=weir) TO 4.5 RAMP 120MIN WHEN DATE >= 01JUN1995;
```

The elevation moves to 4.5 ft over two hours on 01JUN1995. Any rule that wants to change another property of the same device (even `gate_op`) waits until the ramp ends.

---

## 13. Common mistakes and error messages

| Symptom | Cause and fix |
|---|---|
| "syntax error, unexpected NAME, expecting 't'" or an unexplained syntax error for a model name | Model names are case sensitive: write `gate_op`, not `GATE_OP` |
| A syntax error where an identifier value is `to`, `min`, `day`, `t`, `date` ... | These are reserved words and cannot be used as a name or an identifier value (full list below) |
| "Operating rule name ... used more than once" | Two names that are the same after conversion to lower case, or the same name twice in one layer |
| "unknown ..." for a named expression | It uses another expression whose name sorts after it. Rename so that the used expression sorts first |
| "Gate name not found", "Time series unknown ..." | The gate name is not in the gate input, or the series is not listed in `OPRULE_TIME_SERIES` |
| A rule never fires | Its trigger never changes from false to true (always false, always true from the start, or the series name is misspelled). The log shows `TRIGGER_INITIAL` with the inputs it saw |
| A rule fires again and again | The trigger flickers or the date test was written with `AND`; use `DATE`, `DATETIME` or `SEASON`, or hysteresis |
| A rule waits a long time | An overlapping rule on the same device is running a long ramp, or deferred rules form a chain; check the log |
| A date or season is wrong | Month name in lower or mixed case (read as 0); `SEASON <= 30APR` excludes most of 30APR |
| A syntax error in a rule with an empty trigger column | The trigger cannot be blank: write `TRUE` |
| "1e5" is rejected | An exponent needs a decimal point: `1.0e5` |
| `RAMP 1HOUR` rejected | Only minutes: `RAMP 60MIN` |
| The model stops at startup with exit code -3 | A parse error in some rule; the message above the exit names it |
| The model stops with "Flow direction not recognized" | A `gate_coef` with `direction=both`; use `to_node` or `from_node` |
| A rule with `THEN` at the top level stops the model | Write two rules |

Reserved words (cannot be used as names or identifier values): `abs sqrt exp log ln max2 min2 max3 min3 lookup false true startup ifelse or and not set to when while then ramp step accumulate predict linear quad pid ipid date datetime season year month day hour min dt` and the month names. A name that only starts with one of them (`tom_paine`, `minimum`) is fine.

---

## 14. Differences from the website guide

The page at cadwrdeltamodeling.github.io describes the language in general terms. Where it differs from the model as implemented:

| Website | In the model |
|---|---|
| `... WHERE (vamp AND ebb)` | The keyword is `WHEN` |
| `ts(new_time_series)` | `ts(name=new_time_series)` |
| `ext_flow(node=17)` | `ext_flow(name=...)`: the boundary flow's name |
| `chan_stage(chan=206, ...)` | The identifier is `channel` |
| `month == Apr` | Month names must be upper case: `MONTH == APR` |
| `chan_ec`, `chan_surf`, `gate_position` | Not available as rule variables; use `chan_stage` for the water surface and `gate_height` / `gate_elev` for position |
| `gate_nduplicate` listed as a static variable | It is a dynamic variable: a series that drives it keeps being followed after the rule finishes |
| `res_flow(res=..., node=...)` described as "flow from reservoir to node" | Same; `node` is the external node number |
| `PREDICT` recommended for triggers | Currently stops a debug build when used in a trigger; test first |
| `LOOKUP`, `ACCUMULATE`, `PID` | As described; `ACCUMULATE` does not multiply by `DT` |
| Lagged expressions (`name(t-1)`) | They parse but are not implemented: do not use |

---

## 15. Known limitations

These are real behaviours of the current model, listed so that they do not come as a surprise. The reference ([OPRULE_REFERENCE.md](OPRULE_REFERENCE.md), B9) has the full list with details.

- A rise of a trigger while the rule's own action is running is missed (section 6).
- Any two actions on one gate device conflict, whatever the property (section 9).
- A top-level `THEN` chain stops the model; `WHILE` of actions with different `RAMP` lengths stops a debug build.
- `PREDICT` in a trigger stops a debug build; lagged expressions are not implemented.
- `gate_coef` and `gate_install` are set once, never followed.
- `gate_coef` with `direction=both` stops the model.
- Month names in lower case, and the hour of a `SEASON == 01JAN 12:30` literal, are read wrongly.
- Unknown channel numbers and reservoir names are not reported cleanly and can give wrong values or a crash.
- Rule input errors stop the run: there is no "warning and continue".
