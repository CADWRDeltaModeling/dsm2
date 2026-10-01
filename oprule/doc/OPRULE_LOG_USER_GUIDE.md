# Operating rule log: user's guide

This guide explains, for someone who runs DSM2 and wants to know what the operating rules did, how to switch the rule log on, what the file `..._oprule_log.h5` contains, and how to answer questions with it. For the rule language itself see [OPRULE_USER_GUIDE.md](OPRULE_USER_GUIDE.md) (writing rules) and [OPRULE_REFERENCE.md](OPRULE_REFERENCE.md) Part A. For how the log is built and tested see the same file, B10, and [OPRULE_LOG_HDF5_PLAN.md](OPRULE_LOG_HDF5_PLAN.md).

## 1. What the log is for

An operating rule has a **trigger** (a true/false condition, for example "channel velocity is negative") and an **action** (for example "set the gate's op coefficient to 0"). The action starts when the trigger changes from false to true. Gates then change state, and the hydrodynamics respond.

The log answers two questions that the model output alone cannot:

- **What did the rules do, and why?** Which rule fired, at what time, with what input values, whether it had to wait for another rule, what it wrote.
- **What did each gate device do, and who made it do it?** Every change of a gate's operating state, height, elevation, width, number of duplicates or installation, with the rule that caused it (or none, if an input time series did).

Only **changes** are recorded. A rule whose trigger stays false for a month writes nothing for that month.

## 2. Turning it on

Add scalars to the SCALAR table of `hydro.inp` (the same table that holds `output_inst`):

```
SCALAR
NAME                VALUE
...
oprule_log_level    2
END
```

| Scalar | Values | Default | What it does |
|---|---|---|---|
| `oprule_log_level` | 0, 1, 2 | 0 (see note) | 0 writes no log file. 1 records rule events. 2 also records every step of every running action. |
| `oprule_log_file` | a short file name | `<tide file name>_oprule_log.h5` | A bare name is placed in the tide file's directory. The scalar reader accepts at most 32 characters. |
| `oprule_log_devices` | true, false | true | Record gate device transitions and states. False keeps only the rule events. |
| `oprule_log_context` | true, false | true | Add the water level on both sides of the gate and the gate flow to each device transition. |
| `oprule_log_tol_op` | number >= 0 | 0.001 | Smallest change of an op coefficient, caused by an input series, that is written. |
| `oprule_log_tol_dim` | number >= 0 | 0.01 | The same for height, elevation and width (feet). |
| `oprule_log_flush_hours` | number > 0 | 24 | How often (in simulated hours) the file is brought up to date on disk. |
| `oprule_log_text` | true, false | false | Also write a plain text copy, `oprule_log.txt`, for debugging. |
| `tidefile_gate_state` | off, end, mean, both | both | Whether gate device state series are written into the tide file (section 9). |

Note: if `oprule_log_level` is not set, the model's `print_level` decides: 4 gives level 1, 5 or more gives level 2, anything lower gives no log.

The log file is created when the first rule is read, in the same directory as the hydro tide file. For `output/hist_fc_mss.h5` that is `output/hist_fc_mss_oprule_log.h5`. A model without operating rules writes no log. Typical size: about 1 MB for four months of the historical study (87 rules).

The log does not change any model result. This was checked by comparing complete tide files, restart files and run logs with logging off and on.

## 3. How the file is organized

The file contains **tables**, all one-dimensional lists of rows. Think of them as spreadsheets that refer to each other by number.

```
 lookup tables                what the rules did                        what the gates did
 -------------                ------------------                        ------------------
 rules      (names, text)     events        (what happened)             device_transitions (each change)
 variables  (input names)     event_values  (the numbers behind them)   device_intervals   (states over time)
 gates, devices (names)       actions       (each step of an action)
                              episodes      (one attempt, start to end)
                              intervals     (stretches of time)
                              rule_inputs   (what each rule depends on)
```

General conventions:

- **Time** is a whole number of minutes. 1440 is 01JAN1900 00:00. The time of a row is the end of the model step in which it happened. The tool in section 7 converts it to a date.
- **IDs** are numbers that point into another table: `rule_id` into `rules`, `var_id` into `variables`, `gate_id` into `gates`, `device_id` into `devices`, `episode_id` into `episodes`, `event_id` into `events`. An ID of 0 means "none".
- **Timing of a rule.** The trigger is tested at the end of step n. The action's first effect is in step n+1. A trigger at 07:25 therefore shows up as a gate change at 07:30.
- **Gates and devices** are numbered in the order the model reads them from its input. Their names are in `gates` and `devices`. The gate install state belongs to the gate, not to a device, so its `device_id` is 0.
- A missing number is written as `NaN` (not a number).

## 4. The tables

### Lookup tables

**`rules`**: one row per rule or named expression.
`rule_id`, `name`, `kind` (0 rule, 1 named expression), `text` (the full text as read), `trigger_text`, `action_text`.

**`variables`**: a dictionary of every input a rule can see.
`var_id`, `label`, `kind`, `scope_rule_id`.
Labels look like `chan_vel(int_channel=484,dist=5750)`, `ts(name=mscs_op)`, `mscs_calc` (a named expression) or `accumulate.sum` (internal state of a rule). Kinds: 0 a value read from the model, 1 a named expression, 2 internal state of one rule (`scope_rule_id` says which), 3 the model quantity an action writes. Channels are shown by internal index and gates and devices by their Fortran index, not by name; `gates` and `devices` translate the latter.

**`gates`**: `gate_id`, `name`, `n_devices`, `node` (external node number), `object` (the channel number or reservoir it is attached to).

**`devices`**: `device_id` (counted over all gates), `gate_id`, `device_index` (position inside the gate), `name`, `structure_type` (1 weir, 2 pipe).

### What the rules did

**`events`**: one row each time something notable happens to a rule.

| Column | Meaning |
|---|---|
| `event_id` | Row number, starting at 0, in time order |
| `time`, `step` | When, and the step count since the start of the run |
| `rule_id` | Which rule |
| `event` | What happened (codes below) |
| `stage` | The rule's stage after the event: 0 idle, 1 waiting, 2 deferred, 3 active |
| `aux_rule_id` | The other rule involved: the one that blocked it, or replaced it |
| `episode_id` | The attempt this event belongs to (0 if none) |
| `value_start`, `value_count` | The slice of `event_values` holding the inputs seen at this event |

Event codes:

| Code | Name | Meaning |
|---|---|---|
| 1 | `TRIGGER_INITIAL` | The first test of the trigger, and it is false |
| 2 | `TRIGGERED` | The trigger became true (also a first test that is true) |
| 3 | `TRIGGER_CLEARED` | The trigger became false |
| 4 | `ACTIVATED` | The action starts |
| 5 | `DEFERRED` | The action must wait because another rule is using the same gate device (written once per wait) |
| 6 | `DEFER_ENDED` | The trigger went false while the rule was waiting, so it never started |
| 7 | `COMPLETED` | The action finished |
| 8 | `NOT_APPLICABLE` | Triggered, but the action does not apply in the current situation |
| 9 | `IGNORED` | Blocked and dropped (a conflict policy that the DSM2 rules do not use) |
| 10 | `REPLACED` | Stopped by another rule (likewise unused) |

**`event_values`**: `var_id`, `value`. The inputs behind events. For a `TRIGGERED` event these are the model values that made the trigger true: the named expressions first, then the quantities under them.

**`actions`** (level 2 only): one row for each step of a running action.
`time`, `step`, `rule_id`, `episode_id`, `interface_var_id` (what is being written), `elapsed` (seconds since the action started), `fraction` (how far along a ramp, 0 to 1), `base` (where the ramp started), `target` (where it is going; re-read every step), `value` (what was written), `init` (the value read when the action started; NaN when the quantity is read live), `duration`, and a slice of `event_values` with what the target reads.

**`episodes`**: one row per attempt, from the trigger rising to the outcome. This is the table that ties everything together.

| Column | Meaning |
|---|---|
| `episode_id` | Row number, starting at 1 |
| `rule_id`, `trigger_event_id` | The rule, and its `TRIGGERED` event (so the inputs that caused it) |
| `deferred_from`, `blocker_rule_id` | When it had to wait and for which rule (0 if it did not wait) |
| `activation_time`, `completion_time` | Start and end of the action (0 if it never started or never finished) |
| `interface_var_id`, `gate_id`, `device_index`, `property` | What it writes. `gate_id` and `device_index` are 0 if it is not a gate device property. For a rule with several actions, the first one |
| `start_value`, `end_value` | The value before and the target |
| `mode`, `duration` | 0 abrupt or 1 ramp, and the ramp length in seconds |
| `attached_source` | 1 if, when it finished, the rule left its target in place as a permanent data source (so the quantity keeps following it) |
| `outcome` | 0 completed, 1 replaced, 2 ignored, 3 not applicable, 4 wait ended without starting, 5 still open when the run ended |

**`intervals`**: stretches of time in which a rule was in some condition. `rule_id`, `kind` (0 trigger true, 1 deferred, 2 active), `start_time`, `end_time`, `start_event_id`, `end_event_id`, `aux_rule_id` (the blocker, for a deferral), `open_at_end` (1 if the run ended first). Use this table to draw a timeline.

**`rule_inputs`**: `rule_id`, `role` (0 trigger input, 1 action target input, 2 action state), `var_id`. Which variables each rule depends on. It is written when the run ends normally.

### What the gates did

**`device_transitions`**: one row each time a gate device property (or a gate's install state) changes.

| Column | Meaning |
|---|---|
| `transition_id` | Row number, starting at 0 |
| `time`, `step` | The step in which the new value is first used by the solver |
| `gate_id`, `device_id` | Which gate, which device (0 for the gate install state) |
| `property` | 1 op coefficient to node, 2 op coefficient from node, 3 height, 4 elevation, 5 width, 6 number of duplicates, 7 gate installed |
| `old_value`, `new_value` | The change. Op coefficients: 0 closed, 1 open. Installed: 1 installed, 0 removed |
| `target_value` | For a ramp, where it is heading |
| `kind` | 0 initial state, 1 set by a rule, 2 start of a ramp, 3 end of a ramp, 4 changed by a data source |
| `rule_id`, `episode_id` | The rule and attempt responsible (0 if an input series did it) |
| `source_var_id` | For kind 4, the series or expression that drives the value (an entry in `variables`) |
| `z_up`, `z_down`, `gate_flow` | Water level on the water body side and on the node side of the gate, and the flow through the gate, as the model had them from the previous solve. NaN if `oprule_log_context` is false |

What is and is not written:

- At the first step every property gets an `initial` row. After that, a change made by a rule is always written. A ramp is written at its start and its end, not at the steps between.
- A change made by a data source (an input series or an expression a finished rule attached) is written only if it is larger than the tolerance, measured from the last value written. A slow drift is therefore still caught once it has moved far enough.
- Number of duplicates and gate installation are written for any change.
- The gate install state changes only through a rule (`SET gate_install`); an input series does not change it during a run.

**`device_intervals`**: the same changes, as states over time. `device_id`, `gate_id`, `state_class`, `start_time`, `end_time`, `start_transition`, `end_transition`, `open_at_end`.
State classes for a device, from its two op coefficients: 0 closed (both 0), 1 open (both 1), 2 partial, 3 to-node only (open to node, closed from node), 4 from-node only. For a gate (`device_id` 0): 5 installed, 6 removed. Use this table to draw a gate timeline.

## 5. Worked example

Why did the Montezuma Slough radial gate close on 3 September 2014? The rule is `mscs_close`. This is what `oprule_log.py card <log> mscs_close -n 1` prints (lines wrapped here):

```
rule mscs_close
  mscs_close := SET gate_op(gate=montezuma_salinity_control,device=radial_gates,direction=from_node) TO 0.0
                WHEN (mscs_velclose AND mscs_calc) OR (mscs_g3close);
  episode 62: COMPLETED
    triggered 2014-09-03 07:25 with [mscs_velclose=1; chan_vel(int_channel=484,dist=5750)=-0.140688087;
                                     mscs_calc=1; ts(name=mscs_op)=-10; mscs_g3close=0]
    activated 2014-09-03 07:25, completed 2014-09-03 07:30: gate_op(gate=13,device=4,direction=from_node) from 1 to 0
    2014-09-03 07:30 montezuma_salinity_control/radial_gates op_from_node 1 -> 0
      (stage 4.24790223 / 4.24897778, gate flow -810.38125)
```

Reading it:

1. The velocity in channel 484 became negative (-0.14). That made the named expression `mscs_velclose` true, and with `mscs_calc` the whole trigger: a `TRIGGERED` event at the end of the 07:25 step, with these values in `event_values`.
2. The rule started in the same step (`ACTIVATED`). Its duration is 0, so it is abrupt and finishes in its first advance.
3. In the step ending 07:30 the solver used the closed coefficient. That is the row in `device_transitions` (kind 1, rule `mscs_close`, episode 62). The water level on the two sides was 4.248 and 4.249 ft and the flow through the gate was -810 cfs.
4. In `device_intervals` the device changes from open to "to-node only" at 07:30, and to closed once the rule for the other direction has closed it too.

## 6. Questions and where to look

| Question | Where |
|---|---|
| Why did gate G change at time T? | Find the row in `device_transitions` (gate, time). Its `episode_id` gives the episode; `trigger_event_id` gives the `TRIGGERED` event; the slice of `event_values` shows the inputs |
| Which rules fired most? | `events` with code 2 or 4, counted by `rule_id`. `oprule_log.py summary` does this |
| Which rules had to wait, and for whom? | `events` code 5, `aux_rule_id`; or `episodes` with `blocker_rule_id` not 0 |
| How long did a rule wait? | `episodes`: `activation_time` minus `deferred_from` |
| How long was a rule active? | `episodes`, or `intervals` kind 2 |
| When was a device closed (or a gate removed)? | `device_intervals`, class 0 (or 6) |
| Did a gate change with no rule involved? | `device_transitions` with `rule_id` 0: an input series did it, and `source_var_id` names it |
| Did a rule write something that did not change the gate? | `episodes` with `outcome` 0, but no row in `device_transitions` for that `episode_id` (the value was already there) |
| Did a rule never run although its trigger rose? | `episodes` with `outcome` 4 (it was waiting and the trigger went false) |

## 7. Reading the file

### The tool

`oprule/tools/oprule_log.py` needs Python with `h5py` (`python -m pip install --user h5py`).

| Command | What it does |
|---|---|
| `oprule_log.py summary <log>` | Row counts per table, and per rule the number of triggers, activations, deferrals and completions |
| `oprule_log.py card <log> <rule> [-n N]` | For one rule, each episode in plain words as in section 5. `-n` limits how many |
| `oprule_log.py dump <log>` | The events, one per line, like the text log: `time | EVENT | rule | detail` |
| `oprule_log.py check <log>` | Checks that the tables agree with each other; exit code 1 if not |
| `oprule_log.py gates <log> <tide.h5>` | Replays the device transitions and compares the result with the gate state series in the tide file |

### Your own analysis

Each table is a dataset that h5py reads as a numpy structured array. For example, all closings of the radial gate with the rule that caused them:

```python
import h5py, datetime

def when(minutes):                       # model time -> date
    return datetime.datetime(1899, 12, 31) + datetime.timedelta(minutes=int(minutes))

f = h5py.File('output/hist_fc_mss_oprule_log.h5', 'r')
gates = {int(g['gate_id']): g['name'].decode() for g in f['gates'][:]}
rules = {int(r['rule_id']): r['name'].decode() for r in f['rules'][:]}
gate = next(i for i, n in gates.items() if n == 'montezuma_salinity_control')

for t in f['device_transitions'][:]:
    if t['gate_id'] == gate and t['property'] == 2 and t['new_value'] == 0:
        print(when(t['time']), rules.get(int(t['rule_id']), '(input series)'), t['gate_flow'])
```

Text columns (`name`, `label`, `text`) come back as bytes; decode them as above. With pandas, `pandas.DataFrame(f['events'][:])` gives a table you can filter and join on the ID columns. `h5ls -r <file>` and `h5dump -d /events <file>` also work.

## 8. Things to keep in mind

- **A rule is not tested while it is active.** If its trigger changes during the action, the change is seen at the first test after the action ends, and the log records that later time.
- **A killed run** keeps everything up to the last flush (at most `oprule_log_flush_hours` simulated hours back). Intervals and rule inputs still open at that moment are written only when a run ends normally.
- **Several actions in one rule** (joined by `WHILE` or `THEN`): the episode names the first; every device transition still carries the episode.
- **Tolerances hide small drifts.** A device driven by an input series that moves by less than the tolerance between changes is not recorded each time. Set `oprule_log_tol_op` and `oprule_log_tol_dim` to 0 to record every change.
- **Names:** gates and devices are named in `gates` and `devices`; channels in input labels are internal indices and reservoirs appear in the `object` column of `gates`.
- **Not available yet:** a dense record of input values at every step (the scalar `oprule_log_trace_interval` is read but nothing is written) and a plotting tool.

## 9. Gate state in the tide file

Independent of the rule log, the hydro tide file can hold the gate device state at every tide interval (when `output_inst` is true). This is the quickest way to plot a gate next to flows and stages:

| Dataset (under `/hydro/data`) | Shape | Content |
|---|---|---|
| `device state op to node end`, `op from node`, `height`, `elevation`, `width`, `nduplicate` (each `... end` or `... mean`) | gate x device slot (10) x time | The value in the last step of the interval (`end`) or the average over the interval (`mean`; for an op coefficient it is the fraction of the time open) |
| `gate install end`, `gate install mean` | gate x time | 1 installed, 0 removed, or the fraction of the interval installed |

Device slots a gate does not use hold -901. The gate order is the same as in the log's `gates` table. If the tide interval equals the hydro time step (5 minutes in the historical study) `mean` would equal `end`, so with the default `tidefile_gate_state both` only the `end` series are written. Use `mean` to get the averages explicitly.

The two outputs complement each other: the tide file series are regular and easy to plot; the log tells you exactly when and why each change happened. `oprule_log.py gates` shows they agree.
