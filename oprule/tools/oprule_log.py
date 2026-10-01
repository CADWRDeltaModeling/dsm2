#!/usr/bin/env python3
"""Reader and checks for the operating rule log written by DSM2 hydro (oprule_log.h5).

Format: oprule/doc/OPRULE_LOG_HDF5_PLAN.md section 3. Needs h5py and numpy.

  oprule_log.py summary <log.h5>                  counts per table and per rule
  oprule_log.py dump    <log.h5>                  the events as text, one record per line (like the old text log)
  oprule_log.py card    <log.h5> <rule> [-n N]    for a rule: each episode with the inputs that caused it and the
                                                  device changes it made
  oprule_log.py check   <log.h5>                  consistency of the tables (exit code 1 on a problem)
  oprule_log.py gates   <log.h5> <tide.h5>        replay the device transitions and compare them with the gate
                                                  state series in the tide file (exit code 1 on a mismatch)
"""
import argparse
import datetime
import math
import re
import sys

import h5py
import numpy as np

EVENT_NAMES = {1: 'TRIGGER_INITIAL', 2: 'TRIGGERED', 3: 'TRIGGER_CLEARED', 4: 'ACTIVATED', 5: 'DEFERRED',
               6: 'DEFER_ENDED', 7: 'COMPLETED', 8: 'NOT_APPLICABLE', 9: 'IGNORED', 10: 'REPLACED'}
STAGE_NAMES = {0: 'IDLE', 1: 'WAITING', 2: 'DEFERRED', 3: 'ACTIVE'}
OUTCOME_NAMES = {0: 'COMPLETED', 1: 'REPLACED', 2: 'IGNORED', 3: 'NOT_APPLICABLE', 4: 'DEFER_ENDED', 5: 'OPEN_AT_END'}
PROPERTY_NAMES = {1: 'op_to_node', 2: 'op_from_node', 3: 'height', 4: 'elevation', 5: 'width', 6: 'nduplicate',
                  7: 'install'}
KIND_NAMES = {0: 'initial', 1: 'rule_set', 2: 'ramp_start', 3: 'ramp_end', 4: 'source_change'}
CLASS_NAMES = {0: 'closed', 1: 'open', 2: 'partial', 3: 'to_node_only', 4: 'from_node_only', 5: 'installed',
               6: 'removed'}
INTERVAL_NAMES = {0: 'trigger_true', 1: 'deferred', 2: 'active'}

# model time: julian minutes, 01JAN1900 00:00 is 1440
_EPOCH = datetime.datetime(1899, 12, 31)


def julmin_to_datetime(julmin):
    return _EPOCH + datetime.timedelta(minutes=int(julmin))


def datetime_to_julmin(dt):
    return int(round((dt - _EPOCH).total_seconds() / 60.0))


def label(julmin):
    return julmin_to_datetime(julmin).strftime('%Y-%m-%d %H:%M')


def number(value):
    """A number the way the text log writes it: 9 significant digits."""
    if math.isnan(value):
        return 'nan'
    if math.isinf(value):
        return 'unset'
    return '%.9g' % value


def _text(value):
    return value.decode('utf-8') if isinstance(value, bytes) else str(value)


class Log:
    """The tables of a log file, read into numpy arrays and dictionaries."""

    def __init__(self, path):
        self.path = path
        with h5py.File(path, 'r') as f:
            self.version = int(f.attrs['format_version'])
            self.level = int(f.attrs['log_level'])
            self.events = f['events'][:]
            self.values = f['event_values'][:]
            self.actions = f['actions'][:]
            self.intervals = f['intervals'][:]
            self.episodes = f['episodes'][:]
            self.transitions = f['device_transitions'][:]
            self.dev_intervals = f['device_intervals'][:]
            self.rule_inputs = f['rule_inputs'][:]
            self.rules = {int(r['rule_id']): r for r in f['rules'][:]}
            self.variables = {int(v['var_id']): v for v in f['variables'][:]}
            self.gates = {int(g['gate_id']): g for g in f['gates'][:]}
            self.devices = {int(d['device_id']): d for d in f['devices'][:]}
        self.rule_names = {i: _text(r['name']) for i, r in self.rules.items()}
        self.var_labels = {i: _text(v['label']) for i, v in self.variables.items()}

    def rule(self, rule_id):
        return self.rule_names.get(int(rule_id), '') if rule_id else ''

    def inputs(self, start, count):
        """Named values of a slice of event_values."""
        return [(self.var_labels[int(v['var_id'])], float(v['value'])) for v in self.values[start:start + count]]

    def rule_id(self, name):
        for i, n in self.rule_names.items():
            if n == name:
                return i
        raise KeyError('no rule named %s' % name)

    def device_label(self, gate_id, device_id):
        gate = _text(self.gates[int(gate_id)]['name']) if int(gate_id) in self.gates else 'gate%d' % gate_id
        if device_id and int(device_id) in self.devices:
            return '%s/%s' % (gate, _text(self.devices[int(device_id)]['name']))
        return gate


def state_text(items):
    out, seen = [], set()
    for name, value in items:
        if (name, value) in seen:
            continue
        seen.add((name, value))
        out.append('%s=%s' % (name, number(value)))
    return '[' + '; '.join(out) + ']'


def dump_lines(log):
    """Records as the text log wrote them, as far as the tables allow: the details of ACTIVATED (mode and initial
    value) are not in the tables, the interface and duration come from the episode."""
    episodes = {int(e['episode_id']): e for e in log.episodes}
    lines = []
    for name, rid in ((log.rule_names[i], i) for i in sorted(log.rules)):
        rule = log.rules[rid]
        lines.append((0, 0, 'init | %s | %s | text=%s' % (
            'EXPRESSION_LOADED' if rule['kind'] == 1 else 'RULE_LOADED', name, _text(rule['text']))))
    for e in log.events:
        code = int(e['event'])
        name = EVENT_NAMES[code]
        rule = log.rule(e['rule_id'])
        if code in (1, 2, 3):
            detail = 'trigger_inputs=' + state_text(log.inputs(int(e['value_start']), int(e['value_count'])))
        elif code in (5, 9):
            detail = 'blocked_by=' + log.rule(e['aux_rule_id'])
        elif code == 10:
            detail = 'replaced_by=' + log.rule(e['aux_rule_id'])
        elif code == 6:
            detail = 'trigger went false while deferred'
        elif code == 8:
            detail = 'action not applicable in current context'
        elif code == 4:
            ep = episodes.get(int(e['episode_id']))
            detail = ''
            if ep is not None:
                detail = 'interface=%s duration=%s' % (
                    log.var_labels.get(int(ep['interface_var_id']), ''), number(float(ep['duration'])))
        else:
            detail = ''
        lines.append((int(e['time']), int(e['step']) * 2 + 1, '%s | %s | %s | %s' % (label(e['time']), name, rule, detail)))
    for a in log.actions:
        lines.append((int(a['time']), int(a['step']) * 2 + 1, '%s | ACTION | %s | interface=%s' % (
            label(a['time']), log.rule(a['rule_id']), log.var_labels.get(int(a['interface_var_id']), ''))
            + ' elapsed=%s fraction=%s base=%s target=%s value=%s duration=%s init=%s target_inputs=%s' % (
                number(a['elapsed']), number(a['fraction']), number(a['base']), number(a['target']),
                number(a['value']), number(a['duration']),
                'live' if math.isnan(a['init']) else number(a['init']),
                state_text(log.inputs(int(a['value_start']), int(a['value_count']))))))
    return lines


def cmd_summary(log, args):
    print('%s: format %d, level %d' % (log.path, log.version, log.level))
    for name in ('events', 'values', 'actions', 'intervals', 'episodes', 'transitions', 'dev_intervals', 'rule_inputs'):
        print('  %-14s %d rows' % (name, len(getattr(log, name))))
    print('  gates %d, devices %d, rules %d, variables %d' % (len(log.gates), len(log.devices), len(log.rules),
                                                          len(log.variables)))
    counts = {}
    for e in log.events:
        key = (log.rule(e['rule_id']), EVENT_NAMES[int(e['event'])])
        counts[key] = counts.get(key, 0) + 1
    rules = sorted(set(k[0] for k in counts))
    print('  %-28s %6s %6s %6s %6s %6s' % ('rule', 'trig', 'activ', 'defer', 'compl', 'ignor'))
    for r in rules:
        print('  %-28s %6d %6d %6d %6d %6d' % (r, counts.get((r, 'TRIGGERED'), 0), counts.get((r, 'ACTIVATED'), 0),
                                              counts.get((r, 'DEFERRED'), 0), counts.get((r, 'COMPLETED'), 0),
                                              counts.get((r, 'IGNORED'), 0)))
    return 0


def cmd_dump(log, args):
    rows = sorted(dump_lines(log), key=lambda r: (r[0], r[1]))
    # loaded records first (time 0), then in time order; a stable order inside a step
    for _, _, line in rows:
        print(line)
    return 0


def cmd_card(log, args):
    rid = log.rule_id(args.rule)
    rule = log.rules[rid]
    print('rule %s' % args.rule)
    print('  %s' % _text(rule['text']))
    episodes = [e for e in log.episodes if int(e['rule_id']) == rid]
    print('  %d episodes' % len(episodes))
    shown = 0
    for ep in episodes:
        if args.n and shown >= args.n:
            break
        shown += 1
        trig = log.events[int(ep['trigger_event_id'])] if int(ep['trigger_event_id']) >= 0 else None
        print('  episode %d: %s' % (int(ep['episode_id']), OUTCOME_NAMES[int(ep['outcome'])]))
        if trig is not None:
            print('    triggered %s with %s' % (label(trig['time']), state_text(log.inputs(int(trig['value_start']),
                                                                                       int(trig['value_count'])))))
        if int(ep['blocker_rule_id']):
            print('    deferred from %s by %s' % (label(ep['deferred_from']), log.rule(ep['blocker_rule_id'])))
        if int(ep['activation_time']):
            print('    activated %s, completed %s: %s from %s to %s%s' % (
                label(ep['activation_time']), label(ep['completion_time']) if int(ep['completion_time']) else '-',
                log.var_labels.get(int(ep['interface_var_id']), ''), number(float(ep['start_value'])),
                number(float(ep['end_value'])),
                ' (ramp %s s)' % number(float(ep['duration'])) if int(ep['mode']) else ''))
        for t in log.transitions:
            if int(t['episode_id']) == int(ep['episode_id']):
                print('    %s %s %s %s -> %s%s' % (
                    label(t['time']), log.device_label(t['gate_id'], t['device_id']), PROPERTY_NAMES[int(t['property'])],
                    number(float(t['old_value'])), number(float(t['new_value'])),
                    '' if math.isnan(t['z_up']) else '  (stage %s / %s, gate flow %s)' % (
                        number(float(t['z_up'])), number(float(t['z_down'])), number(float(t['gate_flow'])))))
    return 0


def cmd_check(log, args):
    problems = []

    def fail(message):
        problems.append(message)

    ids = [int(e['event_id']) for e in log.events]
    if ids != list(range(len(ids))):
        fail('event ids are not 0..n-1 in order')
    times = [int(e['time']) for e in log.events]
    if times != sorted(times):
        fail('events are not in time order')
    for e in log.events:
        if int(e['rule_id']) not in log.rules:
            fail('event %d names an unknown rule' % int(e['event_id']))
        if int(e['value_start']) + int(e['value_count']) > len(log.values):
            fail('event %d reads past the value table' % int(e['event_id']))
    # per rule, the stage after each event follows the stage diagram
    stage = {}
    for e in log.events:
        r, code, after = int(e['rule_id']), int(e['event']), int(e['stage'])
        before = stage.get(r, 0)
        allowed = {1: [0], 2: [0, 1], 3: [1, 2, 0, 3], 4: [1, 2], 5: [1], 6: [0, 1, 2], 7: [3], 8: [1], 9: [1], 10: [3]}
        if code in (4,) and after != 3:
            fail('rule %s: stage after ACTIVATED is %d' % (log.rule(r), after))
        if code == 7 and after not in (0, 1):
            fail('rule %s: stage after COMPLETED is %d' % (log.rule(r), after))
        if code == 2 and after != 1:
            fail('rule %s: stage after TRIGGERED is %d' % (log.rule(r), after))
        stage[r] = after
    # trigger values alternate: false->true->false (an initial false counts as false)
    last = {}
    for e in log.events:
        r, code = int(e['rule_id']), int(e['event'])
        if code in (1, 2, 3):
            value = code == 2
            if code != 1 and last.get(r, False) == value:
                fail('rule %s: trigger %s twice in a row at %s' % (log.rule(r), 'rose' if value else 'fell', label(e['time'])))
            last[r] = value
    # activation and completion pair up per rule
    active = {}
    for e in log.events:
        r, code = int(e['rule_id']), int(e['event'])
        if code == 4:
            if active.get(r):
                fail('rule %s activated twice without completing' % log.rule(r))
            active[r] = True
        elif code in (7, 10):
            if not active.get(r):
                fail('rule %s completed or replaced while not active' % log.rule(r))
            active[r] = False
    for iv in log.intervals:
        if int(iv['end_time']) < int(iv['start_time']):
            fail('interval of %s ends before it starts' % log.rule(iv['rule_id']))
    for ep in log.episodes:
        trig = int(ep['trigger_event_id'])
        if trig >= 0:
            e = log.events[trig]
            if int(e['rule_id']) != int(ep['rule_id']) or int(e['event']) != 2:
                fail('episode %d does not start at a TRIGGERED event of its rule' % int(ep['episode_id']))
        if int(ep['activation_time']) and int(ep['activation_time']) < (int(log.events[trig]['time']) if trig >= 0 else 0):
            fail('episode %d activates before it is triggered' % int(ep['episode_id']))
        if int(ep['outcome']) == 0 and int(ep['completion_time']) < int(ep['activation_time']):
            fail('episode %d completes before it activates' % int(ep['episode_id']))
    episode_ids = set(int(e['episode_id']) for e in log.episodes)
    for t in log.transitions:
        if int(t['episode_id']) and int(t['episode_id']) not in episode_ids:
            # an episode still open at the end is written at finish, so every id must be there
            fail('transition %d names an unknown episode' % int(t['transition_id']))
        if int(t['rule_id']) and int(t['rule_id']) not in log.rules:
            fail('transition %d names an unknown rule' % int(t['transition_id']))
        if int(t['kind']) in (1, 2, 3) and not int(t['rule_id']):
            fail('transition %d is a rule write without a rule' % int(t['transition_id']))
    # device intervals of one device do not overlap
    per_device = {}
    for d in log.dev_intervals:
        per_device.setdefault((int(d['gate_id']), int(d['device_id']), int(d['state_class']) >= 5), []).append(
            (int(d['start_time']), int(d['end_time'])))
    for key, spans in per_device.items():
        spans.sort()
        for (s1, e1), (s2, e2) in zip(spans, spans[1:]):
            if s2 < e1:
                fail('device intervals overlap for gate %d device %d' % key[:2])
    print('%s: %d events, %d episodes, %d transitions checked' % (log.path, len(log.events), len(log.episodes),
                                                                  len(log.transitions)))
    for p in problems[:30]:
        print('  PROBLEM: %s' % p)
    if len(problems) > 30:
        print('  ... %d more' % (len(problems) - 30))
    print('  %s' % ('OK' if not problems else '%d problems' % len(problems)))
    return 1 if problems else 0


_INTERVAL = re.compile(r'(\d+)\s*MIN', re.I)


def tide_times(tide, name):
    """Model time (julian minutes) of each record of a tide file dataset."""
    ds = tide['/hydro/data/' + name]
    start = _text(ds.attrs['start_time'][0] if hasattr(ds.attrs['start_time'], '__len__') else ds.attrs['start_time'])
    interval = _text(ds.attrs['interval'][0] if hasattr(ds.attrs['interval'], '__len__') else ds.attrs['interval'])
    minutes = int(_INTERVAL.search(interval).group(1))
    t0 = datetime_to_julmin(datetime.datetime.strptime(start.strip(), '%Y-%m-%d %H:%M:%S'))
    return t0, minutes, ds.shape[0]


def cmd_gates(log, args):
    """H5-14: the device transitions reproduce the end-of-interval series in the tide file."""
    tolerance = {1: 0.001, 2: 0.001, 3: 0.01, 4: 0.01, 5: 0.01, 6: 0.0, 7: 0.0}
    bad = 0
    compared = 0
    with h5py.File(args.tide, 'r') as tide:
        for prop in range(1, 8):
            name = ('gate install end' if prop == 7 else 'device state %s end' % PROPERTY_NAMES[prop].replace('_', ' '))
            if '/hydro/data/' + name not in tide:
                print('  %s: not in the tide file, skipped' % name)
                continue
            t0, minutes, n = tide_times(tide, name)
            data = tide['/hydro/data/' + name][:]            # (time, device, gate) or (time, gate)
            rows = [t for t in log.transitions if int(t['property']) == prop]
            by_device = {}
            for t in rows:
                by_device.setdefault((int(t['gate_id']), int(t['device_id']) and int(log.devices[int(t['device_id'])]['device_index'])), []).append(t)
            for (gate, dev), seq in by_device.items():
                seq.sort(key=lambda t: (int(t['time']), int(t['transition_id'])))
                times = np.array([int(t['time']) for t in seq])
                newv = np.array([float(t['new_value']) for t in seq])
                kinds = [int(t['kind']) for t in seq]
                # intervals in which a ramp is under way are not reproduced (only its start and end are logged)
                ramp = []
                start = None
                for t in seq:
                    if int(t['kind']) == 2:
                        start = int(t['time'])
                    elif int(t['kind']) == 3 and start is not None:
                        ramp.append((start, int(t['time'])))
                        start = None
                if start is not None:
                    ramp.append((start, 1 << 60))
                for i in range(1, n):
                    julmin = t0 + i * minutes
                    k = np.searchsorted(times, julmin, side='right') - 1
                    if k < 0:
                        continue
                    if any(a <= julmin < b for a, b in ramp):
                        continue
                    if prop == 7:
                        got = float(data[i, gate - 1])
                    else:
                        got = float(data[i, dev - 1, gate - 1])
                    want = newv[k]
                    compared += 1
                    if abs(got - want) > tolerance[prop] + 1e-5 * max(1.0, abs(want)):
                        bad += 1
                        if bad <= 10:
                            print('  MISMATCH %s gate %d device %d at %s: tide %s, transitions %s' % (
                                name, gate, dev, label(julmin), number(got), number(want)))
    print('%s: compared %d values with %s: %d mismatches' % (log.path, compared, args.tide, bad))
    return 1 if bad or compared == 0 else 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest='command', required=True)
    for name in ('summary', 'dump', 'check'):
        p = sub.add_parser(name)
        p.add_argument('log')
    p = sub.add_parser('card')
    p.add_argument('log')
    p.add_argument('rule')
    p.add_argument('-n', type=int, default=0, help='show only the first N episodes')
    p = sub.add_parser('gates')
    p.add_argument('log')
    p.add_argument('tide')
    args = parser.parse_args(argv)
    log = Log(args.log)
    return {'summary': cmd_summary, 'dump': cmd_dump, 'card': cmd_card, 'check': cmd_check,
            'gates': cmd_gates}[args.command](log, args)


if __name__ == '__main__':
    sys.exit(main())
