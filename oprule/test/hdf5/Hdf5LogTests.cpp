// Tests of the HDF5 sink of the rule log (oprule/lib/hdf5). The files are read back with the HDF5 C API through
// compound types that name each column, so a renamed or retyped column fails here.
// Plan ids: H5-01 (tables), H5-02 (equivalence with the in-memory sink), H5-04 (intervals), H5-06 (exit and kill),
// device intervals (part of H5-11).
#define BOOST_TEST_MODULE oprule_hdf5
#include <boost/test/included/unit_test.hpp>

#include <hdf5.h>

#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

#include <sys/stat.h>
#include <unistd.h>

#include "MockModel.h"
#include "oprule/rule/Hdf5LogSink.h"
#include "oprule/rule/RuleLog.h"

using namespace oprule_test;
using namespace oprule::rule;

namespace {

long g_julmin = 0;
long test_clock() { return g_julmin; }

std::string temp_path(const std::string& name) { return "oprule_hdf5_test_" + name + ".h5"; }

// ---- reading back

struct EventR { long event_id; int time; int step; int rule_id; signed char event; signed char stage; int aux_rule_id; long episode_id; long value_start; int value_count; };
struct ValueR { int var_id; double value; };
struct VarR { int var_id; char* label; signed char kind; int scope_rule_id; };
struct RuleR { int rule_id; char* name; signed char kind; char* text; char* trigger_text; char* action_text; };
struct ActionR { int time; int step; int rule_id; long episode_id; int interface_var_id; double elapsed; double fraction; double base; double target; double value; double init; double duration; long value_start; int value_count; };
struct IntervalR { int rule_id; signed char kind; int start_time; int end_time; long start_event_id; long end_event_id; int aux_rule_id; signed char open_at_end; };
struct EpisodeR { long episode_id; int rule_id; long trigger_event_id; int deferred_from; int blocker_rule_id; int activation_time; int completion_time; int interface_var_id; int gate_id; int device_index; signed char property; double start_value; double end_value; signed char mode; double duration; int n_actions; signed char attached_source; signed char outcome; };
struct TransitionR { long transition_id; int time; int step; int gate_id; int device_id; signed char property; double old_value; double new_value; double target_value; signed char kind; int rule_id; long episode_id; int source_var_id; double z_up; double z_down; double gate_flow; };
struct DevIntervalR { int device_id; int gate_id; signed char state_class; int start_time; int end_time; long start_transition; long end_transition; signed char open_at_end; };
struct GateR { int gate_id; char* name; int n_devices; char* node; char* object; };
struct DeviceR { int device_id; int gate_id; int device_index; char* name; signed char structure_type; };

hid_t str_type() { hid_t t = H5Tcopy(H5T_C_S1); H5Tset_size(t, H5T_VARIABLE); H5Tset_cset(t, H5T_CSET_UTF8); return t; }

#define ADD(T, f, type) H5Tinsert(t, #f, HOFFSET(T, f), type)

template<class R> hid_t mem_type();
template<> hid_t mem_type<EventR>() { hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(EventR)); ADD(EventR, event_id, H5T_NATIVE_LONG); ADD(EventR, time, H5T_NATIVE_INT); ADD(EventR, step, H5T_NATIVE_INT); ADD(EventR, rule_id, H5T_NATIVE_INT); ADD(EventR, event, H5T_NATIVE_SCHAR); ADD(EventR, stage, H5T_NATIVE_SCHAR); ADD(EventR, aux_rule_id, H5T_NATIVE_INT); ADD(EventR, episode_id, H5T_NATIVE_LONG); ADD(EventR, value_start, H5T_NATIVE_LONG); ADD(EventR, value_count, H5T_NATIVE_INT); return t; }
template<> hid_t mem_type<ValueR>() { hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(ValueR)); ADD(ValueR, var_id, H5T_NATIVE_INT); ADD(ValueR, value, H5T_NATIVE_DOUBLE); return t; }
template<> hid_t mem_type<VarR>() { hid_t s = str_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(VarR)); ADD(VarR, var_id, H5T_NATIVE_INT); ADD(VarR, label, s); ADD(VarR, kind, H5T_NATIVE_SCHAR); ADD(VarR, scope_rule_id, H5T_NATIVE_INT); H5Tclose(s); return t; }
template<> hid_t mem_type<RuleR>() { hid_t s = str_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(RuleR)); ADD(RuleR, rule_id, H5T_NATIVE_INT); ADD(RuleR, name, s); ADD(RuleR, kind, H5T_NATIVE_SCHAR); ADD(RuleR, text, s); ADD(RuleR, trigger_text, s); ADD(RuleR, action_text, s); H5Tclose(s); return t; }
template<> hid_t mem_type<ActionR>() { hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(ActionR)); ADD(ActionR, time, H5T_NATIVE_INT); ADD(ActionR, step, H5T_NATIVE_INT); ADD(ActionR, rule_id, H5T_NATIVE_INT); ADD(ActionR, episode_id, H5T_NATIVE_LONG); ADD(ActionR, interface_var_id, H5T_NATIVE_INT); ADD(ActionR, elapsed, H5T_NATIVE_DOUBLE); ADD(ActionR, fraction, H5T_NATIVE_DOUBLE); ADD(ActionR, base, H5T_NATIVE_DOUBLE); ADD(ActionR, target, H5T_NATIVE_DOUBLE); ADD(ActionR, value, H5T_NATIVE_DOUBLE); ADD(ActionR, init, H5T_NATIVE_DOUBLE); ADD(ActionR, duration, H5T_NATIVE_DOUBLE); ADD(ActionR, value_start, H5T_NATIVE_LONG); ADD(ActionR, value_count, H5T_NATIVE_INT); return t; }
template<> hid_t mem_type<IntervalR>() { hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(IntervalR)); ADD(IntervalR, rule_id, H5T_NATIVE_INT); ADD(IntervalR, kind, H5T_NATIVE_SCHAR); ADD(IntervalR, start_time, H5T_NATIVE_INT); ADD(IntervalR, end_time, H5T_NATIVE_INT); ADD(IntervalR, start_event_id, H5T_NATIVE_LONG); ADD(IntervalR, end_event_id, H5T_NATIVE_LONG); ADD(IntervalR, aux_rule_id, H5T_NATIVE_INT); ADD(IntervalR, open_at_end, H5T_NATIVE_SCHAR); return t; }
template<> hid_t mem_type<EpisodeR>() { hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(EpisodeR)); ADD(EpisodeR, episode_id, H5T_NATIVE_LONG); ADD(EpisodeR, rule_id, H5T_NATIVE_INT); ADD(EpisodeR, trigger_event_id, H5T_NATIVE_LONG); ADD(EpisodeR, deferred_from, H5T_NATIVE_INT); ADD(EpisodeR, blocker_rule_id, H5T_NATIVE_INT); ADD(EpisodeR, activation_time, H5T_NATIVE_INT); ADD(EpisodeR, completion_time, H5T_NATIVE_INT); ADD(EpisodeR, interface_var_id, H5T_NATIVE_INT); ADD(EpisodeR, gate_id, H5T_NATIVE_INT); ADD(EpisodeR, device_index, H5T_NATIVE_INT); ADD(EpisodeR, property, H5T_NATIVE_SCHAR); ADD(EpisodeR, start_value, H5T_NATIVE_DOUBLE); ADD(EpisodeR, end_value, H5T_NATIVE_DOUBLE); ADD(EpisodeR, mode, H5T_NATIVE_SCHAR); ADD(EpisodeR, duration, H5T_NATIVE_DOUBLE); ADD(EpisodeR, n_actions, H5T_NATIVE_INT); ADD(EpisodeR, attached_source, H5T_NATIVE_SCHAR); ADD(EpisodeR, outcome, H5T_NATIVE_SCHAR); return t; }
template<> hid_t mem_type<TransitionR>() { hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(TransitionR)); ADD(TransitionR, transition_id, H5T_NATIVE_LONG); ADD(TransitionR, time, H5T_NATIVE_INT); ADD(TransitionR, step, H5T_NATIVE_INT); ADD(TransitionR, gate_id, H5T_NATIVE_INT); ADD(TransitionR, device_id, H5T_NATIVE_INT); ADD(TransitionR, property, H5T_NATIVE_SCHAR); ADD(TransitionR, old_value, H5T_NATIVE_DOUBLE); ADD(TransitionR, new_value, H5T_NATIVE_DOUBLE); ADD(TransitionR, target_value, H5T_NATIVE_DOUBLE); ADD(TransitionR, kind, H5T_NATIVE_SCHAR); ADD(TransitionR, rule_id, H5T_NATIVE_INT); ADD(TransitionR, episode_id, H5T_NATIVE_LONG); ADD(TransitionR, source_var_id, H5T_NATIVE_INT); ADD(TransitionR, z_up, H5T_NATIVE_DOUBLE); ADD(TransitionR, z_down, H5T_NATIVE_DOUBLE); ADD(TransitionR, gate_flow, H5T_NATIVE_DOUBLE); return t; }
template<> hid_t mem_type<DevIntervalR>() { hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(DevIntervalR)); ADD(DevIntervalR, device_id, H5T_NATIVE_INT); ADD(DevIntervalR, gate_id, H5T_NATIVE_INT); ADD(DevIntervalR, state_class, H5T_NATIVE_SCHAR); ADD(DevIntervalR, start_time, H5T_NATIVE_INT); ADD(DevIntervalR, end_time, H5T_NATIVE_INT); ADD(DevIntervalR, start_transition, H5T_NATIVE_LONG); ADD(DevIntervalR, end_transition, H5T_NATIVE_LONG); ADD(DevIntervalR, open_at_end, H5T_NATIVE_SCHAR); return t; }
template<> hid_t mem_type<GateR>() { hid_t s = str_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(GateR)); ADD(GateR, gate_id, H5T_NATIVE_INT); ADD(GateR, name, s); ADD(GateR, n_devices, H5T_NATIVE_INT); ADD(GateR, node, s); ADD(GateR, object, s); H5Tclose(s); return t; }
template<> hid_t mem_type<DeviceR>() { hid_t s = str_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(DeviceR)); ADD(DeviceR, device_id, H5T_NATIVE_INT); ADD(DeviceR, gate_id, H5T_NATIVE_INT); ADD(DeviceR, device_index, H5T_NATIVE_INT); ADD(DeviceR, name, s); ADD(DeviceR, structure_type, H5T_NATIVE_SCHAR); H5Tclose(s); return t; }

struct H5File {
   explicit H5File(const std::string& path) : id(H5Fopen(path.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT)) {}
   ~H5File() { if (id >= 0) H5Fclose(id); }
   bool ok() const { return id >= 0; }
   bool has(const char* name) const { return H5Lexists(id, name, H5P_DEFAULT) > 0; }

   template<class R> std::vector<R> table(const char* name) const {
      std::vector<R> rows;
      hid_t d = H5Dopen2(id, name, H5P_DEFAULT);
      BOOST_REQUIRE_MESSAGE(d >= 0, std::string("missing table ") + name);
      hid_t s = H5Dget_space(d);
      hsize_t n = 0;
      H5Sget_simple_extent_dims(s, &n, NULL);
      rows.resize(n);
      if (n) {
         hid_t t = mem_type<R>();
         BOOST_REQUIRE_MESSAGE(H5Dread(d, t, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rows[0]) >= 0, std::string("cannot read ") + name);
         H5Treclaim(t, s, H5P_DEFAULT, &rows[0]);     // frees the strings only after the caller copied them: see strings()
         H5Tclose(t);
      }
      H5Sclose(s);
      H5Dclose(d);
      return rows;
   }

   int int_attr(const char* name) const {
      int v = -1;
      hid_t a = H5Aopen(id, name, H5P_DEFAULT);
      if (a >= 0) { H5Aread(a, H5T_NATIVE_INT, &v); H5Aclose(a); }
      return v;
   }
   hid_t id;
};

// Tables with strings are read into plain std::string copies (the library frees the originals).

std::vector<std::pair<int, std::string> > read_variables(const H5File& f, std::vector<int>* kinds = 0, std::vector<int>* scopes = 0) {
   std::vector<std::pair<int, std::string> > out;
   hid_t d = H5Dopen2(f.id, "/variables", H5P_DEFAULT);
   hid_t s = H5Dget_space(d);
   hsize_t n = 0;
   H5Sget_simple_extent_dims(s, &n, NULL);
   std::vector<VarR> rows(n);
   if (n) {
      hid_t t = mem_type<VarR>();
      H5Dread(d, t, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rows[0]);
      for (size_t i = 0; i < rows.size(); ++i) {
         out.push_back(std::make_pair(rows[i].var_id, std::string(rows[i].label)));
         if (kinds) kinds->push_back(rows[i].kind);
         if (scopes) scopes->push_back(rows[i].scope_rule_id);
      }
      H5Treclaim(t, s, H5P_DEFAULT, &rows[0]);
      H5Tclose(t);
   }
   H5Sclose(s);
   H5Dclose(d);
   return out;
}

struct RuleCopy { int id; std::string name, text, trigger, action; int kind; };
std::vector<RuleCopy> read_rules(const H5File& f) {
   std::vector<RuleCopy> out;
   hid_t d = H5Dopen2(f.id, "/rules", H5P_DEFAULT);
   hid_t s = H5Dget_space(d);
   hsize_t n = 0;
   H5Sget_simple_extent_dims(s, &n, NULL);
   std::vector<RuleR> rows(n);
   if (n) {
      hid_t t = mem_type<RuleR>();
      H5Dread(d, t, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rows[0]);
      for (size_t i = 0; i < rows.size(); ++i) {
         RuleCopy c = {rows[i].rule_id, rows[i].name, rows[i].text, rows[i].trigger_text, rows[i].action_text, rows[i].kind};
         out.push_back(c);
      }
      H5Treclaim(t, s, H5P_DEFAULT, &rows[0]);
      H5Tclose(t);
   }
   H5Sclose(s);
   H5Dclose(d);
   return out;
}

const char* const kTables[] = {"/rules", "/variables", "/rule_inputs", "/events", "/event_values", "/actions", "/intervals",
                               "/episodes", "/gates", "/devices", "/device_transitions", "/device_intervals"};

// A parser and runtime bench with an HDF5 sink and a memory sink listening to the same log.
struct Bench : ParserFixture, RuntimeFixture {
   explicit Bench(const std::string& name, int level = 2) : path(temp_path(name)) {
      g_julmin = 1440;
      sink = new Hdf5LogSink(path, level);
      BOOST_REQUIRE(sink->ok());
      RuleLog::setClock(&test_clock);
      RuleLog::addSink(&mem);
      RuleLog::addSink(sink);
      RuleLog::setLevel(level);
   }
   ~Bench() {
      RuleLog::setLevel(RuleLog::OFF);
      RuleLog::removeSink(sink);
      RuleLog::removeSink(&mem);
      delete sink;
      RuleLog::setClock(0);
      std::remove(path.c_str());
   }
   OperatingRulePtr add(const std::string& name, const std::string& action, const std::string& trigger) {
      int rc = parse(name + " := " + action + " WHEN " + trigger + ";");
      BOOST_REQUIRE_MESSAGE(rc == 0, "rule failed to parse: " + action);
      OperatingRulePtr r = getOperatingRule(name);
      manager.addRule(r);
      RuleLog::loaded(name, false, name + " := " + action + " WHEN " + trigger + ";");
      return r;
   }
   void run(int n) { for (int i = 0; i < n; ++i) { g_julmin += 15; step(); } }
   void close() { RuleLog::finish(); }
   std::string path;
   Hdf5LogSink* sink;
   MemoryLogSink mem;
};

// first ramps for 30 minutes, second waits for it (deferral); lvl toggles a third rule.
void scenario(Bench& b) {
   b.add("first", "SET mock_var(name=a) TO 1 RAMP 30MIN", "true");
   b.add("second", "SET mock_var(name=a) TO 5", "true");
   b.add("third", "SET mock_var(name=b) TO 2", "mock_ro(name=lvl) > 0");
   g_vars["lvl"] = 0.; b.run(1);
   g_vars["lvl"] = 1.; b.run(2);
   g_vars["lvl"] = 0.; b.run(2);
   g_vars["lvl"] = 1.; b.run(3);
}

}  // namespace

BOOST_AUTO_TEST_SUITE(hdf5_sink)

// H5-01
BOOST_AUTO_TEST_CASE(the_file_has_every_table_and_the_format_version) {
   Bench b("tables");
   b.run(1);
   b.close();
   H5File f(b.path);
   BOOST_REQUIRE(f.ok());
   for (size_t i = 0; i < sizeof(kTables) / sizeof(kTables[0]); ++i) BOOST_CHECK_MESSAGE(f.has(kTables[i]), kTables[i]);
   BOOST_CHECK_EQUAL(f.int_attr("format_version"), (int)Hdf5LogSink::FORMAT_VERSION);
   BOOST_CHECK_EQUAL(f.int_attr("time_epoch"), 1440);
   BOOST_CHECK_EQUAL(f.int_attr("log_level"), 2);
}

BOOST_AUTO_TEST_CASE(a_path_that_cannot_be_created_gives_a_sink_that_is_not_ok) {
   Hdf5LogSink bad("/no_such_directory_for_rule_log/x.h5", 1);
   BOOST_CHECK(!bad.ok());
   LogEvent e;
   e.code = LogEvent::TRIGGERED;
   bad.event(e);                 // must not crash
   bad.flush();
   bad.finish(0);
}

BOOST_AUTO_TEST_CASE(rules_are_stored_with_their_text_split_at_when) {
   Bench b("rules");
   b.add("r", "SET mock_var(name=a) TO 1 RAMP 30MIN", "mock_ro(name=lvl) > 0");
   RuleLog::loaded("e", true, "e := 3.0;");
   b.close();
   H5File f(b.path);
   std::vector<RuleCopy> rules = read_rules(f);
   BOOST_REQUIRE_EQUAL(rules.size(), 2u);
   BOOST_CHECK_EQUAL(rules[0].name, "r");
   BOOST_CHECK_EQUAL(rules[0].kind, 0);
   BOOST_CHECK_EQUAL(rules[0].action, "SET mock_var(name=a) TO 1 RAMP 30MIN");
   BOOST_CHECK_EQUAL(rules[0].trigger, "mock_ro(name=lvl) > 0");
   BOOST_CHECK_EQUAL(rules[1].name, "e");
   BOOST_CHECK_EQUAL(rules[1].kind, 1);
   BOOST_CHECK_EQUAL(rules[1].text, "e := 3.0;");
   BOOST_CHECK_EQUAL(rules[1].trigger, "");
}

// H5-02: every stage event in the file is the one the memory sink saw.
BOOST_AUTO_TEST_CASE(events_in_the_file_equal_those_of_the_memory_sink) {
   Bench b("events");
   scenario(b);
   b.close();
   H5File f(b.path);
   std::vector<EventR> rows = f.table<EventR>("/events");
   std::vector<ValueR> values = f.table<ValueR>("/event_values");
   std::vector<std::pair<int, std::string> > vars = read_variables(f);
   std::vector<const LogEvent*> want;
   for (size_t i = 0; i < b.mem.events.size(); ++i)
      if (b.mem.events[i].id >= 0) want.push_back(&b.mem.events[i]);
   BOOST_REQUIRE_EQUAL(rows.size(), want.size());
   BOOST_REQUIRE(!rows.empty());
   for (size_t i = 0; i < rows.size(); ++i) {
      const LogEvent& e = *want[i];
      BOOST_CHECK_EQUAL(rows[i].event_id, e.id);
      BOOST_CHECK_EQUAL(rows[i].event, e.code);
      BOOST_CHECK_EQUAL(rows[i].rule_id, e.ruleId);
      BOOST_CHECK_EQUAL(rows[i].aux_rule_id, e.auxRuleId);
      BOOST_CHECK_EQUAL(rows[i].stage, e.stage);
      BOOST_CHECK_EQUAL(rows[i].time, e.julmin);
      BOOST_CHECK_EQUAL(rows[i].step, e.step);
      BOOST_CHECK_EQUAL(rows[i].episode_id, e.episodeId);
      size_t expected_count = e.values.size();
      if (e.code == LogEvent::ACTIVATED) { expected_count = 0; for (size_t k = 0; k < e.actions.size(); ++k) expected_count += e.actions[k].inputs.size(); }
      BOOST_CHECK_EQUAL((size_t)rows[i].value_count, expected_count);
      if (e.code == LogEvent::TRIGGERED || e.code == LogEvent::TRIGGER_INITIAL || e.code == LogEvent::TRIGGER_CLEARED)
         for (size_t k = 0; k < e.values.size(); ++k) {
            const ValueR& v = values[rows[i].value_start + k];
            std::string label;
            for (size_t j = 0; j < vars.size(); ++j) if (vars[j].first == v.var_id) label = vars[j].second;
            BOOST_CHECK_EQUAL(label, e.values[k].first);
            BOOST_CHECK_EQUAL(v.value, e.values[k].second);
         }
   }
   // the rule ids in the file name the rules
   std::vector<RuleCopy> rules = read_rules(f);
   BOOST_REQUIRE_EQUAL(rules.size(), 3u);
   BOOST_CHECK_EQUAL(rules[0].id, 1);
   BOOST_CHECK_EQUAL(RuleLog::ruleName(rules[1].id), rules[1].name);
}

BOOST_AUTO_TEST_CASE(actions_in_the_file_equal_those_of_the_memory_sink) {
   Bench b("actions");
   scenario(b);
   b.close();
   H5File f(b.path);
   std::vector<ActionR> rows = f.table<ActionR>("/actions");
   std::vector<const LogEvent*> want;
   for (size_t i = 0; i < b.mem.events.size(); ++i)
      if (b.mem.events[i].code == LogEvent::ACTION) want.push_back(&b.mem.events[i]);
   BOOST_REQUIRE_EQUAL(rows.size(), want.size());
   BOOST_REQUIRE(!rows.empty());
   for (size_t i = 0; i < rows.size(); ++i) {
      const ActionInfo& a = want[i]->actions[0];
      BOOST_CHECK_EQUAL(rows[i].rule_id, want[i]->ruleId);
      BOOST_CHECK_EQUAL(rows[i].time, want[i]->julmin);
      BOOST_CHECK_EQUAL(rows[i].episode_id, want[i]->episodeId);
      BOOST_CHECK_EQUAL(rows[i].elapsed, a.elapsed);
      BOOST_CHECK_EQUAL(rows[i].fraction, a.fraction);
      BOOST_CHECK_EQUAL(rows[i].base, a.base);
      BOOST_CHECK_EQUAL(rows[i].target, a.target);
      BOOST_CHECK_EQUAL(rows[i].value, a.value);
      BOOST_CHECK_EQUAL(rows[i].duration, a.duration);
      if (a.dynamic) BOOST_CHECK(std::isnan(rows[i].init)); else BOOST_CHECK_EQUAL(rows[i].init, a.init);
   }
   std::vector<std::pair<int, std::string> > vars = read_variables(f);
   bool found = false;
   for (size_t i = 0; i < vars.size(); ++i) if (vars[i].second == "mock_var(name=a)") found = true;
   BOOST_CHECK(found);                                    // the interface is in the dictionary
}

// H5-04: intervals and episodes as the memory sink saw them.
BOOST_AUTO_TEST_CASE(intervals_and_episodes_in_the_file_equal_those_of_the_memory_sink) {
   Bench b("intervals");
   scenario(b);
   b.close();
   H5File f(b.path);
   std::vector<IntervalR> iv = f.table<IntervalR>("/intervals");
   BOOST_REQUIRE_EQUAL(iv.size(), b.mem.intervals.size());
   BOOST_REQUIRE(!iv.empty());
   bool any_open = false;
   for (size_t i = 0; i < iv.size(); ++i) {
      const RuleInterval& m = b.mem.intervals[i];
      BOOST_CHECK_EQUAL(iv[i].rule_id, m.ruleId);
      BOOST_CHECK_EQUAL(iv[i].kind, m.kind);
      BOOST_CHECK_EQUAL(iv[i].start_time, m.start);
      BOOST_CHECK_EQUAL(iv[i].end_time, m.end);
      BOOST_CHECK_EQUAL(iv[i].start_event_id, m.startEvent);
      BOOST_CHECK_EQUAL(iv[i].end_event_id, m.endEvent);
      BOOST_CHECK_EQUAL(iv[i].aux_rule_id, m.auxRuleId);
      BOOST_CHECK_EQUAL((bool)iv[i].open_at_end, m.openAtEnd);
      any_open = any_open || m.openAtEnd;
   }
   BOOST_CHECK(any_open);                                 // the run ended with the trigger of "first" still true
   std::vector<EpisodeR> ep = f.table<EpisodeR>("/episodes");
   BOOST_REQUIRE_EQUAL(ep.size(), b.mem.episodes.size());
   BOOST_REQUIRE(!ep.empty());
   for (size_t i = 0; i < ep.size(); ++i) {
      const Episode& m = b.mem.episodes[i];
      BOOST_CHECK_EQUAL(ep[i].episode_id, m.id);
      BOOST_CHECK_EQUAL(ep[i].rule_id, m.ruleId);
      BOOST_CHECK_EQUAL(ep[i].trigger_event_id, m.triggerEventId);
      BOOST_CHECK_EQUAL(ep[i].deferred_from, m.deferredFrom);
      BOOST_CHECK_EQUAL(ep[i].blocker_rule_id, m.blockerRuleId);
      BOOST_CHECK_EQUAL(ep[i].activation_time, m.activation);
      BOOST_CHECK_EQUAL(ep[i].completion_time, m.completion);
      BOOST_CHECK_EQUAL(ep[i].start_value, m.startValue);
      BOOST_CHECK_EQUAL(ep[i].end_value, m.endValue);
      BOOST_CHECK_EQUAL(ep[i].outcome, m.outcome);
      BOOST_CHECK_EQUAL((bool)ep[i].attached_source, m.attachedSource);
      BOOST_CHECK_EQUAL(ep[i].mode, m.duration > 0. ? 1 : 0);
   }
}

BOOST_AUTO_TEST_CASE(the_dictionary_lists_each_variable_once_with_its_kind_and_scope) {
   Bench b("variables");
   b.add("a", "SET mock_var(name=a) TO 1", "mock_ro(name=lvl) > 0 AND ACCUMULATE(1, 0) >= 3");
   b.add("b", "SET mock_var(name=b) TO 1", "mock_ro(name=lvl) > 0 AND ACCUMULATE(1, 0) >= 3");
   g_vars["lvl"] = 1.;
   b.run(6);
   b.close();
   H5File f(b.path);
   std::vector<int> kinds, scopes;
   std::vector<std::pair<int, std::string> > vars = read_variables(f, &kinds, &scopes);
   int lvl = 0, sums = 0;
   for (size_t i = 0; i < vars.size(); ++i) {
      BOOST_CHECK_EQUAL(vars[i].first, (int)i + 1);
      if (vars[i].second == "mock_ro(name=lvl)") { ++lvl; BOOST_CHECK_EQUAL(kinds[i], 0); BOOST_CHECK_EQUAL(scopes[i], 0); }
      if (vars[i].second == "accumulate.sum") { ++sums; BOOST_CHECK_EQUAL(kinds[i], 2); BOOST_CHECK(scopes[i] > 0); }
   }
   BOOST_CHECK_EQUAL(lvl, 1);       // shared by both rules, stored once
   BOOST_CHECK_EQUAL(sums, 2);      // the internal state belongs to one rule each
}

// ---- device transitions and their intervals

BOOST_AUTO_TEST_CASE(gate_tables_transitions_and_state_intervals) {
   Bench b("devices", 1);
   std::vector<GateInfo> gates(1);
   gates[0].id = 1; gates[0].name = "g1"; gates[0].nDevices = 2; gates[0].node = "5"; gates[0].object = "channel 185";
   std::vector<DeviceInfo> devices(2);
   devices[0].id = 1; devices[0].gate = 1; devices[0].index = 1; devices[0].name = "d1"; devices[0].structureType = 2;
   devices[1].id = 2; devices[1].gate = 1; devices[1].index = 2; devices[1].name = "d2"; devices[1].structureType = 1;
   RuleLog::gates(gates, devices);

   struct Row { int property; int device; double oldv, newv; int kind; long t; };
   const Row rows[] = {
      {PROP_OP_TO_NODE,   1, 1, 1, TRANSITION_INITIAL,  100},
      {PROP_OP_FROM_NODE, 1, 1, 1, TRANSITION_INITIAL,  100},
      {PROP_INSTALL,      0, 1, 1, TRANSITION_INITIAL,  100},
      {PROP_OP_TO_NODE,   1, 1, 0, TRANSITION_RULE_SET, 115},      // to node closed, from node open: from node only
      {PROP_OP_FROM_NODE, 1, 1, 0, TRANSITION_SOURCE,   130},      // both closed
      {PROP_HEIGHT,       2, 0, 4, TRANSITION_SOURCE,   132},      // does not change a state class
      {PROP_INSTALL,      0, 1, 0, TRANSITION_RULE_SET, 135},      // gate removed
   };
   for (size_t i = 0; i < sizeof(rows) / sizeof(rows[0]); ++i) {
      g_julmin = rows[i].t;
      DeviceTransition t;
      t.gate = 1; t.device = rows[i].device; t.property = rows[i].property;
      t.oldValue = rows[i].oldv; t.newValue = rows[i].newv; t.kind = rows[i].kind;
      t.source = rows[i].kind == TRANSITION_SOURCE ? "series:tide" : "";
      t.contextValid = i == 3; t.zUp = 2.5; t.zDown = 2.0; t.gateFlow = -7.;
      RuleLog::transition(t);
   }
   g_julmin = 140;
   b.close();

   H5File f(b.path);
   std::vector<TransitionR> tr = f.table<TransitionR>("/device_transitions");
   BOOST_REQUIRE_EQUAL(tr.size(), 7u);
   BOOST_CHECK_EQUAL(tr[3].transition_id, 3);
   BOOST_CHECK_EQUAL(tr[3].time, 115);
   BOOST_CHECK_EQUAL(tr[3].property, PROP_OP_TO_NODE);
   BOOST_CHECK_EQUAL(tr[3].device_id, 1);                 // flat id from the device table
   BOOST_CHECK_EQUAL(tr[5].device_id, 2);
   BOOST_CHECK_EQUAL(tr[6].device_id, 0);                 // the gate install has no device
   BOOST_CHECK_EQUAL(tr[3].kind, TRANSITION_RULE_SET);
   BOOST_CHECK_EQUAL(tr[3].z_up, 2.5);
   BOOST_CHECK_EQUAL(tr[3].gate_flow, -7.);
   BOOST_CHECK(std::isnan(tr[0].z_up));                   // no context recorded
   BOOST_CHECK(tr[4].source_var_id > 0);
   std::vector<std::pair<int, std::string> > vars = read_variables(f);
   BOOST_CHECK_EQUAL(vars[tr[4].source_var_id - 1].second, "series:tide");
   BOOST_CHECK_EQUAL(tr[4].source_var_id, tr[5].source_var_id);   // one dictionary entry

   std::vector<DevIntervalR> di = f.table<DevIntervalR>("/device_intervals");
   // device 1 (op coefficients): open 100-115, from node only 115-130, closed 130-140 (open at the end);
   // gate install: installed 100-135, removed 135-140 (open at the end)
   BOOST_REQUIRE_EQUAL(di.size(), 5u);
   struct Want { int cls; int start, end; bool open; int device; };
   std::vector<Want> got;
   for (size_t i = 0; i < di.size(); ++i) { Want w = {di[i].state_class, di[i].start_time, di[i].end_time, di[i].open_at_end != 0, di[i].device_id}; got.push_back(w); }
   const Want expected[] = {{1, 100, 115, false, 1}, {5, 100, 135, false, 0}, {4, 115, 130, false, 1}, {0, 130, 140, true, 1}, {6, 135, 140, true, 0}};
   // intervals are written when they close, so compare as sets of rows
   for (size_t k = 0; k < 5; ++k) {
      bool found = false;
      for (size_t i = 0; i < got.size(); ++i)
         if (got[i].cls == expected[k].cls && got[i].start == expected[k].start && got[i].end == expected[k].end &&
             got[i].open == expected[k].open && got[i].device == expected[k].device) found = true;
      BOOST_CHECK_MESSAGE(found, "interval of class " << expected[k].cls << " from " << expected[k].start);
   }
   BOOST_CHECK_EQUAL(di[0].start_transition, 1);          // opened by the second initial row (both coefficients are known)
}

// The gate and device tables come through with their names.
BOOST_AUTO_TEST_CASE(gate_and_device_tables_carry_names) {
   Bench b("gatetables", 1);
   std::vector<GateInfo> gates(1);
   gates[0].id = 1; gates[0].name = "montezuma"; gates[0].nDevices = 1; gates[0].node = "77"; gates[0].object = "channel 5";
   std::vector<DeviceInfo> devices(1);
   devices[0].id = 1; devices[0].gate = 1; devices[0].index = 1; devices[0].name = "radial"; devices[0].structureType = 1;
   RuleLog::gates(gates, devices);
   b.close();
   H5File f(b.path);
   hid_t d = H5Dopen2(f.id, "/gates", H5P_DEFAULT);
   hid_t s = H5Dget_space(d);
   std::vector<GateR> g(1);
   hid_t t = mem_type<GateR>();
   BOOST_REQUIRE(H5Dread(d, t, H5S_ALL, H5S_ALL, H5P_DEFAULT, &g[0]) >= 0);
   BOOST_CHECK_EQUAL(std::string(g[0].name), "montezuma");
   BOOST_CHECK_EQUAL(std::string(g[0].node), "77");
   BOOST_CHECK_EQUAL(std::string(g[0].object), "channel 5");
   BOOST_CHECK_EQUAL(g[0].n_devices, 1);
   H5Treclaim(t, s, H5P_DEFAULT, &g[0]);
   H5Tclose(t); H5Sclose(s); H5Dclose(d);
}

// ---- H5-06: what is left when the process ends

namespace {
std::string g_child_path;

void log_some(int events) {
   RuleLog::setClock(&test_clock);
   Hdf5LogSink* sink = new Hdf5LogSink(g_child_path, 1);
   RuleLog::addSink(sink);
   RuleLog::setLevel(1);
   g_julmin = 1500;
   for (int i = 0; i < events; ++i) {
      RuleLog::event(i % 2 ? LogEvent::TRIGGER_CLEARED : LogEvent::TRIGGERED, "r", "");
      g_julmin += 15;
   }
}

void exit_without_finish() { log_some(5); std::exit(3); }                    // the exit hook closes the log
void killed_after_flush() { log_some(3); RuleLog::flush(); RuleLog::event(LogEvent::TRIGGERED, "r", ""); _exit(9); }
}  // namespace

BOOST_AUTO_TEST_CASE(a_run_that_exits_without_finishing_leaves_a_complete_file) {
   g_child_path = temp_path("exit");
   std::remove(g_child_path.c_str());
   BOOST_CHECK_EQUAL(run_in_child(&exit_without_finish), 3);
   H5File f(g_child_path);
   BOOST_REQUIRE(f.ok());
   BOOST_CHECK_EQUAL(f.table<EventR>("/events").size(), 5u);
   std::remove(g_child_path.c_str());
}

BOOST_AUTO_TEST_CASE(a_killed_run_keeps_what_was_flushed) {
   g_child_path = temp_path("kill");
   std::remove(g_child_path.c_str());
   BOOST_CHECK_EQUAL(run_in_child(&killed_after_flush), 9);
   H5File f(g_child_path);
   BOOST_REQUIRE_MESSAGE(f.ok(), "the file written by a killed run cannot be opened");
   BOOST_CHECK_EQUAL(f.table<EventR>("/events").size(), 3u);        // the event after the flush was lost
   BOOST_CHECK(f.has("/rules"));
   std::remove(g_child_path.c_str());
}

BOOST_AUTO_TEST_SUITE_END()
