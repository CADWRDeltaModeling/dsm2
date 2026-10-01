// HDF5 sink of the operating rule log. Layout and meaning of the tables: OPRULE_LOG_HDF5_PLAN.md section 3.
#include "oprule/rule/Hdf5LogSink.h"
#include "oprule/rule/RuleLog.h"

#include <hdf5.h>

#include <cmath>
#include <cctype>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <deque>

namespace oprule {
namespace rule {
namespace hdf5_log {

namespace {

const hsize_t CHUNK_ROWS = 4096;
const size_t FLUSH_ROWS = 4096;

// ---- row layouts (the compound types below mirror them member by member)

struct RuleRow { int rule_id; const char* name; signed char kind; const char* text; const char* trigger_text; const char* action_text; };
struct VarRow { int var_id; const char* label; signed char kind; int scope_rule_id; };
struct RuleInputRow { int rule_id; signed char role; int var_id; };
struct EventRow {
   long event_id; int time; int step; int rule_id; signed char event; signed char stage; int aux_rule_id;
   long episode_id; long value_start; int value_count;
};
struct ValueRow { int var_id; double value; };
struct ActionRow {
   int time; int step; int rule_id; long episode_id; int interface_var_id;
   double elapsed; double fraction; double base; double target; double value; double init; double duration;
   long value_start; int value_count;
};
struct IntervalRow {
   int rule_id; signed char kind; int start_time; int end_time; long start_event_id; long end_event_id;
   int aux_rule_id; signed char open_at_end;
};
struct EpisodeRow {
   long episode_id; int rule_id; long trigger_event_id; int deferred_from; int blocker_rule_id;
   int activation_time; int completion_time; int interface_var_id; int gate_id; int device_index;
   signed char property; double start_value; double end_value; signed char mode; double duration;
   int n_actions; signed char attached_source; signed char outcome;
};
struct GateRow { int gate_id; const char* name; int n_devices; const char* node; const char* object; };
struct DeviceRow { int device_id; int gate_id; int device_index; const char* name; signed char structure_type; };
struct TransitionRow {
   long transition_id; int time; int step; int gate_id; int device_id; signed char property;
   double old_value; double new_value; double target_value; signed char kind; int rule_id; long episode_id;
   int source_var_id; double z_up; double z_down; double gate_flow;
};
struct DeviceIntervalRow {
   int device_id; int gate_id; signed char state_class; int start_time; int end_time;
   long start_transition; long end_transition; signed char open_at_end;
};

hid_t string_type(){
   hid_t t = H5Tcopy(H5T_C_S1);
   H5Tset_size(t, H5T_VARIABLE);
   H5Tset_cset(t, H5T_CSET_UTF8);
   return t;
}

#define MEMBER(T, field, h5type) H5Tinsert(t, #field, HOFFSET(T, field), h5type)

hid_t type_rule(){
   hid_t s = string_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(RuleRow));
   MEMBER(RuleRow, rule_id, H5T_NATIVE_INT); MEMBER(RuleRow, name, s); MEMBER(RuleRow, kind, H5T_NATIVE_SCHAR);
   MEMBER(RuleRow, text, s); MEMBER(RuleRow, trigger_text, s); MEMBER(RuleRow, action_text, s);
   H5Tclose(s);
   return t;
}
hid_t type_var(){
   hid_t s = string_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(VarRow));
   MEMBER(VarRow, var_id, H5T_NATIVE_INT); MEMBER(VarRow, label, s); MEMBER(VarRow, kind, H5T_NATIVE_SCHAR);
   MEMBER(VarRow, scope_rule_id, H5T_NATIVE_INT);
   H5Tclose(s);
   return t;
}
hid_t type_rule_input(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(RuleInputRow));
   MEMBER(RuleInputRow, rule_id, H5T_NATIVE_INT); MEMBER(RuleInputRow, role, H5T_NATIVE_SCHAR);
   MEMBER(RuleInputRow, var_id, H5T_NATIVE_INT);
   return t;
}
hid_t type_event(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(EventRow));
   MEMBER(EventRow, event_id, H5T_NATIVE_LONG); MEMBER(EventRow, time, H5T_NATIVE_INT); MEMBER(EventRow, step, H5T_NATIVE_INT);
   MEMBER(EventRow, rule_id, H5T_NATIVE_INT); MEMBER(EventRow, event, H5T_NATIVE_SCHAR);
   MEMBER(EventRow, stage, H5T_NATIVE_SCHAR); MEMBER(EventRow, aux_rule_id, H5T_NATIVE_INT);
   MEMBER(EventRow, episode_id, H5T_NATIVE_LONG); MEMBER(EventRow, value_start, H5T_NATIVE_LONG);
   MEMBER(EventRow, value_count, H5T_NATIVE_INT);
   return t;
}
hid_t type_value(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(ValueRow));
   MEMBER(ValueRow, var_id, H5T_NATIVE_INT); MEMBER(ValueRow, value, H5T_NATIVE_DOUBLE);
   return t;
}
hid_t type_action(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(ActionRow));
   MEMBER(ActionRow, time, H5T_NATIVE_INT); MEMBER(ActionRow, step, H5T_NATIVE_INT); MEMBER(ActionRow, rule_id, H5T_NATIVE_INT);
   MEMBER(ActionRow, episode_id, H5T_NATIVE_LONG); MEMBER(ActionRow, interface_var_id, H5T_NATIVE_INT);
   MEMBER(ActionRow, elapsed, H5T_NATIVE_DOUBLE); MEMBER(ActionRow, fraction, H5T_NATIVE_DOUBLE);
   MEMBER(ActionRow, base, H5T_NATIVE_DOUBLE); MEMBER(ActionRow, target, H5T_NATIVE_DOUBLE);
   MEMBER(ActionRow, value, H5T_NATIVE_DOUBLE); MEMBER(ActionRow, init, H5T_NATIVE_DOUBLE);
   MEMBER(ActionRow, duration, H5T_NATIVE_DOUBLE); MEMBER(ActionRow, value_start, H5T_NATIVE_LONG);
   MEMBER(ActionRow, value_count, H5T_NATIVE_INT);
   return t;
}
hid_t type_interval(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(IntervalRow));
   MEMBER(IntervalRow, rule_id, H5T_NATIVE_INT); MEMBER(IntervalRow, kind, H5T_NATIVE_SCHAR);
   MEMBER(IntervalRow, start_time, H5T_NATIVE_INT); MEMBER(IntervalRow, end_time, H5T_NATIVE_INT);
   MEMBER(IntervalRow, start_event_id, H5T_NATIVE_LONG); MEMBER(IntervalRow, end_event_id, H5T_NATIVE_LONG);
   MEMBER(IntervalRow, aux_rule_id, H5T_NATIVE_INT); MEMBER(IntervalRow, open_at_end, H5T_NATIVE_SCHAR);
   return t;
}
hid_t type_episode(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(EpisodeRow));
   MEMBER(EpisodeRow, episode_id, H5T_NATIVE_LONG); MEMBER(EpisodeRow, rule_id, H5T_NATIVE_INT);
   MEMBER(EpisodeRow, trigger_event_id, H5T_NATIVE_LONG); MEMBER(EpisodeRow, deferred_from, H5T_NATIVE_INT);
   MEMBER(EpisodeRow, blocker_rule_id, H5T_NATIVE_INT); MEMBER(EpisodeRow, activation_time, H5T_NATIVE_INT);
   MEMBER(EpisodeRow, completion_time, H5T_NATIVE_INT); MEMBER(EpisodeRow, interface_var_id, H5T_NATIVE_INT);
   MEMBER(EpisodeRow, gate_id, H5T_NATIVE_INT); MEMBER(EpisodeRow, device_index, H5T_NATIVE_INT);
   MEMBER(EpisodeRow, property, H5T_NATIVE_SCHAR); MEMBER(EpisodeRow, start_value, H5T_NATIVE_DOUBLE);
   MEMBER(EpisodeRow, end_value, H5T_NATIVE_DOUBLE); MEMBER(EpisodeRow, mode, H5T_NATIVE_SCHAR);
   MEMBER(EpisodeRow, duration, H5T_NATIVE_DOUBLE); MEMBER(EpisodeRow, n_actions, H5T_NATIVE_INT);
   MEMBER(EpisodeRow, attached_source, H5T_NATIVE_SCHAR); MEMBER(EpisodeRow, outcome, H5T_NATIVE_SCHAR);
   return t;
}
hid_t type_gate(){
   hid_t s = string_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(GateRow));
   MEMBER(GateRow, gate_id, H5T_NATIVE_INT); MEMBER(GateRow, name, s); MEMBER(GateRow, n_devices, H5T_NATIVE_INT);
   MEMBER(GateRow, node, s); MEMBER(GateRow, object, s);
   H5Tclose(s);
   return t;
}
hid_t type_device(){
   hid_t s = string_type(), t = H5Tcreate(H5T_COMPOUND, sizeof(DeviceRow));
   MEMBER(DeviceRow, device_id, H5T_NATIVE_INT); MEMBER(DeviceRow, gate_id, H5T_NATIVE_INT);
   MEMBER(DeviceRow, device_index, H5T_NATIVE_INT); MEMBER(DeviceRow, name, s);
   MEMBER(DeviceRow, structure_type, H5T_NATIVE_SCHAR);
   H5Tclose(s);
   return t;
}
hid_t type_transition(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(TransitionRow));
   MEMBER(TransitionRow, transition_id, H5T_NATIVE_LONG); MEMBER(TransitionRow, time, H5T_NATIVE_INT);
   MEMBER(TransitionRow, step, H5T_NATIVE_INT); MEMBER(TransitionRow, gate_id, H5T_NATIVE_INT);
   MEMBER(TransitionRow, device_id, H5T_NATIVE_INT); MEMBER(TransitionRow, property, H5T_NATIVE_SCHAR);
   MEMBER(TransitionRow, old_value, H5T_NATIVE_DOUBLE); MEMBER(TransitionRow, new_value, H5T_NATIVE_DOUBLE);
   MEMBER(TransitionRow, target_value, H5T_NATIVE_DOUBLE); MEMBER(TransitionRow, kind, H5T_NATIVE_SCHAR);
   MEMBER(TransitionRow, rule_id, H5T_NATIVE_INT); MEMBER(TransitionRow, episode_id, H5T_NATIVE_LONG);
   MEMBER(TransitionRow, source_var_id, H5T_NATIVE_INT); MEMBER(TransitionRow, z_up, H5T_NATIVE_DOUBLE);
   MEMBER(TransitionRow, z_down, H5T_NATIVE_DOUBLE); MEMBER(TransitionRow, gate_flow, H5T_NATIVE_DOUBLE);
   return t;
}
hid_t type_device_interval(){
   hid_t t = H5Tcreate(H5T_COMPOUND, sizeof(DeviceIntervalRow));
   MEMBER(DeviceIntervalRow, device_id, H5T_NATIVE_INT); MEMBER(DeviceIntervalRow, gate_id, H5T_NATIVE_INT);
   MEMBER(DeviceIntervalRow, state_class, H5T_NATIVE_SCHAR); MEMBER(DeviceIntervalRow, start_time, H5T_NATIVE_INT);
   MEMBER(DeviceIntervalRow, end_time, H5T_NATIVE_INT); MEMBER(DeviceIntervalRow, start_transition, H5T_NATIVE_LONG);
   MEMBER(DeviceIntervalRow, end_transition, H5T_NATIVE_LONG); MEMBER(DeviceIntervalRow, open_at_end, H5T_NATIVE_SCHAR);
   return t;
}

}   // namespace

/** One extendable one-dimensional dataset with a write buffer. */
class Table {
public:
   Table() : _dset(-1), _type(-1), _rowSize(0), _rows(0) {}
   ~Table(){ close(); }

   bool create(hid_t file, const char* name, hid_t type, size_t rowSize, bool compress){
      _type = type;
      _rowSize = rowSize;
      hsize_t dims = 0, maxDims = H5S_UNLIMITED, chunk = CHUNK_ROWS;
      hid_t space = H5Screate_simple(1, &dims, &maxDims);
      hid_t dcpl = H5Pcreate(H5P_DATASET_CREATE);
      H5Pset_chunk(dcpl, 1, &chunk);
      if (compress){
         H5Pset_shuffle(dcpl);
         H5Pset_deflate(dcpl, 4);
      }
      _dset = H5Dcreate2(file, name, type, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
      H5Pclose(dcpl);
      H5Sclose(space);
      return _dset >= 0;
   }

   /** Copy a row into the buffer; text members must point into keep()ed strings. */
   template<class Row> bool add(const Row& row){
      const char* p = reinterpret_cast<const char*>(&row);
      _buffer.insert(_buffer.end(), p, p + sizeof(Row));
      return _buffer.size() / _rowSize >= FLUSH_ROWS ? flush() : true;
   }

   const char* keep(const std::string& s){
      _pool.push_back(s);
      return _pool.back().c_str();
   }

   long rows() const { return static_cast<long>(_rows) + static_cast<long>(_buffer.size() / (_rowSize ? _rowSize : 1)); }

   bool flush(){
      if (_dset < 0 || _buffer.empty()) return true;
      const hsize_t n = _buffer.size() / _rowSize;
      hsize_t newDims = _rows + n, start = _rows, count = n;
      bool ok = H5Dset_extent(_dset, &newDims) >= 0;
      if (ok){
         hid_t fspace = H5Dget_space(_dset);
         H5Sselect_hyperslab(fspace, H5S_SELECT_SET, &start, NULL, &count, NULL);
         hid_t mspace = H5Screate_simple(1, &count, NULL);
         ok = H5Dwrite(_dset, _type, mspace, fspace, H5P_DEFAULT, &_buffer[0]) >= 0;
         H5Sclose(mspace);
         H5Sclose(fspace);
      }
      if (ok) _rows = newDims;
      _buffer.clear();
      _pool.clear();
      return ok;
   }

   void close(){
      if (_dset >= 0){ H5Dclose(_dset); _dset = -1; }
      if (_type >= 0){ H5Tclose(_type); _type = -1; }
   }

private:
   hid_t _dset, _type;
   size_t _rowSize;
   hsize_t _rows;
   std::vector<char> _buffer;
   std::deque<std::string> _pool;
};

/** The file and its tables. */
class Writer {
public:
   Writer(const std::string& path, int level) : file(-1), failed(false){
      {
         // a file that cannot be created is reported by ok(), not by the library's error stack
         H5E_auto2_t handler = NULL;
         void* data = NULL;
         H5Eget_auto2(H5E_DEFAULT, &handler, &data);
         H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
         file = H5Fcreate(path.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
         H5Eset_auto2(H5E_DEFAULT, handler, data);
      }
      if (file < 0){ failed = true; return; }
      create(rules, "/rules", type_rule(), sizeof(RuleRow), false);
      create(variables, "/variables", type_var(), sizeof(VarRow), false);
      create(ruleInputs, "/rule_inputs", type_rule_input(), sizeof(RuleInputRow), true);
      create(events, "/events", type_event(), sizeof(EventRow), true);
      create(eventValues, "/event_values", type_value(), sizeof(ValueRow), true);
      create(actions, "/actions", type_action(), sizeof(ActionRow), true);
      create(intervals, "/intervals", type_interval(), sizeof(IntervalRow), true);
      create(episodes, "/episodes", type_episode(), sizeof(EpisodeRow), true);
      create(gates, "/gates", type_gate(), sizeof(GateRow), false);
      create(devices, "/devices", type_device(), sizeof(DeviceRow), false);
      create(transitions, "/device_transitions", type_transition(), sizeof(TransitionRow), true);
      create(deviceIntervals, "/device_intervals", type_device_interval(), sizeof(DeviceIntervalRow), true);
      setInt("format_version", Hdf5LogSink::FORMAT_VERSION);
      setInt("log_level", level);
      setInt("time_epoch", 1440);
      setText("time_unit", "julian minutes; 01JAN1900 00:00 is 1440");
      char stamp[32];
      std::time_t now = std::time(NULL);
      std::strftime(stamp, sizeof(stamp), "%Y-%m-%d %H:%M:%S", std::localtime(&now));
      setText("created", stamp);
   }

   ~Writer(){ close(); }

   bool ok() const { return file >= 0 && !failed; }

   void fail(const char* what){
      if (!failed) std::fprintf(stderr, "Warning: the oprule HDF5 log stopped (%s); the model run continues.\n", what);
      failed = true;
   }

   template<class Row> void add(Table& t, const Row& row){
      if (!ok()) return;
      if (!t.add(row)) fail("write error");
   }

   void flush(){
      if (!ok()) return;
      Table* all[] = {&rules, &variables, &ruleInputs, &events, &eventValues, &actions, &intervals, &episodes,
                      &gates, &devices, &transitions, &deviceIntervals};
      for (size_t i = 0; i < sizeof(all) / sizeof(all[0]); ++i)
         if (!all[i]->flush()) { fail("write error"); return; }
      H5Fflush(file, H5F_SCOPE_GLOBAL);
   }

   void close(){
      if (file < 0) return;
      flush();
      Table* all[] = {&rules, &variables, &ruleInputs, &events, &eventValues, &actions, &intervals, &episodes,
                      &gates, &devices, &transitions, &deviceIntervals};
      for (size_t i = 0; i < sizeof(all) / sizeof(all[0]); ++i) all[i]->close();
      H5Fclose(file);
      file = -1;
   }

   hid_t file;
   bool failed;
   Table rules, variables, ruleInputs, events, eventValues, actions, intervals, episodes, gates, devices,
         transitions, deviceIntervals;

private:
   void create(Table& t, const char* name, hid_t type, size_t size, bool compress){
      if (!t.create(file, name, type, size, compress)) fail("cannot create a table");
   }
   void setInt(const char* name, int value){
      hid_t space = H5Screate(H5S_SCALAR);
      hid_t a = H5Acreate2(file, name, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT);
      if (a >= 0){ H5Awrite(a, H5T_NATIVE_INT, &value); H5Aclose(a); }
      H5Sclose(space);
   }
   void setText(const char* name, const char* value){
      hid_t t = H5Tcopy(H5T_C_S1);
      H5Tset_size(t, std::strlen(value) + 1);
      hid_t space = H5Screate(H5S_SCALAR);
      hid_t a = H5Acreate2(file, name, t, space, H5P_DEFAULT, H5P_DEFAULT);
      if (a >= 0){ H5Awrite(a, t, value); H5Aclose(a); }
      H5Sclose(space);
      H5Tclose(t);
   }
};

}   // namespace hdf5_log

using hdf5_log::Writer;

namespace {

int label_kind(const std::string& label){
   if (label.find('(') != std::string::npos) return 0;      // a model variable
   if (label.find('.') != std::string::npos) return 2;      // internal state of a stateful node
   return 1;                                                // a named expression
}

std::string upper(std::string s){
   for (size_t i = 0; i < s.size(); ++i) s[i] = static_cast<char>(std::toupper(static_cast<unsigned char>(s[i])));
   return s;
}

// Splits "name := action WHEN trigger;" into the action and trigger text.
void split_rule(const std::string& text, std::string& action, std::string& trigger){
   std::string::size_type assign = text.find(":=");
   std::string body = assign == std::string::npos ? text : text.substr(assign + 2);
   std::string::size_type when = upper(body).find(" WHEN ");
   if (when == std::string::npos){ action = body; trigger.clear(); }
   else{ action = body.substr(0, when); trigger = body.substr(when + 6); }
   std::string::size_type b = action.find_first_not_of(" ");
   action = b == std::string::npos ? std::string() : action.substr(b);
   while (!trigger.empty() && (trigger[trigger.size() - 1] == ';' || trigger[trigger.size() - 1] == ' '))
      trigger.erase(trigger.size() - 1);
}

int lowest_property(unsigned mask){
   for (int p = 1; p <= 7; ++p) if (mask & (1u << p)) return p;
   return 0;
}

const double EPS = 1e-6;

// 0 closed, 1 open, 2 partial, 3 to node only, 4 from node only
int device_class(double opTo, double opFrom){
   const bool toClosed = std::fabs(opTo) < EPS, toOpen = std::fabs(opTo - 1.) < EPS;
   const bool fromClosed = std::fabs(opFrom) < EPS, fromOpen = std::fabs(opFrom - 1.) < EPS;
   if (toClosed && fromClosed) return 0;
   if (toOpen && fromOpen) return 1;
   if (toOpen && fromClosed) return 3;
   if (fromOpen && toClosed) return 4;
   return 2;
}

}   // namespace

Hdf5LogSink::Hdf5LogSink(const std::string& path, int logLevel)
   : _path(path), _level(logLevel), _w(new Writer(path, logLevel)), _finished(false), _nextTransition(0), _valueRows(0){}

Hdf5LogSink::~Hdf5LogSink(){
   finish(0);
   delete _w;
}

bool Hdf5LogSink::ok() const { return _w->ok(); }

void Hdf5LogSink::begin(){
   _vars.clear();
   _rulesWritten.clear();
   _ruleInputs.clear();
   _deviceIds.clear();
   _states.clear();
   _nextTransition = 0;
}

int Hdf5LogSink::varId(const std::string& label, int kind, int scopeRule){
   char scope[24];
   std::snprintf(scope, sizeof(scope), "%d:%d:", kind, scopeRule);
   const std::string key = scope + label;
   std::map<std::string, int>::iterator it = _vars.find(key);
   if (it != _vars.end()) return it->second;
   const int id = static_cast<int>(_vars.size()) + 1;
   _vars[key] = id;
   hdf5_log::VarRow row;
   row.var_id = id;
   row.label = _w->variables.keep(label);
   row.kind = static_cast<signed char>(kind);
   row.scope_rule_id = scopeRule;
   _w->add(_w->variables, row);
   return id;
}

void Hdf5LogSink::ensureRule(int ruleId){
   if (ruleId <= 0 || _rulesWritten.count(ruleId)) return;
   _rulesWritten.insert(ruleId);
   hdf5_log::RuleRow row;
   row.rule_id = ruleId;
   row.name = _w->rules.keep(RuleLog::ruleName(ruleId));
   row.kind = 0;
   row.text = row.trigger_text = row.action_text = _w->rules.keep("");
   _w->add(_w->rules, row);
}

void Hdf5LogSink::inputs(const LogValues& values, int ruleId, int role, long& start, int& count){
   start = _valueRows;
   count = 0;
   for (size_t i = 0; i < values.size(); ++i){
      const int kind = label_kind(values[i].first);
      const int var = varId(values[i].first, kind, kind == 2 ? ruleId : 0);
      hdf5_log::ValueRow row;
      row.var_id = var;
      row.value = values[i].second;
      _w->add(_w->eventValues, row);
      ++_valueRows;
      ++count;
      std::vector<int> key(3);
      key[0] = ruleId; key[1] = role; key[2] = var;
      _ruleInputs.insert(key);
   }
}

void Hdf5LogSink::event(const LogEvent& e){
   if (_finished || !_w->ok()) return;
   if (e.code == LogEvent::RULE_LOADED || e.code == LogEvent::EXPRESSION_LOADED){
      if (_rulesWritten.count(e.ruleId)) return;
      _rulesWritten.insert(e.ruleId);
      std::string action, trigger;
      split_rule(e.text, action, trigger);
      hdf5_log::RuleRow row;
      row.rule_id = e.ruleId;
      row.name = _w->rules.keep(e.rule);
      row.kind = e.code == LogEvent::EXPRESSION_LOADED ? 1 : 0;
      row.text = _w->rules.keep(e.text);
      row.trigger_text = _w->rules.keep(e.code == LogEvent::RULE_LOADED ? trigger : std::string());
      row.action_text = _w->rules.keep(e.code == LogEvent::RULE_LOADED ? action : std::string());
      _w->add(_w->rules, row);
      return;
   }
   ensureRule(e.ruleId);
   ensureRule(e.auxRuleId);
   if (e.code == LogEvent::ACTION){
      if (e.actions.empty()) return;
      const ActionInfo& a = e.actions[0];
      hdf5_log::ActionRow row;
      std::memset(&row, 0, sizeof(row));
      row.time = static_cast<int>(e.julmin);
      row.step = static_cast<int>(e.step);
      row.rule_id = e.ruleId;
      row.episode_id = e.episodeId;
      row.interface_var_id = varId(a.iface, 3, 0);
      row.elapsed = a.elapsed;
      row.fraction = a.fraction;
      row.base = a.base;
      row.target = a.target;
      row.value = a.value;
      row.init = a.dynamic ? std::nan("") : a.init;
      row.duration = a.duration;
      inputs(a.inputs, e.ruleId, 2, row.value_start, row.value_count);
      _w->add(_w->actions, row);
      return;
   }
   hdf5_log::EventRow row;
   std::memset(&row, 0, sizeof(row));
   row.event_id = e.id;
   row.time = static_cast<int>(e.julmin);
   row.step = static_cast<int>(e.step);
   row.rule_id = e.ruleId;
   row.event = static_cast<signed char>(e.code);
   row.stage = static_cast<signed char>(e.stage);
   row.aux_rule_id = e.auxRuleId;
   row.episode_id = e.episodeId;
   row.value_start = _valueRows;
   row.value_count = 0;
   if (e.code == LogEvent::TRIGGER_INITIAL || e.code == LogEvent::TRIGGERED || e.code == LogEvent::TRIGGER_CLEARED){
      inputs(e.values, e.ruleId, 0, row.value_start, row.value_count);
   }else if (e.code == LogEvent::ACTIVATED){
      long start = _valueRows;
      int count = 0;
      for (size_t i = 0; i < e.actions.size(); ++i){
         long s; int c;
         inputs(e.actions[i].inputs, e.ruleId, 1, s, c);
         count += c;
      }
      row.value_start = start;
      row.value_count = count;
      for (size_t i = 0; i < e.actions.size(); ++i) varId(e.actions[i].iface, 3, 0);   // the interface is in the dictionary
   }
   _w->add(_w->events, row);
}

void Hdf5LogSink::episode(const Episode& ep){
   if (_finished || !_w->ok()) return;
   ensureRule(ep.ruleId);
   ensureRule(ep.blockerRuleId);
   hdf5_log::EpisodeRow row;
   std::memset(&row, 0, sizeof(row));
   row.episode_id = ep.id;
   row.rule_id = ep.ruleId;
   row.trigger_event_id = ep.triggerEventId;
   row.deferred_from = static_cast<int>(ep.deferredFrom);
   row.blocker_rule_id = ep.blockerRuleId;
   row.activation_time = static_cast<int>(ep.activation);
   row.completion_time = static_cast<int>(ep.completion);
   row.interface_var_id = ep.iface.empty() ? 0 : varId(ep.iface, 3, 0);
   row.gate_id = ep.gate;
   row.device_index = ep.device;
   row.property = static_cast<signed char>(lowest_property(ep.propertyMask));
   row.start_value = ep.startValue;
   row.end_value = ep.endValue;
   row.mode = ep.duration > 0. ? 1 : 0;
   row.duration = ep.duration;
   row.n_actions = ep.nActions;
   row.attached_source = ep.attachedSource ? 1 : 0;
   row.outcome = static_cast<signed char>(ep.outcome);
   _w->add(_w->episodes, row);
}

void Hdf5LogSink::interval(const RuleInterval& i){
   if (_finished || !_w->ok()) return;
   ensureRule(i.ruleId);
   ensureRule(i.auxRuleId);
   hdf5_log::IntervalRow row;
   std::memset(&row, 0, sizeof(row));
   row.rule_id = i.ruleId;
   row.kind = static_cast<signed char>(i.kind);
   row.start_time = static_cast<int>(i.start);
   row.end_time = static_cast<int>(i.end);
   row.start_event_id = i.startEvent;
   row.end_event_id = i.endEvent;
   row.aux_rule_id = i.auxRuleId;
   row.open_at_end = i.openAtEnd ? 1 : 0;
   _w->add(_w->intervals, row);
}

void Hdf5LogSink::gates(const std::vector<GateInfo>& g, const std::vector<DeviceInfo>& d){
   if (_finished || !_w->ok()) return;
   for (size_t i = 0; i < g.size(); ++i){
      hdf5_log::GateRow row;
      row.gate_id = g[i].id;
      row.name = _w->gates.keep(g[i].name);
      row.n_devices = g[i].nDevices;
      row.node = _w->gates.keep(g[i].node);
      row.object = _w->gates.keep(g[i].object);
      _w->add(_w->gates, row);
   }
   for (size_t i = 0; i < d.size(); ++i){
      hdf5_log::DeviceRow row;
      row.device_id = d[i].id;
      row.gate_id = d[i].gate;
      row.device_index = d[i].index;
      row.name = _w->devices.keep(d[i].name);
      row.structure_type = static_cast<signed char>(d[i].structureType);
      _w->add(_w->devices, row);
      _deviceIds[static_cast<long>(d[i].gate) * 100 + d[i].index] = d[i].id;
   }
}

void Hdf5LogSink::closeDeviceInterval(int key, long julmin, long endTransition, bool atEnd){
   DeviceState& s = _states[key];
   if (s.stateClass < 0) return;
   hdf5_log::DeviceIntervalRow row;
   std::memset(&row, 0, sizeof(row));
   std::map<long, int>::iterator it = _deviceIds.find(static_cast<long>(s.gate) * 100 + s.device);
   row.device_id = it == _deviceIds.end() ? 0 : it->second;
   row.gate_id = s.gate;
   row.state_class = static_cast<signed char>(s.stateClass);
   row.start_time = static_cast<int>(s.start);
   row.end_time = static_cast<int>(julmin);
   row.start_transition = s.startTransition;
   row.end_transition = endTransition;
   row.open_at_end = atEnd ? 1 : 0;
   _w->add(_w->deviceIntervals, row);
   s.stateClass = -1;
}

// Follows the class of a device (both op coefficients) and of a gate (installed or removed).
void Hdf5LogSink::deviceState(const DeviceTransition& t, long transitionId, int){
   int key, newClass;
   DeviceState* s;
   if (t.property == PROP_INSTALL){
      key = t.gate * 1000;
      s = &_states[key];
      newClass = t.newValue > 0.5 ? 5 : 6;
   }else if (t.property == PROP_OP_TO_NODE || t.property == PROP_OP_FROM_NODE){
      key = t.gate * 1000 + t.device;
      s = &_states[key];
      if (t.property == PROP_OP_TO_NODE) s->opTo = t.newValue; else s->opFrom = t.newValue;
      // the initial rows give the two coefficients one after the other: classify with the second
      if (t.kind == TRANSITION_INITIAL && t.property == PROP_OP_TO_NODE) { s->gate = t.gate; s->device = t.device; return; }
      newClass = device_class(s->opTo, s->opFrom);
   }else{
      return;
   }
   s->gate = t.gate;
   s->device = t.property == PROP_INSTALL ? 0 : t.device;
   if (s->stateClass == newClass) return;
   if (s->stateClass >= 0) closeDeviceInterval(key, t.julmin, transitionId, false);
   s->stateClass = newClass;
   s->start = t.julmin;
   s->startTransition = transitionId;
}

void Hdf5LogSink::transition(const DeviceTransition& t){
   if (_finished || !_w->ok()) return;
   ensureRule(t.ruleId);
   hdf5_log::TransitionRow row;
   std::memset(&row, 0, sizeof(row));
   const long id = _nextTransition++;
   std::map<long, int>::iterator it = _deviceIds.find(static_cast<long>(t.gate) * 100 + t.device);
   const int deviceId = it == _deviceIds.end() ? 0 : it->second;
   row.transition_id = id;
   row.time = static_cast<int>(t.julmin);
   row.step = static_cast<int>(t.step);
   row.gate_id = t.gate;
   row.device_id = deviceId;
   row.property = static_cast<signed char>(t.property);
   row.old_value = t.oldValue;
   row.new_value = t.newValue;
   row.target_value = t.targetValue;
   row.kind = static_cast<signed char>(t.kind);
   row.rule_id = t.ruleId;
   row.episode_id = t.episodeId;
   row.source_var_id = t.source.empty() ? 0 : varId(t.source, 0, 0);
   row.z_up = t.contextValid ? t.zUp : std::nan("");
   row.z_down = t.contextValid ? t.zDown : std::nan("");
   row.gate_flow = t.contextValid ? t.gateFlow : std::nan("");
   _w->add(_w->transitions, row);
   deviceState(t, id, deviceId);
}

void Hdf5LogSink::writeRuleInputs(){
   for (std::set<std::vector<int> >::const_iterator it = _ruleInputs.begin(); it != _ruleInputs.end(); ++it){
      hdf5_log::RuleInputRow row;
      row.rule_id = (*it)[0];
      row.role = static_cast<signed char>((*it)[1]);
      row.var_id = (*it)[2];
      _w->add(_w->ruleInputs, row);
   }
   _ruleInputs.clear();
}

void Hdf5LogSink::flush(){
   if (_finished) return;
   _w->flush();
}

void Hdf5LogSink::finish(long lastJulmin){
   if (_finished) return;
   _finished = true;
   if (_w->ok()){
      for (std::map<int, DeviceState>::iterator it = _states.begin(); it != _states.end(); ++it)
         closeDeviceInterval(it->first, lastJulmin, -1, true);
      writeRuleInputs();
   }
   _w->close();
}

}}     //namespace
