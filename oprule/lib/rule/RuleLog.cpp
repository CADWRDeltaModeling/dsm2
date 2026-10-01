#include "oprule/rule/RuleLog.h"
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <map>
#include <ostream>
#include <sstream>

namespace oprule {
namespace rule {

const char* LogEvent::name(int code){
   switch (code){
      case TRIGGER_INITIAL: return "TRIGGER_INITIAL";
      case TRIGGERED: return "TRIGGERED";
      case TRIGGER_CLEARED: return "TRIGGER_CLEARED";
      case ACTIVATED: return "ACTIVATED";
      case DEFERRED: return "DEFERRED";
      case DEFER_ENDED: return "DEFER_ENDED";
      case COMPLETED: return "COMPLETED";
      case NOT_APPLICABLE: return "NOT_APPLICABLE";
      case IGNORED: return "IGNORED";
      case REPLACED: return "REPLACED";
      case RULE_LOADED: return "RULE_LOADED";
      case EXPRESSION_LOADED: return "EXPRESSION_LOADED";
      case ACTION: return "ACTION";
   }
   return "UNKNOWN";
}

namespace {

std::string text_detail(const LogEvent& e);

// The text sink: "time | EVENT | rule | detail" per event, flushed after each record.
class TextSink : public LogSink {
public:
   explicit TextSink(std::ostream* out) : _out(out) {}
   virtual void event(const LogEvent& e){
      *_out << e.time << " | " << LogEvent::name(e.code) << " | " << e.rule << " | "
            << text_detail(e) << '\n';
      _out->flush();
   }
   virtual void flush(){ _out->flush(); }
private:
   std::ostream* _out;
};

int g_level = RuleLog::OFF;
std::vector<LogSink*> g_sinks;
TextSink* g_text = NULL;
std::ofstream g_file;
RuleLog::TimeSource g_time = NULL;
RuleLog::ClockSource g_clock = NULL;
std::string g_context;
int g_hooks = 0;

long g_step = 0;
long g_nextEvent = 0;
long g_nextEpisode = 1;
long g_lastJul = 0;
int g_nextRule = 1;

struct Open {
   Open() : on(false), start(0), startEvent(-1), aux(0) {}
   bool on;
   long start, startEvent;
   int aux;
};

struct RuleState {
   RuleState() : id(0), stage(STAGE_IDLE), triggerTrue(false), lastRise(-1), hasEpisode(false) {}
   int id;
   std::string name;
   int stage;
   bool triggerTrue;
   long lastRise;
   Open open[3];
   bool hasEpisode;
   Episode episode;
};

std::map<std::string, RuleState> g_rules;
std::map<int, std::string> g_names;

struct SourceNote { int ruleId; long episodeId; };
std::map<long, RuleLog::WriteNote> g_writes;
std::map<long, SourceNote> g_sources;

long note_key(int gate, int device, int property){
   return (static_cast<long>(gate) * 100 + device) * 10 + property;
}

void exit_hook();

RuleState& state_of(const std::string& name){
   std::map<std::string, RuleState>::iterator it = g_rules.find(name);
   if (it == g_rules.end()){
      RuleState s;
      s.id = g_nextRule++;
      s.name = name;
      it = g_rules.insert(std::make_pair(name, s)).first;
      g_names[s.id] = name;
   }
   return it->second;
}

long now_minutes(){
   if (g_clock) g_lastJul = g_clock();
   return g_lastJul;
}

std::string now_label(){ return g_time ? g_time() : std::string(); }

void install_sink(LogSink* sink){
   g_sinks.push_back(sink);
   // Registered with every sink: handlers run last in first out, so the one registered after a sink's
   // library (HDF5) started runs before that library shuts down. finish() is safe to call repeatedly.
   if (g_hooks < 16){ ++g_hooks; std::atexit(&exit_hook); }
}

void remove_sink(LogSink* sink){
   for (size_t i = 0; i < g_sinks.size(); ++i)
      if (g_sinks[i] == sink){ g_sinks.erase(g_sinks.begin() + i); return; }
}

void close_interval(RuleState& rs, int kind, long endJul, long endEvent, bool atEnd){
   Open& o = rs.open[kind];
   if (!o.on) return;
   RuleInterval r;
   r.ruleId = rs.id; r.kind = kind; r.start = o.start; r.end = endJul;
   r.startEvent = o.startEvent; r.endEvent = endEvent; r.auxRuleId = o.aux; r.openAtEnd = atEnd;
   o.on = false;
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->interval(r);
}

void open_interval(RuleState& rs, int kind, long start, long startEvent, int aux){
   Open& o = rs.open[kind];
   o.on = true; o.start = start; o.startEvent = startEvent; o.aux = aux;
}

void close_episode(RuleState& rs, int outcome, long julmin){
   if (!rs.hasEpisode) return;
   rs.episode.outcome = outcome;
   if (outcome == OUTCOME_COMPLETED || outcome == OUTCOME_REPLACED) rs.episode.completion = julmin;
   rs.hasEpisode = false;
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->episode(rs.episode);
}

void exit_hook(){ RuleLog::finish(); }

}   // namespace

void RuleLog::setLevel(int level){
   if (level < OFF) level = OFF;
   if (level > ACTIONS) level = ACTIONS;
   g_level = level;
}

int RuleLog::level(){ return g_level; }

bool RuleLog::enabled(int level){
   return !g_sinks.empty() && level <= g_level && level > OFF;
}

bool RuleLog::hasSink(){ return !g_sinks.empty(); }

bool RuleLog::open(const std::string& path){
   close();
   g_file.open(path.c_str(), std::ios::out | std::ios::trunc);
   if (!g_file.is_open()) return false;
   g_text = new TextSink(&g_file);
   addSink(g_text);
   return true;
}

void RuleLog::close(){
   if (g_text){
      remove_sink(g_text);
      delete g_text;
      g_text = NULL;
   }
   if (g_file.is_open()) g_file.close();
}

void RuleLog::setSink(std::ostream* sink){
   close();
   if (sink){
      g_text = new TextSink(sink);
      addSink(g_text);
   }
}

void RuleLog::addSink(LogSink* sink){
   reset();
   install_sink(sink);
   sink->begin();
}

void RuleLog::removeSink(LogSink* sink){ remove_sink(sink); }

void RuleLog::reset(){
   g_rules.clear();
   g_names.clear();
   g_writes.clear();
   g_sources.clear();
   g_step = 0;
   g_nextEvent = 0;
   g_nextEpisode = 1;
   g_lastJul = 0;
   g_nextRule = 1;
}

void RuleLog::finish(){
   if (g_sinks.empty()) return;
   const long end = now_minutes();
   for (std::map<std::string, RuleState>::iterator it = g_rules.begin(); it != g_rules.end(); ++it){
      RuleState& rs = it->second;
      for (int k = 0; k < 3; ++k) close_interval(rs, k, end, -1, true);
      close_episode(rs, OUTCOME_OPEN_AT_END, end);
   }
   for (size_t i = 0; i < g_sinks.size(); ++i){ g_sinks[i]->flush(); g_sinks[i]->finish(end); }
}

void RuleLog::flush(){
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->flush();
}

void RuleLog::setTimeSource(TimeSource source){ g_time = source; }

void RuleLog::setClock(ClockSource source){ g_clock = source; }

void RuleLog::setContext(const std::string& rule){ g_context = rule; }

const std::string& RuleLog::context(){ return g_context; }

void RuleLog::beginStep(){
   ++g_step;
   now_minutes();
}

long RuleLog::step(){ return g_step; }

std::string RuleLog::ruleName(int ruleId){
   std::map<int, std::string>::iterator it = g_names.find(ruleId);
   return it == g_names.end() ? std::string() : it->second;
}

void RuleLog::loaded(const std::string& name, bool expression, const std::string& text){
   if (!enabled(EVENTS)) return;
   LogEvent e;
   e.code = expression ? LogEvent::EXPRESSION_LOADED : LogEvent::RULE_LOADED;
   e.time = now_label();
   e.julmin = now_minutes();
   e.step = g_step;
   e.rule = name;
   e.ruleId = state_of(name).id;
   e.text = text;
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->event(e);
}

void RuleLog::event(int code, const std::string& rule, const std::string& aux,
                    const LogValues* values, bool unavailable, const std::vector<ActionInfo>* actions){
   if (!enabled(EVENTS)) return;
   RuleState& rs = state_of(rule);
   LogEvent e;
   e.code = code;
   e.id = g_nextEvent++;
   e.time = now_label();
   e.julmin = now_minutes();
   e.step = g_step;
   e.rule = rule;
   e.ruleId = rs.id;
   e.aux = aux;
   e.auxRuleId = aux.empty() ? 0 : state_of(aux).id;
   e.valuesUnavailable = unavailable;
   if (values) e.values = *values;
   if (actions) e.actions = *actions;

   switch (code){
   case LogEvent::TRIGGER_INITIAL:
      rs.triggerTrue = false;
      rs.stage = STAGE_IDLE;
      break;
   case LogEvent::TRIGGERED:
      rs.triggerTrue = true;
      rs.lastRise = e.id;
      rs.stage = STAGE_WAITING;
      open_interval(rs, INTERVAL_TRIGGER_TRUE, e.julmin, e.id, 0);
      close_episode(rs, OUTCOME_OPEN_AT_END, e.julmin);   // not expected: an episode ends with its outcome
      rs.episode = Episode();
      rs.episode.id = g_nextEpisode++;
      rs.episode.ruleId = rs.id;
      rs.episode.rule = rule;
      rs.episode.triggerEventId = e.id;
      rs.hasEpisode = true;
      break;
   case LogEvent::TRIGGER_CLEARED:
      rs.triggerTrue = false;
      rs.stage = STAGE_IDLE;
      close_interval(rs, INTERVAL_TRIGGER_TRUE, e.julmin, e.id, false);
      break;
   case LogEvent::DEFERRED:
      rs.stage = STAGE_DEFERRED;
      open_interval(rs, INTERVAL_DEFERRED, e.julmin, e.id, e.auxRuleId);
      if (rs.hasEpisode && rs.episode.deferredFrom == 0){
         rs.episode.deferredFrom = e.julmin;
         rs.episode.blockerRuleId = e.auxRuleId;
      }
      break;
   case LogEvent::DEFER_ENDED:
      rs.stage = STAGE_IDLE;
      close_interval(rs, INTERVAL_DEFERRED, e.julmin, e.id, false);
      close_episode(rs, OUTCOME_DEFER_ENDED, e.julmin);
      break;
   case LogEvent::ACTIVATED:
      rs.stage = STAGE_ACTIVE;
      close_interval(rs, INTERVAL_DEFERRED, e.julmin, e.id, false);
      open_interval(rs, INTERVAL_ACTIVE, e.julmin, e.id, 0);
      if (!rs.hasEpisode){   // a rule that fires without a recorded rise (log started late)
         rs.episode = Episode();
         rs.episode.id = g_nextEpisode++;
         rs.episode.ruleId = rs.id;
         rs.episode.rule = rule;
         rs.hasEpisode = true;
      }
      rs.episode.activation = e.julmin;
      rs.episode.nActions = actions ? static_cast<int>(actions->size()) : 0;
      if (actions && !actions->empty()){
         const ActionInfo& a = actions->front();
         rs.episode.iface = a.iface;
         rs.episode.gate = a.gate;
         rs.episode.device = a.device;
         rs.episode.propertyMask = a.propertyMask;
         rs.episode.duration = a.duration;
         if (!a.dynamic){ rs.episode.startValue = a.init; rs.episode.started = true; }
      }
      break;
   case LogEvent::COMPLETED:
      rs.stage = rs.triggerTrue ? STAGE_WAITING : STAGE_IDLE;
      close_interval(rs, INTERVAL_ACTIVE, e.julmin, e.id, false);
      e.episodeId = rs.hasEpisode ? rs.episode.id : 0;
      close_episode(rs, OUTCOME_COMPLETED, e.julmin);
      break;
   case LogEvent::REPLACED:
      rs.stage = rs.triggerTrue ? STAGE_WAITING : STAGE_IDLE;
      close_interval(rs, INTERVAL_ACTIVE, e.julmin, e.id, false);
      e.episodeId = rs.hasEpisode ? rs.episode.id : 0;
      close_episode(rs, OUTCOME_REPLACED, e.julmin);
      break;
   case LogEvent::IGNORED:
      e.episodeId = rs.hasEpisode ? rs.episode.id : 0;
      close_episode(rs, OUTCOME_IGNORED, e.julmin);
      break;
   case LogEvent::NOT_APPLICABLE:
      e.episodeId = rs.hasEpisode ? rs.episode.id : 0;
      close_episode(rs, OUTCOME_NOT_APPLICABLE, e.julmin);
      break;
   }
   e.stage = rs.stage;
   if (rs.hasEpisode) e.episodeId = rs.episode.id;
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->event(e);
}

void RuleLog::action(const ActionInfo& a){
   if (!enabled(EVENTS)) return;
   RuleState& rs = state_of(g_context);
   const bool ramp = a.duration > 0.0;
   int kind = TRANSITION_RULE_SET;
   if (ramp){
      if (a.fraction >= 1.0) kind = (rs.hasEpisode && rs.episode.advances > 0) ? TRANSITION_RAMP_END : TRANSITION_RULE_SET;
      else kind = (rs.hasEpisode && rs.episode.advances == 0) ? TRANSITION_RAMP_START : -1;
   }
   if (rs.hasEpisode){
      Episode& ep = rs.episode;
      if (!ep.started){ ep.startValue = a.base; ep.started = true; }
      ep.endValue = a.target;
      ++ep.advances;
   }
   for (int p = 1; p <= 7; ++p){
      if (!(a.propertyMask & (1u << p))) continue;
      WriteNote n;
      n.step = g_step;
      n.ruleId = rs.id;
      n.episodeId = rs.hasEpisode ? rs.episode.id : 0;
      n.kind = kind;
      n.target = a.target;
      g_writes[note_key(a.gate, a.device, p)] = n;
   }
   if (!enabled(ACTIONS)) return;
   LogEvent e;
   e.code = LogEvent::ACTION;
   e.time = now_label();
   e.julmin = now_minutes();
   e.step = g_step;
   e.rule = g_context;
   e.ruleId = rs.id;
   e.episodeId = rs.hasEpisode ? rs.episode.id : 0;
   e.stage = rs.stage;
   e.actions.push_back(a);
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->event(e);
}

void RuleLog::sourceAttached(const ActionInfo& a){
   if (!enabled(EVENTS)) return;
   RuleState& rs = state_of(g_context);
   if (rs.hasEpisode) rs.episode.attachedSource = true;
   for (int p = 1; p <= 7; ++p){
      if (!(a.propertyMask & (1u << p))) continue;
      SourceNote n;
      n.ruleId = rs.id;
      n.episodeId = rs.hasEpisode ? rs.episode.id : 0;
      g_sources[note_key(a.gate, a.device, p)] = n;
   }
}

bool RuleLog::lastWrite(int gate, int device, int property, WriteNote& note){
   std::map<long, WriteNote>::iterator it = g_writes.find(note_key(gate, device, property));
   if (it == g_writes.end()) return false;
   note = it->second;
   return true;
}

bool RuleLog::sourceOwner(int gate, int device, int property, int& ruleId, long& episodeId){
   std::map<long, SourceNote>::iterator it = g_sources.find(note_key(gate, device, property));
   if (it == g_sources.end()) return false;
   ruleId = it->second.ruleId;
   episodeId = it->second.episodeId;
   return true;
}

void RuleLog::gates(const std::vector<GateInfo>& g, const std::vector<DeviceInfo>& d){
   if (!enabled(EVENTS)) return;
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->gates(g, d);
}

void RuleLog::transition(DeviceTransition t){
   if (!enabled(EVENTS)) return;
   t.time = now_label();
   t.julmin = now_minutes();
   t.step = g_step;
   for (size_t i = 0; i < g_sinks.size(); ++i) g_sinks[i]->transition(t);
}

namespace {

std::string describe_action(const ActionInfo& a, bool advance){
   std::ostringstream s;
   if (advance){
      s << "interface=" << a.iface
        << " elapsed=" << RuleLog::number(a.elapsed)
        << " fraction=" << RuleLog::number(a.fraction)
        << " base=" << RuleLog::number(a.base)
        << " target=" << RuleLog::number(a.target)
        << " value=" << RuleLog::number(a.value)
        << " duration=" << RuleLog::number(a.duration)
        << " init=" << (a.dynamic ? std::string("live") : RuleLog::number(a.init));
   }else{
      s << "interface=" << a.iface
        << " mode=" << (a.dynamic ? "time_dependent" : "static")
        << " init=" << (a.dynamic ? std::string("live") : RuleLog::number(a.init))
        << " duration=" << RuleLog::number(a.duration)
        << " elapsed=" << RuleLog::number(a.elapsed);
   }
   s << " target_inputs=" << (a.inputsUnavailable ? std::string("unavailable") : RuleLog::state(a.inputs));
   return s.str();
}

std::string text_detail(const LogEvent& e){
   switch (e.code){
   case LogEvent::TRIGGER_INITIAL:
   case LogEvent::TRIGGERED:
   case LogEvent::TRIGGER_CLEARED:
      return "trigger_inputs=" + (e.valuesUnavailable ? std::string("unavailable") : RuleLog::state(e.values));
   case LogEvent::ACTIVATED: {
      if (e.valuesUnavailable) return "action=unavailable";
      std::string out;
      for (size_t i = 0; i < e.actions.size(); ++i){
         if (!out.empty()) out += " + ";
         out += describe_action(e.actions[i], false);
      }
      return out;
   }
   case LogEvent::ACTION:
      return e.actions.empty() ? std::string() : describe_action(e.actions[0], true);
   case LogEvent::DEFERRED:
   case LogEvent::IGNORED:
      return "blocked_by=" + e.aux;
   case LogEvent::REPLACED:
      return "replaced_by=" + e.aux;
   case LogEvent::DEFER_ENDED:
      return "trigger went false while deferred";
   case LogEvent::NOT_APPLICABLE:
      return "action not applicable in current context";
   case LogEvent::RULE_LOADED:
   case LogEvent::EXPRESSION_LOADED:
      return "text=" + e.text;
   }
   return std::string();
}

}   // namespace

std::string RuleLog::number(double value){
   if (value != value) return "nan";
   if (value == HUGE_VAL || value == -HUGE_VAL) return "unset";
   std::ostringstream s;
   s << std::setprecision(9) << value;
   return s.str();
}

std::string RuleLog::state(const StateList& values){
   std::string out = "[";
   bool first = true;
   for (size_t i = 0; i < values.size(); ++i){
      // the same variable read through several named expressions is listed once
      bool seen = false;
      for (size_t j = 0; j < i && !seen; ++j)
         seen = values[j].first == values[i].first && values[j].second == values[i].second;
      if (seen) continue;
      if (!first) out += "; ";
      first = false;
      out += values[i].first + "=" + number(values[i].second);
   }
   return out + "]";
}

}}     //namespace
