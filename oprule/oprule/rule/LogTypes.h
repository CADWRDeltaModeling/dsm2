#ifndef oprule_rule_LOGTYPES_H__INCLUDED_
#define oprule_rule_LOGTYPES_H__INCLUDED_

#include <string>
#include <utility>
#include <vector>

namespace oprule {
namespace rule {

/** Structured records of the operating rule log (design: OPRULE_LOG_HDF5_PLAN.md).
 *
 * RuleLog builds these and hands them to its sinks (text, memory, HDF5). Times are julian minutes
 * (01JAN1900 00:00 = 1440) and 0 means "not known / not set".
 */

/** Named values: model variables read, named expressions, internal state of stateful nodes. */
typedef std::vector<std::pair<std::string,double> > LogValues;

/** Gate device properties a rule can write; the bit of a property is (1 << value). */
enum DeviceProperty {
   PROP_NONE = 0,
   PROP_OP_TO_NODE = 1,
   PROP_OP_FROM_NODE = 2,
   PROP_HEIGHT = 3,
   PROP_ELEV = 4,
   PROP_WIDTH = 5,
   PROP_NDUPLICATE = 6,
   PROP_INSTALL = 7        ///< gate level; device is 0
};

/** The stage of a rule after an event. */
enum RuleStage { STAGE_IDLE = 0, STAGE_WAITING = 1, STAGE_DEFERRED = 2, STAGE_ACTIVE = 3 };

/** What one action writes and how far it has got. */
struct ActionInfo {
   ActionInfo() : dynamic(false), init(0), duration(0), elapsed(0), fraction(0), base(0), target(0),
                  value(0), inputsUnavailable(false), gate(0), device(0), propertyMask(0) {}
   std::string iface;        ///< what is written, as described by the model interface
   bool dynamic;             ///< the variable is time dependent (init is then "live")
   double init, duration, elapsed, fraction, base, target, value;
   LogValues inputs;         ///< model variables the target expression reads
   bool inputsUnavailable;
   int gate, device;         ///< gate device written (0 if not a gate device)
   unsigned propertyMask;    ///< bits of DeviceProperty
};

/** One logged event (a row of /events, /rules or /actions). */
struct LogEvent {
   enum Code {
      TRIGGER_INITIAL = 1, TRIGGERED = 2, TRIGGER_CLEARED = 3, ACTIVATED = 4, DEFERRED = 5,
      DEFER_ENDED = 6, COMPLETED = 7, NOT_APPLICABLE = 8, IGNORED = 9, REPLACED = 10,
      RULE_LOADED = 11, EXPRESSION_LOADED = 12, ACTION = 13
   };
   LogEvent() : code(0), id(-1), julmin(0), step(0), ruleId(0), auxRuleId(0), stage(STAGE_IDLE),
                episodeId(0), valuesUnavailable(false) {}
   static const char* name(int code);
   int code;
   long id;                  ///< 0-based row among the stage events; -1 for loaded and action records
   std::string time;         ///< model time label
   long julmin;
   long step;                ///< advances of the rule actions since the log was started
   int ruleId;
   std::string rule;
   int auxRuleId;            ///< blocker, replacing rule; 0 if none
   std::string aux;
   int stage;                ///< stage of the rule after this event
   long episodeId;           ///< episode the event belongs to; 0 if none
   LogValues values;         ///< trigger inputs
   bool valuesUnavailable;
   std::vector<ActionInfo> actions;   ///< ACTIVATED: every sub-action; ACTION: the one advanced
   std::string text;         ///< rule text for the loaded records
};

enum EpisodeOutcome {
   OUTCOME_COMPLETED = 0, OUTCOME_REPLACED = 1, OUTCOME_IGNORED = 2, OUTCOME_NOT_APPLICABLE = 3,
   OUTCOME_DEFER_ENDED = 4, OUTCOME_OPEN_AT_END = 5
};

/** From a rule's trigger rising to the outcome of its action. */
struct Episode {
   Episode() : id(0), ruleId(0), triggerEventId(-1), deferredFrom(0), blockerRuleId(0), activation(0),
               completion(0), gate(0), device(0), propertyMask(0), startValue(0), endValue(0),
               duration(0), nActions(0), attachedSource(false), outcome(OUTCOME_OPEN_AT_END),
               started(false), advances(0) {}
   long id;
   int ruleId;
   std::string rule;
   long triggerEventId;
   long deferredFrom;
   int blockerRuleId;
   long activation, completion;
   std::string iface;        ///< first action
   int gate, device;
   unsigned propertyMask;
   double startValue, endValue, duration;
   int nActions;
   bool attachedSource;
   int outcome;
   bool started;             ///< the first action value has been recorded
   long advances;
};

enum IntervalKind { INTERVAL_TRIGGER_TRUE = 0, INTERVAL_DEFERRED = 1, INTERVAL_ACTIVE = 2 };

/** A closed stretch of time in which a rule was in one condition. */
struct RuleInterval {
   RuleInterval() : ruleId(0), kind(0), start(0), end(0), startEvent(-1), endEvent(-1),
                    auxRuleId(0), openAtEnd(false) {}
   int ruleId;
   int kind;
   long start, end;
   long startEvent, endEvent;
   int auxRuleId;
   bool openAtEnd;
};

struct GateInfo {
   GateInfo() : id(0), nDevices(0) {}
   int id;
   std::string name;
   int nDevices;
   std::string node;
   std::string object;
};

struct DeviceInfo {
   DeviceInfo() : id(0), gate(0), index(0), structureType(0) {}
   int id;                   ///< flat 1-based
   int gate;
   int index;                ///< position in the gate, 1-based
   std::string name;
   int structureType;        ///< 1 weir, 2 pipe
};

enum TransitionKind {
   TRANSITION_INITIAL = 0, TRANSITION_RULE_SET = 1, TRANSITION_RAMP_START = 2,
   TRANSITION_RAMP_END = 3, TRANSITION_SOURCE = 4
};

/** A change of a gate device property (or of a gate's install state, device = 0). */
struct DeviceTransition {
   DeviceTransition() : julmin(0), step(0), gate(0), device(0), property(0), oldValue(0), newValue(0),
                        targetValue(0), kind(0), ruleId(0), episodeId(0), contextValid(false),
                        zUp(0), zDown(0), gateFlow(0) {}
   std::string time;
   long julmin, step;
   int gate, device, property;
   double oldValue, newValue, targetValue;
   int kind;
   int ruleId;               ///< rule that wrote it or attached the source (0: input source)
   long episodeId;
   std::string source;       ///< series or expression that drives a source change
   bool contextValid;
   double zUp, zDown, gateFlow;
};

/** Receives what RuleLog logs. All members are called from the model thread. */
class LogSink {
public:
   virtual ~LogSink() {}
   /** A new log session starts (also at the start of every run). */
   virtual void begin() {}
   virtual void event(const LogEvent&) = 0;
   virtual void episode(const Episode&) {}
   virtual void interval(const RuleInterval&) {}
   virtual void gates(const std::vector<GateInfo>&, const std::vector<DeviceInfo>&) {}
   virtual void transition(const DeviceTransition&) {}
   /** Write buffered records to disk. */
   virtual void flush() {}
   /** The session ends: open intervals have been closed. lastJulmin is the last model time seen. */
   virtual void finish(long lastJulmin) {}
};

/** Keeps everything in memory; for tests. */
class MemoryLogSink : public LogSink {
public:
   virtual void begin() { events.clear(); episodes.clear(); intervals.clear(); transitions.clear();
                          gateInfo.clear(); deviceInfo.clear(); finished = false; flushes = 0; }
   virtual void event(const LogEvent& e) { events.push_back(e); }
   virtual void episode(const Episode& e) { episodes.push_back(e); }
   virtual void interval(const RuleInterval& i) { intervals.push_back(i); }
   virtual void gates(const std::vector<GateInfo>& g, const std::vector<DeviceInfo>& d) { gateInfo = g; deviceInfo = d; }
   virtual void transition(const DeviceTransition& t) { transitions.push_back(t); }
   virtual void flush() { ++flushes; }
   virtual void finish(long) { finished = true; }

   MemoryLogSink() : finished(false), flushes(0) {}

   int count(int code, const std::string& rule = "") const {
      int n = 0;
      for (size_t i = 0; i < events.size(); ++i)
         if (events[i].code == code && (rule.empty() || events[i].rule == rule)) ++n;
      return n;
   }

   std::vector<LogEvent> events;
   std::vector<Episode> episodes;
   std::vector<RuleInterval> intervals;
   std::vector<DeviceTransition> transitions;
   std::vector<GateInfo> gateInfo;
   std::vector<DeviceInfo> deviceInfo;
   bool finished;
   int flushes;
};

}}     //namespace
#endif // include guard
