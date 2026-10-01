#ifndef oprule_rule_RULELOG_H__INCLUDED_
#define oprule_rule_RULELOG_H__INCLUDED_

#include <iosfwd>
#include <string>
#include <utility>
#include <vector>
#include "oprule/rule/LogTypes.h"

namespace oprule {
namespace rule {

/** Event log for operating rules (designs: OPRULE_REFERENCE.md B10, OPRULE_LOG_HDF5_PLAN.md).
 *
 * Callers report structured events; the log numbers them, follows each rule's stage, builds the
 * intervals and episodes, and hands everything to its sinks (a text stream, memory, HDF5). Global
 * state, like the parser. The log never evaluates a trigger or an expression; callers pass values
 * they already have.
 *
 * The text sink writes one record per line: "time | EVENT | rule | detail".
 */
class RuleLog {
public:
   /** Each level includes everything from the levels below it. */
   enum Level {
      OFF = 0,       ///< nothing is written (default)
      EVENTS = 1,    ///< loaded, trigger changes, activated, deferred, completed
      ACTIONS = 2    ///< plus the values written by actions at every advance
   };

   /** Produces the model time label written on each record. */
   typedef std::string (*TimeSource)();

   /** Produces the model time in julian minutes. */
   typedef long (*ClockSource)();

   /** Set the level; values outside 0..2 are clamped. */
   static void setLevel(int level);
   static int level();

   /** True if records of this level are written (needs a level and a sink). */
   static bool enabled(int level);

   /** True if a sink of any kind is installed. */
   static bool hasSink();

   /** Open a text file as a sink, replacing any earlier text sink. Returns false if it cannot be opened. */
   static bool open(const std::string& path);

   /** Close the text file sink, if one is open. The level is unchanged. */
   static void close();

   /** Use an existing stream as the text sink (not owned). NULL removes it. Used by tests. */
   static void setSink(std::ostream* sink);

   /** Add a sink (not owned) and start a new session. */
   static void addSink(LogSink* sink);
   static void removeSink(LogSink* sink);

   /** Forget the rule ids, event numbers, stages and open intervals and episodes (a new run). */
   static void reset();

   /** Close the open intervals and episodes, tell the sinks the session ends. Safe to call twice. */
   static void finish();

   /** Ask every sink to write what it has buffered. */
   static void flush();

   /** Set the model time sources; NULL writes an empty label and time 0. */
   static void setTimeSource(TimeSource source);
   static void setClock(ClockSource source);

   /** Name of the rule currently being advanced, used by records written from code that
    *  has no rule name (for example ModelAction). Set and cleared by OperatingRule.
    */
   static void setContext(const std::string& rule);
   static const std::string& context();

   /** Counts one advance of the rule actions; called once per model step. */
   static void beginStep();
   static long step();

   /** A rule or named expression was parsed. */
   static void loaded(const std::string& name, bool expression, const std::string& text);

   /** A stage event of a rule: TRIGGER_*, ACTIVATED, DEFERRED, ... aux is the blocking or replacing
    *  rule. values are the trigger inputs; actions the sub-actions (ACTIVATED only). */
   static void event(int code, const std::string& rule, const std::string& aux,
                     const LogValues* values = 0, bool unavailable = false,
                     const std::vector<ActionInfo>* actions = 0);

   /** One advance of the action of the rule in the context. Always updates the rule's episode and
    *  the write notes; the record itself is written at level ACTIONS. */
   static void action(const ActionInfo& info);

   /** The action of the rule in the context attached its expression as the permanent data source. */
   static void sourceAttached(const ActionInfo& info);

   /** What the rule actions did to a gate device property in the current step. */
   struct WriteNote {
      WriteNote() : step(-1), ruleId(0), episodeId(0), kind(0), target(0) {}
      long step;
      int ruleId;
      long episodeId;
      int kind;          ///< TransitionKind: rule set, ramp start, ramp end, or -1 for a ramp step
      double target;
   };
   static bool lastWrite(int gate, int device, int property, WriteNote& note);

   /** Rule and episode that attached a source to a gate device property; false if none did. */
   static bool sourceOwner(int gate, int device, int property, int& ruleId, long& episodeId);

   /** Static gate and device tables, passed once to the sinks. */
   static void gates(const std::vector<GateInfo>& gates, const std::vector<DeviceInfo>& devices);

   /** A device transition found by the sampler; time, step are filled in here. */
   static void transition(DeviceTransition t);

   static std::string ruleName(int ruleId);

   /** Same type as oprule::expression::StateList. */
   typedef std::vector<std::pair<std::string,double> > StateList;

   /** A number for a record: 9 significant digits; "unset" for the HUGE_VAL a node holds before it
    *  has a value, "nan" for not-a-number.
    */
   static std::string number(double value);

   /** A list of named values as "[a=1; b=2]" (items contain no spaces). An item that repeats an earlier
    *  one with the same name and value is left out. */
   static std::string state(const StateList& values);
};

}}     //namespace
#endif // include guard
