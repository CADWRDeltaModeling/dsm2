#ifndef oprule_rule_HDF5LOGSINK_H__INCLUDED_
#define oprule_rule_HDF5LOGSINK_H__INCLUDED_

#include <map>
#include <set>
#include <string>
#include <vector>
#include "oprule/rule/LogTypes.h"

namespace oprule {
namespace rule {

namespace hdf5_log { class Writer; }

/** Writes the rule log to an HDF5 file (layout: OPRULE_LOG_HDF5_PLAN.md section 3).
 *
 *  Every table is a one-dimensional extendable dataset. Rows are buffered and appended in chunks, and
 *  flush() writes everything so a file left by a killed run opens and holds all records so far.
 *  A failed HDF5 call turns the sink off for the rest of the run (with one message); it never stops the model.
 */
class Hdf5LogSink : public LogSink {
public:
   /** Format of the file; written as the root attribute format_version. */
   static const int FORMAT_VERSION = 1;

   Hdf5LogSink(const std::string& path, int logLevel);
   virtual ~Hdf5LogSink();

   /** False if the file could not be created. */
   bool ok() const;
   const std::string& path() const { return _path; }

   virtual void begin();
   virtual void event(const LogEvent& e);
   virtual void episode(const Episode& e);
   virtual void interval(const RuleInterval& i);
   virtual void gates(const std::vector<GateInfo>& g, const std::vector<DeviceInfo>& d);
   virtual void transition(const DeviceTransition& t);
   virtual void flush();
   virtual void finish(long lastJulmin);

private:
   Hdf5LogSink(const Hdf5LogSink&);
   Hdf5LogSink& operator=(const Hdf5LogSink&);

   int varId(const std::string& label, int kind, int scopeRule);
   void ensureRule(int ruleId);
   void inputs(const LogValues& values, int ruleId, int role, long& start, int& count);
   void deviceState(const DeviceTransition& t, long transitionId, int deviceId);
   void closeDeviceInterval(int key, long julmin, long endTransition, bool atEnd);
   void writeRuleInputs();

   std::string _path;
   int _level;
   hdf5_log::Writer* _w;
   bool _finished;

   std::map<std::string, int> _vars;
   std::set<int> _rulesWritten;
   std::set<std::vector<int> > _ruleInputs;     // (rule, role, var) written at finish
   std::map<long, int> _deviceIds;              // gate * 100 + device index -> flat device id
   long _nextTransition;
   long _valueRows;

   struct DeviceState {
      DeviceState() : opTo(1.), opFrom(1.), stateClass(-1), start(0), startTransition(-1), gate(0), device(0) {}
      double opTo, opFrom;
      int stateClass;
      long start, startTransition;
      int gate, device;
   };
   std::map<int, DeviceState> _states;          // by gate * 1000 + device (device 0: the gate install)
};

}}     //namespace
#endif // include guard
