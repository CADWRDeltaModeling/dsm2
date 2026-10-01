// Helpers for tests that read the oprule log (RuleLog) through a string stream.
// Header only so every test program can use it. Design of the log: OPRULE_REFERENCE.md B10.
#ifndef OPRULE_TEST_LOG_CAPTURE_H
#define OPRULE_TEST_LOG_CAPTURE_H

#include <sstream>
#include <string>
#include <vector>

#include "oprule/rule/RuleLog.h"

namespace oprule_test {

inline std::string fixed_time() { return "T"; }

// Points the log at a string stream for the lifetime of the object and restores the defaults.
struct LogGuard {
   LogGuard(std::ostringstream& out, int level) {
      oprule::rule::RuleLog::setSink(&out);
      oprule::rule::RuleLog::setTimeSource(&fixed_time);
      oprule::rule::RuleLog::setLevel(level);
   }
   ~LogGuard() {
      oprule::rule::RuleLog::setLevel(oprule::rule::RuleLog::OFF);
      oprule::rule::RuleLog::setSink(0);
      oprule::rule::RuleLog::setTimeSource(0);
   }
};

struct LogRecord {
   std::string time, event, rule, detail;
};

// Splits "time | EVENT | rule | detail" lines.
inline std::vector<LogRecord> parse_log(const std::string& text) {
   std::vector<LogRecord> out;
   std::istringstream in(text);
   std::string line;
   while (std::getline(in, line)) {
      LogRecord r;
      std::string* fields[4] = {&r.time, &r.event, &r.rule, &r.detail};
      size_t pos = 0;
      for (int i = 0; i < 4; ++i) {
         size_t next = (i < 3) ? line.find(" | ", pos) : std::string::npos;
         *fields[i] = line.substr(pos, next == std::string::npos ? std::string::npos : next - pos);
         if (next == std::string::npos) break;
         pos = next + 3;
      }
      out.push_back(r);
   }
   return out;
}

// "EVENT:rule" for each record, in order.
inline std::vector<std::string> sequence(const std::vector<LogRecord>& recs) {
   std::vector<std::string> s;
   for (size_t i = 0; i < recs.size(); ++i) s.push_back(recs[i].event + ":" + recs[i].rule);
   return s;
}

inline int count_of(const std::vector<LogRecord>& recs, const std::string& event, const std::string& rule = "") {
   int n = 0;
   for (size_t i = 0; i < recs.size(); ++i)
      if (recs[i].event == event && (rule.empty() || recs[i].rule == rule)) ++n;
   return n;
}

// Detail of the first record with this event and rule ("<none>" if there is none).
inline std::string detail_of(const std::vector<LogRecord>& recs, const std::string& event, const std::string& rule) {
   for (size_t i = 0; i < recs.size(); ++i)
      if (recs[i].event == event && recs[i].rule == rule) return recs[i].detail;
   return "<none>";
}

// The lowest level at which an event is written.
inline int event_level(const std::string& event) {
   if (event == "ACTION") return 2;
   return 1;
}

}  // namespace oprule_test

#endif
