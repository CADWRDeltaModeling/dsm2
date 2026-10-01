#ifndef oprule_rule_RULELOG_H__INCLUDED_
#define oprule_rule_RULELOG_H__INCLUDED_

#include <iosfwd>
#include <string>

namespace oprule {
namespace rule {

/** Event log for operating rules (design: OPRULE_REFERENCE.md B10).
 *
 * One record per line: "time | EVENT | rule | detail". Global state, like the parser.
 * The log never evaluates a trigger or an expression; callers pass values they already have.
 */
class RuleLog {
public:
   /** Each level includes everything from the levels below it. */
   enum Level {
      OFF = 0,       ///< nothing is written (default)
      EVENTS = 1,    ///< loaded, triggered, activated, deferred, completed
      ACTIONS = 2,   ///< plus the values written by actions
      TRIGGERS = 3   ///< plus the trigger value of every inactive rule at every step
   };

   /** Produces the model time label written on each record. */
   typedef std::string (*TimeSource)();

   /** Set the level; values outside 0..3 are clamped. */
   static void setLevel(int level);
   static int level();

   /** True if records of this level are written (needs a level and a sink). */
   static bool enabled(int level);

   /** True if a sink (file or stream) is installed. */
   static bool hasSink();

   /** Open a file as the sink, replacing any earlier sink. Returns false if it cannot be opened. */
   static bool open(const std::string& path);

   /** Close the file sink, if one is open. The level is unchanged. */
   static void close();

   /** Use an existing stream as the sink (not owned). NULL removes the sink. Used by tests. */
   static void setSink(std::ostream* sink);

   /** Set the model time label source; NULL writes an empty time. */
   static void setTimeSource(TimeSource source);

   /** Name of the rule currently being advanced, used by records written from code that
    *  has no rule name (for example ModelAction). Set and cleared by OperatingRule.
    */
   static void setContext(const std::string& rule);
   static const std::string& context();

   /** Write one record if the level is enabled. */
   static void write(int level, const std::string& event,
                     const std::string& rule, const std::string& detail);
};

}}     //namespace
#endif // include guard
