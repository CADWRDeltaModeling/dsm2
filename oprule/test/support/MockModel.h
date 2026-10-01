// Shared mock model and fixtures for the oprule tests (no DSM2 code involved).
//
// The mock model is a bag of named doubles. A VarInterface reads/writes one of them and plays the
// role of a DSM2 ModelInterface. Names starting with "t" in the registered lookup are NOT special;
// time dependence is chosen by the lookup entry (mock_var = static, mock_tvar = time dependent).
#ifndef OPRULE_TEST_MOCK_MODEL_H
#define OPRULE_TEST_MOCK_MODEL_H

#include <map>
#include <string>
#include <vector>

#include <boost/pointer_cast.hpp>
#include <boost/shared_ptr.hpp>

#include "oprule/parser/NamedValueLookupImpl.h"
#include "oprule/parser/ParseSymbolManagement.h"
#include "oprule/rule/ActionResolver.h"
#include "oprule/rule/OperationManager.h"

extern void set_input_string(std::string& input);
extern int op_ruleparse(void);
extern void op_rulerestart(FILE* input_file);  // flex scanner reset (prefix op_rule)

namespace oprule_test {

extern std::map<std::string, double> g_vars;          // model variables by name
extern std::map<std::string, bool> g_datasource_set;  // names that received a permanent data source
extern std::map<std::string, int> g_set_count;        // number of set() calls per variable

class VarInterface : public oprule::rule::ModelInterface<double> {
public:
   typedef VarInterface NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   VarInterface(const std::string& n, bool td) : name(n), timeDep(td) {}
   static NodePtr create(const std::string& n, bool td = false) { return NodePtr(new NodeType(n, td)); }
   virtual oprule::expression::DoubleNodePtr copy() { return NodePtr(new NodeType(name, timeDep)); }
   virtual double eval() { return g_vars[name]; }
   virtual void set(double v) { g_vars[name] = v; ++g_set_count[name]; }
   virtual bool isTimeDependent() const { return timeDep; }
   virtual void setDataExpression(oprule::expression::DoubleNodePtr) { g_datasource_set[name] = true; }
   virtual std::string describe() const { return std::string(timeDep ? "mock_tvar" : "mock_var") + "(name=" + name + ")"; }

   std::string name;
   bool timeDep;
};

// Read-only variable (like chan_stage): evaluates g_vars[name], cannot be set.
class ReadOnlyVar : public oprule::expression::DoubleNode {
public:
   explicit ReadOnlyVar(const std::string& n) : name(n) {}
   virtual oprule::expression::DoubleNodePtr copy() { return oprule::expression::DoubleNodePtr(new ReadOnlyVar(name)); }
   virtual double eval() { return g_vars[name]; }
   virtual bool isTimeDependent() const { return true; }
   virtual std::string describe() const { return "mock_ro(name=" + name + ")"; }
   std::string name;
};

// mock_var(name=x): writable, static.  mock_tvar(name=x): writable, time dependent.
// mock_ro(name=x): read-only.
class MockLookup : public oprule::parser::NamedValueLookupImpl {
public:
   MockLookup();
};

// Two actions overlap when they write the same named variable.
class SameVarResolver : public oprule::rule::ActionResolver {
public:
   virtual bool overlap(oprule::rule::OperationAction& a1, oprule::rule::OperationAction& a2);
   virtual oprule::rule::ActionResolver::RulePriority resolve() { return DEFER_NEW_RULE; }
};

// Trigger whose value the test sets directly.
class FlagTrigger : public oprule::rule::Trigger {
public:
   FlagTrigger() : value(false), steps(0), tests(0) {}
   virtual bool test() { ++tests; return value; }
   virtual void step(double) { ++steps; }
   bool value;
   int steps;
   int tests;   // times test() was called; logging must not add to it
};

oprule::rule::OperationActionPtr make_action(const std::string& var, double target, double ramp_sec,
                                             bool time_dep = false);

struct RuleHandle {
   RuleHandle(const std::string& var, double target, double ramp_sec, bool time_dep = false);
   FlagTrigger* trigger;  // owned by the rule
   oprule::rule::OperatingRulePtr rule;
};

// Records the arguments the grammar passes to the time-node factory.
class RecordingTimeFactory : public oprule::parser::ModelTimeNodeFactory {
public:
   typedef oprule::expression::DoubleNodePtr Ptr;
   RecordingTimeFactory() : refSeasonCalls(0), mon(0), day(0), hour(0), min(0) {}
   virtual Ptr getDateTimeNode(const std::string& dt, const std::string& tm) {
      dateArg = dt; timeArg = tm; return oprule::expression::DoubleScalarNode::create(0.);
   }
   virtual Ptr getDateTimeNode() { return oprule::expression::DoubleScalarNode::create(0.); }
   virtual Ptr getSeasonNode() { return oprule::expression::DoubleScalarNode::create(0.); }
   virtual Ptr getReferenceSeasonNode(int m, int d, int h, int mi) {
      ++refSeasonCalls; mon = m; day = d; hour = h; min = mi;
      return oprule::expression::DoubleScalarNode::create(0.);
   }
   virtual Ptr getYearNode() { return oprule::expression::DoubleScalarNode::create(2001.); }
   virtual Ptr getMonthNode() { return oprule::expression::DoubleScalarNode::create(4.); }
   virtual Ptr getDayNode() { return oprule::expression::DoubleScalarNode::create(15.); }
   virtual Ptr getHourNode() { return oprule::expression::DoubleScalarNode::create(7.); }
   virtual Ptr getMinOfDayNode() { return oprule::expression::DoubleScalarNode::create(421.); }
   virtual Ptr getMinNode() { return oprule::expression::DoubleScalarNode::create(1.); }
   virtual Ptr getTimeStepNode() { return oprule::expression::DoubleScalarNode::create(900.); }
   virtual bool isFixedStepSize() { return true; }

   std::string dateArg, timeArg;
   int refSeasonCalls, mon, day, hour, min;
};

const double DT = 900.;  // seconds, the step used by most runtime tests

// Drives an OperationManager in the same call order as the hydro time loop (see
// dsm2/src/hydrolib/update_network.f90): advance actions, (solve), step expressions, test triggers.
struct RuntimeFixture {
   RuntimeFixture();
   SameVarResolver resolver;
   oprule::rule::OperationManager manager;
   void step(double dt = DT);
};

// Parser with the mock lookup and a recording time factory. The scanner is restarted before each
// parse because a failed parse leaves it mid-buffer.
struct ParserFixture {
   ParserFixture();
   ~ParserFixture();
   int parse(const std::string& text);
   int parse_no_restart(const std::string& text);
   double num(const char* name = 0) { return getDoubleExpression(name)->eval(); }
   bool boolean(const char* name = 0) { return getBoolExpression(name)->eval(); }
   MockLookup* lookup;
   RecordingTimeFactory* timeFactory;
};

// Runs fn in a forked child and returns its exit status, or -signal if it was killed.
// Used for code paths that crash or call exit().
int run_in_child(void (*fn)());

}  // namespace oprule_test

#endif
