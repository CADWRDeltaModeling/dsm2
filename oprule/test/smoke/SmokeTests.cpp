// Smoke tests for the oprule library (parser + rule runtime) using mock model objects.
// Plan IDs refer to oprule/doc/OPRULE_TEST_PLAN.md.
#define BOOST_TEST_MODULE oprule_smoke
#include <boost/test/included/unit_test.hpp>
#include <boost/pointer_cast.hpp>
#include <boost/shared_ptr.hpp>

#include <iostream>
#include <map>
#include <string>

#include "oprule/parser/ParseSymbolManagement.h"
#include "oprule/parser/NamedValueLookupImpl.h"
#include "oprule/rule/ActionResolver.h"
#include "oprule/rule/OperationManager.h"
#include "ParserTestFixture.h"  // TrivialModelTimeNodeFactory

extern void set_input_string(std::string& input);
extern int op_ruleparse(void);
extern void op_rulerestart(FILE* input_file);  // flex scanner reset (prefix op_rule)

namespace {

using oprule::rule::ActionResolver;
using oprule::rule::OperationManager;
using oprule::rule::Trigger;

// ---------------------------------------------------------------- mock model

std::map<std::string, double> g_vars;           // model variables by name
std::map<std::string, bool> g_datasource_set;   // names given a permanent data source

class VarInterface : public oprule::rule::ModelInterface<double> {
public:
   typedef VarInterface NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   VarInterface(const std::string& n, bool td) : name(n), timeDep(td) {}
   static NodePtr create(const std::string& n, bool td = false) {
      return NodePtr(new NodeType(n, td));
   }
   virtual oprule::expression::DoubleNodePtr copy() { return NodePtr(new NodeType(name, timeDep)); }
   virtual double eval() { return g_vars[name]; }
   virtual void set(double v) { g_vars[name] = v; }
   virtual bool isTimeDependent() const { return timeDep; }
   virtual void setDataExpression(oprule::expression::DoubleNodePtr) { g_datasource_set[name] = true; }

   std::string name;
   bool timeDep;
};

oprule::expression::DoubleNodePtr make_var(const NamedValueLookup::ArgMap& args, bool td) {
   NamedValueLookup::ArgMap::const_iterator it = args.find("name");
   if (it == args.end()) throw oprule::parser::MissingIdentifier("name not given");
   return VarInterface::create(it->second, td);
}
oprule::expression::DoubleNodePtr mock_var_factory(const NamedValueLookup::ArgMap& a) { return make_var(a, false); }
oprule::expression::DoubleNodePtr mock_tvar_factory(const NamedValueLookup::ArgMap& a) { return make_var(a, true); }

class MockLookup : public oprule::parser::NamedValueLookupImpl {
public:
   MockLookup() {
      ModelNameInfo info;
      info.params.push_back("name");
      info.type = NamedValueLookup::READWRITE;
      info.name = "mock_var";
      info.factory = &mock_var_factory;
      add("mock_var", info);
      info.name = "mock_tvar";
      info.factory = &mock_tvar_factory;
      add("mock_tvar", info);
   }
};

// Two actions overlap when they write the same named variable.
class SameVarResolver : public ActionResolver {
public:
   virtual bool overlap(oprule::rule::OperationAction& a1, oprule::rule::OperationAction& a2) {
      typedef oprule::rule::ModelAction<double> MA;
      MA* m1 = dynamic_cast<MA*>(&a1);
      MA* m2 = dynamic_cast<MA*>(&a2);
      if (!m1 || !m2) return false;
      boost::shared_ptr<VarInterface> v1 = boost::dynamic_pointer_cast<VarInterface>(m1->getModelInterface());
      boost::shared_ptr<VarInterface> v2 = boost::dynamic_pointer_cast<VarInterface>(m2->getModelInterface());
      return v1 && v2 && v1->name == v2->name;
   }
   virtual ActionResolver::RulePriority resolve() { return ActionResolver::DEFER_NEW_RULE; }
};

class FlagTrigger : public Trigger {
public:
   FlagTrigger() : value(false) {}
   virtual bool test() { return value; }
   virtual void step(double) {}
   bool value;
};

oprule::rule::OperationActionPtr make_action(const std::string& var, double target, double ramp_sec,
                                             bool time_dep = false) {
   oprule::rule::TransitionPtr tr = ramp_sec > 0.
      ? oprule::rule::TransitionPtr(new oprule::rule::LinearTransition(ramp_sec))
      : oprule::rule::TransitionPtr(new oprule::rule::AbruptTransition());
   return oprule::rule::OperationActionPtr(new oprule::rule::ModelAction<double>(
      VarInterface::create(var, time_dep), oprule::expression::DoubleScalarNode::create(target), tr));
}

struct RuleHandle {
   RuleHandle(const std::string& var, double target, double ramp_sec, bool time_dep = false)
      : trigger(new FlagTrigger),
        rule(new oprule::rule::OperatingRule(make_action(var, target, ramp_sec, time_dep),
                                             oprule::rule::TriggerPtr(trigger))) {}
   FlagTrigger* trigger;  // owned by the rule
   oprule::rule::OperatingRulePtr rule;
};

const double DT = 900.;  // seconds

struct RuntimeFixture {
   RuntimeFixture() : manager(resolver) { g_vars.clear(); g_datasource_set.clear(); }
   SameVarResolver resolver;
   OperationManager manager;
   // Same call order as the hydro time loop: advance, (solve), step, test.
   void step(double dt = DT) {
      manager.advanceActions(dt);
      manager.stepExpressions(dt);
      manager.manageActivation();
   }
};

struct ParserFixture {
   ParserFixture()
      : lookup(new MockLookup), timeFactory(new TrivialModelTimeNodeFactory(2001, 4, 15, 7, 1)) {
      g_vars.clear();
      lexer_init();
      init_expression();
      init_rule_names();
      init_lookup(lookup);
      init_model_time_factory(timeFactory);
   }
   ~ParserFixture() {
      clear_temp_expr();
      clear_arg_map();
      init_expression();
      init_rule_names();
      delete lookup;
      delete timeFactory;
   }
   int parse(const std::string& text) {
      op_rulerestart(NULL);  // the scanner keeps its buffer after a failed parse (PSM-02)
      return parse_no_restart(text);
   }
   int parse_no_restart(const std::string& text) {
      std::string s(text);
      set_input_string(s);
      return op_ruleparse();
   }
   double num(const char* name = 0) { return getDoubleExpression(name)->eval(); }
   bool boolean(const char* name = 0) { return getBoolExpression(name)->eval(); }
   MockLookup* lookup;
   TrivialModelTimeNodeFactory* timeFactory;
};

}  // namespace

// =========================================================== parser smoke tests

BOOST_FIXTURE_TEST_SUITE(parser, ParserFixture)

BOOST_AUTO_TEST_CASE(numeric_expressions) {
   BOOST_REQUIRE_EQUAL(parse("2. + 2 * 2 * 2 - 2 / 2 - 2 / 2;"), 0);
   BOOST_CHECK_CLOSE(num(), 8., 1e-9);
   BOOST_REQUIRE_EQUAL(parse("IFELSE( 3>2, 5, 6);"), 0);
   BOOST_CHECK_CLOSE(num(), 5., 1e-9);
   BOOST_REQUIRE_EQUAL(parse("max3(3.,1.,4.);"), 0);
   BOOST_CHECK_CLOSE(num(), 4., 1e-9);
}

BOOST_AUTO_TEST_CASE(boolean_expressions) {
   BOOST_REQUIRE_EQUAL(parse("((20.0 + 10.) <=30.) AND true;"), 0);
   BOOST_CHECK(boolean());
   BOOST_REQUIRE_EQUAL(parse("NOT(false OR false);"), 0);
   BOOST_CHECK(boolean());
   BOOST_REQUIRE_EQUAL(parse("(20. / 10.) < 1. AND (1. > 3.);"), 0);
   BOOST_CHECK(!boolean());
}

BOOST_AUTO_TEST_CASE(named_expressions) {
   BOOST_REQUIRE_EQUAL(parse("A:= 7.25+3.75;"), 0);
   BOOST_CHECK_CLOSE(num("A"), 11., 1e-9);
   BOOST_REQUIRE_EQUAL(parse("B:= A+1.;"), 0);
   BOOST_CHECK_CLOSE(num("B"), 12., 1e-9);
   BOOST_REQUIRE_EQUAL(parse("ABool:= (3.*A== 33.);"), 0);
   BOOST_CHECK(boolean("ABool"));
}

BOOST_AUTO_TEST_CASE(model_time_terms) {
   BOOST_REQUIRE_EQUAL(parse("BBool:= MONTH <= APR;"), 0);  // mock time factory: month = 4
   BOOST_CHECK(boolean("BBool"));
   BOOST_REQUIRE_EQUAL(parse("YEAR + 1.;"), 0);
   BOOST_CHECK_CLOSE(num(), 2002., 1e-9);
}

BOOST_AUTO_TEST_CASE(syntax_error_is_reported) {
   BOOST_CHECK(parse("2 + ;") != 0);
   BOOST_CHECK(parse("nosuchname + 1.;") != 0);
}

BOOST_AUTO_TEST_CASE(rule_is_parsed_and_registered) {
   BOOST_REQUIRE_EQUAL(
      parse("r1 := SET mock_var(name=tgt) TO 5 RAMP 60MIN WHEN mock_var(name=src) > 1;"), 0);
   BOOST_CHECK_EQUAL(get_parsed_type(), oprule::parser::OP_RULE);
   oprule::rule::OperatingRulePtr rule = getOperatingRule("r1");
   BOOST_REQUIRE(rule);
   BOOST_CHECK_EQUAL(rule->getName(), "r1");
}

BOOST_AUTO_TEST_CASE(duplicate_rule_name_is_rejected) {
   const std::string text = "r1 := SET mock_var(name=tgt) TO 5 WHEN TRUE;";
   BOOST_REQUIRE_EQUAL(parse(text), 0);
   BOOST_CHECK(parse(text) != 0);
}

BOOST_AUTO_TEST_SUITE_END()

// ============================================================ runtime smoke tests

BOOST_FIXTURE_TEST_SUITE(runtime, RuntimeFixture)

// RUN-01, RUN-03, RUN-07: edge triggered, abrupt action completes in one advance.
BOOST_AUTO_TEST_CASE(abrupt_rule_fires_once_per_rising_edge) {
   g_vars["x"] = 0.;
   RuleHandle h("x", 5., 0.);
   manager.addRule(h.rule);

   h.trigger->value = true;
   manager.manageActivation();
   BOOST_CHECK(h.rule->isActive());
   BOOST_CHECK_EQUAL(g_vars["x"], 0.);   // nothing is written until the next advance

   manager.advanceActions(DT);
   BOOST_CHECK_EQUAL(g_vars["x"], 5.);
   BOOST_CHECK(!h.rule->isActive());

   manager.manageActivation();            // trigger still true: no new edge
   BOOST_CHECK(!h.rule->isActive());

   h.trigger->value = false;
   manager.manageActivation();
   h.trigger->value = true;
   manager.manageActivation();            // new rising edge
   BOOST_CHECK(h.rule->isActive());
}

// RUN-04: a trigger that becomes true during step n changes the model in step n+1.
BOOST_AUTO_TEST_CASE(action_takes_effect_one_step_after_trigger) {
   g_vars["x"] = 0.;
   RuleHandle h("x", 5., 0.);
   manager.addRule(h.rule);
   h.trigger->value = true;
   step();                                // trigger tested at the end of this step
   BOOST_CHECK_EQUAL(g_vars["x"], 0.);
   step();
   BOOST_CHECK_EQUAL(g_vars["x"], 5.);
}

// TRN-01: linear ramp, dt = 15 min, ramp = 60 min: completes on the 4th advance.
BOOST_AUTO_TEST_CASE(linear_ramp_reaches_target_on_fourth_advance) {
   g_vars["x"] = 0.;
   RuleHandle h("x", 10., 3600.);
   manager.addRule(h.rule);
   h.trigger->value = true;
   manager.manageActivation();
   const double expected[] = {2.5, 5., 7.5, 10.};
   for (int i = 0; i < 4; ++i) {
      manager.advanceActions(DT);
      BOOST_CHECK_CLOSE(g_vars["x"], expected[i], 1e-9);
      BOOST_CHECK_EQUAL(h.rule->isActive(), i < 3);
   }
}

// CFL-01: an overlapping rule is deferred and retried until the active rule finishes.
BOOST_AUTO_TEST_CASE(overlapping_rule_is_deferred_then_activated) {
   g_vars["x"] = 0.;
   RuleHandle a("x", 10., 3600.), b("x", 99., 0.);
   manager.addRule(a.rule);
   manager.addRule(b.rule);

   a.trigger->value = true;
   manager.manageActivation();
   BOOST_REQUIRE(a.rule->isActive());
   b.trigger->value = true;
   manager.manageActivation();
   BOOST_CHECK(!b.rule->isActive());      // deferred

   for (int i = 0; i < 4; ++i) manager.advanceActions(DT);
   BOOST_CHECK(!a.rule->isActive());
   manager.manageActivation();            // deferral reset b's edge memory: it fires now
   BOOST_CHECK(b.rule->isActive());
   manager.advanceActions(DT);
   BOOST_CHECK_EQUAL(g_vars["x"], 99.);
}

// CFL-02, CFL-03: pool order decides a same-step conflict; unrelated rules do not conflict.
BOOST_AUTO_TEST_CASE(same_step_conflict_resolved_by_pool_order) {
   RuleHandle first("x", 1., 0.), second("x", 2., 0.), other("y", 3., 0.);
   manager.addRule(first.rule);
   manager.addRule(second.rule);
   manager.addRule(other.rule);
   first.trigger->value = second.trigger->value = other.trigger->value = true;
   manager.manageActivation();
   BOOST_CHECK(first.rule->isActive());
   BOOST_CHECK(!second.rule->isActive());
   BOOST_CHECK(other.rule->isActive());
}

// A5.5: completing an action on a time-dependent variable attaches a data source; static does not.
BOOST_AUTO_TEST_CASE(time_dependent_target_gets_data_source_on_completion) {
   RuleHandle td("tx", 5., 0., true), st("sx", 5., 0., false);
   manager.addRule(td.rule);
   manager.addRule(st.rule);
   td.trigger->value = st.trigger->value = true;
   manager.manageActivation();
   manager.advanceActions(DT);
   BOOST_CHECK(g_datasource_set.count("tx") == 1);
   BOOST_CHECK(g_datasource_set.count("sx") == 0);
}

// End to end: text -> parser -> rule -> manager -> mock model.
BOOST_AUTO_TEST_CASE(parsed_rule_drives_mock_model) {
   ParserFixture parser_setup;  // installs the mock lookup and time factory
   g_vars["src"] = 0.;
   g_vars["tgt"] = 0.;
   BOOST_REQUIRE_EQUAL(
      parser_setup.parse("r1 := SET mock_var(name=tgt) TO 5 RAMP 60MIN WHEN mock_var(name=src) > 1;"), 0);
   manager.addRule(getOperatingRule("r1"));

   step();
   BOOST_CHECK(!getOperatingRule("r1")->isActive());
   g_vars["src"] = 2.;
   step();                                 // trigger becomes true at the end of this step
   BOOST_CHECK(getOperatingRule("r1")->isActive());
   const double expected[] = {1.25, 2.5, 3.75, 5.};
   for (int i = 0; i < 4; ++i) {
      step();
      BOOST_CHECK_CLOSE(g_vars["tgt"], expected[i], 1e-9);
   }
   BOOST_CHECK(!getOperatingRule("r1")->isActive());
}

BOOST_AUTO_TEST_SUITE_END()

// ================================================== characterization (pin current behaviour)
// These record behaviour that the reference doc lists as limitations or suspected defects.
// When one of them fails after a code change, read the plan entry and update deliberately.

BOOST_FIXTURE_TEST_SUITE(characterization, ParserFixture)

// CASE-01: model names are matched exactly.
BOOST_AUTO_TEST_CASE(model_names_are_case_sensitive) {
   BOOST_CHECK_EQUAL(parse("mock_var(name=tgt) > 1.;"), 0);
   BOOST_CHECK(parse("MOCK_VAR(name=tgt) > 1.;") != 0);
}

// CASE-03: keywords are not.
BOOST_AUTO_TEST_CASE(keywords_are_case_insensitive) {
   BOOST_CHECK_EQUAL(parse("True And TRUE;"), 0);
   BOOST_CHECK_EQUAL(parse("r1 := set mock_var(name=tgt) to 5 when true;"), 0);
}

// CFL-05 / D-07: the action list of a THEN chain is empty, so chains never conflict.
BOOST_AUTO_TEST_CASE(then_chain_has_empty_action_list) {
   oprule::rule::OperationActionPtr chain = chain_actions(make_action("x", 1., 0.), make_action("x", 2., 0.));
   oprule::rule::OperatingRule chain_rule(chain, oprule::rule::TriggerPtr(new FlagTrigger));
   BOOST_CHECK_EQUAL(chain_rule.getActionList().size(), 0u);

   oprule::rule::OperationActionPtr set = group_actions(make_action("x", 1., 0.), make_action("y", 2., 0.));
   oprule::rule::OperatingRule set_rule(set, oprule::rule::TriggerPtr(new FlagTrigger));
   BOOST_CHECK_EQUAL(set_rule.getActionList().size(), 2u);
}

// D-11: lagged expressions parse but cannot be evaluated; the throw is a pointer.
BOOST_AUTO_TEST_CASE(lagged_expression_is_not_implemented) {
   BOOST_REQUIRE_EQUAL(parse("D:= C(t-1)+1.;"), 0);
   bool threw = false;
   try {
      num("D");
   } catch (std::logic_error* e) {
      threw = true;
      delete e;
   }
   BOOST_CHECK(threw);
}

// PSM-02: without a scanner restart the parse after a syntax error is unreliable (recorded, not
// asserted); with a restart it works.
BOOST_AUTO_TEST_CASE(parser_after_syntax_error_needs_scanner_restart) {
   BOOST_CHECK(parse("2 + * 3; 4;") != 0);
   int raw = parse_no_restart("1. + 1.;");
   std::cout << "[CHAR] re-parse after syntax error, no restart: rc=" << raw << std::endl;
   BOOST_REQUIRE_EQUAL(parse("1. + 1.;"), 0);
   BOOST_CHECK_CLOSE(num(), 2., 1e-9);
}

BOOST_AUTO_TEST_SUITE_END()
