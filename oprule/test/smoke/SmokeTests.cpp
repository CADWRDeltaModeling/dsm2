// Smoke tests for the oprule library (parser + rule runtime) using mock model objects.
// Plan IDs refer to oprule/doc/OPRULE_TEST_PLAN.md.
#define BOOST_TEST_MODULE oprule_smoke
#include <boost/test/included/unit_test.hpp>

#include <iostream>
#include <string>

#include "MockModel.h"  // shared mock model, fixtures and helpers (test/support)

using namespace oprule_test;

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
