// Core oprule tests: grammar, lexer, expression nodes, activation / transition / conflict logic.
// Uses mock model variables only (support/MockModel.h); nothing from DSM2.
//
// Suites
//   grammar_*      what the parser accepts and the values expressions produce
//   lexer          token-level behaviour, including reserved words and number formats
//   rule_structure THEN / WHILE nesting, checked through timing
//   runtime        edge triggering, ramps, static vs time-dependent targets, conflicts
//   pinned         behaviour that is believed to be a defect or limitation; each test says which
//                  entry of OPRULE_REFERENCE.md (B9) or OPRULE_TEST_PLAN.md it belongs to. When
//                  one of them fails after a change, decide deliberately whether the change fixed it.
#define BOOST_TEST_MODULE oprule_core
#include <boost/test/included/unit_test.hpp>

#include <cmath>
#include <iostream>
#include <string>

#include "MockModel.h"

using namespace oprule_test;
using oprule::rule::OperatingRulePtr;

namespace {

// Parser plus runtime: parse rule text with the mock lookup and hand the rule to the manager.
struct RuleBench : ParserFixture, RuntimeFixture {
   OperatingRulePtr add(const std::string& name, const std::string& action, const std::string& trigger) {
      int rc = parse(name + " := " + action + " WHEN " + trigger + ";");
      BOOST_REQUIRE_MESSAGE(rc == 0, "rule failed to parse: " + action + " WHEN " + trigger);
      OperatingRulePtr r = getOperatingRule(name);
      manager.addRule(r);
      return r;
   }
   // Fortran order: advance, (solve), step, test. Same as RuntimeFixture::step.
   void run(int n) { for (int i = 0; i < n; ++i) step(); }
};

// Parse "<text>;" and check the numeric result.
void expect_num(ParserFixture& p, const std::string& text, double expected, double tol_pct = 1e-9) {
   BOOST_REQUIRE_MESSAGE(p.parse(text) == 0, "parse failed: " + text);
   if (expected == 0.) BOOST_CHECK_SMALL(p.num(), 1e-12);
   else BOOST_CHECK_CLOSE_FRACTION(p.num(), expected, tol_pct);
}

void expect_bool(ParserFixture& p, const std::string& text, bool expected) {
   BOOST_REQUIRE_MESSAGE(p.parse(text) == 0, "parse failed: " + text);
   BOOST_CHECK_MESSAGE(p.boolean() == expected, text);
}

void expect_parse_error(ParserFixture& p, const std::string& text) {
   BOOST_CHECK_MESSAGE(p.parse(text) != 0, "expected a parse error: " + text);
}

}  // namespace

// ====================================================================== grammar: numbers

BOOST_FIXTURE_TEST_SUITE(grammar_numeric, ParserFixture)

BOOST_AUTO_TEST_CASE(arithmetic_and_precedence) {
   expect_num(*this, "30.0;", 30.);
   expect_num(*this, "(20.0 + 10.);", 30.);
   expect_num(*this, "(20. - 10.);", 10.);
   expect_num(*this, "(2.0 * 10.);", 20.);
   expect_num(*this, "(20. / 10.);", 2.);
   expect_num(*this, "3. + 3. + 1.;", 7.);
   expect_num(*this, "2. + 2 * 2 * 2 - 2 / 2 - 2 / 2;", 8.);
   expect_num(*this, "(((20. - 10.)));", 10.);
   expect_num(*this, "(-20.0 + -10)/3. + 3. /3.+1.;", -8.);
   expect_num(*this, "-(20. - 10.0)/5.;", -2.);
   expect_num(*this, "2 - -3;", 5.);
}

BOOST_AUTO_TEST_CASE(power_functions) {
   expect_num(*this, "sqrt(9.);", 3.);
   expect_num(*this, "(sqrt(3.0))^2.;", 3.);
   expect_num(*this, "sqrt(3^2.);", 3.);
   expect_num(*this, "2^3;", 8.);
   expect_num(*this, "exp(0.);", 1.);
   expect_num(*this, "exp(ln(9));", 9.);
   expect_num(*this, "ln(exp(3));", 3.);
   expect_num(*this, "log(100.);", 2.);          // log is base 10
   expect_num(*this, "10^log(3.);", 3.);
}

BOOST_AUTO_TEST_CASE(min_max_ifelse) {
   expect_num(*this, "max2(3.,2.);", 3.);
   expect_num(*this, "min2(2.,3.);", 2.);
   expect_num(*this, "max3(3.,1.,4.);", 4.);
   expect_num(*this, "max3(4.,1.,3.);", 4.);
   expect_num(*this, "min3(3.,1.,4.);", 1.);
   expect_num(*this, "min3(1.,3.,4.);", 1.);
   expect_num(*this, "IFELSE( 2>3, 5, 6);", 6.);
   expect_num(*this, "IFELSE( 3>2, 5, 6);", 5.);
   expect_num(*this, "IFELSE( 3>2, IFELSE(1>2, 7, 8), 6);", 8.);   // nested
   expect_num(*this, "IFELSE( true AND NOT false, 1, 2);", 1.);
}

BOOST_AUTO_TEST_CASE(lookup_tables) {
   // levels has one more entry than values; value i applies from levels[i] up to (not including) levels[i+1]
   expect_num(*this, "lookup(3.,[1.,3.,7.],[1.,4.]);", 4.);
   expect_num(*this, "lookup(2.,[1.,3.,9.],[1.,4.]);", 1.);
   expect_num(*this, "lookup(60.,[10.,30.,90.],[1.,4.]);", 4.);
   expect_num(*this, "lookup(10.,[10.,30.,90.],[1.,4.]);", 1.);   // lower bound is inclusive
}

BOOST_AUTO_TEST_CASE(lookup_errors) {
   // wrong array lengths and non-monotonic levels are rejected when the node is built (inside the parse)
   BOOST_CHECK_THROW(parse("lookup(60.,[10.,30.],[1.,4.]);"), std::domain_error);
   BOOST_CHECK_THROW(parse("lookup(60.,[10.,30.,55.],[1.,4.,3.]);"), std::domain_error);
   BOOST_CHECK_THROW(parse("lookup(60.,[30.,10.,55.],[1.,4.]);"), std::domain_error);
   // an argument outside the table is rejected when the expression is evaluated
   BOOST_REQUIRE_EQUAL(parse("lookup(5.,[10.,30.,90.],[1.,4.]);"), 0);
   BOOST_CHECK_THROW(num(), std::domain_error);
   BOOST_REQUIRE_EQUAL(parse("lookup(95.,[10.,30.,90.],[1.,4.]);"), 0);
   BOOST_CHECK_THROW(num(), std::domain_error);
}

BOOST_AUTO_TEST_SUITE_END()

// ===================================================================== grammar: booleans

BOOST_FIXTURE_TEST_SUITE(grammar_boolean, ParserFixture)

BOOST_AUTO_TEST_CASE(comparisons) {
   expect_bool(*this, "(20.0 + 10.) <=30.;", true);
   expect_bool(*this, "(20.0 + 10.) >=30.;", true);
   expect_bool(*this, "(20.0 + 10.) ==30.;", true);
   expect_bool(*this, "(20.0 + 10.) <> 30.;", false);
   expect_bool(*this, "(20.0 + 10.) <30.;", false);
   expect_bool(*this, "(20.0 + 10.) >30.;", false);
   expect_bool(*this, "3+2+1 == 1+2+3;", true);
}

BOOST_AUTO_TEST_CASE(logic_and_precedence) {
   expect_bool(*this, "true AND true AND true OR false;", true);
   expect_bool(*this, "false OR (true AND true AND true);", true);
   expect_bool(*this, "NOT(false OR false);", true);
   expect_bool(*this, "NOT false AND false;", false);          // NOT binds tighter than AND
   expect_bool(*this, "true OR false AND false;", true);       // AND binds tighter than OR
   expect_bool(*this, "(1+2 < 4) AND (2*2 == 4);", true);
   expect_bool(*this, "STARTUP;", true);                       // STARTUP is lexed as TRUE
   expect_bool(*this, "FALSE;", false);
}

BOOST_AUTO_TEST_CASE(type_errors) {
   expect_parse_error(*this, "true + 1.;");
   expect_parse_error(*this, "IFELSE(1, 2, 3);");
   expect_parse_error(*this, "1. AND 2.;");
}

BOOST_AUTO_TEST_SUITE_END()

// ============================================================== grammar: names and time

BOOST_FIXTURE_TEST_SUITE(grammar_names_and_time, ParserFixture)

BOOST_AUTO_TEST_CASE(named_numeric_and_boolean_expressions) {
   BOOST_REQUIRE_EQUAL(parse("A:= 7.25+3.75;"), 0);
   BOOST_CHECK_CLOSE(num("A"), 11., 1e-9);
   BOOST_REQUIRE_EQUAL(parse("B:= A+1.;"), 0);
   BOOST_CHECK_CLOSE(num("B"), 12., 1e-9);
   BOOST_REQUIRE_EQUAL(parse("ABool:= (3.*A== 33.);"), 0);
   BOOST_CHECK(boolean("ABool"));
   BOOST_REQUIRE_EQUAL(parse("BBool:= ABool AND (B+B + 2*B)==48.;"), 0);
   BOOST_CHECK(boolean("BBool"));
   BOOST_REQUIRE_EQUAL(parse("CBool:= NOT ABool OR (B>100.);"), 0);
   BOOST_CHECK(!boolean("CBool"));
   BOOST_REQUIRE_EQUAL(parse("DBool:= BBool;"), 0);
   BOOST_CHECK(boolean("DBool"));
}

BOOST_AUTO_TEST_CASE(named_expression_tracks_live_model_values) {
   g_vars["x"] = 1.;
   BOOST_REQUIRE_EQUAL(parse("live := mock_ro(name=x) * 2;"), 0);
   BOOST_CHECK_CLOSE(num("live"), 2., 1e-9);
   g_vars["x"] = 5.;
   BOOST_CHECK_CLOSE(num("live"), 10., 1e-9);   // evaluated lazily, not at parse time
}

BOOST_AUTO_TEST_CASE(boolean_name_cannot_be_used_as_a_number) {
   BOOST_REQUIRE_EQUAL(parse("flag := 1 < 2;"), 0);
   expect_parse_error(*this, "flag + 1.;");
}

BOOST_AUTO_TEST_CASE(redefining_a_name_is_reported) {
   BOOST_REQUIRE_EQUAL(parse("A:= 1.;"), 0);
   int rc = parse("A:= 2.;");
   std::cout << "[CHAR] redefinition: rc=" << rc << " parsed_type=" << get_parsed_type() << std::endl;
   BOOST_CHECK(rc != 0 || get_parsed_type() == oprule::parser::REASSIGNMENT);
   BOOST_CHECK_CLOSE(num("A"), 1., 1e-9);       // original definition kept
}

BOOST_AUTO_TEST_CASE(time_terms_use_the_time_factory) {
   // RecordingTimeFactory: year 2001, month 4, day 15, hour 7, minute 1, minute-of-day 421, dt 900 s
   expect_num(*this, "YEAR;", 2001.);
   expect_num(*this, "MONTH;", 4.);
   expect_num(*this, "DAY;", 15.);
   expect_num(*this, "HOUR;", 7.);
   expect_num(*this, "MIN;", 1.);
   expect_num(*this, "DT;", 900.);
   expect_bool(*this, "MONTH <= APR;", true);
   expect_bool(*this, "MONTH == APR;", true);
   expect_bool(*this, "MONTH > MAR AND MONTH < MAY;", true);
   expect_num(*this, "DEC;", 12.);              // month names are numbers (upper case; see pinned for lower case)
}

BOOST_AUTO_TEST_CASE(date_literals_reach_the_factory_as_text) {
   BOOST_REQUIRE_EQUAL(parse("DATETIME >= 28SEP1992 00:00;"), 0);
   BOOST_CHECK_EQUAL(timeFactory->dateArg, "28SEP1992");
   BOOST_CHECK_EQUAL(timeFactory->timeArg, "00:00");
   BOOST_REQUIRE_EQUAL(parse("DATE == 01JAN2004;"), 0);
   BOOST_CHECK_EQUAL(timeFactory->dateArg, "01JAN2004");
   BOOST_CHECK_EQUAL(timeFactory->timeArg, "00:00");     // time defaults to midnight
   BOOST_REQUIRE_EQUAL(parse("DATETIME < 20sep2018 12:30;"), 0);
   BOOST_CHECK_EQUAL(timeFactory->dateArg, "20sep2018");
   BOOST_CHECK_EQUAL(timeFactory->timeArg, "12:30");
}

BOOST_AUTO_TEST_CASE(season_literals_reach_the_factory_as_numbers) {
   BOOST_REQUIRE_EQUAL(parse("SEASON > 16MAY;"), 0);
   BOOST_CHECK_EQUAL(timeFactory->mon, 5);
   BOOST_CHECK_EQUAL(timeFactory->day, 16);
   BOOST_CHECK_EQUAL(timeFactory->hour, 0);
   BOOST_CHECK_EQUAL(timeFactory->min, 0);
   BOOST_REQUIRE_EQUAL(parse("SEASON < 01DEC;"), 0);
   BOOST_CHECK_EQUAL(timeFactory->mon, 12);
   BOOST_CHECK_EQUAL(timeFactory->day, 1);
}

BOOST_AUTO_TEST_SUITE_END()

// ============================================================================ lexer

BOOST_FIXTURE_TEST_SUITE(lexer, ParserFixture)

BOOST_AUTO_TEST_CASE(whitespace_is_ignored) {
   expect_num(*this, "  2   +\n 3  ;", 5.);
   BOOST_CHECK_EQUAL(parse("mock_var( name = tgt ) > 1.;"), 0);   // spaces around '=' in arguments
}

BOOST_AUTO_TEST_CASE(number_formats) {
   expect_num(*this, "1.5E-3;", 1.5e-3);
   expect_num(*this, "1.0e5;", 1.0e5);
   expect_num(*this, ".5;", 0.5);
   expect_num(*this, "5.;", 5.);
   expect_num(*this, "007;", 7.);
}

BOOST_AUTO_TEST_CASE(keywords_in_any_case) {
   expect_bool(*this, "True And TRUE;", true);
   expect_bool(*this, "not FALSE or false;", true);
   expect_num(*this, "Sqrt(4.) + MAX2(1.,2.) + Ifelse(true,1,0);", 5.);
   BOOST_CHECK_EQUAL(parse("r1 := set mock_var(name=tgt) to 5 when true;"), 0);
}

BOOST_AUTO_TEST_CASE(model_name_arguments) {
   BOOST_CHECK_EQUAL(parse("mock_var(name=tgt) > 1.;"), 0);
   BOOST_CHECK_EQUAL(parse("mock_var(name='quoted name') > 1.;"), 0);       // single quotes, spaces allowed
   BOOST_CHECK_EQUAL(parse("mock_var(name=\"dq\") > 1.;"), 0);             // double quotes
   BOOST_CHECK_EQUAL(parse("mock_var(name=old_r@tracy_barrier) > 1.;"), 0); // '@' is allowed in bare names
   BOOST_CHECK_EQUAL(parse("mock_var(name=tgt;) > 1.;") == 0, false);       // trailing ';' inside the list
}

BOOST_AUTO_TEST_CASE(argument_separators) {
   // arguments may be separated by ',' or ';' (the DSM2 inputs use ',')
   BOOST_CHECK_EQUAL(parse("mock_var(name=tgt, other=1) > 1.;"), 0);
   BOOST_CHECK_EQUAL(parse("mock_var(name=tgt; other=1) > 1.;"), 0);
}

BOOST_AUTO_TEST_CASE(unknown_names_and_bad_tokens) {
   expect_parse_error(*this, "nosuchname + 1.;");
   expect_parse_error(*this, "MOCK_VAR(name=tgt) > 1.;");   // model names are case sensitive
   expect_parse_error(*this, "a = b;");                      // '=' only separates arguments
   expect_parse_error(*this, "1 != 2;");                     // not-equal is <>
   expect_parse_error(*this, "mock_var(name='1abc') > 1.;"); // quoted names must start with a letter
}

BOOST_AUTO_TEST_CASE(date_and_time_literal_shapes) {
   BOOST_CHECK_EQUAL(parse("DATETIME >= 01JAN2004;"), 0);
   BOOST_CHECK_EQUAL(parse("DATETIME >= 01jan2004 00:00;"), 0);
   expect_parse_error(*this, "DATETIME >= 1JAN2004;");        // day needs two digits
}

BOOST_AUTO_TEST_CASE(ramp_forms) {
   BOOST_CHECK_EQUAL(parse("r1 := SET mock_var(name=a) TO 1 RAMP 60MIN WHEN true;"), 0);
   BOOST_CHECK_EQUAL(parse("r2 := SET mock_var(name=a) TO 1 RAMP 60 MIN WHEN true;"), 0);
   BOOST_CHECK_EQUAL(parse("r3 := SET mock_var(name=a) TO 1 RAMP 0MIN WHEN true;"), 0);
   BOOST_CHECK_EQUAL(parse("r4 := SET mock_var(name=a) TO 1 RAMP 0.5MIN WHEN true;"), 0);
   expect_parse_error(*this, "r5 := SET mock_var(name=a) TO 1 RAMP 1HOUR WHEN true;");   // minutes only
   expect_parse_error(*this, "r6 := SET mock_var(name=a) TO 1 RAMP 60 WHEN true;");
}

BOOST_AUTO_TEST_CASE(reserved_words_cannot_be_names) {
   const char* reserved[] = {"min", "dt", "day", "month", "year", "hour", "season", "date", "set", "to", "when",
                             "while", "then", "ramp", "or", "and", "not", "lookup", "log", "ln", "exp", "abs",
                             "sqrt", "t", "dec"};
   for (size_t i = 0; i < sizeof(reserved) / sizeof(reserved[0]); ++i)
      expect_parse_error(*this, std::string(reserved[i]) + " := 1.;");
   // names that merely start with a reserved word or "t" are fine
   BOOST_CHECK_EQUAL(parse("tom_paine := 1.;"), 0);
   BOOST_CHECK_EQUAL(parse("tt := 1.;"), 0);
   BOOST_CHECK_EQUAL(parse("minimum := 1.;"), 0);
   BOOST_CHECK_EQUAL(parse("daylight := 1.;"), 0);
   BOOST_CHECK_EQUAL(parse("old_r@tracy := 1.;"), 0);
}

BOOST_AUTO_TEST_SUITE_END()

// =============================================================== expression nodes with state

BOOST_FIXTURE_TEST_SUITE(stateful_nodes, ParserFixture)

BOOST_AUTO_TEST_CASE(accumulate_adds_the_expression_every_step) {
   // ACCUMULATE(expression, initial [, reset condition]); no scaling by dt (see pinned/accumulate_ignores_dt)
   BOOST_REQUIRE_EQUAL(parse("ACCUMULATE(mock_ro(name=x), 0.);"), 0);
   oprule::expression::DoubleNodePtr n = getDoubleExpression();
   n->init();
   BOOST_CHECK_SMALL(n->eval(), 1e-12);
   g_vars["x"] = 1.; n->step(DT);
   BOOST_CHECK_CLOSE(n->eval(), 1., 1e-9);
   g_vars["x"] = 2.; n->step(DT);
   BOOST_CHECK_CLOSE(n->eval(), 3., 1e-9);
   g_vars["x"] = 3.; n->step(DT);
   BOOST_CHECK_CLOSE(n->eval(), 6., 1e-9);
}

BOOST_AUTO_TEST_CASE(accumulate_with_reset_condition) {
   BOOST_REQUIRE_EQUAL(parse("ACCUMULATE(3., 1., mock_ro(name=reset) > 0);"), 0);
   oprule::expression::DoubleNodePtr n = getDoubleExpression();
   n->init();
   BOOST_CHECK_CLOSE(n->eval(), 1., 1e-9);
   n->step(DT);  BOOST_CHECK_CLOSE(n->eval(), 4., 1e-9);
   n->step(DT);  BOOST_CHECK_CLOSE(n->eval(), 7., 1e-9);
   g_vars["reset"] = 1.;
   n->step(DT);  BOOST_CHECK_CLOSE(n->eval(), 4., 1e-9);   // reset to the initial value, then the step is added
   g_vars["reset"] = 0.;
   n->step(DT);  BOOST_CHECK_CLOSE(n->eval(), 7., 1e-9);
}

BOOST_AUTO_TEST_CASE(predict_linear_extrapolates_by_the_requested_time) {
   // predicted = new + (new - old) * (time / dt)
   BOOST_REQUIRE_EQUAL(parse("PREDICT(mock_ro(name=y), LINEAR, 15MIN);"), 0);
   oprule::expression::DoubleNodePtr n = getDoubleExpression();
   g_vars["y"] = 0.;
   n->init();
   g_vars["y"] = 1.; n->step(900.);
   BOOST_CHECK_CLOSE(n->eval(), 2., 1e-9);
   g_vars["y"] = 3.; n->step(900.);
   BOOST_CHECK_CLOSE(n->eval(), 5., 1e-9);

   BOOST_REQUIRE_EQUAL(parse("PREDICT(mock_ro(name=y), LINEAR, 30MIN);"), 0);
   n = getDoubleExpression();
   g_vars["y"] = 0.;
   n->init();
   g_vars["y"] = 1.; n->step(900.);
   BOOST_CHECK_CLOSE(n->eval(), 3., 1e-9);   // two steps ahead
}

BOOST_AUTO_TEST_CASE(predict_quad_continues_a_parabola) {
   BOOST_REQUIRE_EQUAL(parse("PREDICT(mock_ro(name=y), QUAD, 15MIN);"), 0);
   oprule::expression::DoubleNodePtr n = getDoubleExpression();
   g_vars["y"] = 0.;
   n->init();
   g_vars["y"] = 1.; n->step(900.);
   g_vars["y"] = 4.; n->step(900.);
   BOOST_CHECK_CLOSE(n->eval(), 9., 1e-9);   // 0, 1, 4 -> 9
}

BOOST_AUTO_TEST_CASE(pid_output_is_bounded_and_responds) {
   // PID(y, setpoint, ulow, uhigh, k, ti, td, tt, b): bounds and gains must be constants
   BOOST_REQUIRE_EQUAL(parse("PID(mock_ro(name=y), mock_ro(name=sp), 0, 1, 1, 1, 1, 1, 1);"), 0);
   oprule::expression::DoubleNodePtr n = getDoubleExpression();
   g_vars["y"] = 0.; g_vars["sp"] = 1.;
   n->step(DT);
   BOOST_CHECK_CLOSE(n->eval(), 1., 1e-9);       // full error saturates at uhigh
   for (int i = 0; i < 20; ++i) {
      g_vars["y"] = 0.1 * i;
      n->step(DT);
      BOOST_CHECK(n->eval() >= 0. && n->eval() <= 1.);
   }
   g_vars["y"] = 5.; n->step(DT);                // far above the setpoint: still within bounds
   BOOST_CHECK(n->eval() >= 0. && n->eval() <= 1.);
}

BOOST_AUTO_TEST_CASE(incremental_pid_parses_and_steps) {
   BOOST_REQUIRE_EQUAL(parse("IPID(mock_ro(name=y), mock_ro(name=sp), mock_ro(name=u0), 0, 1, 1, 1, 1, 1);"), 0);
   oprule::expression::DoubleNodePtr n = getDoubleExpression();
   g_vars["y"] = 0.; g_vars["sp"] = 1.; g_vars["u0"] = 0.5;
   n->step(DT);
   BOOST_CHECK(std::isfinite(n->eval()));
}

BOOST_AUTO_TEST_SUITE_END()

// =============================================================== THEN / WHILE nesting by timing
// Each rule below is "WHEN true", so it activates at the end of step 1 and its first advance is step 2.
// Ramps are 15-minute steps: RAMP 30MIN = 2 advances, RAMP 60MIN = 4 advances.

BOOST_FIXTURE_TEST_SUITE(rule_structure, RuleBench)

BOOST_AUTO_TEST_CASE(while_runs_actions_in_parallel) {
   // equal durations only: unequal ones trip an assertion (see pinned_runtime)
   add("r", "SET mock_var(name=a) TO 1 RAMP 60MIN WHILE SET mock_var(name=b) TO 2 RAMP 60MIN", "true");
   run(1);
   BOOST_CHECK(getOperatingRule("r")->isActive());
   run(1);
   BOOST_CHECK_CLOSE(g_vars["a"], 0.25, 1e-9);
   BOOST_CHECK_CLOSE(g_vars["b"], 0.5, 1e-9);
   run(3);
   BOOST_CHECK_CLOSE(g_vars["a"], 1., 1e-9);
   BOOST_CHECK_CLOSE(g_vars["b"], 2., 1e-9);
   BOOST_CHECK(!getOperatingRule("r")->isActive());
}

BOOST_AUTO_TEST_CASE(abrupt_while_pair_like_the_dcc_gate_rule) {
   add("r", "SET mock_var(name=up) TO 0.5 WHILE SET mock_var(name=down) TO 0.5", "true");
   run(2);
   BOOST_CHECK_CLOSE(g_vars["up"], 0.5, 1e-9);
   BOOST_CHECK_CLOSE(g_vars["down"], 0.5, 1e-9);
   BOOST_CHECK(!getOperatingRule("r")->isActive());
}

BOOST_AUTO_TEST_CASE(then_binds_tighter_than_while) {
   // A(60) WHILE B(30) THEN C(30)  ==  A WHILE (B THEN C): finishes after 4 advances, not 6
   add("r", "SET mock_var(name=a) TO 1 RAMP 60MIN WHILE SET mock_var(name=b) TO 1 RAMP 30MIN THEN SET mock_var(name=c) TO 1 RAMP 30MIN", "true");
   run(5);                                            // activate + 4 advances
   BOOST_CHECK_CLOSE(g_vars["a"], 1., 1e-9);
   BOOST_CHECK_CLOSE(g_vars["c"], 1., 1e-9);
   BOOST_CHECK(!getOperatingRule("r")->isActive());
}

BOOST_AUTO_TEST_CASE(then_chain_inside_a_while_group) {
   // (A(30) THEN B(30)) WHILE C(60): everything finishes after 4 advances
   add("r", "SET mock_var(name=a) TO 1 RAMP 30MIN THEN SET mock_var(name=b) TO 1 RAMP 30MIN WHILE SET mock_var(name=c) TO 1 RAMP 60MIN", "true");
   run(4);
   BOOST_CHECK(getOperatingRule("r")->isActive());    // b and c still running after 3 advances
   run(1);
   BOOST_CHECK_CLOSE(g_vars["b"], 1., 1e-9);
   BOOST_CHECK_CLOSE(g_vars["c"], 1., 1e-9);
   BOOST_CHECK(!getOperatingRule("r")->isActive());
}

BOOST_AUTO_TEST_CASE(set_target_can_be_an_expression_of_other_variables) {
   g_vars["src"] = 4.;
   add("r", "SET mock_var(name=a) TO mock_ro(name=src) * 2 + 1", "true");
   run(2);
   BOOST_CHECK_CLOSE(g_vars["a"], 9., 1e-9);
}

BOOST_AUTO_TEST_SUITE_END()

// A rule whose TOP-LEVEL action is a THEN chain cannot be added to an OperationManager (see
// pinned_runtime/adding_a_chain_rule_to_the_manager), so these tests drive the rule directly:
// activate it, then call advanceAction once per step. They check the chain semantics themselves.

BOOST_FIXTURE_TEST_SUITE(top_level_chains, ParserFixture)

namespace {
OperatingRulePtr parse_rule_only(ParserFixture& p, const std::string& action) {
   BOOST_REQUIRE_EQUAL(p.parse("r := " + action + " WHEN true;"), 0);
   OperatingRulePtr r = getOperatingRule("r");
   r->setActive(true);
   return r;
}
void advance(OperatingRulePtr r, int n) { for (int i = 0; i < n; ++i) r->advanceAction(DT); }
}  // namespace

BOOST_AUTO_TEST_CASE(then_runs_actions_in_sequence) {
   OperatingRulePtr r = parse_rule_only(*this, "SET mock_var(name=a) TO 1 RAMP 30MIN THEN SET mock_var(name=b) TO 1 RAMP 30MIN");
   advance(r, 1);
   BOOST_CHECK_CLOSE(g_vars["a"], 0.5, 1e-9);
   BOOST_CHECK_SMALL(g_vars["b"], 1e-12);             // b has not started
   advance(r, 1);                                     // a completes; b starts (and is advanced by 0)
   BOOST_CHECK_CLOSE(g_vars["a"], 1., 1e-9);
   BOOST_CHECK_SMALL(g_vars["b"], 1e-12);
   advance(r, 1);
   BOOST_CHECK_CLOSE(g_vars["b"], 0.5, 1e-9);
   BOOST_CHECK(r->isActive());
   advance(r, 1);
   BOOST_CHECK_CLOSE(g_vars["b"], 1., 1e-9);
   BOOST_CHECK(!r->isActive());
}

BOOST_AUTO_TEST_CASE(abrupt_chain_completes_in_a_single_advance) {
   OperatingRulePtr r = parse_rule_only(*this, "SET mock_var(name=a) TO 1 THEN SET mock_var(name=b) TO 2 THEN SET mock_var(name=c) TO 3");
   advance(r, 1);
   BOOST_CHECK_CLOSE(g_vars["a"], 1., 1e-9);
   BOOST_CHECK_CLOSE(g_vars["b"], 2., 1e-9);
   BOOST_CHECK_CLOSE(g_vars["c"], 3., 1e-9);
   BOOST_CHECK(!r->isActive());
}

BOOST_AUTO_TEST_CASE(parentheses_override_precedence) {
   // (A(60) WHILE B(60)) THEN C(30): C starts only after both finish (4 advances), so 6 in total
   OperatingRulePtr r = parse_rule_only(*this,
      "(SET mock_var(name=a) TO 1 RAMP 60MIN WHILE SET mock_var(name=b) TO 1 RAMP 60MIN) THEN SET mock_var(name=c) TO 1 RAMP 30MIN");
   advance(r, 4);
   BOOST_CHECK_CLOSE(g_vars["a"], 1., 1e-9);
   BOOST_CHECK_CLOSE(g_vars["b"], 1., 1e-9);
   BOOST_CHECK_SMALL(g_vars["c"], 1e-12);
   BOOST_CHECK(r->isActive());
   advance(r, 2);
   BOOST_CHECK_CLOSE(g_vars["c"], 1., 1e-9);
   BOOST_CHECK(!r->isActive());
}

BOOST_AUTO_TEST_SUITE_END()

// ================================================================================ runtime

BOOST_FIXTURE_TEST_SUITE(runtime, RuleBench)

BOOST_AUTO_TEST_CASE(trigger_expression_reads_the_model) {
   add("r", "SET mock_var(name=a) TO 1", "mock_ro(name=level) > 2.0");
   g_vars["level"] = 1.; run(3);
   BOOST_CHECK_SMALL(g_vars["a"], 1e-12);
   g_vars["level"] = 3.; run(1);                      // true at the end of this step ...
   BOOST_CHECK_SMALL(g_vars["a"], 1e-12);
   run(1);                                            // ... so applied in the next
   BOOST_CHECK_CLOSE(g_vars["a"], 1., 1e-9);
}

BOOST_AUTO_TEST_CASE(hysteresis_pair_like_the_dicu_rules) {
   // off below 2.0, on above 2.2, nothing in between
   add("off", "SET mock_var(name=q) TO 0", "mock_ro(name=level) <= 2.0");
   add("on", "SET mock_var(name=q) TO 5", "mock_ro(name=level) > 2.2");
   g_vars["level"] = 2.1; g_vars["q"] = 5.;
   run(3);
   BOOST_CHECK_CLOSE(g_vars["q"], 5., 1e-9);          // dead band: untouched
   g_vars["level"] = 1.9; run(2);
   BOOST_CHECK_SMALL(g_vars["q"], 1e-12);
   g_vars["level"] = 2.1; run(2);
   BOOST_CHECK_SMALL(g_vars["q"], 1e-12);             // still off in the dead band
   g_vars["level"] = 2.3; run(2);
   BOOST_CHECK_CLOSE(g_vars["q"], 5., 1e-9);
   g_vars["level"] = 1.0; run(2);                     // each excursion fires once more
   BOOST_CHECK_SMALL(g_vars["q"], 1e-12);
}

BOOST_AUTO_TEST_CASE(ramp_longer_than_a_step_and_shorter_than_a_step) {
   add("slow", "SET mock_var(name=a) TO 10 RAMP 60MIN", "true");
   run(2);
   BOOST_CHECK_CLOSE(g_vars["a"], 2.5, 1e-9);
   BOOST_CHECK(getOperatingRule("slow")->isActive());

   add("fast", "SET mock_var(name=b) TO 10 RAMP 5MIN", "true");   // shorter than the 15 min step
   run(2);                                            // activates at the end of the first of these
   BOOST_CHECK_CLOSE(g_vars["b"], 10., 1e-9);         // full value in one advance
   BOOST_CHECK(!getOperatingRule("fast")->isActive());
}

BOOST_AUTO_TEST_CASE(ramp_zero_minutes_is_abrupt) {
   add("r", "SET mock_var(name=a) TO 10 RAMP 0MIN", "true");
   run(2);
   BOOST_CHECK_CLOSE(g_vars["a"], 10., 1e-9);
   BOOST_CHECK(!getOperatingRule("r")->isActive());
}

BOOST_AUTO_TEST_CASE(ramp_target_is_reevaluated_each_step) {
   g_vars["tgt"] = 10.;
   add("r", "SET mock_var(name=a) TO mock_ro(name=tgt) RAMP 60MIN", "true");
   run(1);
   run(1);                                            // f = 0.25, target 10 -> 2.5
   BOOST_CHECK_CLOSE(g_vars["a"], 2.5, 1e-9);
   g_vars["tgt"] = 20.;
   run(1);                                            // f = 0.5 -> 0 * 0.5 + 20 * 0.5 (base is the activation value 0)
   BOOST_CHECK_CLOSE(g_vars["a"], 10., 1e-9);
}

BOOST_AUTO_TEST_CASE(static_target_ramps_from_its_value_at_activation) {
   g_vars["a"] = 0.;
   add("r", "SET mock_var(name=a) TO 10 RAMP 60MIN", "true");
   run(1);                                            // activates: snapshot a = 0
   g_vars["a"] = 100.;                                // model changes the variable before the first advance
   run(1);
   BOOST_CHECK_CLOSE(g_vars["a"], 2.5, 1e-9);         // base remained the snapshot
}

BOOST_AUTO_TEST_CASE(time_dependent_target_ramps_from_the_value_loaded_each_step) {
   // In DSM2 the "loaded" value comes from the target's data source (e.g. a DSS time series).
   add("r", "SET mock_tvar(name=tx) TO 10 RAMP 60MIN", "true");
   g_vars["tx"] = 100.;
   run(1);
   g_vars["tx"] = 102.; run(1);                       // f = 0.25: 102 * 0.75 + 10 * 0.25
   BOOST_CHECK_CLOSE(g_vars["tx"], 79., 1e-9);
   g_vars["tx"] = 103.; run(1);                       // f = 0.5: 103 * 0.5 + 10 * 0.5
   BOOST_CHECK_CLOSE(g_vars["tx"], 56.5, 1e-9);
   g_vars["tx"] = 104.; run(2);                       // completes on the 4th advance
   BOOST_CHECK(!getOperatingRule("r")->isActive());
   BOOST_CHECK(g_datasource_set.count("tx") == 1);    // the expression is now the permanent source
}

BOOST_AUTO_TEST_CASE(re_arming_restarts_from_the_current_value) {
   g_vars["lvl"] = 1.;
   add("r", "SET mock_var(name=a) TO mock_ro(name=target) RAMP 30MIN", "mock_ro(name=lvl) > 0");
   g_vars["target"] = 10.;
   run(4);                                            // fires, ramps 5, completes at 10
   BOOST_CHECK_CLOSE(g_vars["a"], 10., 1e-9);
   g_vars["lvl"] = 0.; run(1);                        // trigger false for one test
   g_vars["target"] = 0.; g_vars["lvl"] = 1.; run(1); // new rising edge
   run(1);
   BOOST_CHECK_CLOSE(g_vars["a"], 5., 1e-9);          // from 10 toward 0, halfway after 1 advance
   run(1);
   BOOST_CHECK_SMALL(g_vars["a"], 1e-12);
}

BOOST_AUTO_TEST_CASE(every_rule_is_stepped_even_when_inactive) {
   RuleHandle h("x", 1., 0.);
   manager.addRule(h.rule);
   int before = h.trigger->steps;
   run(3);
   BOOST_CHECK_EQUAL(h.trigger->steps - before, 3);
}

BOOST_AUTO_TEST_CASE(rules_on_different_variables_do_not_interact) {
   add("r1", "SET mock_var(name=a) TO 1 RAMP 60MIN", "true");
   add("r2", "SET mock_var(name=b) TO 2 RAMP 60MIN", "true");
   run(2);
   BOOST_CHECK(getOperatingRule("r1")->isActive());
   BOOST_CHECK(getOperatingRule("r2")->isActive());
}

BOOST_AUTO_TEST_CASE(overlapping_rules_defer_in_pool_order) {
   // both trigger on the same step; the one added first wins, the other is retried until it can run
   add("first", "SET mock_var(name=a) TO 1 RAMP 30MIN", "true");
   add("second", "SET mock_var(name=a) TO 5", "true");
   run(1);
   BOOST_CHECK(getOperatingRule("first")->isActive());
   BOOST_CHECK(!getOperatingRule("second")->isActive());
   run(3);                                            // first completes after two advances
   BOOST_CHECK(!getOperatingRule("first")->isActive());
   run(1);
   BOOST_CHECK_CLOSE(g_vars["a"], 5., 1e-9);          // second ran afterwards, so its value is final
}

BOOST_AUTO_TEST_CASE(priority_is_defer_or_compatible_only) {
   RuleHandle a("x", 1., 3600.), b("x", 2., 0.), c("y", 3., 0.);
   BOOST_CHECK_EQUAL(manager.checkActionPriority(*a.rule, *b.rule), oprule::rule::ActionResolver::DEFER_NEW_RULE);
   BOOST_CHECK_EQUAL(manager.checkActionPriority(*a.rule, *c.rule), oprule::rule::ActionResolver::RULES_COMPATIBLE);
   BOOST_CHECK(manager.actionsOverlap(*a.rule, *b.rule));
   BOOST_CHECK(!manager.actionsOverlap(*a.rule, *c.rule));
}

BOOST_AUTO_TEST_SUITE_END()

// ================================================== manager policies that nothing uses yet
// OperationManager supports REPLACE_OLD_RULE and IGNORE_NEW_RULE, but checkActionPriority never
// returns them. These tests subclass the manager to prove the code paths work, for future policies.

namespace {

class PolicyManager : public oprule::rule::OperationManager {
public:
   typedef oprule::rule::ActionResolver::RulePriority RulePriority;
   PolicyManager(oprule::rule::ActionResolver& r, RulePriority outcome)
      : oprule::rule::OperationManager(r), old_rule(0), new_rule(0), outcome_(outcome) {}
   virtual RulePriority checkActionPriority(oprule::rule::OperatingRule& oldR, oprule::rule::OperatingRule& newR) {
      if (&oldR == old_rule && &newR == new_rule) return outcome_;
      return oprule::rule::ActionResolver::RULES_COMPATIBLE;
   }
   oprule::rule::OperatingRule* old_rule;
   oprule::rule::OperatingRule* new_rule;
private:
   RulePriority outcome_;
};

}  // namespace

BOOST_AUTO_TEST_SUITE(alternative_conflict_policies)

BOOST_AUTO_TEST_CASE(replace_old_rule_stops_the_old_rule_and_starts_the_new_one) {
   RuntimeFixture f;
   PolicyManager mgr(f.resolver, oprule::rule::ActionResolver::REPLACE_OLD_RULE);
   RuleHandle a("x", 10., 3600.), b("x", 99., 0.);
   mgr.old_rule = a.rule.get();
   mgr.new_rule = b.rule.get();
   mgr.addRule(a.rule);
   mgr.addRule(b.rule);
   a.trigger->value = true;
   mgr.manageActivation();
   BOOST_REQUIRE(a.rule->isActive());
   b.trigger->value = true;
   mgr.manageActivation();
   BOOST_CHECK(!a.rule->isActive());
   BOOST_CHECK(b.rule->isActive());
}

BOOST_AUTO_TEST_CASE(ignore_new_rule_does_not_retry) {
   RuntimeFixture f;
   PolicyManager mgr(f.resolver, oprule::rule::ActionResolver::IGNORE_NEW_RULE);
   RuleHandle a("x", 10., 900.), b("x", 99., 0.);
   mgr.old_rule = a.rule.get();
   mgr.new_rule = b.rule.get();
   mgr.addRule(a.rule);
   mgr.addRule(b.rule);
   a.trigger->value = true;
   mgr.manageActivation();
   b.trigger->value = true;
   mgr.manageActivation();
   BOOST_CHECK(!b.rule->isActive());
   mgr.advanceActions(DT);                            // a completes
   mgr.manageActivation();
   BOOST_CHECK(!b.rule->isActive());                  // unlike deferral, b is not retried while its trigger stays true
   b.trigger->value = false; mgr.manageActivation();
   b.trigger->value = true;  mgr.manageActivation();
   BOOST_CHECK(b.rule->isActive());                   // a fresh rising edge is needed
}

BOOST_AUTO_TEST_SUITE_END()

// ============================================================================ pinned
// Behaviour that is believed to be a defect or limitation. Each test states what it pins.

namespace {
void add_chain_rule_to_manager() {
   RuntimeFixture f;
   oprule::rule::OperationActionPtr chain =
      chain_actions(make_action("x", 1., 0.), make_action("x", 2., 0.));
   oprule::rule::OperatingRulePtr rule(new oprule::rule::OperatingRule(chain, oprule::rule::TriggerPtr(new FlagTrigger)));
   f.manager.addRule(rule);   // OperationManager::addRule calls rule->setActive(false)
}

void run_predict_rule() {
   RuleBench b;
   b.add("r", "SET mock_var(name=a) TO 1", "PREDICT(mock_ro(name=y), LINEAR, 15MIN) > 0");
   b.run(2);
}

void run_unequal_while() {
   RuntimeFixture f;
   oprule::rule::OperationActionPtr set =
      group_actions(make_action("a", 1., 3600.), make_action("b", 1., 1800.));
   FlagTrigger* trig = new FlagTrigger;
   oprule::rule::OperatingRulePtr rule(new oprule::rule::OperatingRule(set, oprule::rule::TriggerPtr(trig)));
   f.manager.addRule(rule);
   trig->value = true;
   f.manager.manageActivation();
   for (int i = 0; i < 4; ++i) f.manager.advanceActions(DT);   // b finishes after 2 advances
}
}  // namespace

BOOST_FIXTURE_TEST_SUITE(pinned, ParserFixture)

// REFERENCE B9.1 / PLAN D-01: the hour of "SEASON == 01JAN 12:30" is dropped (hour is taken from the
// wrong substring of "HH:MM"); the minute is right. Flip the hour expectation to 12 when fixed.
BOOST_AUTO_TEST_CASE(seasonal_literal_with_time_loses_the_hour) {
   BOOST_REQUIRE_EQUAL(parse("SEASON == 01JAN 12:30;"), 0);
   BOOST_CHECK_EQUAL(timeFactory->min, 30);
   BOOST_CHECK_EQUAL(timeFactory->hour, 0);      // DEFECT: should be 12
}

// REFERENCE B9.11 / PLAN D-11: lagged expressions parse but cannot be evaluated (the throw is a pointer).
BOOST_AUTO_TEST_CASE(lagged_expressions_are_not_implemented) {
   BOOST_REQUIRE_EQUAL(parse("D:= C(t-1)+1.;"), 0);
   bool threw = false;
   try { num("D"); } catch (std::logic_error* e) { threw = true; delete e; }
   BOOST_CHECK(threw);
}

// REFERENCE B9.13 / PLAN D-14: accumulate adds the expression once per step; it does not multiply by dt.
BOOST_AUTO_TEST_CASE(accumulate_ignores_dt) {
   BOOST_REQUIRE_EQUAL(parse("ACCUMULATE(2., 0.);"), 0);
   oprule::expression::DoubleNodePtr n = getDoubleExpression();
   n->init();
   n->step(900.);
   n->step(900.);
   BOOST_CHECK_CLOSE(n->eval(), 4., 1e-9);       // 2 per step, not 2 * 900 per step
}

// Lexer: an exponent is only recognised after a decimal point.
BOOST_AUTO_TEST_CASE(exponent_needs_a_decimal_point) {
   expect_parse_error(*this, "1e5;");
}

// Grammar: unary minus takes the precedence of '-', so it binds looser than '^'.
BOOST_AUTO_TEST_CASE(unary_minus_binds_looser_than_power) {
   expect_num(*this, "-2^2;", -4.);
   expect_num(*this, "2^3^2;", 64.);             // '^' is left associative: (2^3)^2
}

// abs() of a double: the node uses the C abs; confirm it does not truncate to an integer.
BOOST_AUTO_TEST_CASE(abs_keeps_the_fraction) {
   BOOST_REQUIRE_EQUAL(parse("abs(-2.5);"), 0);
   double v = num();
   std::cout << "[CHAR] abs(-2.5) = " << v << std::endl;
   BOOST_CHECK_CLOSE(v, 2.5, 1e-9);
}

// PLAN PSM-02: after a failed parse the next parse needs a scanner restart.
BOOST_AUTO_TEST_CASE(failed_parse_needs_scanner_restart) {
   BOOST_CHECK(parse("2 + * 3; 4;") != 0);
   int raw = parse_no_restart("1. + 1.;");
   std::cout << "[CHAR] re-parse without restart: rc=" << raw << std::endl;
   BOOST_CHECK(raw != 0);
   BOOST_REQUIRE_EQUAL(parse("1. + 1.;"), 0);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_FIXTURE_TEST_SUITE(pinned_runtime, RuleBench)

// REFERENCE B6 / PLAN CFL-05, CFL-06 (D-07): chain actions are left out of a rule's action list, so a chain nested
// in a WHILE group cannot conflict with another rule even though it writes the same variable.
BOOST_AUTO_TEST_CASE(chain_inside_a_while_group_is_invisible_to_conflict_detection) {
   add("blocker", "SET mock_var(name=a) TO 1 RAMP 60MIN", "true");
   run(1);
   BOOST_REQUIRE(getOperatingRule("blocker")->isActive());
   add("grouped", "SET mock_var(name=a) TO 7 THEN SET mock_var(name=b) TO 8 WHILE SET mock_var(name=c) TO 9", "true");
   BOOST_CHECK_EQUAL(getOperatingRule("grouped")->getActionList().size(), 1u);   // only 'c' is listed
   run(2);                                            // activates at the end of the first step, writes in the second
   BOOST_CHECK_CLOSE(g_vars["c"], 9., 1e-9);
   BOOST_CHECK_CLOSE(g_vars["a"], 7., 1e-9);           // the grouped rule overwrote 'a', which "blocker" is ramping
}

// PLAN RUN-06 (D-16): the trigger is not tested while a rule is active, so a rising edge that happens
// during the action (or on the step it completes) is missed.
BOOST_AUTO_TEST_CASE(rising_edge_while_active_is_missed) {
   add("r", "SET mock_var(name=a) TO mock_ro(name=target) RAMP 30MIN", "mock_ro(name=lvl) > 0");
   g_vars["target"] = 10.; g_vars["lvl"] = 1.;
   run(1);                                            // fires (rising edge)
   g_vars["lvl"] = 0.; run(1);                        // trigger drops while the rule is running (not observed)
   g_vars["lvl"] = 1.; run(1);                        // and rises again; the rule completes on this step
   BOOST_CHECK(!getOperatingRule("r")->isActive());
   g_vars["target"] = 20.;
   run(6);
   BOOST_CHECK_CLOSE(g_vars["a"], 10., 1e-9);         // never re-fired: it would be 20 if the edge had been seen
}

// PLAN D-17: adding a rule whose top-level action is a THEN chain to the manager calls
// ActionChain::setActive(false), which dereferences an iterator that was never set. Run in a child
// process because it crashes. This makes top-level THEN unusable in the model.
BOOST_AUTO_TEST_CASE(adding_a_chain_rule_to_the_manager) {
   int status = run_in_child(&add_chain_rule_to_manager);
   std::cout << "[CHAR] addRule(chain rule) in child: exit status " << status << std::endl;
   BOOST_CHECK(status != 0);                          // DEFECT: crashes (a negative status is the signal number)
}

// PLAN D-22 (new): ActionSet::advance advances every child, including children that already finished.
// ModelAction::advance asserts it is active, so a WHILE of actions with different durations aborts in a
// Debug build (the build used by build_hpc5.sh); in a Release build the finished child is advanced again.
BOOST_AUTO_TEST_CASE(while_with_unequal_durations_advances_a_finished_child) {
   int status = run_in_child(&run_unequal_while);
   std::cout << "[CHAR] WHILE with unequal ramps in child: exit status " << status << std::endl;
#ifdef NDEBUG
   BOOST_CHECK_EQUAL(status, 0);
#else
   BOOST_CHECK(status != 0);                          // DEFECT: assertion failure (SIGABRT)
#endif
}

// PLAN D-23 (new): nothing calls init() on expression nodes, but PREDICT's eval() asserts that init() was
// called. A rule whose trigger uses PREDICT aborts in a Debug build on its first trigger test.
BOOST_AUTO_TEST_CASE(predict_in_a_trigger_asserts_because_init_is_never_called) {
   int status = run_in_child(&run_predict_rule);
   std::cout << "[CHAR] PREDICT in a trigger in child: exit status " << status << std::endl;
#ifdef NDEBUG
   BOOST_CHECK_EQUAL(status, 0);
#else
   BOOST_CHECK(status != 0);                          // DEFECT: assertion failure (SIGABRT)
#endif
}

BOOST_AUTO_TEST_SUITE_END()
