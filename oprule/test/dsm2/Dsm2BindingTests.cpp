// Tests of the REAL DSM2 oprule C++ binding (dsm2/src/oprule_interface/*.cpp) running against a mock of
// the Fortran model (support/Dsm2FortranMock.*). Read Dsm2FortranMock.h first: it explains what the mock
// stands in for and which Fortran routine each mocked function mirrors.
#define BOOST_TEST_MODULE oprule_dsm2_binding
#include <boost/test/included/unit_test.hpp>

#include <cmath>
#include <cstring>
#include <iostream>
#include <string>

#include "Dsm2Harness.h"
#include "InpReader.h"
#include "dsm2_expressions.h"
#include "dsm2_interface_fortran.h"
#include "dsm2_model_interface.h"
#include "dsm2_model_interface_gate.h"
#include "dsm2_named_value_lookup.h"
#include "dsm2_time_interface_fortran.h"
#include "oprule/parser/ModelNameParseError.h"
#include "oprule/parser/ParseSymbolManagement.h"

extern void op_rulerestart(FILE* input_file);  // flex scanner reset (prefix op_rule)

using namespace dsm2mock;
using oprule::parser::InvalidIdentifier;
using oprule::parser::MissingIdentifier;
using oprule::parser::NamedValueLookup;
using oprule::expression::DoubleScalarNode;
using oprule::expression::DoubleNodePtr;

namespace {

// ------------------------------------------------------------------ small model-building helpers

// A model with a bit of everything, used by the lookup / interface / resolver tests:
//   channels 185 (1000 ft) and 232 (20000 ft), gate g1 {d1, d2}, gate g2 {d1}, external flows q1 q2,
//   transfers t1 t2, reservoir res1 with nodes 10 and 20, time series ts1 (oprule) and shared (not oprule).
void build_basic_model() {
   Model& m = model();
   m.reset();
   m.add_channel(185, 1000.);
   m.add_channel(232, 20000.);
   m.add_gate("g1");
   m.add_device("g1", "d1");
   m.add_device("g1", "d2");
   m.add_gate("g2");
   m.add_device("g2", "d1");
   m.add_external_flow("q1", -3.);
   m.add_external_flow("q2", 0.);
   m.add_transfer("t1", 1.);
   m.add_transfer("t2", 2.);
   m.add_node(10);
   m.add_node(20);
   m.add_reservoir("res1", std::vector<int>({10, 20}));
   m.add_path_input("ts1", 5.);
   m.add_path_input("shared", 7., false);
}

struct Args {
   NamedValueLookup::ArgMap m;
   Args& add(const std::string& k, const std::string& v) { m[k] = v; return *this; }
};

Args gate_dev(const std::string& g, const std::string& d) { return Args().add("gate", g).add("device", d); }

// A harness (resets the model and the parser) with the basic model installed.
struct Basic : Harness {
   Basic() { build_basic_model(); }
   DSM2HydroNamedValueLookup lk;
   oprule::rule::ModelInterface<double>::NodePtr iface(const std::string& name, const Args& a) {
      return lk.getModelInterface(name, a.m);
   }
   DoubleNodePtr expr(const std::string& name, const Args& a) { return lk.getModelExpression(name, a.m); }
   double value(const std::string& name) { return getDoubleExpression(name.c_str())->eval(); }
};

}  // namespace

// ============================================================ the mock itself: Fortran contract
// These tests keep the mock honest. Each one states a property of the real Fortran (see the comment on
// the mocked function in support/Dsm2FortranMock.cpp for the routine it comes from).

BOOST_AUTO_TEST_SUITE(fortran_mock_contract)

BOOST_AUTO_TEST_CASE(time_functions_are_consistent) {
   build_basic_model();
   BOOST_CHECK_EQUAL(julday(1900, 1, 1), 1);
   model().set_time(2018, 9, 20, 13, 45);
   BOOST_CHECK_EQUAL(get_model_year(), 2018);
   BOOST_CHECK_EQUAL(get_model_month(), 9);
   BOOST_CHECK_EQUAL(get_model_day(), 20);
   BOOST_CHECK_EQUAL(get_model_hour(), 13);
   BOOST_CHECK_EQUAL(get_model_minute(), 45);
   BOOST_CHECK_EQUAL(get_model_minute_of_day(), 13 * 60 + 45);
   BOOST_CHECK_EQUAL(get_model_day_of_year(), 262);                    // 0-based; 31+28+31+30+31+30+31+31+19
   BOOST_CHECK_EQUAL(get_model_minute_of_year(), 262 * 1440 + 13 * 60 + 45);
   BOOST_CHECK_EQUAL(get_model_ticks(), model().julmin);
   int mon = 9, day = 20, hour = 13, min = 45;
   BOOST_CHECK_EQUAL(get_reference_minute_of_year(mon, day, hour, min), get_model_minute_of_year());
   const std::string s = "20SEP2018 1345";
   BOOST_CHECK_EQUAL(cdate_to_jul_min(s.c_str(), (unsigned)s.size()), model().julmin);
   model().dt_seconds = 900;
   BOOST_CHECK_EQUAL(time_step_seconds(), 900);
}

BOOST_AUTO_TEST_CASE(reference_minute_uses_the_model_year) {
   build_basic_model();
   int mon = 3, day = 1, hour = 0, min = 0;
   model().set_time(2019, 6, 1, 0, 0);
   BOOST_CHECK_EQUAL(get_reference_minute_of_year(mon, day, hour, min), (31 + 28) * 1440);
   model().set_time(2020, 6, 1, 0, 0);
   BOOST_CHECK_EQUAL(get_reference_minute_of_year(mon, day, hour, min), (31 + 29) * 1440);   // leap year
}

BOOST_AUTO_TEST_CASE(name_lookups_follow_the_fortran_case_rules) {
   build_basic_model();
   BOOST_CHECK_EQUAL(gate_index("G1", 2), 1);                // lower-cased, found
   BOOST_CHECK_EQUAL(gate_index("nope", 4), MISS_I);
   int g = 1;
   BOOST_CHECK_EQUAL(device_index(g, "D2", 2), 2);           // lower-cased
   BOOST_CHECK_EQUAL(device_index(g, "zz", 2), MISS_I);
   BOOST_CHECK_EQUAL(reservoir_index("RES1", 4), 1);         // lower-cased
   BOOST_CHECK_EQUAL(qext_index("q1", 2), 1);
   BOOST_CHECK_EQUAL(qext_index("Q1", 2), MISS_I);           // NOT lower-cased
   BOOST_CHECK_EQUAL(transfer_index("T2", 2), MISS_I);       // NOT lower-cased
   BOOST_CHECK_EQUAL(ts_index("ts1", 3), 1);
   BOOST_CHECK_EQUAL(ts_index("TS1", 3), -1);                // exact match; absent is -1, not miss_val_i
   BOOST_CHECK_EQUAL(ts_index("shared", 6), 2);              // all input paths are searched, not only oprule ones
}

BOOST_AUTO_TEST_CASE(external_numbers_map_to_internal_indices) {
   build_basic_model();
   int c185 = 185, c232 = 232, bad = 999, n10 = 10, n20 = 20;
   BOOST_CHECK_EQUAL(ext2int(c185), 1);
   BOOST_CHECK_EQUAL(ext2int(c232), 2);
   BOOST_CHECK_EQUAL(ext2int(bad), 0);                       // 0, not miss_val_i, for an unknown channel
   BOOST_CHECK_EQUAL(ext2intnode(n20), 2);
   BOOST_CHECK_EQUAL(ext2intnode(bad), 0);
   int one = 1, zero = 0;
   BOOST_CHECK_CLOSE(channel_length(one), 1000., 1e-9);
   BOOST_CHECK_THROW(channel_length(zero), FortranContractViolation);   // the Fortran would read chan_geom(0)
   int res = 1, internal10 = ext2intnode(n10);
   BOOST_CHECK_EQUAL(reservoir_connect_index(res, internal10), 1);   // takes the INTERNAL node number
   BOOST_CHECK_EQUAL(reservoir_connect_index(res, zero), MISS_I);
}

BOOST_AUTO_TEST_CASE(comp_point_interpolation_weights) {
   build_basic_model();
   model().set_stage(185, [](double x) { return x / 1000.; });   // 0 at the top, 1 at the bottom
   int chan = 1, pts[2];
   double w[2], dist = 250.;
   chan_comp_point(chan, dist, pts, w);
   BOOST_CHECK_CLOSE(w[0] + w[1], 1., 1e-9);
   BOOST_CHECK_CLOSE(w[0] * get_surf_elev(pts[0]) + w[1] * get_surf_elev(pts[1]), 0.25, 1e-9);
}

BOOST_AUTO_TEST_CASE(data_source_semantics) {
   build_basic_model();
   Model& m = model();
   int q = 1, expr = register_express_for_data_source(DoubleScalarNode::create(42.));
   double v = 42.;
   set_external_flow_datasource(q, expr, v, true);
   BOOST_CHECK_EQUAL(m.qexts[0].datasource.source_type, (int)EXPRESSION_DATA);
   BOOST_CHECK_CLOSE(m.fetch_data(m.qexts[0].datasource), 42., 1e-9);   // evaluated through the real C++ registry
   set_external_flow_datasource(q, expr, v, false);
   BOOST_CHECK_EQUAL(m.qexts[0].datasource.source_type, (int)CONST_DATA);
   BOOST_CHECK_CLOSE(m.fetch_data(m.qexts[0].datasource), 42., 1e-9);   // the stored constant
   DataSource unset;
   unset.source_type = 0;
   BOOST_CHECK_CLOSE(m.fetch_data(unset), MISS_R, 1e-9);                 // unknown type: miss_val_r every step
   DataSource dss;
   dss.source_type = DSS_DATA;
   dss.indx_ptr = 1;
   m.paths[0].value = 9.;
   BOOST_CHECK_CLOSE(m.fetch_data(dss), 9., 1e-9);
}

BOOST_AUTO_TEST_CASE(store_values_copies_every_source_into_the_model) {
   build_basic_model();
   Model& m = model();
   m.device("g1", "d1").op_to_node.value = 0.25;     // a const source with a different value
   m.qext("q1").datasource.value = -8.;
   m.store_values();
   BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefToNode, 0.25, 1e-9);
   BOOST_CHECK_CLOSE(m.qext("q1").flow, -8., 1e-9);
   BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefFromNode, 1., 1e-9);   // untouched sources reproduce their input
}

BOOST_AUTO_TEST_CASE(gate_install_and_set_free) {
   build_basic_model();
   Model& m = model();
   m.gate("g1").free = false;
   m.device("g1", "d1").flow = 5.;
   int g = 1;
   double zero = 0., half = 0.5, one = 1.;
   set_gate_install(g, zero);                                // exactly 0.0 frees the gate and zeroes device flows
   BOOST_CHECK(m.gate("g1").free);
   BOOST_CHECK_SMALL(m.device("g1", "d1").flow, 1e-12);
   BOOST_CHECK_SMALL(is_gate_install(g), 1e-12);
   m.device("g1", "d1").flow = 5.;
   set_gate_install(g, zero);                                // already free: flows are not zeroed again
   BOOST_CHECK_CLOSE(m.device("g1", "d1").flow, 5., 1e-9);
   set_gate_install(g, half);                                // any other value installs
   BOOST_CHECK(!m.gate("g1").free);
   BOOST_CHECK_CLOSE(is_gate_install(g), 1., 1e-9);
   (void)one;
}

BOOST_AUTO_TEST_CASE(device_coefficient_quirks) {
   build_basic_model();
   Model& m = model();
   int g = 1, d = 1, to = direct_to_node(), from = direct_from_node(), both = direct_to_from_node();
   m.device("g1", "d1").opCoefToNode = 0.2;
   m.device("g1", "d1").opCoefFromNode = 0.6;
   BOOST_CHECK_CLOSE(get_device_op_coef(g, d, to), 0.2, 1e-9);
   BOOST_CHECK_CLOSE(get_device_op_coef(g, d, from), 0.6, 1e-9);
   // FORTRAN QUIRK: the to/from "average" is (from + from)/2
   BOOST_CHECK_CLOSE(get_device_op_coef(g, d, both), 0.6, 1e-9);
   double v = 0.9;
   set_device_op_coef(g, d, both, v);                        // setting both directions really sets both
   BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefToNode, 0.9, 1e-9);
   BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefFromNode, 0.9, 1e-9);
   int bad = 7;
   BOOST_CHECK_CLOSE(get_device_op_coef(g, d, bad), -901., 1e-9);
   set_device_op_coef(g, d, bad, v);                         // unknown direction: silently nothing
   // flow coefficients only know to_node / from_node; anything else is exit(3)
   set_device_flow_coef(g, d, to, v);
   BOOST_CHECK_CLOSE(get_device_flow_coef(g, d, to), 0.9, 1e-9);
   BOOST_CHECK_THROW(get_device_flow_coef(g, d, both), FortranExit);
   BOOST_CHECK_THROW(set_device_flow_coef(g, d, both, v), FortranExit);
}

BOOST_AUTO_TEST_CASE(nduplicate_is_rounded_by_the_setter_only) {
   build_basic_model();
   int g = 1, d = 1;
   double v = 2.4, half = 2.5;
   set_device_nduplicate(g, d, v);
   BOOST_CHECK_CLOSE(get_device_nduplicate(g, d), 2., 1e-9);
   set_device_nduplicate(g, d, half);
   BOOST_CHECK_CLOSE(get_device_nduplicate(g, d), 3., 1e-9);   // nint rounds half away from zero
}

BOOST_AUTO_TEST_SUITE_END()

// ================================================================ names the rule language knows

BOOST_FIXTURE_TEST_SUITE(name_registry, Basic)

BOOST_AUTO_TEST_CASE(read_only_names) {
   const char* names[] = {"chan_stage", "chan_flow", "chan_vel", "res_flow", "res_stage", "ts"};
   for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); ++i) {
      BOOST_REQUIRE_MESSAGE(lk.isModelName(names[i]), names[i]);
      BOOST_CHECK_MESSAGE(lk.readWriteType(names[i]) == NamedValueLookup::READONLY, names[i]);
      BOOST_CHECK_MESSAGE(lk.takesArguments(names[i]), names[i]);
   }
}

BOOST_AUTO_TEST_CASE(writable_names) {
   const char* names[] = {"ext_flow", "transfer_flow", "gate_install", "gate_op", "gate_height",
                          "gate_nduplicate", "gate_elev", "gate_width", "gate_coef"};
   for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); ++i) {
      BOOST_REQUIRE_MESSAGE(lk.isModelName(names[i]), names[i]);
      BOOST_CHECK_MESSAGE(lk.readWriteType(names[i]) == NamedValueLookup::READWRITE, names[i]);
      BOOST_CHECK_MESSAGE(lk.takesArguments(names[i]), names[i]);
   }
}

BOOST_AUTO_TEST_CASE(constants_take_no_arguments) {
   const char* names[] = {"INSTALL", "REMOVE", "OPEN", "CLOSE"};
   for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); ++i) {
      BOOST_REQUIRE_MESSAGE(lk.isModelName(names[i]), names[i]);
      BOOST_CHECK(!lk.takesArguments(names[i]));
      BOOST_CHECK(lk.readWriteType(names[i]) == NamedValueLookup::READONLY);
   }
   BOOST_CHECK_CLOSE(expr("INSTALL", Args())->eval(), 1., 1e-9);
   BOOST_CHECK_CLOSE(expr("OPEN", Args())->eval(), 1., 1e-9);
   BOOST_CHECK_SMALL(expr("REMOVE", Args())->eval(), 1e-12);
   BOOST_CHECK_SMALL(expr("CLOSE", Args())->eval(), 1e-12);
}

BOOST_AUTO_TEST_CASE(names_are_exact_case) {
   BOOST_CHECK(!lk.isModelName("CHAN_STAGE"));
   BOOST_CHECK(!lk.isModelName("Gate_Op"));
   BOOST_CHECK(!lk.isModelName("install"));      // constants are registered in upper case only
   BOOST_CHECK(!lk.isModelName("open"));
   BOOST_CHECK(!lk.isModelName("not_a_name"));
   BOOST_CHECK_THROW(lk.getModelExpression("not_a_name", Args().m), oprule::parser::ModelNameNotFound);
}

BOOST_AUTO_TEST_SUITE_END()

// ====================================================== argument handling in the real factories

BOOST_FIXTURE_TEST_SUITE(factory_arguments, Basic)

BOOST_AUTO_TEST_CASE(channel_arguments) {
   BOOST_CHECK_NO_THROW(expr("chan_stage", Args().add("channel", "185").add("dist", "0")));
   BOOST_CHECK_NO_THROW(expr("chan_stage", Args().add("channel", "185").add("dist", "1000")));   // = length
   BOOST_CHECK_NO_THROW(expr("chan_stage", Args().add("channel", "185").add("dist", "length")));
   BOOST_CHECK_NO_THROW(expr("chan_stage", Args().add("channel", "185").add("dist", "LENGTH")));  // case insensitive
   BOOST_CHECK_THROW(expr("chan_stage", Args().add("dist", "0")), MissingIdentifier);
   BOOST_CHECK_THROW(expr("chan_stage", Args().add("channel", "185")), MissingIdentifier);
   BOOST_CHECK_THROW(expr("chan_stage", Args().add("channel", "185").add("dist", "1000.5")), InvalidIdentifier);
   BOOST_CHECK_THROW(expr("chan_stage", Args().add("channel", "185").add("dist", "-1")), InvalidIdentifier);
   for (const char* n : {"chan_flow", "chan_vel"})
      BOOST_CHECK_NO_THROW(expr(n, Args().add("channel", "232").add("dist", "6038")));
}

// FORTRAN CONTRACT: ext2int returns 0 for an unknown channel and channel_length(0) indexes chan_geom(0).
// The C++ does not check for it, so a typo in a channel number is not reported cleanly. The mock throws
// FortranContractViolation where the real model would read out of bounds.
BOOST_AUTO_TEST_CASE(unknown_channel_is_not_validated) {
   BOOST_CHECK_THROW(expr("chan_stage", Args().add("channel", "999").add("dist", "0")), FortranContractViolation);
}

BOOST_AUTO_TEST_CASE(reservoir_arguments) {
   model().reservoir("res1").stage = 4.5;
   model().reservoir("res1").qres[1] = -12.;
   BOOST_CHECK_CLOSE(expr("res_stage", Args().add("res", "res1"))->eval(), 4.5, 1e-9);
   BOOST_CHECK_CLOSE(expr("res_stage", Args().add("res", "RES1"))->eval(), 4.5, 1e-9);   // lower-cased by Fortran
   BOOST_CHECK_CLOSE(expr("res_flow", Args().add("res", "res1").add("node", "20"))->eval(), -12., 1e-9);
   BOOST_CHECK_THROW(expr("res_stage", Args()), MissingIdentifier);
   BOOST_CHECK_THROW(expr("res_flow", Args().add("res", "res1")), MissingIdentifier);
   BOOST_CHECK_THROW(expr("res_flow", Args().add("res", "res1").add("connect", "20")), MissingIdentifier);  // registration label says 'connect', code reads 'node'
   BOOST_CHECK_THROW(expr("res_flow", Args().add("res", "res1").add("node", "999")), InvalidIdentifier);
}

// FORTRAN CONTRACT / PLAN LK-03: an unknown reservoir name is not an error for res_stage (the factory
// returns an empty pointer, which the parser would then dereference), and for res_flow the Fortran would
// index res_geom(miss_val_i).
BOOST_AUTO_TEST_CASE(unknown_reservoir_is_not_validated) {
   DoubleNodePtr n = expr("res_stage", Args().add("res", "nosuch"));
   BOOST_CHECK(!n);
   BOOST_CHECK_THROW(expr("res_flow", Args().add("res", "nosuch").add("node", "10")), FortranContractViolation);
}

BOOST_AUTO_TEST_CASE(gate_arguments) {
   const Args ok = gate_dev("g1", "d1");
   BOOST_CHECK_NO_THROW(iface("gate_height", ok));
   BOOST_CHECK_THROW(iface("gate_height", Args().add("device", "d1")), MissingIdentifier);
   BOOST_CHECK_THROW(iface("gate_height", gate_dev("nosuch", "d1")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("gate_height", gate_dev("g1", "nosuch")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("gate_height", Args().add("gate", "g1")), InvalidIdentifier);   // device is required
   BOOST_CHECK_NO_THROW(iface("gate_install", Args().add("gate", "g1")));
   BOOST_CHECK_NO_THROW(iface("gate_install", gate_dev("g1", "d1")));                       // device is ignored
   BOOST_CHECK_THROW(iface("gate_install", Args()), MissingIdentifier);
   BOOST_CHECK_THROW(iface("gate_install", Args().add("gate", "nosuch")), InvalidIdentifier);
}

BOOST_AUTO_TEST_CASE(gate_and_device_names_are_case_insensitive) {
   BOOST_CHECK_NO_THROW(iface("gate_height", gate_dev("G1", "D1")));
   BOOST_CHECK_NO_THROW(iface("gate_install", Args().add("gate", "G2")));
}

// PLAN CASE-05: the names below are compared exactly (no lower-casing in the Fortran lookups)
BOOST_AUTO_TEST_CASE(external_flow_transfer_and_series_names_are_exact) {
   BOOST_CHECK_NO_THROW(iface("ext_flow", Args().add("name", "q1")));
   BOOST_CHECK_THROW(iface("ext_flow", Args().add("name", "Q1")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("ext_flow", Args().add("name", "nosuch")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("ext_flow", Args()), MissingIdentifier);
   BOOST_CHECK_NO_THROW(iface("transfer_flow", Args().add("transfer", "t1")));
   BOOST_CHECK_THROW(iface("transfer_flow", Args().add("transfer", "T1")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("transfer_flow", Args().add("name", "t1")), MissingIdentifier);   // argument is 'transfer'
   BOOST_CHECK_NO_THROW(expr("ts", Args().add("name", "ts1")));
   BOOST_CHECK_THROW(expr("ts", Args().add("name", "TS1")), InvalidIdentifier);
   BOOST_CHECK_THROW(expr("ts", Args().add("name", "nosuch")), InvalidIdentifier);
   BOOST_CHECK_THROW(expr("ts", Args()), MissingIdentifier);
}

// PLAN TS-04: ts() searches every input path, so it also resolves names that are not oprule time series.
BOOST_AUTO_TEST_CASE(ts_resolves_any_input_path) {
   model().path("shared").value = 7.;
   BOOST_CHECK_CLOSE(expr("ts", Args().add("name", "shared"))->eval(), 7., 1e-9);
}

// Direction keywords differ between gate_op and gate_coef (PLAN D-10).
BOOST_AUTO_TEST_CASE(direction_keywords) {
   for (const char* d : {"to_node", "from_node", "to_from_node", "bidir"})
      BOOST_CHECK_NO_THROW(iface("gate_op", gate_dev("g1", "d1").add("direction", d)));
   BOOST_CHECK_THROW(iface("gate_op", gate_dev("g1", "d1").add("direction", "both")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("gate_op", gate_dev("g1", "d1").add("direction", "To_Node")), InvalidIdentifier);  // exact case
   BOOST_CHECK_THROW(iface("gate_op", gate_dev("g1", "d1")), MissingIdentifier);

   for (const char* d : {"to_node", "from_node", "both"})
      BOOST_CHECK_NO_THROW(iface("gate_coef", gate_dev("g1", "d1").add("direction", d)));
   BOOST_CHECK_THROW(iface("gate_coef", gate_dev("g1", "d1").add("direction", "to_from_node")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("gate_coef", gate_dev("g1", "d1").add("direction", "bidir")), InvalidIdentifier);
   BOOST_CHECK_THROW(iface("gate_coef", gate_dev("g1", "d1")), MissingIdentifier);
}

BOOST_AUTO_TEST_SUITE_END()

// ======================================================= model interfaces: values and data sources

BOOST_FIXTURE_TEST_SUITE(model_interfaces, Basic)

// Whether an interface attaches a data source on completion (isTimeDependent), per REFERENCE A4.
BOOST_AUTO_TEST_CASE(time_dependence_of_each_writable_name) {
   BOOST_CHECK(iface("ext_flow", Args().add("name", "q1"))->isTimeDependent());
   BOOST_CHECK(iface("transfer_flow", Args().add("transfer", "t1"))->isTimeDependent());
   BOOST_CHECK(iface("gate_op", gate_dev("g1", "d1").add("direction", "to_node"))->isTimeDependent());
   BOOST_CHECK(iface("gate_height", gate_dev("g1", "d1"))->isTimeDependent());
   BOOST_CHECK(iface("gate_elev", gate_dev("g1", "d1"))->isTimeDependent());
   BOOST_CHECK(iface("gate_width", gate_dev("g1", "d1"))->isTimeDependent());
   BOOST_CHECK(iface("gate_nduplicate", gate_dev("g1", "d1"))->isTimeDependent());
   BOOST_CHECK(!iface("gate_install", Args().add("gate", "g1"))->isTimeDependent());                 // static
   BOOST_CHECK(!iface("gate_coef", gate_dev("g1", "d1").add("direction", "to_node"))->isTimeDependent());  // static
}

BOOST_AUTO_TEST_CASE(set_and_eval_round_trip) {
   Model& m = model();
   iface("ext_flow", Args().add("name", "q1"))->set(-9.);
   BOOST_CHECK_CLOSE(m.qext("q1").flow, -9., 1e-9);
   iface("transfer_flow", Args().add("transfer", "t2"))->set(4.);
   BOOST_CHECK_CLOSE(m.transfer("t2").flow, 4., 1e-9);
   iface("gate_height", gate_dev("g1", "d2"))->set(3.);
   BOOST_CHECK_CLOSE(m.device("g1", "d2").height, 3., 1e-9);
   iface("gate_elev", gate_dev("g1", "d2"))->set(-1.5);
   BOOST_CHECK_CLOSE(m.device("g1", "d2").baseElev, -1.5, 1e-9);          // elevation lives in baseElev
   iface("gate_width", gate_dev("g1", "d2"))->set(18.);
   BOOST_CHECK_CLOSE(m.device("g1", "d2").maxWidth, 18., 1e-9);           // width lives in maxWidth
   iface("gate_nduplicate", gate_dev("g1", "d2"))->set(2.6);
   BOOST_CHECK_CLOSE(m.device("g1", "d2").nDuplicate, 3., 1e-9);          // rounded by the Fortran setter
   iface("gate_coef", gate_dev("g1", "d2").add("direction", "from_node"))->set(0.65);
   BOOST_CHECK_CLOSE(m.device("g1", "d2").flowCoefFromNode, 0.65, 1e-9);
   BOOST_CHECK_CLOSE(iface("gate_coef", gate_dev("g1", "d2").add("direction", "from_node"))->eval(), 0.65, 1e-9);
}

BOOST_AUTO_TEST_CASE(gate_op_directions) {
   Model& m = model();
   iface("gate_op", gate_dev("g1", "d1").add("direction", "to_node"))->set(0.3);
   BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefToNode, 0.3, 1e-9);
   BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefFromNode, 1., 1e-9);
   iface("gate_op", gate_dev("g1", "d1").add("direction", "from_node"))->set(0.4);
   BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefFromNode, 0.4, 1e-9);
   for (const char* both : {"to_from_node", "bidir"}) {
      iface("gate_op", gate_dev("g1", "d1").add("direction", both))->set(0.8);
      BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefToNode, 0.8, 1e-9);
      BOOST_CHECK_CLOSE(m.device("g1", "d1").opCoefFromNode, 0.8, 1e-9);
   }
}

// FORTRAN QUIRK (PLAN D-04): reading a to_from_node op coefficient returns the from-node value.
BOOST_AUTO_TEST_CASE(gate_op_both_directions_reads_the_from_node_value) {
   Model& m = model();
   m.device("g1", "d1").opCoefToNode = 0.2;
   m.device("g1", "d1").opCoefFromNode = 0.6;
   BOOST_CHECK_CLOSE(iface("gate_op", gate_dev("g1", "d1").add("direction", "to_from_node"))->eval(), 0.6, 1e-9);  // intended: 0.4
}

BOOST_AUTO_TEST_CASE(gate_install_interface) {
   Model& m = model();
   oprule::rule::ModelInterface<double>::NodePtr gi = iface("gate_install", Args().add("gate", "g1"));
   BOOST_CHECK_CLOSE(gi->eval(), 1., 1e-9);
   m.device("g1", "d1").flow = 5.;
   gi->set(0.);                                       // REMOVE
   BOOST_CHECK(m.gate("g1").free);
   BOOST_CHECK_SMALL(gi->eval(), 1e-12);
   BOOST_CHECK_SMALL(m.device("g1", "d1").flow, 1e-12);
   gi->set(1.);                                       // INSTALL
   BOOST_CHECK(!m.gate("g1").free);
}

BOOST_AUTO_TEST_CASE(data_expression_time_dependent_vs_constant) {
   Model& m = model();
   oprule::rule::ModelInterface<double>::NodePtr q = iface("ext_flow", Args().add("name", "q1"));
   q->setDataExpression(expr("ts", Args().add("name", "ts1")));                 // depends on time
   BOOST_CHECK_EQUAL(m.qext("q1").datasource.source_type, (int)EXPRESSION_DATA);
   m.path("ts1").value = 11.;
   m.store_values();
   BOOST_CHECK_CLOSE(m.qext("q1").flow, 11., 1e-9);
   m.path("ts1").value = 12.;
   m.store_values();
   BOOST_CHECK_CLOSE(m.qext("q1").flow, 12., 1e-9);                              // follows the series

   q->setDataExpression(DoubleScalarNode::create(0.));                           // constant
   BOOST_CHECK_EQUAL(m.qext("q1").datasource.source_type, (int)CONST_DATA);
   m.store_values();
   BOOST_CHECK_SMALL(m.qext("q1").flow, 1e-12);
}

// Static interfaces cannot take a data expression.
BOOST_AUTO_TEST_CASE(static_interfaces_reject_data_expressions) {
   oprule::rule::ModelInterface<double>::NodePtr gi = iface("gate_install", Args().add("gate", "g1"));
   bool threw = false;
   try { gi->setDataExpression(DoubleScalarNode::create(1.)); } catch (std::logic_error* e) { threw = true; delete e; }
   BOOST_CHECK(threw);   // thrown as a pointer
}

BOOST_AUTO_TEST_SUITE_END()

// ===================================================== read-only nodes: channels, reservoirs, time

BOOST_FIXTURE_TEST_SUITE(read_only_nodes, Basic)

BOOST_AUTO_TEST_CASE(channel_values_are_interpolated_along_the_channel) {
   model().set_stage(185, [](double x) { return x / 1000.; });
   model().set_flow(185, [](double x) { return 100. - x / 10.; });
   model().set_velocity(185, [](double x) { return -0.001 * x; });
   BOOST_CHECK_CLOSE(expr("chan_stage", Args().add("channel", "185").add("dist", "250"))->eval(), 0.25, 1e-9);
   BOOST_CHECK_CLOSE(expr("chan_flow", Args().add("channel", "185").add("dist", "250"))->eval(), 75., 1e-9);
   BOOST_CHECK_CLOSE(expr("chan_stage", Args().add("channel", "185").add("dist", "0"))->eval() + 1., 1., 1e-9);
   BOOST_CHECK_CLOSE(expr("chan_stage", Args().add("channel", "185").add("dist", "length"))->eval(), 1., 1e-9);
   BOOST_CHECK_CLOSE(expr("chan_vel", Args().add("channel", "185").add("dist", "400"))->eval(), -0.4, 1e-9);
}

// REFERENCE B9.2 / PLAN D-02: copies of chan_stage / chan_flow nodes truncate the distance to an int
// (rules copy their trigger and target expressions when they are built).
BOOST_AUTO_TEST_CASE(copy_of_channel_node_truncates_a_fractional_distance) {
   model().comp_point_requests.clear();
   DoubleNodePtr n = expr("chan_flow", Args().add("channel", "185").add("dist", "100.5"));
   BOOST_REQUIRE_EQUAL(model().comp_point_requests.size(), 1u);
   BOOST_CHECK_CLOSE(model().comp_point_requests.back().second, 100.5, 1e-9);
   DoubleNodePtr c = n->copy();
   BOOST_REQUIRE_EQUAL(model().comp_point_requests.size(), 2u);
   BOOST_CHECK_CLOSE(model().comp_point_requests.back().second, 100., 1e-9);    // DEFECT: intended 100.5
}

BOOST_AUTO_TEST_CASE(copy_of_velocity_node_keeps_the_distance) {
   double seen = -1.;
   model().set_velocity(185, [&seen](double x) { seen = x; return 0.; });
   DoubleNodePtr n = expr("chan_vel", Args().add("channel", "185").add("dist", "100.5"));
   n->copy()->eval();
   BOOST_CHECK_CLOSE(seen, 100.5, 1e-9);
}

BOOST_AUTO_TEST_CASE(time_series_node_reads_the_current_path_value) {
   DoubleNodePtr n = expr("ts", Args().add("name", "ts1"));
   BOOST_CHECK(n->isTimeDependent());
   model().path("ts1").value = 1.5;
   BOOST_CHECK_CLOSE(n->eval(), 1.5, 1e-9);
   model().path("ts1").value = 2.5;
   BOOST_CHECK_CLOSE(n->eval(), 2.5, 1e-9);
   BOOST_CHECK_CLOSE(n->copy()->eval(), 2.5, 1e-9);
}

// REFERENCE B9.3 / PLAN D-03: device interface equality compares devndx with the other's ndx.
BOOST_AUTO_TEST_CASE(device_interface_equality_is_wrong) {
   DeviceOpInterface a(1, 2, 1), same(1, 2, 1), other_device(1, 1, 1);
   BOOST_CHECK(!(a == same));            // DEFECT: identical devices compare unequal
   DeviceOpInterface b(1, 1, 1);
   BOOST_CHECK(b == a);                  // DEFECT: different devices (1,1) and (1,2) compare equal
   (void)other_device;
}

BOOST_AUTO_TEST_SUITE_END()

// ========================================================================= model time as rules see it

BOOST_FIXTURE_TEST_SUITE(time_nodes, Basic)

BOOST_AUTO_TEST_CASE(calendar_terms) {
   model().set_time(2018, 9, 20, 13, 45);
   model().dt_seconds = 900;
   BOOST_REQUIRE(add_expression("y", "YEAR"));
   BOOST_REQUIRE(add_expression("mo", "MONTH"));
   BOOST_REQUIRE(add_expression("d", "DAY"));
   BOOST_REQUIRE(add_expression("h", "HOUR"));
   BOOST_REQUIRE(add_expression("mi", "MIN"));
   BOOST_REQUIRE(add_expression("dtv", "DT"));
   BOOST_CHECK_CLOSE(value("y"), 2018., 1e-9);
   BOOST_CHECK_CLOSE(value("mo"), 9., 1e-9);
   BOOST_CHECK_CLOSE(value("d"), 20., 1e-9);
   BOOST_CHECK_CLOSE(value("h"), 13., 1e-9);
   BOOST_CHECK_CLOSE(value("mi"), 45., 1e-9);
   BOOST_CHECK_CLOSE(value("dtv"), 900., 1e-9);
}

BOOST_AUTO_TEST_CASE(absolute_date_comparisons) {
   BOOST_REQUIRE(add_expression("after", "DATETIME >= 20SEP2018 12:00"));
   BOOST_REQUIRE(add_expression("exact", "DATETIME == 20SEP2018 12:00"));
   model().set_time(2018, 9, 20, 11, 45);
   BOOST_CHECK(!getBoolExpression("after")->eval());
   model().set_time(2018, 9, 20, 12, 0);
   BOOST_CHECK(getBoolExpression("after")->eval());
   BOOST_CHECK(getBoolExpression("exact")->eval());
   model().set_time(2018, 9, 20, 12, 15);
   BOOST_CHECK(!getBoolExpression("exact")->eval());
   BOOST_REQUIRE(add_expression("midnight", "DATETIME >= 28SEP1992"));     // time defaults to 00:00
   model().set_time(1992, 9, 28, 0, 0);
   BOOST_CHECK(getBoolExpression("midnight")->eval());
}

BOOST_AUTO_TEST_CASE(season_comparisons_follow_the_model_year) {
   BOOST_REQUIRE(add_expression("is_mar1", "SEASON == 01MAR"));
   model().set_time(2019, 3, 1, 0, 0);
   BOOST_CHECK(getBoolExpression("is_mar1")->eval());
   model().set_time(2020, 3, 1, 0, 0);                                     // leap year: day 60
   BOOST_CHECK(getBoolExpression("is_mar1")->eval());
   model().set_time(2020, 2, 29, 0, 0);
   BOOST_CHECK(!getBoolExpression("is_mar1")->eval());

   BOOST_REQUIRE(add_expression("in_vamp", "SEASON >15APR AND SEASON <16MAY"));
   model().set_time(2018, 4, 14, 12, 0);
   BOOST_CHECK(!getBoolExpression("in_vamp")->eval());
   model().set_time(2018, 4, 20, 12, 0);
   BOOST_CHECK(getBoolExpression("in_vamp")->eval());
   model().set_time(2018, 5, 16, 12, 0);
   BOOST_CHECK(!getBoolExpression("in_vamp")->eval());
}

// A season window that crosses the new year is written with OR (SEASON>01DEC OR SEASON<15APR); a window
// written with AND would never be true. Seasons do not wrap.
BOOST_AUTO_TEST_CASE(seasons_do_not_wrap_around_the_new_year) {
   BOOST_REQUIRE(add_expression("winter_or", "SEASON>01DEC OR SEASON<15APR"));
   BOOST_REQUIRE(add_expression("winter_and", "SEASON>01DEC AND SEASON<15APR"));
   model().set_time(2018, 12, 15, 0, 0);
   BOOST_CHECK(getBoolExpression("winter_or")->eval());
   BOOST_CHECK(!getBoolExpression("winter_and")->eval());
   model().set_time(2019, 2, 1, 0, 0);
   BOOST_CHECK(getBoolExpression("winter_or")->eval());
   BOOST_CHECK(!getBoolExpression("winter_and")->eval());
   model().set_time(2018, 7, 1, 0, 0);
   BOOST_CHECK(!getBoolExpression("winter_or")->eval());
}

// REFERENCE B9.1 / PLAN D-01: the hour of a seasonal literal with a time is dropped.
BOOST_AUTO_TEST_CASE(seasonal_literal_with_a_time_ignores_the_hour) {
   BOOST_REQUIRE(add_expression("at_1230", "SEASON == 01JAN 12:30"));
   BOOST_REQUIRE(add_expression("at_0030", "SEASON == 01JAN 00:30"));
   model().set_time(2018, 1, 1, 0, 30);
   BOOST_CHECK(getBoolExpression("at_0030")->eval());
   model().set_time(2018, 1, 1, 12, 30);
   BOOST_CHECK(!getBoolExpression("at_1230")->eval());   // DEFECT: should be true
}

BOOST_AUTO_TEST_SUITE_END()

// ======================================================== which rules the DSM2 resolver treats as overlapping
// Rules are in conflict when their actions touch the same thing (REFERENCE B6). Each row builds two
// actions from the real lookup and asks the real resolver.

namespace {
struct Spec {
   const char* name;
   Args args;
};

Spec ext(const char* n) { return Spec{"ext_flow", Args().add("name", n)}; }
Spec xfer(const char* n) { return Spec{"transfer_flow", Args().add("transfer", n)}; }
Spec inst(const char* g) { return Spec{"gate_install", Args().add("gate", g)}; }
Spec dev(const char* what, const char* g, const char* d) { return Spec{what, gate_dev(g, d)}; }
Spec op(const char* g, const char* d, const char* dir) {
   return Spec{"gate_op", gate_dev(g, d).add("direction", dir)};
}
Spec coef(const char* g, const char* d, const char* dir) {
   return Spec{"gate_coef", gate_dev(g, d).add("direction", dir)};
}
}  // namespace

BOOST_FIXTURE_TEST_SUITE(resolver_overlap, Basic)

namespace {
bool overlaps(Basic& b, const Spec& s1, const Spec& s2) {
   using oprule::rule::ModelAction;
   oprule::rule::TransitionPtr abrupt(new oprule::rule::AbruptTransition());
   ModelAction<double> a1(b.iface(s1.name, s1.args), DoubleScalarNode::create(0.), abrupt);
   ModelAction<double> a2(b.iface(s2.name, s2.args), DoubleScalarNode::create(0.), abrupt);
   return b.resolver.overlap(a1, a2);
}
}  // namespace

BOOST_AUTO_TEST_CASE(same_target_overlaps_different_target_does_not) {
   BOOST_CHECK(overlaps(*this, ext("q1"), ext("q1")));
   BOOST_CHECK(!overlaps(*this, ext("q1"), ext("q2")));
   BOOST_CHECK(overlaps(*this, xfer("t1"), xfer("t1")));
   BOOST_CHECK(!overlaps(*this, xfer("t1"), xfer("t2")));
   BOOST_CHECK(overlaps(*this, inst("g1"), inst("g1")));
   BOOST_CHECK(!overlaps(*this, inst("g1"), inst("g2")));
}

BOOST_AUTO_TEST_CASE(different_kinds_never_overlap) {
   BOOST_CHECK(!overlaps(*this, ext("q1"), xfer("t1")));
   BOOST_CHECK(!overlaps(*this, xfer("t1"), ext("q1")));
   BOOST_CHECK(!overlaps(*this, ext("q1"), dev("gate_height", "g1", "d1")));
   BOOST_CHECK(!overlaps(*this, dev("gate_height", "g1", "d1"), ext("q1")));
   BOOST_CHECK(!overlaps(*this, xfer("t1"), inst("g1")));
}

BOOST_AUTO_TEST_CASE(gate_install_overlaps_every_device_of_the_same_gate) {
   BOOST_CHECK(overlaps(*this, inst("g1"), dev("gate_height", "g1", "d1")));
   BOOST_CHECK(overlaps(*this, dev("gate_height", "g1", "d2"), inst("g1")));     // either order
   BOOST_CHECK(!overlaps(*this, inst("g1"), dev("gate_height", "g2", "d1")));
   BOOST_CHECK(overlaps(*this, inst("g1"), op("g1", "d1", "to_node")));
}

// PLAN CFL-08: any two actions on the same gate device overlap, whatever the property or direction.
BOOST_AUTO_TEST_CASE(any_two_actions_on_one_device_overlap) {
   BOOST_CHECK(overlaps(*this, op("g1", "d1", "to_node"), op("g1", "d1", "from_node")));
   BOOST_CHECK(overlaps(*this, op("g1", "d1", "to_node"), dev("gate_height", "g1", "d1")));
   BOOST_CHECK(overlaps(*this, dev("gate_elev", "g1", "d1"), dev("gate_width", "g1", "d1")));
   BOOST_CHECK(overlaps(*this, dev("gate_nduplicate", "g1", "d1"), op("g1", "d1", "from_node")));
   BOOST_CHECK(overlaps(*this, coef("g1", "d1", "to_node"), op("g1", "d1", "to_node")));
   BOOST_CHECK(overlaps(*this, coef("g1", "d1", "to_node"), coef("g1", "d1", "from_node")));
   // different devices, or the same device name on a different gate, do not overlap
   BOOST_CHECK(!overlaps(*this, op("g1", "d1", "to_node"), op("g1", "d2", "to_node")));
   BOOST_CHECK(!overlaps(*this, op("g1", "d1", "to_node"), op("g2", "d1", "to_node")));
}

BOOST_AUTO_TEST_SUITE_END()

// =============================================================== study input files, end to end
// The rule inputs used by the studies (oprule/test/data/*.inp, subsets of dsm2_studies/common_input/oprule_*.inp)
// are read the way the model reads them, the mock model is populated with exactly the gates, devices,
// external flows, channels and time series the rules mention, and every rule is parsed by the real binding.
// What this does NOT check: that the names exist in a real DSM2 study (the mock invents them).

#include <dirent.h>
#include <cstdlib>
#include <map>
#include <regex>
#include <set>

namespace {

using oprule_test::InpTable;
using oprule_test::OpruleStatement;

std::string data_path(const std::string& file) { return std::string(OPRULE_TEST_DATA_DIR) + "/" + file; }

std::string lower_str(std::string s) {
   for (size_t i = 0; i < s.size(); ++i) s[i] = (char)std::tolower((unsigned char)s[i]);
   return s;
}

std::string trim_str(const std::string& s) {
   size_t b = s.find_first_not_of(" \t");
   if (b == std::string::npos) return "";
   return s.substr(b, s.find_last_not_of(" \t") - b + 1);
}

// Create in the mock model everything the statements refer to (see the comment above).
void populate_model(const std::vector<InpTable>& tables) {
   Model& m = model();
   m.reset();
   std::vector<OpruleStatement> stmts = oprule_test::statements_in_parse_order(tables);
   std::vector<std::string> texts;
   for (size_t i = 0; i < stmts.size(); ++i) {
      texts.push_back(stmts[i].action);
      texts.push_back(stmts[i].trigger);
      texts.push_back(stmts[i].definition);
   }
   std::regex call("(gate_[a-z_]+|ext_flow|transfer_flow|chan_[a-z]+)\\s*\\(([^)]*)\\)");
   std::map<int, double> channels;   // external number -> longest distance used
   std::vector<std::pair<std::string, std::map<std::string, std::string> > > calls;
   for (size_t t = 0; t < texts.size(); ++t) {
      for (std::sregex_iterator it(texts[t].begin(), texts[t].end(), call), end; it != end; ++it) {
         std::map<std::string, std::string> args;
         std::string list = (*it)[2];
         size_t pos = 0;
         while (pos <= list.size()) {
            size_t next = list.find_first_of(",;", pos);
            std::string item = list.substr(pos, next == std::string::npos ? std::string::npos : next - pos);
            size_t eq = item.find('=');
            if (eq != std::string::npos) args[trim_str(item.substr(0, eq))] = trim_str(item.substr(eq + 1));
            if (next == std::string::npos) break;
            pos = next + 1;
         }
         calls.push_back(std::make_pair((*it)[1].str(), args));
         if (args.count("channel")) {
            double d = args.count("dist") ? std::atof(args["dist"].c_str()) : 0.;
            double& longest = channels[std::atoi(args["channel"].c_str())];
            if (d > longest) longest = d;
         }
      }
   }
   for (std::map<int, double>::iterator c = channels.begin(); c != channels.end(); ++c)
      m.add_channel(c->first, std::max(20000., c->second + 1000.));
   for (size_t i = 0; i < calls.size(); ++i) {
      const std::string& fn = calls[i].first;
      std::map<std::string, std::string>& a = calls[i].second;
      if (fn.compare(0, 5, "gate_") == 0 && a.count("gate")) {
         std::string g = lower_str(a["gate"]);
         bool have = false;
         for (size_t k = 0; k < m.gates.size(); ++k) have = have || m.gates[k].name == g;
         if (!have) m.add_gate(g);
         if (a.count("device")) {
            std::string d = lower_str(a["device"]);
            bool dev_have = false;
            for (size_t k = 0; k < m.gate(g).devices.size(); ++k) dev_have = dev_have || m.gate(g).devices[k].name == d;
            if (!dev_have) m.add_device(g, d);
         }
      } else if (fn == "ext_flow" && a.count("name")) {
         bool have = false;
         for (size_t k = 0; k < m.qexts.size(); ++k) have = have || m.qexts[k].name == lower_str(a["name"]);
         if (!have) m.add_external_flow(a["name"], 0.);
      } else if (fn == "transfer_flow" && a.count("transfer")) {
         bool have = false;
         for (size_t k = 0; k < m.transfers.size(); ++k) have = have || m.transfers[k].name == lower_str(a["transfer"]);
         if (!have) m.add_transfer(a["transfer"], 0.);
      }
   }
   std::vector<std::string> series = oprule_test::time_series_names(tables);
   for (size_t i = 0; i < series.size(); ++i) m.add_path_input(series[i], 0.);
}

// Parse every expression then every rule of the tables into the harness (model parse order).
// Returns the statements that failed to parse.
std::vector<std::string> load_tables(Harness& h, const std::vector<InpTable>& tables, size_t* n_rules = 0,
                                     size_t* n_expressions = 0) {
   std::vector<OpruleStatement> stmts = oprule_test::statements_in_parse_order(tables);
   std::vector<std::string> failed;
   size_t rules = 0, exprs = 0;
   for (size_t i = 0; i < stmts.size(); ++i) {
      bool ok = stmts[i].is_rule ? h.add_rule(stmts[i].name, stmts[i].action, stmts[i].trigger)
                                 : h.add_expression(stmts[i].name, stmts[i].definition);
      (stmts[i].is_rule ? rules : exprs) += 1;
      if (!ok) failed.push_back(stmts[i].name);
   }
   if (n_rules) *n_rules = rules;
   if (n_expressions) *n_expressions = exprs;
   return failed;
}

// A harness driven by named time series values; the test sets `series[...]`.
struct Sim : Harness {
   Sim() {
      ts_source = [this](const std::string& n, int) {
         std::map<std::string, double>::iterator it = series.find(n);
         return it == series.end() ? 0. : it->second;
      };
   }
   std::map<std::string, double> series;

   // Build the mock model for a data file, then load it. Returns the failed statements.
   std::vector<std::string> load_file(const std::string& file) {
      std::vector<InpTable> tables = oprule_test::read_inp_file(data_path(file));
      BOOST_REQUIRE_MESSAGE(!tables.empty(), "cannot read " + data_path(file));
      populate_model(tables);
      model().dt_seconds = 900;
      return load_tables(*this, tables);
   }
   void set_level(int channel, double h) { model().set_stage(channel, [h](double) { return h; }); }
   // run until the predicate holds or max_steps passed; returns steps taken
   int run_until(const std::function<bool()>& done, int max_steps) {
      int n = 0;
      while (n < max_steps && !done()) { step(); ++n; }
      return n;
   }
};

}  // namespace

BOOST_AUTO_TEST_SUITE(study_inputs)

struct FileCount { const char* file; size_t rules; size_t expressions; };
const FileCount kFiles[] = {
   {"oprule_historical_gate.inp", 15, 5},
   {"oprule_hist_restoration.inp", 10, 0},
   {"oprule_hist_temp_barriers.inp", 17, 0},
   {"oprule_montezuma_planning_gate.inp", 4, 3},
   {"oprule_temp_barriers_planning.inp", 13, 9},
};

BOOST_AUTO_TEST_CASE(reader_follows_the_input_storage_rules) {
   std::vector<InpTable> t = oprule_test::read_inp_file(data_path("oprule_hist_temp_barriers.inp"));
   BOOST_REQUIRE(!t.empty());
   BOOST_CHECK_EQUAL(t[0].name, "OPERATING_RULE");
   size_t retired = 0;
   for (size_t i = 0; i < t[0].rows.size(); ++i) retired += (t[0].rows[i][0] == "^retired_rule");
   BOOST_CHECK_EQUAL(retired, 0u);                                   // '^' rows are dropped
   BOOST_CHECK_EQUAL(t[0].rows[0].size(), 3u);                       // NAME ACTION TRIGGER
   BOOST_CHECK_EQUAL(t[0].rows[0][2], "TRUE");                       // bare token
   BOOST_CHECK_EQUAL(t[0].rows[1][2], "ts(name=glc_install) >= 1.0"); // quotes removed, spaces kept
   BOOST_CHECK_EQUAL(oprule_test::time_series_names(t).size(), 10u);
}

BOOST_AUTO_TEST_CASE(statements_come_in_model_parse_order) {
   std::vector<InpTable> t = oprule_test::read_inp_file(data_path("oprule_hist_temp_barriers.inp"));
   std::vector<OpruleStatement> s = oprule_test::statements_in_parse_order(t);
   BOOST_REQUIRE(s.size() > 3);
   // upper case sorts before lower case, so FalseBarrier_* come first
   BOOST_CHECK_EQUAL(s[0].name, "FalseBarrier_elev");
   for (size_t i = 1; i < s.size(); ++i) BOOST_CHECK_MESSAGE(s[i - 1].name <= s[i].name, s[i].name);
}

BOOST_AUTO_TEST_CASE(every_rule_and_expression_parses) {
   for (size_t f = 0; f < sizeof(kFiles) / sizeof(kFiles[0]); ++f) {
      Sim s;
      size_t rules = 0, exprs = 0;
      std::vector<InpTable> tables = oprule_test::read_inp_file(data_path(kFiles[f].file));
      populate_model(tables);
      std::vector<std::string> failed = load_tables(s, tables, &rules, &exprs);
      BOOST_CHECK_MESSAGE(failed.empty(), std::string(kFiles[f].file) + ": " + (failed.empty() ? "" : failed[0]) + " failed");
      BOOST_CHECK_MESSAGE(rules == kFiles[f].rules, std::string(kFiles[f].file) + " rules");
      BOOST_CHECK_MESSAGE(exprs == kFiles[f].expressions, std::string(kFiles[f].file) + " expressions");
   }
}

// Optional: parse the complete real files. Set OPRULE_CORPUS_DIR to the directory with oprule_*.inp.
BOOST_AUTO_TEST_CASE(complete_study_files_parse_when_available) {
   const char* dir = std::getenv("OPRULE_CORPUS_DIR");
   if (!dir) { std::cout << "[SKIP] OPRULE_CORPUS_DIR not set" << std::endl; return; }
   DIR* d = opendir(dir);
   BOOST_REQUIRE_MESSAGE(d, std::string("cannot open ") + dir);
   std::vector<std::string> files;
   while (dirent* e = readdir(d)) {
      std::string n = e->d_name;
      if (n.compare(0, 7, "oprule_") == 0 && n.size() > 4 && n.compare(n.size() - 4, 4, ".inp") == 0) files.push_back(n);
   }
   closedir(d);
   BOOST_CHECK(!files.empty());
   for (size_t i = 0; i < files.size(); ++i) {
      Sim s;
      std::vector<InpTable> tables = oprule_test::read_inp_file(std::string(dir) + "/" + files[i]);
      populate_model(tables);
      size_t rules = 0, exprs = 0;
      std::vector<std::string> failed = load_tables(s, tables, &rules, &exprs);
      std::cout << "[CORPUS] " << files[i] << ": " << rules << " rules, " << exprs << " expressions, "
                << failed.size() << " failed" << std::endl;
      for (size_t k = 0; k < failed.size(); ++k) BOOST_ERROR(files[i] + ": cannot parse " + failed[k]);
   }
}

BOOST_AUTO_TEST_SUITE_END()

// ============================================================ the study rules, behaviour over time
// Each test follows one pattern used in the study inputs through the real binding and the mock Fortran
// time loop (Harness::step). Steps are 15 minutes. A trigger that becomes true at the end of step n is
// applied in step n+1.

namespace {
void check_device_property(const char* fn, double Device::*value, DataSource Device::*source, bool rounds) {
   Sim s;
   Model& m = model();
   m.add_gate("g");
   m.add_device("g", "d");
   m.add_path_input("x", 0.);
   s.series["x"] = 3.7;
   BOOST_REQUIRE(s.add_rule("r", std::string("SET ") + fn + "(gate=g,device=d) TO ts(name=x)", "TRUE"));
   Device& d = m.device("g", "d");
   s.step();
   s.step();
   BOOST_CHECK_MESSAGE(std::fabs(d.*value - (rounds ? 4. : 3.7)) < 1e-9, std::string(fn) + " after the set");
   BOOST_CHECK_MESSAGE((d.*source).source_type == EXPRESSION_DATA, std::string(fn) + " source");
   s.step();
   // a time-series source reproduces the value every step; for nduplicate this means the model sees 3.7,
   // not the rounded 4 that the setter stored
   BOOST_CHECK_MESSAGE(std::fabs(d.*value - 3.7) < 1e-9, std::string(fn) + " from the data source");
}
}  // namespace

BOOST_FIXTURE_TEST_SUITE(study_rule_behaviour, Sim)

// clfct_gate_op, glc_barrier_elev, ...: SET <device property> TO ts(...) WHEN TRUE
BOOST_AUTO_TEST_CASE(gate_op_from_a_time_series_becomes_a_permanent_source) {
   Model& m = model();
   m.add_gate("clifton_court");
   m.add_device("clifton_court", "reservoir_gates");
   m.add_path_input("clfct_op", 0.);
   series["clfct_op"] = 0.5;
   BOOST_REQUIRE(add_rule("clfct_gate_op",
      "SET gate_op(gate=clifton_court,device=reservoir_gates,direction=from_node) TO ts(name=clfct_op)", "TRUE"));
   Device& d = m.device("clifton_court", "reservoir_gates");
   step();                                            // trigger true at the end of this step
   BOOST_CHECK_CLOSE(d.opCoefFromNode, 1., 1e-9);     // nothing written yet
   step();
   BOOST_CHECK_CLOSE(d.opCoefFromNode, 0.5, 1e-9);
   BOOST_CHECK_EQUAL(d.op_from_node.source_type, (int)EXPRESSION_DATA);
   BOOST_CHECK_EQUAL(d.op_to_node.source_type, (int)CONST_DATA);          // the other direction is untouched
   BOOST_CHECK_CLOSE(d.opCoefToNode, 1., 1e-9);
   BOOST_CHECK(!active("clfct_gate_op"));
   series["clfct_op"] = 0.25;
   step();
   BOOST_CHECK_CLOSE(d.opCoefFromNode, 0.25, 1e-9);                       // follows the series from now on
   series["clfct_op"] = 0.75;
   step();
   BOOST_CHECK_CLOSE(d.opCoefFromNode, 0.75, 1e-9);
}

BOOST_AUTO_TEST_CASE(height_elevation_width_and_nduplicate_follow_a_time_series) {
   check_device_property("gate_height", &Device::height, &Device::height_ds, false);
   check_device_property("gate_elev", &Device::baseElev, &Device::elev, false);
   check_device_property("gate_width", &Device::maxWidth, &Device::width, false);
   check_device_property("gate_nduplicate", &Device::nDuplicate, &Device::nduplicate, true);
}

// clfct_gate_cf_from / _to: the expression is evaluated once, when the rule runs (gate_coef is static).
BOOST_AUTO_TEST_CASE(gate_coef_from_an_expression_is_applied_only_once) {
   Model& m = model();
   m.add_channel(232, 20000.);
   m.add_gate("clifton_court");
   m.add_device("clifton_court", "reservoir_gates");
   m.add_path_input("clfct_height", 0.);
   set_level(232, 2.0);
   series["clfct_height"] = 6.0;
   BOOST_REQUIRE(add_rule("clfct_gate_cf_from",
      "SET gate_coef(gate=clifton_court,device=reservoir_gates,direction=from_node) TO "
      "MIN2( ts(name=clfct_height)/(chan_stage(channel=232,dist=0)+13.2), 1)*0.8 + 0.75", "TRUE"));
   Device& d = m.device("clifton_court", "reservoir_gates");
   step();
   step();
   const double expected = std::min(6.0 / (2.0 + 13.2), 1.) * 0.8 + 0.75;
   BOOST_CHECK_CLOSE(d.flowCoefFromNode, expected, 1e-9);
   series["clfct_height"] = 12.;
   set_level(232, 10.);
   step();
   step();
   BOOST_CHECK_CLOSE(d.flowCoefFromNode, expected, 1e-9);                 // later changes are ignored
   BOOST_CHECK_SMALL(d.flowCoefToNode, 1e-12);
}

// dcc_gate_op: two abrupt SET actions joined by WHILE
BOOST_AUTO_TEST_CASE(while_pair_sets_both_directions) {
   Model& m = model();
   m.add_gate("delta_cross_channel");
   m.add_device("delta_cross_channel", "cross_channel_gates");
   m.add_path_input("dcc_op", 0.);
   series["dcc_op"] = 1.0;
   BOOST_REQUIRE(add_rule("dcc_gate_op",
      "SET gate_op(gate=delta_cross_channel,device=cross_channel_gates,direction=from_node) TO ts(name=dcc_op)/2 WHILE "
      "SET gate_op(gate=delta_cross_channel,device=cross_channel_gates,direction=to_node) TO ts(name=dcc_op)/2", "TRUE"));
   Device& d = m.device("delta_cross_channel", "cross_channel_gates");
   step();
   step();
   BOOST_CHECK_CLOSE(d.opCoefFromNode, 0.5, 1e-9);
   BOOST_CHECK_CLOSE(d.opCoefToNode, 0.5, 1e-9);
   series["dcc_op"] = 0.8;
   step();
   BOOST_CHECK_CLOSE(d.opCoefFromNode, 0.4, 1e-9);
   BOOST_CHECK_CLOSE(d.opCoefToNode, 0.4, 1e-9);
}

// morrow_c_change_ndup: a constant set at a calendar date; a constant becomes a CONST data source
BOOST_AUTO_TEST_CASE(datetime_trigger_sets_a_constant) {
   Model& m = model();
   m.add_gate("morrow_c_line_outfall");
   m.add_device("morrow_c_line_outfall", "pipes");
   begin_run(1992, 9, 27, 22, 0, 900);                // step n ends at 22:00 + n * 15 min
   BOOST_REQUIRE(add_rule("morrow_c_change_ndup",
      "SET gate_nduplicate(gate=morrow_c_line_outfall,device=pipes) TO 2", "DATETIME >= 28SEP1992 00:00"));
   Device& d = m.device("morrow_c_line_outfall", "pipes");
   steps(7);
   BOOST_CHECK(!active("morrow_c_change_ndup"));
   step();                                            // step 8 ends at 28SEP1992 00:00
   BOOST_CHECK(active("morrow_c_change_ndup"));
   BOOST_CHECK_CLOSE(d.nDuplicate, 1., 1e-9);
   step();
   BOOST_CHECK_CLOSE(d.nDuplicate, 2., 1e-9);
   BOOST_CHECK_EQUAL(d.nduplicate.source_type, (int)CONST_DATA);
   BOOST_CHECK_CLOSE(d.nduplicate.value, 2., 1e-9);
   steps(5);
   BOOST_CHECK_CLOSE(d.nDuplicate, 2., 1e-9);         // the date stays true; the rule does not fire again
}

// mscs_close_from / mscs_close_to: two rules for the two directions of ONE device, same trigger. Because the
// resolver treats any two actions on a device as overlapping, the second waits for the first to finish.
BOOST_AUTO_TEST_CASE(directions_of_one_device_are_ramped_one_after_the_other) {
   BOOST_REQUIRE(load_file("oprule_montezuma_planning_gate.inp").empty());
   Model& m = model();
   Device& d = m.device("montezuma_salinity_control", "radial_gates");
   m.set_velocity(512, [](double) { return -0.5; });  // mscs_velclose: chan_vel < -0.1
   set_level(512, 0.);
   set_level(513, 0.);
   series["mscs_op"] = 0.;                            // mscs_op_season: ts < 1
   step();
   BOOST_CHECK(active("mscs_close_from"));
   BOOST_CHECK(!active("mscs_close_to"));             // deferred
   BOOST_CHECK(!active("mscs_open_from") && !active("mscs_open_to"));
   step();
   BOOST_CHECK_CLOSE(d.opCoefFromNode, 0.5, 1e-9);
   BOOST_CHECK_CLOSE(d.opCoefToNode, 1., 1e-9);
   step();
   BOOST_CHECK_SMALL(d.opCoefFromNode, 1e-12);
   BOOST_CHECK_CLOSE(d.opCoefToNode, 1., 1e-9);
   BOOST_CHECK(active("mscs_close_to"));              // starts only now
   step();
   BOOST_CHECK_CLOSE(d.opCoefToNode, 0.5, 1e-9);
   step();
   BOOST_CHECK_SMALL(d.opCoefToNode, 1e-12);
   BOOST_CHECK(!active("mscs_close_to"));
}

// decker_is_north_weir_close_frm / _open_frm: a static gate_coef ramped over 2 hours; the opposite rule waits
BOOST_AUTO_TEST_CASE(restoration_weir_ramps_down_then_up) {
   BOOST_REQUIRE(load_file("oprule_hist_restoration.inp").empty());
   Model& m = model();
   begin_run(2018, 9, 20, 10, 0, 900);                // step n ends at 10:00 + n * 15 min
   Device& d = m.device("decker_is_north_weir", "weir");
   d.flowCoefFromNode = 0.2;
   step();
   BOOST_CHECK(active("decker_is_north_weir_close_frm"));        // DATETIME < 12:00
   BOOST_CHECK(!active("decker_is_north_weir_open_frm"));
   steps(4);                                          // 4 advances of 8
   BOOST_CHECK_CLOSE(d.flowCoefFromNode, 0.1, 1e-9);
   steps(3);                                          // step 8: 12:00 reached, the open rule becomes true
   BOOST_CHECK(!active("decker_is_north_weir_open_frm"));        // but the close ramp still owns the device
   step();                                            // 8th advance: close ramp complete
   BOOST_CHECK_SMALL(d.flowCoefFromNode, 1e-12);
   BOOST_CHECK(!active("decker_is_north_weir_close_frm"));
   BOOST_CHECK(active("decker_is_north_weir_open_frm"));         // deferred rule starts now, from 0.0
   steps(4);
   BOOST_CHECK_CLOSE(d.flowCoefFromNode, 0.2, 1e-9);
   steps(4);
   BOOST_CHECK_CLOSE(d.flowCoefFromNode, 0.4, 1e-9);
   BOOST_CHECK(!active("decker_is_north_weir_open_frm"));
}

// dicu_div_151_off / _on: stage hysteresis on an external flow, alternating constant and series sources
BOOST_AUTO_TEST_CASE(external_flow_switches_with_stage) {
   BOOST_REQUIRE(load_file("oprule_hist_temp_barriers.inp").empty());
   Model& m = model();
   ExternalFlow& q = m.qext("dicu_div_151");
   q.datasource.value = -3.;
   series["dicu_div_151_flow"] = 3.;
   set_level(185, 3.0);                               // above 2.2: "on"
   steps(2);
   BOOST_CHECK_CLOSE(q.flow, -3., 1e-9);              // -1 * series
   BOOST_CHECK_EQUAL(q.datasource.source_type, (int)EXPRESSION_DATA);
   series["dicu_div_151_flow"] = 4.;
   step();
   BOOST_CHECK_CLOSE(q.flow, -4., 1e-9);
   set_level(185, 1.5);                               // below 2.0: "off"
   steps(2);
   BOOST_CHECK_SMALL(q.flow, 1e-12);
   BOOST_CHECK_EQUAL(q.datasource.source_type, (int)CONST_DATA);
   set_level(185, 2.1);                               // dead band: nothing changes
   steps(3);
   BOOST_CHECK_SMALL(q.flow, 1e-12);
   set_level(185, 2.5);                               // "on" again
   steps(2);
   BOOST_CHECK_CLOSE(q.flow, -4., 1e-9);
   BOOST_CHECK_EQUAL(q.datasource.source_type, (int)EXPRESSION_DATA);
   series["dicu_div_151_flow"] = 5.;
   step();
   BOOST_CHECK_CLOSE(q.flow, -5., 1e-9);
}

// glc_install_in / _out: install and remove a gate from a time series
BOOST_AUTO_TEST_CASE(gate_is_installed_and_removed_by_a_series) {
   BOOST_REQUIRE(load_file("oprule_hist_temp_barriers.inp").empty());
   Model& m = model();
   Gate& g = m.gate("grant_line_barrier");
   g.free = true;
   series["glc_install"] = 1.;
   run_until([&] { return !g.free; }, 12);
   BOOST_CHECK(!g.free);
   m.device("grant_line_barrier", "pipes").flow = 5.;
   series["glc_install"] = 0.;
   run_until([&] { return g.free; }, 12);
   BOOST_CHECK(g.free);
   BOOST_CHECK_SMALL(m.device("grant_line_barrier", "pipes").flow, 1e-12);   // setFree zeroes device flows
}

// glc_barrier_elev / glc_barrier_in: seasonal triggers; the second rule waits for the first (same device)
BOOST_AUTO_TEST_CASE(seasonal_rules_start_on_the_right_step_and_queue_per_device) {
   BOOST_REQUIRE(load_file("oprule_temp_barriers_planning.inp").empty());
   Model& m = model();
   begin_run(2018, 5, 15, 22, 0, 900);                // step n ends at 22:00 + n * 15 min; 16MAY 00:00 is step 8
   series["vernalis_flow"] = 5000.;
   series["orhrb_install"] = 0.5;                     // neither install nor remove for the head barrier
   Device& b = m.device("grant_line_barrier", "barrier");
   b.op_to_node.value = b.op_from_node.value = 0.;
   steps(8);
   BOOST_CHECK(!active("glc_barrier_elev"));          // SEASON > 16MAY is not true AT 16MAY 00:00
   step();
   BOOST_CHECK(active("glc_barrier_elev"));
   BOOST_CHECK(!active("glc_barrier_in"));            // also true now, but waits for the elevation ramp
   steps(3);
   BOOST_CHECK_CLOSE(b.baseElev, 2.813 * 0.75, 1e-9);
   step();
   BOOST_CHECK_CLOSE(b.baseElev, 2.813, 1e-9);
   BOOST_CHECK(!active("glc_barrier_elev"));
   BOOST_CHECK(active("glc_barrier_in"));
   step();
   BOOST_CHECK_CLOSE(b.opCoefToNode, 0.25, 1e-9);     // OPEN = 1.0 reached over 60 min, both directions
   BOOST_CHECK_CLOSE(b.opCoefFromNode, 0.25, 1e-9);
}

// vamp_8500_remove_wip: the trigger reads a writable name (gate_install) and compares it with INSTALL
BOOST_AUTO_TEST_CASE(trigger_reads_the_gate_install_state) {
   BOOST_REQUIRE(load_file("oprule_temp_barriers_planning.inp").empty());
   Model& m = model();
   begin_run(2018, 4, 20, 12, 0, 900);                // inside the a_vamp window (15APR..16MAY)
   series["vernalis_flow"] = 9000.;                   // > 8500
   series["orhrb_install"] = 0.5;
   Gate& g = m.gate("old_r@head_barrier");
   g.free = false;
   step();
   BOOST_CHECK(!g.free);
   step();
   BOOST_CHECK(g.free);                               // REMOVE applied
   steps(3);
   BOOST_CHECK(g.free);                               // trigger is now false (not installed); stays removed
}

BOOST_AUTO_TEST_SUITE_END()

// ============================================== what the language offers but the study inputs do not use

struct BasicSim : Sim {
   BasicSim() { build_basic_model(); }
};

BOOST_FIXTURE_TEST_SUITE(features_not_in_the_studies, BasicSim)

BOOST_AUTO_TEST_CASE(reservoir_values_as_triggers) {
   model().reservoir("res1").stage = 4.;
   BOOST_REQUIRE(add_rule("high_res", "SET ext_flow(name=q1) TO 0", "res_stage(res=res1) > 5"));
   BOOST_REQUIRE(add_rule("big_outflow", "SET ext_flow(name=q2) TO 1", "res_flow(res=res1, node=20) < -10"));
   steps(3);
   BOOST_CHECK_CLOSE(model().qext("q1").flow, -3., 1e-9);
   model().reservoir("res1").stage = 6.;
   model().reservoir("res1").qres[1] = -20.;
   steps(2);
   BOOST_CHECK_SMALL(model().qext("q1").flow, 1e-12);
   BOOST_CHECK_CLOSE(model().qext("q2").flow, 1., 1e-9);
}

BOOST_AUTO_TEST_CASE(transfer_flow_can_be_set_from_a_series) {
   series["ts1"] = 3.;
   BOOST_REQUIRE(add_rule("xfer", "SET transfer_flow(transfer=t1) TO ts(name=ts1) * 2", "TRUE"));
   steps(2);
   BOOST_CHECK_CLOSE(model().transfer("t1").flow, 6., 1e-9);
   BOOST_CHECK_EQUAL(model().transfer("t1").datasource.source_type, (int)EXPRESSION_DATA);
   series["ts1"] = 4.;
   step();
   BOOST_CHECK_CLOSE(model().transfer("t1").flow, 8., 1e-9);
}

BOOST_AUTO_TEST_CASE(channel_flow_as_a_trigger) {
   model().set_flow(185, [](double) { return 100.; });
   BOOST_REQUIRE(add_rule("reverse", "SET ext_flow(name=q2) TO 7", "chan_flow(channel=185, dist=500) < 0"));
   steps(3);
   BOOST_CHECK_SMALL(model().qext("q2").flow, 1e-12);
   model().set_flow(185, [](double) { return -5.; });
   steps(2);
   BOOST_CHECK_CLOSE(model().qext("q2").flow, 7., 1e-9);
}

BOOST_AUTO_TEST_CASE(startup_trigger_fires_once) {
   BOOST_REQUIRE(add_rule("init", "SET ext_flow(name=q1) TO 0", "STARTUP"));
   steps(2);
   BOOST_CHECK_SMALL(model().qext("q1").flow, 1e-12);
   model().qext("q1").datasource.value = -3.;         // restore the input value
   steps(5);
   BOOST_CHECK_CLOSE(model().qext("q1").flow, -3., 1e-9);   // not applied again
}

BOOST_AUTO_TEST_CASE(calendar_terms_as_triggers) {
   begin_run(2018, 6, 30, 22, 0, 900);                // 01JUL 00:00 is the end of step 8
   BOOST_REQUIRE(add_rule("july", "SET ext_flow(name=q1) TO 0", "MONTH == 7"));
   steps(7);
   BOOST_CHECK(!active("july"));
   step();
   BOOST_CHECK(active("july"));
   step();
   BOOST_CHECK_SMALL(model().qext("q1").flow, 1e-12);
}

BOOST_AUTO_TEST_CASE(lookup_table_on_a_series) {
   BOOST_REQUIRE(add_rule("table", "SET ext_flow(name=q1) TO lookup(ts(name=ts1), [0,10,20], [100,200])", "TRUE"));
   series["ts1"] = 15.;
   steps(2);
   BOOST_CHECK_CLOSE(model().qext("q1").flow, 200., 1e-9);
   series["ts1"] = 5.;
   step();
   BOOST_CHECK_CLOSE(model().qext("q1").flow, 100., 1e-9);     // the table is part of the permanent source
}

BOOST_AUTO_TEST_CASE(accumulate_in_a_trigger_counts_steps) {
   BOOST_REQUIRE(add_rule("after5", "SET ext_flow(name=q1) TO 0", "ACCUMULATE(1, 0) >= 5"));
   steps(4);
   BOOST_CHECK(!active("after5"));
   step();
   BOOST_CHECK(active("after5"));
}

// REFERENCE B9.12 / PLAN D-13: gate_coef accepts direction=both, but the Fortran flow-coefficient routines
// end the run (exit 3) for it. The first call happens when the rule is activated.
BOOST_AUTO_TEST_CASE(gate_coef_with_direction_both_ends_the_run) {
   BOOST_REQUIRE(add_rule("both", "SET gate_coef(gate=g1,device=d1,direction=both) TO 0.5", "TRUE"));
   BOOST_CHECK_THROW(step(), FortranExit);
}

BOOST_AUTO_TEST_CASE(bad_names_are_rejected_when_the_rule_is_parsed) {
   BOOST_CHECK(!add_rule("a", "SET gate_op(gate=nosuch,device=d1,direction=to_node) TO 0", "TRUE"));
   BOOST_CHECK(!add_rule("b", "SET ext_flow(name=q1) TO ts(name=undefined_series)", "TRUE"));
   BOOST_CHECK(!add_rule("c", "SET GATE_OP(gate=g1,device=d1,direction=to_node) TO 0", "TRUE"));
   BOOST_CHECK(!add_rule("d", "SET ext_flow(name=q1) TO 0", "nonsense > 1"));
   BOOST_CHECK(add_rule("e", "SET ext_flow(name=q1) TO 0", "TRUE"));
}

// PLAN CASE-06, CASE-07: names are lower-cased when stored, but names inside the text are not
BOOST_AUTO_TEST_CASE(rule_and_expression_name_case) {
   BOOST_CHECK(add_rule("Abc", "SET ext_flow(name=q1) TO 0", "TRUE"));
   BOOST_CHECK(!add_rule("abc", "SET ext_flow(name=q2) TO 0", "TRUE"));   // same name once lower-cased
   BOOST_REQUIRE(add_expression("Sjr_Flow", "ts(name=ts1)"));
   BOOST_CHECK(add_rule("lower_ref", "SET ext_flow(name=q2) TO 0", "sjr_flow > 1"));
   BOOST_CHECK(!add_rule("mixed_ref", "SET ext_flow(name=q2) TO 0", "Sjr_Flow > 1"));
}

// The functions Fortran calls (dsm2_oprule_management.cpp), driving the process-wide manager.
BOOST_AUTO_TEST_CASE(fortran_entry_points) {
   init_parser_f_();
   op_rulerestart(NULL);   // earlier tests left failed parses behind; parse_rule_ itself does not restart the scanner
   char text[] = "gr := SET ext_flow(name=q2) TO 9 WHEN TRUE;";
   BOOST_REQUIRE(parse_rule_(text, (int)std::strlen(text)));
   double dt = 900.;
   for (int i = 0; i < 3; ++i) {
      model().store_values();
      advanceopruleactions_(&dt);
      stepopruleexpressions_(&dt);
      testopruleactivation_();
   }
   BOOST_CHECK_CLOSE(model().qext("q2").flow, 9., 1e-9);
}

BOOST_AUTO_TEST_SUITE_END()



