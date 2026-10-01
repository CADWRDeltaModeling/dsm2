#include "Dsm2Harness.h"

#include <cctype>

#include "oprule/parser/ParseSymbolManagement.h"

extern void set_input_string(std::string& input);
extern int op_ruleparse(void);
extern void op_rulerestart(FILE* input_file);

namespace dsm2mock {

Harness::Harness() : manager(resolver) {
   model().reset();
   init_parser_f_();  // lexer_init, init_expression, new DSM2HydroNamedValueLookup, new DSM2HydroTimeNodeFactory
   init_rule_names();
   get_lagged_vals().clear();
   begin_run(2001, 1, 1, 0, 0, 900);
}

Harness::~Harness() {
   clear_temp_expr();
   clear_arg_map();
   init_expression();
   init_rule_names();
}

static std::string lower(std::string s) {
   for (size_t i = 0; i < s.size(); ++i) s[i] = (char)std::tolower((unsigned char)s[i]);
   return s;
}

bool Harness::parse_text(const std::string& text) {
   op_rulerestart(NULL);
   std::string s(text);
   set_input_string(s);
   int rc = op_ruleparse();
   if (rc != 0 || get_parsed_type() == oprule::parser::PARSE_ERROR) return false;
   if (get_parsed_type() == oprule::parser::OP_RULE) manager.addRule(getOperatingRule());
   return true;
}

bool Harness::add_expression(const std::string& name, const std::string& definition) {
   return parse_text(lower(name) + " := " + definition + ";");
}

bool Harness::add_rule(const std::string& name, const std::string& action, const std::string& trigger) {
   return parse_text(lower(name) + " := " + action + " WHEN " + trigger + ";");
}

void Harness::begin_run(int year, int month, int day, int hour, int minute, int dt_seconds) {
   model().dt_seconds = dt_seconds;
   model().set_time(year, month, day, hour, minute);
   model().julmin += dt_seconds / 60;
}

void Harness::step() {
   Model& m = model();
   double dt = (double)m.dt_seconds;
   m.set_boundary_values_from_data(ts_source);
   manager.advanceActions(dt);
   if (after_advance) after_advance();
   if (solver) solver(dt);
   manager.stepExpressions(dt);
   manager.manageActivation();
   m.julmin += m.dt_seconds / 60;
}

oprule::rule::OperatingRulePtr Harness::rule(const std::string& name) {
   oprule::rule::OperatingRulePtr r = getOperatingRule(lower(name));
   if (!r) throw std::logic_error("test error: no rule named " + name);
   return r;
}

}  // namespace dsm2mock
