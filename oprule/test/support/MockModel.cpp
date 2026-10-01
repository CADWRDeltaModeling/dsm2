#include "MockModel.h"

#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

namespace oprule_test {

std::map<std::string, double> g_vars;
std::map<std::string, bool> g_datasource_set;
std::map<std::string, int> g_set_count;

using oprule::parser::NamedValueLookup;

namespace {
oprule::expression::DoubleNodePtr arg_name_to_var(const NamedValueLookup::ArgMap& args, bool td) {
   NamedValueLookup::ArgMap::const_iterator it = args.find("name");
   if (it == args.end()) throw oprule::parser::MissingIdentifier("name not given");
   return VarInterface::create(it->second, td);
}
oprule::expression::DoubleNodePtr mock_var_factory(const NamedValueLookup::ArgMap& a) { return arg_name_to_var(a, false); }
oprule::expression::DoubleNodePtr mock_tvar_factory(const NamedValueLookup::ArgMap& a) { return arg_name_to_var(a, true); }
oprule::expression::DoubleNodePtr mock_ro_factory(const NamedValueLookup::ArgMap& a) {
   NamedValueLookup::ArgMap::const_iterator it = a.find("name");
   if (it == a.end()) throw oprule::parser::MissingIdentifier("name not given");
   return oprule::expression::DoubleNodePtr(new ReadOnlyVar(it->second));
}
}  // namespace

MockLookup::MockLookup() {
   oprule::parser::ModelNameInfo info;
   info.params.push_back("name");
   info.type = NamedValueLookup::READWRITE;
   info.name = "mock_var";
   info.factory = &mock_var_factory;
   add("mock_var", info);
   info.name = "mock_tvar";
   info.factory = &mock_tvar_factory;
   add("mock_tvar", info);
   info.type = NamedValueLookup::READONLY;
   info.name = "mock_ro";
   info.factory = &mock_ro_factory;
   add("mock_ro", info);
}

bool SameVarResolver::overlap(oprule::rule::OperationAction& a1, oprule::rule::OperationAction& a2) {
   typedef oprule::rule::ModelAction<double> MA;
   MA* m1 = dynamic_cast<MA*>(&a1);
   MA* m2 = dynamic_cast<MA*>(&a2);
   if (!m1 || !m2) return false;
   boost::shared_ptr<VarInterface> v1 = boost::dynamic_pointer_cast<VarInterface>(m1->getModelInterface());
   boost::shared_ptr<VarInterface> v2 = boost::dynamic_pointer_cast<VarInterface>(m2->getModelInterface());
   return v1 && v2 && v1->name == v2->name;
}

oprule::rule::OperationActionPtr make_action(const std::string& var, double target, double ramp_sec, bool time_dep) {
   oprule::rule::TransitionPtr tr = ramp_sec > 0.
      ? oprule::rule::TransitionPtr(new oprule::rule::LinearTransition(ramp_sec))
      : oprule::rule::TransitionPtr(new oprule::rule::AbruptTransition());
   return oprule::rule::OperationActionPtr(new oprule::rule::ModelAction<double>(
      VarInterface::create(var, time_dep), oprule::expression::DoubleScalarNode::create(target), tr));
}

RuleHandle::RuleHandle(const std::string& var, double target, double ramp_sec, bool time_dep)
   : trigger(new FlagTrigger),
     rule(new oprule::rule::OperatingRule(make_action(var, target, ramp_sec, time_dep),
                                          oprule::rule::TriggerPtr(trigger))) {}

RuntimeFixture::RuntimeFixture() : manager(resolver) {
   g_vars.clear();
   g_datasource_set.clear();
   g_set_count.clear();
}

void RuntimeFixture::step(double dt) {
   manager.advanceActions(dt);
   manager.stepExpressions(dt);
   manager.manageActivation();
}

ParserFixture::ParserFixture() : lookup(new MockLookup), timeFactory(new RecordingTimeFactory) {
   g_vars.clear();
   g_datasource_set.clear();
   g_set_count.clear();
   lexer_init();
   init_expression();
   init_rule_names();
   get_lagged_vals().clear();
   init_lookup(lookup);
   init_model_time_factory(timeFactory);
}

ParserFixture::~ParserFixture() {
   clear_temp_expr();
   clear_arg_map();
   init_expression();
   init_rule_names();
   delete lookup;
   delete timeFactory;
}

int ParserFixture::parse(const std::string& text) {
   op_rulerestart(NULL);  // the scanner keeps its buffer after a failed parse
   return parse_no_restart(text);
}

int ParserFixture::parse_no_restart(const std::string& text) {
   std::string s(text);
   set_input_string(s);
   return op_ruleparse();
}

int run_in_child(void (*fn)()) {
   fflush(NULL);
   pid_t pid = fork();
   if (pid == 0) {
      // Boost.Test installed handlers that would resume the whole test run inside the child.
      const int sigs[] = {SIGSEGV, SIGABRT, SIGBUS, SIGFPE, SIGILL};
      for (size_t i = 0; i < sizeof(sigs) / sizeof(sigs[0]); ++i) signal(sigs[i], SIG_DFL);
      fn();
      _exit(0);
   }
   int status = 0;
   waitpid(pid, &status, 0);
   if (WIFEXITED(status)) return WEXITSTATUS(status);
   if (WIFSIGNALED(status)) return -WTERMSIG(status);
   return -1;
}

}  // namespace oprule_test
