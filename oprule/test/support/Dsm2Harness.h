// Drives the real DSM2 oprule C++ binding against the mock Fortran model, in the same order as the
// hydro time loop. Use one Harness per test case: it resets the mock model and the parser state.
#ifndef OPRULE_TEST_DSM2_HARNESS_H
#define OPRULE_TEST_DSM2_HARNESS_H

#include <functional>
#include <string>

#include "Dsm2FortranMock.h"
#include "oprule/rule/ModelAction.h"  // ModelInterfaceActionResolver.h needs it but does not include it
#include "dsm2_model_interface_resolver.h"
#include "oprule/rule/ModelInterfaceActionResolver.h"
#include "oprule/rule/OperationManager.h"

// Entry points that Fortran calls (dsm2/src/oprule_interface/dsm2_oprule_management.cpp).
extern "C" {
void init_parser_f_();
bool parse_rule_(char* text, int len);
void stepopruleexpressions_(double* dt_sec);
void advanceopruleactions_(double* dt_sec);
void testopruleactivation_();
}

namespace dsm2mock {

class Harness {
public:
   Harness();
   ~Harness();

   // fixed/process_oprule.f90 process_oprule_expression: lower-cases the name, parses
   // "<name> := <definition>;". Returns false on a parse error (the model would exit(-3)).
   bool add_expression(const std::string& name, const std::string& definition);

   // fixed/process_oprule.f90 process_oprule: lower-cases the name, parses
   // "<name> := <action> WHEN <trigger>;" and, if it is a rule, adds it to the manager
   // (what dsm2_oprule_management.cpp parse_rule does with its global manager).
   // The scanner is restarted first so one failed parse cannot poison the next.
   bool add_rule(const std::string& name, const std::string& action, const std::string& trigger);

   // hydro/fourpt.f90 fourpt_init: the model starts the first step with julmin = start + dt, i.e. the
   // time at the END of the first step is what rules see during step 1.
   void begin_run(int year, int month, int day, int hour, int minute, int dt_seconds);

   // hydrolib/update_network.f90 UpdateNetwork:
   //   SetBoundaryValuesFromData  (time series refreshed, every data source copied into the model)
   //   AdvanceOpRuleActions       (active rules write values)
   //   ApplyBoundaryValues        (copies model values to the solver: nothing to mock)
   //   Newton iteration           (the optional `solver` hook updates stage/flow/velocity)
   //   StepOpRuleExpressions, TestOpRuleActivation
   // then fourpt_step advances julmin by dt.
   void step();
   void steps(int n) { for (int i = 0; i < n; ++i) step(); }

   oprule::rule::OperatingRulePtr rule(const std::string& name);
   bool active(const std::string& name) { return rule(name)->isActive(); }

   std::function<double(const std::string&, int)> ts_source;  // value of a time series at julmin
   std::function<void(double)> solver;                        // called with dt after advance, before step/test
   std::function<void()> after_advance;                       // called right after the actions advance (the device sampler)

   oprule::rule::ModelInterfaceActionResolver<DSM2Resolver, DSM2ModelInterfaceResolver> resolver;
   oprule::rule::OperationManager manager;

private:
   bool parse_text(const std::string& text);
};

}  // namespace dsm2mock

#endif
