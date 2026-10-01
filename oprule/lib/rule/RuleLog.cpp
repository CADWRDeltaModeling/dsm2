#include "oprule/rule/RuleLog.h"
#include <cmath>
#include <fstream>
#include <iomanip>
#include <ostream>
#include <sstream>

namespace oprule {
namespace rule {

namespace {
int g_level = RuleLog::OFF;
std::ostream* g_sink = NULL;
std::ofstream g_file;
RuleLog::TimeSource g_time = NULL;
std::string g_context;
}

void RuleLog::setLevel(int level){
   if (level < OFF) level = OFF;
   if (level > ACTIONS) level = ACTIONS;
   g_level = level;
}

int RuleLog::level(){ return g_level; }

bool RuleLog::enabled(int level){
   return g_sink != NULL && level <= g_level && level > OFF;
}

bool RuleLog::hasSink(){ return g_sink != NULL; }

bool RuleLog::open(const std::string& path){
   close();
   g_file.open(path.c_str(), std::ios::out | std::ios::trunc);
   if (!g_file.is_open()) return false;
   g_sink = &g_file;
   return true;
}

void RuleLog::close(){
   if (g_sink == &g_file) g_sink = NULL;
   if (g_file.is_open()) g_file.close();
}

void RuleLog::setSink(std::ostream* sink){
   close();
   g_sink = sink;
}

void RuleLog::setTimeSource(TimeSource source){ g_time = source; }

void RuleLog::setContext(const std::string& rule){ g_context = rule; }

const std::string& RuleLog::context(){ return g_context; }

void RuleLog::write(int level, const std::string& event,
                    const std::string& rule, const std::string& detail){
   if (!enabled(level)) return;
   *g_sink << (g_time ? g_time() : std::string()) << " | " << event << " | "
           << rule << " | " << detail << '\n';
   g_sink->flush();
}

std::string RuleLog::number(double value){
   if (value != value) return "nan";
   if (value == HUGE_VAL || value == -HUGE_VAL) return "unset";
   std::ostringstream s;
   s << std::setprecision(9) << value;
   return s.str();
}

std::string RuleLog::state(const StateList& values){
   std::string out = "[";
   bool first = true;
   for (size_t i = 0; i < values.size(); ++i){
      // the same variable read through several named expressions is listed once
      bool seen = false;
      for (size_t j = 0; j < i && !seen; ++j)
         seen = values[j].first == values[i].first && values[j].second == values[i].second;
      if (seen) continue;
      if (!first) out += "; ";
      first = false;
      out += values[i].first + "=" + number(values[i].second);
   }
   return out + "]";
}

}}     //namespace
