#include "InpReader.h"

#include <algorithm>
#include <cctype>
#include <fstream>

namespace oprule_test {

namespace {

std::string trim(const std::string& s) {
   size_t b = s.find_first_not_of(" \t\r\n");
   if (b == std::string::npos) return "";
   size_t e = s.find_last_not_of(" \t\r\n");
   return s.substr(b, e - b + 1);
}

std::vector<std::string> split_fields(const std::string& line) {
   std::vector<std::string> out;
   size_t i = 0, n = line.size();
   while (i < n) {
      while (i < n && std::isspace((unsigned char)line[i])) ++i;
      if (i >= n) break;
      std::string field;
      if (line[i] == '"') {
         ++i;
         while (i < n && line[i] != '"') field += line[i++];
         ++i;  // closing quote
      } else {
         while (i < n && !std::isspace((unsigned char)line[i])) field += line[i++];
      }
      out.push_back(field);
   }
   return out;
}

bool is_table_name(const std::string& s) {
   if (s.empty() || s == "END") return false;
   for (size_t i = 0; i < s.size(); ++i)
      if (!(std::isupper((unsigned char)s[i]) || s[i] == '_')) return false;
   return true;
}

std::string lower(std::string s) {
   for (size_t i = 0; i < s.size(); ++i) s[i] = (char)std::tolower((unsigned char)s[i]);
   return s;
}

}  // namespace

std::vector<InpTable> read_inp(std::istream& in) {
   std::vector<InpTable> tables;
   std::string raw;
   bool in_table = false, need_header = false;
   while (std::getline(in, raw)) {
      size_t hash = raw.find('#');
      if (hash != std::string::npos) raw = raw.substr(0, hash);
      std::string line = trim(raw);
      if (line.empty()) continue;
      if (!in_table) {
         if (is_table_name(line)) {
            InpTable t;
            t.name = line;
            tables.push_back(t);
            in_table = need_header = true;
         }
         continue;
      }
      if (line == "END") {
         in_table = false;
         continue;
      }
      if (need_header) {
         need_header = false;
         continue;
      }
      if (line[0] == '^') continue;
      tables.back().rows.push_back(split_fields(line));
   }
   return tables;
}

std::vector<InpTable> read_inp_file(const std::string& path) {
   std::ifstream f(path.c_str());
   if (!f) return std::vector<InpTable>();
   return read_inp(f);
}

std::vector<OpruleStatement> statements_in_parse_order(const std::vector<InpTable>& tables) {
   std::vector<OpruleStatement> expressions, rules;
   for (size_t t = 0; t < tables.size(); ++t) {
      for (size_t r = 0; r < tables[t].rows.size(); ++r) {
         const std::vector<std::string>& row = tables[t].rows[r];
         OpruleStatement s;
         if (tables[t].name == "OPRULE_EXPRESSION" && row.size() >= 2) {
            s.is_rule = false;
            s.name = row[0];
            s.definition = row[1];
            expressions.push_back(s);
         } else if (tables[t].name == "OPERATING_RULE" && row.size() >= 3) {
            s.is_rule = true;
            s.name = row[0];
            s.action = row[1];
            s.trigger = row[2];
            rules.push_back(s);
         }
      }
   }
   struct ByName {
      bool operator()(const OpruleStatement& a, const OpruleStatement& b) const { return a.name < b.name; }
   };
   std::stable_sort(expressions.begin(), expressions.end(), ByName());
   std::stable_sort(rules.begin(), rules.end(), ByName());
   expressions.insert(expressions.end(), rules.begin(), rules.end());
   return expressions;
}

std::vector<std::string> time_series_names(const std::vector<InpTable>& tables) {
   std::vector<std::string> names;
   for (size_t t = 0; t < tables.size(); ++t)
      if (tables[t].name == "OPRULE_TIME_SERIES")
         for (size_t r = 0; r < tables[t].rows.size(); ++r)
            if (!tables[t].rows[r].empty()) names.push_back(lower(tables[t].rows[r][0]));
   return names;
}

}  // namespace oprule_test
