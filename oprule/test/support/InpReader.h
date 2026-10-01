// Minimal reader for the DSM2 text input tables used by operating rules:
//   OPERATING_RULE (NAME ACTION TRIGGER), OPRULE_EXPRESSION (NAME DEFINITION),
//   OPRULE_TIME_SERIES (NAME FILLIN FILE PATH).
//
// It follows the behaviour of input_storage/src (InputState.cpp, ItemInputState.cpp):
//   * '#' starts a comment anywhere on a line, even inside quotes (everything after it is dropped)
//   * a table is: a line with the table name, a header line, data lines, END
//   * a data line starting with '^' is ignored (marks the item unused)
//   * fields are separated by white space; double quotes group a field and are removed
// ${VAR} environment substitution and layering are NOT done here.
#ifndef OPRULE_TEST_INP_READER_H
#define OPRULE_TEST_INP_READER_H

#include <istream>
#include <string>
#include <vector>

namespace oprule_test {

struct InpTable {
   std::string name;                              // e.g. OPERATING_RULE
   std::vector<std::vector<std::string> > rows;   // data rows, fields already unquoted
};

std::vector<InpTable> read_inp(std::istream& in);
std::vector<InpTable> read_inp_file(const std::string& path);  // empty if the file cannot be opened

// A rule or expression ready to hand to the parser.
struct OpruleStatement {
   std::string name;        // as written in the input
   std::string action;      // rules only
   std::string trigger;     // rules only
   std::string definition;  // expressions only
   bool is_rule;
};

// All rules/expressions found in the tables, in the order the model parses them: expressions
// first, then rules, each group sorted lexicographically by NAME (the input_storage buffer is
// sorted by identifier before it is handed to Fortran).
std::vector<OpruleStatement> statements_in_parse_order(const std::vector<InpTable>& tables);

// Names of OPRULE_TIME_SERIES entries.
std::vector<std::string> time_series_names(const std::vector<InpTable>& tables);

}  // namespace oprule_test

#endif
