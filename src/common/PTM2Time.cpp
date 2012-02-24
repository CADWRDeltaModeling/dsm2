/*
 * Author: Kijin Nam, knam@water.ca.gov
 */

#include "PTM2Time.h"
#include "boost/algorithm/string.hpp"

namespace PTM2
{

PTM2Time* PTM2Time::pInstance_ = NULL;

void PTM2Time::parseUndelimitedUSdate(PTM2Time::Date& date,
                                      const std::string& s) const
{
  int offsets[] = { 2, 3, 4 };
  int pos = 0;
  typedef Date::month_type month_type;

  unsigned short y = 0, m = 0, d = 0;

  boost::offset_separator osf(offsets, offsets + 3, false, false);

  typedef boost::tokenizer<boost::offset_separator,
      std::basic_string<char>::const_iterator, std::basic_string<char> > tokenizer_type;
  tokenizer_type tok(s, osf);
  for (tokenizer_type::iterator ti = tok.begin(); ti != tok.end(); ++ti) {
    switch (pos) {
    case 0:
      d = boost::lexical_cast<unsigned short>(*ti);
      break;
    case 1:
      m = boost::date_time::month_str_to_ushort<month_type>(*ti);
      break;
    case 2:
      y = boost::lexical_cast<unsigned short>(*ti);
      break;
    }
    pos++;
  }
  date = Date(y, m, d);
}

void PTM2Time::parseUndelimited24Hours(int& minutes, const std::string& s) const
{
  int offsets[] = { 2, 2 };
  int pos = 0;

  int h = 0, m = 0;

  boost::offset_separator osf(offsets, offsets + 2, false, false);

  typedef boost::tokenizer<boost::offset_separator,
      std::basic_string<char>::const_iterator, std::basic_string<char> > tokenizer_type;
  tokenizer_type tok(s, osf);
  for (tokenizer_type::iterator ti = tok.begin(); ti != tok.end(); ++ti) {
    const unsigned short i = boost::lexical_cast<int>(*ti);
    switch (pos) {
    case 0:
      h = i;
      break;
    case 1:
      m = i;
      break;
    }
    pos++;
  }
  minutes = h * 36 + m;
}

void PTM2Time::datesec2julsec(Real& julsec,
                              const Date& date,
                              const Real& seconds) const
{
  const long days = (date - PTM2Time::Date(1899, 12, 31)).days();
  julsec = Real(days) * 86400. + seconds;
}

void PTM2Time::datemin2julmin(int& julmin,
                              const Date& date,
                              const int& minutes) const
{
  const long days = (date - PTM2Time::Date(1899, 12, 31)).days();
  julmin = int(days) * 1440 + minutes;
}

void PTM2Time::interval2seconds(Real& sec, const std::string& s) const
{
  std::string str(s);
  boost::algorithm::to_lower(str);
  std::size_t pos;
  pos = str.rfind("min");
  if (pos != std::string::npos) {
    sec = boost::lexical_cast<Real>(str.substr(0, pos)) * 60.;
    return;
  }
  pos = str.rfind("hour");
  if (pos != std::string::npos) {
    sec = boost::lexical_cast<Real>(str.substr(0, pos)) * 3600.;
    return;
  }
  throw std::runtime_error("Not a correct duration format.");
}

void PTM2Time::interval2minutes(int& minutes, const std::string& s) const
{
  std::string str(s);
  boost::algorithm::to_lower(str);
  std::size_t pos;
  pos = str.rfind("min");
  if (pos != std::string::npos) {
    minutes = boost::lexical_cast<int>(str.substr(0, pos));
    return;
  }
  pos = str.rfind("hour");
  if (pos != std::string::npos) {
    minutes = boost::lexical_cast<int>(str.substr(0, pos)) * 60;
    return;
  }
  throw std::runtime_error("Not a correct duration format.");
}

}
