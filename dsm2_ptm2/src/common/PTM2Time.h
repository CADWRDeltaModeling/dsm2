/*
 * Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _PTM2Time_h_
#define _PTM2Time_h_

#include "PTM2.h"
#include "boost/date_time/posix_time/posix_time.hpp"

namespace PTM2
{

/**
 * @brief A singleton to manage time in the PTM2
 *
 * This class is a singleton class to manage or manage different time units.
 * The major time unit in the model is in julian seconds based on 1990.
 * Thus it will be one day off from Gregorian days.
 * Also, it does not account leap seconds correctly.
 * The leap seconds since 1950 are total 25 seconds so far.
 */
class PTM2Time
{
  // Date types
public:
  // Refer to boost posix_time for details.
  typedef boost::gregorian::date Date;
  typedef boost::posix_time::ptime Time;
  typedef boost::posix_time::time_duration TimeDuration;
  typedef boost::gregorian::date_duration DateDuration;
//  using boost::posix_time::minutes;
//  using boost::posix_time::hours;
//  using boost::posix_time::seconds;

// Methods
public:
  /**
   * @brief Get the sigleton pointer
   */
  static PTM2Time* instance()
  {
    if (!pInstance_)
      pInstance_ = new PTM2Time;
    return pInstance_;
  }
  void setBase(const int& v)
  {
    julminBase_ = v;
  }
  const int& getBase() const
  {
    return julminBase_;
  }

  // Tools with Boost date_time library
  /**
   * @brief Parse DDMMMYYY format string into Date format.
   */
  void parseUndelimitedUSdate(Date& date, const std::string& s) const;
  /**
   * @brief Parse HHMM into min duration
   */
  void parseUndelimited24Hours(int& min, const std::string& s) const;
  /**
   * @brief Convert date and seconds duration into Julian seconds based on 1990
   */
  void datesec2julsec(Real& julsec,
                      const Date& date,
                      const Real& seconds = 0.) const;
  void datemin2julmin(int& julmin,
                      const Date& date,
                      const int& minutes = 0) const;
  void interval2minutes(int& minutes, const std::string& s) const;
  void interval2seconds(Real& sec, const std::string& s) const;
  /**
   * @brief Convert Julian seconds to Julian minutes.  Discard remnant seconds.
   */
  void julsec2julmin(int& julmin, const Real& julsec) const
  {
    julmin = int(julsec / 60.);
  }
  /**
   * @brief Convert Julian seconds to Julian minutes.  Discard remnant seconds.
   */
  int julsec2julmin(const Real& julsec) const
  {
    return int(julsec / 60.);
  }
  Time julsec2Time(const Real& julsec) const
  {
    // This is necessary to prevent overrun.
    long julmin = long(julsec / 60.);
    return Time(Date(1899, 12, 31), TimeDuration(0, long(julmin), 0));
  }
  Time julmin2Time(const int& julmin) const
  {
    // This is necessary to prevent overrun.
    return Time(Date(1899, 12, 31), TimeDuration(0, julmin, 0));
  }

private:
  // Constructor is private
  PTM2Time()
  {
  }

  // Properties
private:
  static PTM2Time* pInstance_;
  int julminBase_;
};

}

#endif /* __PTM2Time_h__ */
