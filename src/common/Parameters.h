/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Parameters_h_
#define _Parameters_h_

#include "PTM2.h"
#include <string>

namespace PTM2
{

/**
 * @brief Parameter class 
 *
 * This class hold parameters of the model.
 **/
class Parameters
{
protected:
  std::string modifier_;
  // All time units are in Julian minutes based on 1900/1/1
  // This is chosen on purpose to avoid floating number comparison issues.
  // This means that minimal resolution for users are minute for control.
  int timeStart_;
  int timeEnd_;
  int dt_;
  int timeCurrent_;
  int timeNext_;
  Real timeNextSecond_;

  std::string hdf5OutputFilename_;
  int h5Trace_;
  int h5Detail_;

public:
  const std::string& getModifier() const
  {
    return modifier_;
  }
  void setModifier(const std::string& str)
  {
    modifier_ = str;
  }
  const int& getTimeStart() const
  {
    return timeStart_;
  }
  void setTimeStart(const int& timeStart)
  {
    timeStart_ = timeStart;
  }
  const int& getTimeEnd() const
  {
    return timeEnd_;
  }
  void setTimeEnd(const int& timeEnd)
  {
    timeEnd_ = timeEnd;
  }
  int& getTimeCurrent()
  {
    return timeCurrent_;
  }
  const int& getTimeCurrent() const
  {
    return timeCurrent_;
  }
  int& getTimeNext()
  {
    return timeNext_;
  }
  const int& getTimeNext() const
  {
    return timeNext_;
  }
  void setTimeNext(const int& timeNext)
  {
    timeNext_ = timeNext;
    timeNextSecond_ = Real(timeNext_) * 60.;
  }
  const Real& getTimeNextSecond() const
  {
    return timeNextSecond_;
  }
  const int& getDt() const
  {
    return dt_;
  }
  void setDt(const int& dt)
  {
    dt_ = dt;
  }
  const std::string& getHdf5OutputFilename() const
  {
    return hdf5OutputFilename_;
  }
  void setHdf5OutputFilename(const std::string& fname)
  {
    hdf5OutputFilename_ = fname;
  }
  void setH5Trace(const int flag)
  {
    h5Trace_ = flag;
  }
  int getH5Trace() const
  {
    return h5Trace_;
  }
  void setH5Detail(const int flag)
  {
    h5Detail_ = flag;
  }
  int getH5Detail() const
  {
    return h5Detail_;
  }
};

}

#endif
