/**
 * Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _PTM2GroupOutput_h_
#define _PTM2GroupOutput_h_

#include <vector>
#include <string>

namespace PTM2
{

class PTM2GroupOutput
{
public:
  std::string name_;
  std::vector<int> waterbodyTypes_;
  std::vector<int> waterbodies_;
  int interval_;
  std::string fname_;

public:
  PTM2GroupOutput();
  PTM2GroupOutput(const PTM2GroupOutput& r)
    : name_(r.name_), waterbodies_(r.waterbodies_),
    waterbodyTypes_(r.waterbodyTypes_), interval_(r.interval_), fname_(r.fname_)
  {}
  PTM2GroupOutput(const std::string& name,
                 const std::vector<int>& waterbodyTypes,
                 const std::vector<int>& waterbodies,
                 const int& interval,
                 const std::string& fname);
  const std::string& getName() const { return name_; }
  const std::string& getFname() const { return fname_; }
  const int getInterval() const { return interval_; }
  int match(const int& fromWb,
            const int& fromWbT,
            const int& toWb,
            const int& toWbT) const;
  bool inWaterbodies(const int& wb,
                     const int& wbT,
                     const std::vector<int>& waterbodies,
                     const std::vector<int>& waterbodiesT) const;
};

}

#endif //
