/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _PTM2FluxOutput_h_
#define _PTM2FluxOutput_h_

#include "Waterbody.h"
#include <string>
#include <vector>

namespace PTM2 {

class PTM2FluxOutput {
public:
  std::string name_;
  std::vector<int> fromWaterbodyTypes_;
  std::vector<int> fromWaterbodies_;
  std::vector<int> toWaterbodyTypes_;
  std::vector<int> toWaterbodies_;
  int interval_;
  std::string fname_;

public:
  PTM2FluxOutput();
  PTM2FluxOutput(const PTM2FluxOutput& r) 
    : name_(r.name_), fromWaterbodies_(r.fromWaterbodies_),
    fromWaterbodyTypes_(r.fromWaterbodyTypes_),
    toWaterbodies_(r.toWaterbodies_), toWaterbodyTypes_(r.toWaterbodyTypes_),
    interval_(r.interval_), fname_(r.fname_)
  {}
  PTM2FluxOutput(const std::string& name,
                 const std::vector<int>& fromWaterbodyTypes,
                 const std::vector<int>& fromWaterbodies,
                 const std::vector<int>& toWaterbodyTypes,
                 const std::vector<int>& toWaterbodies,
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

#endif
