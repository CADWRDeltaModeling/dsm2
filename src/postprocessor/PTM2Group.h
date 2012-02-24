/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _PTM2Group_h_
#define _PTM2Group_h_

#include "Waterbody.h"

namespace PTM2
{

class PTM2Group
{
private:
  std::string name_;
  std::vector<const Waterbody*> waterbodies_;

public:
  PTM2Group(const std::string& name) : name_(name) {}
  PTM2Group(const PTM2Group& group) : name_(group.name_), waterbodies_(group.waterbodies_) {}
  void setName(const std::string& name) { name_ = name; }
  void addWaterbody(const Waterbody* waterbody) { waterbodies_.push_back(waterbody); }
  bool operator==(const PTM2Group& group) const { return name_.compare(group.name_) ==  0 ? true : false; }
  bool operator==(const std::string& name) const { return name_.compare(name_) ==  0 ? true : false; }
  int nWaterbodies() const { return waterbodies_.size(); }
  const Waterbody* getWaterbody(const int i) const { return waterbodies_[i]; }
};

}

#endif
