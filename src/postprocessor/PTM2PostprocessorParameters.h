/**
 * Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _PTM2PostprocessorParameters_h_
#define _PTM2PostprocessorParameters_h_

#include "Parameters.h"
#include "PTM2Group.h"
#include "PTM2FluxOutput.h"
#include "PTM2GroupOutput.h"
#include <list>
#include <vector>

namespace PTM2
{

class PTM2PostprocessorParameters : public Parameters
{
private:
  std::vector<PTM2Group> groups_;
  std::list<PTM2FluxOutput> fluxItems_;
  std::vector<PTM2GroupOutput> groupItems_;
  bool accumulated_;
  bool fluxPercentaged_;
  bool groupPercentaged_;
  int nParticles_;
 
public:
  PTM2PostprocessorParameters();
  void addGroup(const PTM2Group& group) { groups_.push_back(group); }
  PTM2Group* findGroup(const std::string& name);
  const std::vector<PTM2Group>& getGroups() const { return groups_; }
  void addFluxOutput(const PTM2FluxOutput& flux) { fluxItems_.push_back(flux); }
  const std::list<PTM2FluxOutput>& getFluxOutputs() const { return fluxItems_; }
  void addGroupOutput(const PTM2GroupOutput& group) { groupItems_.push_back(group); }
  const std::vector<PTM2GroupOutput>& getGroupOutputs() const { return groupItems_; }
  void setAccumulated(const bool f) { accumulated_ = f; }
  bool getAccumulated() const { return accumulated_; }
  void setFluxPercentaged(const bool f) { fluxPercentaged_ = f; }
  bool getFluxPercentaged() const { return fluxPercentaged_; }
  void setGroupPercentaged(const bool f) { groupPercentaged_ = f; }
  bool getGroupPercentaged() const { return groupPercentaged_; }
  void setTotalNParticles(const int& nParticles) { nParticles_ = nParticles; }
  int getTotalNParticles() const { return nParticles_; }
};

}

#endif
