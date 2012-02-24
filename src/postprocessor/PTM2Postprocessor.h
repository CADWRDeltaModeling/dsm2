/**
 * Author: Kijin Nam
 **/

#ifndef _PTM2Postprocessor_h_
#define _PTM2Postprocessor_h_

#include "PTM2PostprocessorParameters.h"
#include "Grid.h"
#include <string>
#include <vector>
//#include <map>

namespace PTM2
{

class PTM2Postprocessor
{
private:
  Grid grid_;
  PTM2PostprocessorParameters parameters_;
  //std::vector<std::map<int, int> > fluxes_;
  std::vector<std::vector<float> > processedFluxes_;
  std::vector<std::vector<float> > groupCounts_;

  // cache
  std::vector<int> pIds_;
  std::vector<int> timestamp_;
  std::vector<int> fromWbT_;
  std::vector<int> fromWb_;
  std::vector<int> toWbT_;
  std::vector<int> toWb_;

public:
  Grid& getGrid() { return grid_; }
  const Grid& getGrid() const { return grid_; }
  PTM2PostprocessorParameters& getParameters() { return parameters_; }
  const PTM2PostprocessorParameters& getParameters() const { return parameters_; }
  void run();

private:
  void readTracesAndCalculate();
  void calculateFluxes();
  void processFluxes();
  void writeFluxes();
  void processGroups();
  void writeGroups();
  std::string convertIntervalToString(const int interval) const;
};

}

#endif
