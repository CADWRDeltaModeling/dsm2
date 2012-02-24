/**
 * Author: Kijin Nam, knam@wate.ca.gov
 */

#ifndef _DSM2Input_h_
#define _DSM2Input_h_

#include "PTM2.h"
#include "Grid.h"
#include "Parameters.h"

namespace PTM2
{

/**
 * @brief DSM2 Input class
 */
class DSM2Input
{
protected:
  Grid& grid_;
  Parameters& parameters_;

public:
  /**
   *  @brief Constructor
   */
  DSM2Input(Grid& grid, Parameters& parameters)
    : grid_(grid), parameters_(parameters) {}
//
protected:
  bool prepareReading(const std::string& fname);
  bool checkIfFileExist(const std::string& fname);
  void registerEnvvars();
  void registerScalars();
  void readGridInformationFromTidefile();
  void registerGrid();
  void registerIOfiles();
  void registerChannels();
  void registerReservoirs();
  void registerReservoirNodes();
  void registerSourceFlows();
  void registerBoundaryStages();
  Real convertToSec(const char* pStr);
  void collectReservoirFlows(const int& reservoirId,
    std::vector<int>& connections);
  int findNodeFlowConnections(const int& flowId);
};

} // namespace PTM2

#endif // __DSM2PTMInput_h__
