/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _Grid_h_
#define _Grid_h_

#include <vector>
#include <string>
#include "PTM2.h"
#include "Waterbody.h"
#include "Channel.h"
#include "Reservoir.h"
#include "Node.h"
#include "SourceFlow.h"
#include "Transfer.h"
#include "reservoirConnect.h"
#include "Stage.h"
#include "typeinfo.h"

namespace PTM2
{

/**
 * @brief DSM2 Grid class
 */
class Grid
{
  // Properties
protected:
  std::vector<Node*> nodes_;
  std::vector<Channel*> channels_;
  std::vector<Reservoir*> reservoirs_;
  std::vector<SourceFlow*> sourceFlows_;
  std::vector<Transfer*> transfers_; 
  std::vector<Stage*> stages_;
  //std::vector<ReservoirConnect*> reservoirConnects_;
  int nReservoirConnects_;

  // Methods
public:
  /**
   * @brief Constructor
   */
  Grid();

  /**
   * @brief Destructor
   */
  ~Grid();

  ///**
  // * @brief Get a pointer of a waterbody by a direct index
  // */
  //Waterbody* getWaterbody(const int& id)
  //{
  //  return waterbodies_[id];
  //}

  ///**
  //  * @brief Get a constant reference of a waterbody by a direct index
  //  */
  //const Waterbody* getWaterbody(const int id) const
  //{
  //  return waterbodies_[id];
  //}

  //const int getNumberOfWaterbodies() const { return waterbodies_.size(); }
  const int getNumberOfChannels() const { return channels_.size(); }
  const int getNumberOfReservoirs() const { return reservoirs_.size(); }
  const int getNumberOfReservoirConnects() const { return nReservoirConnects_; };
  const int getNumberOfSourceFlows() const { return sourceFlows_.size(); }
  const int getNumberOfTransfers() const { return transfers_.size(); }
  const int getNumberOfStages() const { return stages_.size(); }
  const std::vector<Channel*>& getChannels() const { return channels_; }
  const std::vector<Reservoir*> getReservoirs() const { return reservoirs_; }
  const std::vector<SourceFlow*> getSourceFlows() const { return sourceFlows_; }
  const std::vector<Stage*> getStages() const { return stages_; }

  void addChannel(const std::string& externalId,
    const Real& length,
    const Real& manning,
    const Real& dispersionCoeff,
    const std::string& upnode,
    const std::string& downnode);
  void addReservoir(const std::string& externalId,
                    const Real& area, const Real& bottomElevation);
  void addReservoirConnect(const std::string& reservoirName, const std::string& nodeName);
  void addBoundaryStage(const std::string& externalId, const std::string& nodeName);
  void addChannelSourceFlow(const std::string& externalId, const std::string& nodeName);
  void addReservoirSourceFlow(const std::string& externalId, const std::string& reservoirName);
  Node* findNodeByName(const std::string& externalId);
  Channel* findChannelByName(const std::string& externalId);
  Reservoir* findReservoirByName(const std::string& externalId);
  SourceFlow* findSourceFlowByName(const std::string& externalId);
  Stage* findStageByName(const std::string& externalId);

private:
  void clearMemory();
  Node* addNode(const std::string& externalId);
  bool compareNodeId(const Node* node, const std::string& externalId);
};

}

#endif // __Grid___
