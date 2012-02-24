/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Node_h_
#define _Node_h_

#include "Waterbody.h"

namespace PTM2
{

/**
 * @brief Node waterbody
 **/
class Node : public Waterbody
{
protected:
  enum NodeType {NORMAL, BOUNDARY_STAGE};
  int nodeType_;

public:
  Node(const int& id, const std::string& externalId, const NodeType& nodeType = NORMAL)
    : Waterbody(id, externalId), nodeType_(nodeType) {}
  void makeBoundayStage() { nodeType_ = BOUNDARY_STAGE; }
  bool isBoundaryStage() const { return nodeType_ == BOUNDARY_STAGE; }
  int getType() const { return NODE; }
};

}

#endif
