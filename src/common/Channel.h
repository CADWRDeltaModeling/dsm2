/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Channel_h_
#define _Channel_h_

//#include "PTM2.h"
#include "Waterbody.h"
#include "Node.h"

namespace PTM2
{

/**
 * @brief channel waterbody
 **/
class Channel : public Waterbody
{
protected:
  enum NeighborIndex {UP, DOWN};
  Real length_;
  Real manning_;
  Real dispersionCoeff_;

public:
  ~Channel() { Waterbody::~Waterbody(); }
  Channel(const int& id,
          const std::string& externalId,
          const Real& length,
          const Real& manning,
          const Real& dispersionCoeff,
          const Node* upNode,
          const Node* downNode)
    : Waterbody(id, externalId),
      length_(length), manning_(manning),
      dispersionCoeff_(dispersionCoeff)
  {
    // Keep the order of adding to access them with indices directly later
    addConnectedWaterbody(upNode, 0);
    addConnectedWaterbody(downNode, 0);
  }
  const Real& getLength() const { return length_; }
  int getType() const { return CHANNEL; }
};

}

#endif
