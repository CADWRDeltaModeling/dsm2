/**
*  Author: Kijin Nam, knam@water.ca.gov
*/

#ifndef _Waterbody_h_
#define _Waterbody_h_

#include <vector>
#include <cassert>
#include <string>

namespace PTM2
{

/**
 * Base class of Waterbody
 */
class Waterbody
{
  // Properties
public:
  enum WaterbodyType {NODE, CHANNEL, RESERVOIR, SOURCEFLOW,
    TRANSFER, RESERVOIRCONNECT, STAGE, GROUP, GROUPALL};

protected:
  int id_;
  std::string externalId_;
  std::vector<const Waterbody*> connectedWaterbodies_;
  std::vector<int> internalIdInConnectedWaterbody_;

  // Methods
public:
  /**
  * @brief Constructor
  */
  Waterbody(const int id, const std::string& externalId = "")
    : id_(id), externalId_(externalId)
  {
  }
  /**
  * @brief Destructor
  */
  virtual ~Waterbody();

  const int& getId() const { return id_; }
  const std::string& getExternalId() const { return externalId_; }
  int getNumberOfConnectedWaterbodies() const
  { 
    return connectedWaterbodies_.size();
  }
  void resizeConnectedWaterbodies(const int& n)
  { 
    assert(n >= 0);
    return connectedWaterbodies_.resize(n);
  }

  //Waterbody* getConnectedWaterbody(const int& i)
  //{ 
  //  assert(i >=0);
  //  assert((unsigned int)(i) < connectedWaterbodies_.size());
  //  return connectedWaterbodies_[i];
  //}

  const Waterbody* getConnectedWaterbody(const int& i) const
  { 
    assert(i >=0);
    assert((unsigned int)(i) < connectedWaterbodies_.size());
    return connectedWaterbodies_[i];
  }
  const int getInternalIdOfConnectedWaterbody(const int& i) const
  { 
    assert(i >=0);
    assert((unsigned int)(i) < internalIdInConnectedWaterbody_.size());
    return internalIdInConnectedWaterbody_[i];
  }

  void addConnectedWaterbody(const Waterbody* waterbody, const int& internalId)
  {
    connectedWaterbodies_.push_back(waterbody);
    internalIdInConnectedWaterbody_.push_back(internalId);
  }

  virtual int getType() const = 0;
};

}

#endif // __Waterbody_h__
