/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "Grid.h"
#include "boost/lambda/lambda.hpp"
#include "boost/lambda/bind.hpp"
#include "boost/foreach.hpp"
#include <iostream>
#include <algorithm>

namespace PTM2
{

Grid::Grid()
: nReservoirConnects_(0)
{

}


Grid::~Grid()
{
  clearMemory();
}

void Grid::clearMemory()
{
  // clear nodes
  for (std::vector<Node*>::iterator it = nodes_.begin();
    it != nodes_.end();
    ++it) {
      if (*it != NULL)
        delete *it;
  }
  // clear channels
  for (std::vector<Channel*>::iterator it = channels_.begin();
    it != channels_.end();
    ++it) {
      if (*it != NULL)
        delete *it;
  }
  // clear reservoirs
  for (std::vector<Reservoir*>::iterator it = reservoirs_.begin();
    it != reservoirs_.end();
    ++it) {
      if (*it != NULL)
        delete *it;
  }
  // Source flow
  for (std::vector<SourceFlow*>::iterator it = sourceFlows_.begin();
    it != sourceFlows_.end();
    ++it) {
      if (*it != NULL)
        delete *it;
  }
  // Stage
  for (std::vector<Stage*>::iterator it = stages_.begin();
    it != stages_.end();
    ++it) {
      if (*it != NULL)
        delete *it;
  }
  //// Reservoir Connects
  //for (std::vector<ReservoirConnect*>::iterator it = reservoirConnects_.begin();
  //  it != reservoirConnects_.end();
  //  ++it) {
  //    if (*it != NULL)
  //      delete *it;
  //}
}

void Grid::addChannel(const std::string& externalId,
                      const Real& length,
                      const Real& manning,
                      const Real& dispersionCoeff,
                      const std::string& upnode,
                      const std::string& downnode)
{
  // Upnode
  Node* pUpNode = findNodeByName(upnode);
  if (pUpNode == NULL) {
    pUpNode = addNode(upnode);
  }
  // Downnode
  Node* pDownNode = findNodeByName(downnode);
  if (pDownNode == NULL) {
    pDownNode = addNode(downnode);
  }
  const int id = channels_.size();
  Channel* ch = new Channel(id, externalId, length, manning, dispersionCoeff,
                                  pUpNode, pDownNode);
  channels_.push_back(ch);
  pUpNode->addConnectedWaterbody(ch, 0);
  pDownNode->addConnectedWaterbody(ch, 1);
}

void Grid::addReservoir(const std::string& externalId,
                        const Real& area, const Real& bottomElevation)
{
  const int id = reservoirs_.size();
  Reservoir* res = new Reservoir(id, externalId, area, bottomElevation);
  reservoirs_.push_back(res);
}

void Grid::addReservoirConnect(const std::string& reservoirName, const std::string& nodeName)
{
  Reservoir* reservoir = findReservoirByName(reservoirName);
  assert(reservoir != NULL);
  Node* node = findNodeByName(nodeName);
  assert(node != NULL);
  int nReservoirConnects = reservoir->getNumberOfReservoirConnects();
  // The internal id of the node will be index of the reservoir flows.
  reservoir->addConnectedWaterbody(node, nReservoirConnects_);
  node->addConnectedWaterbody(reservoir, nReservoirConnects);
  ++nReservoirConnects_;
}

void Grid::addBoundaryStage(const std::string& externalId, const std::string& nodeName)
{
  const int id = stages_.size();
  Stage* stage = new Stage(id, externalId);
  Node* node = findNodeByName(nodeName);
  stage->addConnectedWaterbody(node, 0);
  stages_.push_back(stage);
  node->addConnectedWaterbody(stage, 0);
}

void Grid::addChannelSourceFlow(const std::string& externalId, const std::string& nodeName)
{
  const int id = sourceFlows_.size();
  SourceFlow* sourceFlow = new SourceFlow(id, externalId);
  Node* node = findNodeByName(nodeName);
  assert(node != NULL);
  sourceFlow->addConnectedWaterbody(node, 0);
  sourceFlows_.push_back(sourceFlow);
  node->addConnectedWaterbody(sourceFlow, 0);
}

void Grid::addReservoirSourceFlow(const std::string& externalId, const std::string& reservoirName)
{
  const int id = sourceFlows_.size();
  SourceFlow* sourceFlow = new SourceFlow(id, externalId);
  Reservoir* reservoir = findReservoirByName(reservoirName);
  assert(reservoir != NULL);
  const int nConnected = reservoir->getNumberOfConnectedWaterbodies();
  sourceFlow->addConnectedWaterbody(reservoir, nConnected);
  sourceFlows_.push_back(sourceFlow);
  reservoir->addConnectedWaterbody(sourceFlow, 0);
}

Node* Grid::findNodeByName(const std::string& externalId)
{
  std::vector<Node*>::iterator it
    = find_if(nodes_.begin(), nodes_.end(),
    boost::lambda::bind(&Grid::compareNodeId, &*this, boost::lambda::_1, externalId));
  if (it == nodes_.end())
    return NULL;
  else
    return dynamic_cast<Node*>(*it);
}

Reservoir* Grid::findReservoirByName(const std::string& externalId)
{
  BOOST_FOREACH(Reservoir* res, reservoirs_) {
    if (externalId.compare(res->getExternalId()) == 0) {
      return res;
    }
  }
  return NULL;
}

Channel* Grid::findChannelByName(const std::string& externalId)
{
  BOOST_FOREACH(Channel* ch, channels_) {
    if (externalId.compare(ch->getExternalId()) == 0) {
      return ch;
    }
  }
  return NULL;
}

SourceFlow* Grid::findSourceFlowByName(const std::string& externalId)
{
  BOOST_FOREACH(SourceFlow* src, sourceFlows_) {
    if (externalId.compare(src->getExternalId()) == 0) {
      return src;
    }
  }
  return NULL;
}

Stage* Grid::findStageByName(const std::string& externalId)
{
  BOOST_FOREACH(Stage* stage, stages_) {
    if (externalId.compare(stage->getExternalId()) == 0) {
      return stage;
    }
  }
  return NULL;
}

bool Grid::compareNodeId(const Node* node, const std::string& externalId)
{
  return externalId.compare(node->getExternalId()) == 0 ? true : false;
}

Node* Grid::addNode(const std::string& externalId)
{
  const int id = nodes_.size();
  Node* node = new Node(id, externalId);
  nodes_.push_back(node);
  return node;
}

}
