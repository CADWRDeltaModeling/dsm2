#include "PTM2PostprocessorInput.h"
#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include "input_state_map.h"
#include "buffer_actions_c.h"
#include "boost/lexical_cast.hpp"
#include "boost/algorithm/string.hpp"
#include "boost/regex.hpp"
#include "boost/format.hpp"

namespace PTM2
{

void PTM2PostprocessorInput::readInputFile(const std::string& fname)
{
  prepareReading(fname);

  registerEnvvars();
  registerScalars();
  registerIOfiles();
  readGridInformationFromTidefile();
  registerGrid();

  registerPTMScalars();
  registerGroup();
  registerGroupMembers();
  registerFluxOutput();
  registerGroupOutput();
  registerInsertions();
}

void PTM2PostprocessorInput::registerPTMScalars()
{
  scalar_table& a_scalar_table = scalar_table::instance();
  const unsigned int n_items = a_scalar_table.buffer().size();

  bool accumulated = false;
  bool fluxPercent = false;
  bool groupPercent = false;
  for (unsigned int i = 0; i < n_items; ++i) {
    const scalar& a_scalar = a_scalar_table.buffer()[i];
    const char* name = a_scalar.name;
    std::string value(a_scalar.value);
    boost::to_lower(value);
    if (!strcmp(name, "ptm_flux_cumulative")) {
      if (value.compare("t") == 0) {
        accumulated = true;
      }
    }
    else if (!strcmp(name, "ptm_flux_percent")) {
      if (value.compare("t") == 0) {
        fluxPercent = true;
      }
    }
    else if (!strcmp(name, "ptm_group_percent")) {
      if (value.compare("t") == 0) {
        groupPercent = true;
      }
    }
  }
  postprocessorParameters_.setAccumulated(accumulated);
  postprocessorParameters_.setFluxPercentaged(fluxPercent);
  postprocessorParameters_.setGroupPercentaged(groupPercent);
}

void PTM2PostprocessorInput::registerGroup()
{
  group_table& the_table = group_table::instance();
  const unsigned int nitems = the_table.buffer().size();
  for (unsigned int i = 0; i < nitems; ++i) {
    group obj = group_table::instance().buffer()[i];
    std::string name(obj.name);
    boost::to_lower(name);
    //const std::list<PTM2Group>& groups = postprocessorParameters_.getGroups();
    //const std::list<PTM2Group>::const_iterator found = find(groups.begin(), groups.end(), name);
    postprocessorParameters_.addGroup(PTM2Group(name));
  }
}

void PTM2PostprocessorInput::registerGroupMembers()
{
  const Grid& grid = postprocessor_.getGrid();
  group_member_table& the_table = group_member_table::instance();
  const unsigned int nitems = the_table.buffer().size();
  for (unsigned int i = 0; i < nitems; ++i) {
    group_member obj = group_member_table::instance().buffer()[i];
    const std::string group_name = obj.group_name;
    std::string member_type = obj.member_type;
    boost::trim(member_type);
    boost::to_lower(member_type);
    std::string pattern = obj.pattern;
    boost::to_lower(pattern);
    if (pattern.compare("all") == 0) {
      throw std::runtime_error("Cannot youn group name all.  It is reserved.");
    }
    PTM2Group* group = postprocessorParameters_.findGroup(group_name);
    if (group == NULL) {
      std::cerr << "No group named " << group_name << " found...\n";
      throw std::runtime_error("Missing group definition");
    }assert(group != NULL);
    Waterbody* wb;
    boost::regex e(pattern);
    if (member_type.compare("channel") == 0) {
      std::vector<Channel*> channels = grid.getChannels();
      for (int i = 0; i < channels.size(); ++i) {
        const Channel* channel = channels[i];
        if (boost::regex_match(channel->getExternalId(), e)) {
          group->addWaterbody(channel);
        }
      }
    }
    else if (member_type.compare("reservoir") == 0) {
      std::vector<Reservoir*> reservoirs = grid.getReservoirs();
      for (int i = 0; i < reservoirs.size(); ++i) {
        const Reservoir* reservoir = reservoirs[i];
        if (boost::regex_match(reservoir->getExternalId(), e)) {
          group->addWaterbody(reservoir);
        }
      }
    }
    else if (member_type.compare("qext") == 0) {
      std::vector<SourceFlow*> sourceFlows = grid.getSourceFlows();
      for (int i = 0; i < sourceFlows.size(); ++i) {
        const SourceFlow* sourceFlow = sourceFlows[i];
        if (boost::regex_match(sourceFlow->getExternalId(), e)) {
          group->addWaterbody(sourceFlow);
        }
      }
    }
    else if (member_type.compare("stage") == 0) {
      std::vector<Stage*> stages = grid.getStages();
      for (int i = 0; i < stages.size(); ++i) {
        const Stage* stage = stages[i];
        if (boost::regex_match(stage->getExternalId(), e)) {
          group->addWaterbody(stage);
        }
      }
    }
    else {
      std::cerr << "Unsupported waterbody type in flux: " << member_type
          << "\n";
      throw std::runtime_error("Unsupported waterbody type");
    }
  }
}

void PTM2PostprocessorInput::registerFluxOutput()
{
  // Process particle flux output
  particle_flux_output_table& the_table =
      particle_flux_output_table::instance();
  const unsigned int nitems = the_table.buffer().size();
  for (unsigned int i = 0; i < nitems; ++i) {
    // query buffer
    particle_flux_output obj =
        particle_flux_output_table::instance().buffer()[i];
    const std::string name = obj.name;
    const std::string from_wb = obj.from_wb;
    const std::string to_wb = obj.to_wb;
    const std::string interval = obj.interval;
    const std::string fname = obj.file;

    const int intervalMin = this->convertToMin(interval);

    // Need to parse out what waterbodies are involved here
    std::vector<int> fromWaterbodyTypes;
    std::vector<int> fromWaterbodies;
    processFluxOutput(from_wb, fromWaterbodyTypes, fromWaterbodies);

    std::vector<int> toWaterbodyTypes;
    std::vector<int> toWaterbodies;
    processFluxOutput(to_wb, toWaterbodyTypes, toWaterbodies);

    postprocessorParameters_.addFluxOutput(
        PTM2FluxOutput(name, fromWaterbodyTypes, fromWaterbodies,
                       toWaterbodyTypes, toWaterbodies, intervalMin, fname));
  }
}

void PTM2PostprocessorInput::registerGroupOutput()
{
  // Process particle flux output
  particle_group_output_table& the_table =
      particle_group_output_table::instance();
  const unsigned int nitems = the_table.buffer().size();
  for (unsigned int i = 0; i < nitems; ++i) {
    // query buffer
    particle_group_output obj =
        particle_group_output_table::instance().buffer()[i];
    std::string name(obj.name);
    boost::to_lower(name);
    std::string group_name(obj.group_name);
    boost::to_lower(group_name);
    std::string mod_group_name(
        boost::str(boost::format("group:%s") % group_name));
    const std::string interval = obj.interval;
    std::string fname = obj.file;

    const int intervalMin = this->convertToMin(interval);

    // Need to parse out what waterbodies are involved here
    std::vector<int> waterbodyTypes;
    std::vector<int> waterbodies;
    processFluxOutput(mod_group_name, waterbodyTypes, waterbodies);

    postprocessorParameters_.addGroupOutput(
        PTM2GroupOutput(name, waterbodyTypes, waterbodies, intervalMin, fname));
  }
}

void PTM2PostprocessorInput::processFluxOutput(const std::string& wb,
                                               std::vector<int>& waterbodyTypes,
                                               std::vector<int>& waterbodies)
{
  std::string objType(this->getObjType(wb));
  std::string objName(this->getObjName(wb));
  boost::to_lower(objType);
  boost::to_lower(objName);

  if (objType.compare("group") == 0) {
    if (objName.compare("all") == 0) {
      waterbodyTypes.push_back(Waterbody::GROUPALL);
      waterbodies.push_back(-1);
    }
    else {
      PTM2Group* groupPtr = postprocessorParameters_.findGroup(objName);
      if (groupPtr == NULL) {
        throw std::runtime_error(
            "PTM2PostprocessorInput::processFluxOutput: Cannot find the given group name.");
      }
      for (int i = 0; i < groupPtr->nWaterbodies(); ++i) {
        const Waterbody* wbPtr = groupPtr->getWaterbody(i);
        waterbodyTypes.push_back(wbPtr->getType());
        waterbodies.push_back(wbPtr->getId());
      }
    }
  }
  else {
    if (objType.compare("chan") == 0) {
      waterbodyTypes.push_back(Waterbody::CHANNEL);
      const Channel* chanPtr = grid_.findChannelByName(objName);
      if (chanPtr == NULL) {
        throw std::runtime_error(
            "PTM2PostprocessorInput::processFluxOutput: Cannot find the given channel name.");
      }
      const int chanId = chanPtr->getId();
      waterbodies.push_back(chanId);
    }
    else if (objType.compare("res") == 0) {
      waterbodyTypes.push_back(Waterbody::RESERVOIR);
      const Reservoir* resPtr = grid_.findReservoirByName(objName);
      if (resPtr == NULL) {
        throw std::runtime_error(
            "PTM2PostprocessorInput::processFluxOutput: Cannot find the given reservoir name.");
      }
      const int resId = resPtr->getId();
      waterbodies.push_back(resId);
    }
    else if (objType.compare("qext") == 0) {
      waterbodyTypes.push_back(Waterbody::SOURCEFLOW);
      const SourceFlow* wbPtr = grid_.findSourceFlowByName(objName);
      if (wbPtr == NULL) {
        throw std::runtime_error(
            "PTM2PostprocessorInput::processFluxOutput: Cannot find the given qext name.");
      }
      const int id = wbPtr->getId();
      waterbodies.push_back(id);
    }
    else if (objType.compare("stage") == 0) {
      waterbodyTypes.push_back(Waterbody::STAGE);
      const Stage* wbPtr = grid_.findStageByName(objName);
      if (wbPtr == NULL) {
        throw std::runtime_error(
            "PTM2PostprocessorInput::processFluxOutput: Cannot find the given stgaes name.");
      }
      const int id = wbPtr->getId();
      waterbodies.push_back(id);
    }
    else {
      throw std::runtime_error(
          "PTM2PostprocessorInput::processFluxOutput: unsupported object type.");
    }
  }
}

const std::string PTM2PostprocessorInput::getObjType(const std::string& objname)
{
  size_t found = objname.find(":");
  if (found == std::string::npos) {
    throw std::runtime_error(
        "PTM2PostprocessorInput::getObjType: No waterbody type is specified");
  }
  std::string objType = objname.substr(0, found);
  boost::to_lower(objType);
  return objType;
}

const std::string PTM2PostprocessorInput::getObjName(const std::string& objname)
{
  size_t found = objname.find(":");
  if (found == std::string::npos) {
    throw std::runtime_error(
        "PTM2PostprocessorInput::getObjType: No waterbody type is specified");
  }
  std::string objName = objname.substr(found + 1, objname.size());
  return objName;
}

int PTM2PostprocessorInput::convertToMin(const std::string& interval) const
{
  const char* numeric = "0123456789";
  std::string str(interval);
  boost::trim(str);
  boost::to_lower(str);
  const size_t pos = str.find_last_of(numeric);
  const std::string num(str.substr(0, pos + 1));
  const std::string unit(str.substr(pos + 1));
  if (unit.substr(0, 3).compare("min") == 0) {
    return boost::lexical_cast<int>(num);
  }
  else if (unit.substr(0, 4).compare("hour") == 0) {
    return boost::lexical_cast<int>(num) * 60;
  }
  else if (unit.substr(0, 3).compare("day") == 0) {
    return boost::lexical_cast<int>(num) * 60 * 24;
  }
  return 0;
}

void PTM2PostprocessorInput::registerInsertions()
{
  int nParticles = 0;
  // Process particle insertion output
  particle_insertion_table& the_table = particle_insertion_table::instance();
  const unsigned int nitems = the_table.buffer().size();
  for (unsigned int i = 0; i < nitems; ++i) {
    // query buffer
    particle_insertion obj = particle_insertion_table::instance().buffer()[i];
    nParticles += obj.nparts;
  }
  postprocessorParameters_.setTotalNParticles(nParticles);
}

//void getWaterbodyUniqueId()
//{
//
//}

}
