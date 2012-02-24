/**
 * Author: Kijin Nam, knam@water.ca.gov
 */
#include "DSM2Input.h"
#include "Parameters.h"
#include "PTM2Time.h"

#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include "input_state_map.h"
#include "buffer_actions_c.h"

#include "boost/filesystem.hpp"
#include "boost/algorithm/string.hpp"
#include "boost/limits.hpp"

namespace PTM2
{

bool DSM2Input::prepareReading(const std::string& fname)
{
  if (!checkIfFileExist(fname)) {
    throw std::runtime_error("The given input file cannot found.");
  }

  // initialize file reader
  ApplicationTextReader& reader = ApplicationTextReader::instance();
  reader.setInputStateMap(input_state_map());

  // Get everything in the "ptm" profile and set these keywords to be
  // allowed
  std::string name("PTM");
  reader.setInitialContextItems(profile(name));

  // first pass
  // Turn off the text substitution
  reader.getTextSubstitution().setEnabled(false);
  //reader.getTextSubstitution().setNotFoundIsError(false);
  name = "envvar";
  reader.setActiveItems(profile(name));

  // read the text into a buffer.
  assert(reader.getInitialContextItems().size() != 0);
  assert(reader.getActiveItems().size() != 0);
  std::string layername = LayerManager::instance().generateLayerName(fname);
  LayerManager::instance().addLayer(layername);
  reader.processInput(fname);

  // process text substitution
  EnvSubstitution sub;
  vector<envvar> &envvars = HDFTableManager < envvar > ::instance().buffer();
  for (size_t i = 0; i < envvars.size(); i++) {
    sub.add(envvars[i].name, envvars[i].value);
  }
  reader.setTextSubstitution(sub);

  // Set it the substitution flag back.
  reader.getTextSubstitution().setEnabled(true);
  //reader.getTextSubstitution().setEnabledEnvironment(true);

  // Clear the buffer.
  clear_all_buffers_c();

  // Set a new active profile.
  name = "PTM";
  reader.setActiveItems(profile(name));
  // read the text into a buffer.
  assert(reader.getInitialContextItems().size() != 0);
  assert(reader.getActiveItems().size() != 0);
  LayerManager::instance().addLayer(layername);
  reader.processInput(fname);

  prioritize_all_buffers_c();
  return true;
}

void DSM2Input::registerEnvvars()
{
  envvar_table& a_envvar_table = envvar_table::instance();
  const unsigned int n_items = a_envvar_table.buffer().size();
  for (unsigned int i = 0; i < n_items; ++i) {
    const envvar& a_envvar = a_envvar_table.buffer()[i];
    std::string name(a_envvar.name);
    boost::to_lower(name);
    std::string value(a_envvar.value);
    boost::to_lower(value);
    if (name.compare("dsm2modifier") == 0) {
      parameters_.setModifier(value);
    }
  }
}

void DSM2Input::readGridInformationFromTidefile()
{
  // Initialize the hdf5
  // NOTE: This assumes that the grid did not change over multiple files
  std::string tidefile_name(tidefile_table::instance().buffer()[0].file);
  boost::trim(tidefile_name);
  hid_t fapl_id;
  fapl_id = H5Fopen(tidefile_name.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
  if (fapl_id < 0)
    throw std::runtime_error("Cannot open the HDF5 file:");

  std::string gname("hydro");
  hid_t gapl_id;
  gapl_id = H5Gopen(fapl_id, gname.c_str(), H5P_DEFAULT);
  if (gapl_id < 0)
    throw std::runtime_error("Cannot open a group hydro.");
  gname = "input";
  hid_t sgapl_id;
  sgapl_id = H5Gopen(gapl_id, gname.c_str(), H5P_DEFAULT);
  if (sgapl_id < 0)
    throw std::runtime_error("Cannot open a group input.");

  int err;
  // Read buffer profile (meaning reading all the grid in the file)
  err = read_buffer_profile_from_hdf5_c("Grid", sgapl_id);
  if (err != 0)
    throw std::runtime_error(
        "DSM2Input::readGridInformationFromTidefile:: Cannot read a grid from a HDF5");

  H5Gclose(sgapl_id);

  gname = "geometry";
  sgapl_id = H5Gopen(gapl_id, gname.c_str(), H5P_DEFAULT);
  if (sgapl_id < 0)
    throw std::runtime_error("Cannot open a group input.");
  err = read_buffer_profile_from_hdf5_c("Geometry", sgapl_id);
  if (err != 0)
    throw std::runtime_error(
        "DSM2Input::readGridInformationFromTidefile:: Cannot read a geometry from a HDF5");
  H5Gclose(sgapl_id);

  H5Gclose(gapl_id);
  H5Fclose(fapl_id);
  H5close();
}

bool DSM2Input::checkIfFileExist(const std::string& fname)
{
  boost::filesystem::path fpath(fname);
  return boost::filesystem::exists(fpath);
}

void DSM2Input::registerIOfiles()
{
  io_file_table& a_io_file_table = io_file_table::instance();
  unsigned int n_items = a_io_file_table.buffer().size();
  for (unsigned int i = 0; i < n_items; ++i) {
    io_file& a_io_file = a_io_file_table.buffer()[i];
    const std::string a_file_type = a_io_file.type;
    if (a_file_type.compare("hdf5") == 0) {
      const std::string a_filename = a_io_file.file;
      parameters_.setHdf5OutputFilename(a_filename);
    }
  }
}

void DSM2Input::registerScalars()
{
  PTM2Time& ptm2time = *PTM2Time::instance();

  // Some variables
  PTM2Time::Date dateStart;
  PTM2Time::Date dateEnd;
  int minStart;
  int minEnd;
  int dt;

  scalar_table& a_scalar_table = scalar_table::instance();
  const unsigned int n_items = a_scalar_table.buffer().size();
  for (unsigned int i = 0; i < n_items; ++i) {
    const scalar& a_scalar = a_scalar_table.buffer()[i];
    std::string name(a_scalar.name);
    boost::to_lower(name);
    std::string value(a_scalar.value);
    boost::to_lower(value);
    if (name.compare("run_start_date") == 0) {
      ptm2time.parseUndelimitedUSdate(dateStart, value);
    }
    if (name.compare("run_start_time") == 0) {
      ptm2time.parseUndelimited24Hours(minStart, value);
    }
    if (name.compare("run_end_date") == 0) {
      ptm2time.parseUndelimitedUSdate(dateEnd, value);
    }
    if (name.compare("run_end_time") == 0) {
      ptm2time.parseUndelimited24Hours(minEnd, value);
    }
    if (name.compare("ptm_time_step") == 0) {
      ptm2time.interval2minutes(dt, value);
    }
    if (name.compare("hdf5_trace") == 0) {
      int flag = value.compare("T") == 0 ? 1 : 0;
      parameters_.setH5Trace(flag);
    }
    if (name.compare("hdf5_detail") == 0) {
      int flag = value.compare("T") == 0 ? 1 : 0;
      parameters_.setH5Detail(flag);
    }
  }
  int julmin;
  ptm2time.datemin2julmin(julmin, dateStart, minStart);
  parameters_.setTimeStart(julmin);
  ptm2time.datemin2julmin(julmin, dateEnd, minEnd);
  parameters_.setTimeEnd(julmin);
  parameters_.setDt(dt);
}

void DSM2Input::registerGrid()
{
  // The sequence of registration of waterbodies is important.
  // Do not change the sequence.
  registerChannels();
  registerReservoirs();
  // Register nodes connected to reservoirs first.
  registerReservoirNodes();
  registerSourceFlows();
  registerBoundaryStages();
}

void DSM2Input::registerChannels()
{
  channel_table& a_channel_table = channel_table::instance();
  unsigned int n_items = a_channel_table.buffer().size();
  for (unsigned int i = 0; i < n_items; ++i) {
    channel& ch = a_channel_table.buffer()[i];
    grid_.addChannel(boost::lexical_cast<std::string>(ch.chan_no),
                     Real(ch.length), ch.manning, ch.dispersion,
                     boost::lexical_cast<std::string>(ch.upnode),
                     boost::lexical_cast<std::string>(ch.downnode));
  }
}

void DSM2Input::registerReservoirs()
{
  reservoir_table& a_reservoir_table = reservoir_table::instance();
  const int nReservoirs = a_reservoir_table.buffer().size();
  for (int i = 0; i < nReservoirs; ++i) {
    reservoir& res = a_reservoir_table.buffer()[i];
    std::string reservoirName(res.name);
    boost::trim(reservoirName);
    grid_.addReservoir(reservoirName, res.area, res.bot_elev);
  }
}

void DSM2Input::registerReservoirNodes()
{
  reservoir_node_connect_table& a_reservoir_node_connect_table =
      reservoir_node_connect_table::instance();
  const int n_items = a_reservoir_node_connect_table.buffer().size();
  for (int i = 0; i < n_items; ++i) {
    reservoir_node_connect& a_reservoir_node_connect =
        a_reservoir_node_connect_table.buffer()[i];
    std::string reservoirName(a_reservoir_node_connect.res_name, 32);
    boost::trim(reservoirName);
    const std::string nodeId = boost::lexical_cast<std::string>(
        a_reservoir_node_connect.ext_node_no);
    grid_.addReservoirConnect(reservoirName, nodeId);
  }
}

void DSM2Input::registerBoundaryStages()
{
  stage_boundaries_table& a_stage_table = stage_boundaries_table::instance();
  const int n_items = a_stage_table.buffer().size();
  for (int i = 0; i < n_items; ++i) {
    stage_boundaries& stage = a_stage_table.buffer()[i];
    std::vector<int> connections;
    std::string stageName(stage.name, 32);
    boost::trim(stageName);
    std::string nodeName = boost::lexical_cast<std::string>(stage.ext_node_no);
    boost::trim(nodeName);
    grid_.addBoundaryStage(stageName, nodeName);
  }
}

void DSM2Input::registerSourceFlows()
{
  enum ObjType
  {
    CHANNEL = 2, RESERVOIR
  };

  qext_table& a_qext_table = qext_table::instance();
  const int n_items = a_qext_table.buffer().size();
  for (int i = 0; i < n_items; ++i) {
    qext& a_qext = a_qext_table.buffer()[i];
    std::string name(a_qext.name, 32);
    boost::trim(name);
    std::string attachedObjName(a_qext.attach_obj_name, 32);
    boost::trim(attachedObjName);
    const int& attachedObjNo = a_qext.attached_obj_no - 1; // Adjust to zero-based
    const int& attchedObjType = a_qext.attached_obj_type;
    switch (attchedObjType) {
    case CHANNEL: {
      grid_.addChannelSourceFlow(name, attachedObjName);
    }
      break;
    case RESERVOIR: {
      grid_.addReservoirSourceFlow(name, attachedObjName);
    }
      break;
    default:
      throw std::runtime_error("Not supported source flow type.");
    }
  }
}

int DSM2Input::findNodeFlowConnections(const int& flowId)
{
  node_flow_connections_table& a_node_flow_connections_table =
      node_flow_connections_table::instance();
  const int n_items = a_node_flow_connections_table.buffer().size();
  for (int i = 0; i < n_items; ++i) {
    node_flow_connections& a_node_flow_connections =
        a_node_flow_connections_table.buffer()[i];
    if (a_node_flow_connections.flow_index == (flowId + 1)) {
      return a_node_flow_connections.int_node_no - 1;
    }
  }
  throw std::runtime_error(
      "DSM2Input::findNodeFlowConnections: Flow is not found");
}

void DSM2Input::collectReservoirFlows(const int& flowId,
                                      std::vector<int>& connections)
{
  reservoir_flow_connections_table& a_reservoir_flow_connections_table =
      reservoir_flow_connections_table::instance();
  const int nItems = a_reservoir_flow_connections_table.buffer().size();
  for (int i = 0; i < nItems; ++i) {
    reservoir_flow_connections& a_reservoir_flow_connections =
        a_reservoir_flow_connections_table.buffer()[i];
    if (a_reservoir_flow_connections.flow_index == (flowId + 1)) {
      connections.push_back(a_reservoir_flow_connections.res_index - 1);
    }
  }
}

Real DSM2Input::convertToSec(const char* pStr)
{
  const char* numeric = "0123456789";
  std::string str(pStr);
  boost::trim(str);
  boost::to_lower(str);
  const size_t pos = str.find_last_of(numeric);
  const std::string num(str.substr(0, pos + 1));
  const std::string unit(str.substr(pos + 1));
  if (unit.substr(0, 3).compare("min") == 0) {
    return boost::lexical_cast<Real>(num) * 60.;
  }
  else if (unit.substr(0, 4).compare("hour") == 0) {
    return boost::lexical_cast<Real>(num) * 3600.;
  }
  else if (unit.substr(0, 3).compare("day") == 0) {
    return boost::lexical_cast<Real>(num) * 3600. * 24.;
  }

  return 0.;
}

} // namespace PTM2
