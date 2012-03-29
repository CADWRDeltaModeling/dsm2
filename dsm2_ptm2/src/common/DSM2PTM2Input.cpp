/**
 *
 */
#include "DSM2PTM2Input.h"
#include "PTM2Time.h"

#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include "input_state_map.h"
#include "buffer_actions_c.h"
#include "boost/algorithm/string.hpp"

namespace PTM2
{

// @todo: This function is rather long.  Think about splitting.
void DSM2PTM2Input::readInputFile(const std::string& fname)
{
  prepareReading(fname);

  // Finally, passing the information read in.
  // Do not disturb the order of registration
  registerScalars();
  registerTidefiles();
  registerIOfiles();
  registerInsertions();

  readGridInformationFromTidefile();
  registerGrid();
}

void DSM2PTM2Input::registerScalars()
{
  DSM2Input::registerScalars();

  scalar_table& a_scalar_table = scalar_table::instance();
  const unsigned int n_items = a_scalar_table.buffer().size();
  for (unsigned int i = 0; i < n_items; ++i) {
    const scalar& a_scalar = a_scalar_table.buffer()[i];
    std::string name(a_scalar.name);
    boost::to_lower(name);
    std::string value(a_scalar.value);
    boost::to_lower(value);
    if (name.compare("hdf5_pos_output") == 0) {
      const int pos_output = boost::lexical_cast<int>(value);
      ptm2Parameters_.setH5positionOutputs(pos_output);
    }
    else if (name.compare("ptm_trans_constant") == 0) {
      const Real val = boost::lexical_cast<Real>(value);
      ptm2Parameters_.setTransverseDispersionCoeff(val);
    }
  }
}

void DSM2PTM2Input::registerInsertions()
{
  // Process particle insertion output
  particle_insertion_table& the_table = particle_insertion_table::instance();
  const unsigned int nitems = the_table.buffer().size();
  for (unsigned int i = 0; i < nitems; ++i) {
    // query buffer
    particle_insertion obj = particle_insertion_table::instance().buffer()[i];
    const Real delay = convertToSec(obj.delay);
    const Real duration = convertToSec(obj.duration);
    model_.addInsertion(obj.node, obj.nparts, delay, duration);
  }
}

void DSM2PTM2Input::registerTidefiles()
{
  tidefile_table& a_tidefile_table = tidefile_table::instance();
  const unsigned int n_items = a_tidefile_table.buffer().size();
  for (unsigned int i = 0; i < n_items; ++i) {
    std::string startDate(a_tidefile_table.buffer()[i].start_date);
    boost::trim(startDate);

    int julminUsageStart;
    PTM2Time& ptm2time = *PTM2Time::instance();
    if (startDate.compare("runtime") != 0) {
      PTM2Time::Date dateStart;
      ptm2time.parseUndelimitedUSdate(dateStart, startDate);
      ptm2time.datemin2julmin(julminUsageStart, dateStart);
    }
    else {
      julminUsageStart = model_.getParameters().getTimeStart();
    }

    std::string endDate(a_tidefile_table.buffer()[i].end_date);
    boost::trim(endDate);
    int julminUsageEnd;
    if (endDate.compare("length") != 0) {
      PTM2Time::Date dateEnd;
      ptm2time.parseUndelimitedUSdate(dateEnd, endDate);
      ptm2time.datemin2julmin(julminUsageStart, dateEnd);
    }
    else {
      julminUsageEnd = model_.getParameters().getTimeEnd();
    }

    std::string tidefileName(a_tidefile_table.buffer()[i].file);
    boost::trim(tidefileName);
    model_.addTideFile(tidefileName, julminUsageStart, julminUsageEnd);
  }
}

}
