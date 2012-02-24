 /**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "H5TideReader.h"
#include "PTM2Time.h"
#include "boost/foreach.hpp"
#include "boost/lambda/lambda.hpp"
#include "boost/lambda/bind.hpp"
#include "boost/filesystem.hpp"
#include <sstream>

namespace PTM2
{

void H5TideReader::addTideFile(const std::string& filename,
                 const int& julminStart,
                 const int& julminEnd)
{
  tideFiles_.push_back(H5TideFile(filename, julminStart, julminEnd));
}

void H5TideReader::updateTideAtOrBefore(const int& julminToRead,
                                    Tide* tide)
{
  if (!tide->isAllocated()) {
    tide->allocateMemory();
  }
  const H5TideFile& tideFile = findFile(julminToRead);
  openFile(tideFile.getFilename());
  getH5TimeStart();
  getH5TimeInterval();
  getH5NumberOfRecords();

  long index;
  calculateH5Index(index, julminToRead);
  readTide(index, tide);
  const int julminCursor = julminStart_ + minInterval_ * index;
  tide->setTimeCursor(julminCursor);

  closeFile();
}

void H5TideReader::updateTideAfter(const int& julminToRead,
                                   Tide* tide)
{
  if (!tide->isAllocated()) {
    tide->allocateMemory();
  }
  const H5TideFile& tideFile = findFile(julminToRead);
  openFile(tideFile.getFilename());
  getH5TimeStart();
  getH5TimeInterval();
  getH5NumberOfRecords();

  long index;
  calculateH5Index(index, julminToRead);
  index++;  // This line is only difference from 'updateTideBefore.'
  readTide(index, tide);
  const int julminCursor = julminStart_ + minInterval_ * int(index);
  tide->setTimeCursor(julminCursor);

  closeFile();
}

void H5TideReader::readTide(const long& index, Tide* tide)
{
  readChannelData(index, tide);
  readReservoirData(index, tide);
  readSourceFlowData(index, tide);
  readReservoirFlowData(index, tide);
}

void H5TideReader::calculateH5Index(long& index, const int& julminToRead)
{
  const long relativeJulianMin = julminToRead - julminStart_;
  index = relativeJulianMin / minInterval_;
}

void H5TideReader::readChannelData(const long& index, Tide* tide)
{
  readChannelStageData(index, tide);
  readChannelFlowData(index, tide);
  readChannelAreaData(index, tide);
  readChannelAverageAreaData(index, tide);
}

void H5TideReader::readReservoirData(const long& index, Tide* tide)
{
  readReservoirHeightData(index, tide);
}

void H5TideReader::readReservoirHeightData(const long& index, Tide* tide)
{
  const int& nReservoirs = tide->getNumberOfReservoirs();
  if (nReservoirs == 0)
    return;
  const hsize_t fsubset_dims[2] = {1, nReservoirs};
  const int mdata_rank = 1;
  const hsize_t mdata_dims[1] = { nReservoirs };
  const hsize_t h_offset[2] = { index, 0 };

  hid_t dset_id = H5Dopen(hydrodataGid_, "reservoir height", H5P_DEFAULT);
  if (dset_id < 0) {
    throw std::runtime_error("Cannot open a data set.");
  }
  hid_t fspace_id = H5Dget_space(dset_id);
  if (fspace_id < 0) {
    throw std::runtime_error("Cannot get a file dataspace.");
  }
  h5Err_ = H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, h_offset,
    NULL, fsubset_dims, NULL);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot select channel slab.");
  }
  hid_t mspace_id = H5Screate_simple(mdata_rank,
    mdata_dims, mdata_dims);

  double* reservoirHegihts = tide->getReservoirHeights();
  h5Err_ = H5Dread(dset_id, H5T_NATIVE_DOUBLE, mspace_id,
    fspace_id, H5P_DEFAULT, reservoirHegihts);
  H5Sclose(fspace_id);
  H5Sclose(mspace_id);
  H5Dclose(dset_id);
}

void H5TideReader::readChannelStageData(const long& index, Tide* tide)
{
  const int& nChannels = tide->getNumberOfChannels();
  const hsize_t chan_z_fsubset_dims[3] = { 1, nChannels, 2 };
  const int chan_z_mdata_rank = 2;
  const hsize_t chan_z_mdata_dims[2] = { nChannels, 2 };
  const hsize_t h_offset[3] = { index, 0, 0 };

  hid_t chan_z_dset_id =
    H5Dopen(hydrodataGid_, "channel stage", H5P_DEFAULT);
  if (chan_z_dset_id < 0) {
    throw std::runtime_error("Cannot open a data set.");
  }
  hid_t chan_z_fspace_id = H5Dget_space(chan_z_dset_id);
  if (chan_z_fspace_id < 0) {
    throw std::runtime_error("Cannot get a file dataspace.");
  }
  h5Err_ = H5Sselect_hyperslab(chan_z_fspace_id, H5S_SELECT_SET, h_offset,
    NULL, chan_z_fsubset_dims, NULL);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot select channel slab.");
  }

  hid_t chan_z_mspace_id = H5Screate_simple(chan_z_mdata_rank,
    chan_z_mdata_dims, chan_z_mdata_dims);
  double* channelStage = tide->getChannelStages();
  h5Err_ = H5Dread(chan_z_dset_id, H5T_NATIVE_DOUBLE, chan_z_mspace_id,
    chan_z_fspace_id, H5P_DEFAULT, channelStage);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot read channel stage.");
  }

  H5Sclose(chan_z_mspace_id);
  H5Sclose(chan_z_fspace_id);
  H5Dclose(chan_z_dset_id);
}

void H5TideReader::readChannelFlowData(const long& index, Tide* tide)
{
  const int& nChannels = tide->getNumberOfChannels();
  const hsize_t chan_q_fsubset_dims[3] = { 1, nChannels, 2 };
  const int chan_q_mdata_rank = 2;
  const hsize_t chan_q_mdata_dims[2] = { 2, nChannels };
  const hsize_t h_offset[3] = { index, 0, 0 };

  hid_t chan_q_dset_id = H5Dopen(hydrodataGid_, "channel flow", H5P_DEFAULT);
  if (chan_q_dset_id < 0) {
    throw std::runtime_error("Cannot open a data set.");
  }
  hid_t chan_q_fspace_id = H5Dget_space(chan_q_dset_id);
  if (chan_q_fspace_id < 0) {
    throw std::runtime_error("Cannot get a file dataspace.");
  }
  h5Err_ = H5Sselect_hyperslab(chan_q_fspace_id, H5S_SELECT_SET, h_offset,
    NULL, chan_q_fsubset_dims, NULL);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot select channel slab.");
  }
  hid_t chan_q_mspace_id = H5Screate_simple(chan_q_mdata_rank,
    chan_q_mdata_dims, chan_q_mdata_dims);

  double* channelFlows = tide->getChannelFlows();
  h5Err_ = H5Dread(chan_q_dset_id, H5T_NATIVE_DOUBLE, chan_q_mspace_id,
    chan_q_fspace_id, H5P_DEFAULT, channelFlows);

  H5Sclose(chan_q_mspace_id);
  H5Sclose(chan_q_fspace_id);
  H5Dclose(chan_q_dset_id);
}

void H5TideReader::readChannelAreaData(const long& index,
                                       Tide* tide)
{
  const int& nChannels = tide->getNumberOfChannels();
  const hsize_t chan_a_fsubset_dims[3] = { 1, nChannels, 2 };
  const int chan_a_mdata_rank = 2;
  const hsize_t chan_a_mdata_dims[2] = { 2, nChannels };
  const hsize_t h_offset[3] = { index, 0, 0 };

  hid_t chan_a_dset_id = H5Dopen(hydrodataGid_, "channel area", H5P_DEFAULT);
  if (chan_a_dset_id < 0) {
    throw std::runtime_error("Cannot open a data set.");
  }
  hid_t chan_a_fspace_id = H5Dget_space(chan_a_dset_id);
  if (chan_a_fspace_id < 0) {
    throw std::runtime_error("Cannot get a file dataspace.");
  }
  h5Err_ = H5Sselect_hyperslab(chan_a_fspace_id, H5S_SELECT_SET, h_offset,
    NULL, chan_a_fsubset_dims, NULL);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot select channel slab.");
  }
  hid_t chan_a_mspace_id = H5Screate_simple(chan_a_mdata_rank,
    chan_a_mdata_dims, chan_a_mdata_dims);

  double* channelArea = tide->getChannelAreas();
  h5Err_ = H5Dread(chan_a_dset_id, H5T_NATIVE_DOUBLE, chan_a_mspace_id,
    chan_a_fspace_id, H5P_DEFAULT, channelArea);
  H5Sclose(chan_a_fspace_id);
  H5Sclose(chan_a_mspace_id);
  H5Dclose(chan_a_dset_id);
}

void H5TideReader::readChannelAverageAreaData(const long& index,
                                              Tide* tide)
{
  const int& nChannels = tide->getNumberOfChannels();
  const hsize_t chan_aa_fsubset_dims[2] = { 1, nChannels };
  const int chan_aa_mdata_rank = 1;
  const hsize_t chan_aa_mdata_dims[1] = { nChannels };
  const hsize_t h_offset[2] = { index, 0 };

  hid_t chan_aa_dset_id = H5Dopen(hydrodataGid_, "channel avg area",
    H5P_DEFAULT);
  if (chan_aa_dset_id < 0) {
    throw std::runtime_error("Cannot open a data set.");
  }
  hid_t chan_aa_fspace_id = H5Dget_space(chan_aa_dset_id);
  if (chan_aa_fspace_id < 0) {
    throw std::runtime_error("Cannot get a file dataspace.");
  }
  h5Err_ = H5Sselect_hyperslab(chan_aa_fspace_id, H5S_SELECT_SET, h_offset,
    NULL, chan_aa_fsubset_dims, NULL);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot select channel slab.");
  }
  hid_t chan_aa_mspace_id = H5Screate_simple(chan_aa_mdata_rank,
    chan_aa_mdata_dims, chan_aa_mdata_dims);

  // @todo: Do we need to read all of the channel all the time?
  double* channelAverageArea = tide->getChannelAverageAreas();
  h5Err_ = H5Dread(chan_aa_dset_id, H5T_NATIVE_DOUBLE, chan_aa_mspace_id,
    chan_aa_fspace_id, H5P_DEFAULT, channelAverageArea);
  H5Sclose(chan_aa_fspace_id);
  H5Sclose(chan_aa_mspace_id);
  H5Dclose(chan_aa_dset_id);
}

void H5TideReader::readReservoirFlowData(const long& index,
                                         Tide* tide)
{
  const int& nItems = tide->getNumberOfReservoirConnects();
  if (nItems == 0) return;
  const hsize_t fsubset_dims[2] = { 1, nItems };
  const int mdata_rank = 1;
  const hsize_t mdata_dims[1] = { nItems };
  const hsize_t h_offset[2] = { index, 0 };

  hid_t dset_id = H5Dopen(hydrodataGid_, "reservoir flow",
    H5P_DEFAULT);
  if (dset_id < 0) {
    throw std::runtime_error("Cannot open a data set.");
  }
  hid_t fspace_id = H5Dget_space(dset_id);
  if (fspace_id < 0) {
    throw std::runtime_error("Cannot get a file dataspace.");
  }
  h5Err_ = H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, h_offset,
    NULL, fsubset_dims, NULL);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot select channel slab.");
  }
  hid_t mspace_id = H5Screate_simple(mdata_rank,
    mdata_dims, mdata_dims);

  // @todo: Do we need to read all of the channel all the time?
  double* reservoirFlows = tide->getReservoirFlows();
  h5Err_ = H5Dread(dset_id, H5T_NATIVE_DOUBLE, mspace_id,
    fspace_id, H5P_DEFAULT, reservoirFlows);
  H5Sclose(fspace_id);
  H5Sclose(mspace_id);
  H5Dclose(dset_id);
}

void H5TideReader::readSourceFlowData(const long& index,
                                      Tide* tide)
{
  const int& nSourceFlows = tide->getNumberOfSourceFlows();
  if (nSourceFlows == 0)
    return;
  const hsize_t fsubset_dims[2] = {1, nSourceFlows};
  const int mdata_rank = 1;
  const hsize_t mdata_dims[1] = { nSourceFlows };
  const hsize_t h_offset[2] = { index, 0 };

  hid_t dset_id = H5Dopen(hydrodataGid_, "qext flow", H5P_DEFAULT);
  if (dset_id < 0) {
    throw std::runtime_error("Cannot open a data set.");
  }
  hid_t fspace_id = H5Dget_space(dset_id);
  if (fspace_id < 0) {
    throw std::runtime_error("Cannot get a file dataspace.");
  }
  h5Err_ = H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, h_offset,
    NULL, fsubset_dims, NULL);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot select channel slab.");
  }
  hid_t mspace_id = H5Screate_simple(mdata_rank,
    mdata_dims, mdata_dims);

  double* sourceFlow = tide->getSourceFlows();
  h5Err_ = H5Dread(dset_id, H5T_NATIVE_DOUBLE, mspace_id,
    fspace_id, H5P_DEFAULT, sourceFlow);
  H5Sclose(fspace_id);
  H5Sclose(mspace_id);
  H5Dclose(dset_id);
}

void H5TideReader::retrieveTimeCoverageOfTideFiles()
{
  // Loop through the file list
  int fileCounter = 0;
  BOOST_FOREACH(H5TideFile& tideFile, tideFiles_) {
    openFile(tideFile.getFilename());
    this->getH5TimeStart();
    this->getH5NumberOfRecords();
    this->getH5TimeInterval();
    tideFile.setDataStartTime(julminStart_);
    tideFile.setDataEndTime(julminStart_ + long(minInterval_ * nRecords_));
    closeFile();
  }
}

void H5TideReader::openFile(const std::string& filename)
{
  // Check the file
  if (!boost::filesystem::exists(boost::filesystem::path(filename))) {
    std::stringstream ss;
    ss << "A tide file cannot be found: " << filename << std::endl;
    throw std::runtime_error(ss.str());
  }

  // Open a hdf5 file and close it
  //H5open();
  fid_ = H5Fopen(filename.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
  if (fid_ < 0)
    throw std::runtime_error("Cannot open the HDF5 file:");

  std::string gname("hydro");
  hydroGid_ = H5Gopen(fid_, gname.c_str(), H5P_DEFAULT);
  if (hydroGid_ < 0)
    throw std::runtime_error("Cannot open a group hydro.");

  gname = "data";
  hydrodataGid_ = H5Gopen(hydroGid_, gname.c_str(), H5P_DEFAULT);
  if (hydrodataGid_ < 0)
    throw std::runtime_error("Cannot open a group data.");
}

void H5TideReader::getH5TimeStart()
{
  hid_t a_type_id = H5Tcopy(H5T_NATIVE_INT);
  hid_t attr_id = H5Aopen_name(hydrodataGid_, "Start Time");
  h5Err_ = H5Aread(attr_id, a_type_id, &julminStart_);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot open an attribute.");
  }
  h5Err_ = H5Aclose(attr_id);
}

void H5TideReader::getH5TimeInterval()
{
  hid_t a_type_id = H5Tcopy(H5T_NATIVE_INT);
  hid_t attr_id = H5Aopen_name(hydrodataGid_, "Time Interval");
  h5Err_ = H5Aread(attr_id, a_type_id, &minInterval_);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot open an attribute: Time Interval.");
  }
  h5Err_ = H5Aclose(attr_id);
}

void H5TideReader::getH5NumberOfRecords()
{
  hid_t a_type_id = H5Tcopy(H5T_NATIVE_INT);
  hid_t attr_id = H5Aopen_name(hydroGid_, "Number of intervals");
  h5Err_ = H5Aread(attr_id, a_type_id, &nRecords_);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot open an attribute: # of intervals");
  }
  h5Err_ = H5Aclose(attr_id);
}


void H5TideReader::closeFile()
{
  // Close
  h5Err_ = H5Gclose(hydrodataGid_);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot close a group.");
  }
  h5Err_ = H5Gclose(hydroGid_);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot close a group.");
  }
  h5Err_ = H5Fclose(fid_);
  if (h5Err_ < 0) {
    throw std::runtime_error("Cannot close a file id.");
  }
  //h5Err_ = H5close();
  //if (h5Err_ < 0) {
  //  throw std::runtime_error("Cannot close a file.");
  //}
}

const H5TideFile& H5TideReader::findFile(const int& t)
{
  std::vector<H5TideFile>::const_iterator found
    = find_if(tideFiles_.begin(), tideFiles_.end(),
    boost::lambda::bind(&H5TideReader::compareTime, &*this, boost::lambda::_1, t));
  if ( found == tideFiles_.end() ) {
    throw std::runtime_error("No tide file before the given time..");
  }
  return *found;
}

bool H5TideReader::compareTime(const H5TideFile& tidefile, const int& julmin)
{
  return tidefile.getUsageStartTime() <= julmin && julmin < tidefile.getUsageEndTime();
}

}
