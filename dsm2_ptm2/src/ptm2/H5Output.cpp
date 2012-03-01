/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "H5Output.h"
#include "Channel.h"
#include "Reservoir.h"
#include "boost/format.hpp"
#include "boost/foreach.hpp"

namespace PTM2
{

hid_t H5Output::fileId_ = -1;

H5Output::~H5Output()
{
}

void H5Output::open(const std::string& fname)
{
  hid_t plistId = H5Pcreate(H5P_FILE_ACCESS);
  int mdc_nelmts;
  size_t rdcc_nelmts;
  size_t rdcc_nbytes;
  double rddc_w0;
  H5Pget_cache(plistId, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rddc_w0);
  int rdcc_nslots = 8000000;
  H5Pset_cache(plistId, mdc_nelmts, rdcc_nslots, rdcc_nbytes, rddc_w0);
  fileId_ = H5Fcreate(fname.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, plistId);
  //fileId_ = H5Fcreate(fname.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if (fileId_ < 0) {
    throw std::runtime_error("H5Output::open: Cannot open a hdf5 output file.");
  }

  nSteps_ = 0;
}

void H5Output::close()
{
  H5Fflush(fileId_, H5F_SCOPE_LOCAL);
  //H5Fclose(fileId_);
}

void H5Output::writeGrid(const Grid& grid)
{
  herr_t h5err;
  std::string groupName("geometry");
  hid_t lcpl_id = H5P_DEFAULT;
  hid_t gcpl_id = H5P_DEFAULT;
  hid_t gapl_id = H5P_DEFAULT;
  hid_t geometryGroupId = H5Gcreate(fileId_, groupName.c_str(), lcpl_id,
                                    gcpl_id, gapl_id);

  // Channel
  const int nChannels = grid.getNumberOfChannels();
  int* channelIds = new int[nChannels];
  float* channelLengths = new float[nChannels];
  int i = 0;
  BOOST_FOREACH(const Channel* channel, grid.getChannels()) {
    channelIds[i] = boost::lexical_cast<int>(channel->getExternalId());
    channelLengths[i] = float(channel->getLength());
      ++i;
  }
  int rank = 1;
  hsize_t dims[1];
  dims[0] = nChannels;
  std::string dataName("channel_number");
  writeIntData(dataName, geometryGroupId, rank, dims, channelIds);
  delete[] channelIds;
  dataName = "channel_length";
  writeFloatData(dataName, geometryGroupId, rank, dims, channelLengths);
  delete[] channelLengths;

  H5Gclose(geometryGroupId);
}

void H5Output::writeIntData(const std::string& name,
                            hid_t hid,
                            const int rank,
                            const hsize_t* dims,
                            const int* data)
{
  hid_t spaceId = H5Screate_simple(rank, dims, dims);
  hid_t dsetId = H5Dcreate(hid, name.c_str(), H5T_NATIVE_INT, spaceId,
                           H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hsize_t offset[1];
  offset[0] = 0;
  hsize_t stride[1];
  stride[0] = 0;
  hsize_t counts[1];
  counts[0] = dims[0];
  H5Sselect_hyperslab(spaceId, H5S_SELECT_SET, offset, NULL, counts, NULL);
  H5Dwrite(dsetId, H5T_NATIVE_INT, spaceId, H5S_ALL, H5P_DEFAULT, data);
  H5Dclose(dsetId);
  H5Sclose(spaceId);
}

void H5Output::writeFloatData(const std::string& name,
                              hid_t hid,
                              const int rank,
                              const hsize_t* dims,
                              const float* data)
{
  hid_t spaceId = H5Screate_simple(rank, dims, dims);
  hid_t dsetId = H5Dcreate(hid, name.c_str(), H5T_NATIVE_FLOAT, spaceId,
                           H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  hsize_t offset[1];
  offset[0] = 0;
  hsize_t stride[1];
  stride[0] = 0;
  hsize_t counts[1];
  counts[0] = dims[0];
  H5Sselect_hyperslab(spaceId, H5S_SELECT_SET, offset, NULL, counts, NULL);
  H5Dwrite(dsetId, H5T_NATIVE_FLOAT, spaceId, H5S_ALL, H5P_DEFAULT, data);
  H5Dclose(dsetId);
  H5Sclose(spaceId);
}

void H5Output::createTimeStep(const int& julmin)
{
  hid_t lcpl_id = H5P_DEFAULT;
  hid_t gcpl_id = H5P_DEFAULT;
  hid_t gapl_id = H5P_DEFAULT;
  const std::string baseGroupName("location");
  hid_t base_gid;
  htri_t trial = H5Lexists(fileId_, baseGroupName.c_str(), lcpl_id);
  if (!trial) {
    base_gid = H5Gcreate(fileId_, baseGroupName.c_str(), lcpl_id, gcpl_id,
                         gapl_id);
  }
  else {
    base_gid = H5Gopen(fileId_, baseGroupName.c_str(), gapl_id);
  }
  if (base_gid < 0) {
    std::runtime_error("H5Output::createTimeStep: Cannot open a group.");
  }
  std::string groupName = boost::str(boost::format("%d") % nSteps_);
  currentTimeGroupId_ = H5Gcreate(base_gid, groupName.c_str(), lcpl_id, gcpl_id,
                                  gapl_id);
  if (currentTimeGroupId_ < 0) {
    throw std::runtime_error(
        "H5Output::createTimeStep: cannot create a group for a time step.");
  }
  ++nSteps_;

  // Write time stamp
  {
    hsize_t dims[1];
    dims[0] = 1;
    hid_t spaceId = H5Screate_simple(1, dims, dims);
    hid_t attrId = H5Acreate(currentTimeGroupId_, "jul_min", H5T_NATIVE_INT,
                             spaceId, H5P_DEFAULT, H5P_DEFAULT);

    PTM2Time& ptm2time = *PTM2Time::instance();
    H5Awrite(attrId, H5T_NATIVE_INT, &julmin);
    H5Aclose(attrId);
    H5Sclose(spaceId);
  }
}

void H5Output::closeTimeStep()
{
  H5Gclose(currentTimeGroupId_);
}

void H5Output::writeInserting(const Particle& particle)
{

}

void H5Output::writeExiting(const Particle& particle)
{

}

void H5Output::writeParticles(const std::vector<Particle*>& particles)
{
  const int nParticles = particles.size();
  int nLiveParticles = 0;
  // Body type
  {
    intCache_.resize(nParticles);
    BOOST_FOREACH(const Particle* particle, particles) {
      if (particle->isActive()) {
        const Waterbody* wb = particle->getCoordinate().getWaterbody();
        intCache_[nLiveParticles] = wb->getType();
        ++nLiveParticles;
      }
    }

    if (nLiveParticles != 0) {
      int rank = 1;
      hsize_t dims[1];
      dims[0] = nLiveParticles;
      hid_t spaceId = H5Screate_simple(rank, dims, NULL);
      hid_t dsetId = H5Dcreate(currentTimeGroupId_, "waterbody_type",
                               H5T_NATIVE_INT, spaceId, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT);
      hsize_t offset[1];
      offset[0] = 0;
      hsize_t stride[1];
      stride[0] = 0;
      hsize_t counts[1];
      counts[0] = nLiveParticles;
      H5Sselect_hyperslab(spaceId, H5S_SELECT_SET, offset, NULL, counts, NULL);
      H5Dwrite(dsetId, H5T_NATIVE_INT, spaceId, H5S_ALL, H5P_DEFAULT,
               &intCache_[0]);

      H5Dclose(dsetId);
      H5Sclose(spaceId);
    }
  }

  {
    hsize_t dims[1];
    dims[0] = 1;
    hid_t nullSpaceId = H5Screate_simple(1, dims, NULL);
    hid_t attrId = H5Acreate(currentTimeGroupId_, "n_particles", H5T_NATIVE_INT,
                             nullSpaceId, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attrId, H5T_NATIVE_INT, &nLiveParticles);
    H5Aclose(attrId);
    H5Sclose(nullSpaceId);
  }

  // body id
  {
    if (nLiveParticles != 0) {
      int i = 0;
      BOOST_FOREACH(const Particle* particle, particles) {
        if (particle->isActive()) {
          intCache_[i] = particle->getCoordinate().getWaterbody()->getId();
          ++i;
        }
      }
      int rank = 1;
      hsize_t dims[1];
      dims[0] = nLiveParticles;
      hid_t spaceId = H5Screate_simple(rank, dims, dims);
      hid_t dsetId = H5Dcreate(currentTimeGroupId_, "waterbody_id",
                               H5T_NATIVE_INT, spaceId, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT);
      hsize_t offset[1];
      offset[0] = 0;
      hsize_t stride[1];
      stride[0] = 0;
      hsize_t counts[1];
      counts[0] = nLiveParticles;
      H5Sselect_hyperslab(spaceId, H5S_SELECT_SET, offset, NULL, counts, NULL);
      H5Dwrite(dsetId, H5T_NATIVE_INT, spaceId, H5S_ALL, H5P_DEFAULT,
               &intCache_[0]);

      H5Dclose(dsetId);
      H5Sclose(spaceId);
    }
  }

  // X positions
  {
    if (nLiveParticles != 0) {
      doubleCache_.resize(nParticles);
      int i = 0;
      BOOST_FOREACH(const Particle* particle, particles) {
        if (particle->isActive()) {
          doubleCache_[i] = particle->getCoordinate()(0);
          ++i;
        }
      }
      int rank = 1;
      hsize_t dims[1];
      dims[0] = nLiveParticles;
      hid_t spaceId = H5Screate_simple(rank, dims, dims);
      hid_t dsetId = H5Dcreate(currentTimeGroupId_, "x", H5T_NATIVE_DOUBLE,
                               spaceId, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      hsize_t offset[1];
      offset[0] = 0;
      hsize_t stride[1];
      stride[0] = 0;
      hsize_t counts[1];
      counts[0] = nLiveParticles;
      H5Sselect_hyperslab(spaceId, H5S_SELECT_SET, offset, NULL, counts, NULL);
      H5Dwrite(dsetId, H5T_NATIVE_DOUBLE, spaceId, H5S_ALL, H5P_DEFAULT,
               &doubleCache_[0]);

      H5Dclose(dsetId);
      H5Sclose(spaceId);
    }
  }
  H5Fflush(fileId_, H5F_SCOPE_LOCAL);
}

}
