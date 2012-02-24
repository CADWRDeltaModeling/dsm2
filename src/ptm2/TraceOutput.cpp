/**
 * Author: Kijin Nam, knam@water.ca.gov
 */

#include "TraceOutput.h"
#include "H5Output.h"
#include "PTM2Time.h"
#include "boost/format.hpp"

namespace PTM2
{

TraceOutput::TraceOutput()
: fileId_(-1), nRecords_(0),
//  id_(maxRecords_),  ts_(maxRecords_), wbTypeFrom_(maxRecords_),
//  wbFrom_(maxRecords_), wbTypeTo_(maxRecords_), wbTo_(maxRecords_),
intCache_(TraceOutput::INT_NUM_FIELDS), ptm2time_(*PTM2Time::instance())
{
	for (int i = 0; i < INT_NUM_FIELDS; ++i) {
		intCache_[i].resize(FLUSH_LIMIT);
	}
	intFieldNames_.resize(INT_NUM_FIELDS);
	intFieldNames_[ID] = "particle_id";
	intFieldNames_[TS] = "timestamp";
	intFieldNames_[WB_T_FROM] = "wb_type_from";
	intFieldNames_[WB_FROM] = "wb_from";
	intFieldNames_[WB_T_TO] = "wb_type_to";
	intFieldNames_[WB_TO] = "wb_to";
}

TraceOutput::~TraceOutput()
{
}            

void TraceOutput::open(const std::string& fname)
{
  //std::cout << "Open a file : " << fname << "\n";
  //hid_t plistId = H5Pcreate(H5P_FILE_ACCESS);
  //int mdc_nelmts;
  //size_t rdcc_nelmts ;
  //size_t rdcc_nbytes;
  //double rddc_w0;
  //H5Pget_cache(plistId, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rddc_w0);
  //int rdcc_nslots = 8000000;
  //H5Pset_cache(plistId, mdc_nelmts, rdcc_nslots, rdcc_nbytes, rddc_w0);
  //fileId_ = H5Fcreate(fname.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, plistId);
  //fileId_ = H5Fcreate(fname.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  fileId_ = H5Output::getH5FileHandle();
  if (fileId_ < 0 ) {
    throw std::runtime_error("TraceOutput::open: Cannot open a hdf5 output file.");
  }
  else {
    {
      char fname[255];
      H5Fget_name(fileId_, fname, 254);
      std::cout << fname << "\n";
    }
  }
}

void TraceOutput::close()
{
  if (nRecords_ > 0) {
    flush(true);
    //H5Fflush(fileId_, H5F_SCOPE_LOCAL);
    //H5Fclose(fileId_)
  }
}

void TraceOutput::writeHeader()
{
}

void TraceOutput::writeRecord(const Particle& particle,
                              const Waterbody* wbFrom,
                              const Waterbody* wbTo)
{
  const int nSize = intCache_[ID].size();
  if (nRecords_ >= nSize) {
  	for (int i = 0; i < INT_NUM_FIELDS; ++i) {
	  	intCache_[i].resize(nSize + FLUSH_CHUNK);
	  }
  }
  assert(nRecords_ < intCache_[ID].size());
  intCache_[ID][nRecords_] = particle.getId();
  intCache_[TS][nRecords_] = ptm2time_.julsec2julmin(particle.getCurrentTime());
  intCache_[WB_T_FROM][nRecords_] = wbFrom->getType();
  intCache_[WB_FROM][nRecords_] = wbFrom->getId();
  intCache_[WB_T_TO][nRecords_] = wbTo->getType();
  intCache_[WB_TO][nRecords_] = wbTo->getId();
  ++nRecords_;
}

void TraceOutput::flush(const bool flag)
{
  if (nRecords_ < FLUSH_LIMIT && !flag) return;

	if (fileId_ < 0) {
		fileId_ = H5Output::getH5FileHandle();
	}

  const std::string base_gname("traces");
  hid_t base_gid;
  htri_t trial= H5Lexists(fileId_, base_gname.c_str(), H5P_DEFAULT);
  if (!trial) {
    base_gid = H5Gcreate(fileId_, base_gname.c_str(), H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  }
  else {
    base_gid = H5Gopen(fileId_, base_gname.c_str(), H5P_DEFAULT);
  }
  if (base_gid < 0) {
  	std::runtime_error("TraceOutput::flush: Fail to obtain the trace group.");
  }

  hid_t group_gid;
  {
  	hsize_t attr_dims = 1;
  	const std::string attr_name("num_trace_groups");
  	htri_t trial = H5Aexists(base_gid, attr_name.c_str());
  	int nGroups;
  	if (!trial) {
  		hid_t dataspace = H5Screate_simple(1, &attr_dims, NULL);
  		hid_t attr_id = H5Acreate(base_gid, attr_name.c_str(), H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT);
      nGroups = 1;
  		H5Awrite(attr_id, H5T_NATIVE_INT, &nGroups);
  		H5Aclose(attr_id);
  	}
  	else {
  		hid_t attr_id = H5Aopen(base_gid, attr_name.c_str(), H5P_DEFAULT);
  		H5Aread(attr_id, H5T_NATIVE_INT, &nGroups);
  		++nGroups;
  		H5Awrite(attr_id, H5T_NATIVE_INT, &nGroups);
  		H5Aclose(attr_id);
  	}
  	const std::string gname(boost::str(boost::format("%d") % (nGroups - 1)));
		group_gid = H5Gcreate(base_gid, gname.c_str(), H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  }

  {
  	hsize_t dims = nRecords_;
  	const int rank = 1;
    for (int i = 0; i < INT_NUM_FIELDS; ++i) {
    	hid_t dataspace = H5Screate_simple(rank, &dims, NULL);
    	hid_t dataset = H5Dcreate(group_gid, intFieldNames_[i].c_str(), H5T_NATIVE_INT,
    			dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    	H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &intCache_[i][0]);
    	H5Dclose(dataset);
   	  H5Sclose(dataspace);
    }
    hsize_t attr_dims = 1;
    std::string attr_name("num_traces");
    hid_t file_dataspace = H5Screate_simple(1, &attr_dims, NULL);
    hid_t attr_id = H5Acreate(group_gid, attr_name.c_str(), H5T_NATIVE_INT, file_dataspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr_id, H5T_NATIVE_INT, &nRecords_);
    H5Aclose(attr_id);
  }
  H5Gclose(group_gid);
  H5Gclose(base_gid);
  clearUp();
}

void TraceOutput::clearUp()
{
  nRecords_ = 0;
}

}
