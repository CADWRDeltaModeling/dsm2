/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _H5TideFile_h_
#define _H5TideFile_h_

#include "PTM2.h"
#include <string>

namespace PTM2
{

/**
 * @brief Tidefile class
 **/
class H5TideFile
{
  // Properties
protected:
  std::string filename_;
  int julminUsageStart_;
  int julminUsageEnd_;
  int julminDataStart_;
  int julminDataEnd_;

  // Methods
public:
  // Constructors
  H5TideFile()
  {
  }
  H5TideFile(const std::string& filename,
             const int& julminStart,
             const int& julminEnd) :
      filename_(filename), julminUsageStart_(julminStart), julminUsageEnd_(
          julminEnd)
  {
  }
  H5TideFile(const H5TideFile& r) :
      filename_(r.filename_), julminUsageStart_(r.julminUsageStart_), julminUsageEnd_(
          r.julminUsageEnd_)
  {
  }

  // Accessors
  std::string& getFilename()
  {
    return filename_;
  }
  const std::string& getFilename() const
  {
    return filename_;
  }
  int& getUsageStartTime()
  {
    return julminUsageStart_;
  }
  const int& getUsageStartTime() const
  {
    return julminUsageStart_;
  }
  void setUsageStartTime(const int& julminUsageStart)
  {
    julminUsageStart_ = julminUsageStart;
  }
  int& getUsageEndTime()
  {
    return julminUsageEnd_;
  }
  const int& getUsageEndTime() const
  {
    return julminUsageEnd_;
  }
  void setUsageEndTime(const int& julminUsageEnd)
  {
    julminUsageEnd_ = julminUsageEnd;
  }

  int& getDataStartTime()
  {
    return julminDataStart_;
  }
  const int& getDataStartTime() const
  {
    return julminDataStart_;
  }
  void setDataStartTime(const int& julminDataStart)
  {
    julminDataStart_ = julminDataStart;
  }
  int& getDataEndTime()
  {
    return julminDataEnd_;
  }
  const int& getDataEndTime() const
  {
    return julminDataEnd_;
  }
  void setDataEndTime(const int& julminDataEnd)
  {
    julminDataEnd_ = julminDataEnd;
  }
};

}

#endif
