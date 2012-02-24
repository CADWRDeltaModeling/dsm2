/**
 * Author: Kijin Nam
 **/

#ifndef _PTM2PostprocessorInput_h_
#define _PTM2PostprocessorInput_h_

#include "PTM2Postprocessor.h"
#include "PTM2FluxOutput.h"
#include "DSM2Input.h"

namespace PTM2
{

class PTM2PostprocessorInput : public DSM2Input
{
private:
  PTM2Postprocessor& postprocessor_;
  PTM2PostprocessorParameters& postprocessorParameters_;

public:
  PTM2PostprocessorInput(PTM2Postprocessor& postprocessor)
    : postprocessor_(postprocessor), postprocessorParameters_(postprocessor.getParameters()),
    DSM2Input(postprocessor.getGrid(), postprocessor.getParameters()) {}
  void readInputFile(const std::string& filename);

private:
  void registerPTMScalars();
  void registerGroup();
  void registerGroupMembers();
  void registerFluxOutput();
  void registerGroupOutput();
  void registerInsertions();
  void processFluxOutput(const std::string& wb,
    std::vector<int>& waterbodyTypes,
    std::vector<int>& waterbodies);

  //void getWaterbodyUniqueId();
  const std::string getObjType(const std::string& objname);
  const std::string getObjName(const std::string& objname);
  int convertToMin(const std::string& interval) const;
};

}

#endif
