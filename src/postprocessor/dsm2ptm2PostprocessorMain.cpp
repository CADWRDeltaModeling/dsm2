/**
 * Author: Kijin Nam, knam@water.ca.gov
 */
#include <iostream>
#include "PTM2Postprocessor.h"
#include "PTM2PostprocessorInput.h"

using namespace PTM2;

int main(int argc, char* argv[])
{ 
  try
  {
    if ( argc < 2 ) {
      std::cout << "Usage: " << argv[0]
      << " input_file_name" << std::endl;
      throw std::runtime_error("A user did not provided an input file.");
    }
    std::string inputFileName;
    inputFileName = argv[1];

    PTM2Postprocessor postprocessor;
    PTM2PostprocessorInput dsm2input(postprocessor);
    dsm2input.readInputFile(inputFileName);
    postprocessor.run();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << std::endl;
    return -1;
  }
  return 0;
}