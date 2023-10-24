#include "exception_trapping.h"

#include <iostream>
#include <string>
void input_storage_log_err_f(const std::string & message)
{
   std::cerr << message << std::endl;
}
