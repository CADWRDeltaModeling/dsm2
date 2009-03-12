#ifndef _EXCEPTION_TRAPPING_H
#define _EXCEPTION_TRAPPING_H
#include <string>
static int SUCCESS = 0;
static int RUNTIME_ERROR = -3;
static int LOGIC_ERROR = -2;
static int GENERAL_FAIL = -1;

void input_storage_log_err_f(const std::string & message);



/** This macro defines the way C++ exceptions are trapped
    and converted to error codes that can be passed back
    to the calling routine. _CODE is the code to be wrapped
    and checked for exceptions, _ERROR_VAR is the variable
    to which to assign an error code. As a side effect,
    input_storage_log_err_f(message) will be called.
*/
#define _TRAP_EXCEPT(_ERROR_VAR,_CODE) \
    try{ _CODE } \
    catch (std::runtime_error rte) \
    {\
        std::string message("\n*****\nRuntime/Input error:"); \
        message += rte.what();\
        input_storage_log_err_f(message);\
        _ERROR_VAR= RUNTIME_ERROR;\
    }\
    catch (std::logic_error le) \
    {\
        std::string message("\n*****\nLogical/Programming error:");\
        message += le.what();\
        input_storage_log_err_f(message);\
        _ERROR_VAR = LOGIC_ERROR;\
    }\
    catch (...) \
    {\
        std::string message("\n*****\nGeneral error (no info):");\
        input_storage_log_err_f(message);\
        _ERROR_VAR = GENERAL_FAIL;\
    }\




#endif