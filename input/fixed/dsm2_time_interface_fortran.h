
#ifndef DSM2_TIME_IF_H_RER00KS34
#define DSM2_TIME_IF_H_RER00KS34

#define get_model_year GETMODELYEAR
#define get_model_day_of_year GETMODELDAYOFYEAR
#define get_model_minute_of_year GETMODELMINUTEOFYEAR
#define get_reference_minute_of_year GETREFERENCEMINUTEOFYEAR

#define get_model_month GETMODELMONTH
#define get_model_day GETMODELDAY
#define get_model_minute_of_day GETMODELMINUTEOFDAY
#define get_model_hour GETMODELHOUR
#define get_model_minute GETMODELMINUTE
#define get_model_ticks GETMODELTICKS
#define cdate_to_jul_min CDT2JMIN
#define time_step_seconds NETWORKTIMEINCREMENT

extern "C" {
   int __stdcall get_model_year();
   int __stdcall get_model_month();
   int __stdcall get_model_day();
   int __stdcall get_model_day_of_year();
   int __stdcall get_model_ticks();
   int __stdcall get_model_minute_of_day();
   int __stdcall get_model_minute_of_year();
   int __stdcall get_reference_minute_of_year(int &mon, int &day, int &hour, int &min);
   int __stdcall get_model_minute();
   int __stdcall get_model_hour();
   int __stdcall cdate_to_jul_min(const char* name, 
                                    unsigned int len);
   int __stdcall time_step_seconds();
}
#endif //include guard