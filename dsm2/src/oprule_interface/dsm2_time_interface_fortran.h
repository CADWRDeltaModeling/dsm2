
#ifndef DSM2_TIME_IF_H_RER00KS34
#define DSM2_TIME_IF_H_RER00KS34

#ifdef _WIN32
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
#define time_step_seconds NETCNTRL_mp_NETWORKTIMEINCREMENT
#else
#define get_model_year getmodelyear_
#define get_model_day_of_year getmodeldayofyear_
#define get_model_minute_of_year getmodelminuteofyear_
#define get_reference_minute_of_year getreferenceminuteofyear_

#define get_model_month getmodelmonth_
#define get_model_day getmodelday_
#define get_model_minute_of_day getmodelminuteofday_
#define get_model_hour getmodelhour_
#define get_model_minute getmodelminute_
#define get_model_ticks getmodelticks_
#define cdate_to_jul_min utilities_mp_cdt2jmin_
#define time_step_seconds netcntrl_mp_networktimeincrement_
#endif

#define STDCALL
extern "C" {
   int STDCALL get_model_year();
   int STDCALL get_model_month();
   int STDCALL get_model_day();
   int STDCALL get_model_day_of_year();
   int STDCALL get_model_ticks();
   int STDCALL get_model_minute_of_day();
   int STDCALL get_model_minute_of_year();
   int STDCALL get_reference_minute_of_year(int &mon, int &day, int &hour, int &min);
   int STDCALL get_model_minute();
   int STDCALL get_model_hour();
   int STDCALL cdate_to_jul_min(const char* name,
                                    unsigned int len);
   int time_step_seconds();
}
#endif //include guard
