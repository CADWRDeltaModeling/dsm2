#include "hdf5.h"
#include "hdf5_hl.h"
#include<time.h>
#include <stdio.h>

#define attach_hydro_scales __stdcall ATTACH_HYDRO_DIMSCALES
#define iso_time __stdcall ISO_TIME

void attach_dim(hid_t * fid, char*setname,
				char*dimsetname,char*label,
				unsigned int ndx){
 /**
  attaches dimension scale
  */

 herr_t err = 0;
 // open the main dataset
 hid_t dsetid = H5Dopen(* fid, setname);
 if(dimsetname != 0){
	 // open the dimension scale set
	 hid_t dimsetid = H5Dopen(* fid,dimsetname);
	 // convert to dimension scale
	 err = H5DSset_scale(dimsetid, label);
	 // attach to dataset
	 err = H5DSattach_scale(dsetid, dimsetid, 0);
	 // close dimension
	 err = H5Dclose(dimsetid);
 }
 // add label
 err = H5DSset_label(dsetid, ndx, label);
 // close main dataset
 err = H5Dclose(dsetid);
}


void attach_hydro_scales(hid_t * fid){
 
 char*setname = "hydro/data/channel stage";

 attach_dim( fid, setname,
            "hydro/geometry/channel_number",
			"channel_number",0);
 attach_dim(fid, setname,
            "hydro/geometry/channel_location",
			"channel_number",1);
 attach_dim( fid, setname,
             0,"time",2);

 setname = "hydro/data/channel flow";

 attach_dim( fid, setname,
            "hydro/geometry/channel_number",
			"channel_number",0);
 attach_dim(fid, setname,
            "hydro/geometry/channel_location",
			"channel_number",1);
 attach_dim( fid, setname,
             0,"time",2);

 setname = "hydro/data/channel area";
 
 attach_dim( fid, setname,
            "hydro/geometry/channel_number",
			"channel_number",0);
 attach_dim(fid, setname,
            "hydro/geometry/channel_location",
			"channel_number",1);
 attach_dim( fid, setname,
             0,"time",2);

 setname = "hydro/data/channel avg area";

 attach_dim( fid, setname,
            "hydro/geometry/channel_number",
			"channel_number",0);
 attach_dim( fid, setname,
             0,"time",1);

 setname = "hydro/data/reservoir height";
 attach_dim( fid, setname,
            "hydro/geometry/reservoir_names",
			"reservoir",0);
 attach_dim( fid, setname,
             0,"time",1);

 setname = "hydro/data/reservoir flow";
 attach_dim( fid, setname,
            "hydro/geometry/reservoir_names",
			"reservoir",0);
 attach_dim( fid, setname,
             0,"connection_no",1);
 attach_dim( fid, setname,
             0,"time",2);


 setname = "hydro/data/qext changed";
 attach_dim( fid, setname,
            "hydro/geometry/external_flow_names",
			"name",0);
 attach_dim( fid, setname,
             0,"time",1);

setname = "hydro/data/transfer flow";
 attach_dim( fid, setname,
            "hydro/geometry/transfer_names",
			"name",0);
 attach_dim( fid, setname,
             0,"time",1);



}


void iso_time(int* julmin,char*timestr,int n){
	
   const time_t tm_sec = *julmin*60;
   struct tm * t = gmtime(&tm_sec);

   strftime(timestr,n,"%Y-%m-%d %H:%M:%S",t);
   
}