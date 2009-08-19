function [C_s_upstream,C_s_downstream, Flux_s_upstream_n,...
    Flux_s_upstream_n_plus_one,Boundary_condition_flag_upstream,...
    Boundary_condition_flag_downstream]=load_b_c(i_case,Time,dt_sed)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%this part for MATLAB function
   Boundary_condition_flag_upstream = 1;
   Boundary_condition_flag_downstream = 1;
   C_s_upstream = 0;
   C_s_downstream = 0;
   
   Boundary_condition_flag_upstream = 0;
   Boundary_condition_flag_downstream = 1;
   Flux_s_upstream_n=2-2*pi*sin(0.05*pi)*exp(-1*pi*pi/4*(Time-dt_sed));
   Flux_s_upstream_n_plus_one=2.-2*pi*sin(0.05*pi)*exp(-1*pi*pi/4*Time);
   C_s_downstream = 2;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if i_case==1
   Boundary_condition_flag_upstream = 1;
   Boundary_condition_flag_downstream = 1;
   C_s_upstream = 0;
   C_s_downstream = 0;
elseif i_case==2
   Boundary_condition_flag_upstream = 0;
   Boundary_condition_flag_downstream = 1;
   Flux_s_upstream_n=2-2*pi*sin(0.05*pi)*exp(-1*pi*pi/4*(Time-dt_sed));
   Flux_s_upstream_n_plus_one=2.-2*pi*sin(0.05*pi)*exp(-1*pi*pi/4*Time);
   C_s_downstream = 2;
end 
%elseif i_case==3
%
%
%elseif i_case==4
%
return