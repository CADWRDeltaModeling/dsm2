%PROGRAM diffusion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
% Department of Water Resources.
% This file is part of DSM2.

% The Delta Simulation Model 2 (DSM2) is free software: 
% you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.

% DSM2 is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with DSM2.  If not, see <http://www.gnu.org/licenses>.

% Delta Modeling Section 
% Modeling Support Branch
% Bay-Delta Office
% California Department of Water Resources
% http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/index.cfm
%
% PURPOSE:
%
% It solves the dispersion of sediment in the longitudinal direction
% Partial_(A C_s)/Partial_t = Partial/Partial_x [A K_s Partial_(C_s)/Partial_x]
% 
% These sub-routines are being developed by the Department of Civil and 
% Environmental Engineering at the University of California, Davis and DWR
% Coded by F.A. Bombardelli and K. Zamani
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variables: data dictionary.  Define variables types, definitions and units

% C_s_n_plus_one(i): concentration of sediment in suspension averaged in the volume i
%      for time t + 1
% C_S_n(i): concentration of sediment in suspension averaged in the volume
%         for time t
% K_s_n_plus_one(i): dispersion coefficient at the left face of volume i for time t + 1
% K_s_n(i): dispersion coefficient at the left face of volume i for time t
% Area_n_plus_one(i): Area of cross section evaluated at the left face of volume i at time
%                     t + 1. This is an output coming from HYDRO
% Area_n(i): Area of cross section evaluated at the left face of volume i at time
%            t. This is an output coming from HYDRO
% A(i): Values of the coefficients below diagonal in matrix
% B(i): Values of the coefficients at the diagonal in matrix
% C(i): Values of the coefficients above diagonal in matrix
% D(i): Values of the independent term
% N_volumes: Number of volumes (computational cross sections) whose
%          sediment concentration will be computed
% dt_sed: Time step for the computation of sediment transport (in seconds)
% dx_sed: Spatial step for the computation of sediment transport (in feet for STM)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic setting
clear all
clc
close all
format long
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%set the input parameters
[dt_sed,dx_sed,i_case,Theta,N_volumes,N,Cal_D,Cal_D_one,N_times]=call_setting();
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get input from HYDRO
[Area_n,Area_n_plus_one,K_s_n,K_s_n_plus_one]=call_hydro_A_KS(N);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%C(x,0) Initial condition of C(x,t)
[C_s_n]=call_initial_c(i_case,N,dx_sed);
result(:,1)=C_s_n;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% marching on time
for j=1:N_times
     Time = dt_sed * j ; 
     % B.C: C(0,t) and C(L,t)
[C_s_upstream,C_s_downstream, Flux_s_upstream_n,Flux_s_upstream_n_plus_one...
    ,Boundary_condition_flag_upstream,Boundary_condition_flag_downstream]...
    =load_b_c(i_case,Time,dt_sed);
% BigK*C=U - where C is unkowns matrix of sediment concentration
 [BigK,U]=make_bigk_u(N,Area_n_plus_one,Area_n,Cal_D,Cal_D_one,...
     K_s_n_plus_one,K_s_n,C_s_n,Flux_s_upstream_n,dx_sed);
 % apply BC to BigK and U    
[U,BigK]=apply_b_c(Boundary_condition_flag_upstream,...
    Boundary_condition_flag_downstream,BigK,U,Area_n,Area_n_plus_one,K_s_n,...
    K_s_n_plus_one,C_s_downstream,C_s_upstream,Flux_s_upstream_n,...
    Flux_s_upstream_n_plus_one,dx_sed,Cal_D,...
    Cal_D_one,C_s_n,N);
%solve
C_s_n=inv(BigK)*U;
%put in a file
result(:,j+1)=C_s_n;
end
%end marching on time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
post_process(dx_sed,N,N_times,result,dt_sed)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

