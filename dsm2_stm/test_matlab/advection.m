%PROGRAM Advection 
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
% It solves the advection of sediment in the longitudinal direction
% Partial_(A C_s)/Partial_t - Partial[U A C_s]/Partial_x = Sink/Source/Decay 
% 
% These sub-routines are being developed by the Department of Civil and 
% Environmental Engineering at the University of California, Davis and DWR
% Coded by F.A. Bombardelli and K. Zamani
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Dictionary  %%%%%%%%% it must check at the end 

%a

%c_s

%c_half

%c_bar-half

%c_bar_front

%c_bar_back

% c_half_front

% c_half_back

%t

%x

%ks

%big_s

%diffusion

%big_q

%dif_c_limited

%diffusion

%a_c_bar_n_plus_one 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic setting
clear all
clc
close all
format long
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[dt,dx,n_vol,n_time,flux_limiter_type]=setting_advection();
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[a,big_q,c_s,ks]=read_a_ks_cs_big_q(n_vol,n_time);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[c_s]=initial_cond_c_s(c_s,n_vol);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[c_s]=boundary_cond_c_s(n_time,c_s);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for j=1:n_time
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Part one c_bar_half and c_half
    for i=1:n_vol
    dif_c_limited(i,j) = flux_limiter(flux_limiter_type,i,c_s,j,dx,n_vol);
    delta_c = dif_c_limited(i,j)*dx;
 
    c_bar_front(i)=c_s(i,j)+ (delta_c/2)*(1- (dt/dx)*((big_q(i,j)+...
        big_q(i+1,j))/(a(i+1,j)+a(i,j))));
    c_bar_back(i)=c_s(i,j)+ (delta_c/2)*(-1-(dt/dx)*((big_q(i,j)+...
        big_q(i+1,j))/(a(i+1,j)+a(i,j))));
    big_s = sink_source(i,j);% it must defined from other STM sub_modules
    end

    for i=2:n_vol
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%we do not have c_s(i+1,j) at th ending point
    %diffusion(i,j)=dif_function(ks,c_s,a,n_vol,n_time,dx,i,j); 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if  (big_q(i,j)>=0)
        c_hat_front = c_bar_front(i);
        c_hat_back = c_bar_front(i-1);
    else
        c_hat_front = c_bar_back(i+1);%%%%%%%in flow comes reverse we should know the BC at the end!!!! and it does not work
        c_hat_back = c_bar-back(i);
    end
    
    c_half_front= c_hat_front; % + dt*(diffusion(i,j) + big_s)/(2*a(i+1,j));
    c_half_back= c_hat_back; % + dt*(diffusion(i,j) + big_s)/(2*a(i,j));
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Part two new c_s from c_bar_half
    a_c_bar_n_plus_one = (a(i+1,j)+a(i,j))*c_s(i,j)/2 -...
             dt*(big_q(i+1,j)*c_half_front-big_q(i,j)*c_half_back)/dx...
             + dt*big_s; %%%%%%%%%% not coded yet
    
   c_s(i,j+1)= (a(i+1,j)+a(i,j))*c_s(i,j)/2 -...
       dt*(big_q(i+1,j)*c_half_front-big_q(i,j)*c_half_back)/dx...
       + (dt/2)* (big_s + big_s);%%%%%%%%%%%%%%%%%%%%% big_s is not coded yet
   c_s(i,j+1)=2*c_s(i,j+1)/(a(i+1,j+1)+a(i,j+1));
         
    end %i
 end% march on j
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postprocess(c_s,n_time,n_vol)

