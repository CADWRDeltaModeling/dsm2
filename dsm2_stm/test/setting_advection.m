function [dt,dx,n_vol,n_time,flux_limiter_type]=setting_advection()% read Q and A to calculate U for CFL
T=200;
L=100;
n_vol=100;
n_time=100000;
dt=T/n_time;
dx=L/n_vol;

%flux_limiter_types = van_Leer , none, Koren, HQUICK
flux_limiter_type='van_Leer';

%Courant Number Check
% Q./A.=U % dot is important here
% a = max(max(U))
%if (a>= dx/dt)
% error('Please reset dx and dt to reduce Courant number')
% pause
%end

return