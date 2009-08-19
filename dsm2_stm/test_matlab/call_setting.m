function [dt_sed,dx_sed,i_case,Theta,N_volumes,N,Cal_D,Cal_D_one,N_times]=call_setting()
N_volumes=21;
Theta = 0.5;
dt_sed = 0.001;
% FLAG i_case=1: B.C. Dirichlet_begin & Dirichlet_end
% FLAG i_case=2: B.C. Neumann_begin & Dirichlet_end
% FLAG i_case=3: B.C. Dirichlet_begin & Neumann_end
% FLAG i_case=4: B.C. Neumann_begin & Neumann_end
i_case=2 ;
if i_case==1
        dx_sed=0.05;
    elseif i_case==2
        dx_sed=0.045;
    elseif i_case==3
        dx_sed=0.45;
    elseif i_case==4
        dx_sed=0.045;
    else
        error('Please check B.C. Flag!')
end
N = N_volumes;
Cal_D = Theta * dt_sed / dx_sed /dx_sed;
Cal_D_one = (1 - Theta) * dt_sed / dx_sed / dx_sed;
N_times = 500;
return