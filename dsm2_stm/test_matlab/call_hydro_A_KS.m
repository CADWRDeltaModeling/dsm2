function [Area_n,Area_n_plus_one,K_s_n,K_s_n_plus_one]=call_hydro_A_KS(N)
    for i=1:N+1
        Area_n(i) = 1;
        Area_n_plus_one(i) = 1;
        K_s_n(i) = 1;
        K_s_n_plus_one(i) = 1; 
    end
return