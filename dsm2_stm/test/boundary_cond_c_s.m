function [c_s]=boundary_cond_c_s(c_s,n_time)
    for j=1:n_time/5
                c_s(1,j)=1;
    end
return