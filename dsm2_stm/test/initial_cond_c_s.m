function [c_s]=initial_cond_c_s(c_s,n_vol)
%C=C(x,t) , C(x,0) defines here
for i=1:n_vol
    c_s(i,1)=0.0;
end
return