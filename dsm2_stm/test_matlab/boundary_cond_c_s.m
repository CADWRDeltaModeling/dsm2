function [c_s]=boundary_cond_c_s(n_time,c_s)

%     for j=1:(n_time/5 +1)
%                c_s(1,j)=sin((j-1)*pi/(n_time/5));
%     end
    
    for j=1:(n_time/5 +1)
               c_s(1,j)=1;
    end
          
return