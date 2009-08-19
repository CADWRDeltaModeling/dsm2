function [C_s_n]=call_initial_c(i_case,N,dx_sed)

if(i_case==1) 
       for ak=1: (N - 1) / 2 + 1
         C_s_n(ak) = 1/0.5 * (ak - 1) * dx_sed;
       end
              
       for ak=(N - 1) / 2 + 2: N
         C_s_n(ak) = 2 - 1/0.5 * (ak - 1) * dx_sed;
       end
       C_s_n(N)=0;
end
     
if i_case==2
       for ak=1:N
         ax = (ak - 1) * dx_sed + 0.1;
         C_s_n(ak) = 2 * ax + 4 * cos(0.5 * pi * ax);
       end 
end 
return