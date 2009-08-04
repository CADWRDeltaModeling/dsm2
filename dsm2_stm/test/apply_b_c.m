
function  [U,BigK]=apply_b_c(Boundary_condition_flag_upstream,...
    Boundary_condition_flag_downstream,BigK,U,Area_n,Area_n_plus_one,K_s_n,...
    K_s_n_plus_one,C_s_downstream,C_s_upstream,Flux_s_upstream_n,...
    Flux_s_upstream_n_plus_one,dx_sed,Cal_D,...
    Cal_D_one,C_s_n,N)
          
                
   if Boundary_condition_flag_upstream==1
     BigK(1,1) = 1;
     BigK(1) = 0;
     U(N) = C_s_upstream;

   elseif Boundary_condition_flag_upstream==0
     BigK(1,1)=BigK(1,1);
     BigK(1,2)=BigK(1,2)-Cal_D*Area_n_plus_one(1)*K_s_n_plus_one(1);
     U(1)=U(1)-2*dx_sed*Flux_s_upstream_n_plus_one*Cal_D*... %%%%%%%%%%%%%%Flux_s_upstream_n_plus_one
         Area_n_plus_one(1)*K_s_n_plus_one(1);
    
   end

   if Boundary_condition_flag_downstream==1
     BigK(N,N)=1;
     BigK(N,N-1)=0;
     U(N) =C_s_downstream;
   elseif Boundary_condition_flag_downstream==0

   end    

return