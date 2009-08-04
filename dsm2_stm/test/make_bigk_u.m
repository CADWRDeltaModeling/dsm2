function [BigK,U]=make_bigk_u(N,Area_n_plus_one,Area_n,Cal_D,...
    Cal_D_one,K_s_n_plus_one,K_s_n,C_s_n,Flux_s_upstream_n,dx_sed)
 
%  Variables: data dictionary.  Define variables types, definitions and units
%  Area_aux_n: Area evaluated at the center of the volume i at time t
%  Areas_aux_n_plus_one: Area evaluated at the center of the volume i at time t + 1
%  A(i): Values of the coefficients below diagonal in matrix
%  B(i): Values of the coefficients at the diagonal in matrix
%  C(i): Values of the coefficients above diagonal in matrix
%  U(i): Values of the independent term

BigK=zeros(N);
U=zeros(N,1);

for i=1:N
    Area_aux_n_plus_one=(Area_n_plus_one(i)+Area_n_plus_one(i+1))/2.0d0;
    Area_aux_n=(Area_n(i) + Area_n(i+1)) / 2;
    B(i)=Area_aux_n_plus_one+Cal_D*Area_n_plus_one(i+1)*K_s_n_plus_one(i+1);
    B(i)=B(i) + Cal_D * Area_n_plus_one(i) * K_s_n_plus_one(i) ;
    A(i)= -1.0d0*Cal_D * Area_n_plus_one(i) * K_s_n_plus_one(i);
    C(i)= -1.0d0*Cal_D * Area_n_plus_one(i+1) * K_s_n_plus_one(i+1);
end 

for i=1:N-1
    Area_aux_n_plus_one=(Area_n_plus_one(i)+Area_n_plus_one(i+1))/2;
    Area_aux_n=(Area_n(i) + Area_n(i+1)) / 2;
    D(i)=Area_aux_n*C_s_n(i)+Cal_D_one*C_s_n(i+1)*Area_n(i+1)*K_s_n(i+1);
    D(i)= D(i)-Cal_D_one * C_s_n(i) * Area_n(i+1) * K_s_n(i+1);
    D(i)= D(i)-Cal_D_one * C_s_n(i) * Area_n(i) * K_s_n(i);
    if i==1
    C_s_n_star= C_s_n(2)-2*dx_sed*Flux_s_upstream_n;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%watch
    D(i)= D(i) + Cal_D_one*C_s_n_star*Area_n(i)*K_s_n(i);%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%C_s_n(i-1)=C_s_n_star
    else
    D(i)= D(i)+ Cal_D_one * C_s_n(i-1) * Area_n(i) * K_s_n(i);
    end
end

for i=1:N
    BigK(i,i)=B(i);
end



for i=1:N-1
BigK(i,i+1)=C(i);
BigK(i+1,i)=A(i);
U(i)=D(i);%%%%%%%%%%%%%%% may be inverse
end

return