function diffusion=dif_function(ks,c_s,a,n_vol,n_time,dx,i,j)   
diffusion=sparse(zeros(n_vol,n_time));
    if i<n_vol
        diffusion (i,j) = a(i+1,j)*((ks(i+1)+ks(i))/2)*(c_s(i+1,j)-c_s(i,j))-... 
                   a(i,j)*((ks(i)+ks(i-1))/2)*(c_s(i,j)-c_s(i-1,j));
        else
        diffusion (i,j)= a(i+1,j)*((ks(i)+ks(i))/2)*(c_s(i,j)-c_s(i,j))-... %%%%%%%%%%%%%%% repeated part ks(i+1)
                   a(i,j)*((ks(i)+ks(i-1))/2)*(c_s(i,j)-c_s(i-1,j));
    end
           
   diffusion= diffusion/dx/dx;
   
return
