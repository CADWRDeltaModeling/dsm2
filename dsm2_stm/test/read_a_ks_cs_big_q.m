function [a,big_q,c_s,ks]=read_a_ks_cs_big_q(n_vol,n_time)

        a=ones(n_vol+1,n_time+1);
        big_q=ones(n_vol+1,n_time+1);
   
        c_s= zeros(n_vol,n_time+1);
        ks=ones(n_vol,n_time+1)./200;
   

return