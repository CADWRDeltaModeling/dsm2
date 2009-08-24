function dif_c_limited=flux_limiter(flux_limiter_type,i,c_s,j,dx,n_vol)

switch lower(flux_limiter_type)
    case  'van_leer'
        
    % if i==n_vol
    %     c_s_star=c_s(i,j);
    % else
    %     c_s_star=c_s(i+1,j);
    % end
    % del_r_cs=c_s_star-c_s(i,j);
    % del_c_cs=c_s_star-c_s(i-1,j);
    % del_l_cs=c_s(i,j)-c_s(i-1,j);
    % 
    % 
    %     delta= min(2*abs(del_r_cs),min(2*abs(del_l_cs),0.5*abs(del_c_cs)));
    %         
    %     if (del_r_cs*del_l_cs>0)
    %             dif_c_limited=delta*sign(del_c_cs);
    %     else
    %             dif_c_limited=0;
    %     end
    %     dif_c_limited=dif_c_limited/dx;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %         
    %         
       
    case 'none'

    if i==n_vol
        c_s_star=c_s(i,j);
    else
        c_s_star=c_s(i+1,j);
    end

    if i==1
        c_s_star_2=c_s(i,j);
    else
        c_s_star_2=c_s(i-1,j);
    end
        
          dif_c_limited= (c_s_star-c_s_star_2)/2/dx;
       
     
    otherwise 
        disp('Please mention flux limiter type; van Leer, HQUICK, Koren,or None')
        pause
end
return