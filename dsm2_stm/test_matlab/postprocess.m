function postprocess(c_s,n_time,n_vol)
      
    plot(c_s(1,1:n_time))
    hold on
    grid minor
    plot(c_s(floor(n_vol/6),1:n_time),'r')
    plot(c_s(floor(n_vol/3),1:n_time),'k')
    plot(c_s(floor(n_vol/2),1:n_time),'g')
    plot(c_s(floor(2*n_vol/3),1:n_time),'c')
    plot(c_s(floor(5*n_vol/6),1:n_time),'m')
    plot(c_s(n_vol,1:n_time),'y')
    title('Advection Lax two step method')
    ylabel('Concentration')
    xlabel('Time')
    
return