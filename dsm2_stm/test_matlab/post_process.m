function post_process(dx_sed,N,N_times,result,dt_sed)

for i=1:N
    for j=1:N_times+1
        C_e(i,j)=2*((i-1)*dx_sed+.1)+4*cos(.5*pi*((i-1)*dx_sed+.1))*exp(-(j-1)*dt_sed*(pi^2)/4);
    end
end


plot(1:N,result(:,1))
hold on
plot(1:N,C_e(:,1),'r o')
axis([0 21 1 4.5])
xlabel('X (0.1 to 1.0)'); 
ylabel('C');
title('Comparision of C_ exact (red circle) and C_ Numerical (line)')


for i=1:5
plot(result(:,100*i+1))
hold on
plot(C_e(:,100*i+1),'r o')
end

error=abs(C_e-result);
norm_error=norm(error)


