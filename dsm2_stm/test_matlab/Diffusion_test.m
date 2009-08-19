% diffusion program
clear all
clc
%define variables 
dx= 1;
T=20;
dt=.1;
L=100;
theta= .0 ;% (between 0 and 1)
Num_x=L/dx;
Num_t=T/dt;
coef=dt/(dx^2);

BC_type=1;   %( D_D=1,D_N=2,N_N=3,N_D=4)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
BC_D_start=0
BC_D_end=0
BC_N_Start=1
BC_N_end=1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%i is a spase counter
%j is a time counter

%  A & Ks from HYDRO
for i=1:(Num_x+1)
    for j=1:Num_t+1
    A(i,j)=1;
    Ks(i,j)=.1;
    end
end 

for i=1:Num_x+1
    for j=1:Num_t+1
        AKs(i,j)=A(i,j)*Ks(i,j);
    end
end

% solving for K*X=U
% K dimention
K= sparse(zeros(Num_x));
%U and X  and result dimention
U=sparse(zeros(Num_x,1));
C_result=zeros(Num_x,Num_t);

% initializing c(1,j)?????????????????????????????????????????????

for i=1:Num_x/2+1
        C(i,1)=(2/L)*(i-1)*dx;
end

for i=Num_x/2+1:Num_x
    C(i,1)=2-(2/L)*(i-1)*dx;
end



for i=1:Num_x
C_result(i,1)=C(i,1);
end
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for j=1: Num_t %% counter on time
%making K
for i=1:Num_x
   K(i,i)= A(i,j+1)+A(i+1,j+1)/2 +coef*theta*(A(i+1,j+1)*Ks(i+1,j+1)+ A(i,j+1)*Ks(i,j+1));
    for i=1:Num_x-1%this for can merege in future 
    K(i,i+1)= coef*theta*A(i+1,j+1)*Ks(i+1,j+1);
    K(i+1,i)=coef*theta*A(i,j+1)*Ks(i,j+1);
    end
end
% End of making k
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% making u
U=zeros(10,1)

for i=2:Num_x-1 % First and last will be put by BC 
    U(i)=(C(i,j)+C(i+1,j))*(A(i,j)+A(i+1,j))/4+(1-theta)*coef*(AKs(i+1,j)*C(i+1,j)-AKs(i+1,j)*C(i,j)-AKs(i,j)*C(i,j)+AKs(i,j)*C(i-1,j));
     
end
% apply BC
if BC_type ==1
    % Begin
    U(1)=BC_D_start;
    K(1,2)=0;
    K(1,1)=1;
    % End
    U(Num_x)=BC_D_end;
    K(Num_x,Num_x)=1;
    K(Num_x,Num_x-1)=0;
    
% else if  BC_type==2 
%     % Begin
%     U(1)=BC_D_start
%     K(1,2)=0
%     K(1,1)=1
%     % End
%     
%     
% else if BC_type==3
%             % begin 
%             
%             
%             
%             
%             % end
%             
%             
%             
%   else 
%             %begin
%             
%             
%             
%             % end
%             
  end % if            

% solving for result

C_result(:,j+1)= (inv(K))*U;
for i=1:Num_x
        C(i,j+1)=C_result(i,j+1);
end

%puting in answer

 end %of j counter
%j counter on time
C_result



