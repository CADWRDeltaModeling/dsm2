program diffusion

! copyright (c) 1996, 1997, 1998, 2001, 2007, 2009 state of california,
! department of water resources.
! this file is part of dsm2.

! the delta simulation model 2 (dsm2) is free software: 
! you can redistribute it and/or modify
! it under the terms of the gnu general public license as published by
! the free software foundation, either version 3 of the license, or
! (at your option) any later version.

! dsm2 is distributed in the hope that it will be useful,
! but without any warranty; without even the implied warranty of
! merchantability or fitness for a particular purpose.  see the
! gnu general public license for more details.

! you should have received a copy of the gnu general public license
! along with dsm2.  if not, see <http://www.gnu.org/licenses>.

! delta modeling section 
! modeling support branch
! bay-delta office
! california department of water resources
! http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/index.cfm
!
! purpose:
!
! it solves the dispersion of sediment in the longitudinal direction
! partial_(a c_s)/partial_t = partial/partial_x [a k_s partial_(c_s)/partial_x]
! 
! these sub-routines are being developed by the department of civil and 
! environmental engineering at the university of california, davis and dwr
!
! record of revisions:
!       date             programmers                 description of change
!       ====             ===========                 =====================
!       09/17/09        kaveh zamani                  rewrite and clean up  
!****************************************************************************

! variables: data dictionary.  define variables types, definitions and units

! conc(i): concentration of sediment in suspension averaged in the volume
!         for time t
! ks_lo(i): dispersion coefficient at the left face of volume i for time t 
! ks_hi(i): dispersion coefficient at the right face of volume i for time t
! area_lo(i): area of cross section low side evaluated at the left face of volume i at time
!                     t. this is an output coming from hydro
! area_hi(i): area of cross section evaluated at the right face of volume i at time
!            t. this is an output coming from hydro
! a(i): values of the coefficients below diagonal in matrix
! b(i): values of the coefficients at the diagonal in matrix
! c(i): values of the coefficients above diagonal in matrix
! d(i): values of the independent term
! ncell: number of volumes (computational cross sections) whose
!          sediment concentration will be computed
! dt: time step for the computation of sediment transport (in seconds)
! dx: spatial step for the computation of sediment transport (in feet for stm)  

implicit none

integer,parameter :: ncell = 7169
integer,parameter :: ntime = 3202
integer,parameter:: stm_real=8

real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed

integer :: ivar, jvar, kvar, ivar2, jvar2 
integer:: i_case
integer :: bc_flag_up
integer :: bc_flag_down

real(stm_real), parameter :: pi = 3.14159265358979323846264338327950288

real(stm_real) :: area_lo(ncell,ntime+1), area_hi(ncell,ntime+1)
real(stm_real) :: ks_lo(ncell,ntime+1), ks_hi(ncell,ntime+1)


real(stm_real) :: conc(ncell)
real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real) :: dt
real(stm_real) :: dx
real(stm_real) :: theta
real(stm_real) :: coef, coef_one
real(stm_real) :: time
real(stm_real) :: c_s_up, c_s_down
real(stm_real) :: ax
real(stm_real) :: flux_up_n, flux_up_n_plus_one, flux_down_n, flux_down_n_plus_one
real(stm_real) :: conc_result(ncell, ntime)
real(stm_real) :: conc_exact(ncell, ntime)
real(stm_real) :: error(ncell, ntime)
real(stm_real) :: L1,L2,L_infinity

           
theta = 1.0d0
dt = 0.001d0 
i_case = 3   

if(i_case == 1)dx = 0.05d0
if(i_case == 2)dx = 0.9d0/(ncell-1)             
if(i_case == 3) dx= 0.015625d0  !!!!!!!!!!!!!!! 100.0d0/(ncell-1)
if(i_case == 4) dx= 0.045d0




coef = theta * dt / dx /dx
coef_one = (1 - theta) * dt / dx / dx

open(3,file='results.dat',status='unknown')
open(4,file='resultsevery100step.dat',status='unknown')

call initial_c(conc, &
                       i_case,ncell,dx)  

do jvar=1,ntime

            time = dt * jvar
            write (3,*) 
            write(3,*)jvar,dt,time
           
            call read_hydro_area_ks(area_lo,area_hi,ks_lo,ks_hi,&
                                                    ncell,jvar,ntime)

            call bigk(a, b, c, d, &
                          ncell, coef, coef_one, area_lo, area_hi, ks_lo, ks_hi, conc,jvar,ntime)                                                                        
                                                       

            call make_bc (c_s_up,c_s_down,flux_up_n,flux_up_n_plus_one,flux_down_n,flux_down_n_plus_one & 
                          ,i_case,bc_flag_up,bc_flag_down,time,dt)


            call impose_bc(a, b, c, d,&
                          bc_flag_up, bc_flag_down, ncell, c_s_up, c_s_down, coef, coef_one, area_lo, ks_lo, area_hi, &
                          ks_hi, flux_up_n, flux_up_n_plus_one, dx, conc,ntime,jvar)


            call tridi_solver(x & 
                        ,a, b, c, d, ncell)

            do ivar=1, ncell
                 write(3,*) jvar*dt,((1-ncell)/2+ivar-1)*dx,x(ivar)
                 conc(ivar)=x(ivar)
                conc_result(ivar,jvar)=x(ivar)
                 
                 if (mod(jvar,100) == 0) then
                      if (abs(((1-ncell)/2+ivar-1)*dx)<= 7) then
                        write (4,*) jvar*dt,((1-ncell)/2+ivar-1)*dx,x(ivar)
                      end if
                 end if
                 
            end do 

end do 

!call exact_sol(error,conc_exact &  
                        !          ,ntime,ncell,i_case,dx,dt,pi,conc_result)
                                   
!call norm_calculator ( L1, L2 ,L_infinity &
                                           !,ntime ,ncell, error, conc_exact,conc_result,dx,dt,theta)

end program diffusion

!*****************************************************************************************
subroutine initial_c(conc, &
                            i_case,ncell,dx)

                               
integer :: i_case
integer :: ncell
integer :: kvar

integer,parameter :: stm_real=8

real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed

real (stm_real) :: ax,dx
real (stm_real) :: conc(ncell)

real (stm_real),parameter :: pi=3.14159265358979323846264338327950288

if(i_case == 1) then  
do kvar=1, (ncell - 1) / 2 + 1
 conc(kvar) = two * (kvar - 1) * dx
end do
      
 do kvar=(ncell - 1) / 2 + 2, ncell
 conc(kvar) = two - two * (kvar - 1) * dx
end do 



elseif(i_case == 2)then  
 do kvar=1, ncell
 ax = (kvar - 1) * dx + 0.1d0
 conc(kvar) = two * ax + two*two * cos(0.5d0 * pi * ax)

     
end do 

elseif (i_case == 3)then
do kvar =1 , ncell
conc(kvar)= zero
end do
conc(kvar/2)=1.0d0/dx


elseif (i_case == 4) then


else



print *, 'Error! please recheck the boundary condition flag'  
end if

!print *, 'Initial condition was loaded'

return
end
!*************************************************************************************

 subroutine read_hydro_area_ks(area_lo,area_hi,ks_lo,ks_hi,&
                                                               ncell,jvar,ntime)                                                                  

integer,parameter :: stm_real=8

real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed

integer :: ncell,ntime
integer :: ivar, jvar

real(stm_real) :: area_lo(ncell,ntime+1),area_hi(ncell,ntime+1)
real(stm_real) :: ks_lo(ncell,ntime+1),ks_hi(ncell,ntime+1)


do ivar=1,ncell 

    area_lo(ivar,jvar) = one
    area_hi(ivar,jvar) = one
    
    area_lo(ivar,jvar+1) = one
    area_hi(ivar,jvar+1) = one
   
end do

do ivar=1,ncell+1

   ks_lo(ivar,jvar) = two
   ks_hi(ivar,jvar) = two
   
   ks_lo(ivar,jvar+1) = two
   ks_hi(ivar,jvar+1) = two
  
end do

!print *, 'Area and KS were loaded from Hydro time step',jvar

return
end

!*****************************************************************************************
                              
  subroutine  bigk(a, b, c, d, &
                              ncell, coef, coef_one, area_lo, area_hi, ks_lo, ks_hi, conc,jvar,ntime)


integer,parameter:: stm_real=8
  
integer :: ncell,ntime 
integer ::ivar, jvar, kvar
  
real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real) :: area_lo(ncell,ntime+1), area_hi(ncell,ntime+1)

real(stm_real) :: ks_lo(ncell,ntime+1), ks_hi(ncell,ntime+1)
real(stm_real) :: conc(ncell)
real(stm_real) :: area_aux, area_aux_plus_one

real(stm_real) :: coef, coef_one
  
real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed
  
    
    do ivar = 2, ncell -1
  
        area_aux_plus_one = (area_lo(ivar,jvar+1) + area_hi(ivar,jvar+1)) / two
        area_aux = (area_lo(ivar,jvar) + area_hi(ivar,jvar)) / two

        b(ivar)=area_aux_plus_one + coef * area_hi(ivar,jvar+1) * ks_hi(ivar,jvar+1) 

        b(ivar)=b(ivar) + coef * area_lo(ivar,jvar+1) * ks_lo(ivar,jvar+1) 

        a(ivar)= minus * coef * area_lo(ivar,jvar+1) * ks_lo(ivar,jvar+1)

        c(ivar)= minus * coef * area_hi(ivar,jvar+1) * ks_hi(ivar,jvar+1)    

        d(ivar)=area_aux * conc(ivar) + coef_one * conc(ivar+1) * area_hi(ivar,jvar) * ks_hi(ivar,jvar) 

        d(ivar)= d(ivar) -  coef_one * conc(ivar) * area_hi(ivar,jvar) * ks_hi(ivar,jvar)

        d(ivar)= d(ivar) -  coef_one * conc(ivar) * area_lo(ivar,jvar) * ks_lo(ivar,jvar)

        d(ivar)= d(ivar) +  coef_one * conc(ivar-1) * area_lo(ivar,jvar) * ks_lo(ivar,jvar)
       
    
   end do
         
return
end


!*****************************************************************************************   

subroutine make_bc (c_s_up,c_s_down,flux_up_n,flux_up_n_plus_one,flux_down_n,flux_down_n_plus_one & 
                                                                                                    ,i_case,bc_flag_up,bc_flag_down,time,dt)

integer,parameter :: stm_real=8

integer ::i_case ,bc_flag_up,bc_flag_down

real(stm_real),parameter :: pi=	3.14159265358979323846264338327950288

real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed

real(stm_real)::c_s_up,c_s_down,flux_up_n,flux_up_n_plus_one,time,dt,flux_down_n,flux_down_n_plus_one

if(i_case == 1)then
   bc_flag_up = 1
   bc_flag_down =1
   c_s_up = zero
   c_s_down = zero


elseif(i_case == 2)then
   bc_flag_up = 0
   bc_flag_down = 1
   flux_up_n = two - two * pi * sin(0.05d0 * pi) * exp(minus * pi * pi / 4.0d0 * (time - dt))
   flux_up_n_plus_one = two - two * pi * sin(0.05d0 * pi) * exp(minus * pi * pi / 4.0d0 * time)
   c_s_down = two
elseif (i_case == 3)then

bc_flag_up = 1 !!!!
bc_flag_down = 1 !!!!

c_s_up = zero
c_s_down = zero
 
 flux_up_n = zero
 flux_up_n_plus_one = zero
 flux_down_n = zero 
 flux_down_n_plus_one = zero


else

print *, 'please check the test case flag'

end if

!print *,'BC was made in the time ', time


return
end subroutine
!***************************************************************************************** 
subroutine impose_bc(a, b, c, d,&
                              bc_flag_up, bc_flag_down, ncell, c_s_up, c_s_down, coef, coef_one, area_lo, ks_lo, area_hi, &
                              ks_hi, flux_up_n, flux_up_n_plus_one, dx, conc,ntime,jvar)
               

integer,parameter:: stm_real=8
  
integer :: ncell,ntime
integer ::jvar 
integer :: bc_flag_up
integer :: bc_flag_down
  
real(stm_real):: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)

real(stm_real):: area_lo(ncell,ntime), area_hi(ncell,ntime)


real(stm_real) ::area_aux_plus_one,area_aux
real(stm_real) :: ks_lo(ncell,ntime+1), ks_hi(ncell,ntime+1)
real(stm_real) :: c_s_up, c_s_down
real(stm_real) :: flux_up_n, flux_up_n_plus_one
real(stm_real) :: dx
real(stm_real) :: coef, coef_one
real(stm_real) :: conc(ncell)
   
real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed
   
  
   if(bc_flag_up == 1)then
     b(1) = one
     c(1) = zero
     d(ncell) = c_s_up
   end if

   if(bc_flag_up == 0)then
   
   
        area_aux_plus_one = (area_lo(1,jvar+1) + area_hi(1,jvar+1)) / two
        area_aux = (area_lo(1,jvar) + area_hi(1,jvar)) / two

        b(1)=area_aux_plus_one + coef * area_hi(1,jvar+1) * ks_hi(1,jvar+1) 

        b(1)=b(1) + coef * area_lo(1,jvar+1) * ks_lo(1,jvar+1) 

        a(1)= minus * coef * area_lo(1,jvar+1) * ks_lo(1,jvar+1)

        c(1)= minus * coef * area_hi(1,jvar+1) * ks_hi(1,jvar+1)    

        d(1)=area_aux * conc(1) + coef_one * conc(2) * area_hi(1,jvar) * ks_hi(1,jvar) 

        d(1)= d(1) -  coef_one * conc(1) * area_hi(1,jvar) * ks_hi(1,jvar)

        d(1)= d(1) -  coef_one * conc(1) * area_lo(1,jvar) * ks_lo(1,jvar)

        d(1)= d(1) +  coef_one * (conc(2)- two * dx * flux_up_n) * area_lo(1,jvar) * ks_lo(1,jvar)
   
        
    c(1) = c(1) - coef * area_lo(1,jvar+1) * ks_lo(1,jvar+1)
     
    d(1) = d(1) - two * dx * flux_up_n_plus_one * coef * area_lo(1,jvar+1) * ks_lo(1,jvar+1)
      
   
   end if
   
   if(bc_flag_down == 1)then
     a(ncell)= zero
     b(ncell)= one
     d(ncell) = c_s_down
   end if

   if(bc_flag_down == 0)then
  
  
   area_aux_plus_one = (area_lo(ncell,jvar+1) + area_hi(ncell,jvar+1)) / two
        area_aux = (area_lo(ncell,jvar) + area_hi(ncell,jvar)) / two

        b(ncell)=area_aux_plus_one + coef * area_hi(ncell,jvar+1) * ks_hi(ncell,jvar+1) 

        b(ncell)=b(ncell) + coef * area_lo(ncell,jvar+1) * ks_lo(ncell,jvar+1) 

        a(ncell)= minus * coef * area_lo(ncell,jvar+1) * ks_lo(ncell,jvar+1)

        c(ncell)= minus * coef * area_hi(ncell,jvar+1) * ks_hi(ncell,jvar+1)    

        d(ncell)=area_aux * conc(ncell) + coef_one * (conc(ncell-1) + two* dx * flux_down_n) * area_hi(ncell,jvar) * ks_hi(ncell,jvar) !!!!!!!!!!!!!conc(ncell+1)

        d(ncell)= d(ncell) -  coef_one * conc(ncell) * area_hi(ncell,jvar) * ks_hi(ncell,jvar)

        d(ncell)= d(ncell) -  coef_one * conc(ncell) * area_lo(ncell,jvar) * ks_lo(ncell,jvar)

        d(ncell)= d(ncell) +  coef_one * conc(ncell-1) * area_lo(ncell,jvar) * ks_lo(ncell,jvar)
  
  a(ncell)=a(ncell)+  minus * coef * area_hi(ncell,jvar+1) * ks_hi(ncell,jvar+1)
  
  d(ncell)=d(ncell) + two * dx * flux_down_n_plus_one * coef * area_hi(1,jvar+1) * ks_hi(1,jvar+1)
  
  
  
     
   end if
   
!Print *, 'BC imposed on BIGK matrix'


return
end

!*****************************************************************************************    
subroutine tridi_solver(x & 
                    ,a, b, c, d, ncell)
! PURPOSE:

! Solves a tridiagonal system for X. This sub-routine was taken from the Numerical Recipes,
! page 43 (Edition of 1992)
! 
!             [B1,C1,00,00,00]  
! [X1,X2,...] [A2,B2,C2,00,00] = [D1,D2,....]
!             [00,A3,B3,C3,00]  
!             [00,00,A4,B4,C4]
!             [00,00,00,A5,B5]    

! Variables:
!
! A(i): Values of the coefficients below diagonal in matrix
! B(i): Values of the coefficients at the diagonal in matrix
! C(i): Values of the coefficients above diagonal in matrix
! D(i): Values of the independent term 
! X(i): Values of the computed solution

integer,parameter:: stm_real=8


integer :: ncell
integer :: ivar

real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real) :: gam(ncell)
real(stm_real) :: bet

if(b(1) == 0)then
  print *, 'there is a problem here'
  stop
end if

bet = b(1)
x(1) = d(1) / bet

do ivar=2, ncell
  gam(ivar) = c(ivar - 1) / bet
  bet = b(ivar) - a(ivar) * gam(ivar)
  if(bet == 0)then
    print *, 'tridiagonal solver failed'
  end if
  x(ivar) = (d(ivar) - a(ivar) * x(ivar - 1)) / bet 
end do

do ivar= ncell-1, 1, -1
  x(ivar) = x(ivar) - gam(ivar + 1) * x(ivar + 1)
end do

return
end 

!*******************************************************************************************
subroutine exact_sol(error,conc_exact &  
                                    ,ntime,ncell,i_case,dx,dt,pi,conc_result)

integer :: i_case
integer :: ntime
integer :: ncell
integer :: ivar,jvar,kvar

integer,parameter:: stm_real=8

real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed

real(stm_real) :: conc_exact(ncell,ntime)
real(stm_real) :: error(ncell,ntime)
real(stm_real) :: conc_result(ncell,ntime)
real(stm_real) ::pi,dx,dt



if (i_case==2) then
do ivar=1,ncell
    do jvar=1,ntime
        conc_exact(ivar,jvar)=two*((ivar-1)*dx+1.0d-1)+two*two*cos(half*pi*((ivar-1)*dx+1.0d-1))*exp(-(jvar)*dt*(pi**2.0d0)/4.0d0)
    end do
end do

elseif (i_case == 3) then
     do ivar=1,ncell
        do jvar=1,ntime
        conc_exact(ivar,jvar)= (1.0d0/dx)*exp(minus*((((1-ncell)/2+ivar-1)*dx)**2))/sqrt(4*pi*jvar*dt)
        end do
     end do 
else 

 print *, 'there is no exact solution for this case' 
end if

do ivar2=1,ncell
 do jvar2=1,ntime
    error(ivar2,jvar2)= abs(conc_exact(ivar2,jvar2)-conc_result(ivar2,jvar2))/conc_exact(ivar2,jvar2)
 end do
end do


open (100,file= 'errorcomp.dat',status= 'unknown')
    write (100,*) ' Number of volumes are    ' , ncell-1
    write (100,*) '    X     -', '      error percentage    -   ' , '         abs (Uexact-Unum) error'
do kvar=1,ntime
    write (100,*) kvar,'time'
    write (100,*) kvar*dt
    do ivar2=1,ncell
    write (100,*)  ((ivar2-1)*dx+1.0d-1), error (ivar2,kvar), error (ivar2,kvar)*conc_exact(ivar2,jvar2)
   end do
    
end do

return
end
!**************************************************************************************************
subroutine norm_calculator ( L1, L2 ,L_infinity &
                                           ,ntime ,ncell, error, conc_exact,conc_result,dx,dt,theta)

integer :: ntime
integer :: ncell
integer :: ivar,jvar
integer :: aa(2)

integer,parameter:: stm_real=8

real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed

real(stm_real) :: error(ncell, ntime)
real(stm_real) :: conc_exact(ncell,ntime)
real(stm_real) :: conc_result (ncell,ntime)
real(stm_real) :: L1,L2,L_infinity  
real(stm_real) :: theta,dx,dt


L1=zero
L2=zero

do ivar=1,ncell
        do jvar=1,ntime
            error(ivar,javr)= conc_result(ivar,jvar)-conc_exact(ivar,jvar)
            L1= L1 + error(ivar,javr)
            L2= L2 + error(ivar, jvar)**2
            
        end do
end do

L_infinity = maxval (error)
aa= maxloc(error)
L1= L1/ncell/ntime
L2= sqrt(L2)/ncell/ntime


open(7,file='Norms.dat',status='unknown')

write (7,*) 'Short report of the run'
Write (7,*) '======================='
Write (7,*) 'Number of Volumes are :',ncell-1 
Write (7,*) 'Number of time steps are :',ntime
Write (7,*) 'Theta =',theta
Write (7,*) 'dt/dx^2 = ', dt/(dx*dx) 
Write (7,*) 'L-1 = ',L1
Write (7,*) 'L_2 = ',L2
Write (7,*) 'L_infinity = ' ,L_infinity 
write (7,*) 
Write (7,*) 'Location of Maximum error is volume' , aa(1)
Write (7,*) 'Time of Maximum error is itration number' ,aa(2)
                                         
                                           
return
end subroutine                                            
!*************************************************************************************************
!subroutine stm_precision(stm_real,minus,zero,one,two,half,fourth)
!
!!> precision of real
!integer, parameter :: stm_real=8
!
!real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
!real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
!real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
!real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
!real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
!real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed
!
!!> absurd high value, for initialization and for marking undefined
!!> data in calculations. this makes bugs easier to spot.
!real(stm_real) :: largereal = 1.23456789d8
!
!return
!end 
!******************************************************************************************************
