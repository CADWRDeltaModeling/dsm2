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

! conc_plus_one(i): concentration of sediment in suspension averaged in the volume i
!      for time t + 1
! conc(i): concentration of sediment in suspension averaged in the volume
!         for time t
! k_s_plus_one(i): dispersion coefficient at the left face of volume i for time t + 1
! k_s(i): dispersion coefficient at the left face of volume i for time t
! area_plus_one(i): area of cross section evaluated at the left face of volume i at time
!                     t + 1. this is an output coming from hydro
! area(i): area of cross section evaluated at the left face of volume i at time
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

integer,parameter :: ncell = 26
integer,parameter :: ntime = 5000
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

real(stm_real) :: area(ncell+1,ntime), area_plus_one(ncell+1,ntime)! change the logic in area 
real(stm_real) :: k_s(ncell+1,ntime), k_s_plus_one(ncell+1,ntime)
real(stm_real) :: conc(ncell), conc_plus_one(ncell)
real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real) :: dt
real(stm_real) :: dx
real(stm_real) :: theta
real(stm_real) :: coef, coef_one
real(stm_real) :: time
real(stm_real) :: c_s_up, c_s_down
real(stm_real) :: ax
real(stm_real) :: flux_s_up_n, flux_s_up_n_plus_one, flux_s_down_n, flux_s_down_n_plus_one
real(stm_real) :: conc_result(ncell, ntime)
real(stm_real) :: conc_exact(ncell, ntime)
real(stm_real) :: error(ncell, ntime)

           
theta = 0.5d0
dt = 0.001d0/2.0d0
i_case = 2

if(i_case == 1)dx = 0.05d0
if(i_case == 2)dx = 0.9d0/(ncell-1)                        !0.045d0
if(i_case == 3) dx= 0.045d0
if(i_case == 4) dx= 0.045d0

coef = theta * dt / dx /dx
coef_one = (1 - theta) * dt / dx / dx

open(3,file='results.dat',status='unknown')


call initial_c(conc, &
                       i_case,ncell,dx)

marchontime :do jvar=1,ntime

      time = dt * jvar
      write(3,*)jvar,dt,time
      write(3,*)

      call read_hydro_area_ks(area,area_plus_one,k_s,k_s_plus_one,&
                                                                ncell,jvar,ntime)

      call bigk(a, b, c, d, &
                              ncell, coef, coef_one, area, area_plus_one, k_s, k_s_plus_one, conc,jvar,ntime)
                              
      call make_bc (c_s_up,c_s_down,flux_s_up_n,flux_s_up_n_plus_one,flux_s_down_n,flux_s_down_n_plus_one & 
                              ,i_case,bc_flag_up,bc_flag_down,time,dt)

      call impose_bc(a, b, c, d,&
                              bc_flag_up, bc_flag_down, ncell, c_s_up, c_s_down, coef, coef_one, area, k_s, area_plus_one, &
                              k_s_plus_one, flux_s_up_n, flux_s_up_n_plus_one, dx, conc,ntime,jvar)

      call tridi_solver(x & 
                        ,a, b, c, d, ncell)

   storeresult:  do ivar=1, ncell
             write(3,*)x(ivar)
             conc(ivar)=x(ivar)
             conc_result(ivar,jvar)=x(ivar)
       end do storeresult

end do marchontime

call exact_sol(error,conc_exact &  
                                    ,ntime,ncell,i_case,dx,dt,pi,conc_result)

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
real (stm_real) ::conc(ncell)

real (stm_real),parameter :: pi=3.14159265358979323846264338327950288

if(i_case == 1) then
do kvar=1, (ncell - 1) / 2 + 1
 conc(kvar) = two * (kvar - 1) * dx
end do
      
kvar_march: do kvar=(ncell - 1) / 2 + 2, ncell
 conc(kvar) = two - two * (kvar - 1) * dx
end do kvar_march



elseif(i_case == 2)then
kvar_march2: do kvar=1, ncell
 ax = (kvar - 1) * dx + 0.1d0
 conc(kvar) = two * ax + two*two * cos(0.5d0 * pi * ax)

     
end do kvar_march2

elseif (i_case == 3)then



elseif (i_case == 4) then

else
print *, 'Error! please recheck the boundary condition flag'  
end if

print *, 'Boundary condition was loaded'

return
end
!*************************************************************************************

subroutine read_hydro_area_ks(area,area_plus_one,k_s,k_s_plus_one,&
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

real(stm_real) :: area(ncell+1,ntime),area_plus_one(ncell+1,ntime),k_s(ncell+1,ntime),k_s_plus_one(ncell+1,ntime)

do ivar=1,ncell+1
   area(ivar,jvar) = one
   area_plus_one(ivar,jvar) = one
   k_s(ivar,jvar) = one
   k_s_plus_one(ivar,jvar) = one
end do

return
end

!*****************************************************************************************
subroutine bigk(a, b, c, d, &
                                ncell, coef, coef_one, area, area_plus_one, k_s, k_s_plus_one, conc,jvar,ntime)

integer,parameter:: stm_real=8
  
integer :: ncell,ntime 
integer ::ivar, jvar, kvar
  
real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real) :: area(ncell+1,ntime), area_plus_one(ncell+1,ntime)
real(stm_real) :: k_s(ncell+1,ntime), k_s_plus_one(ncell+1,ntime)
real(stm_real) :: conc(ncell), conc_plus_one(ncell)
real(stm_real) :: area_aux_n, area_aux_n_plus_one
real(stm_real) :: coef, coef_one
  
real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed
  
    
   do ivar=1,ncell
    area_aux_n_plus_one = (area_plus_one(ivar,jvar) + area_plus_one(ivar+1,jvar)) / two
    area_aux_n = (area(ivar,jvar) + area(ivar+1,jvar)) / two
    b(ivar)=area_aux_n_plus_one + coef * area_plus_one(ivar+1,jvar) * k_s_plus_one(ivar+1,jvar) 
    b(ivar)=b(ivar) + coef * area_plus_one(ivar,jvar) * k_s_plus_one(ivar,jvar) 
    a(ivar)= minus * coef * area_plus_one(ivar,jvar) * k_s_plus_one(ivar,jvar)
    c(ivar)= minus * coef * area_plus_one(ivar+1,jvar) * k_s_plus_one(ivar+1,jvar)
   
!       if (ivar == ncell) then 
!       concstar=conc(ivar)
!       else
!       concstar=conc(ivar+1) 
!       end if  
   
    d(ivar)=area_aux_n * conc(ivar) + coef_one * conc(ivar+1) * area(ivar+1,jvar) * k_s(ivar+1,jvar)
      d(ivar)= d(ivar) -  coef_one * conc(ivar) * area(ivar+1,jvar) * k_s(ivar+1,jvar)
    d(ivar)= d(ivar) -  coef_one * conc(ivar) * area(ivar,jvar) * k_s(ivar,jvar)
   
!        if (ivar == 1) then 
!        concstar2 =conc(ivar)
!        else
!        concstar2=conc(ivar-1)
!        end if 
   
       d(ivar)= d(ivar) +  coef_one * conc(ivar-1) * area(ivar,jvar) * k_s(ivar,jvar)
   end do
         
return
end


!*****************************************************************************************   


subroutine make_bc (c_s_up,c_s_down,flux_s_up_n,flux_s_up_n_plus_one,flux_s_down_n,flux_s_down_n_plus_one & 
                                                                                                    ,i_case,bc_flag_up,bc_flag_down,time,dt)

integer,parameter :: stm_real=8

integer ::i_case,bc_flag_up,bc_flag_down

real(stm_real),parameter :: pi=	3.14159265358979323846264338327950288

real(stm_real) :: minus = -1.d0    !< real constant -1. properly typed
real(stm_real) :: zero  =  0.d0    !< real constant  0. properly typed
real(stm_real) :: one   =  1.d0    !< real constant  1. properly typed
real(stm_real) :: two   =  2.d0    !< real constant  2. properly typed
real(stm_real) :: half   =  5.d-1  !< real constant  0.5 properly typed
real(stm_real) :: fourth =  2.5d-1 !< real constant  0.25 properly typed

real(stm_real)::c_s_up,c_s_down,flux_s_up_n,flux_s_up_n_plus_one,time,dt,flux_s_down_n,flux_s_down_n_plus_one

if(i_case == 1)then
   bc_flag_up = one
   bc_flag_down = one
   c_s_up = zero
   c_s_down = zero
end if

if(i_case == 2)then
   bc_flag_up = zero
   bc_flag_down = one
   flux_s_up_n = two - two * pi * sin(0.05d0 * pi) * exp(minus * pi * pi / 4.0d0 * (time - dt))
   flux_s_up_n_plus_one = two - two * pi * sin(0.05d0 * pi) * exp(minus * pi * pi / 4.0d0 * time)
   c_s_down = two
end if

return
end subroutine
!***************************************************************************************** 
subroutine impose_bc(a, b, c, d,&
                                     bc_flag_up, bc_flag_down, ncell, c_s_up, c_s_down, coef, coef_one, area, k_s, area_plus_one, k_s_plus_one, flux_s_up_n, flux_s_up_n_plus_one, dx, conc,ntime,jvar)

integer,parameter:: stm_real=8
  
integer :: ncell,ntime
integer ::jvar 
integer :: bc_flag_up
integer :: bc_flag_down
  
real(stm_real):: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real):: area(ncell+1,ntime), area_plus_one(ncell+1,ntime)
real(stm_real):: k_s(ncell+1,ntime), k_s_plus_one(ncell+1,ntime)
real(stm_real) :: c_s_up, c_s_down
real(stm_real) :: flux_s_up_n, flux_s_up_n_plus_one
real(stm_real) :: dx
real(stm_real) :: coef, coef_one
real(stm_real) :: conc(ncell), conc_plus_one(ncell)
   
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
     b(1) = b(1)
     c(1) = c(1) - coef * area_plus_one(1,jvar) * k_s_plus_one(1,jvar)
     d(1) = d(1) - two * dx * flux_s_up_n_plus_one * coef * area_plus_one(1,jvar) * k_s_plus_one(1,jvar)
     d(1) = d(1) - two * dx * flux_s_up_n * coef_one * area(1,jvar) * k_s(1,jvar)
     d(1) = d(1) + coef_one * area(1,jvar) * k_s(1,jvar) * conc(2) 
   end if
   
   if(bc_flag_down == 1)then
     b(ncell)= one
     a(ncell)= zero
     d(ncell) = c_s_down
   end if

   if(bc_flag_down == 0)then
        
   end if
   

return
end

!*****************************************************************************************    
subroutine tridi_solver(x & 
                    ,a, b, c, d, ncell)


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
else 
 print *, 'there is no exact solution for this case' 
end if

do ivar2=1,ncell
 do jvar2=1,ntime
    error(ivar2,jvar2)= abs(conc_exact(ivar2,jvar2)-conc_result(ivar2,jvar2))/conc_exact(ivar2,jvar2)
 end do
end do

!open(4,file='error.dat',status='unknown')
open (100,file= 'errorcomp.dat',status= 'unknown')
    write (100,*) ' Number of volumes are    ' , ncell-1
    write (100,*) '    X     -', '      error percentage    -   ' , '         abs (Uexact-Unum) error'
do kvar=1,ntime
!    write (4,*) 'step ',  kvar
!    write (4,*) 
!    write (4,*) error (:,kvar)
    
    write (100,*) kvar,'time'
    write (100,*) kvar*dt
    do ivar2=1,ncell
    write (100,*)  ((ivar2-1)*dx+1.0d-1), error (ivar2,kvar), error (ivar2,kvar)*conc_exact(ivar2,jvar2)
   end do
    
end do

return
end
!**************************************************************************************************
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
!***********************************************************************************************************
