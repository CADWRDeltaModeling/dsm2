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
module diffusion

use stm_precision
!use allocation
use initialcondition
use readhydrodata
use coefmatrix
use exactsolution
use imposeboundarycondition
use makeboundarycondition
use matrixsolver




contains
subroutine diffusion_sub
implicit none


integer,parameter :: ncell = 97
integer,parameter :: ntime = 5000
integer,parameter:: stm_real=8


integer :: ivar, jvar, kvar, ivar2, jvar2 
integer:: i_case
integer :: bc_flag_up
integer :: bc_flag_down


real(stm_real) :: area(ncell+1,ntime), area_plus_one(ncell+1,ntime)
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

!call allocationdiff

call initial_c(conc, &
                       i_case,ncell,dx)

open(3,file='results.dat',status='unknown')

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


 return
        end subroutine diffusion_sub
end module diffusion
