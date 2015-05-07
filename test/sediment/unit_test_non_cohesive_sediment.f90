!<license>
!    Copyright (C) 2015 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>
!> Tests the suspended non cohesive sediment subroutines
!>@ingroup test_sediment
module test_non_cohesive

contains

!> Tests Einsstein's first integral 
! todo: incase the main subroutine replaced somewhere else this counterpart should place in the correct test package
subroutine test_first_einstein_integral

use fruit
use non_cohesive_source
use gtm_precision

implicit none
!---args
integer,parameter :: ncell = 1                 !< Number of volumes              
integer,parameter :: nclass =1                !< Number of sediment grain classes
real(gtm_real)  :: rouse(ncell,nclass)         !< Rouse dimenssionless number  
real(gtm_real)  :: delta                      !< Relative bed layer thickness = b/H 
real(gtm_real)  :: J_1(ncell,nclass)           !< First Einstein integral value  

!--- local
real(gtm_real)  :: hand_calc_value


delta = 0.01d0
rouse = 0.1d0
hand_calc_value = 0.630990839362793d0 !MATLAB calculation
 
call first_einstein_integral(J_1,      &
                             delta,  &
                             rouse,&
                             ncell,     & 
                             nclass)  
                                       
call assertEquals(hand_calc_value,J_1(1,1),weak_eps,"Error in subroutine first Einstein integral!")

rouse = 0.7d0
hand_calc_value = 0.075646372654714d0 !MATLAB calculation
 
call first_einstein_integral(J_1,      &
                             delta,  &
                             rouse,&
                             ncell,     &
                             nclass)   
                                      
call assertEquals(hand_calc_value,J_1(1,1),weak_eps,"Error in subroutine first Einstein integral!")

rouse = 1.7d0
hand_calc_value = 0.011612330444738d0 !MATLAB calculation
 
call first_einstein_integral(J_1,      &
                             delta,  &
                             rouse,&
                             ncell,     &
                             nclass)
                                       
call assertEquals(hand_calc_value,J_1(1,1),weak_eps,"Error in subroutine first Einstein integral!")

rouse = 2.7d0
hand_calc_value = 0.005925241451994d0 !MATLAB calculation
 
call first_einstein_integral(J_1,      &
                             delta,  &
                             rouse,&
                             ncell,     & 
                             nclass)  
                                       
call assertEquals(hand_calc_value,J_1(1,1),weak_eps,"Error in subroutine first Einstein integral!")


rouse = one
hand_calc_value = 0.03651687056d0 !MATLAB calculation
 
call first_einstein_integral(J_1,      &
                             delta,  &
                             rouse,&
                             ncell,     &
                             nclass) 
                                       
call assertEquals(hand_calc_value,J_1(1,1),weak_eps,"Error in subroutine first Einstein integral integer=1!")


rouse = two
hand_calc_value =   0.009262285443120d0 !MATLAB calculation
 
call first_einstein_integral(J_1,      &
                             delta,  &
                             rouse,&
                             ncell,     & 
                             nclass)  
                                       
call assertEquals(hand_calc_value,J_1(1,1),weak_eps,"Error in subroutine first Einstein integral integer=2!")

rouse = three
hand_calc_value =  0.004859662341771d0 !MATLAB calculation
 
call first_einstein_integral(J_1,      &
                             delta,  &
                             rouse,&
                             ncell,     & 
                             nclass)  
                                       
call assertEquals(hand_calc_value,J_1(1,1),weak_eps,"Error in subroutine first Einstein integral integer=3!")




return
end subroutine

subroutine test_es_garcia_parker()

use fruit
use non_cohesive_source
use gtm_precision

integer,parameter :: ncell = 3                  !< Number of computational volumes in a channel
integer,parameter :: nclass = 2                !< Number of non-cohesive sediment grain classes
real(gtm_real) :: e_s(ncell,nclass)             !< Dimenssionless rate of entrainment of bed sediment into suspension 
real(gtm_real) :: shear_v(ncell)                !< Shear Velocity
real(gtm_real) :: exp_re_p(nclass)             !< Explicit particle Reynolds number
real(gtm_real) :: settling_v(nclass)           !< Settling velocity
!---local
real(gtm_real) :: hand_calc_value(ncell,nclass)
integer :: ivol

shear_v =[0.1d0,0.4d0,one]
exp_re_p =[two,ten]
settling_v = [0.001d0,0.1d0]

hand_calc_value = reshape ([0.29995136236d0,	0.29999995249d0,	0.29999999951d0, &
                            0.00012994369d0,	0.09220539342d0,	0.29323308271d0],[3,2])


call es_garcia_parker(e_s,         &
                      shear_v,     &
                      exp_re_p,    &
                      settling_v,  & 
                      nclass,      &
                      ncell)
                      
do ivol=1,ncell
  call assertEquals(hand_calc_value(ivol,1),e_s(ivol,1),weak_eps,"Error in subroutine es_garcia_parker")
  call assertEquals(hand_calc_value(ivol,2),e_s(ivol,2),weak_eps,"Error in subroutine es_garcia_parker")
end do 




return
end subroutine

!> Example spatial and time variables that prints an error and bails
 subroutine example_spatiotemporal_data_sediment(velocity,  &
                                                 depth,     &
                                                 ncell,     &
                                                 time,      &
                                                 dx,        &
                                                 dt)
     use gtm_precision
     use error_handling
     implicit none
     
        integer, intent(in) :: ncell                    !< Number of cells (in)
        real(gtm_real), intent(in)  :: time             !< Time of request (in)
        real(gtm_real), intent(in)  :: dx               !< Spatial step (in)
        real(gtm_real), intent(in)  :: dt               !< Time step  (in)
        real(gtm_real), intent(out) :: velocity(ncell)  !< Cell and time centered velocity (out)
        real(gtm_real), intent(out) :: depth(ncell)     !< Cell center depth (out)
        ! just to avoid warning
        velocity = LARGEREAL
        depth = minus* LARGEREAL
     
        call gtm_fatal('ERROR IN SPATIOTEMPORAL DATA !')
 
 end subroutine 
 
 subroutine sediment_velocity_width(velocity,  &
                                    depth,     &
                                    ncell,     &
                                    time,      &
                                    dx,        &
                                    dt)
     use gtm_precision
     use error_handling
     implicit none
     
        integer, intent(in) :: ncell                    !< Number of cells (in)
        real(gtm_real), intent(in)  :: time             !< Time of request (in)
        real(gtm_real), intent(in)  :: dx               !< Spatial step (in)
        real(gtm_real), intent(in)  :: dt               !< Time step  (in)
        real(gtm_real), intent(out) :: velocity(ncell)  !< Cell and time centered velocity (out)
        real(gtm_real), intent(out) :: depth(ncell)     !< Cell center depth (out)
        ! just to avoid warning
        velocity = .8d0
        depth = 3.5d0
     
 
 end subroutine
 

end module
