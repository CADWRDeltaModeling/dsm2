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

    use fruit
    use gtm_precision
    use sediment_variables
    use non_cohesive_source
    
    contains

    !> Test suite for non cohesive subroutines
    subroutine test_all_non_cohesive
        implicit none
        call test_source_non_cohesive
        call test_es_garcia_parker
        call test_parker_rouse_profile
        call test_teeter
        return
    end subroutine    

    !> test source_non_cohesive subroutine
    subroutine test_source_non_cohesive()
        implicit none
        integer, parameter :: ncell = 10
        integer, parameter :: nclass = 3    
        real(gtm_real) :: conc(ncell)
        real(gtm_real) :: flow(ncell)
        real(gtm_real) :: area(ncell)
        real(gtm_real) :: width(ncell)
        real(gtm_real) :: hydro_radius(ncell) 
        real(gtm_real) :: manning(ncell)
        real(gtm_real) :: diameter(3)
        real(gtm_real) :: resuspension(ncell)
        real(gtm_real) :: deposition(ncell)        
        real(gtm_real) :: vertical_flux(ncell)
        integer :: i
        
        flow = 1500.d0 *0.0283168
        area = 1500.d0 *0.092903
        width = 100.d0 *0.3048
        hydro_radius = 110.d0 *0.3048
        conc(:) = 0.2       
        manning(:) = 0.022d0
        diameter(1) = 200.0d-6  ! fine sand 125-250 mu-m
        diameter(2) = 20.0d-6   ! silt 3.9-62.5 mu-m
        diameter(3) = 2.0d-6    ! clay 0.98-3.9 mu-m
        
        do i = 1, 3       
        call source_non_cohesive(vertical_flux,    &
                                 resuspension,     &
                                 deposition,       &        
                                 conc,             &
                                 flow,             &
                                 area,             &
                                 width,            &
                                 hydro_radius,     &
                                 manning,          &
                                 diameter(i),      &
                                 ncell)
        end do        
        return
    end subroutine    


    !> Test es_garcia_parker
    subroutine test_es_garcia_parker()
        implicit none
        integer,parameter :: ncell = 3         !< Number of computational volumes in a channel
        integer,parameter :: nclass = 2        !< Number of non-cohesive sediment grain classes
        real(gtm_real) :: e_s(ncell)           !< Dimenssionless rate of entrainment of bed sediment into suspension 
        real(gtm_real) :: shear_v(ncell)       !< Shear Velocity
        real(gtm_real) :: exp_re_p(ncell)      !< Explicit particle Reynolds number
        real(gtm_real) :: settling_v(ncell)    !< Settling velocity
        !---local
        real(gtm_real) :: hand_calc_value(ncell,nclass)
        integer :: icell, iclass

        shear_v =[0.1d0,0.4d0,one]

        hand_calc_value = reshape ([0.29995136236d0,	0.29999995249d0,	0.29999999951d0, &
                                0.00012994369d0,	0.09220539342d0,	0.29323308271d0],[3,2])
        exp_re_p = two
        settling_v = 0.001d0        
        call es_garcia_parker(e_s,         &
                              shear_v,     &
                              exp_re_p,    &
                              settling_v,  & 
                              ncell)       
        do icell = 1, ncell                                                 
            call assertEquals(hand_calc_value(icell,1),e_s(icell),weak_eps,"Error in subroutine es_garcia_parker1")             
        end do 

        exp_re_p = ten
        settling_v = 0.1d0        
        call es_garcia_parker(e_s,         &
                              shear_v,     &
                              exp_re_p,    &
                              settling_v,  & 
                              ncell)       
        do icell = 1, ncell                                                 
            call assertEquals(hand_calc_value(icell,2),e_s(icell),weak_eps,"Error in subroutine es_garcia_parker2")             
        end do 
        return
    end subroutine


    !> Test parker_rouse_profile
    subroutine test_parker_rouse_profile()
        implicit none
        integer,parameter :: ncell = 3                 !< Number of computational volumes in a channel
        integer,parameter :: nclass = 6                !< Number of non-cohesive sediment grain classes
        real(gtm_real) :: c_b(ncell)                   !< deposition 
        real(gtm_real) :: shear_v(ncell)               !< Shear Velocity
        real(gtm_real) :: settling_v(ncell)           !< Settling velocity
        real(gtm_real) :: conc(ncell)                  !< Concentration
        !---local
        real(gtm_real) :: hand_calc_value(ncell,nclass)
        integer :: icell
 
        conc = 0.2d0
        shear_v =[0.003909042d0, 0.039090417d0, 0.390904175d0]
        
        hand_calc_value(1,:) = [0.200000024d0, 0.200019875d0, 0.216531406859513d0, 1.90956287962282d0, 2.65685239749921d0, 5.41144112606585d0 ]
        hand_calc_value(2,:) = [0.200000001d0, 0.200000689d0, 0.200573205d0, 0.259276851494945d0, 0.285188135778715d0, 0.380699888484132d0 ]
        hand_calc_value(3,:) = [0.2d0, 0.200000024d0, 0.200019875d0, 0.202055347d0, 0.202953787d0, 0.206265530904005d0 ]
        
        settling_v = 6.67177d-9
        call parker_rouse_profile(c_b,          &
                                  shear_v,      &                                   
                                  settling_v,   &
                                  conc,         &
                                  ncell)
        do icell = 1, ncell                                                              
            call assertEquals(hand_calc_value(icell,1),c_b(icell),weak_eps,"Error in subroutine parker_rouse_profile")
        end do 
        
        settling_v = 6.67177d-7
        call parker_rouse_profile(c_b,          &
                                  shear_v,      &                                   
                                  settling_v,   &
                                  conc,         &
                                  ncell)
        do icell = 1, ncell                                                              
            call assertEquals(hand_calc_value(icell,2),c_b(icell),weak_eps,"Error in subroutine parker_rouse_profile")
        end do 
        
        settling_v = 6.67177d-5
        call parker_rouse_profile(c_b,          &
                                  shear_v,      &                                   
                                  settling_v,   &
                                  conc,         &
                                  ncell)
        do icell = 1, ncell                                                          
            call assertEquals(hand_calc_value(icell,3),c_b(icell),weak_eps,"Error in subroutine parker_rouse_profile")
        end do 
        
        settling_v = 0.001599879d0
        call parker_rouse_profile(c_b,          &
                                  shear_v,      &                                   
                                  settling_v,   &
                                  conc,         &
                                  ncell)
        do icell = 1, ncell                                                               
            call assertEquals(hand_calc_value(icell,4),c_b(icell),weak_eps,"Error in subroutine parker_rouse_profile")
        end do 
        
        settling_v = 0.002050972d0
        call parker_rouse_profile(c_b,          &
                                  shear_v,      &                                   
                                  settling_v,   &
                                  conc,         &
                                  ncell)
        do icell = 1, ncell                                                              
            call assertEquals(hand_calc_value(icell,5),c_b(icell),weak_eps,"Error in subroutine parker_rouse_profile")
        end do 
        
        settling_v = 0.003432763d0
        call parker_rouse_profile(c_b,          &
                                  shear_v,      &                                   
                                  settling_v,   &
                                  conc,         &
                                  ncell)
        do icell = 1, ncell                                                             
            call assertEquals(hand_calc_value(icell,6),c_b(icell),weak_eps,"Error in subroutine parker_rouse_profile")
        end do 
        return
    end subroutine
     

    !> Test teeter
    subroutine test_teeter()
        implicit none
        integer,parameter :: ncell = 3        !< Number of computational volumes in a channel
        integer,parameter :: nclass = 6       !< Number of non-cohesive sediment grain classes
        real(gtm_real) :: c_b(ncell)          !< deposition 
        real(gtm_real) :: shear_v(ncell)      !< Shear Velocity
        real(gtm_real) :: settling_v(ncell)   !< Settling velocity
        real(gtm_real) :: conc(ncell)         !< Concentration
        !---local
        real(gtm_real) :: hand_calc_value(ncell,nclass)
        integer :: icell
 
        conc = 0.2d0
        shear_v =[0.003909042d0, 0.039090417d0, 0.390904175d0]
        hand_calc_value(1,:) = [ 0.200003996d0, 0.20039963d0, 0.239963002100908d0, 1.15830593438020d0, 1.42850455493674d0, 2.25617871990369d0 ]
        hand_calc_value(2,:) = [ 0.2000004d0, 0.200039963d0, 0.203996300516787d0, 0.295830600792554d0, 0.322850464921851d0, 0.405617887770544d0 ]
        hand_calc_value(3,:) = [ 0.20000004d0, 0.200003996d0, 0.20039963d0, 0.209583059956680d0, 0.212285046335049d0, 0.220561788514051d0 ]

        settling_v = 6.67177d-9
        call teeter(c_b,                &
                    shear_v,            &                                   
                    settling_v, &
                    conc,               &
                    ncell)  
        do icell=1,ncell                                           
            call assertEquals(hand_calc_value(icell,1),c_b(icell),weak_eps,"Error in subroutine teeter")
        end do 
        
        settling_v = 6.67177d-7
        call teeter(c_b,                &
                    shear_v,            &                                   
                    settling_v, &
                    conc,               &
                    ncell)  
        do icell=1,ncell                                           
            call assertEquals(hand_calc_value(icell,2),c_b(icell),weak_eps,"Error in subroutine teeter")
        end do 
                
        settling_v = 6.67177d-5
        call teeter(c_b,                &
                    shear_v,            &                                   
                    settling_v, &
                    conc,               &
                    ncell)  
        do icell=1,ncell                                           
            call assertEquals(hand_calc_value(icell,3),c_b(icell),weak_eps,"Error in subroutine teeter")
        end do 
              
        settling_v = 0.001599879d0
        call teeter(c_b,                &
                    shear_v,            &                                   
                    settling_v, &
                    conc,               &
                    ncell)  
        do icell=1,ncell                                           
            call assertEquals(hand_calc_value(icell,4),c_b(icell),weak_eps,"Error in subroutine teeter")
        end do 
               
        settling_v = 0.002050972d0
        call teeter(c_b,                &
                    shear_v,            &                                   
                    settling_v, &
                    conc,               &
                    ncell)  
        do icell=1,ncell                                           
            call assertEquals(hand_calc_value(icell,5),c_b(icell),weak_eps,"Error in subroutine teeter")
        end do 
                
        settling_v = 0.003432763d0
        call teeter(c_b,                &
                    shear_v,            &                                   
                    settling_v, &
                    conc,               &
                    ncell)  
        do icell=1,ncell                                           
            call assertEquals(hand_calc_value(icell,6),c_b(icell),weak_eps,"Error in subroutine teeter")
        end do         
                
        return
    end subroutine
     
end module
