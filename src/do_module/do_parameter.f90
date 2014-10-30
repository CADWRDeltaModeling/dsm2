!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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

!> parameters for DO module
!>@ingroup do_module
module do_parameter
 
    use gtm_precision

    integer :: i_bod  = 0
    integer :: i_do   = 0
    integer :: i_orgn = 0
    integer :: i_nh3  = 0
    integer :: i_no2  = 0
    integer :: i_no3  = 0
    integer :: i_orgp = 0
    integer :: i_po4  = 0
    integer :: i_alg  = 0
    integer :: i_temp = 0
    integer :: i_ec   = 0
    integer :: i_tds  = 0
    integer :: i_cl   = 0

    real(gtm_real) :: alg_bod         =  1.00d0
    real(gtm_real) :: alg_chl_ratio   =  10.00d0 ! Chlorophyll to biomass ratio
    real(gtm_real) :: algaefract_n    =  0.09d0     ! fraction of algae as Nitrogen
    real(gtm_real) :: algaefract_p    =  0.012d0     ! fraction of algae as Phosphorus
    real(gtm_real) :: evapcoeff_a     =  0.00068d0    ! set values for temperature related consts
    real(gtm_real) :: evapcoeff_b     =  0.0002d0   ! set values for temperature related consts
    real(gtm_real) :: klight_half     =  0.085d0	! MM half-saturation constant for light Jon used klight_half=0.085*60;
    real(gtm_real) :: knit_half       =  0.05d0 ! MM half-saturation constant for nitrogen
    real(gtm_real) :: kpho_half       =  0.01d0	! MM half-saturation const. for phosphorus
    real(gtm_real) :: lambda0         =  0.26d0   ! Non algal light extinction coefficient
    real(gtm_real) :: lambda1         =  0.003d0    ! Linear algal self shading coefficient
    real(gtm_real) :: lambda2         =  0.0165d0  ! Linear algal self shading coefficient
    real(gtm_real) :: oxy_nh3         =  3.0d0   ! Oxygen used in oxidiation of NH3 to NO2
    real(gtm_real) :: oxy_no2         =  1.14d0 ! Oxygen used in oxidiation of NO2 to NO3
    real(gtm_real) :: oxy_photo       =  1.6d0 ! Oxygen produced per unit of algal growth
    real(gtm_real) :: oxy_resp        =  2.0d0 ! O2 uptake per unit of algal respiration
    real(gtm_real) :: pref_factor     =  0.5d0  ! algal pref. factor for NH3
    real(gtm_real) :: dust_attcoeff   =  0.04d0
    real(gtm_real) :: elev            =  0.d0  
    real(gtm_real) :: alat             =  38*pi/180 !set alat in radians
    real(gtm_real) :: lat             =  38.00d0 !set alat in degree
    real(gtm_real) :: longitude       =  121.5d0 !set longitude in degree
    real(gtm_real) :: long_std_merid  =  120.00d0     !set longtitude of sandard time meridian in degrees

    integer :: it_bod_decay = 1
    integer :: it_bod_set = 2
    integer :: it_reaer = 3
    integer :: it_do_ben = 4    
    integer :: it_orgn_decay = 5
    integer :: it_orgn_set = 6
    integer :: it_nh3_decay = 7
    integer :: it_nh3_ben = 8
    integer :: it_no2_decay = 9
    integer :: it_orgp_decay = 10
    integer :: it_orgp_set = 11
    integer :: it_po4_ben = 12
    integer :: it_alg_grow = 13
    integer :: it_alg_resp = 14
    integer :: it_alg_set = 15
    integer :: it_alg_die = 16
    
    real(gtm_real), allocatable :: bod_decay(:)
    real(gtm_real), allocatable :: bod_settle(:)         
    real(gtm_real), allocatable :: do_benthic(:)
    real(gtm_real), allocatable :: orgn_decay(:)
    real(gtm_real), allocatable :: orgn_settle(:)
    real(gtm_real), allocatable :: nh3_decay(:)    
    real(gtm_real), allocatable :: nh3_benthic(:)
    real(gtm_real), allocatable :: no2_decay(:)
    real(gtm_real), allocatable :: orgp_decay(:)
    real(gtm_real), allocatable :: orgp_settle(:)
    real(gtm_real), allocatable :: po4_benthic(:)
    real(gtm_real), allocatable :: alg_grow(:)
    real(gtm_real), allocatable :: alg_resp(:)
    real(gtm_real), allocatable :: alg_settle(:)
    real(gtm_real), allocatable :: alg_die(:)          

    real(gtm_real) :: thet(16)                  !< temperature coefficients
    real(gtm_real) :: thet_lookup_tbl(16,80)    !< output lookup table

    contains
    
    subroutine set_do_parameters(ncell, nvar)
        implicit none
        integer, intent(in) :: ncell
        integer, intent(in) :: nvar
        call find_do_conc_index(nvar)
        call create_temp_lookup
        call set_do_cell_param(ncell)
        return
    end subroutine    
    
    subroutine deallocate_do_param()
        implicit none
        deallocate(bod_decay)
        deallocate(bod_settle)
        deallocate(do_benthic)
        deallocate(orgn_decay)
        deallocate(orgn_settle)
        deallocate(nh3_decay)
        deallocate(nh3_benthic)
        deallocate(no2_decay)
        deallocate(orgp_decay)
        deallocate(orgp_settle)
        deallocate(po4_benthic)
        deallocate(alg_grow)
        deallocate(alg_resp)   
        deallocate(alg_settle)         
        deallocate(alg_die)               
        return
    end subroutine    
    
    !> set parameters
    subroutine set_do_cell_param(ncell)
        implicit none
        integer, intent(in) :: ncell
        allocate(bod_decay(ncell))
        allocate(bod_settle(ncell))
        allocate(do_benthic(ncell))
        allocate(orgn_decay(ncell))
        allocate(orgn_settle(ncell))
        allocate(nh3_decay(ncell))
        allocate(nh3_benthic(ncell))
        allocate(no2_decay(ncell))
        allocate(orgp_decay(ncell))
        allocate(orgp_settle(ncell))
        allocate(po4_benthic(ncell))
        allocate(alg_grow(ncell))
        allocate(alg_resp(ncell))
        allocate(alg_settle(ncell))
        allocate(alg_die(ncell))    
        bod_decay    =  LARGEREAL
        bod_settle   =  LARGEREAL            
        do_benthic   =  LARGEREAL
        orgn_decay   =  LARGEREAL
        orgn_settle  =  LARGEREAL
        nh3_decay    =  LARGEREAL    
        nh3_benthic  =  LARGEREAL
        no2_decay    =  LARGEREAL
        orgp_decay   =  LARGEREAL
        orgp_settle  =  LARGEREAL
        po4_benthic  =  LARGEREAL
        alg_grow     =  LARGEREAL
        alg_resp     =  LARGEREAL
        alg_settle   =  LARGEREAL
        alg_die      =  LARGEREAL        
        return
    end subroutine
    
    
    !> create temperature adjustment parameters lookup table
    subroutine create_temp_lookup()
        implicit none
        integer :: it, i                                         ! local variables
        real(gtm_real) :: tmp                                    ! local variables
        thet(1) =   1.047d0  ! 1: temp_bod_decay
        thet(2) =   1.024d0  ! 2: temp_bod_set
        thet(3) =   1.024d0  ! 3: temp_reaer  
        thet(4) =   1.06d0   ! 4: temp_do_ben
        thet(5) =   1.047d0  ! 5: temp_orgn_decay
        thet(6) =   1.024d0  ! 6: temp_orgn_set  
        thet(7) =   1.074d0  ! 7: temp_nh3_ben
        thet(8) =   1.083d0  ! 8: temp_nh3_decay
        thet(9) =   1.047d0  ! 9: temp_no2_decay
        thet(10) =  1.047d0  ! 10: temp_orgp_decay
        thet(11) =  1.024d0  ! 11: temp_orgp_set
        thet(12) =  1.074d0  ! 12: temp_po4_ben
        thet(13) =  1.07d0   ! 13: temp_alg_grow
        thet(14) =  1.047d0  ! 14: temp_alg_resp
        thet(15) =  1.024d0  ! 15: temp_alg_set 
        thet(16) =  1.047d0  ! 16: temp_alg_die                       
        do it = 1, 80
           tmp = dble(it)*half-dble(20.)
           do i = 1, 16
               thet_lookup_tbl(i,it) = thet(i)**(tmp)
           end do
        end do
        return    
    end subroutine
    
    
    !> calculate temperature adjustment factors
    subroutine tempfactors(temp, thetadj)
        implicit none
        real(gtm_real), intent(in) :: temp                   !< temprature 
        real(gtm_real), intent(out) :: thetadj(16)           !< output adjusted temperaure coefficients
        real(gtm_real) :: frac                               ! local variables
        integer :: itemp, i                                  ! local variables
        
        if ((temp.ge.0.5d0) .and. (temp.le.40.d0)) then  ! temperature in the right range, interpolate between values
            itemp = int(temp*two)
            frac = temp*two-dble(itemp)
            do i = 1, 16
                thetadj(i) = (one-frac)*thet_lookup_tbl(i, itemp) + frac*thet_lookup_tbl(i,itemp+1)
            end do 
        else                                             ! temperature not in the right range. go ahead and calculate the values.
            thetadj(:) = thet(:)**(temp-dble(20.))
        end if        
        return
    end subroutine    


    !> find index
    subroutine find_do_conc_index(nvar)
        use common_dsm2_qual, only: constituents
        implicit none
        integer, intent(in) :: nvar
        integer :: i
        do i = 1, nvar
            if (trim(constituents(i)%name)=='ec')   i_ec = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='tds')  i_tds = constituents(i)%conc_no    
            if (trim(constituents(i)%name)=='cl')   i_cl = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='bod')  i_bod = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='do')   i_do = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='orgn') i_orgn = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='nh3')  i_nh3 = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='no2')  i_no2 = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='no3')  i_no3 = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='orgp') i_orgp = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='po4')  i_po4 = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='alg')  i_alg = constituents(i)%conc_no
            if (trim(constituents(i)%name)=='temp') i_temp = constituents(i)%conc_no
        end do            
        return
    end subroutine

end module