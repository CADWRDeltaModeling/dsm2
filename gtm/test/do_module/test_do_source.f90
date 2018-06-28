!<license>
!    Copyright (C) 2017 State of California,
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
!> Routines to test do module parameter calculation
!>@ingroup test_do_module

module ut_do_source

    use fruit
   
    contains
    
    subroutine test_do_module
        use source_sink 
        use do_source
        use do_parameter
        use do_state_variables
        use common_dsm2_qual, only: constituents
        implicit none
        integer, parameter :: ncell = 1    ! bathtub
        integer, parameter :: nvar = 10
        real(gtm_real) :: flow(ncell)
        real(gtm_real) :: area(ncell)
        real(gtm_real) :: conc(ncell,nvar)
        real(gtm_real) :: source(ncell,nvar)
        real(gtm_real) :: time_step
        compute_source => do_module_source
        
        allocate(constituents(10))
        constituents(1)%name = 'bod'
        constituents(2)%name = 'do'  
        constituents(3)%name = 'orgn'
        constituents(4)%name = 'nh3'
        constituents(5)%name = 'no2'
        constituents(6)%name = 'no3'
        constituents(7)%name = 'orgp'
        constituents(8)%name = 'po4'
        constituents(9)%name = 'alg'
        constituents(10)%name = 'temp'
        constituents(1)%conc_no = 1
        constituents(2)%conc_no = 2
        constituents(3)%conc_no = 3
        constituents(4)%conc_no = 4
        constituents(5)%conc_no = 5
        constituents(6)%conc_no = 6
        constituents(7)%conc_no = 7
        constituents(8)%conc_no = 8
        constituents(9)%conc_no = 9
        constituents(10)%conc_no = 10
        
        call set_do_parameters(ncell,nvar)
        call allocate_do_state(ncell)       
        
        atmpr = 30.d0   ! is it in minibar?
        wetblb = 61.d0
        dryblb = 73.d0
        cloud = 0.d0    ! no cloud 
        wind = 8.d0     ! unit?     
        depth = 2.d0    ! in ft
        flow = 0.d0     ! no flow
        area = 2.d0     ! to calculate velocity

        time_step = one !in mins
                
        conc(1,1) = 5.98d0    ! bod
        conc(1,2) = 8.59d0    ! do
        conc(1,3) = 0.498d0   ! orgn
        conc(1,4) = 0.4d0     ! nh3
        conc(1,5) = 0.2d0     ! no2
        conc(1,6) = 1.7d0     ! no3
        conc(1,7) = 0.09986d0 ! orgp
        conc(1,8) = 0.15d0    ! po4
        conc(1,9) = 0.995d0   ! algae
        conc(1,10) = 20.0d0   ! temp                  

        bod_decay    =  dble(0.12/24)
        bod_settle   =  dble(0.1/24)           
        do_benthic   =  dble(30/24)
        orgn_decay   =  dble(0.1/24)
        orgn_settle  =  dble(0.01/24)
        nh3_decay    =  dble(0.1/24)
        nh3_benthic  =  dble(0.0/24)
        no2_decay    =  dble(0.2/24)
        orgp_decay   =  dble(0.1/24)
        orgp_settle  =  dble(0.01/24)
        po4_benthic  =  dble(0.0/24)
        alg_grow     =  dble(3/24)
        alg_resp     =  dble(0.15/24)
        alg_settle   =  dble(0.2/24)
        alg_die      =  dble(0.2/24)

        call compute_source(source, & 
                            conc,   &
                            area,   &
                            flow,   &
                            ncell,  &
                            nvar,   &
                            time_step)
        deallocate(constituents)
        return    
    end subroutine        
            
end module    