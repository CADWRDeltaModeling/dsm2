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
!> This module contains unit test for readdss() function
!> Only 15min and irr are tested here, since ut_gtm_dss_readtvd will 
!> cover testing of all time periods and it calls readdss() anyway. 
!>@ingroup test_process_io
module ut_gtm_dss_readdss

    use fruit
    use gtm_precision
    use gtm_dss, only: mins15, irrs
    use gtm_dss_open
    use gtm_dss_read
    
    contains

    !> Unit test of readdss() subroutine for 15min and irr data
    subroutine test_readdss()
   
        use common_dsm2_vars, only: pathinput, ifltab_in, dataqual_t
        implicit none
        integer :: pathnumber
        integer :: jmin
        integer :: inpaths_dim
        integer :: block_dim
        integer :: num_dssfiles
        integer, parameter :: ninpath_15min = 1
        integer, parameter :: ninpath_irr = 1
        character(len=130) :: indssfilenames(1)
        character*8 :: per_type
        type(dataqual_t) :: indata_15min(ninpath_15min, mins15)
        type(dataqual_t) :: indata_irr(ninpath_irr, irrs)

        allocate(pathinput(2))
        
        pathinput(1)%filename = "tutorial.dss"
        pathinput(1)%path = "/TUTORIAL/DOWNSTREAM/STAGE//15MIN/REALISTIC/"
        pathinput(1)%ndx_file = 1
        pathinput(1)%intvl_path = 1
        
        pathinput(2)%filename = "tutorial.dss"
        pathinput(2)%path = "/TUTORIAL/GATE/FLAP_OP//IR-YEAR/TIMEVAR/"
        pathinput(2)%ndx_file = 1
        pathinput(2)%intvl_path = 1        
        pathinput(2)%interval = "ir-year"
        
        num_dssfiles = 1
        indssfilenames(1) = pathinput(1)%filename

        allocate(ifltab_in(600,num_dssfiles ))                
        call opendss(ifltab_in, num_dssfiles, indssfilenames)       
        
        jmin = 48422520       ! 24JAN1992 1800
        
        pathnumber = 1       
        call readdss (pathnumber,    & 
                      jmin,          &
                      ninpath_15min, &
                      mins15,        &
                      indata_15min,  &
                      per_type)    
        call assertEquals (indata_15min(1,2)%data, dble(1.8628), weakest_eps, "problem in readdss indata_15min(2,1)")
        
        pathnumber = 2
        call readdss (pathnumber,    & 
                      jmin,          &
                      ninpath_irr,   &
                      irrs,          &
                      indata_irr,    &
                      per_type)          
        call assertEquals  (indata_irr(1,1)%data, dble(0.0000), weakest_eps, "problem in readdss indata_irr(1,1)")
         
        call zclose(ifltab_in)
        deallocate(pathinput)
        deallocate(ifltab_in) 
        return
    end subroutine

end module        
    
    
    
    