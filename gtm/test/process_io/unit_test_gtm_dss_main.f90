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
!> This module contains unit test for dss_main() function, which is the
!> interface used to request time-varying data by julmin.
!>@ingroup test_process_io
module ut_gtm_dss_main

    use fruit
    use gtm_precision
    use gtm_dss, only: mins15, irrs
    use gtm_dss_read
    use gtm_dss_main
 
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
    
    subroutine test_dss_main
        use common_dsm2_vars, only: pathinput, ifltab_in, dataqual_t,              &
                                    per_type_inst_val, n_inputpaths
        implicit none
        character(len=130) :: indssfilenames(2)
        integer :: num_dssfiles
        integer :: start_julmin, end_julmin
        integer :: jmin, prev_jmin
        
        start_julmin = 48389400
        end_julmin =   48390000
        
        n_inputpaths = 6
        allocate(pathinput(n_inputpaths))
        
        pathinput(1)%filename = "tutorial.dss"
        pathinput(1)%path = "/TUTORIAL/DOWNSTREAM/STAGE//15MIN/REALISTIC/"
        pathinput(1)%ndx_file = 1
        pathinput(1)%intvl_path = 1
        pathinput(1)%per_type = per_type_inst_val
        
        pathinput(2)%filename = "tutorial.dss"
        pathinput(2)%path = "/TUTORIAL/GATE/FLAP_OP//IR-YEAR/TIMEVAR/"
        pathinput(2)%ndx_file = 1
        pathinput(2)%intvl_path = 1        
        pathinput(2)%interval = "ir-year"
        pathinput(2)%per_type = per_type_inst_val
        
        pathinput(3)%filename = "hist_19902007.dss"
        pathinput(3)%path = "/BAY/SHWSF001/STAGE//1HOUR/DWR-DMS-ASTRO/"
        pathinput(3)%ndx_file = 2
        pathinput(3)%intvl_path = 1        
        pathinput(3)%interval = "1hour"
        pathinput(3)%per_type = per_type_inst_val        

        pathinput(4)%filename = "hist_19902007.dss"
        pathinput(4)%path = "/FILL+CHAN/EASTSIDE/DOC//1DAY/DWR-DMS-200609/"
        pathinput(4)%ndx_file = 2
        pathinput(4)%intvl_path = 1        
        pathinput(4)%interval = "1day"
        pathinput(4)%per_type = per_type_inst_val   

        pathinput(5)%filename = "hist_19902007.dss"
        pathinput(5)%path = "/FILL+CHAN/LODI/PRECIP//1MON/DWR-DMS-200701/"
        pathinput(5)%ndx_file = 2
        pathinput(5)%intvl_path = 1        
        pathinput(5)%interval = "1mon"
        pathinput(5)%per_type = per_type_inst_val           

        pathinput(6)%filename = "constant"
        pathinput(6)%path = "20"
        pathinput(6)%constant_value = 20
        pathinput(6)%ndx_file = 0
        pathinput(6)%intvl_path = 1
        pathinput(6)%interval = "1year"
        pathinput(6)%per_type = per_type_inst_val 
                
        num_dssfiles = 2
        indssfilenames(1) = pathinput(1)%filename
        indssfilenames(2) = pathinput(3)%filename

        npthsin_min15 = 1
        npthsin_hour1 = 1
        npthsin_day1 = 1
        npthsin_month1 = 1
        npthsin_year1 = 1
        npthsin_irr = 1      
       
        call allocate_datain
        
        ptin_min15(1) = 1        
        ptin_irr(1) = 2        
        ptin_hour1(1) = 3
        ptin_day1(1) = 4
        ptin_month1(1) = 5
        ptin_year1(1) = 6
        
        allocate(ifltab_in(600,num_dssfiles ))     
        call opendss(ifltab_in, num_dssfiles, indssfilenames)       
        
        jmin = 48422655
        prev_jmin = jmin - 15
        call get_inp_value(jmin, prev_jmin)

        call zclose(ifltab_in)
        deallocate(pathinput)        
        deallocate(ifltab_in) 
        call deallocate_datain 
        
        return
    end subroutine
    
end module    
   