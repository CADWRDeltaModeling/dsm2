
!> This module contains unit test for readtvd() 
!> function, which is used to retrieve time-varing
!> data from DSS file.
!>@ingroup test_process_io
module ut_gtm_dss_readtvd

    use fruit
    use gtm_precision
    
    contains
    
    !> Unit test for readtvd()
    subroutine test_readtvd()
    
        use gtm_dss
        use gtm_dss_open
        use gtm_dss_read   
        use gtm_dss_readtvd 
        use common_dsm2_vars, only: pathinput, ifltab_in, dataqual_t, &
                                    julmin, prev_julmin, start_julmin, end_julmin, &
                                    per_type_inst_val, n_inputpaths
        use time_utilities
        
        implicit none
        integer :: jmin, prev_jmin
        integer :: inpaths_dim
        integer :: block_dim
        integer :: num_dssfiles
        integer :: ninpath_15min, ninpath_1hr, ninpath_irr, ninpath_1day, ninpath_1mon
        integer, dimension(1) :: inpath_ptr_15min, inpath_ptr_1hr, inpath_ptr_irr, inpath_ptr_1day, inpath_ptr_1mon
        integer ::  i
        character(len=130) :: indssfilenames(2)
        character*8 :: per_type
        type(dataqual_t) :: indata_15min(100, mins15)
        type(dataqual_t) :: indata_1hr(100, hrs)
        type(dataqual_t) :: indata_irr(100, irrs)
        type(dataqual_t) :: indata_1day(100, dys)
        type(dataqual_t) :: indata_1mon(100, mths)

        start_julmin = 48389400
        end_julmin =   48390000
        
        n_inputpaths = 5
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
        
        num_dssfiles = 2
        indssfilenames(1) = pathinput(1)%filename
        indssfilenames(2) = pathinput(3)%filename

        ninpath_15min = 1
        ninpath_irr = 1           
        ninpath_1hr = 1        
        ninpath_1day = 1
        ninpath_1mon = 1
        inpath_ptr_15min(1) = 1        
        inpath_ptr_irr(1) = 2        
        inpath_ptr_1hr(1) = 3
        inpath_ptr_1day(1) = 4
        inpath_ptr_1mon(1) = 5
        
        allocate(ifltab_in(600, num_dssfiles ))    
        call opendss(ifltab_in, num_dssfiles, indssfilenames)

        !---- test for 15 min data ----
        jmin = 48422520       ! 24JAN1992 1800
        prev_jmin = jmin - 15
        call readtvd(indata_15min, jmin, prev_jmin, ninpath_15min, mins15, n_inputpaths, inpath_ptr_15min)
        call assertEquals (indata_15min(1,1)%data, dble(1.9993), weakest_eps, "problem in readtvd reading 15min data")
        call assertEquals (pathinput(1)%value, dble(1.9993), weakest_eps, "problem in readtvd reading 15min data")
        
        !---- test for irregular data ----
        call readtvd(indata_irr, jmin, prev_jmin,ninpath_irr, irrs, n_inputpaths, inpath_ptr_irr)
        call assertEquals (pathinput(2)%value, dble(0.0000), weakest_eps, "problem in readtvd reading irr data")
       
        jmin = 48422640       ! 24JAN1992 2000
        prev_jmin = jmin - 15       
        call readtvd(indata_irr, jmin, prev_jmin,ninpath_irr, irrs, n_inputpaths, inpath_ptr_irr)
        call assertEquals (pathinput(2)%value, dble(1.0000), weakest_eps, "problem in readtvd reading irr data at 24JAN1992 2000")       
  
        jmin = 48422655       ! 24JAN1992 2015
        prev_jmin = jmin - 15   
        call readtvd(indata_irr, jmin, prev_jmin, ninpath_irr, irrs, n_inputpaths, inpath_ptr_irr)
        call assertEquals (pathinput(2)%value, dble(0.8333), weakest_eps, "problem in readtvd reading irr data at 24JAN1992 2015")      

        jmin = 48422670       ! 24JAN1992 2030
        prev_jmin = jmin - 15   
        call readtvd(indata_irr, jmin, prev_jmin, ninpath_irr, irrs, n_inputpaths, inpath_ptr_irr)
        call assertEquals (pathinput(2)%value, dble(0.5833), weakest_eps, "problem in readtvd reading irr data at 24JAN1992 2030")      
  
        jmin = 48422685       ! 24JAN1992 2045
        prev_jmin = jmin - 15   
        call readtvd(indata_irr, jmin, prev_jmin, ninpath_irr, irrs, n_inputpaths, inpath_ptr_irr)
        call assertEquals (pathinput(2)%value, dble(0.3333), weakest_eps, "problem in readtvd reading irr data at 24JAN1992 2045")                   
     
        !---- test for hourly data ----
        jmin = 48422685       ! 24JAN1992 2045 (hr data: -1.4699 at 20:00, -1.6051 at 21:00)
        prev_jmin = jmin - 15   
        call readtvd(indata_1hr, jmin, prev_jmin, ninpath_1hr, hrs, n_inputpaths, inpath_ptr_1hr)
        call assertEquals (pathinput(3)%value, dble(-1.4699), weakest_eps, "problem in readtvd reading hour data at 24JAN1992 2045")   

        !---- test for daily data ----
        jmin = 48422685       ! 24JAN1992 2045 (daily data: 0.8438 at 23JAN1992 2400, 0.8400 at 24JAN1992 2400)
        prev_jmin = jmin - 15   
        call readtvd(indata_1day, jmin, prev_jmin, ninpath_1day, dys, n_inputpaths, inpath_ptr_1day)
        call assertEquals (pathinput(4)%value, dble(0.8400), weakest_eps, "problem in readtvd reading daily data at 24JAN1992 2045")   

        !---- test for monthly data ----
        jmin = 48422685       ! 24JAN1992 2045 (monthly data: 1.81 at 31JAN1992 2400)
        prev_jmin = jmin - 15   
        call readtvd(indata_1mon, jmin, prev_jmin, ninpath_1mon, mths, n_inputpaths, inpath_ptr_1mon)
        call assertEquals (pathinput(5)%value, dble(1.81), weakest_eps, "problem in readtvd reading monthly data at 24JAN1992 2045") 
                
        call zclose(ifltab_in)
        deallocate(pathinput)
        deallocate(ifltab_in) 
        n_inputpaths = 0
        return
    end subroutine
        
end module    