




!> This module contains unit test for readdss() function
!> Only 15min and irr are tested here, since ut_gtm_dss_readtvd will 
!> cover testing of all time periods and it calls readdss() anyway. 
!>@ingroup test_process_io
module ut_gtm_dss_readdss

    use fruit
    use gtm_precision
    use gtm_dss, only: mins15, irrs, max_inp_min, max_inp_irr
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
        character(len=130) :: indssfilenames(1)
        character*8 :: per_type
        type(dataqual_t) :: indata_15min(max_inp_min, mins15)
        type(dataqual_t) :: indata_irr(max_inp_irr, irrs)

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
        
        call opendss(ifltab_in, num_dssfiles, indssfilenames)       
        
        jmin = 48422520       ! 24JAN1992 1800
        
        pathnumber = 1       
        call readdss (pathnumber,    & 
                      jmin,          &
                      max_inp_min,   &
                      mins15,        &
                      indata_15min,  &
                      per_type)    
        call assertEquals (indata_15min(2,1)%data, dble(1.8628), weakest_eps, "problem in readdss indata_15min(2,1)")
        call assertEquals (indata_15min(100,1)%data, dble(1.9398), weakest_eps, "problem in readdss indata_15min(100,1)")
        call assertEquals (indata_15min(1,2)%data, dble(1.8795), weakest_eps, "problem in readdss indata_15min(1,2)")
        
        pathnumber = 2
        call readdss (pathnumber,    & 
                      jmin,          &
                      max_inp_irr,   &
                      irrs,          &
                      indata_irr,    &
                      per_type)          
        call assertEquals  (indata_irr(1,1)%data, dble(0.0000), weakest_eps, "problem in readdss indata_irr(1,1)")
         
        call zclose(ifltab_in)
        deallocate(pathinput)
        
        return
    end subroutine

end module        
    
    
    
    