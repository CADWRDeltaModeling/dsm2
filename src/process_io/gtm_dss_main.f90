
!> Interface to request time-varying data by specified julmin
!> and prev_julmin. 
!> The time series could be time-varying data from DSS or constant
!> value. It calls readtvd module which was originally developed
!> by Ralph Finch and last modified in September 1996.
!>@ingroup process_io
module gtm_dss_main
    
    use gtm_dss

    contains
    
    !> Return values from all time-varying data for the time specified
    subroutine get_inp_value(jmin, prev_jmin)
        use common_dsm2_vars, only: n_inputpaths, pathinput
        use gtm_dss
        use gtm_dss_readtvd
        implicit none
        integer, intent(in) :: jmin        !< current julmin
        integer, intent(in) :: prev_jmin   !< previous julmin
        integer :: i 
        
        do i=1, npthsin_min15
            call readtvd(datain_min15, jmin, prev_jmin, npthsin_min15, mins15, n_inputpaths, ptin_min15)
        enddo    
        do i=1, npthsin_hour1
            call readtvd(datain_hour1, jmin, prev_jmin, npthsin_hour1, hrs, n_inputpaths, ptin_hour1)
        enddo  
        do i=1, npthsin_day1
            call readtvd(datain_day1, jmin, prev_jmin, npthsin_day1, dys, n_inputpaths, ptin_day1)
        enddo  
        do i=1, npthsin_week1
            call readtvd(datain_week1, jmin, prev_jmin, npthsin_week1, wks, n_inputpaths, ptin_week1)
        enddo  
        do i=1, npthsin_month1
            call readtvd(datain_month1, jmin, prev_jmin, npthsin_month1, mths, n_inputpaths, ptin_month1)
        enddo  
        do i=1, npthsin_year1
            call readtvd(datain_year1, jmin, prev_jmin, npthsin_year1, yrs, n_inputpaths, ptin_year1)
        enddo          
        do i=1, npthsin_irr
            call readtvd(datain_irr, jmin, prev_jmin, npthsin_irr, irrs, n_inputpaths, ptin_irr)
        enddo    
      
        do i = 1, n_inputpaths
            call get_inp_data(i)
            !print*, pathinput(i)%value
        end do
          
        return
    end subroutine
    
end module