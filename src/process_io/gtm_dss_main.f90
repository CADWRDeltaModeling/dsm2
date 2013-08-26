


module gtm_dss_main
    
    use gtm_dss

    contains
    
    !> Allocate datain array for different time intervals
    subroutine setup_indata_arr()
        implicit none
        call allocate_datain        
        return
    end subroutine    
    
    !> Return values from all time-varying data for the time specified
    subroutine get_inp_value(jmin, prev_jmin)
        use common_dsm2_vars, only: n_inputpaths, pathinput
        use gtm_dss
        
        implicit none
        integer, intent(in) :: jmin        !< current julmin
        integer, intent(in) :: prev_jmin   !< previous julmin
        integer :: i 
        do i=1, npthsin_min15
            call readtvd(datain_min15, jmin, prev_jmin, npthsin_min15, mins15, npthsin_min15, ptin_min15)
        enddo    

!npthsin_hour1
! npthsin_day1
! npthsin_week1
! npthsin_month1
!    npthsin_year1
!npthsin_irr      
        
        
        do i = 1, n_inputpaths
            call get_inp_data(i)
            print*, pathinput(i)%value
        end do
        
    
        return
    end subroutine
    
end module