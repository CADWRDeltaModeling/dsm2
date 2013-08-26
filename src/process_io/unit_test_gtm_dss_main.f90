


module ut_gtm_dss_main

    use fruit
    
    contains
    
    subroutine test_dss_main
        use gtm_dss
        use gtm_dss_main
        implicit none
        
        npthsin_min15 = 1
        npthsin_hour1 = 1
        npthsin_day1 = 1
        npthsin_month1 = 1
        npthsin_year1 = 1
        npthsin_irr = 1
        call setup_indata_arr

        return
    end subroutine
    
end module    
   