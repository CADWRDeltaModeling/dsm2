program dsm2gtm_driver
    use dsm2gtm
    use common_variables, only: gtm_start_jmin, gtm_end_jmin, gtm_time_interval
    implicit none
    print *, 'New GTM loop!!!'
    call gtm_prepare1()
    call get_command_args(init_input_file)
    call gtm_prepare2()

    call gtm_prepare_loop()
    print *, 'start', gtm_start_jmin !todel
    print *, 'end', gtm_end_jmin !todel
    do current_time = gtm_start_jmin, gtm_end_jmin, gtm_time_interval
        call gtm_loop()
    end do
    print *, gtm_start_jmin !todel
    print *, gtm_end_jmin !todel
    print *, gtm_time_interval !todel
    print *, current_time !todel
    call gtm_wrapup()
    call exit(0)

end program dsm2gtm_driver
