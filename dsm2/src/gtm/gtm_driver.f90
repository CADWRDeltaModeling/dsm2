program dsm2gtm_driver
    use dsm2gtm
    use gtm_vars, only: gtm_start_jmin, gtm_end_jmin, gtm_time_interval
    implicit none

    call gtm_prepare1()
    call get_command_args(gtm_init_input_file)
    call gtm_prepare2()
    call gtm_prepare_loop()

    do current_time = gtm_start_jmin, gtm_end_jmin, gtm_time_interval
        call gtm_loop()
    end do

    call gtm_wrapup()
    call exit(0)

end program dsm2gtm_driver
