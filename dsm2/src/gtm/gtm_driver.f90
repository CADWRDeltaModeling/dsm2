program dsm2gtm_driver
    use dsm2gtm
    ! use runtime_data
    ! use runtime_data, only: julmin, end_julmin
    implicit none
    print *, 'New GTM loop!!!'
    call gtm_prepare1()
    call get_command_args(init_input_file)
    call gtm_prepare2()

    call gtm_prepare_loop()
    call gtm_loop()
    ! do while (julmin .le. end_julmin)
    !     call gtm_loop()
    ! end do
    call gtm_wrapup()
    call exit(0)

end program dsm2gtm_driver
