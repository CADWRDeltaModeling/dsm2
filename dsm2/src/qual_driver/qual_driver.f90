program dsm2qual_driver
    use dsm2qual
    print *, 'New qual loop!!!'
    call qual_prepare1()
    call get_command_args(init_input_file, model_name, echo_only)
    call qual_prepare2()
    if (check_input_data) then
        call qual_check_input_data()
    else
        call qual_prepare_loop()
        do while (julmin .le. end_julmin)
            call qual_loop()
        end do
        call qual_wrapup1()
    end if
    call qual_wrapup2()
    call exit(0)
end program dsm2qual_driver