program Hydro
    use fourpt
    implicit none
    call fourpt_init
    call get_command_args(init_input_file, model_name, echo_only)
    call fourpt_main

end program Hydro
