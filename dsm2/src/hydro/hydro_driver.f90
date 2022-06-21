program Hydro
    use fourpt
    implicit none
    call prepare_hydro
    call get_command_args(init_input_file, model_name, echo_only)
    call fourpt_main

end program Hydro
