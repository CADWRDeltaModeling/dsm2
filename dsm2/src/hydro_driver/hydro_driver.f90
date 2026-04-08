program Hydro
    use fourpt
    use floweq1d, only: DynamicWaveEq
    implicit none
    call prepare_hydro
    call get_command_args(init_input_file, model_name, echo_only)

    call fourpt_main

end program Hydro
