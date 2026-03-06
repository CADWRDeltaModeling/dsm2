program hydro_gtm
    use constants
    use io_units
    use runtime_data
    use hdfvars
    use fourpt
    use common_gtm_vars, only: gtm
    use dsm2gtm, only: gtm_prepare2, gtm_prepare_loop, gtm_loop, gtm_init_input_file, current_time, gtm_time_interval, gtm_wrapup
    use gtm_vars, only: memory_buffer, gtm_start_jmin
    use mod_init_oprules_hydrogtm, only: init_oprules_hydrogtm
    use init_hydrogtm, only: initialize_hydrogtm

    implicit none

    ! call prepare_hydro()
    dsm2_module = hydro
    dsm2_name = 'Hydro'
    open ( &
        unit_screen, &
        carriagecontrol='list', &
        buffered='NO', &
        iostat=istat &
        )
    open ( &
        unit_error, &
        carriagecontrol='list', &
        buffered='NO', &
        iostat=istat &
        ) !! <NT>

    ! From fourpt
    call get_command_args_hydro_gtm(init_input_file, gtm_init_input_file)

    call fourpt_init(init_oprules_hydrogtm)

    dsm2_module = gtm
    dsm2_name = 'GTM'
    ! call get_command_args(init_input_file)
    ! gtm_init_input_file = 'gtm.inp'

    ! We need to read only on time step at a time
    memory_buffer = 1
    call gtm_prepare2(file_id)
    call gtm_prepare_loop()

    dsm2_name = "Hydro_GTM"
    call initialize_hydrogtm()
    do while (julmin .le. end_julmin) ! normal time run
        call fourpt_step()
        if (prev_julmin .ge. gtm_start_jmin) then
            current_time = prev_julmin
            do while (current_time .lt. julmin)
                call gtm_loop()
                current_time = current_time + gtm_time_interval
            end do
        end if
    end do
    dsm2_name = "hydro_gtm"
    call gtm_wrapup()
    call fourpt_winddown()
end program hydro_gtm
