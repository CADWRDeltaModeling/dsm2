

subroutine process_text_gate_input()
    use input_storage_fortran
    use constants

    implicit none
    integer :: nitem
    character*(128) filename
    integer :: icount
    character*(32) name
    integer :: ierror = 0


    ! output_channel
    character*16 variable
    character*392 inpath
    character*8  fillin
    character*32 ::device


    nitem = input_gate_buffer_size()
    do icount = 1,nitem
        call input_gate_query_from_buffer(icount, &
            name, &
            device, &
            variable, &
            fillin,    &
            filename, &
            inpath, &
            ierror)


        call process_input_gate(name, &
            device, &
            variable, &
            fillin,    &
            filename, &
            inpath)
    end do
    print *,"Number of gate inputs processed: ", nitem

    return
end subroutine


subroutine process_text_oprule_ts_input()
    use input_storage_fortran
    use constants

    implicit none
    integer :: nitem
    character*(128) filename
    integer :: icount
    character*(32) name
    character*392 inpath
    character*8  fillin

    integer :: ierror = 0
    nitem = oprule_time_series_buffer_size()
    do icount = 1,nitem
        call oprule_time_series_query_from_buffer(icount, &
            name, &
            fillin, &
            filename, &
            inpath, &
            ierror)


        call process_input_oprule(name, &
            filename, &
            inpath, &
            fillin)
    end do
    print *,"Number of oprule time series processed: ", nitem
    return
end subroutine

!======================================================================
subroutine process_text_oprule_input()
    use input_storage_fortran
    use constants

    implicit none
    integer :: nitem
    integer :: icount
    character*(32) name
    character*512 action, trigger, definition
    integer :: ierror = 0




    nitem = oprule_expression_buffer_size()
    do icount = 1,nitem
        call oprule_expression_query_from_buffer(icount, &
            name, &
            definition,ierror)
        call process_oprule_expression(name, &
            definition)
    end do
    print *,"Number of operating rule expressions processed: ", nitem


    nitem = operating_rule_buffer_size()
    do icount = 1,nitem
        call operating_rule_query_from_buffer(icount, &
            name, &
            action, &
            trigger, &
            ierror)

        call process_oprule(name, &
            action, &
            trigger)
    end do
    print *,"Number of operating rules processed: ", nitem
    return
end subroutine
