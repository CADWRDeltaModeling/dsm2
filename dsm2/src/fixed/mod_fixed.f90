module mod_fixed
    use error_handling
    !! Module to process common input data for DSM2
    interface
        module subroutine buffer_input_common()
        end subroutine buffer_input_common

        module subroutine process_scalar(Param, Val)
            character(LEN=32), intent(in) :: Val   ! parameter Val
            character(LEN=32), intent(in) :: Param  ! parameter
        end subroutine process_scalar

        module subroutine process_reservoir(id,reser_name,reser_area,reser_botelv)
            integer :: id
            character(len=32) :: reser_name
            real*8 :: reser_area
            real*8 :: reser_botelv
        end subroutine process_reservoir

        module subroutine process_reservoir_vol(resname, &
                                               reselev, &
                                               reser_area)
            character(len=32) :: resname
            real*8 :: reselev
            real*8 :: reser_area
        end subroutine process_reservoir_vol

        module subroutine process_reservoir_connection(resname, &
                                                      con_node, &
                                                      rescon_incoef, &
                                                      rescon_outcoef)
            character(len=32) :: resname
            integer :: con_node
            real*8 :: rescon_incoef
            real*8 :: rescon_outcoef
        end subroutine process_reservoir_connection

        module subroutine process_input_reservoir(Name, &
                                                    LocName, &
                                                    Param, &
                                                    Sign, &
                                                    Fillin, &
                                                    Filename, &
                                                    InPath)
                character(len=32) :: Name
                character(len=32) :: LocName
                character(len=16) :: Param
                integer*4 :: Sign
                character(len=8) :: Fillin
                character(len=128) :: Filename
                character(len=392) :: InPath
            end subroutine process_input_reservoir

        module subroutine process_output_reservoir(Name, &
                                                  LocName, &
                                                  SubLoc, &
                                                  Param, &
                                                  Interval, &
                                                  PerOp, &
                                                  SourceGroup, &
                                                  Filename)
            character(len=32) :: Name
            character(len=32) :: LocName
            integer*4 :: SubLoc
            character(len=16) :: Param
            character(len=16) :: Interval
            character(len=8) :: PerOp
            character(len=32) :: SourceGroup
            character(len=128) :: Filename
        end subroutine process_output_reservoir

        module subroutine process_input_gate(LocName, &
                                              SubLoc, &
                                              Param, &
                                              Fillin, &
                                              Filename, &
                                              InPath)
            character(len=32) :: LocName
            character(len=32) :: SubLoc
            character(len=16) :: Param
            character(len=32) :: Fillin
            character(len=128) :: Filename
            character(len=392) :: InPath
        end subroutine process_input_gate

        module subroutine process_input_transfer(Name, &
                                              Param, &
                                              Fillin, &
                                              Filename, &
                                              InPath)
            character(len=32) :: Name
            character(len=16) :: Param
            character(len=8) :: Fillin
            character(len=128) :: Filename
            character(len=392) :: InPath
        end subroutine process_input_transfer

        module subroutine process_input_node(Name, &
                                            LocNum, &
                                            param, &
                                            Sign, &
                                            RoleName, &
                                            Fillin, &
                                            Filename, &
                                            InPath)
            character(len=32) :: Name
            integer*4 :: LocNum
            character(len=16) :: param
            integer*4 :: Sign
            character(len=32) :: RoleName
            character(len=8) :: Fillin
            character(len=128) :: Filename
            character(len=392) :: InPath
        end subroutine process_input_node

        module subroutine process_channel_ic(channel, &
                                  dist, &
                                  stage, &
                                  flow)
            integer :: channel
            character*8 :: dist
            real*8 :: stage
            real*8 :: flow
        end subroutine process_channel_ic

        module subroutine process_reservoir_ic(resname, &
                                      stage)
            character(len=32) :: resname
            real*8 :: stage
        end subroutine process_reservoir_ic

        module subroutine process_rate_coef(group_name, &
                                    constituent, &
                                    rate_variable, &
                                    rate_value)
            character*32 :: group_name
            character*16 :: constituent
            character*16 :: rate_variable
            real*8 :: rate_value
        end subroutine process_rate_coef

        module subroutine process_output_gate(name, &
                                    LocName, &
                                    SubLoc, &
                                    param, &
                                    interval, &
                                    perop, &
                                    filename)
            character*32 :: name
            character*32 :: LocName
            character*32 :: SubLoc
            character*16 :: param
            character*16 :: interval
            character*8 :: perop
            character*128 :: filename
        end subroutine process_output_gate

        module subroutine process_output_channel(name, &
                                  channo, &
                                  distance, &
                                  param, &
                                  interval, &
                                  perop, &
                                  sourcegroup, &
                                  filename)
            character*32 :: name
            integer :: channo
            integer :: distance
            character*16 :: param
            character*16 :: interval
            character*8  :: perop
            character*32 :: sourcegroup
            character*128 :: filename
        end subroutine process_output_channel

        module subroutine process_channel( &
                                  extcounter, &
                                  id, &
                                  channo, &
                                  chan_len, &
                                  chan_manning, &
                                  chan_dispersion, &
                                  chan_upnode, &
                                  chan_downnode, &
                                  chan_dx_col )
            integer :: extcounter
            integer :: id
            integer :: channo
            integer :: chan_len
            real*8 :: chan_manning
            real*8 :: chan_dispersion
            integer :: chan_upnode
            integer :: chan_downnode
            real*8 :: chan_dx_col
        end subroutine process_channel

        module subroutine process_xsect(channo, chan_fdist, xsectid, xsectno)
            integer :: channo
            real*8 :: chan_fdist
            integer :: xsectid
            integer, intent(out) :: xsectno
        end subroutine process_xsect

        module subroutine process_xsect_layer_full(chan_no, dist, elev, area, width, wetperim)
            integer :: chan_no
            real*8 :: dist
            real*8 :: elev
            real*8 :: area
            real*8 :: width
            real*8 :: wetperim
        end subroutine process_xsect_layer_full

        module subroutine process_xsect_layer(xsectno, elev, area, width, wetperim)
            integer :: xsectno
            real*8 :: elev
            real*8 :: area
            real*8 :: width
            real*8 :: wetperim
        end subroutine process_xsect_layer

        module logical function order_nodes()
        end function order_nodes

        module subroutine process_xsect_csdp(channo, fdist, filename)
            integer :: channo
            real*8 :: fdist
            character*128 :: filename
        end subroutine process_xsect_csdp

        module subroutine read_grid_from_tidefile()
        end subroutine read_grid_from_tidefile
    end interface

contains
    subroutine input_text(filename)
!     Read in all text starting from input filename
        use hdf5
        use input_storage_fortran
        use envvar
        use runtime_data

        implicit none
        integer :: ierror = 0
        character * (*) :: filename

        call clear_all_buffers(ierror)

        call init_file_reader(ierror)        ! Prepare for a read of everything
        call set_initial_context_profile(dsm2_name)

!-----Do a first pass reading the input, activating only ENVVARS for use in later text substitution
        call set_user_substitution_enabled(.false., ierror)   ! don't try to substitute now
        call set_substitution_not_found_is_error(.false., ierror)
        call set_active_profile("envvar", ierror)        ! read only ENVVAR blocks
        call verify_error(ierror, "Error setting active profile")
        call read_buffer_from_text(filename, ierror)            ! read starting from this file
        call verify_error(ierror, "Error reading from text (envvar pass)")
        !
        ! process the results
        !
        call process_text_substitution(ierror)
        call set_user_substitution_enabled(.true., ierror)    ! substitute now
        call set_substitution_not_found_is_error(.true., ierror)
        ! clear the buffer so that envvars are not loaded redundantly

        call clear_all_buffers(ierror)          ! Clear the envvar buffer
        print *, "Read and processed text substitution (ENVVARS), reading all data from text"

!-----Do a second pass on all the input, making use of the text substitution we just prepped
        call set_active_profile(dsm2_name, ierror)          ! activate all keywords for the model
        call read_buffer_from_text(filename, ierror)        ! Perform the read into buffers
        call verify_error(ierror, "Error reading from text (full pass)")
        print *, "Read text into buffers"
        print *, "No of layers=", xsect_layer_buffer_size()
        call prioritize_all_buffers(ierror)                ! Enforce the "layering"
        call verify_error(ierror, "Error prioritizing buffers, sorting layers")
        print *, "Prioritized buffer"
        return
    end subroutine
!==================================================================

    subroutine write_input_buffers()
!     Writes in all text starting from input filename
        use input_storage_fortran
        use iopath_data
        use runtime_data
        use envvar
        implicit none
        integer :: ierror = 0
        logical :: append_text = .false.
!-----Write all buffers to text in the order they were defined
        if (io_files(dsm2_module, io_echo, io_write) .use) then
            append_text = .false.
            call write_buffer_profile_to_text(trim(dsm2_name), &
                                              trim(io_files(dsm2_module, &
                                                            io_echo, &
                                                            io_write) .filename), &
                                              append_text, ierror)
            call verify_error(ierror, "Error writing echoed text")
            print *, "text written"
        end if

        return
    end subroutine
!============================================================

!====================================================================
    subroutine process_initial_text

        use hdf5
        use input_storage_fortran
        use envvar
        implicit none
        integer :: nitem
        integer :: icount
        character * (32) name
        character * (64) value
        character * (32) envname
        character * (128) envval
        integer :: ierror = 0
        nitem = envvar_buffer_size()
        do icount = 1, nitem
            call envvar_query_from_buffer(icount, envname, envval, ierror)
            call add_envvar(envname, envval)
        end do
        print *, "Number of envvar: ", nitem

        nitem = scalar_buffer_size()
        do icount = 1, nitem
            call scalar_query_from_buffer(icount, name, value, ierror)
            call process_scalar(name, value)
        end do
        print *, "Number of scalars: ", nitem

        return
    end subroutine

end module mod_fixed
