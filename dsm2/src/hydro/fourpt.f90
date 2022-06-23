!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.

!    The Delta Simulation Model 2 (DSM2) is free software:
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!== Public  (FourPt) =================================================

module fourpt
    use iso_c_binding
    use hdf5, only: h5open_f
    use io_units, only: unit_screen, unit_error, unit_output
    use runtime_data, only: dsm2_module, dsm2_name, model_name, &
                            prev_julmin, julmin, start_julmin, &
                            current_date, time_step, &
                            display_intvl, flush_intvl, end_julmin, &
                            initialize_runtimes, get_command_args
    use iopath_data, only: hydro, io_hdf5, io_write, io_files, &
                           to_boundary, io_restart, check_input_data, &
                           need_tmp_outfiles, binary_output, &
                           max_dssinfiles, infilenames, &
                           ifltab_in, ifltab_out, &
                           dss_direct, max_dssoutfiles, outfilenames
    use grid_data, only: nreser, res_geom

    use chconnec, only: YResOld, Yres, VResOld, QResOld, QRes
    use chstatus, only: QOld, Q, &
                        InitializeChannelNetwork, WriteNetworkRestartFile
    use virt_xsect, only: virtual_xsect
    use netblnce, only: InitNetBalance, UpdateNetBalance
    use gate_calc, only: AssignGateCompPoints
    use netcntrl, only: TotalNetworkIterations, &
                        IncrementNetworkTimeStep, Restart_Write
    use solvealloc, only: InitializeSolver
    use netbnd, only: SetBoundaryValuesFromData
    use oprule_management, only: InitOpRules
    use tidefile, only: AverageFlow, &
                        InitHydroTidefile, WriteHydroToTidefile, &
                        DetermineFirstTidefileInterval
    use reservoirs, only: InitReservoirFlow
    use reservoir_geometry, only: calculateReservoirGeometry
    use channel_schematic, only: TotalStreamLocations
    use update_network, only: UpdateNetwork
    use utilities, only: jmin2cdt, incr_intvl
    implicit none

    !   Purpose:  Compute 1-dimensional streamflow in a network of open
    !             channels, in terms of discharge and water-surface
    !             elevation.
    !
    !             Optionally density is allowed to vary with time and
    !             distance.  Currently density at both ends of each channel
    !             must be read from a user-supplied file if this option is
    !             used.
    !
    !             Optionally sinuosity is allowed to vary with depth of flow
    !             and distance.  Sinuosity information is supplied through
    !             channel-properties tables.

    !   Version:  1993.01 (FORTRAN)

    !                      Lew DeLong
    !                      U.S.G.S., Water Resources Division
    !                      Office of Surface Water
    !                      Stennis Space Center, MS, 39529
    !                      January 1992
    !
    !                      David Thompson, formerly with U.S.G.S., currently
    !                      with Texas Tech University, is primarily responsible
    !                      for adding file utilities, buffered output of
    !                      time-series and space-series results, string
    !                      utilities, the master-file look-up module for
    !                      relating user-supplied file names and unit numbers
    !                      with internal defaults.
    !
    !                      Janice Fulford contributed routines necessary
    !                      to represent general 3-parameter ratings
    !                      capable of representing hydraulic structures.
    !
    !                      Other persons involved in the coding or
    !                      modification of code used by FourPt include
    !                      Barry Wicktom, Jenifer Johnson, Victoria Israel.
    !-----include '../input/time-varying/writedss.inc'

    !   Local variables:
    LOGICAL :: OK, isopen, echo_only, file_exists

    ! integer*4, external :: &
    !     incr_intvl          ! increment julian minute by interval function
    integer*4 &
        next_output_flush, &  ! next time to flush output
        next_display, &       ! next time to display model time
        next_restart_output   ! next time to write restart file

    integer &
        istat, &              ! status of fixed input
        i, j                  ! loop index

    character &
        init_input_file*130    ! initial input file on command line [optional]
    ! character, external :: &
    !     jmin2cdt*14            ! convert from julian minute to char date/time

    logical :: updated
    real*8 reser_area, reser_vol
    integer fstat
    !   Programmed by: Lew DeLong
    !   Date:          February 1991
    !   Modified by:   Lew DeLong
    !   Last modified: December 1992
    !   Version 93.01, January, 1993
    !   Last modified: October 1994 Parviz Nader DWR
    !   Last modified: September 1996 Ralph Finch DWR

    data init_input_file/' '/
contains
    subroutine prepare_hydro()
        !-----DSM2 module, name and version number
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

        !-----get optional starting input file from command line and
        !-----simulation name for Database read
    end subroutine prepare_hydro

    subroutine fourpt_init()
        !-----dsm2 initialization
        call dsm2_hydro_init

        !---- hdf5 api on
        call h5open_f(istat)

        if (.Not. InitOpRules()) Then
            write (unit_error, *) &
                ' Initialization of Gate Ops failed...'
            call exit(1)
        end if

        !---- begin data reading

        !---- read all text into buffers and process envvironmental variables
        if (init_input_file .ne. ' ') then
            inquire (file=init_input_file, exist=file_exists)
            if (.not. file_exists) then
                write (unit_error, *) "Input file does not exist: ", init_input_file
                call exit(1)
            end if
            call input_text(init_input_file)  ! reads and echoes text
            call process_initial_text()       ! process scalar and envvars
            call initialize_runtimes()
            call buffer_input_grid()    ! processes grid
        end if

        !------ process input that is in buffers
        call buffer_input_common()
        call buffer_input_hydro()

        call process_text_gate_input()
        call process_text_oprule_ts_input()
        call write_input_buffers()
        if (echo_only) call exit(1)

        !------ end of input reading and echo, start checking data

        call check_fixed(istat)
        if (istat .ne. 0) then
            write (unit_error, *) &
                'Error in checking fixed data; run stopped.'
            call exit(1)
        end if

        call virtual_xsect        ! create virtual cross-section lookup table

        call check_fixed_hydro(istat)
        if (istat .ne. 0) then
            write (unit_error, *) &
                'Error in checking fixed fourpt data; run stopped.'
            call exit(1)
        end if

        prev_julmin = 0
        julmin = start_julmin
        current_date = jmin2cdt(julmin)

        !-----calculate julian minute of end of each DSS interval
        call update_intervals

        if (.Not. InitializeChannelNetwork()) Then
            write (unit_error, *) &
                ' Initialization of Channel network failed...'
            call exit(1)
        end if

        call AssignGateCompPoints() ! Attach gates to hydro computational points

        if (.Not. InitReservoirFlow()) Then
            write (unit_error, *) &
                ' Initialization of Reservoir flow failed...'
            call exit(1)
        end if

        ! Initialize time series with data from initial time
        ! todo: make sure this behaves with reservoirs
        OK = SetBoundaryValuesFromData()

        ! Oprules cannot be parsed until channel network is defined
        call process_text_oprule_input()

        if (.not. InitializeSolver()) THEN
            write (unit_error, *) &
                ' Initialization of SPARSE matrix solver failed...'
            call exit(1)
        end if

        OK = InitNetBalance()
        call init_store_outpaths(istat)

        if (io_files(hydro, io_hdf5, io_write) .use) then ! hydro binary file output
            call DetermineFirstTidefileInterval()
            OK = InitHydroTidefile()
            !--special treatment to avoid averaging in the begining
            julmin = julmin - time_step
            OK = AverageFlow()
            julmin = julmin + time_step

            OK = WriteHydroToTidefile(.TRUE.)
        end if

605     format('Starting DSM2-Hydro at time: ', a)
        write (unit_output, 605) current_date
        write (unit_screen, 605) current_date
        call store_outpaths(.false.)
        next_display = incr_intvl(start_julmin, display_intvl, TO_BOUNDARY)
        next_output_flush = incr_intvl(start_julmin, flush_intvl, TO_BOUNDARY)
        if (io_files(hydro, io_restart, io_write) .use) then
            next_restart_output = incr_intvl(start_julmin, io_files(hydro, &
                                                                    io_restart, io_write) .interval, TO_BOUNDARY)
        end if

        prev_julmin = julmin
        julmin = julmin + time_step
        current_date = jmin2cdt(julmin)
    end subroutine fourpt_init

    subroutine fourpt_step()
        DO I = 1, TotalStreamLocations()
            QOld(I) = Q(I)
        END DO

        DO i = 1, Nreser
            YResOld(i) = YRes(i)
            call calculateReservoirGeometry(i, Yres(i), &
                                            reser_area, reser_vol)
            VResOld(i) = reser_vol
            DO j = 1, res_geom(i) .nnodes
                QResOld(i, j) = QRes(i, j)
            END DO
        END DO

        OK = IncrementNetworkTimeStep()
        !--------calculate julian minute of end of each DSS interval
        !call update_intervals

        if (julmin .ge. next_display) then
610         format('Starting Hydro computations for time: ', a)
            write (unit_output, 610) current_date
            write (unit_screen, 610) current_date
            next_display = incr_intvl(next_display, display_intvl, &
                                      TO_BOUNDARY)
        end if

        if (check_input_data) then
            !-----------just check input data for bogus values; no simulation
            OK = SetBoundaryValuesFromData()
        else                   ! full simulation
            updated = UpdateNetwork()
            IF (Updated) THEN
                OK = UpdateNetBalance()
            else
                write (unit_error, *) &
                    ' Network update failed at time ', current_date
                write (unit_error, *) ' Abnormal program end.'
                call exit(1)
            end if

            if (julmin .ge. next_output_flush) then
                next_output_flush = incr_intvl(next_output_flush, &
                                               flush_intvl, TO_BOUNDARY)
                call store_outpaths(.true.)
            else
                call store_outpaths(.false.)
            end if

            if (io_files(hydro, io_hdf5, io_write) .use) then
                OK = AverageFlow()
                OK = WriteHydroToTidefile(.FALSE.)
            end if

            if (io_files(hydro, io_restart, io_write) .use) then
                if (Restart_Write .and. julmin .ge. next_restart_output) then
                    ! Write the hydrodynamic information at the end of
                    ! every interval to ascii file in case of any
                    ! interruptions to the model
                    next_restart_output = incr_intvl(next_restart_output, &
                                                     io_files(hydro, io_restart, io_write) .interval, &
                                                     TO_BOUNDARY)
                    OK = WriteNetworkRestartFile()
                end if

                if (.not. restart_write) then
                    next_restart_output = incr_intvl(start_julmin, io_files(hydro, &
                                                                            io_restart, io_write) .interval, TO_BOUNDARY)
                    restart_write = .true.
                    !  todo: if the model is working, this next line should be removed
                    !io_files(hydro,io_hdf5,io_write).use=.true.
                end if
            end if
        end if

        prev_julmin = julmin
        julmin = julmin + time_step
        current_date = jmin2cdt(julmin)
    end subroutine fourpt_step

    subroutine fourpt_winddown()
        if (julmin .gt. end_julmin) then
            julmin = prev_julmin
            prev_julmin = prev_julmin - time_step
            current_date = jmin2cdt(julmin)
        end if

        if (.not. check_input_data) then
            if (io_files(hydro, io_restart, io_write) .use) then
                !-----------Write network restart file.
                OK = WriteNetworkRestartFile()
            end if

            !--------Write time-series network results.
            call store_outpaths(.true.) ! flush temp files
            if (need_tmp_outfiles .and. &
                .not. binary_output) call wrt_outpaths
        end if

        !--------close HDF5
        if (io_files(hydro, io_hdf5, io_write) .use) then
            call CloseHDF5()
        end if

        !-----close all DSS input files
        i = 1
        do while (i .le. max_dssinfiles .and. &
                  infilenames(i) .ne. ' ')
            call zclose(ifltab_in(1, i))
            i = i + 1
        end do

        if (dss_direct) then
            !--------close all DSS output files
            i = 1
            do while (i .le. max_dssoutfiles .and. &
                      outfilenames(i) .ne. ' ')
                call zclose(ifltab_out(1, i))
                i = i + 1
            end do
        end if

        !-----Compute and report final volume and mass balances.
        !--         OK = ReportNetBalance()

900     WRITE (unit_screen, *) '   -----------------------------'
        WRITE (unit_screen, *) ' '
        WRITE (unit_screen, *) ' ', &
            TotalNetworkIterations(), ' total network iterations...'

        WRITE (unit_screen, *) ' '
        WRITE (unit_screen, *) '   Normal program end.'
        WRITE (unit_screen, *) ' '
        WRITE (unit_screen, *) '   -----------------------------'

        WRITE (unit_output, *) '   -----------------------------'
        WRITE (unit_output, *) ' '
        WRITE (unit_output, *) &
            TotalNetworkIterations(), '  total network iterations...'
        WRITE (unit_output, *) ' '
        WRITE (unit_output, *) ' Normal program end.'
        WRITE (unit_output, *) &
            '   -----------------------------'

        !      OK = CloseSolver()

        inquire (unit_output, opened=isopen)
        if (isopen) close (unit_output, err=1222)
        inquire (unit_screen, opened=isopen)
        if (isopen) close (unit_screen, err=1222)

1222    call exit(0)
    end subroutine fourpt_winddown

    subroutine fourpt_main()
        call fourpt_init()

        do while (julmin .le. end_julmin) ! normal time run
            call fourpt_step()
        end do
        call fourpt_winddown()
    end subroutine fourpt_main

end module fourpt
