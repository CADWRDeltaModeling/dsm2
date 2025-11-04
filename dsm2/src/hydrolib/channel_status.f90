!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

!===== BOF chstatus.inc ==================================================
!   Version 93.01, January, 1993

module chstatus
    use constants, only: MAXQUADPTS
    use array_limits
    use network
    use linear, only: Linear1D
    implicit none
    double precision,target,save :: WS(MAX_LOCATIONS)
    double precision,target,save :: Q(MAX_LOCATIONS)
    double precision,target,save :: H(MAX_LOCATIONS)
    double precision, save:: Rho1(MAX_LOCATIONS), Rho2(MAX_LOCATIONS)
    double precision, save:: QOld(MAX_LOCATIONS)
contains

    !***********************************************************************
    !
    !    This file is a FORTRAN module containing 1-D channel flow values,
    !    including density, for networks of open channels.
    !
    !
    !     Module note: Only functions or subroutines marked "public" should be
    !                   used outside of this module as those marked "private"
    !                   may not be supported by future revisions of this module
    !                   or replacement modules.  Likewise, no data or common
    !                   blocks contained within this module should be accessed
    !                   by routines outside of this module accept through the
    !                   use of "public" functions or subroutines contained
    !                   within this module.
    !
    !     Non-standard usage: Symbolic names in this module may be represented by
    !                         as many as 31 characters in order to provide better
    !                         definition directly in the code.  Standard FORTRAN
    !                         allows only 6 characters, but this restriction is
    !                         generally extended to 32 characters by most compilers.
    !
    !
    !   Public functions:
    !
    !     REAL FUNCTION StreamFlow(LocationNumber)
    !            - returns volumetric discharge.
    !
    !     LOGICAL FUNCTION SetStreamFlow(LocationNumber, Value)
    !            - store volumetric discharge.
    !
    !     REAL FUNCTION StreamDepth(LocationNumber)
    !            - return depth of flow.
    !
    !     LOGICAL FUNCTION SetStreamDepth(LocationNumber, Value)
    !            - store depth of flow.
    !
    !     REAL FUNCTION StreamSurfaceElevation(LocationNumber)
    !            - return watersurface elevation.
    !
    !     LOGICAL FUNCTION SetStreamSurfaceElevation(LocationNumber, Value)
    !            - store watersurface elevation.
    !
    !      REAL FUNCTION OldStreamDensity(LocationNumber)
    !             - returns water density at LocationNumber,
    !               at the beginning of the current time step.
    !
    !      REAL FUNCTION NewStreamDensity(LocationNumber)
    !             - returns water density at LocationNumber,
    !               at the end of the current time step.
    !
    !      REAL FUNCTION EstOldStreamDensity(DownStreamDistance)
    !             - returns interpolated density at DownStreamDistance,
    !               at the begining of the current time step.
    !
    !      REAL FUNCTION EstOldStreamDensity(DownStreamDistance)
    !             - returns interpolated density at DownStreamDistance,
    !               at the begining of the current time step.
    !
    !      LOGICAL FUNCTION SetNewLinearStreamDensity(
    !                         UpstreamDensity, DownstreamDensity
    !                                             )
    !             - sets density at computational locations, at the
    !               end of the current time step.
    !
    !      LOGICAL FUNCTION SetConstantStreamDensity()
    !             - sets stream density to 1.0.
    !
    !      LOGICAL FUNCTION SetNewLinearStreamDensity(
    !                         UpstreamDensity, DownstreamDensity
    !                                             )
    !             - sets density at computational locations, at the
    !               end of the current time step.
    !
    !
    !   Arguments:
    !     LocationNumber - computational location index, begining with
    !                      1 in the current channel.
    !     Value          - value of the specific variable to be stored.
    !     UpstreamDensity - density at the upstream end of current channel.
    !     DownstreamDensity - density at downstream end of current channel.
    !     DownstreamDistance - distance from upstream end of current channel.
    !     LocationNumber - computational cross-section sequence number,
    !                       begining with 1 at upstream end of current
    !                       channel.
    !
    !
    !   Module data:
    !
    !    'network.inc'
    !     MaxChannels - maximum number of channels.
    !     NumCh - current number of channels.
    !     Branch - current selected or active channel.
    !     MaxLocations - maximum number of locations (computational or user).
    !
    !    'chstatus.inc'
    !   Definitions:
    !     WS(i) - water surface elevation at computational location "i".
    !     Q(i) - volumetric discharge at computational location "i".
    !     H(i) - depth of flow at computational location "i".
    !     Rho(i) - density at computational location "i".
    !
    !
    !***********************************************************************

    !== Public (WriteNetworkRestartFile) ===================================

    logical function WriteNetworkRestartFile()
        use IO_Units
        use runtime_data
        use iopath_data
        use grid_data
        use channel_schematic
        use chconnec
        use netcntrl
        !use netbnd, only: reservoir_source_sink
        use fileutil, only: OpenNewText
        implicit none

        !   Purpose:  Write current values of dependent and independent
        !             variables to a file that may be used as initial
        !             conditions to restart the model.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:
        integer      fUnit, I, J
        character*130 FileName
        logical      OK


        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          Dec   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        WriteNetworkRestartFile = .false.

        !-----Open restart file.

        fUnit = io_files(hydro,io_restart,io_write)%unit
        FileName = io_files(hydro,io_restart,io_write)%filename
        if (.not. OpenNewText( fUnit, FileName ))  then
            write(UNIT_ERROR,*) ' ####Error(WriteNetworkRestartFile)'
            write(UNIT_ERROR,*) ' Failed to open restart file: ',FileName
            call exit(2)
        end if

        write(funit,900)dsm2_version,current_date
900     format('Hydro Version ',a, &
            /'The following data corresponds to   ',a14//)
        write(fUnit,*) NumberOfChannels(),'/Channels'

        !-----Loop on channels.

        do 200 I=1,NumberOfChannels()

            if( OpenChannel(I) ) then

                write(fUnit,*) chan_geom(I)%chan_no, '/Channel' ! write external number
                write(fUnit,*) NumberOfStreamLocations(),'/Locations'

                do 100 J=1,NumberOfStreamLocations()

                    !--------------WRITE(fUnit,'(3(F16.3,1X),A20)')
                    write(fUnit,'(F16.3,1X,F16.7,1X,F16.3,1X,A20)') &
                        StreamDistance(J), StreamSurfaceElevation(J), StreamFlow(J), &
                        '/Distance, WSElev, Q'

100             continue

                OK = CloseChannel()

            else
                write(UNIT_ERROR,*) ' ####Error(WriteNetworkRestartFile)'
                write(UNIT_ERROR,*) ' Could not open channel ',I
                return
            end if

200     continue

        write(funit,901)Nreser
901     format(/i5,' /Number of Reservoir')
        do I=1,Nreser
            !--------Reservoir Stage, nodal flows
            write(funit,902)i,res_geom(i)%nnodes,Yres(i)
902         format(I5,' /Reservoir Number' &
                /I5,' /Connections' &
                /5X,1P,E15.7,' /Yres')
            write(funit,903)(J,QRes(I,J),J=1, res_geom(i)%nnodes)
903         format(I5,1P,E15.7,' /Connection, Qres')
        enddo

        close( fUnit )

        WriteNetworkRestartFile = .true.

        return
    end function

    !== Private (ReadNetworkInitialConditions) ==============================

    logical function ReadNetworkInitialConditions()
        use IO_Units
        use runtime_data
        use grid_data
        use iopath_data
        use network
        use chconnec
        use chnlcomp
        use chinitcd
        use channel_schematic
        use fileutil, only: OpenOldText
        implicit none

        !   Purpose:  Read initial values of dependent variables.

        !   Arguments:

        !   Argument definitions:

        !   Module data:


        !   Local Variables:
        integer      Channels,extchan,intchan
        integer      fUnit, I, K, J, IRes, NConnect, IConnect, nLoc
        character*130 FileName

        !   Routines by module:

        !**** Local:
        integer  NresStart_File
        character*80 Header



        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          Dec   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        ReadNetworkInitialConditions = .false.

    

        !-----Open initial-condition file.

        fUnit = io_files(hydro,io_restart,io_read)%unit
        FileName = io_files(hydro,io_restart,io_read)%filename
        if( OpenOldText( fUnit, FileName ) ) then
        else
            write(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
            write(UNIT_ERROR,*) ' Could not open file...',FileName
            return
        end if

        !-----Begin reading of data.

        !-----read header, then test to see if it's really the header (old restart file)
        !-----or the restart file version (new file)
        restart_version=' '
        read(funit,'(a)') header
        if (header(:14) == 'Hydro Version') then
            restart_version=header(15:)
            read(funit,'(a)') header
        endif
        read(fUnit,*) Channels
        if( Channels /= NumberOfChannels() ) then
            write(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
            write(UNIT_ERROR,*) ' Number of channels in restart file'
            write(UNIT_ERROR,*) ' not the same as number of configured channels...'
            write(UNIT_ERROR,*) ' ', Channels,' <> ', NumberOfChannels()
            return
        end if
    
        if(not(allocated(Locations)))allocate(Locations(Channels+1))
        if(not(allocated(InitialConditionIndex)))allocate(InitialConditionIndex(Channels+1))

        !-----Initialize channel-number cross-reference numbers.
        do 50 I=1,Channels
            InitialConditionIndex(I) = 0
50      continue

        
        K = 0
        do 200 I=1,Channels

            read(fUnit,*) extchan  ! external channel number
            intchan=i
            if (chan_geom(i)%chan_no /= extchan)then
                write(unit_error)"Channel numbers in restart do not correspond with the ones in this simulation"
                write(unit_error)"Restart channel number: ",extchan
                call exit(-2)
            end if

            read(fUnit,*) nLoc            ! Number of comp. points in channel, according to restart file
            if (nLoc /= NumberofCompLocations(intchan)) then
                write(unit_error,610) extchan, nLoc, &
                    NumberofCompLocations(i)

610             format(/'####Error(ReadNetworkInitialConditions)' &
                    'For channel ',i3, &
                    /'Number of restart file computational locations (', &
                    i2, ')' &
                    /'not equal to number of locations in run (', &
                    i2, ').' &
                    /'Probably caused by different DELTAX value in SCALAR input section.'/)
                return
            endif

            Locations(I) = nLoc
            if( ( K + nLoc ) <= MAX_LOCATIONS ) then

                FirstLocation(I) = K + 1
                do 100 J=1,nLoc

                    K = K + 1
                    read(fUnit,*) &
                        InitialX(K), InitialWS(K), InitialQ(K)

100             continue
            else
                write(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
                write(UNIT_ERROR,*) ' Reading initial conditions for channel...', &
                    extchan
                write(UNIT_ERROR,*) ' Maximum number of loactions exceeded.'
                write(UNIT_ERROR,*) ' Attempted...', K + Locations(I)
                write(UNIT_ERROR,*) ' Allowed.....', MAX_LOCATIONS
                return
            end if
            InitialConditionIndex( intchan ) = I
200     continue

        read(funit,*)NresStart_File
        if(NresStart_File==Nreser) then
            if (restart_version /= ' ') then
                !-----------restart file version supports reservoir connection flows
                !-- fixme: this is a bad idea. Should include just the model state (Yres)
                !          and then calculate derived variables.
                do I=1,Nreser
                    read(funit,*)IRes ! reservoir number
                    read(funit,*)NConnect   ! number of connections
                    read(funit,*)Yres(IRes) ! reservoir stage
                    if (NConnect == res_geom(ires)%nnodes) then
                        do K=1, res_geom(ires)%nnodes
                            read(funit,*)IConnect,QRes(IRes,IConnect)
                        enddo
                    else
                        write(UNIT_ERROR,901) IRes
901                     format('Error(ReadNetworkInitialConditions)' &
                            /'Number of Reservoir Connections does not match with the Restart File:' &
                            /'Reservoir ',I5)
                        return
                    endif
                end do
            else                   ! no reservoir connection flows
                do I=1,Nreser
                    read(funit,*)IRes,Yres(IRes)
                end do
            end if
        else
            write(UNIT_ERROR,902)
902         format('Error(ReadNetworkInitialConditions)'/ &
                'Number of Reservoirs does not match with the Restart File'/)
            return
        endif

        !          No interpolation is necessary, since the computational points are matched exactly

        ReadNetworkInitialConditions = .true.

        return
    end function

    !== Private (ApproxReadInitialConditions) ==============================

    logical function ApproxReadInitialConditions()
        use IO_Units
        use netcntrl
        use channel_schematic, &
            only: DownstreamPointer, UpstreamPointer, NumberOfStreamLocations, &
            CurrentChannel, StreamDistance
        use chinitcd
        use channel_xsect_tbl, only: btmelev
        implicit none

        !   Purpose:  Approximate initial values of dependent variables
        !             for current channel from values read from an
        !             initial-condition file.

        !   Arguments:

        !   Argument definitions:


        !   Local Variables:
        integer I, J, K
        real*8    CompLocation_lcl(MAX_LOCATIONS)
        logical UpstreamFlag, DownstreamFlag



        !   Intrinsics:
        integer   INT
        intrinsic INT
        !   Suspected not to be used at all.
        !   Programmed by: Lew DeLong
        !   Date:          Dec   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        ApproxReadInitialConditions = .false.

        !-----Set sequence number for initial conditions, check validity.

        I = InitialConditionIndex( CurrentChannel() )

        if( I > 0 ) then
        else
            write(UNIT_ERROR,*) ' ####Error(ApproxReadInitialConditions)'
            write(UNIT_ERROR,*) ' Initial conditions not available for channel', &
                CurrentChannel()
            return
        end if

        !-----Check that initial conditions exist for first and last
        !     cross section of channel.

        if( INT( 100.0 * StreamDistance(1) ) &
            == &
            INT( 100.0 * InitialX( FirstLocation(I) ) ) )  then

            UpstreamFlag = .true.

        else
            UpstreamFlag = .false.
        end if

        if( INT( 100.0 * StreamDistance(NumberOfStreamLocations() ) ) &
            == &
            INT( 100.0 * InitialX( FirstLocation(I)+Locations(I)-1) ) ) &
            then

            DownstreamFlag = .true.

        else
            DownstreamFlag = .false.
        end if

        !-----If bad match, report errors and RETURN.

        if( .not. UpstreamFlag .or. .not. DownstreamFlag ) then

            write(UNIT_ERROR,*) ' ####Error(ApproxReadInitialConditions)'
            write(UNIT_ERROR,*) ' Location of channel extremities do not match.'
            write(UNIT_ERROR,*) ' Channel...', CurrentChannel()

            if( .not. UpstreamFlag ) then
                write(UNIT_ERROR,*) ' Upstream, expected...........', &
                    StreamDistance(1)
                write(UNIT_ERROR,*) ' Initial-condition location...', &
                    InitialX( FirstLocation(I) )
            end if

            if( .not. DownstreamFlag ) then
                write(UNIT_ERROR,*) ' Downstream, expected.........', &
                    StreamDistance(NumberOfStreamLocations())
                write(UNIT_ERROR,*) ' Initial-condition location...', &
                    InitialX( FirstLocation(I)+Locations(I)-1 )
            end if

            return

        end if

        !-----Get computational stream locations.

        K = 0
        do 50 J=UpstreamPointer(),DownstreamPointer()
            K = K + 1
            CompLocation_lcl(J) = StreamDistance(K)
50      continue

        !-----Approximate streamflow.

        call Linear1D( &
            NumberOfStreamLocations(), &
            CompLocation_lcl(UpstreamPointer()), &
            Locations(I), InitialX( FirstLocation(I) ), &
            InitialQ( FirstLocation(I) ), &
            Q(UpstreamPointer()) &
            )

        !-----Approximate watersurface elevation.

        call Linear1D( &
            NumberOfStreamLocations(), &
            CompLocation_lcl(UpstreamPointer()), &
            Locations(I), InitialX( FirstLocation(I) ), &
            InitialWS( FirstLocation(I) ), &
            WS(UpstreamPointer()) &
            )

        !-----Approximate depth of flow.

        do 100 J=UpstreamPointer(),DownstreamPointer()

            H( J ) = WS( J ) - BtmElev( CompLocation_lcl( J ) )

100     continue

        ApproxReadInitialConditions = .true.

        return
    end function

    !== Private (SetNetworkInitCndRead) ================================================

    logical function SetNetworkInitCndRead( State )

        use chinitcd
        implicit none

        !   Purpose:  Set state of network initial-condition reading.
        !             [.TRUE. ] if read, or
        !             [.FALSE.] if not read.

        !   Arguments:
        logical State

        !   Argument definitions:
        !     State - .TRUE. if read, .FALSE. otherwise.
        !   Local Variables:

        !   Routines by module:

        !**** Local:

        !   Intrinsics:

        !   Programmed by: Lew DeLong
        !   Date:          Dec   1992
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        InitCndInitialized = State
        SetNetworkInitCndRead = .true.

        return
    end function

    !== Public (StreamFlow) ================================================

    real*8 function StreamFlow(LocationNumber)
        use IO_Units
        use channel_schematic, &
            only: UpstreamPointer, CurrentChannel, CheckChannelCompLocationRange
        implicit none

        !   Purpose:  Return current value of stream flow in the current channel
        !             at a location corresponding to the index LocationNumber.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location sequence number within
        !                      current channel.

        !   Module data:

        !   Local Variables:
        integer J

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        !      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

        StreamFlow = Q( J )

        !      ELSE

        !         WRITE(UNIT_ERROR,*) ' Range error...(StreamFlow)'
        !         WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
        !         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
        !         CALL EXIT(1)

        !      END IF

        return
    end function

    !== Public (GlobalStreamFlow) ==========================================

    real*8 function GlobalStreamFlow(LocationNumber)


        implicit none

        !   Purpose:  Return current value of stream flow
        !             at a location corresponding to the
        !             index LocationNumber.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location sequence number within
        !                      current channel.


        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        GlobalStreamFlow = Q( LocationNumber )

        return
    end function

    !== Public (SetStreamFlow) =============================================

    logical function SetStreamFlow(LocationNumber, Value)
        use IO_Units
        use channel_schematic, &
            only: UpstreamPointer, CheckChannelCompLocationRange
        implicit none

        !   Purpose:  Set current value of stream flow in the current channel
        !             at a location corresponding to the index LocationNumber.

        !   Arguments:
        integer LocationNumber
        real*8    Value

        !   Argument definitions:
        !     LocationNumber - computational-location sequence number within
        !                      current channel.
        !     Value  - value to be stored.



        !   Local Variables:
        integer J

        !   Routines by module:
        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        !     IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

        Q(J) = Value
        SetStreamFlow = .true.

        !    ELSE

        !         WRITE(UNIT_ERROR,*) ' Range error...(SetStreamFlow)'
        !         SetStreamFlow = .False.

        !      END IF

        return
    end function

    !== Public (StreamDepth) ===============================================

    real*8 function StreamDepth(LocationNumber)
        use IO_Units
        use channel_schematic, &
            only: UpstreamPointer, CheckChannelCompLocationRange
        implicit none

        !   Purpose:  Return current value of depth of flow in the current
        !             channel at a location corresponding to the index
        !             LocationNumber.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location number (index) within
        !                      current channel.

        !   Local Variables:
        integer J

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        !     IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

        StreamDepth = H( J )

        !      ELSE

        !        WRITE(UNIT_ERROR,*) ' Range error...(StreamDepth)'
        !         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
        !         CALL EXIT(1)

        !      END IF

        return
    end function

    !== Public (GlobalStreamDepth) =========================================

    real*8 function GlobalStreamDepth(LocationNumber)

        implicit none

        !   Purpose:  Return current value of depth of flow
        !             at a location corresponding to the index
        !             LocationNumber.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location number (index) within
        !                      current channel.

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        GlobalStreamDepth = H( LocationNumber )

        return
    end function

    !== Public (SetStreamDepth) ============================================

    logical function SetStreamDepth(LocationNumber, Value)
        use IO_Units
        use channel_schematic, &
            only: UpstreamPointer, CheckChannelCompLocationRange
        implicit none

        !   Purpose:  Set current value of depth of flow in the current
        !             channel at a location corresponding to the index
        !             LocationNumber.

        !   Arguments:
        integer LocationNumber
        real*8    Value

        !   Argument definitions:
        !     LocationNumber - computational-location number (index) within
        !                      current channel.
        !     Value - current value to be set.


        !   Local Variables:
        integer J

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        !      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

        H( J ) = Value

        SetStreamDepth = .true.

        !      ELSE
        !         WRITE(UNIT_ERROR,*) ' Range error...(SetStreamDepth)'
        !         SetStreamDepth = .FALSE.
        !      END IF

        return
    end function

    !== Public (StreamSurfaceElevation) ====================================

    real*8 function StreamSurfaceElevation(LocationNumber)
        use IO_Units
        use channel_schematic, &
            only: UpstreamPointer, CurrentChannel, CheckChannelCompLocationRange
        implicit none

        !   Purpose:  Return current value of water-surface elevation in the
        !             current channel at a location corresponding to the index
        !             LocationNumber.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location number (index) within
        !                      current channel.

        !   Local Variables:
        integer J

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1

        !     IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

        StreamSurfaceElevation = WS( J )

        !     ELSE
        !        WRITE(UNIT_ERROR,*) ' Range error...(StreamSurfaceElevation)'
        !        WRITE(UNIT_ERROR,*) ' Abnormal program end.'
        !        CALL EXIT(1)
        !     END IF

        return
    end function

    !== Public (GlobalStreamSurfaceElevation) ==============================

    real*8 function GlobalStreamSurfaceElevation(LocationNumber)

        implicit none

        !   Purpose:  Return current value of water-surface elevation
        !             at a location corresponding to the index
        !             LocationNumber.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location number (index).

        !   Local Variables:

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        GlobalStreamSurfaceElevation = WS( LocationNumber )

        return
    end function

    !== Public (SetStreamSurfaceElevation) =================================

    logical function SetStreamSurfaceElevation &
        (LocationNumber, Value)
        use IO_Units
        use channel_schematic, &
            only: UpstreamPointer, StreamDistance, CheckChannelCompLocationRange
        use channel_xsect_tbl, &
             only: BtmElev, BtmElevAtLocationNumber
        implicit none

        !   Purpose:  Set current value of water-surface elevation in the
        !             current channel at a location corresponding to the index
        !             LocationNumber.  Also, set corresponding depth of flow.

        !   Arguments:
        integer LocationNumber
        real*8    Value

        !   Argument definitions:
        !     LocationNumber - computational-location number (index) within
        !                      current channel.
        !     Value  - value to be stored.

        !   Local Variables:
        integer J

        !   Routines by module:


        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        !      IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

        WS( J ) = Value
        H( J ) = Value - BtmElevAtLocationNumber(LocationNumber)

        SetStreamSurfaceElevation = .true.

        !      ELSE
        !         WRITE(UNIT_ERROR,*) ' Range error...(SetStreamSurfaceElevation)'
        !         SetStreamSurfaceElevation = .FALSE.
        !      END IF

        return
    end function

    !== Public (InitializeNetworkFlowValues) ===============================

    logical function InitializeNetworkFlowValues()
        use PhysicalConstants, only: gravity
        use IO_Units
        use grid_data
        use netcntrl

        use chinitcd
        use chnluser
        use chconnec

        use channel_schematic, &
            only :  NumberOfStreamLocations, &
            UpstreamPointer, DownstreamPointer, &
            StreamDistance, &
            NumberOfChannels, &
            OpenChannel, CloseChannel, &
            GetUserStreamflow, GetUserStreamSurfaceElevation

        use channel_xsect_tbl, &
            only: BtmElev, CxArea, ChannelWidth, &
            Conveyance, dConveyance
        implicit none

        !   Purpose:  Set initial values of water-surface elevation and flow
        !             in a network of channels.

        !   Arguments:

        !   Argument definitions:

        !   Local Variables:
        integer I, J, K
        integer Channelnumber_L, UserLocations
        real*8    Velocity, delx, CrNo, dtr, G, FrNo
        real*8    CompLocation_lcl(MAX_LOCATIONS)
        real*8    WidthRatio, Width, WSSlope, WSSlopeRatio, WSSlopeChange
        logical OK

        !     CompLocation_lcl(j) - local downstream distance coordinate.
        !     DummyArray(j) - a REAL value dependent upon context.
        !     DummyArray2(j) - a REAL value dependent upon context.
        !     DummyCharArray(j) - a CHARACTER value dependent upon context.

        !   Routines by module:

        !**** Local:
        integer FirstLoc

        !   Intrinsics:
        real*8      SQRT, ABS
        intrinsic SQRT, ABS

        !   Programmed by: Lew DeLong
        !   Date:          Nov   1990
        !   Modified by:   Lew DeLong
        !   Last modified: August   1993
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        InitializeNetworkFlowValues = .false.

        dtr = DFLOAT( NetworkTimeIncrement() )
        G = gravity

        !-----Set initial-condition read flag to "not initialized".

        OK = .false.
        OK = SetNetworkInitCndRead( OK )

        if (Restart_Read) then    ! Read initial conditions from restart file
            if( ReadNetworkInitialConditions() ) then
                InitCndInitialized = .true.
                Q=InitialQ
                QOld=InitialQ
                WS=InitialWS
                YResOld=YRes
                CompLocation_lcl = InitialX
            else
                write(UNIT_ERROR,*) ' ####Error(ReadNetworkInitialConditions)'
                write(UNIT_ERROR,*) ' Reading of initial conditions failed...'
                return
            end if
        else                      ! Interpolate user points
            do 100 I = 1, NumberOfChannels()
                Channelnumber_L = I
                if( OpenChannel( Channelnumber_L )  ) then

                    !--------------Get computational stream locations.
                    K = 0
                    do J=UpstreamPointer(),DownstreamPointer()
                        K = K + 1
                        CompLocation_lcl(J) = StreamDistance(K)
                    end do

                    !--------------Initial conditions approximated from user input.

                    UserLocations = NUserInitLocations(branch)
                    FirstLoc=FirstLocation(branch)

                    if (FirstLoc == 0) then ! no initial conditions at all
                        write(unit_error,*)'No default initial conditions or restart file'
                        return
                    end if

                    !--------------Approximate streamflow and ws linearly from user input.

                    call Linear1D( &
                        NumberOfStreamLocations(), &
                        CompLocation_lcl(UpstreamPointer()), &
                        UserLocations, InitialX(FirstLoc), InitialQ(FirstLoc), &
                        Q(UpstreamPointer()) &
                        )

                    call Linear1D( &
                        NumberOfStreamLocations(), &
                        CompLocation_lcl(UpstreamPointer()), &
                        UserLocations, InitialX(FirstLoc), InitialWS(FirstLoc), &
                        WS(UpstreamPointer()) &
                        )
                    OK = CloseChannel()
                end if

100         continue
        end if



        do i=1,MAX_LOCATIONS
            QOld(i)=Q(i)
        end do
        do i=1,NReser
            YResOld(i)=YRes(i)
        end do



        do 200 I = 1, NumberOfChannels()

            Channelnumber_L = I

            if( OpenChannel( Channelnumber_L )  ) then
                !-----------Approximate depth of flow.

                do  J=UpstreamPointer(),DownstreamPointer()
                    H( J ) = WS( J ) - BtmElev( CompLocation_lcl( J ) )
                end do


                !-----------if (Restart_Read)
                !-----------&           OK = ApproxReadInitialConditions()               WHY??????!!!!!!

                !-----------Upstream end of channel.     WHY???????/

                K = 1
                J = UpstreamPointer()
                Velocity = Q(J) / CxArea( CompLocation_lcl(J), WS( J ) )
                delX = CompLocation_lcl(J+1) - CompLocation_lcl(J)
                WSSlope = ( WS(J+1) - WS(J) ) / delX

                FrNo = Velocity / SQRT( G * H(J) )
                CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / delX
                Width = ChannelWidth( CompLocation_lcl(J), WS( J ) )

                !-----------Intervening cross sections.

                if( (DownstreamPointer() - UpstreamPointer()) > 2) then
                    do 150 J=UpstreamPointer()+1,DownstreamPointer()-1
                        K = K + 1
                        Velocity = Q(J) / CxArea( CompLocation_lcl(J), WS( J ) )
                        delX = ( CompLocation_lcl(J+1) - CompLocation_lcl(J-1) )
                        WSSlope = ( WS(J+1) - WS(J-1) ) / delX
                        CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / ( 0.5 * delX )
                        FrNo = Velocity / SQRT( G * H(J) )
                        delX   = CompLocation_lcl(J+1) - CompLocation_lcl(J)
                        if( ABS( ( WS(J) - WS(J-1) ) ) > 1.0e-10 ) then
                            WSSlopeRatio = ( WS(J+1) - WS(J) ) / ( WS(J) - WS(J-1) ) &
                                * ( CompLocation_lcl(J) - CompLocation_lcl(J-1) ) &
                                / ( CompLocation_lcl(J+1) - CompLocation_lcl(J) )
                        else
                            WSSlopeRatio = 0.0
                        end if
                        WSSlopeChange = ( WS(J+1) - WS(J) ) &
                            / ( CompLocation_lcl(J+1) - CompLocation_lcl(J) ) &
                            - ( WS(J) - WS(J-1) ) &
                            / ( CompLocation_lcl(J) - CompLocation_lcl(J-1) )
                        Width = ChannelWidth( CompLocation_lcl(J), WS( J ) )
                        WidthRatio = ChannelWidth( CompLocation_lcl(J+1), WS(J+1) ) &
                            / ChannelWidth( CompLocation_lcl(J-1), WS( J-1) )
150                 continue
                end if

                !-----------Downstream end of channel.

                K = K + 1
                J = DownstreamPointer()
                Velocity = Q(J) / CxArea( CompLocation_lcl(J), WS( J ) )
                !dX = CompLocation_lcl(J) - CompLocation_lcl(J-1)
                FrNo = Velocity / SQRT( G * H(J) )
                CrNo = dtr * ( Velocity + SQRT( G * H(J) ) ) / delX
                WSSlope = ( WS(J) - WS(J-1) ) / delX
                Width = ChannelWidth( CompLocation_lcl(J), WS( J ) )

                !-----------Check friction / WS slope / dX relation.

                OK = CloseChannel()

            else
                write(UNIT_ERROR,*) ' Could not open channel...', &
                    chan_geom(Channelnumber_L)%chan_no
                write(UNIT_ERROR,*) ' (InitializeNetworkFlowValues)'
                return
            end if

200     continue

        InitializeNetworkFlowValues = .true.

        return
    end function


    !== Public (InitializeChannelNetwork) ==================================

    logical function InitializeChannelNetwork()
        use IO_Units
        use netcntrl
        use channel_schematic, only: SetCompLocations
        implicit none

        !   Purpose:  Initialize a network of channels.

        !   Arguments:

        !   Argument definitions:

        !   Module data:


        !   Local Variables:
        logical :: OK


        !   Programmed by: Lew DeLong
        !   Date:          November 1990
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        InitializeChannelNetwork = .false.

        !-----Read network schematic data.
        !      IF( .not. InitializeNetworkSchematic() ) THEN
        !         WRITE(UNIT_ERROR,*) ' Attempt to initialize network schematic failed...'
        !         RETURN
        !      END IF

        !-----Determine computational locations and set channel pointers.
        if( .not. SetCompLocations() ) then
            write(UNIT_ERROR,*) &
                ' Attempt to set computational locations failed...'
            return
        endif

        !-----Set initial channel flow values.
        if( .not. InitializeNetworkFlowValues() ) then
            write(UNIT_ERROR,*) ' Attempt to set initial flow channel', &
                ' values failed...'
            return
        end if

        !-----Set initial water density.

        if( VariableStreamDensity() ) then
        else if(VariableStreamSinuosity() ) then
            OK = SetConstantStreamDensity()
        end if

        InitializeChannelNetwork = .true.

        return
    end function

    !== Public (OldStreamDensity) ================================================

    real*8 function OldStreamDensity(LocationNumber)
        use IO_Units
        use channel_schematic, only: UpstreamPointer, &
            CurrentChannel, CheckChannelCompLocationRange


        implicit none

        !   Purpose:  Return current value of stream density in the current
        !             channel at a location corresponding to the index
        !             LocationNumber, at the begining of the current time step.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location sequence number within
        !                      current channel.

        !   Local Variables:
        integer J

        !   Routines by module:

        !**** Channel schematic:
        !   Programmed by: Lew DeLong
        !   Date:          October 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        if( CheckChannelCompLocationRange( LocationNumber ) ) then

            OldStreamDensity = Rho1( J )

        else

            write(UNIT_ERROR,*) ' Range error...(OldStreamDensity)'
            write(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
            write(UNIT_ERROR,*) ' Abnormal program end.'
            call EXIT(1)

        end if

        return
    end function

    !== Public (NewStreamDensity) ================================================

    real*8 function NewStreamDensity(LocationNumber)
        use IO_Units
        use channel_schematic, only: UpstreamPointer, CurrentChannel, &
            CheckChannelCompLocationRange
        implicit none

        !   Purpose:  Return current value of stream density in the current
        !             channel at a location corresponding to the index
        !             LocationNumber, at the end of the current time step.

        !   Arguments:
        integer LocationNumber

        !   Argument definitions:
        !     LocationNumber - computational-location sequence number within
        !                      current channel.

        !   Local Variables:
        integer J

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          October 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        if( CheckChannelCompLocationRange( LocationNumber ) ) then

            NewStreamDensity = Rho2( J )

        else

            write(UNIT_ERROR,*) ' Range error...(NewStreamDensity)'
            write(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
            write(UNIT_ERROR,*) ' Abnormal program end.'
            call EXIT(1)

        end if

        return
    end function

    !== Public (SetOldStreamDensity) ================================================

    logical function SetOldStreamDensity(LocationNumber, Value)
        use IO_Units
        use channel_schematic, only: UpstreamPointer, CurrentChannel, &
            CheckChannelCompLocationRange

        implicit none

        !   Purpose:  Set current value of stream density in the current
        !             channel at a location corresponding to the index
        !             LocationNumber, at the begining of the current time step.

        !   Arguments:
        integer LocationNumber
        real*8    Value

        !   Argument definitions:
        !     LocationNumber - computational-location sequence number within
        !                      current channel.

        !   Local Variables:
        integer J

        !   Programmed by: Lew DeLong
        !   Date:          October 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        if( CheckChannelCompLocationRange( LocationNumber ) ) then

            Rho1( J ) = Value

        else

            write(UNIT_ERROR,*) ' Range error...(SetOldStreamDensity)'
            write(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
            write(UNIT_ERROR,*) ' Abnormal program end.'
            call EXIT(1)

        end if

        SetOldStreamDensity = .true.

        return
    end function

    !== Public (SetNewStreamDensity) ================================================

    logical function SetNewStreamDensity(LocationNumber, Value)
        use IO_Units
        use channel_schematic, only: UpstreamPointer, CurrentChannel, &
            CheckChannelCompLocationRange

        implicit none

        !   Purpose:  Set current value of stream density in the current
        !             channel at a location corresponding to the index
        !             LocationNumber, at the end of the current time step.

        !   Arguments:
        integer LocationNumber
        real*8    Value

        !   Argument definitions:
        !     LocationNumber - computational-location sequence number within
        !                      current channel.
        !     Value - value to be set.

        !   Local Variables:
        integer J


        !   Programmed by: Lew DeLong
        !   Date:          October 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        J = UpstreamPointer() + LocationNumber - 1
        !     IF( CheckChannelCompLocationRange( LocationNumber ) ) THEN

        Rho2( J ) = Value

        !     ELSE

        !        WRITE(UNIT_ERROR,*) ' Range error...(SetNewStreamDensity)'
        !        WRITE(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
        !        WRITE(UNIT_ERROR,*) ' Abnormal program end.'
        !        CALL EXIT(1)

        !      END IF

        SetNewStreamDensity = .true.

        return
    end function

    !== Public (EstOldStreamDensity) ================================================

    real*8 function EstOldStreamDensity(DownstreamDistance)
        use IO_Units
        use channel_schematic, only: UpstreamPointer, DownstreamPointer, &
            StreamDistance, CurrentChannel
        implicit none

        !   Purpose:  Return current value of stream density in the current
        !             channel at a  downstream distance of
        !             DownStreamDistance, at the begining of the current
        !             time step.

        !   Arguments:
        real*8 DownstreamDistance

        !   Argument definitions:
        !     DownstreamDistance - downstream distance to location within
        !                           current channel.

        !   Local Variables:
        integer I, N
        real*8    XUp, dX, Shape

        !   Routines by module:

        !   Programmed by: Lew DeLong
        !   Date:          October 1991
        !   Modified by:
        !   Last modified:
        !   Version 93.01, January, 1993

        !-----Implementation -----------------------------------------------------

        N = 0
        do 100 I=UpstreamPointer(),DownstreamPointer()
            N = N + 1
            if(StreamDistance(N) <= DownstreamDistance ) then
            else
                go to 102
            end if
100     continue
        write(UNIT_ERROR,*) ' Range error...(EstOldStreamDensity)'
        write(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
        write(UNIT_ERROR,*) ' Downstream distance ...', DownstreamDistance
102 continue

    XUp = StreamDistance(N-1)
    dX = StreamDistance(N) - XUp
    Shape = (DownstreamDistance - XUp) / dX
    EstOldStreamDensity = Rho1(N) * Shape + Rho1(N-1) * (1.0 - Shape)

    return
end function

!== Public (EstNewStreamDensity) ================================================

real*8 function EstNewStreamDensity(DownstreamDistance)
    use IO_Units
    use channel_schematic, only: UpstreamPointer, DownstreamPointer, &
        CurrentChannel, StreamDistance

    implicit none

    !   Purpose:  Return current value of stream density in the current
    !             channel at a location at a downstream distance of
    !             DownStreamDistance, at the end of the current
    !             time step.

    !   Arguments:
    real*8 DownstreamDistance

    !   Argument definitions:
    !     DownstreamDistance - downstream distance to location within
    !                           current channel.

    !   Local Variables:
    integer I, N
    real*8    XUp, dX, Shape

    !   Routines by module:


    !   Programmed by: Lew DeLong
    !   Date:          October 1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    N = 0
    do 100 I=UpstreamPointer(),DownstreamPointer()
        N = N + 1
        if(StreamDistance(N) <= DownstreamDistance ) then
        else
            go to 102
        end if
100 continue
    write(UNIT_ERROR,*) ' Range error...(EstNewStreamDensity)'
    write(UNIT_ERROR,*) ' Channel ',CurrentChannel(),'...'
    write(UNIT_ERROR,*) ' Downstream distance ...', DownstreamDistance
102 continue

    XUp = StreamDistance(N-1)
    dX = StreamDistance(N) - XUp
    Shape = (DownstreamDistance - XUp) / dX
    EstNewStreamDensity = Rho2(N) * Shape + Rho2(N-1) * (1.0 - Shape)

    return
end function

!== Public (SetConstantStreamDensity) ====================================

logical function SetConstantStreamDensity()
    use IO_Units
    use channel_schematic, only: UpstreamPointer, DownstreamPointer, NumberOfChannels, &
         OpenChannel, CloseChannel

    implicit none

    !   Purpose:  Set stream density to 1.0 for all channels.

    !   Arguments:

    !   Argument definitions:
    !   Local Variables:
    integer I, M, Channel
    logical  OK

    !   Routines by module:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          October  1991
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    do 200 M=1,NumberOfChannels()

        Channel = M
        if( OpenChannel( Channel ) ) then
            do 100 I=UpstreamPointer(),DownstreamPointer()
                Rho1(I) = 1.0
                Rho2(I) = 1.0
100         continue
        else
            write(UNIT_ERROR,*) ' ####error(SetConstantStreamDensity)'
            write(UNIT_ERROR,*) ' could not open channel...',Channel
            write(UNIT_ERROR,*) ' Abnormal program end.'
            call EXIT(1)
        end if

        OK = CloseChannel()

200 continue

    SetConstantStreamDensity = .true.

    return
end function



!== Public (StreamEnergy) ================================================

real*8 function StreamEnergy( X, Q, Z )
    use PhysicalConstants,only: gravity
    use channel_xsect_tbl, only: CxArea, BtmElev

    implicit none

    !   Purpose:  Approximate specific energy in the current channel at X,
    !             given flow (Q) and water-surface elevation (Z).

    !   Arguments:
    real*8 X, Q, Z

    !   Argument definitions:
    !     X - downstream distance in current channel.
    !     Q - discharge.
    !     Z - water-surface elevation.

    !   Module data:

    !   Local Variables:
    real*8 Depth, Velocity

    !   Routines by module:

    !**** Local:


    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Depth = Z - BtmElev( X )
    if( Depth > 0.0 ) then
        Velocity = Q / CxArea( X, Z )
        StreamEnergy = Z + Velocity**2 / (2.D0 * gravity)
    else
        StreamEnergy = 0.0
    end if

    return
end function

!== Public (dStreamEnergydZ) ================================================

real*8 function dStreamEnergydZ( X, Q, Z )
    use PhysicalConstants,only: gravity
    use channel_xsect_tbl, only: CxArea, ChannelWidth, BtmElev
    implicit none
    !   Purpose:  Approximate the gradient of specific energy
    !             with depth of flow.

    real*8 X, Q, Z

    !   Argument definitions:
    !     X - downstream distance in current channel.
    !     Q - discharge.
    !     Z - water-surface elevation.

    !   Module data:

    !   Local Variables:
    real*8 A, Depth, Velocity

    !   Routines by module:

    !**** Local:

    !**** Network control:


    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Depth = Z - BtmElev( X )
    if( Depth > 0.0 ) then
        A = CxArea( X, Z )
        Velocity = Q / A
        dStreamEnergydZ = 1.0 &
            - ChannelWidth( X, Z ) * Velocity**2 /(A*gravity)
    else
        dStreamEnergydZ = 0.0
    end if

    return
end function

!== Public (StreamResistance) ================================================

real*8 function StreamResistance( Xu, Xd, Zu, Zd, Q )
    use channel_xsect_tbl, only: BtmElev, Conveyance
    use netcntrl, only: NetworkQuadPts, NetworkQuadPtWt
    implicit none

    !   Purpose:  Approximate head loss due to flow resistance between
    !             an upstream and downstream cross section.

    !   Arguments:
    real*8 Xu, Xd, Zu, Zd, Q

    !   Argument definitions:
    !     Xu - down stream distance to upstream cross section.
    !     Xd - down stream distance to downstream cross section.
    !     Zu - upstream water-surface elevation.
    !     Zd - downstream water-surface elevation.
    !     Q  - discharge.


    !   Local Variables:
    integer K, QuadPts
    real*8 N( MaxQuadPts )
    real*8 Hd, Hu
    real*8 H, X, QuadWt, QuadPt, fric, Z

    !   Routines by module:

    !**** Local:


    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Hu = Zu - BtmElev( Xu )
    Hd = Zd - BtmElev( Xd )

    fric = 0.0
    QuadPts = NetworkQuadPts()

    do 100 K=1,QuadPts

        !--------Estimate quadrature-point values.

        call NetworkQuadPtWt( K, QuadPt, QuadWt )

        !--------Interpolation functions.
        N(1) = 1.0 - QuadPt
        N(2) = QuadPt

        !--------Location of quadrature point.
        X = N(1) * Xu + N(2) * Xd

        !--------Dependent variables.
        H = N(1) * Hu + N(2) * Hd
        Z = N(1) * Zu + N(2) * Zd

        if( H > 0.0 ) then
            fric = fric + QuadWt / ( Conveyance(X,Z) ** 2 )
        end if

100 continue

    StreamResistance = Q * ABS( Q ) * fric * ( Xd - Xu )

    return
end function

!== Public (dStreamResistancedZu) ================================================

real*8 function dStreamResistancedZu( Xu, Xd, Zu, Zd, Q )
    use network
    use channel_xsect_tbl, only: BtmElev, Conveyance, dConveyance
    use netcntrl, only: NetworkQuadPts, NetworkQuadPtWt
    implicit none

    !   Purpose:  Approximate gradient, with respect to a change in upstream
    !             water-surface elevation, of head loss due to flow
    !             resistance between an upstream and downstream cross section.

    !   Arguments:
    real*8 Xu, Xd, Zu, Zd, Q

    !   Argument definitions:
    !     Xu - down stream distance to upstream cross section.
    !     Xd - down stream distance to downstream cross section.
    !     Zu - upstream water-surface elevation.
    !     Zd - downstream water-surface elevation.
    !     Q  - discharge.
    !   Local Variables:
    integer K, QuadPts
    real*8 N( MaxQuadPts )
    real*8 Hd, Hu
    real*8 H, X, QuadWt, QuadPt, fric, Z

    !   Routines by module:

    !**** Local:


    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Hu = Zu - BtmElev( Xu )
    Hd = Zd - BtmElev( Xd )

    fric = 0.0
    QuadPts = NetworkQuadPts()

    do 100 K=1,QuadPts

        !--------Estimate quadrature-point values.

        call NetworkQuadPtWt( K, QuadPt, QuadWt )

        !--------Interpolation functions.
        N(1) = 1.0 - QuadPt
        N(2) = QuadPt

        !--------Location of quadrature point.
        X = N(1) * Xu + N(2) * Xd

        !--------Dependent variables.
        H = N(1) * Hu + N(2) * Hd
        Z = N(1) * Zu + N(2) * Zd

        !--------Integrate friction term.

        if( H > 0.0 ) then
            fric = fric - QuadWt * N(1) *  2.0 * dConveyance(X,Z) &
                / ( Conveyance(X,Z) ** 3 )
        end if

100 continue

    dStreamResistancedZu = Q * ABS( Q ) * fric * ( Xd - Xu )

    return
end function

!== Public (GlobalStreamSurfaceSlope) ====================================

real*8 function GlobalStreamSurfaceSlope(LocationNumber)
    use channel_schematic, only: UpstreamPointer, DownstreamPointer, GlobalStreamDistance

    implicit none

    !   Purpose:  Return current value of water-surface slope
    !             at global location corresponding to LocationNumbe.

    !   Arguments:
    integer LocationNumber

    !   Argument definitions:
    !     LocationNumber - global computational-location number.


    !   Local Variables:

    !   Routines by module:


    !   Programmed by: Lew DeLong
    !   Date:          September 1993
    !   Modified by:
    !   Last modified:
    !   Version 93.01, January, 1993

    !-----Implementation -----------------------------------------------------

    if( LocationNumber == UpstreamPointer() ) then

        !--------Upstream end of channel.

        GlobalStreamSurfaceSlope = &
            ( WS( LocationNumber + 1 )  - WS( LocationNumber ) ) &
            / &
            ( &
            GlobalStreamDistance( LocationNumber + 1 ) &
            - GlobalStreamDistance( LocationNumber     ) &
            )

    else if( LocationNumber ==  DownstreamPointer() ) then

        !--------Downstream end of channel.

        GlobalStreamSurfaceSlope = &
            ( &
            WS( LocationNumber     ) &
            - WS( LocationNumber - 1 ) &
            )  /  ( &
            GlobalStreamDistance( LocationNumber     ) &
            - GlobalStreamDistance( LocationNumber - 1 ) &
            )

    else

        !--------Intervening reaches of channel.

        GlobalStreamSurfaceSlope = &
            ( &
            WS( LocationNumber + 1 ) &
            - WS( LocationNumber - 1 ) &
            )  /  ( &
            GlobalStreamDistance( LocationNumber + 1 ) &
            - GlobalStreamDistance( LocationNumber - 1 ) &
            )

    end if

    return
end function


!== Public (StreamMoFlux) ================================================

real*8 function StreamMoFlux( X, Q, Z )
    use channel_xsect_tbl, only: CxArea, BtmElev, Beta

    implicit none

    !   Purpose:  Approximate momentum flux in the current channel at X,
    !             given flow (Q) and water-surface elevation (Z).

    !   Arguments:
    real*8 X, Q, Z

    !   Argument definitions:
    !     X - downstream distance in current channel.
    !     Q - discharge.
    !     Z - water-surface elevation.

    !   Module data:

    !   Local Variables:
    real*8 H

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    H = Z - BtmElev( X )
    if( H > 0.0 ) then
        StreamMoFlux = Beta(X,Z) * Q ** 2 / CxArea(X,Z)
    else
        StreamMoFlux = 0.0
    end if

    return
end function

!== Public (dStreamMoFluxdZ) ================================================

real*8 function dStreamMoFluxdZ( X, Q, Z )
    use channel_xsect_tbl, only: CxArea, ChannelWidth, BtmElev, Beta, dBeta
    implicit none

    !   Purpose:  Approximate the gradient of momentum flux
    !             with depth of flow.

    real*8 X, Q, Z

    !   Argument definitions:
    !     X - downstream distance in current channel.
    !     Q - discharge.
    !     Z - water-surface elevation.

    !   Module data:

    !   Local Variables:
    real*8 A, H

    !   Routines by module:

    !**** Local:


    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    H = Z - BtmElev( X )
    if( H > 0.0 ) then
        A = CxArea( X, Z )
        dStreamMoFluxdZ = Q**2 / A * ( &
            - ChannelWidth( X,Z ) * Beta(X,Z) / A &
            + dBeta(X,Z) &
            )
    else
        dStreamMoFluxdZ = 0.0
    end if

    return
end function

!== Public (gASf) ================================================

real*8 function gASf( Xu, Xd, Zu, Zd, Q )
    use PhysicalConstants, only: gravity
    use network
    use channel_xsect_tbl, only: BtmElev, Conveyance, CxArea
    use netcntrl, only: NetworkQuadPts, NetworkQuadPtWt


    implicit none

    !   Purpose:  Approximate momentum loss due to flow resistance between
    !             an upstream and downstream cross section.

    !   Arguments:
    real*8 Xu, Xd, Zu, Zd, Q

    !   Argument definitions:
    !     Xu - down stream distance to upstream cross section.
    !     Xd - down stream distance to downstream cross section.
    !     Zu - upstream water-surface elevation.
    !     Zd - downstream water-surface elevation.
    !     Q  - discharge.

    !   Local Variables:
    integer K, QuadPts
    real*8 N( MaxQuadPts )
    real*8 Hd, Hu, G
    real*8 H, X, QuadWt, QuadPt, fric,Z

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Hu = Zu - BtmElev( Xu )
    Hd = Zd - BtmElev( Xd )

    G = gravity

    fric = 0.0
    QuadPts = NetworkQuadPts()

    do 100 K=1,QuadPts

        !--------Estimate quadrature-point values.

        call NetworkQuadPtWt( K, QuadPt, QuadWt )

        !--------Interpolation functions.
        N(1) = 1.0 - QuadPt
        N(2) = QuadPt

        !--------Location of quadrature point.
        X = N(1) * Xu + N(2) * Xd

        !--------Dependent variables.
        H = N(1) * Hu + N(2) * Hd
        Z = N(1) * Zu + N(2) * Zd

        if( H > 0.0 ) then
            fric = fric + QuadWt * CxArea(X,Z) / ( Conveyance(X,Z) ** 2 )
        end if

100 continue

    gASf = G * Q * ABS( Q ) * fric * ( Xd - Xu )

    return
end function

!== Public (dgASfdZu) ================================================

real*8 function dgASfdZu( Xu, Xd, Zu, Zd, Q )
    use PhysicalConstants,only: gravity
    use network
    use channel_xsect_tbl, only: BtmElev, Conveyance, dConveyance, &
        CxArea, ChannelWidth
    use netcntrl, only: NetworkQuadPts, NetworkQuadPtWt
    implicit none

    !   Purpose:  Approximate gradient, with respect to a change in upstream
    !             water-surface elevation, of momentum loss due to flow
    !             resistance between an upstream and downstream cross section.

    !   Arguments:
    real*8 Xu, Xd, Zu, Zd, Q

    !   Argument definitions:
    !     Xu - down stream distance to upstream cross section.
    !     Xd - down stream distance to downstream cross section.
    !     Zu - upstream water-surface elevation.
    !     Zd - downstream water-surface elevation.
    !     Q  - discharge.

    !   Local Variables:
    integer K, QuadPts
    real*8 N( MaxQuadPts )
    real*8 Hd, Hu, G
    real*8 H, X, QuadWt, QuadPt, fric, Z
    real*8 Conv

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Hu = Zu - BtmElev( Xu )
    Hd = Zd - BtmElev( Xd )

    G = gravity

    fric = 0.0
    QuadPts = NetworkQuadPts()

    do 100 K=1,QuadPts

        !--------Estimate quadrature-point values.

        call NetworkQuadPtWt( K, QuadPt, QuadWt )

        !--------Interpolation functions.
        N(1) = 1.0 - QuadPt
        N(2) = QuadPt

        !--------Location of quadrature point.
        X = N(1) * Xu + N(2) * Xd

        !--------Dependent variables.
        H = N(1) * Hu + N(2) * Hd
        Z = N(1) * Zu + N(2) * Zd

        !--------Integrate friction term.

        if( H > 0.0 ) then
            Conv = Conveyance(X,Z)
            fric = fric + QuadWt * N(1) *  ( &
                - 2.0 * dConveyance(X,Z) * CxArea(X,Z) / Conv &
                + ChannelWidth(X,Z) &
                ) &
                / ( Conv ** 2 )

        end if

100 continue

    dgASfdZu = G * Q * ABS( Q ) * fric * ( Xd - Xu )

    return
end function

!== Public (gAdZdX) ================================================

real*8 function gAdZdX( Xu, Xd, Zu, Zd )
    use PhysicalConstants,only: gravity
    use network
    use channel_xsect_tbl, only: BtmElev, CxArea
    use netcntrl, only: NetworkQuadPts, NetworkQuadPtWt

    implicit none

    !   Purpose:  Approximate momentum gain due to WS slope between
    !             an upstream and downstream cross section.

    !   Arguments:
    real*8 Xu, Xd, Zu, Zd

    !   Argument definitions:
    !     Xu - down stream distance to upstream cross section.
    !     Xd - down stream distance to downstream cross section.
    !     Zu - upstream water-surface elevation.
    !     Zd - downstream water-surface elevation.

    !   Local Variables:
    integer K, QuadPts
    real*8 N( MaxQuadPts )
    real*8 dNdX( MaxQuadPts )
    real*8 Hd, Hu, G
    real*8 H, X, QuadWt, QuadPt, Slope, A, Z

    !   Routines by module:

    !**** Local:



    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Hu = Zu - BtmElev( Xu )
    Hd = Zd - BtmElev( Xd )

    G = gravity

    A = 0.0
    QuadPts = NetworkQuadPts()

    do 100 K=1,QuadPts

        !--------Estimate quadrature-point values.

        call NetworkQuadPtWt( K, QuadPt, QuadWt )

        !--------Interpolation functions.
        N(1) = 1.0 - QuadPt
        N(2) = QuadPt
        dNdX(1) = -1.0 / (Xu - Xd)
        dNdX(2) = - dNdX(1)

        !--------Location of quadrature point.
        X = N(1) * Xu + N(2) * Xd

        !--------Dependent variables.
        H = N(1) * Hu + N(2) * Hd
        Z = N(1) * Zu + N(2) * Zd

        !--------WS Slope.

        Slope = dNdX(1) * Zu + dNdX(2) * Zd

        if( H > 0.0 ) then
            A = A + QuadWt * CxArea(X,Z) * Slope
        end if

100 continue

    gAdZdX = G * A * (Xd - Xu)

    return
end function

!== Public (dgAdZdXdZu) ================================================

real*8 function dgAdZdXdZu( Xu, Xd, Zu, Zd )
    use PhysicalConstants,only: gravity
    use network
    use channel_xsect_tbl, only: BtmElev, CxArea, ChannelWidth
    use netcntrl, only: NetworkQuadPts, NetworkQuadPtWt


    implicit none

    !   Purpose:  Approximate gradient, with respect to a change in upstream
    !             water-surface elevation, of momentum gain due to WS
    !             slope between an upstream and downstream cross section.

    !   Arguments:
    real*8 Xu, Xd, Zu, Zd

    !   Argument definitions:
    !     Xu - down stream distance to upstream cross section.
    !     Xd - down stream distance to downstream cross section.
    !     Zu - upstream water-surface elevation.
    !     Zd - downstream water-surface elevation.


    !   Local Variables:
    integer K, QuadPts
    real*8 N( MaxQuadPts )
    real*8 dNdX( MaxQuadPts )
    real*8 Hd, Hu, G
    real*8 H, X, QuadWt, QuadPt, A, Slope, Z

    !   Routines by module:

    !**** Local:

    !   Intrinsics:

    !   Programmed by: Lew DeLong
    !   Date:          Sept  1993
    !   Modified by:
    !   Last modified:

    !-----Implementation -----------------------------------------------------

    Hu = Zu - BtmElev( Xu )
    Hd = Zd - BtmElev( Xd )

    G = gravity

    QuadPts = NetworkQuadPts()

    A = 0.0
    do 100 K=1,QuadPts

        !--------Estimate quadrature-point values.

        call NetworkQuadPtWt( K, QuadPt, QuadWt )

        !--------Interpolation functions.
        N(1) = 1.0 - QuadPt
        N(2) = QuadPt
        dNdX(1) = -1.0 / (Xd - Xu)
        dNdX(2) = - dNdX(1)

        !--------Location of quadrature point.
        X = N(1) * Xu + N(2) * Xd

        !--------Dependent variables.
        H = N(1) * Hu + N(2) * Hd
        Z = N(1) * Zu + N(2) * Zd

        !--------WS Slope.

        Slope = dNdX(1) * Zu + dNdX(2) * Zd

        !--------Integrate friction term.

        if( H > 0.0 ) then
            A = A + QuadWt * ( &
                N(1) * ChannelWidth(X,Z) * Slope &
                + dNdX(1) * CxArea(X,Z) &
                )

        end if

100 continue

    dgAdZdXdZu = G * A * (Xd - Xu)

    return
end function




!==== EOF chstatus ======================================================

end module
!   Definitions:
!     WS(i) - water surface elevation at computational location "i".
!     Q(i) - volumetric discharge at computational location "i".
!     H(i) - depth of flow at computational location "i".
!     Rho1(i) - density at computational location "i", at the
!               begining of the current time increment.
!     Rho1(i) - density at computational location "i", at the
!               the end of the current time increment.
!     ConvergedSteady(m) - .TRUE. if steady solution converged,
!                           otherwise .FALSE.

!===== EOF chstatus.inc ==================================================
