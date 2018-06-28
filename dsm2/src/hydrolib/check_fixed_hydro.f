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

subroutine check_fixed_hydro(istat)

    !-----Check the fixed input for omissions and errors before starting
    !-----the model run.  Supply default values where possible.  Translate
    !-----from nodes to channel numbers, and from external channel numbers
    !-----to internal.  Write to FourPt arrays.
    use Gates, only:gateArray, nGate
    use PhysicalConstants
    use IO_units
    use logging
    use grid_data
    use runtime_data
    use constants
    use iopath_data

    use common_xsect
    use common_tide
    use network
    use netcntrl
    use chconnec
    use chnluser
    use channel_xsect_tbl
    use dss
    use mod_readdss
    use mod_writedss
    implicit none

    !-----Local variables

    integer*4 &
        incr_intvl           ! increment julian minute by interval function

    integer &
        istat                &! status of call (returned)
        ,i,j,kk,pth          &! indices
        ,intchan,intchan2    &! internal/external channels
        ,node,xs             &! node, xsect numbers
        ,neqstage            &! number of equal-stage boundaries
        ,nsumq               &! number of sum-of-flow boundaries
        ,channo              &! channel number--do loop counter
        ,vsecno              &! number of virt section within channel
        ,nsec                &! number of sections based on deltax_requested
        ,loc,loccarr         &! character array string locator
        ,node2hydrochan      &! function to convert node to hydro channel
        ,nintnode(max_nodes) &! number of internal and external flows at nodes and reservoirs
        ,nintres(max_reservoirs) &
        ,nextnode(max_nodes) &
        ,nextres(max_reservoirs) &
        ,flowdone &
        ,role

    integer*2 number          ! object number

    real*8 &
        totalweight          &! check quadrature weighting
        ,delx                &! actual distance between xsect
        ,dx_r                ! stores value of deltax_requested

    character cresnames(max_reservoirs)*20 ! reservoir names array
    character cinputnames(max_inputpaths)*20 ! input names array
    character*32 name         ! object name

    integer prevchan
    integer USR, LNUM
    real*8    R23, R53 ,leng, distance,f1,f2
    parameter (R23 = 2.0/3.0, R53 = 5.0/3.0)
    logical firsttime(maxchannels),updefined,downdefined
    logical orphannode
    logical is_node_flow
    real*8,external :: fetch_data

    !-----statement function to calculate indices of virtual data arrays
    !-----dindex(channo,vsecno,virtelev)
    !     &     =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1

602 format(/'Error opening/reading restart input file: ',a)

605 format(/a,' date incorrect: ',a)

607 format('Using restart file date: ',a)

610 format(/'Quadrature weights do not add to 1.0:' &
        /'No.  Point   Weight' &
        /(i2,2f8.3/))

612 format(/'Warning--Variable Density reset to false because' &
        ' flow method is not Dynamic Wave.')

614 format(/'Warning--Variable Sinuosity reset to true because' &
        ' flow method is Dynamic Wave.')

616 format(/'Warning--Variable Sinuosity reset to false because' &
        ' flow method is Kinematic Wave.')

630 format(/'Incorrect internal node boundary for node:',i5 &
        /'1 channel must be sum-of-flow boundary,' &
        /'n-1 channels must be equal stage.')

655 format(/'The following path was not found in the list of gates:' &
        /a)

660 format(/'No channel given: either incomplete gate specs, ' &
        /' misspelled gate name, or reservoir gate name not' &
        /' found in reservoir names: ',a)

670 format(/'Reservoir gate node number not found in reservoir list: ',i5)

690 format(' Error.... Channel :',i5/ &
        '           As a minimum, X-Sections need to be specified'/ &
        '           at both ends of the channel;')
700 format(' Error.... Channel :',i5/ &
        '           The flow area for the first level defined='/ &
        f15.5 &
        '           has to be zero. Zero Assumed')
710 format(/'"',a,'" reservoir name: ',a,' not recognized.')

715 format(/a,' object to object input path label not recognized.')

720 format(/'MaxLocations too small (',i4, &
        '), needs to be increased to' &
        /'accomodate requested deltax (',i4,').')

730 format(/'Too many stage boundaries, increase max_stgbnd: ',i2)

735 format(/'Error: only one channel allowed to a fixed-stage node: ', &
        i3)

830 format(/'Error: too many ',a,' flows connected to ',a,' ',a)


&!-----check constants
        if (.not. verify_gravity_terms()) then
        write(unit_error,*)"Error checking fixed data"
        istat=-3
        return
    end if

 
    !----  nquadpts=nquadpts-1

    if (time_step_intvl_hydro /= ' ') then
        !--------time_step_intvl_hydro should be in form: '5min' or '1hour'
        time_step=incr_intvl(0,time_step_intvl_hydro,TO_BOUNDARY)
    else
        time_step=5            ! 5 minutes is default time step
    endif

    !-----set FourPt values
    numch=nchans
    numuserloc=0
    timestep=0
    dt=time_step*60
    PrintLevel=print_level

    Restart_Read=io_files(hydro,io_restart,io_read)%use
    Restart_Write=io_files(hydro,io_restart,io_write)%use

    !-----hydro binary/hdf5 output file interval


    if( io_files(hydro,io_hdf5,io_write)%use ) then
        TideFileWriteInterval=incr_intvl(0,&
            io_files(hydro,io_hdf5,io_write)%interval,TO_BOUNDARY)
    end if



    if (io_files(hydro,io_hdf5,io_write)%use) then
        if (mod(TideFileWriteInterval,time_step) == 0) then
            Nsample=TideFileWriteInterval/time_step
        else
            write(unit_output,'(a)') ' time_step=',time_step,' minutes'
            write(unit_output,'(a)') ' Tidefile Interval must be an'
            write(unit_output,'(a)') ' integer multiple of time_step'
            Nsample=12
            TideFileWriteInterval=Nsample*time_step
            write(unit_output,'(a)') ' Tidefile Interval changed to: ', &
                TideFileWriteInterval
        endif
    endif

    !-----don't use boundary equations
    do i=1,2*maxchannels+1
        eqnumber(i)=0
    enddo

    !-----Quadrature Weighting
    !-----supply default values if necessary
    if (nquadpts == 1) then
        !         nquadpts=1
        quadpt(1)=0.5
        quadwt(1)=1.0
    else if (nquadpts == 2) then
        quadpt(1)=0.
        quadpt(2)=1.0
        quadwt(1)=0.5
        quadwt(2)=0.5
    else if (nquadpts == 3) then
        quadpt(1)=0.
        quadpt(2)=0.5
        quadpt(3)=1.0
        quadwt(1)=0.25
        quadwt(2)=0.5
        quadwt(3)=0.25
    endif
    !-----check that weights total 1.0
    totalweight = 0.0
    do i=1,nquadpts
        totalweight = totalweight + quadwt(i)
    enddo
    if (abs(totalweight-1.0) > 1.0e-6) then
        write(unit_error,610) (i,quadpt(i),quadwt(i),i=1,nquadpts)
        goto 900
    endif
    quadpts=nquadpts          ! FourPt variable

    !-----1D open-channel flow method default
    if ( &
        terms /= 1 .and. &
        terms /= 2 .and. &
        terms /= 3 &
        ) terms=1            ! default to dynamic wave

    !-----variable density and sinuousity defaults
    if ( &
        variabledensity .and. &
        (terms == 2 .or. &
        terms == 3) &
        ) then
        variabledensity=.false.
        write(unit_error, 612)
    endif

    if ( &
        .not. variablesinuosity .and. &
        variabledensity .and. &
        terms == 1 &
        ) then
        variablesinuosity=.true.
        write(unit_error, 614)
    endif

    if ( &
        variablesinuosity .and. &
        terms == 3 &
        ) then
        variablesinuosity=.false.
        write(unit_error, 616)
    endif

    !-----Connectivity
    !-----Rules:
    !-----1) At least one (external) node must be a specified-stage boundary.
    !-----2) For internal nodes, 1 channel must be associated with sum-of-flow boundary.
    !-----3) For internal nodes, n-1 channels must be associated with equal-stage boundary,
    !-----gates or other specialty condition.
    !-----4) For internal nodes, for number of connections count only
    !-----stage-flow connections, not stage-stage.    ?????????? fixme: what is this? -eli
    !-----4) For external nodes, if no boundary type specified, assume flow.

    !-----Specialty codes such as gates should be set first

    !-----generate channel end connection types
    !     FP Boundary-condition code       Meaning

    !       water surface
    !         1                         explicitly known
    !         11                        equal to another water surface
    !         52                        gate
    !
    !       flow
    !         2                         explicitly known
    !         12                        sum of flows equal zero
    !         52                        gate

    !-----first set codes for all gates
    !-----the flow direction of all the gates must be known before this
    !-----gate.flowDirect is a double precision variable that tells how the
    !-----convention for gate flow (water body to node) aligns with the flow
    !-----direction in the water body (e.g., upstream to downstream) flowDirect =1.D0
    !-----if the direction is the same (gate at downstream end of channel), -1.D0 otherwise

    !     add gate codes
    do i = 1,ngate
        if (gateArray(i)%inUse .and. (gateArray(i)%objConnectedType &
            == obj_channel)) then
            channo=gateArray(i)%objConnectedID
            node=gateArray(i)%node
            !-----------verify that the node is internal
            if ((node_geom(node)%nup + node_geom(node)%ndown) <= 1) then ! external (error)
                write(unit_error,712) trim(gateArray(i)%name)
712             format(/'Gate may not be placed at external node. Gate: ',a)
                call exit(2)
            end if
            !-----------set appropriate boundary condition code
            if (gateArray(i)%flowDirection  == 1.D0) then ! gate at downstream end
                downboundarycode(channo) = 10452
            elseif (gateArray(i)%flowDirection == -1.D0) then ! upstream end
                upboundarycode(channo) = 10452
            else
                write (unit_error,718) trim(gateArray(i)%name)
718             format(/'Flow direction not correctly set for gate: ',a)
                call exit(2)
            end if
        end if
    end do

    !-----set condition codes (external boundaries, internal compatibility conditions)
    nstgbnd=0
    nflwbnd=0
    !      nodeSumQChan = 0          ! Array initialization to zero
    do node=1,max_nodes
        if ((node_geom(node)%nup + node_geom(node)%ndown) == 0) then
        !-----------This node not connected to any channel...do nothing

        elseif ((node_geom(node)%nup + node_geom(node)%ndown) == 1) then

            !-----------external node
            if (node_geom(node)%boundary_type == flow_boundary .or. &
                node_geom(node)%boundary_type == miss_val_i) then
                !--------------specified-flow boundary type or none given, assume s-f
                node_geom(node)%boundary_type=flow_boundary
                do i=1,node_geom(node)%ndown
                    intchan=node_geom(node)%downstream(i)
                    downboundarycode(intchan)=2
                    node_geom(node)%sumQChan = -intchan
                enddo
                do i=1,node_geom(node)%nup
                    intchan=node_geom(node)%upstream(i)
                    upboundarycode(intchan)=2
                    node_geom(node)%sumQChan = intchan
                enddo              
            else if (node_geom(node)%boundary_type == stage_boundary) then ! specified-stage bc
                do i=1,node_geom(node)%ndown
                    intchan=node_geom(node)%downstream(i)
                    downboundarycode(intchan)=1
                    node_geom(node)%sumQChan = -intchan
                enddo
                do i=1,node_geom(node)%nup
                    intchan=node_geom(node)%upstream(i)
                    upboundarycode(intchan)=1
                    node_geom(node)%sumQChan = intchan
                enddo
                !--------------set up stage boundary object structure
                nstgbnd=nstgbnd+1
                if (nstgbnd > max_stgbnd) then
                    write(unit_error,730) max_stgbnd
                    goto 900
                endif
                stgbnd(nstgbnd)%node=node
                stgbnd(nstgbnd)%name=' '
                !--------------search for input path with stage boundary at this node
                do i=1,ninpaths
                    if (pathinput(i)%useobj .and. &
                        pathinput(i)%data_type == obj_stage .and. &
                        pathinput(i)%obj_type == obj_node .and. &
                        pathinput(i)%obj_no == node) then
                        stgbnd(nstgbnd)%name=pathinput(i)%name
                        ! fixme: needs to be changed when constant data is moved from pathinput to datasource
                        call datasource_from_path(stgbnd(nstgbnd)%datasource, &
                            i,pathinput(i))
                    endif
                enddo
            endif 
        else


            !-----------internal node (multiple channels connected)
            if (node_geom(node)%boundary_type == stage_boundary) then
                write(unit_error,735) node_geom(node)%node_ID !no fixed-stage nodes allowed internally
                goto 900
            endif

            !-----------test that rules above are met; if no boundary condition set,
            !-----------apply rules, if bc's are set but aren't correct, stop
            neqstage=0          ! number of bc's that are equal-stage
            nsumq=0             ! number of bc's that are sum-of-flow
            do i=1,node_geom(node)%nup
                intchan=node_geom(node)%upstream(i)
                if (upboundarycode(intchan) == 0) then
                    if (nsumq == 0) then
                        upboundarycode(intchan)=12
                        node_geom(node)%sumQChan = intchan
                        nsumq=1
                    else
                        upboundarycode(intchan)=11
                        neqstage=neqstage+1
                    end if
                end if
            enddo
            do i=1,node_geom(node)%ndown
                intchan=node_geom(node)%downstream(i)
                if (downboundarycode(intchan) == 0) then
                    if (nsumq == 0) then
                        downboundarycode(intchan) = 12
                        node_geom(node)%sumQChan = -intchan
                        nsumq=1
                    else
                        downboundarycode(intchan) = 11
                        neqstage=neqstage+1
                    end if
                end if
            enddo

            !-----------end if        !?????????? fixme: is this loop needed check above loops

            !-----------verify that at least a sum of flow exists
            if (nsumq /= 1) then
                write(unit_error,630) node_geom(node)%node_ID
                call exit(2)
            endif
        endif
        do i=1,ninpaths
            if (pathinput(i)%useobj .and. &
                pathinput(i)%data_type == obj_boundary_flow .and. &
                pathinput(i)%obj_type == obj_node .and. &
                pathinput(i)%obj_no == node) then
                nflwbnd=nflwbnd+1
            endif
        enddo          
    enddo

    !-----fill FourPt connection arrays
    do intchan=1,nchans
        firsttime(intchan)=.true.
        upnumberofconnections(intchan)=0
        node=chan_geom(intchan)%upnode
        orphannode = (node_geom(node)%nup + node_geom(node)%ndown) <= 1
        if (orphannode .or. upboundarycode(intchan) == 12) then
            flowdone = 1        ! no sum-of-flow neighbor
        else
            flowdone = 0        ! flowdone is used to put the sum of flow
        end if                 ! channel in the first slot in the connection arrays
        if (node > 0) then
            do j=1,node_geom(node)%nup
                intchan2=node_geom(node)%upstream(j)
                if (intchan /= intchan2) then
                    upnumberofconnections(intchan)=upnumberofconnections(intchan)+1
                    if(upboundarycode(intchan2) == 12) then
                        upconnection((intchan-1)*maxconnectingchannels + 1) =  intchan2
                        flowdone = 1
                    else
                        upconnection((intchan-1)*maxconnectingchannels + &
                            upnumberofconnections(intchan) - flowdone + 1 ) = intchan2
                    end if
                endif
            enddo
            do j=1,node_geom(node)%ndown
                intchan2=node_geom(node)%downstream(j)
                if (intchan /= intchan2) then
                    upnumberofconnections(intchan)=upnumberofconnections(intchan)+1
                    if(downboundarycode(intchan2) == 12) then
                        upconnection( (intchan-1) * maxconnectingchannels + 1) = - intchan2
                        flowdone = 1
                    else
                        upconnection((intchan-1)*maxconnectingchannels + &
                            upnumberofconnections(intchan) - flowdone + 1) = -intchan2
                    end if
                endif
            enddo
        endif

        downnumberofconnections(intchan)=0
        node=chan_geom(intchan)%downnode
        orphannode = (node_geom(node)%nup + node_geom(node)%ndown) <= 1
        if (  orphannode .or. downboundarycode(intchan) == 12) then
            flowdone = 1        ! no sum-of-flow neighbor
        else
            flowdone = 0        ! flowdone is used to put the sum of flow
        end if                 ! channel in the first slot in the connection arrays
        if (node > 0) then
            do j=1,node_geom(node)%ndown
                intchan2=node_geom(node)%downstream(j)
                if ( intchan /= intchan2) then
                    downnumberofconnections(intchan)=downnumberofconnections(intchan)+1
                    if(downboundarycode(intchan2) == 12) then
                        downconnection((intchan-1)*maxconnectingchannels + 1) = - intchan2
                        flowdone = 1
                    else
                        downconnection((intchan-1)*maxconnectingchannels + &
                            downnumberofconnections(intchan) - flowdone + 1) = - intchan2
                    endif
                endif
            enddo
            do j=1,node_geom(node)%nup
                intchan2=node_geom(node)%upstream(j)
                if (intchan /= intchan2) then
                    downnumberofconnections(intchan)=downnumberofconnections(intchan)+1
                    if(upboundarycode(intchan2) == 12) then
                        downconnection((intchan-1)*maxconnectingchannels + 1) = intchan2
                        flowdone = 1
                    else
                        downconnection((intchan-1)*maxconnectingchannels + &
                            downnumberofconnections(intchan) - flowdone + 1) = intchan2
                    endif
                endif
            enddo
        endif
    enddo

    !-----fill the FourPt channel ID string

    do i=1,MaxLocations
        write(userlocationid(i),'(i5.5)') i
    enddo

    USR=0
    LNUM=0
    PreviousX   = -9
    PreviousBranch = 999
    PreviousH100   = 99999

    !-----fill FourPt channel and related arrays and check validity
    if (deltax_requested <= 0.) then
        write(unit_error,*) "Delta x not specified"
        call exit(3)
    end if
    do intchan=1,nchans
        Lines(intchan)=2       ! means values given at two depths
        FirstTable(intchan) = USR+1
        dx(intchan)=deltax_requested
        OneOverManning(intchan)=1./chan_geom(intchan)%manning
    enddo

    !-----Now process the generated irregular X-Sections

    updefined=.true.
    downdefined=.true.

    do intchan=1,nchans
        do vsecno=1,num_virt_sec(intchan)
            if (firsttime(intchan)) then
                !--------------first irregular x-section defined for this channel
                !--------------This will override any previous (rectangular) specification
                firsttime(intchan)=.false.
                prevchan=intchan
                updefined=.false.
                downdefined=.false.
                lines(intchan) = num_layers(intchan) ! Number of levels defined
            endif
            USR=USR+1
            if (USR>MaxTables) then
950             write(unit_error,*)' Error..  (check_fixed_hydro)'
                write(unit_error,*)' Too many irregular x-sections generated.'
                write(unit_error,*)' Increase Delta_x, or increase array dimension'
                write(unit_error,*)' Maxtables.   Current value=',MaxTables
                goto 900
            endif
            if (USR > MaxLocations) then
                write(unit_error, 720) MaxLocations, nint(deltax_requested)
                goto 900
            endif

            !-----------Datum should always be zero because min elev subtracted in readirreg
            Datum(USR)=virt_min_elev(minelev_index(intchan)+vsecno-1)

            Offset(USR)=LNUM+1
            leng=float(chan_geom(intchan)%length)
            if (deltax_requested == 0) then
                dx_r = chan_geom(intchan)%length
            elseif (deltax_requested /= 0) then
                dx_r = deltax_requested
            endif
            if (float(chan_geom(intchan)%length) <= dx_r) then
                nsec=1
            elseif (float(chan_geom(intchan)%length) > dx_r) then
                nsec=int( float(chan_geom(intchan)%length) /dx_r)
            endif
            delx=chan_geom(intchan)%length/nsec
            distance=(vsecno-1)*delx/2
            XDistance(USR)=distance
            xs=USR
            write(ID(USR),'(i5.5)') xs
            if (XDistance(USR) == 0.) then
                upuserpointer(intchan)=xs
                updefined=.true.
                FirstTable(intchan)=USR
            elseif (XDistance(USR) <= float(chan_geom(intchan)%length)) then
                downuserpointer(intchan)=xs
                downdefined=.true.
                LastTable(intchan)=USR
            else
                f1=1. -distance/leng
                f2=1.-f1
            endif
            if ((LNUM+Lines(intchan)+1) > MaxLines) then
                write(unit_error,*) '***error (check_fixed_hydro)'
                write(unit_error,*) 'Maximum number of lines exceeded...'
                write(unit_error,*) 'Current value MaxLines=',MaxLines
                write(unit_error,*) 'Cross section ',USR
                write(unit_error,*) 'Current line number = ',LNUM
                goto 900
            end if
        enddo
    enddo

951 continue

    do i=1,nreser
        if (res_geom(i)%nnodes == 0) then
            write(unit_error, 953) trim(res_geom(i)%name)
953         format(/'Note: no connection or gate records retrieved for reservoir ',a)
        endif
    end do

    !-----convert reservoir info for fourpt variables
    do i=1,nreser
        !--------channel connections to this reservoir
       ! Ares(i)=res_geom(i)%toparea * 1.0e6
        !--------Yres(i)=res_geom(i)%stage
        Dres(i)=res_geom(i)%botelv
        do j=1,res_geom(i)%nconnect
            node=res_geom(i)%node_no(j)
            !-----------assign connection to a fourpt sum-of-flow channel
            intchan=0
            do kk=1,node_geom(res_geom(i)%node_no(j))%nup
                intchan=node_geom(node)%upstream(kk)
                if (upboundarycode(intchan) == 12) goto 501
            enddo
            do kk=1,node_geom(res_geom(i)%node_no(j))%ndown
                intchan=-node_geom(node)%downstream(kk)
                if (downboundarycode(abs(intchan)) == 12) goto 501
            enddo
501     continue
        ResConnectingChannels(i,j)=intchan
    enddo
    do j=1,res_geom(i)%nconnect
        ReservoirCoeff(i,j,1)=res_geom(i)%coeff2chan(j)*sqrt2g
        ReservoirCoeff(i,j,2)=res_geom(i)%coeff2res(j)*sqrt2g
    enddo
enddo

do pth=1,ninpaths
    !--------convert node flow and stage inputs to a hydro channel
    role = pathinput(pth)%data_type
    is_node_flow = role == obj_boundary_flow .or. role == obj_source_sink
    if (pathinput(pth)%obj_type == obj_node) then
        node=pathinput(pth)%obj_no
        pathinput(pth)%locnum=node2hydrochan(node,is_node_flow)
    endif
    !--------set external flow index
    if (is_node_flow) then
        ! external flow or source/sink for node
        nqext=nqext+1
        qext(nqext)%name=pathinput(pth)%name
        call datasource_from_path(qext(nqext)%datasource,pth,pathinput(pth))
        qext(nqext)%attach_obj_type=pathinput(pth)%obj_type
        qext(nqext)%attach_obj_name=pathinput(pth)%obj_name
        qext(nqext)%attach_obj_no=pathinput(pth)%obj_no
        qext(nqext)%mass_frac=pathinput(pth)%mass_frac
    !-----------for node, use node number for object name
    endif

100 continue
    enddo

    !-----convert obj2obj node flow to a hydro channel
    do i=1,nobj2obj
        if (obj2obj(i)%from_obj%obj_type == obj_node) then
            node=obj2obj(i)%from_obj%obj_no
            obj2obj(i)%from_obj%hydrochan=node2hydrochan(node,.true.)
        endif
        if (obj2obj(i)%to_obj%obj_type == obj_node) then
            node=obj2obj(i)%to_obj%obj_no
            obj2obj(i)%to_obj%hydrochan=node2hydrochan(node,.true.)
        endif
    enddo

    !-----fill in reservoir numbers for object-to-object connections with
    !-----reservoirs, and input path indices for labeled connections
    do i=1,max_reservoirs
        cresnames(i)=res_geom(i)%name
    enddo

    do i=1,ninpaths
        cinputnames(i)=pathinput(i)%name
    enddo
    if (ninpaths < max_inputpaths) cinputnames(ninpaths+1)=' '

    do i=1,nobj2obj
        obj2obj(i)%datasource%indx_ptr=miss_val_i
        !--------assign an input path (dss or constant valued)
        do pth=1,ninpaths
            if ((pathinput(pth)%obj_type == obj_obj2obj) &
                .and. (pathinput(pth)%obj_name == obj2obj(i)%name)) then ! fixme:
                call datasource_from_path(obj2obj(i)%datasource,pth,pathinput(pth))
                obj2obj(i)%flow=fetch_data(obj2obj(i)%datasource)
                ! todo: is below implemented right? Seems to be initialized as -901.
                ! making it irrelevant
                obj2obj(i)%prev_flow=obj2obj(i)%flow
                exit
            end if
        end do

        if (obj2obj(i)%datasource%indx_ptr == miss_val_i) then
            write(unit_error,'(a/,a)') &
                'No input path found for transfer: ' &
                ,obj2obj(i)%name
            goto 900
        end if

    enddo

    !-----generate an index of which internal flows
    !-----correspond to which nodes and reservoirs;
    do i=1,max_nodes
        nintnode(i)=0
        nextnode(i)=0
    enddo
    do i=1,max_reservoirs
        nintres(i)=0
        nextres(i)=0
    enddo
    do i=1,nobj2obj
        number=obj2obj(i)%from_obj%obj_no
        name=obj2obj(i)%from_obj%obj_name
        if (obj2obj(i)%from_obj%obj_type == obj_node) then
            if (nintnode(number) < max_qobj) then
                nintnode(number)=nintnode(number)+1
            else
                write(unit_error,830) 'internal','node',trim(name)
                call exit(2)
            endif
            node_geom(number)%qinternal(nintnode(number))=i
        endif
        if (obj2obj(i)%from_obj%obj_type == obj_reservoir) then
            if (nintres(number) < max_qobj) then
                nintres(number)=nintres(number)+1
            else
                write(unit_error,830) 'internal','reservoir',trim(name)
                call exit(2)
            endif
            res_geom(number)%qinternal(nintres(number))=i
        endif

        !--------to object
        number=obj2obj(i)%to_obj%obj_no
        name=obj2obj(i)%to_obj%obj_name
        if (obj2obj(i)%to_obj%obj_type == obj_node) then
            if (nintnode(number) < max_qobj) then
                nintnode(number)=nintnode(number)+1
            else
                write(unit_error,830) 'internal','node',trim(name)
                call exit(2)
            endif
            node_geom(number)%qinternal(nintnode(number))=i
        endif
        if (obj2obj(i)%to_obj%obj_type == obj_reservoir) then
            if (nintres(number) < max_qobj) then
                nintres(number)=nintres(number)+1
            else
                write(unit_error,830) 'internal','reservoir',trim(name)
                call exit(2)
            endif
            res_geom(number)%qinternal(nintres(number))=i
        endif

    enddo

    !-----generate an index of which external flows
    !-----correspond to which nodes and reservoirs;
    do i=1,nqext
        number=qext(i)%attach_obj_no
        name=qext(i)%attach_obj_name
        if (qext(i)%attach_obj_type == obj_node) then
            if (nextnode(number) < max_qobj) then
                nextnode(number)=nextnode(number)+1
            else
                write(unit_error,830) 'external','node',trim(name)
                call exit(2)
            endif
            node_geom(number)%qext(nextnode(number))=i
        endif
        if (qext(i)%attach_obj_type == obj_reservoir) then
            if (nextres(number) < max_qobj) then
                nextres(number)=nextres(number)+1
            else
                write(unit_error,830) 'external','reservoir',trim(name)
                call exit(2)
            endif
            res_geom(number)%qext(nextres(number))=i
        endif

    enddo

    call print_output(istat)  ! echo fixed output

    return

900 continue                  ! here for fatal error

    istat=-2
    return

end

integer function node2hydrochan(node,data_flow_type)
      
    !-----convert a DSM2 node number to a hydro connecting channel,
    !-----depending on input data type (flow or stage)
    use IO_Units
    use grid_data
    use constants
    use network
    use netcntrl
    use chconnec
    use chnluser
    use channel_xsect_tbl
    implicit none

    !-----arguments
    integer node              ! DSM2 node number [INPUT]
    logical data_flow_type    ! true if input is flow type [INPUT]

    !-----include files

      
    !-----local variables
    integer j                 ! index
    !-----flow inputs must go to a hydro channel with a flow condition code
    !-----(2 or 12)
    node2hydrochan=miss_val_i
    if (node_geom(node)%boundary_type == stage_boundary) then ! specified-stage boundary condition
        !--------cannot have flow to fixed stage node
        if (data_flow_type) then
            write(unit_error, 610) node_geom(node)%node_ID
610         format(/'Error: cannot have external or internal flow' &
                /'at specified-stage node: ',i3)
            call exit(2)
        endif
        if (node_geom(node)%ndown > 0) then
            node2hydrochan=-node_geom(node)%downstream(1)
        else
            node2hydrochan=node_geom(node)%upstream(1)
        endif
    endif
      
    if (data_flow_type) then  ! node flow input
        node2hydrochan=0
        !--------check upstream channel end connections to node first...
        ! eli: heavily rewritten for intel
        do j=1,node_geom(node)%nup
            if (upboundarycode(node_geom(node)%upstream(j)) == 12 &
                .or. &
                upboundarycode(node_geom(node)%upstream(j)) == 2) then
                node2hydrochan=node_geom(node)%upstream(j)
                return
            end if
        enddo
        do j=1, node_geom(node)%ndown
            if(downboundarycode(node_geom(node)%downstream(j)) == 12 &
                .or. &
                downboundarycode(node_geom(node)%downstream(j)) == 2) then
                node2hydrochan=-node_geom(node)%downstream(j)
                return
            end if
        enddo
    endif

    if (node2hydrochan == miss_val_i) then
        write(unit_error, 611) node_geom(node)%node_ID
611     format(/'Error: Node ',i3,' is neither stage- nor flow-boundary condition')
        call exit(2)
    endif

    return
end


subroutine datasource_from_path(source,ndx,path)
    use iopath_data
    use constants
    implicit none

    type(pathinput_t) path
    type(datasource_t) source
    integer ndx
    if (path%constant_value /= miss_val_r) then
        source%value=path%constant_value
        source%source_type=const_data
        source%indx_ptr=0
    else
        source%value=miss_val_r
        source%source_type=dss_data
        source%indx_ptr=ndx
    end if
    return
end subroutine

