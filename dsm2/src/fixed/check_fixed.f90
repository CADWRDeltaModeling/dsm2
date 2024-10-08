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

subroutine check_fixed(istat)

!-----Check the fixed input for omissions and errors before starting
!-----the model run.  Supply default values where possible.  Translate
!-----from nodes to channel numbers, and from external channel numbers
!-----to internal.
    use io_units

    use gates, only: gateArray, nGate
    use groups, only: groupArray, groupContains, ConvertGroupPatternsToMembers, &
                      GROUP_ALL, IsAllChannelReservoir, GroupTarget
    use constants
    use logging
    use common_qual
    use grid_data
    use iopath_data
    use runtime_data
    use common_xsect
    use common_tide
    use envvar

    use network
    use chconnec
    ! use dss
    use mod_readdss
    use mod_writedss

    use utilities, only: loccarr, jmin2cdt, incr_intvl

    implicit none

!-----Local variables

    logical &
        constituent_input(max_constituent), & ! true if an input path was found for constituent
        uniq_constituent, & ! function to determine if this constituent is unique
        ncc, & ! function to determine if this constituent is non-conservative
        lstat               ! logical status

    integer &
        istat, & ! status of call (returned)
        ios, & ! i/o status
        i, j, k, p, & ! indices
        nlen, & ! character string length
        n_tidefiles_used, & ! number of tide files used to cover simulation period
        loc, & ! array index; function to find string in char array
        data_types, & ! function to determine type of data (stage, flow,...)
        replace_status
    integer*4 &
        jmin                ! julian minute

    integer &
        advance, & ! added to the grpindx
        adjpathout, & ! adjusted pathoutput (original npathouts + ngroups)
        outindx, & ! holds value of original npathouts
        itmp                ! index
    integer ext2int           ! function for converting external channel # to internal

    integer &
        extchan, &
        intchan, &
        extupnode, &
        extdownnode, &
        intupnode, &
        intdownnode, &
        intgatenode, &
        extgatenode

    integer &
        target_type, &
        target_id

    integer :: inode, ibound

    integer, parameter:: &
        max_nc = 11, &
        n_required_do = 7, &
        n_required_algae = 7

    character &
        cdist*10, & ! channel distance
        modifier*32, & ! DSS f_part
        ctmp*150, & ! temporary string
        dsm2_agency*150, & ! temp storage for
        ca*32, cb*32, cc*32, cf*32, & ! DSS path parts
        path*(6*32), & ! temp DSS pathname
        res_names(max_reservoirs)*32, & ! vector of reservoir names
        const_names(max_constituent)*32, & ! vector of constituent names
        diff2dates*14, & ! return DSS date given start and diff
        path_constituent*16, & ! constituent name for this path
        non_constituents(max_nc)*16, & ! list of inputs that are not constituents
        required_do(n_required_do)*32, & ! list of NCCs required for DO
        required_algae(n_required_algae)*32, & ! list of NCCs required for algae
        grpnum*3            ! string conversion of group number

    data &
        constituent_input/max_constituent*.false./ &
        const_names/max_constituent*' '/ &
        non_constituents/ &
        'stage ', &
        'flow ', &
        'flow-net ', &
        'flow-source ', &
        'pump ', &
        'vel ', &
        'cloud ', &
        'dry_bulb ', &
        'wet_bulb ', &
        'wind ', &
        'atm_pressure' &
        /

601 format(/'Error opening output file:', a &
            /'Status value: ', i3, /'(Does the directory exist?)')

602 format(/'Error opening/reading restart input file: ', a)

603 format('Using ', a, ' file date to start run: ', a)

606 format(/'Invalid ', a, ' date of ', a, ' in tidefile:' &
            /a)

607 format(/'max_translations parameter must be set bigger;' &
            /'Need ', i3, ' currently ', i3)

608 format(/'Number of channels in CHANNEL section:', i4 &
            /'not equal to number given in LIST_CHAN section:', i4)

609 format(/'The ', a, ' name ''', a, ''' cannot be used as a translation name.')

610 format(/'No ', a, ' with name: ', a)

620 format(/'Flow output requested for reservoir ', a, &
            ' but no node was specified.')

630 format(/'For reservoir ', a, ' invalid node specified' &
            /'for flow output: ', i5)

635 format(/'Output pathname' &
            /a/ &
            'has an incorrect channel distance: ', i6)

640 format(/'Cross-section', i4, ' at channel', i4, &
            /'did not have an initial value entered for stage and flow.')

642 format(/a, i4, ' does not have a name, channel, node or reservoir.')

644 format(/a, ' name ', a, ' does not have a translation.')

646 format(/'Input line: '4(a, ' ')/' has only a channel/distance translation.')

649 format(/'No name, node, or channel number for this input path' &
            i3)

651 format(/'Number of input paths for ', a &
            ' exceed allowable limit:', i4)

652 format(/'Number of output paths for ', a &
            ' exceed allowable limit:', i4)

670 format(/'Output line: '4(a, ' ') &
            /'is given for chan/dist and node (', i3, '); node output used.')

671 format(/'Output line: '4(a, ' ') &
            /'is not valid at a node (', i3, '); use channel/distance instead.')

650 format(/'Unrecognized ', a, ' data interval: ', i4, 1x, a)

685 format(/'Error: data syncronization requested for this path ' &
            /a/'but the time interval is not minutes, hours, day, or months.')

690 format(/'Warning: the following input path is not being used,' &
            /'perhaps because no output was requested of its constituent:'/a)

695 format(/'Error: Maximum number of unique accounting names exceeded:', i3)

697 format(/'Error: 'a, ' accounting name ''', a, '''' &
            /' was not given in the TYPE input section.')

698 format(/'Error: warmup run requested but first tidefile ', &
            'is not a repeating tide.')

705 format(' Error...flow output at reservoir ', a, &
           ' is at incorrect node: ', i3)

!-----check if output filename supplied, if not use default
    if (output_filename .eq. ' ') then
        output_filename = 'DSM2-'//dsm2_name
    end if

!-----open output file(s)
    open ( &
        unit=unit_output, & ! text output for run
        file=output_filename, &
        iostat=ios, &
        buffered='NO', &
        err=901 &
        )

!-----adjust totals
    nprints = nprints - 1

!-----warning fix, until scalar variables fixed
    cont_missing = cont_missing .and. cont_bad

!-----fixme: make sure reservoir is attached to something?

!-----assign rectangular or irregular xsects to upstream,
!-----middle, or downstream of channels;
!-----copy bottom elevations to chan_geom structure

    call process_irreg

!-----identify gate location (up- or down-stream)
!     flowDirect indicates whether gate is upstream or downstream by identifying
!     how the flow convention for the gate (waterbody to node) is oriented with the
!     flow convention for the water body (e.g., upstream to downstream)

    do i = 1, ngate
        gateArray(i)%flowDirection = 1.0D0 ! this initialization works for gates from reservoirs
        intchan = gateArray(i)%objConnectedID
        extchan = chan_geom(intchan)%chan_no
        intupnode = chan_geom(intchan)%upnode
        intdownnode = chan_geom(intchan)%downnode
        extupnode = node_geom(intupnode)%node_ID
        extdownnode = node_geom(intdownnode)%node_ID
        intgatenode = gateArray(i)%node
        extgatenode = node_geom(intgatenode)%node_ID
        if (gateArray(i)%objConnectedType .eq. obj_channel) then
            ! fixme: does this still work?
            if (extupnode .eq. extgatenode) then
                gateArray(i)%flowDirection = -1.D0
            else if (extdownnode .eq. extgatenode) then
                gateArray(i)%flowDirection = 1.D0
            else
                write (unit_error, *) &
                    'Invalid node number for gate ', &
                    trim(gateArray(i)%name), &
                    ' channel #', extchan, &
                    ' node #', extgatenode
                goto 900
            end if
!-----------id must be reset to internal channel number.
            gateArray(i)%objConnectedID = intchan
        end if
    end do

!-----reservoir name vectors to be able to use
    do i = 1, max_reservoirs
        res_names(i) = res_geom(i)%name
    end do

    inode = 0
    do i = 1, nreser
        res_geom(i)%first_connect_index = inode + 1
        do j = 1, res_geom(i)%nnodes
            inode = inode + 1
        end do
    end do

    do i = 1, noutpaths          ! output paths
        if (pathoutput(i)%use) then
!-----------change stage output at node to channel/distance
!-----------fixme: this
!-----------could be dangerous near gates. If upstream(1) isn't
!-----------guaranteed to be a reference channel for the gate. It is
!-----------also married to the "equal stage" compatibility condition
!-----------which wasn't always used in FourPt, isn't used in the Fisher
!-----------Delta Model (both consider equal energy instead).
!-----------Why is allowing stage at a node important
!-----------given that it is more ambiguous, redundant.
            if (pathoutput(i)%obj_type .eq. obj_node) &
                then
                if (pathoutput(i)%meas_type .eq. 'stage') then
                    pathoutput(i)%obj_type = obj_channel
                    if (node_geom(pathoutput(i)%obj_no)%nup .gt. 0) then
                        pathoutput(i)%obj_no = &
                            node_geom(pathoutput(i)%obj_no)%upstream(1)
                        pathoutput(i)%chan_dist = 0
                    else
                        pathoutput(i)%obj_no = &
                            node_geom(pathoutput(i)%obj_no)%downstream(1)
                        pathoutput(i)%chan_dist = chan_length
                    end if
                end if

!--------------try to change flow or velocity output at node to channel/distance:
!--------------must have only two channels connecting

                if (pathoutput(i)%meas_type .eq. 'flow' .or. &
                    pathoutput(i)%meas_type .eq. 'vel') then
                    if (node_geom(pathoutput(i)%obj_no)%nup .eq. 1) then
                        pathoutput(i)%obj_type = obj_channel
                        pathoutput(i)%obj_no = node_geom(pathoutput(i)%obj_no)%upstream(1)
                        pathoutput(i)%chan_dist = 0
                    else if (node_geom(pathoutput(i)%obj_no)%ndown .eq. 1) then
                        pathoutput(i)%obj_type = obj_channel
                        pathoutput(i)%obj_no = node_geom(pathoutput(i)%obj_no)%downstream(1)
                        pathoutput(i)%chan_dist = chan_length
                    else
!--------------------can't have flow/vel at a node with multiple channels
                        write (unit_error, 671) &
                            trim(pathoutput(i)%name), &
                            trim(pathoutput(i)%meas_type), &
                            trim(pathoutput(i)%interval), &
                            trim(pathoutput(i)%modifier), &
                            pathoutput(i)%obj_no
                        goto 900
                    end if
                end if
            end if

!-----------check reservoir output
            if (pathoutput(i)%name .ne. ' ' .and. &
                pathoutput(i)%obj_type .eq. obj_reservoir) then
                if (pathoutput(i)%meas_type .eq. 'stage') then
                    pathoutput(i)%res_node_no = 0
                end if
!--------------check for valid reservoir name
                loc = loccarr(pathoutput(i)%obj_name, res_names, max_reservoirs, &
                              EXACT_MATCH)
                if (loc .le. 0) then ! no reservoir with that name
                    write (unit_error, 610) 'reservoir', &
                        trim(pathoutput(i)%obj_name)
                    goto 900
                else
                    pathoutput(i)%obj_no = loc
                    if (pathoutput(i)%res_node_no .gt. 0) then
!--------------------check that if a node was given, flow was specified
                        if (pathoutput(i)%meas_type .ne. 'flow') then
                            write (unit_error, 620) trim(pathoutput(i)%obj_name)
                            goto 900
                        end if
!--------------------check for valid node number
                        if (node_geom(pathoutput(i)%res_node_no)%nup + &
                            node_geom(pathoutput(i)%res_node_no)%ndown &
                            .eq. 0) then
                            write (unit_error, 630) pathoutput(i)%obj_name, &
                                pathoutput(i)%res_node_no
                            goto 900
                        end if
!                  else
!                     if(
!     &                   pathoutput(i).meas_type .ne. 'flow-net' .and.
!     &                   pathoutput(i).meas_type .ne. 'flow-source' ) then
!                         write(unit_error, *)
!     &                      "Only flow-net or flow-source output allowed with no node='none': ",
!     &                      trim(pathoutput(i).obj_name)
!                         goto 900
!                     endif
                    end if
                end if

            end if
        end if
    end do

!-----create DSS input pathnames, check for sign change for each path

    npthsin_min15 = 0
    npthsin_hour1 = 0
    npthsin_day1 = 0
    npthsin_week1 = 0
    npthsin_month1 = 0
    npthsin_year1 = 0
    npthsin_irr = 0

    do p = 1, ninpaths
        if (pathinput(p)%no_intervals .eq. 1 .and. &
            pathinput(p)%interval .eq. '15min') then ! eli
            npthsin_min15 = npthsin_min15 + 1
            if (npthsin_min15 .gt. max_inp_min) then
                write (unit_error, 651) '15MIN', max_inp_min
                goto 900
            end if
            pathinput(p)%intvl_path = npthsin_min15
            ptin_min15(npthsin_min15) = p
        else if (pathinput(p)%no_intervals .eq. 1 .and. &
                 pathinput(p)%interval(:5) .eq. '1hour') then !eli could be 1hour
            npthsin_hour1 = npthsin_hour1 + 1
            if (npthsin_hour1 .gt. max_inp_hour) then
                write (unit_error, 651) '1HOUR', max_inp_hour
                goto 900
            end if
            pathinput(p)%intvl_path = npthsin_hour1
            ptin_hour1(npthsin_hour1) = p
        else if (pathinput(p)%no_intervals .eq. 1 .and. &
                 pathinput(p)%interval(:4) .eq. '1day') then
            npthsin_day1 = npthsin_day1 + 1
            if (npthsin_day1 .gt. max_inp_day) then
                write (unit_error, 651) '1DAY', max_inp_day
                goto 900
            end if
            pathinput(p)%intvl_path = npthsin_day1
            ptin_day1(npthsin_day1) = p
        else if (pathinput(p)%no_intervals .eq. 1 .and. &
                 pathinput(p)%interval(:5) .eq. '1week') then
            npthsin_week1 = npthsin_week1 + 1
            if (npthsin_week1 .gt. max_inp_week) then
                write (unit_error, 651) '1WEEK', max_inp_week
                goto 900
            end if
            pathinput(p)%intvl_path = npthsin_week1
            ptin_week1(npthsin_week1) = p
        else if (pathinput(p)%no_intervals .eq. 1 .and. &
                 pathinput(p)%interval(:4) .eq. '1mon') then
            npthsin_month1 = npthsin_month1 + 1
            if (npthsin_month1 .gt. max_inp_month) then
                write (unit_error, 651) '1MON', max_inp_month
                goto 900
            end if
            pathinput(p)%intvl_path = npthsin_month1
            ptin_month1(npthsin_month1) = p
        else if ((pathinput(p)%no_intervals .eq. 1 .and. &
                  pathinput(p)%interval(:5) .eq. '1year') .or. &
                 pathinput(p)%constant_value .ne. miss_val_r &! constant value: use 1year
                 ) then
            pathinput(p)%no_intervals = 1
            pathinput(p)%interval = 'year'
            npthsin_year1 = npthsin_year1 + 1
            if (npthsin_year1 .gt. max_inp_year) then
                write (unit_error, 651) '1YEAR', max_inp_year
                goto 900
            end if
            pathinput(p)%intvl_path = npthsin_year1
            ptin_year1(npthsin_year1) = p
        else if (pathinput(p)%interval(:3) .eq. 'ir-') then ! irregular interval
            npthsin_irr = npthsin_irr + 1
            if (npthsin_irr .gt. max_inp_irr) then
                write (unit_error, 651) 'IR-', max_inp_irr
                goto 900
            end if
            pathinput(p)%intvl_path = npthsin_irr
            ptin_irr(npthsin_irr) = p
        else                   ! unrecognized interval
            write (unit_error, 650) 'input', pathinput(p)%no_intervals, &
                trim(pathinput(p)%interval)
            goto 900
        end if
        call upcase(pathinput(p)%path) ! convert to upper case
    end do

    dsm2_agency = ' '
    replace_status = 0
    !replace_status=replace_envvars('$(DSM2AGENCY)', dsm2_agency)

    if (dsm2_module .eq. qual) then

!--------Conservative and nonconservative constituents.
!--------For conservative constituents (CCs), note that a 'constituent' is
!--------really each unique combination of constituent and source group, while for
!--------non-conservative constituents (NCCs), each constituent type
!--------is considered unique, regardless of source, since
!--------source tracking is not allowed for NCCs.

!--------First form a list of constituents to track from the output
!--------pathnames. Assume that any DSS C outputpath part that is not
!--------stage, flow, meteorological, or non-conservative, is a
!--------conservative constituent.  For CCs, use *only* the output paths,
!--------since there's no point in tracking a constituent in the input if
!--------the user doesn't want its output.  For NCCs, since one
!--------constituent may require others to react with, we may have to add
!--------others constituents to the tracking list.

!--------After the list is created, abort if there is not at least one
!--------input for each constituent to track.  That checking is done
!--------in check_fixed_qual

        no_of_constituent = 0
        no_of_nonconserve_constituent = 0
        do p = 1, noutpaths

            loc = loccarr(pathoutput(p)%meas_type, non_constituents, max_nc, &
                          EXACT_MATCH)
            if (loc .le. 0) then ! not a "non-constituent" - must be a CC or NCC water quality constituent
                if (uniq_constituent(p)) then
!-----------------new, unique constituent to add to tracking list
                    no_of_constituent = no_of_constituent + 1
                    if (no_of_constituent .gt. max_constituent) then
                        write (unit_error, *) &
                            'Error: too many different output constituents specified.'
                        write (unit_error, *) &
                            'Max allowed is ', max_constituent
                        goto 900
                    end if

                    pathoutput(p)%const_ndx = no_of_constituent
                    constituents(no_of_constituent)%name = &
                        pathoutput(p)%meas_type
                    constituents(no_of_constituent)%conservative = &
                        .not. ncc(pathoutput(p)%meas_type)
                    constituents(no_of_constituent)%group_ndx = &
                        pathoutput(p)%source_group_ndx

                    if (.not. constituents(no_of_constituent)%conservative) then ! non-conservative constituent
                        no_of_nonconserve_constituent = no_of_nonconserve_constituent + 1
                        nonconserve_ptr(no_of_nonconserve_constituent) = no_of_constituent
                    end if
                end if
            end if
        end do

!--------for conservative constituents, make sure that each different
!--------chemical constituent has at least one 'all' source if a
!--------restart file is requested
        if (io_files(qual, io_restart, io_write)%use) then
            j = 0
            do i = 1, no_of_constituent
                if (constituents(i)%conservative .and. &
                    loccarr(constituents(i)%name, const_names, &
                            no_of_constituent, EXACT_MATCH) .le. 0) then
!-----------------conservative chemical not found before
                    j = j + 1
                    const_names(j) = constituents(i)%name
!-----------------check if an 'all' source already exists for this one
                    do k = i, no_of_constituent
                        if ((constituents(i)%name .eq. constituents(k)%name) .and. &
                            constituents(k)%group_ndx .eq. GROUP_ALL) then
                            goto 800
                        end if
                    end do
!-----------------no 'all' source found, make one
                    no_of_constituent = no_of_constituent + 1
                    if (no_of_constituent .ge. max_constituent) then
                        write (unit_error, *) &
                            'Error: too many different output constituents specified.'
                        write (unit_error, *) &
                            'Max allowed is ', max_constituent
                        goto 900
                    end if
                    constituents(no_of_constituent)%name = &
                        constituents(i)%name
                    constituents(no_of_constituent)%conservative = .true.
                    constituents(j)%group_ndx = GROUP_ALL
800                 continue      ! here to skip making 'all' source
                end if
            end do
        end if

!--------add needed constituents for DO and algae
!--------passing slices of structures in calls, chokes
        do i = 1, max_constituent
            const_names(i) = constituents(i)%name
        end do

!--------DO first
        loc = loccarr(nonconserve_list(ncc_do), const_names, &
                      max_constituent, EXACT_MATCH)

        if (loc .gt. 0) then   ! DO is tracked; are other required NCCs in list?
            required_do(1) = nonconserve_list(ncc_organic_n)
            required_do(2) = nonconserve_list(ncc_organic_p)
            required_do(3) = nonconserve_list(ncc_nh3)
            required_do(4) = nonconserve_list(ncc_no3)
            required_do(5) = nonconserve_list(ncc_po4)
            required_do(6) = nonconserve_list(ncc_bod)
            required_do(7) = nonconserve_list(ncc_algae)
            do i = 1, n_required_do
                loc = loccarr(required_do(i), const_names, max_constituent, EXACT_MATCH)
                if (loc .le. 0) then ! required NCC is not in list; add it
                    no_of_constituent = no_of_constituent + 1
                    if (no_of_constituent .ge. max_constituent) then
                        write (unit_error, *) &
                            'Error: too many different output constituents specified.'
                        write (unit_error, *) &
                            'Max allowed is ', max_constituent
                        goto 900
                    end if
                    constituents(no_of_constituent)%name = required_do(i)
                    constituents(no_of_constituent)%conservative = .false.
!fixme:groups is this right?
                    constituents(no_of_constituent)%group_ndx = GROUP_ALL
                    no_of_nonconserve_constituent = no_of_nonconserve_constituent + 1
                    nonconserve_ptr(no_of_nonconserve_constituent) = no_of_constituent
                end if
            end do
        end if

!--------update const_names if any constituent/s were asked for simulation
!--------which already fall within the group required for DO simulation
        do i = 1, max_constituent
            const_names(i) = constituents(i)%name
        end do

!--------then algae
        loc = loccarr(nonconserve_list(ncc_algae), const_names, &
                      max_constituent, EXACT_MATCH)

        if (loc .gt. 0) then   ! algae is tracked; are other required NCCs in list?
            required_algae(1) = nonconserve_list(ncc_organic_n)
            required_algae(2) = nonconserve_list(ncc_organic_p)
            required_algae(3) = nonconserve_list(ncc_nh3)
            required_algae(4) = nonconserve_list(ncc_no3)
            required_algae(5) = nonconserve_list(ncc_po4)
            required_algae(6) = nonconserve_list(ncc_bod)
            required_algae(7) = nonconserve_list(ncc_do)
            do i = 1, n_required_algae
                loc = loccarr(required_algae(i), const_names, max_constituent, EXACT_MATCH)
                if (loc .le. 0) then ! required NCC is not in list; add it
                    no_of_constituent = no_of_constituent + 1
                    if (no_of_constituent .ge. max_constituent) then
                        write (unit_error, *) &
                            'Error: too many different output constituents specified.'
                        write (unit_error, *) &
                            'Max allowed is ', max_constituent
                        goto 900
                    end if
                    constituents(no_of_constituent)%name = required_algae(i)
                    constituents(no_of_constituent)%conservative = .false.
                    !fixme:groups is this right?
                    constituents(no_of_constituent)%group_ndx = GROUP_ALL
                    no_of_nonconserve_constituent = no_of_nonconserve_constituent + 1
                    nonconserve_ptr(no_of_nonconserve_constituent) = no_of_constituent
                end if
            end do
        end if

!--------pathinput().const_ndx keeps track of multiple source
!--------constituents which may come from the same input (applies only to conservative).

        do p = 1, ninpaths
            pathinput(p)%n_consts = 0
            path_constituent = pathinput(p)%variable
            do j = 1, no_of_constituent
                if (path_constituent .eq. constituents(j)%name) then
                    if ( &
                        (.not. constituents(j)%conservative) &
                        .or. &
                        (constituents(j)%group_ndx .eq. GROUP_ALL)) then
                        pathinput(p)%n_consts = pathinput(p)%n_consts + 1
                        pathinput(p)%const_ndx(pathinput(p)%n_consts) = j
                        constituent_input(j) = .true.
                    else
                        call GroupTarget(pathinput(p)%data_type, pathinput(p)%name, &
                                         target_type, target_id)
                        if (target_id .eq. miss_val_i) then
                            !fixme: stage boundaries not identified in qual at this stage??
                            call GroupTarget(obj_stage, pathinput(p)%name, target_type, target_id)
                        end if
                        if (target_id .eq. miss_val_i) then
                            write (unit_error, *) "Source group not found: ", pathinput(p)%name
                            call exit(2)
                        end if
                        if (constituents(j)%group_ndx .lt. 0) then
                            write (unit_error, *) "Error with constituent group index"
                            write (unit_error, *) "Constituent index: ", j, &
                                " Name: ", constituents(j)%name, &
                                "Group index: ", constituents(j)%group_ndx
                            call exit(2)
                        end if

                        if (GroupContains(constituents(j)%group_ndx, &
                                          target_type, target_id)) then
                            pathinput(p)%n_consts = pathinput(p)%n_consts + 1
                            pathinput(p)%const_ndx(pathinput(p)%n_consts) = j
                            constituent_input(j) = .true.
                        end if
                    end if
                end if
            end do
!-----------generate warnings about input constituents that aren't used
            loc = loccarr(path_constituent, non_constituents, max_nc, &
                          EXACT_MATCH)
            if (pathinput(p)%n_consts .eq. 0 .and. loc .le. 0) then
                write (unit_error, 690) trim(pathinput(p)%path)
            end if
        end do

!--------create index array of constituents from all sources
        no_all_source = 0
        do i = 1, no_of_constituent
            if (constituents(i)%group_ndx .eq. GROUP_ALL) then
                no_all_source = no_all_source + 1
                all_source_ptr(no_all_source) = i
            end if
        end do
    end if

!-----do tracked constituents have inputs?
    do j = 1, no_of_constituent
        if (.not. constituent_input(j)) then
            write (unit_error, 631) &
                trim(constituents(j)%name), &
                trim(groupArray(constituents(j)%group_ndx)%name)
631         format(/'Warning:   Output constituent does not have any matching input.'/ &
                    'Check input and output to make sure constituent name is not misspelled.'/ &
                    'This is either an error or the constituent is totally dependent on initial condition.'/ &
                    'Requested output constituent: ', a, '; source group: ', a)

        end if
    end do

!-----create DSS output pathnames, check for sign change for each path
!-----change channel distances to marker if == channel length
    do p = 1, noutpaths
        pathoutput(p)%need_tmp_outfile = .false.
!--------remove output file, if text, and flag if tmp output files are needed
        if (index(pathoutput(p)%filename, '.dss') .eq. 0) then
            call unlink(pathoutput(p)%filename)
            need_tmp_outfiles = .true.
            pathoutput(p)%need_tmp_outfile = .true.
        else                   ! .DSS file
            if (.not. dss_direct) then
                need_tmp_outfiles = .true.
                pathoutput(p)%need_tmp_outfile = .true.
            end if
        end if
!--------replace magic number channel length with correct channel length
        if (pathoutput(p)%chan_dist .eq. chan_length) &
            pathoutput(p)%chan_dist = &
            chan_geom(pathoutput(p)%obj_no)%length

!-------replace op-to-node with op_to_node
        if (pathoutput(p)%meas_type .eq. 'op-to-node') &
            pathoutput(p)%meas_type = 'op_to_node'
        if (pathoutput(p)%meas_type .eq. 'op-from-node') &
            pathoutput(p)%meas_type = 'op_from_node'

!--------DSS a part
        if (pathoutput(p)%a_part .ne. ' ') then
            ca = pathoutput(p)%a_part
        else                   ! not explicitly given, use IEP format
!-----------ctmp='DSM2-' // trim(dsm2_name) // '-' //
!-----------&           dsm2_version
            ctmp = trim(dsm2_name)//dsm2_version
            ca = ctmp
!-----------if (pathoutput(p).obj_type .eq. obj_channel) then
!-----------ca=trim(ctmp) // '+CHAN'
!-----------else if (pathoutput(p).obj_type .eq. obj_node) then
!-----------ca=trim(ctmp) // '+NODE'
!-----------else if (pathoutput(p).obj_type .eq. obj_reservoir) then
!-----------ca=trim(ctmp) // '+RSVR'
!-----------else if (pathoutput(p).obj_type .eq. obj_gate) then
!-----------ca=trim(ctmp) // '+GATE'
!-----------else if (pathoutput(p).obj_type .eq. obj_qext) then
!-----------ca=trim(ctmp) // '+QEXT'
!-----------else if (pathoutput(p).obj_type .eq. obj_obj2obj) then
!-----------ca=trim(ctmp) // '+OBJ2OBJ'
!-----------else if (pathoutput(p).obj_type .eq. obj_flux) then
!-----------ca=trim(ctmp) // '+FLUX'
!-----------else if (pathoutput(p).obj_type .eq. obj_stage) then
!-----------ca=trim(ctmp) // '+STAGE'
!-----------else
!-----------ca=trim(ctmp) // '+UNK'
!-----------endif
        end if
!--------DSS b part
        if (pathoutput(p)%b_part .ne. ' ') then
            cb = pathoutput(p)%b_part
        else                   ! not explicitly given
!-----------if name given, use that, else channel/dist; node number; reservoir name
            if (pathoutput(p)%name .ne. ' ') then ! use translation name
                cb = pathoutput(p)%name
!--------------object is a reservoir, and output is flow through a node,
!--------------then add on node number to reservoir name
                if (pathoutput(p)%obj_type .eq. obj_reservoir .and. &
                    pathoutput(p)%res_node_no .gt. 0) then
                    write (ctmp, '(i3)') node_geom(pathoutput(p)%res_node_no)%node_id
                    cb = trim(cb)//'-NODE'//trim(ctmp)
                end if
            else                ! use chan/dist; node number
                if (pathoutput(p)%obj_type .eq. obj_channel) then
                    write (cdist, '(i10)') pathoutput(p)%chan_dist
                    write (cb, '(i3.3,''_'',a)') pathoutput(p)%obj_no, cdist
                else if (pathoutput(p)%obj_type .eq. obj_node) then
                    write (cb, '(i3)') node_geom(pathoutput(p)%obj_no)%node_ID
                end if
            end if
        end if

!--------DSS c part
        if (pathoutput(p)%c_part .ne. ' ') then
            cc = pathoutput(p)%c_part
        else                   ! not explicitly given
!-----------c part will be measurement type
            cc = pathoutput(p)%meas_type
        end if

!--------DSS f part
        if (pathoutput(p)%f_part .ne. ' ') then
            cf = pathoutput(p)%f_part
        else                   ! not explicitly given, use IEP format
            cf = ' '
            modifier = ' '
            ctmp = ' '
!-----------agency name
            cf = trim(dsm2_agency)

!-----------optional modifier (study name, etc)
            if (pathoutput(p)%modifier .ne. ' ') then
                modifier = pathoutput(p)%modifier
            else
                replace_status = replace_envvars('$(DSM2MODIFIER)', ctmp)
                if (ctmp .ne. ' ') then
                    modifier = ctmp
                end if
            end if
            if (modifier .eq. 'runtime') then ! fill with runtime: yymmdd.hhmm
                modifier = crdt10(1:6)//'.'//crdt10(7:10)
            end if
            if (modifier .eq. 'none') then
                modifier = ' '
            end if

            if (modifier .ne. ' ' .and. &
                cf .ne. ' ') then
                modifier = '+'//modifier
            end if

            cf = trim(cf)//modifier

!-----------if output is water quality constituent, identify source and
!-----------modify f part
            if (dsm2_module .eq. qual) then
                if (cf .eq. ' ') then
                    ctmp = 'FROM-'
                else
                    ctmp = '+FROM-'
                end if
                do j = 1, no_of_constituent
                    if (constituents(j)%name .eq. pathoutput(p)%meas_type) then
                        if (pathoutput(p)%source_group_ndx .eq. constituents(j)%group_ndx) then
                            pathoutput(p)%const_ndx = j
                            cf = trim(cf)//trim(ctmp) &
                                 //groupArray(constituents(j)%group_ndx)%name
                        end if
                    end if
                end do
            end if
        end if

!--------DSS e part

        if (pathoutput(p)%no_intervals .eq. 1 .and. &
            pathoutput(p)%interval(1:5) .eq. '15min') then
            npthsout_min15 = npthsout_min15 + 1
            if (npthsout_min15 .gt. max_out_min) then
                write (unit_error, 652) '15MIN', max_out_min
                goto 900
            end if
            pathoutput(p)%intvl_path = npthsout_min15
            ptout_min15(npthsout_min15) = p
            if (pathoutput(p)%need_tmp_outfile) then
                need_tmpfile_min15 = .true.
            end if
        else if (pathoutput(p)%no_intervals .eq. 1 .and. &
                 pathoutput(p)%interval(1:5) .eq. '1hour') then
            npthsout_hour1 = npthsout_hour1 + 1
            if (npthsout_hour1 .gt. max_out_hour) then
                write (unit_error, 652) '1HOUR', max_out_hour
                goto 900
            end if
            pathoutput(p)%intvl_path = npthsout_hour1
            ptout_hour1(npthsout_hour1) = p
            if (pathoutput(p)%need_tmp_outfile) need_tmpfile_hour1 = .true.
        else if (pathoutput(p)%no_intervals .eq. 1 .and. &
                 pathoutput(p)%interval(1:4) .eq. '1day') then
            npthsout_day1 = npthsout_day1 + 1
            if (npthsout_day1 .gt. max_out_day) then
                write (unit_error, 652) '1DAY', max_out_day
                goto 900
            end if
            pathoutput(p)%intvl_path = npthsout_day1
            ptout_day1(npthsout_day1) = p
            if (pathoutput(p)%need_tmp_outfile) need_tmpfile_day1 = .true.
        else if (pathoutput(p)%no_intervals .eq. 1 .and. &
                 pathoutput(p)%interval(1:5) .eq. '1week') then
            npthsout_week1 = npthsout_week1 + 1
            if (npthsout_week1 .gt. max_out_week) then
                write (unit_error, 652) '1WEEK', max_out_week
                goto 900
            end if
            pathoutput(p)%intvl_path = npthsout_week1
            ptout_week1(npthsout_week1) = p
            if (pathoutput(p)%need_tmp_outfile) need_tmpfile_week1 = .true.
        else if (pathoutput(p)%no_intervals .eq. 1 .and. &
                 pathoutput(p)%interval(1:4) .eq. '1mon') then
            npthsout_month1 = npthsout_month1 + 1
            if (npthsout_month1 .gt. max_out_month) then
                write (unit_error, 652) '1MON', max_out_month
                goto 900
            end if
            pathoutput(p)%intvl_path = npthsout_month1
            ptout_month1(npthsout_month1) = p
            if (pathoutput(p)%need_tmp_outfile) need_tmpfile_month1 = .true.
        else if (pathoutput(p)%no_intervals .eq. 1 .and. &
                 pathoutput(p)%interval(1:5) .eq. '1year') then
            npthsout_year1 = npthsout_year1 + 1
            if (npthsout_year1 .gt. max_out_year) then
                write (unit_error, 652) '1YEAR', max_out_year
                goto 900
            end if
            pathoutput(p)%intvl_path = npthsout_year1
            ptout_year1(npthsout_year1) = p
            if (pathoutput(p)%need_tmp_outfile) need_tmpfile_year1 = .true.
        else                   ! unrecognized interval
            write (unit_error, 650) 'output', pathoutput(p)%no_intervals, &
                pathoutput(p)%interval
            goto 900
        end if

        path = '/' &
               //trim(ca)//'/' & ! a part
               //trim(cb)//'/' & ! b part
               //trim(cc)//'/' & ! c part
               //'/' & ! d part
               //trim(pathoutput(p)%interval)//'/' & ! e part
               //trim(cf)//'/' ! f part

        call remblk(path, pathoutput(p)%path, nlen)
        call upcase(pathoutput(p)%path) ! convert to upper case
        call zchkpn(trim(path), len_trim(path), istat)
        if (istat .ne. 0) then
            write (unit_error, "(a,a,a,i5)") "Illegal pathname: ", &
                trim(path), " status: ", istat
            goto 900
        end if

!--------check for valid output channel distance
        if (pathoutput(p)%obj_type .eq. obj_channel) then
            if (pathoutput(p)%chan_dist .eq. -1) then
                pathoutput(p)%chan_dist = chan_geom(pathoutput(p)%obj_no)%length
            else if (pathoutput(p)%chan_dist .gt. &
                     chan_geom(pathoutput(p)%obj_no)%length) then
                write (unit_error, 635) trim(pathoutput(p)%path), &
                    pathoutput(p)%chan_dist
                goto 900
            end if
        end if
    end do                     ! pathoutput loop

!-----process DSS input files
!-----set some DSS parameters
    call zset('MLEVEL', '', print_level)

!-----Open the DSS files for reading
    i = 1
    do while (i .le. max_dssinfiles .and. &
              infilenames(i) .ne. ' ')
        call zfname(trim(infilenames(i)), ctmp, nlen, lstat)
        if (.not. lstat) then
            write (unit_error, '(a/a/a)') 'Fatal error - DSS input file', &
                ctmp(:nlen), 'does not exist.'
            goto 900
        end if
        call zopen(ifltab_in(1, i), infilenames(i), istat)
        if (istat .gt. 0) then
            write (unit_error, '(a,a)') 'Unable to open the file ', &
                infilenames(i)
            goto 900
        end if
        i = i + 1
    end do

    return

900 continue                  ! here for fatal error

    istat = -2
    return

901 continue                  ! here for output file open error
    write (unit_error, 601) output_filename, ios
    istat = -1
    return

905 continue                  ! here for restart open/read file error
    write (unit_error, 602) trim(io_files(hydro, io_restart, io_read)%filename)
    istat = -1
    return

end subroutine

!     Convert an external channel number to internal number
!     using a binary search.
integer function ext2int(extchan)
    use ifport
    use grid_data
    implicit none
    integer extchan
    ext2int = bsearchqq(loc(extchan), loc(int2ext(1)), nchans, SRT$INTEGER4)
    return
end function

!     Compare two integers (e.g., as needed for qsort)
integer(2) function compareInt(arg1, arg2)
    implicit none
    integer arg1, arg2
    compareInt = arg1 - arg2
    return
end function

!-----Convert an external node number to an internal one using
!     binary search
integer function ext2intnode(extnode)
    use ifport
    use grid_data
    implicit none
    integer extnode
    ext2intnode = bsearchqq(loc(extnode), loc(nodelist(1)), &
                            nintnodes, SRT$INTEGER4)
    if (ext2intnode .gt. 0) return
    ext2intnode = bsearchqq(loc(extnode), loc(nodelist(nintnodes + 1)), &
                            (nnodes - nintnodes), SRT$INTEGER4)
    if (ext2intnode .gt. 0) then
        ext2intnode = ext2intnode + nintnodes
    end if

    return
end function

logical function ncc(chemical_name)

!-----Return true if given chemical name is a non-conservative constituent
!-----name.

    use common_qual
    use constants
    use utilities, only: loccarr
    implicit none

    character*(*) chemical_name

    ncc = loccarr(chemical_name, nonconserve_list, &
                  max_constituent, EXACT_MATCH) .gt. 0

    return
end

logical function uniq_constituent(outpath)

!-----Check if the specifications for the outpath determine a unique
!-----constituent.  Unique for conservative means the combination
!-----of chemical constituent name, and one of group, source
!-----flow type, or source location, are unique; for nonconservative,
!-----only the chemical name need be unique.
    use common_qual
    use iopath_data
    use constants
    use utilities, only: loccarr
    implicit none

!-----argument

    integer &
        outpath              ! pointer to outputpath structure index

!-----local variables

    logical &
        ncc                  ! function to determine if this constituent is non-conservative

    integer &
        loc, & ! array index; function to find string in char array
        j, n_uniq            ! loop index, number of uniq c parts

    character*15 &
        uniq_c_list(max_constituent) ! list of unique C parts in DSS outputpaths

    data &
        uniq_c_list/max_constituent*' '/ &
        n_uniq/0/

    save uniq_c_list, n_uniq

    uniq_constituent = .false.
    loc = loccarr(pathoutput(outpath)%meas_type, uniq_c_list, &
                  n_uniq, EXACT_MATCH)
    if (loc .le. 0) then      ! new chemical constituent
        n_uniq = n_uniq + 1
        uniq_c_list(n_uniq) = pathoutput(outpath)%meas_type
        uniq_constituent = .true.
        return
    else
        if (ncc(pathoutput(outpath)%meas_type)) then
            uniq_constituent = .false.
            return
        end if
    end if

!-----not a new chemical, and conservative, now check for
!-----different group

    do j = 1, no_of_constituent
        if ( &
            (constituents(j)%name .eq. pathoutput(outpath)%meas_type) &
            .and. &
            (pathoutput(outpath)%source_group_ndx .eq. constituents(j)%group_ndx) &
            ) then
!-----------found same combination, not unique
            uniq_constituent = .false.
            return
        end if
    end do

!-----didn't find same combination
    uniq_constituent = .true.

    return
end
