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

subroutine check_fixed_qual(istat)
    use common_tide
    use common_qual
    use runtime_data
    use grid_data
!-----Check the fixed input for omissions and errors before starting
!-----the model run.  Supply default values where possible.  Translate
!-----from nodes to channel numbers, and from external channel numbers
!-----to internal.  Write to Qual arrays.
    use IO_Units
    use utilities

    use rate_coeff_assignment, only: check_rate_for_waterbody
    implicit none

    include '../qual/param.inc'
    include '../qual/bltm1.inc'
    include '../qual/bltm3.inc'
    include '../qual/bltm2.inc'

!-----Local variables

    logical &
        reorder              ! true if reordering for obj2obj occurred

    integer &
        istat, &! status of call (returned)
        i, j, ij, &! indices
        k, l, &! indices
        ires, &! reservoir index
        chan, intnode        ! channel, node, xsect numbers

    character &
        ctemp(max_reservoirs)*20 ! scratch array

    character errm*128 !error message,Jon

605 format(/'Warning: no rate coefficients given for reservoir ', a/)
607 format(/'Error: unknown reservoir name given for rate coeffients:' &
            /a)

    if (time_step_intvl_qual .ne. ' ') then
!--------time_step_intvl should be in form: '15min' or '1hour'
        time_step = incr_intvl(0, time_step_intvl_qual, TO_BOUNDARY)
    else
        time_step = 15           ! 15 minutes is default time step
    end if

    if (.not. dispersion) then
        do chan = 1, nchans
            chan_geom(chan)%disp = 0
        end do
    end if

!-----set BLTM values
    nbrch = nchans
    neq = no_of_constituent

    do i = 1, nbrch
        nxsec(i) = 2
        dqq(i) = chan_geom(i)%disp
    end do

!-----flag qual internal (not on boundary) nodes
    do intnode = 1, nnodes
        if (node_geom(intnode)%nup + node_geom(intnode)%ndown .gt. 1) then
            node_geom(intnode)%qual_int = .true.
        end if
        do i = 1, node_geom(intnode)%nup
            chan = node_geom(intnode)%upstream(i)
            jncu(chan) = intnode
        end do

        do i = 1, node_geom(intnode)%ndown
            chan = node_geom(intnode)%downstream(i)
            jncd(chan) = intnode
        end do
    end do

!-----Now the reservoirs

    nconres = 0
    do ires = 1, nreser
!--------channel connections to this reservoir
        !ares(ires)=res_geom(ires).toparea
        !hres(ires)=res_geom(ires).botelv
        nresjunc(ires) = res_geom(ires)%nnodes
        do j = 1, res_geom(ires)%nnodes
            reschjunc(ires, j) = res_geom(ires)%node_no(j)
            lresjunc(ires, j) = res_geom(ires)%node_no(j)
            ij = lresjunc(ires, j)

            nconres(ij) = nconres(ij) + 1 ! Number of reservoirs connected to node ij

!-----------For each junction figure out which reservoir (ires) is connected
!-----------Also figure out in the list of connecting junctions to
!-----------this reservoir, which one is this (J)

            lconres(ij, nconres(ij), 1) = ires
            lconres(ij, nconres(ij), 2) = j
        end do

    end do

    if (.not. (check_rate_for_waterbody(unit_error))) then
        write (unit_screen, *) "fatal error checking nonconservative constituent rates"
        call exit(2)
    end if

!-----Half saturation const. for light:
!-----unit conversion from 1/minute to 1/hour

    klight_half = klight_half*60.

!-----Initialize constituent locations
    mtds = 0
    mec = 0
    mcl = 0
    mbod = 0
    mdo = 0
    morgn = 0
    mnh3 = 0
    mno2 = 0
    mno3 = 0
    morgp = 0
    mpo4 = 0
    malg = 0
    mtemp = 0
!-----Loop through list mapping constituent locations
    do i = 1, no_of_constituent
!--------check that group is given
        if (constituents(i)%conservative) then
            if (constituents(i)%group_ndx .eq. miss_val_i) then
                write (unit_error, 620) trim(constituents(i)%name)
                goto 900
620             format(/'Error: the constituent ', a, ' must have specified a group')
            end if
        end if
        if (constituents(i)%name .eq. 'tds') then
            mtds = i
        elseif (constituents(i)%name .eq. 'ec') then
            mec = i
        elseif (constituents(i)%name .eq. 'cl') then
            mcl = i
!-----------now nonconservative constituents
        elseif (constituents(i)%name .eq. 'do') then
            mdo = i
            constituent_ptr(mdo) = ncc_do
        elseif (constituents(i)%name .eq. 'organic_n') then
            morgn = i
            constituent_ptr(morgn) = ncc_organic_n
        elseif (constituents(i)%name .eq. 'nh3') then
            mnh3 = i
            constituent_ptr(mnh3) = ncc_nh3
        elseif (constituents(i)%name .eq. 'no2') then
            mno2 = i
            constituent_ptr(mno2) = ncc_no2
        elseif (constituents(i)%name .eq. 'no3') then
            mno3 = i
            constituent_ptr(mno3) = ncc_no3
        elseif (constituents(i)%name .eq. 'organic_p') then
            morgp = i
            constituent_ptr(morgp) = ncc_organic_p
        elseif (constituents(i)%name .eq. 'po4') then
            mpo4 = i
            constituent_ptr(mpo4) = ncc_po4
        elseif (constituents(i)%name .eq. 'algae') then
            malg = i
            constituent_ptr(malg) = ncc_algae
        elseif (constituents(i)%name .eq. 'bod') then
            mbod = i
            constituent_ptr(mbod) = ncc_bod
        elseif (index(constituents(i)%name, 'temp') .gt. 0) then
            constituents(i)%name = 'temp'
            mtemp = i
            constituent_ptr(mtemp) = ncc_temp
        else
            if (.not. constituents(i)%conservative) then
                write (unit_error, 610) trim(constituents(i)%name)
610             format('Error:  Not one of the expected nonconservative constituents,'/ &
                       ' so can not be simulated: Constituent name is: ', a/)
                goto 900
            end if
        end if
    end do

    return
900 continue                  ! here for fatal error
    istat = -2
    return

end
