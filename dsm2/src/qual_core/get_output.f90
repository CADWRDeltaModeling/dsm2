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

module qual_get_output
    Use io_units
    use mod_qual_node
    implicit none
contains

real*8 function get_output_qual(ptr) result(fn_result)

!-----Get the desired output variable from the particular DSM module
    Use groups_data
    use common_tide
    use grid_data
    use iopath_data
    use qual_param
    use bltm
    use mod_res_rate
    implicit none

!-----arguments

    integer &
        ptr                  ! output path pointer

!-----local variables

    byte object               ! object type (node, channel, ...)

    integer &
        qualchan &             ! dsm and bltm channel numbers
        , object_no &           ! object number
        , hydro_res_no &        ! hydro reservoir number
        , hydro_node_no &       ! hydro node number
        , nx &                  ! number of cross sections for channel
        , i &                   ! loop index
        , lnblnk              ! last non blank intrinsic function

    integer &
        const_no             ! constituent number

    real*8 &
        val_x &               ! interpolated value statement function
        , val_up, val_down &     ! value at upstream and downstream end of chan
        , chan_dist &           ! distance along channel
        , chan_len &            ! channel length
        , objflow, massrate(max_constituent) ! flow and massrate at an object

!-----statement function to interpolate value along channel
    val_x(val_up, val_down, chan_dist, chan_len) = val_up - (val_up &
                                                             - val_down)*(chan_dist/chan_len)

    object = pathoutput(ptr) .obj_type
    object_no = pathoutput(ptr) .obj_no
    if (object .eq. obj_channel) then ! output at channel requested
        qualchan = object_no
    end if
    const_no = pathoutput(ptr) .const_ndx
    if (const_no .gt. 0) then ! water quality constituent requested
        if (object .eq. obj_channel) then ! output at channel requested
            fn_result = chan_qual(qualchan, pathoutput(ptr) .chan_dist, const_no)
        else if (object .eq. obj_node) then ! output at node requested
            fn_result = node_qual(object_no, const_no)
        else if (object .eq. obj_reservoir) then ! output in reservoir requested
            fn_result = cres(object_no, const_no)
        elseif (index(pathoutput(ptr) .meas_type, 'mass') .gt. 0) then
        else                   ! error, can't figure out where this output is
            write (unit_error, '(a/a/a)') 'Unable to produce output for path:', &
                '  '//pathoutput(ptr) .path(:lnblnk(pathoutput(ptr) .path)), &
                '  No location given.'
            call exit(2)
        end if
    elseif (pathoutput(ptr) .meas_type .eq. 'stage') then
        if (object .eq. obj_channel) then ! channel output
            nx = chan_geom(object_no) .nxsect ! last X-Section (i.e. downstream)
            nx = 2 !  fixme: the whole chan_geom(..).nxsect and .bottomelev is bogus and
            ! should be removed
            fn_result = val_x( &
                         zchan(1, qualchan), &
                         zchan(2, qualchan), &
                         dble(pathoutput(ptr) .chan_dist), &
                         dble(chan_geom(object_no) .length))
        else if (object .eq. obj_reservoir) then ! reservoir output
            hydro_res_no = pathoutput(ptr) .obj_no
            fn_result = eresv(hydro_res_no)
        end if
    elseif (pathoutput(ptr) .meas_type .eq. 'flow') then
        if (object .eq. obj_channel) then
            fn_result = val_x(qchan(1, qualchan), qchan(2, qualchan), &
                               dble(pathoutput(ptr) .chan_dist), &
                               dble(chan_geom(object_no) .length))
        else if (object .eq. obj_reservoir) then
            hydro_res_no = pathoutput(ptr) .obj_no
            hydro_node_no = pathoutput(ptr) .res_node_no
            fn_result = -qres(hydro_res_no, hydro_node_no) ! + qres: flow from res to chan
        end if
    elseif (pathoutput(ptr) .meas_type .eq. 'pump') then ! all reservoir pumping
        call res_rate(object_no, FROM_OBJ, NO_CONNECT, objflow, massrate)
        fn_result = objflow
    elseif (pathoutput(ptr) .meas_type .eq. 'flow-net') then ! net reservoir pump+gate flow
        if (object .eq. obj_reservoir) then
            fn_result = 0.0
            do i = 1, res_geom(pathoutput(ptr) .obj_no) .nnodes
                hydro_node_no = res_geom(pathoutput(ptr) .obj_no) .node_no(i)
                fn_result = fn_result - qres(hydro_res_no, i) ! + qres: flow from res to chan
            end do

            call res_rate(object_no, FROM_OBJ, NO_CONNECT, objflow, massrate)
            fn_result = fn_result + objflow
        end if
    else
        write (unit_error, 600) &
            trim(pathoutput(ptr) .meas_type), &
            trim(groupArray(pathoutput(ptr) .source_group_ndx) .name)
600     format('Error: The following constituent' &
               ' is not simulated in this run; run stopped' &
               /'Constituent name: ', a &
               /'Constituent source group: ', a)
        call exit(1)

    end if

    return
end function

real*8 function chan_qual(qualchan, chan_distance, const_no)
    use grid_data
    use common_vars_qual
    use qual_param
    use bltm

!-----this routine reports water quality of DSM2-Qual channel

    implicit none

!-----subroutine arguments

    integer &
        qualchan &             ! channel no.
        , chan_distance       ! location within a channel

    integer &
        const_no             ! constituent number

!-----common blocks

!-----local variables

    integer &
        k                   ! loop index

    real*8 &
        xdist

!        fixme: qualchan --- what is it?
    xdist = dble(chan_distance)/dble(chan_geom(qualchan) .length) &
            *(nxsec(qualchan) - 1) + 1.
    if (chan_distance .eq. 0) then
!--------pick the first parcel concentration
        chan_qual = gpt(const_no, 1, qualchan)
        return
    else if (xdist .ge. px(ns(qualchan), qualchan)) then
!--------pick the last parcel concentration
        chan_qual = gpt(const_no, ns(qualchan), qualchan)
        return
    else
        do k = 2, ns(qualchan)
            if (xdist .le. px(k, qualchan)) then
                chan_qual = gpt(const_no, k - 1, qualchan)
                return
            end if
        end do
        write (unit_error, *) &
            'Error inside chan_qual.f; run stopped.'
        call exit(1)
    end if

    return
end

end module qual_get_output
