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

module constants_ptm

    !-----particle passing between waterbodies (flux output)
    integer :: &
        ptm_from_wb
    integer :: ptm_to_wb
    integer :: ptm_interval
    integer :: ptm_filename
    integer :: ptm_modifier
    integer :: b_part
    integer :: ptm_group

    parameter ( &
        ptm_from_wb=1 &
        ,ptm_to_wb=2 &
        ,ptm_interval=3 &
        ,ptm_filename=4 &
        ,ptm_modifier=5 &
        ,b_part=6 &
        ,ptm_group=7 &
        )

    !-----magic numbers for particle injection
    integer :: &
        partno_node
    integer :: partno_nparts
    integer :: partno_slength
    integer :: partno_length
    integer :: partno_sdate
    integer :: partno_stime
    integer :: partno_edate
    integer :: partno_etime
    integer :: partno_type

    parameter ( &
        partno_node=1 &
        ,partno_nparts=2 &
        ,partno_slength=3 &
        ,partno_length=4 &
        ,partno_sdate=5 &
        ,partno_stime=6 &
        ,partno_edate=7 &
        ,partno_etime=8 &
        ,partno_type=9 &
        )


    !-----magic numbers for particle output groups
    integer :: &
        group_name
    integer :: group_memtype
    integer :: group_memid

    parameter ( &
        group_name=1 &
        ,group_memtype=2 &
        ,group_memid=3 &
        )

    !-----magic number for 'all' keyword in flux output
    integer :: alltypes
    parameter (alltypes = -100)

    !-----magic numbers for profile of particle distribution option

    integer :: &
        partdist_chan
    integer :: partdist_slength
    integer :: partdist_sdate
    integer :: partdist_stime

    parameter ( &
        partdist_chan=1 &
        ,partdist_slength=2 &
        ,partdist_sdate=3 &
        ,partdist_stime=4 &
        )

    integer :: &
        unit_binary

    parameter ( &
        unit_binary=3 &
        )
     
end module
