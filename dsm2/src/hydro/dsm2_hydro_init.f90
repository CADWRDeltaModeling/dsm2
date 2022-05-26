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

subroutine dsm2_hydro_init
    use Groups, only: InitGroupAll
    use rate_coeff_assignment, only: initialize_rate_coefficient
    use IO_Units, only: unit_output
    use ifport !, only: getpid,getenvqq, rand              !! <INTEL>
    use constants, only: miss_val_i, miss_val_c, &
                         io_hdf5, io_write, io_restart, io_echo, &
                         hydro, &
                         obj_node, obj_channel, obj_flux, obj_stage, obj_gate, &
                         obj_null, obj_reservoir, obj_qext, obj_obj2obj, &
                         per_type_per_aver, per_type_per_cum, &
                         per_type_per_min, per_type_per_max, &
                         per_type_inst_val, per_type_inst_cum, per_type_null
    use logging, only: print_level
    use runtime_data, only: irid, crid, crdt10, crdt14, ntitles
    use iopath_data, only: io_files, max_file_types, max_iogroups, &
                           output_filename, obj_names, per_type_names
    use common_tide, only: tide_files, max_tide_files, current_tidefile
    use mod_writedss
    !@# For dynamic allocation
    use common_xsect, only: allocate_virt_xsect_hq

    implicit none
!-----initialize variables for DSM2

!-----local variables

    character ctemp1*20, ctemp2*20, ctmpl*250 ! temporary
    integer istat, &            ! status variable (returned)
        i, iu, k, j, &
        itmp1, itmp2, &         ! temp variables
        ihr, imin, isec, ihundredth

!-----filled in check_fixed
    print_level = miss_val_i

    per_type_names(per_type_per_aver) = 'PER-AVER'
    per_type_names(per_type_per_cum) = 'PER-CUM'
    per_type_names(per_type_per_min) = 'PER-MIN'
    per_type_names(per_type_per_max) = 'PER-MAX'
    per_type_names(per_type_inst_val) = 'INST-VAL'
    per_type_names(per_type_inst_cum) = 'INST-CUM'
    per_type_names(per_type_null) = ' '

    obj_names(obj_channel) = 'channel'
    obj_names(obj_node) = 'node'
    obj_names(obj_reservoir) = 'reservoir'
    obj_names(obj_gate) = 'gate'
    obj_names(obj_qext) = 'qext'
    obj_names(obj_obj2obj) = 'o2o'
    obj_names(obj_flux) = 'flux'
    obj_names(obj_stage) = 'stage'
    obj_names(obj_null) = miss_val_c

    need_tmpfile_min15 = .false.
    need_tmpfile_hour1 = .false.
    need_tmpfile_day1 = .false.
    need_tmpfile_week1 = .false.
    need_tmpfile_month1 = .false.
    need_tmpfile_year1 = .false.
    output_filename = ' '
    iu = unit_output + 1
    do i = 1, max_iogroups
        do j = 1, max_file_types
            do k = 1, 2
                io_files(i, j, k) .use = .false.
                io_files(i, j, k) .filename = ' '
                io_files(i, j, k) .interval = ' '
                io_files(i, j, k) .unit = iu ! fill in file unit numbers
                iu = iu + 1
            end do
        end do
    end do

!-----groups. initialize the "wildcard" group
    call InitGroupAll()

!------non-converative constitute rate coefficients initialization Jon 4/12/06
    call initialize_rate_coefficient

!-----default checkpoint intervals
    io_files(hydro, io_restart, io_write) .interval = '1HOUR'
!-----default binary output intervals
    io_files(hydro, io_echo, io_write) .interval = miss_val_c
!-----default HDF5 output intervals
    io_files(hydro, io_hdf5, io_write) .interval = '15MIN'

    current_tidefile = miss_val_i
    do i = 1, max_tide_files
        tide_files(i) .start_date = ' '
        tide_files(i) .end_date = ' '
        tide_files(i) .filename = ' '
    end do

    ntitles = 0

!-----irregular xsects preparation
    !@# Dynamic allocation
    call allocate_virt_xsect_hq
    call prep_irreg

!-----set runtime ID; can be either the process ID
!-----(multi-tasking OS) or random number (other OS)
    crid = ' '
    irid = abs(getpid())        ! Sun Unix and NT
!!OTHER  irid=int(rand(0)*1000000)  ! others
    write (crid, *) irid
!-----prepend custom ID to run ID?
    ctemp1 = ''
    istat = getenvqq('CID', ctemp1) !! <NT>
    if (istat .gt. 0) then    ! custom ID
        read (ctemp1, *) itmp1
        crid = trim(ctemp1)//'_'//trim(crid)
    end if
!-----date of run
    ctemp1 = ''
    call cdate(ctemp1)
    call datjul(ctemp1, itmp1, istat)
    crdt14 = ' '
    call juldat(itmp1, 104, crdt14(1:9), itmp2) ! DDMMMYYYY
    call juldat(itmp1, -11, ctemp1, itmp2)
    ctemp1 = ctemp1(:itmp2)
    crdt10 = ' '
    crdt10(1:2) = ctemp1(7:8)   ! YYMMDD (easy to sort on)
    crdt10(3:4) = ctemp1(1:2)
    crdt10(5:6) = ctemp1(4:5)
!-----time of run
    ctemp1 = ' '
!      call ctime(ctemp1)
    call gettim(ihr, imin, isec, ihundredth)
    write (ctemp1, '(2I2)') ihr, imin
    crdt14(11:12) = ctemp1(1:2) ! hhmm
    crdt14(13:14) = ctemp1(3:4)
    crdt10(7:8) = ctemp1(1:2)   ! hhmm
    crdt10(9:10) = ctemp1(3:4)

    return
end
