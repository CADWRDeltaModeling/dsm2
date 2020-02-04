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


module iopath_data
    use type_defs
    use constants
    implicit none

    !-----path input (time-varying data)
    integer:: ninpaths = 0
    integer, parameter :: max_inputpaths=4200
    type(pathinput_t):: pathinput(0:max_inputpaths)
    logical:: check_input_data             ! true to warn about bad data
    logical:: cont_missing             ! true to warn about bad data
    logical:: cont_unchecked             ! true to warn about bad data
    logical:: cont_question             ! true to warn about bad data
    logical:: cont_bad             ! true to warn about bad data
    logical:: warn_missing             ! true to warn about bad data
    logical:: warn_unchecked             ! true to warn about bad data
    logical:: warn_question             ! true to warn about bad data
    logical:: warn_bad             ! true to warn about bad data
     
    logical:: dss_direct = .false.
    logical:: binary_output = .false.
    logical:: need_tmp_outfiles = .false.


    ! max number of unique dss input files
    integer,parameter :: max_dssinfiles = 100
    character(len=130), dimension(max_dssinfiles)::infilenames= ' ' ! unique dss input file names
    integer:: ifltab_in(600,max_dssinfiles)           ! DSS table for each input file


    integer,parameter :: max_outputpaths=1000  ! maximum number of output pathnames
    integer:: noutpaths = 0                  ! actual number of output pathnames


    character(len=20)::temp_dir= ' '          ! directory for temporary files
                                            !todo: short

    type(pathoutput_t):: pathoutput(max_outputpaths)
    integer:: max_dssoutfiles   ! max number of unique dss output files
    parameter (max_dssoutfiles=15)
    character(len=130), dimension(max_dssoutfiles)::outfilenames= ' '
    integer:: ifltab_out(600,max_dssoutfiles) ! DSS table for each output file


    !-----input/output file names

    integer:: &
        max_iogroups       ! number of types of files (restart, tide, animation,...)
    integer:: max_file_types       ! number of types of files (restart, tide, animation,...)

    parameter ( &
        max_iogroups=3 &          ! one per model
        ,max_file_types=7 &       ! named in constants with names like io_restart
        )

    character(len=130)::output_filename      ! output filename

    type(io_file_t):: io_files(max_iogroups,max_file_types,2)

    character(len=20)::obj_names(obj_null) ! names for object codes; set in read_fixed.f
    character(len=8)::per_type_names(per_type_null) ! data type names (e.g. 'PER-AVER')





end module
