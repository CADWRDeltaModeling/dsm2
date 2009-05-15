C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>


      module iopath_data
      use type_defs
      use constants

c-----path input (time-varying data)      
      integer :: ninpaths = 0
      integer, parameter :: max_inputpaths=4200
      type(pathinput_t) pathinput(0:max_inputpaths)
      logical 
     &     check_input_data     ! true to only check time-varying input data
     &     ,cont_missing        ! true to continue on missing data (use previous value)
     &     ,cont_unchecked      ! true to continue on unchecked data
     &     ,cont_question       ! true to continue on questionable data
     &     ,cont_bad            ! true to continue on bad data
     &     ,warn_missing        ! true to warn about fing data
     &     ,warn_unchecked      ! true to warn about unchecked data
     &     ,warn_question       ! true to warn about questionable data
     &     ,warn_bad            ! true to warn about bad data
     
      logical :: dss_direct = .false.
      logical :: binary_output = .false.
      logical :: need_tmp_outfiles = .false.            


      ! max number of unique dss input files
      integer,parameter :: max_dssinfiles = 20
      character*130,dimension(max_dssinfiles) :: infilenames = ' ' ! unique dss input file names
      integer ifltab_in(600,max_dssinfiles)           ! DSS table for each input file


      integer,parameter :: max_outputpaths=1000  ! maximum number of output pathnames
      integer :: noutpaths = 0                  ! actual number of output pathnames


      character :: temp_dir*20 = ' '          ! directory for temporary files
                                              !todo: short

      type(pathoutput_t) pathoutput(max_outputpaths)
      integer max_dssoutfiles   ! max number of unique dss output files
      parameter (max_dssoutfiles=10)
      character*130,dimension(max_dssoutfiles) :: outfilenames = ' '
      integer ifltab_out(600,max_dssoutfiles) ! DSS table for each output file


c-----input/output file names

      integer
     &     max_iogroups         ! number of io groups (hydro, qual, ptm)
     &     ,max_file_types      ! number of types of files (restart, tide, animation,...)

      parameter (
     &     max_iogroups=3         ! one per model
     &     ,max_file_types=7      ! named in constants with names like io_restart
     &     )

      character*130
     &     output_filename      ! output filename

      type(io_file_t) io_files(max_iogroups,max_file_types,2)

      character*20 obj_names(obj_null) ! names for object codes; set in read_fixed.f
      character*8 per_type_names(per_type_null) ! data type names (e.g. 'PER-AVER')





      end module