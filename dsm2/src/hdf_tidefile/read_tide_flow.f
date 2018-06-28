C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

      subroutine read_tide_flow(desired_time,new_tidefile,
     &     first_used_tidefile,tidefile,
     &     tide_block_no,process_data)

c-----Read the time-varying flow values from a DSM2-Hydro
c-----binary or HDF5 tidefile.  This routine assumes that the correct
c-----tidefile is being used for the desired time specified, however,
c-----it will search within the tidefile for the correct tide block.

      use IO_Units
      use hdfvars
      use common_tide
      use grid_data
      implicit none


c-----arguments

      integer*4 desired_time     ! desired time of tide block, in julian minutes (INPUT)
      logical new_tidefile      ! true if new tidefile (INPUT)
     &     ,process_data        ! check and process tide data
      integer  tidefile         ! current tidefile number that is being used (INPUT)
     &     ,first_used_tidefile ! number of first tidefile that was used
     &     ,tide_block_no       ! tide block number used (INPUT & RETURNED)

c-----local variables

      integer, external :: GetCurrentTideTime
      integer, external :: SetHDF5ToTime

         
	if (GetCurrentTideTime() .lt. desired_time) then
	  ! flow values must be updated
	  TideTime=SetHDF5ToTime(desired_time)
	   call ReadDataFromHDF5(TideTime)

         if (process_data) then
            call process_tide(new_tidefile,
     &           first_used_tidefile,current_tidefile,tide_block_no)
         endif
      endif
      return


      end




