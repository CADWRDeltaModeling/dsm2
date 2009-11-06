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

      subroutine VerifyHDF5(error,message)
	use io_units
	use hdfvars
	implicit none

	integer error
	character*(*)message

	if (error .ne. 0) then
        write(unit_error,1212) ,message,error, hdf5point
	  call exit(2)
      end if
1212  format(/"Verify HDF failed: ",a,/"Error: ",i4,/"HDF point: ",i12)
	return
	end subroutine



      subroutine ReportOpenData()
	use io_units
	use hdfvars
	use qextvars
	use inclvars
      implicit none
      integer(HID_T)    :: loc_id      ! File or group identifier 
      character(LEN=60) :: obj_name        ! Name of the group
      integer :: nmembers            ! Number of members in the group
      integer :: hdferr 
	integer :: idx
	integer :: obj_type
	
	if (.not.(mod(hdf5point,12) .lt. 2)) goto 999
	
  	call h5gn_members_f(file_id, "hydro/data", nmembers, hdferr)
      write(unit_screen,*)"HDFPoint: ", hdf5point," Number data members open: ",nmembers

	do idx=0,(nmembers-1)
        call h5gget_obj_info_idx_f(file_id, "hydro/data", idx, 
     &                             obj_name, obj_type, hdferr)

	  write(unit_screen,"(a,i5)")trim(obj_name), obj_type

	end do

999	return
	end subroutine



      subroutine DumpHdf5Data
      use common_tide
      use runtime_data
      implicit none
      include '../qual/param.inc'
      include '../hydrolib/network.inc'
      include '../qual/bltm1.inc'
      logical :: firstTime = .true.
      integer :: i,j
      if (firstTime)then
         open(unit=1919,file = "dump.txt",status = 'unknown')
      end if
      
      write(1919,*)"Time step: ", julmin
      write(1919,*)"Channel: "
      do i = 1,nchans
          write(1919, '(i,7f24.16)')i,HChan(1,i),HChan(2,i),AChan(1,i),AChan(2,i),QChan(1,i),QChan(2,i),AChan_avg(i)
      end do
      
      
      write(1919,*)"Qext data"
      do i = 1,nqext
          write(1919, *)Qext(i).avg
      end do
      write (1919,*)"Reservoir data"
      do i = 1, nreser
         do j = 1,res_geom(i).nnodes
            write(1919,'(2i,f24.16)'),i,j, qres(i,j)
          end do
      end do
      firstTime = .false.
      return
      end subroutine
      
      
      
      

