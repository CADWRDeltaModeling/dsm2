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

      subroutine init_store_outpaths(istat)

!-----Initialization for storing time-varying output data in temporary
!-----files.
      use IO_Units
      use iopath_data
      use runtime_data
      use constants
      use dss
      use mod_writedss
      use utilities, only: incr_intvl
      implicit none

!-----local variables

      character &
          tmp_dir*50 &          ! scratch file directory &
          ,dir_arr(10)*30 &     ! list of possible scratch directories &
          ,dsm2mod*20 &         ! dsm2modifier &
          ,tmp_file*80 &        ! scratch file name &
          ,ctmp*100            ! scratch variable

      integer &
          istat &               ! file status &
          ,ptr &                ! global pointer for pathouput &
          ,i &                  ! loop index &
          ,npaths &             ! path count &
          ,getdir              ! get directory function

!-----For each interval with output data, create a scratch file
!-----and write header info.

!-----scratch directory; if not specified in user input, try
!-----plausible locations
      dir_arr(1)=temp_dir
      dir_arr(2)='/tmp'
      dir_arr(4)='c:' // backslash // 'temp'
      dir_arr(5)='c:' // backslash // 'tmp'
      dir_arr(6)='d:' // backslash // 'temp'
      dir_arr(7)='d:' // backslash // 'tmp'
      dir_arr(8)='.'
      dir_arr(9)=miss_val_c     ! array list must end with this
      ptr=getdir(dir_arr)
      if (ptr .ne. 0) then
         tmp_dir=dir_arr(ptr)
      else
 605     format(/a,(/a))
         write(unit_error,605) 'Could not find a valid directory in this list:', &
             (dir_arr(ptr),ptr=1,8)
         goto 901
      endif

      ctmp=trim(crid) // '.bin'

      julstout_minutes15=incr_intvl(start_julmin,'15min', &
          NEAREST_BOUNDARY)
      if (need_tmpfile_min15) then
         call mkfilename(tmp_dir, 'tmp_min15-' // trim(ctmp), tmp_file)
         scratch_file_array(1)=tmp_file
         open ( &
             unit=unit_min15 &
             ,file=tmp_file &
             ,form='unformatted' &
             ,buffered='yes' &
             ,buffercount=50 &
             ,iostat=istat &
             ,err=901 &
             )
!--------count number of paths to write
         npaths=0
         do i=1,npthsout_min15
            ptr=ptout_min15(i)
            if (pathoutput(ptr)%need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_min15) npaths

         do i=1,npthsout_min15
            ptr=ptout_min15(i)
!-----------only write those paths that need tmp file output
            if (pathoutput(ptr)%need_tmp_outfile) &
                write (unit=unit_min15) ptr,pathoutput(ptr), &
                per_type_names(pathoutput(ptr)%per_type)
         enddo
      else
         scratch_file_array(1)=miss_val_c
      endif

      julstout_hours1=incr_intvl(start_julmin,'1hour', &
          NEAREST_BOUNDARY)
      if (need_tmpfile_hour1) then
         call mkfilename(tmp_dir, 'tmp_hour1-' // trim(ctmp), tmp_file)
         scratch_file_array(2)=tmp_file
         open ( &
             unit=unit_hour1 &
             ,file=tmp_file &
             ,form='unformatted' &
             ,buffered='yes' &
             ,buffercount=50 &
             ,iostat=istat &
             ,err=901 &
             )
!--------count number of paths to write
         npaths=0
         do i=1,npthsout_hour1
            ptr=ptout_hour1(i)
            if (pathoutput(ptr)%need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_hour1) npaths

         do i=1,npthsout_hour1
            ptr=ptout_hour1(i)
!-----------only write those paths that need tmp file output
            if (pathoutput(ptr)%need_tmp_outfile) &
                write (unit=unit_hour1) ptr,pathoutput(ptr), &
                per_type_names(pathoutput(ptr)%per_type)
         enddo
      else
         scratch_file_array(2)=miss_val_c
      endif

      julstout_days1=incr_intvl(start_julmin,'1day', &
          NEAREST_BOUNDARY)
      if (need_tmpfile_day1) then
         call mkfilename(tmp_dir, 'tmp_day1-' // trim(ctmp), tmp_file)
         scratch_file_array(3)=tmp_file
         open ( &
             unit=unit_day1 &
             ,file=tmp_file &
             ,form='unformatted' &
             ,buffered='yes' &
             ,buffercount=10 &
             ,iostat=istat &
             ,err=901 &
             )
!--------count number of paths to write
         npaths=0
         do i=1,npthsout_day1
            ptr=ptout_day1(i)
            if (pathoutput(ptr)%need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_day1) npaths

         do i=1,npthsout_day1
            ptr=ptout_day1(i)
!-----------only write those paths that need tmp file output
            if (pathoutput(ptr)%need_tmp_outfile) &
                write (unit=unit_day1) ptr,pathoutput(ptr), &
                per_type_names(pathoutput(ptr)%per_type)
         enddo
      else
         scratch_file_array(3)=miss_val_c
      endif

      julstout_weeks1=incr_intvl(start_julmin,'1week', &
          NEAREST_BOUNDARY)
      if (need_tmpfile_week1) then
         call mkfilename(tmp_dir, 'tmp_week1-' // trim(ctmp), tmp_file)
         scratch_file_array(4)=tmp_file
         open ( &
             unit=unit_week1 &
             ,file=tmp_file &
             ,form='unformatted' &
             ,iostat=istat &
             ,err=901 &
             )
!--------count number of paths to write
         npaths=0
         do i=1,npthsout_week1
            ptr=ptout_week1(i)
            if (pathoutput(ptr)%need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_week1) npaths

         do i=1,npthsout_week1
            ptr=ptout_week1(i)
!-----------only write those paths that need tmp file output
            if (pathoutput(ptr)%need_tmp_outfile) &
                write (unit=unit_week1) ptr,pathoutput(ptr), &
                per_type_names(pathoutput(ptr)%per_type)
         enddo
      else
         scratch_file_array(4)=miss_val_c
      endif

      julstout_months1=incr_intvl(start_julmin,'1month', &
          NEAREST_BOUNDARY)
      if (need_tmpfile_month1) then
         call mkfilename(tmp_dir, 'tmp_month1-' // trim(ctmp), tmp_file)
         scratch_file_array(5)=tmp_file
         open ( &
             unit=unit_month1 &
             ,file=tmp_file &
             ,form='unformatted' &
             ,iostat=istat &
             ,err=901 &
             )
!--------count number of paths to write
         npaths=0
         do i=1,npthsout_month1
            ptr=ptout_month1(i)
            if (pathoutput(ptr)%need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_month1) npaths

         do i=1,npthsout_month1
            ptr=ptout_month1(i)
!-----------only write those paths that need tmp file output
            if (pathoutput(ptr)%need_tmp_outfile) &
                write (unit=unit_month1) ptr,pathoutput(ptr), &
                per_type_names(pathoutput(ptr)%per_type)
         enddo
      else
         scratch_file_array(5)=miss_val_c
      endif

      julstout_years1=incr_intvl(start_julmin,'1year', &
          NEAREST_BOUNDARY)
      if (need_tmpfile_year1) then
         call mkfilename(tmp_dir, 'tmp_year1-' // trim(ctmp), tmp_file)
         scratch_file_array(6)=tmp_file
         open ( &
             unit=unit_year1 &
             ,file=tmp_file &
             ,form='unformatted' &
             ,iostat=istat &
             ,err=901 &
             )
!--------count number of paths to write
         npaths=0
         do i=1,npthsout_year1
            ptr=ptout_year1(i)
            if (pathoutput(ptr)%need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_year1) npaths

         do i=1,npthsout_year1
            ptr=ptout_year1(i)
!-----------only write those paths that need tmp file output
            if (pathoutput(ptr)%need_tmp_outfile) &
                write (unit=unit_year1) ptr,pathoutput(ptr), &
                per_type_names(pathoutput(ptr)%per_type)
         enddo
      else
         scratch_file_array(6)=miss_val_c
      endif

      return

 901  continue                  ! scratch file open error
      write(unit_error, "('Error opening binary scratch file',1x,a)")tmp_file
      call exit(2)

      return
      end

      integer function getdir(dir_arr)

!-----Find a usable directory from the given list; return the
!-----array index of the one to use.  The list must end with
!-----miss_val_c.
      use constants
      implicit none


!-----argument
      character*(*) dir_arr(*)  ! list of directory names to try

!-----local variables
      integer &
          ndx &                 ! directory array index &
          ,nlen &               ! character length &
          ,statarr(13) &        ! file status array &
          ,stat &               ! file status intrinsic function &
          ,istat &              ! file status value &
          ,lnblnk               ! intrinsic function

      ndx=1
      do while (dir_arr(ndx) .ne. miss_val_c)
         nlen=lnblnk(dir_arr(ndx))
         if (nlen .eq. 0) goto 100
         istat=stat(dir_arr(ndx),statarr)
         if (istat .eq. 0) then ! this directory name ok
            getdir=ndx
            return
         endif
 100     continue
         ndx=ndx+1
      enddo

      getdir=0
      return
      end

      subroutine mkfilename( &
          dir, &
          file, &
          dirfile &
          )

!-----Make a full filename (directory + filename) from directory name
!-----and filename.
      use constants
      implicit none


!-----arguments
      character*(*) &
          dir &                 ! directory name &
          ,file &               ! filename &
          ,dirfile             ! directory+filename

!-----local variables
      integer &
          nlen &                ! length of character string &
          ,ndx &                ! array index &
          ,lnblnk &             ! intrinsic &
          ,index               ! intrinsic

      character &
          dirchar              ! directory delimiter (/ or \)

      nlen=lnblnk(dir)
!-----try to find / or \ in directory name
      ndx=index(dir,'/')
      if (ndx .gt. 0) then      ! unix
         dirchar=dir(ndx:ndx)
      else
         ndx=index(dir,backslash)
         if (ndx .gt. 0) then   ! pc
            dirchar=dir(ndx:ndx)
         else                   ! unknown
            dirchar='/'
         endif
      endif
!-----directory name must end in either / or \ before
!-----appending filename
      if (dir(nlen:nlen) .ne. dirchar) then
         nlen=nlen+1
         dir(nlen:nlen)=dirchar
      endif

      dirfile=dir(:nlen) // file

      return
      end
