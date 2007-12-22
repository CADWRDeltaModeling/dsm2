C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!
C!    For more information about DSM2, contact:
C!
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

      subroutine init_store_outpaths(istat)

c-----Initialization for storing time-varying output data in temporary
c-----files.
      use IO_Units
      use iopath_data
      use runtime_data
      use constants
      implicit none

      include 'dss.inc'
      include 'writedss.inc'

c-----local variables

      character
     &     tmp_dir*50           ! scratch file directory
     &     ,dir_arr(10)*30      ! list of possible scratch directories
     &     ,dsm2mod*20          ! dsm2modifier
     &     ,tmp_file*80         ! scratch file name
     &     ,ctmp*100            ! scratch variable

      integer
     &     istat                ! file status
     &     ,ptr                 ! global pointer for pathouput
     &     ,i                   ! loop index
     &     ,npaths              ! path count
     &     ,getdir              ! get directory function

      integer*4
     &     incr_intvl           ! increment julmin minute function

c-----For each interval with output data, create a scratch file
c-----and write header info.

c-----scratch directory; if not specified in user input, try
c-----plausible locations
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
         write(unit_error,605) 'Could not find a valid directory in this list:',
     &        (dir_arr(ptr),ptr=1,8)
         goto 901
      endif

      ctmp=trim(crid) // '.bin'

      julstout_minutes15=incr_intvl(start_julmin,'15min',
     &     NEAREST_BOUNDARY)
      if (need_tmpfile_min15) then
         call mkfilename(tmp_dir, 'tmp_min15-' // trim(ctmp), tmp_file)
         scratch_file_array(1)=tmp_file
         open (
     &        unit=unit_min15
     &        ,file=tmp_file
     &        ,form='unformatted'
     &        ,buffered='yes'
     &        ,buffercount=50
     &        ,iostat=istat
     &        ,err=901
     &        )
c--------count number of paths to write
         npaths=0
         do i=1,npthsout_min15
            ptr=ptout_min15(i)
            if (pathoutput(ptr).need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_min15) npaths

         do i=1,npthsout_min15
            ptr=ptout_min15(i)
c-----------only write those paths that need tmp file output
            if (pathoutput(ptr).need_tmp_outfile)
     &           write (unit=unit_min15) ptr,pathoutput(ptr),
     &           per_type_names(pathoutput(ptr).per_type)
         enddo
      else
         scratch_file_array(1)=miss_val_c
      endif

      julstout_hours1=incr_intvl(start_julmin,'1hour',
     &     NEAREST_BOUNDARY)
      if (need_tmpfile_hour1) then
         call mkfilename(tmp_dir, 'tmp_hour1-' // trim(ctmp), tmp_file)
         scratch_file_array(2)=tmp_file
         open (
     &        unit=unit_hour1
     &        ,file=tmp_file
     &        ,form='unformatted'
     &        ,buffered='yes'
     &        ,buffercount=50
     &        ,iostat=istat
     &        ,err=901
     &        )
c--------count number of paths to write
         npaths=0
         do i=1,npthsout_hour1
            ptr=ptout_hour1(i)
            if (pathoutput(ptr).need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_hour1) npaths

         do i=1,npthsout_hour1
            ptr=ptout_hour1(i)
c-----------only write those paths that need tmp file output
            if (pathoutput(ptr).need_tmp_outfile)
     &           write (unit=unit_hour1) ptr,pathoutput(ptr),
     &           per_type_names(pathoutput(ptr).per_type)
         enddo
      else
         scratch_file_array(2)=miss_val_c
      endif

      julstout_days1=incr_intvl(start_julmin,'1day',
     &     NEAREST_BOUNDARY)
      if (need_tmpfile_day1) then
         call mkfilename(tmp_dir, 'tmp_day1-' // trim(ctmp), tmp_file)
         scratch_file_array(3)=tmp_file
         open (
     &        unit=unit_day1
     &        ,file=tmp_file
     &        ,form='unformatted'
     &        ,buffered='yes'
     &        ,buffercount=10
     &        ,iostat=istat
     &        ,err=901
     &        )
c--------count number of paths to write
         npaths=0
         do i=1,npthsout_day1
            ptr=ptout_day1(i)
            if (pathoutput(ptr).need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_day1) npaths

         do i=1,npthsout_day1
            ptr=ptout_day1(i)
c-----------only write those paths that need tmp file output
            if (pathoutput(ptr).need_tmp_outfile)
     &           write (unit=unit_day1) ptr,pathoutput(ptr),
     &           per_type_names(pathoutput(ptr).per_type)
         enddo
      else
         scratch_file_array(3)=miss_val_c
      endif

      julstout_weeks1=incr_intvl(start_julmin,'1week',
     &     NEAREST_BOUNDARY)
      if (need_tmpfile_week1) then
         call mkfilename(tmp_dir, 'tmp_week1-' // trim(ctmp), tmp_file)
         scratch_file_array(4)=tmp_file
         open (
     &        unit=unit_week1
     &        ,file=tmp_file
     &        ,form='unformatted'
     &        ,iostat=istat
     &        ,err=901
     &        )
c--------count number of paths to write
         npaths=0
         do i=1,npthsout_week1
            ptr=ptout_week1(i)
            if (pathoutput(ptr).need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_week1) npaths

         do i=1,npthsout_week1
            ptr=ptout_week1(i)
c-----------only write those paths that need tmp file output
            if (pathoutput(ptr).need_tmp_outfile)
     &           write (unit=unit_week1) ptr,pathoutput(ptr),
     &           per_type_names(pathoutput(ptr).per_type)
         enddo
      else
         scratch_file_array(4)=miss_val_c
      endif

      julstout_months1=incr_intvl(start_julmin,'1month',
     &     NEAREST_BOUNDARY)
      if (need_tmpfile_month1) then
         call mkfilename(tmp_dir, 'tmp_month1-' // trim(ctmp), tmp_file)
         scratch_file_array(5)=tmp_file
         open (
     &        unit=unit_month1
     &        ,file=tmp_file
     &        ,form='unformatted'
     &        ,iostat=istat
     &        ,err=901
     &        )
c--------count number of paths to write
         npaths=0
         do i=1,npthsout_month1
            ptr=ptout_month1(i)
            if (pathoutput(ptr).need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_month1) npaths

         do i=1,npthsout_month1
            ptr=ptout_month1(i)
c-----------only write those paths that need tmp file output
            if (pathoutput(ptr).need_tmp_outfile)
     &           write (unit=unit_month1) ptr,pathoutput(ptr),
     &           per_type_names(pathoutput(ptr).per_type)
         enddo
      else
         scratch_file_array(5)=miss_val_c
      endif

      julstout_years1=incr_intvl(start_julmin,'1year',
     &     NEAREST_BOUNDARY)
      if (need_tmpfile_year1) then
         call mkfilename(tmp_dir, 'tmp_year1-' // trim(ctmp), tmp_file)
         scratch_file_array(6)=tmp_file
         open (
     &        unit=unit_year1
     &        ,file=tmp_file
     &        ,form='unformatted'
     &        ,iostat=istat
     &        ,err=901
     &        )
c--------count number of paths to write
         npaths=0
         do i=1,npthsout_year1
            ptr=ptout_year1(i)
            if (pathoutput(ptr).need_tmp_outfile) npaths=npaths+1
         enddo
         write (unit=unit_year1) npaths

         do i=1,npthsout_year1
            ptr=ptout_year1(i)
c-----------only write those paths that need tmp file output
            if (pathoutput(ptr).need_tmp_outfile)
     &           write (unit=unit_year1) ptr,pathoutput(ptr),
     &           per_type_names(pathoutput(ptr).per_type)
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

c-----Find a usable directory from the given list; return the
c-----array index of the one to use.  The list must end with
c-----miss_val_c.
      use constants
      implicit none


c-----argument
      character*(*) dir_arr(*)  ! list of directory names to try

c-----local variables
      integer
     &     ndx                  ! directory array index
     &     ,nlen                ! character length
     &     ,statarr(13)         ! file status array
     &     ,stat                ! file status intrinsic function
     &     ,istat               ! file status value
     &     ,lnblnk              ! intrinsic function

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

      subroutine mkfilename(
     &     dir,
     &     file,
     &     dirfile
     &     )

c-----Make a full filename (directory + filename) from directory name
c-----and filename.
      use constants
      implicit none


c-----arguments
      character*(*)
     &     dir                  ! directory name
     &     ,file                ! filename
     &     ,dirfile             ! directory+filename

c-----local variables
      integer
     &     nlen                 ! length of character string
     &     ,ndx                 ! array index
     &     ,lnblnk              ! intrinsic
     &     ,index               ! intrinsic

      character
     &     dirchar              ! directory delimiter (/ or \)

      nlen=lnblnk(dir)
c-----try to find / or \ in directory name
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
c-----directory name must end in either / or \ before
c-----appending filename
      if (dir(nlen:nlen) .ne. dirchar) then
         nlen=nlen+1
         dir(nlen:nlen)=dirchar
      endif

      dirfile=dir(:nlen) // file

      return
      end
