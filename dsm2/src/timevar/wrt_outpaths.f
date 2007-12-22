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

      subroutine wrt_outpaths

c-----Write from scratch files to DSS or text files.
      use IO_Units
      use constants
      use iopath_data
      implicit none

      include 'dss.inc'
      include 'writedss.inc'

c-----local variables

      logical
     &     text_file            ! true if output for path is to text (not DSS) file
     &     ,generic             ! true if date is generic
     &     ,lopen               ! true if scratch file is already open

      integer
     &     npaths               ! number of output paths for this interval
     &     ,file                ! scratch file number
     &     ,unit_scratch        ! temp file unit number
     &     ,i,j,k,p             ! loop index
     &     ,ptr                 ! global pointer to pathoutput
     &     ,ios                 ! status
     &     ,nvals               ! number of values written in each block
     &     ,nv                  ! number of values in data block
     &     ,ndays,nhours,nmins  ! number of days,hours for generic date
     &     ,path_ptr(max(max_out_min,max_out_hour,max_out_day,max_out_month)) ! output path pointers
     &     ,itemp

      integer*4
     &     cdt2jmin             ! convert character date/time to julian minute
     &     ,dmins               ! delta minutes
     &     ,incr_intvl          ! increment julian minute by interval function
     &     ,jdt                 ! julian minutes

      REAL*8
     &     outdata_arr(maxinpsize) ! output data array: dimension to the largest
                                ! time block (mins15, dys, wks ...) in dss.inc

      character
     &     date_time*14         ! char date/time of start of block
     &     ,dt*25               ! char date/time of each value
     &     ,jmin2cdt*14         ! convert julian minute to character date/time
     &     ,e_part*10           ! DSS interval
     &     ,ctype*8             ! DSS types, e.g. 'PER-AVER'
     &     ,ctemp*80            ! scratch

 610  format('Writing data for ',a/a)

      do file=1,6

         if (scratch_file_array(file)(1:1) .ne. miss_val_c) then

            inquire(
     &           file=scratch_file_array(file)
     &           ,opened=lopen
     &           ,number=unit_scratch
     &           )

            if (lopen) close(unit_scratch)
            unit_scratch=unit_year1
            open (
     &           unit=unit_scratch
     &           ,file=scratch_file_array(file)
     &           ,form='unformatted'
     &           ,action='READ'
     &           ,buffercount=50
     &           ,err=902
     &           )

c-----------for each data interval, process the data for one path, then
c-----------rewind and process data for the next path, etc.

            p=1
            npaths=1
            do while (p .le. npaths)
               rewind(unit_scratch)
               read(unit_scratch) npaths
               if (p .gt. npaths) goto 802
               do i=1,npaths
                  read(unit_scratch) ptr,pathoutput(ptr),ctype
                  path_ptr(i)=ptr
               enddo

               ptr=path_ptr(p)
			 write(unit_screen,610)
     &              trim(pathoutput(ptr).path),trim(pathoutput(ptr).filename)
               generic=.false.
               e_part=pathoutput(ptr).interval
               if (index(pathoutput(ptr).filename,'.dss') .eq. 0) then
                  text_file=.true.
                  open (        ! text output file
     &                 unit=unit_text
     &                 ,file=pathoutput(ptr).filename
     &                 ,form='formatted'
     &                 ,access='append'
     &                 ,status='unknown'
     &                 ,buffered='yes'
     &                 ,iostat=ios
     &                 ,err=901
     &                 )
               else
                  text_file=.false.
               endif
               if (text_file) then
                  write(unit_text,'(a)')
     &                 trim(pathoutput(ptr).path)
                  ctemp=pathoutput(ptr).units
                  call upcase(ctemp)
                  write(unit_text,'(a)') trim(ctemp)
                  write(unit_text,'(a)') trim(ctype)
               endif
               ios=0
               do while (ios .eq. 0)
                  read(unit_scratch, iostat=ios, end=810) nvals
                  read(unit_scratch, iostat=ios, end=810) date_time

                  if (date_time .eq. generic_date) generic=.true.
                  nv=nvals
                  do j=1,npaths
                     read(unit_scratch) ptr,(outdata_arr(k), k=1,nvals)
                     if (p .eq. j) then
                        if (text_file) then
                           dt=date_time
                           jdt=cdt2jmin(date_time)
                           do k=1,nvals
                              dmins=incr_intvl(jdt,e_part,TO_BOUNDARY)-jdt
                              dt=jmin2cdt(jdt)
                              write(unit_text,'(1p,a,1x,g10.3)')
     &                             trim(dt), outdata_arr(k)
                              jdt=jdt+dmins
                           enddo
                        else
                           call writedss(ptr,date_time,outdata_arr(1), nv)
                        endif
                     endif
                  enddo
 810              continue      ! here for eof on scratch output file
               enddo
               if (text_file) close(unit_text)
 802           continue
               p=p+1
            enddo

            close(unit=unit_scratch, status='delete',err=903)

         endif

      enddo

c-----close all DSS output files
      i=1
      do while(i .le. max_dssoutfiles .and.
     &     outfilenames(i) .ne. ' ')
         call zinqir(ifltab_out(1,i), 'UNIT', ctemp, itemp)
         if (itemp .ne. 0) call zclose (ifltab_out(1,i))
         i=i+1
      enddo

      return

 620  format(/'Error ',a,' ',a,' file ',a)

 901  continue                  ! error on opening text outputfile
      write(unit_error,620) 'opening', 'text output',
     &     trim(pathoutput(ptr).filename)
      call exit(2)

 902  continue                  ! error on opening scratch file
      write(unit_error,620) 'opening', 'scratch',
     &     trim(scratch_file_array(file))
      call exit(2)

 903  continue
      write(unit_error,620) 'closing', 'scratch',
     &     trim(scratch_file_array(file))
      call exit(2)

      end
