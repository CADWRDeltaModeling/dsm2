C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/


      subroutine wrt_outpaths

c-----Write from scratch files to DSS or text files.

      implicit none

      include '../fixed/common.f'
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
     &     ,ios                 ! status
     &     ,nvals               ! number of values written in each block
     &     ,nv                  ! number of values in data block
     &     ,ndays,nhours,nmins  ! number of days,hours for generic date
     &     ,lnblnk              ! last-non-blank intrinsic
     &     ,itemp

      integer*4
     &     cdt2jmin             ! convert character date/time to julian minute
     &     ,dmins               ! delta minutes
     &     ,incr_intvl          ! increment julian minute by interval function
     &     ,jdt                 ! julian minutes

      REAL*8
     &     outdata_arr(maxinpsize)  ! output data array: dimension to the largest 
                                ! time block (mins15, dys, wks ...) in dss.inc 

      character
     &     date_time*14         ! char date/time of start of block
     &     ,dt*25               ! char date/time of each value
     &     ,jmin2cdt*14         ! convert julian minute to character date/time
     &     ,e_part*10           ! DSS interval
     &     ,ctemp*80            ! scratch

 610  format('Writing data for ',a/a)

      do file=1,6

         if (scratch_file_array(file)(1:1) .ne. miss_val_c) then

            inquire(
     &           file=scratch_file_array(file)
     &           ,opened=lopen
     &           ,number=unit_scratch
     &           )

            if (.not. lopen) then
               unit_scratch=unit_year1
               open (
     &              unit=unit_scratch
     &              ,file=scratch_file_array(file)
     &              ,form='unformatted'
     &              ,err=902
     &              )
            endif

c-----------for each data interval, process the data for one path, then
c-----------rewind and process data for the next path, etc.

            p=1
            npaths=1
            do while (p .le. npaths)
               rewind(unit_scratch)
               read(unit_scratch) npaths
               if (p .gt. npaths) goto 802
               do i=1,npaths
                  read(unit_scratch) pathoutput(i)
               enddo

               write(unit_screen,610)
     &              pathoutput(p).path(:lnblnk(pathoutput(p).path)),
     &              pathoutput(p).filename(:lnblnk(pathoutput(p).filename))
               generic=.false.
               e_part=pathoutput(p).interval
               if (index(pathoutput(p).filename,'.dss') .eq. 0) then
                  text_file=.true.
                  open (        ! text output file
     &                 unit=unit_text
     &                 ,file=pathoutput(p).filename
     &                 ,form='formatted'
     &                 ,access='append'
     &                 ,status='unknown'
     &                 ,iostat=ios
     &                 ,err=901
     &                 )
               else
                  text_file=.false.
               endif
               if (text_file) then
                  write(unit_text,'(a)')
     &                 pathoutput(p).path(:lnblnk(pathoutput(p).path))
                  ctemp=pathoutput(p).units
                  call upcase(ctemp)
                  write(unit_text,'(a)') ctemp(1:lnblnk(ctemp))
                  write(unit_text,'(a)')
     &                 per_type_names(pathoutput(p).per_type)
     &                 (:lnblnk(per_type_names(pathoutput(p).per_type)))
               endif
               ios=0
               do while (ios .eq. 0)
                  read(unit_scratch, iostat=ios, end=810) nvals
                  read(unit_scratch, iostat=ios, end=810) date_time

                  if (date_time .eq. generic_dt) generic=.true.
                  nv=nvals
                  do j=1,npaths
                     read(unit_scratch) (outdata_arr(k), k=1,nvals)
                     if (p .eq. j) then
                        if (text_file) then
                           dt=date_time
                           jdt=cdt2jmin(date_time)
                           do k=1,nvals
                              dmins=incr_intvl(jdt,e_part,TO_BOUNDARY)-jdt
                              if (generic) then ! instead of date/time, print interval (5DAY 3HOUR)
                                 ndays=dmins/(60*24)
                                 nhours=(dmins-ndays*60*24)/60
                                 nmins=dmins-ndays*60*24-nhours*60
                                 write(dt,'(i4,''DAY '',i2,''HOUR '',i2.2,''MIN'')')
     &                                ndays,nhours,nmins
                              else ! standard date
                                 dt=jmin2cdt(jdt)
                              endif
                              write(unit_text,'(1p,a,1x,g10.3)')
     &                             dt(:lnblnk(dt)), outdata_arr(k)
                              jdt=jdt+dmins
                           enddo
                        else
                           call writedss(p,date_time,outdata_arr(1), nv)
                        endif
                     endif
                  enddo
 810              continue      ! here for eof on scratch output file
               enddo
               if (text_file) close(unit_text)
 802           continue
               p=p+1
            enddo

            close(unit=unit_scratch, status='delete')

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

 620  format(/'Error opening ',a,' file ',a)

 901  continue                  ! error on opening text outputfile
      write(unit_error,620) 'text output',
     &     pathoutput(p).filename(:lnblnk(pathoutput(p).filename))
      call exit(2)

 902  continue                  ! error on opening scratch file
      write(unit_error,620) 'scratch',
     &     scratch_file_array(file)(:lnblnk(scratch_file_array(file)))
      call exit(2)

      end
