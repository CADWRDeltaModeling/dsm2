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
