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

      subroutine restart_file(action)

!-----read or write a restart file for qual
      use common_qual
      use runtime_data
      use iopath_data
      Use IO_Units
      use utilities
      implicit none

!-----include files

      include 'param.inc'
      include 'bltm1.inc'

!-----arguments and local variables

      integer &
          action &               ! whether to read or write the restart file
          ,i,k,l,n &             ! loop indices
          ,unit_restart &        ! output unit
          ,n_all &               ! number of incoming chemical constituents
          ,ext2int

      external ext2int

      real &
          salavg(max_constituent) &
          ,vol

      character &
          header*150 &           ! header line
          ,cchem*20(max_constituent) ! incoming chemical constituent names

      if (action .eq. io_write) then
!--------write restart file
         unit_restart=io_files(qual,io_restart,io_write).unit
         open(unit=unit_restart,file=io_files(qual,io_restart &
             ,io_write).filename,status='unknown',err=901)

         write(unit_restart,900) dsm2_version, current_date
 900     format('Qual Version ',a &
             /'The following data corresponds to   ',a14//)
         write(unit_restart,970) &
             (trim(constituents(all_source_ptr(l)).name), &
             l=1,no_all_source)
 970     format('Initial Channel Concentration'/'Channel',11(1x,a11))

!--------figure out the average concentration in each channel
!--------only do calcs for constituents from all sources
         do n=1,nbrch
            do l=1,no_all_source
               salavg(all_source_ptr(l))=0
            enddo
            vol=0
            do k=1,ns(n)
               vol=vol+gpv(n,k)
               do l=1,no_all_source
                  salavg(all_source_ptr(l))=salavg(all_source_ptr(l)) + &
                      gpv(n,k)*gpt(all_source_ptr(l),k,n)
               enddo
            enddo
            do l=1,no_all_source
               salavg(all_source_ptr(l))=salavg(all_source_ptr(l))/vol
            enddo
            write(unit_restart,971) int2ext(n), &
                (salavg(all_source_ptr(l)),l=1,no_all_source)
 971        format(i7,1p,11(1x,g11.3))
         enddo
         write(unit_restart,975) (trim(constituents(all_source_ptr(l)).name), &
             l=1,no_all_source)
 975     format('Initial Reservoir Salinity'/ &
             'Reservoir',11(1x,a11))

         do i=1,nreser
            write(unit_restart,976) i, &
                (cres(i,all_source_ptr(l)),l=1,no_all_source)
 976        format(i9,1p,11(1x,g11.3))
         enddo
         close(unit_restart)
      else
!--------read restart file
         unit_restart=io_files(qual,io_restart,io_read).unit
         open(file=io_files(qual,io_restart,io_read).filename, &
             unit=unit_restart,status='old',err=901)
!--------read header, then test to see if it's really the header (old restart file)
!--------or the restart file version (new file)
         restart_version=' '
         read(unit_restart,'(a)') header
         if (header(:12) .eq. 'Qual Version') then
            restart_version=header(13:)
            read(unit_restart,'(a)') header
         endif
!--------get chemical names
         read(unit_restart,910) header
 910     format(///a)
         i=1
         cchem(1)=get_substring(header,' ') ! first field is 'Channel', discard
         do while (header .ne. ' ' .and. i .le. max_constituent)
            cchem(i)=get_substring(header,' ')
            call locase(cchem(i))
            i=i+1
         enddo
         n_all=i-1
!--------for each different chemical read in, copy to possibly multiple
!--------constituent locations
         do n=1,nbrch
            read(unit_restart,*) k, (salavg(i), i=1,n_all)
            do l=1,neq
               cstart(ext2int(k),l)=0.0
               do i=1,n_all
                  if (constituents(l).name .eq. cchem(i)) then
                     cstart(ext2int(k),l)=salavg(i)
                  endif
               enddo
            enddo
         enddo
!--------skip reservoir headers, assume chemical names are in same order
!--------as for channels
         read(unit_restart,911) header
 911     format(/a)
         do n=1,nreser
            read(unit_restart,*) k, (salavg(i), i=1,n_all)
            do l=1,neq
               cres(k,l)=0.0
               do i=1,n_all
                  if (constituents(l).name .eq. cchem(i)) then
                     cres(k,l)=salavg(i)
                  endif
               enddo
            enddo
         enddo
         close(unit_restart)
      endif

      return

 901  continue                  ! open error on restart file
      write(unit_error,605) io_files(qual,io_restart,action).filename
 605  format(/'Error opening restart file:',a)
      call exit(2)
      return
      end subroutine
