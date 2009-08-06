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

      subroutine writedss(pathnumber, cdt, in_values, nvals)

c-----Write out a block of data to DSS
      use io_units
      use runtime_data
      use iopath_data
      implicit none

      include 'dss.inc'
      include 'writedss.inc'

      logical,dimension(max_dssoutfiles) :: isopen = .false.
      character
     &     cdt*(*)              ! date/time for start of data
     &     ,cdatx*9             ! date
     &     ,ctimx*4             ! time
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,cunits*8            ! units (e.g. FEET, CFS)

      integer
     &     nvals ,istat, iostat
     &     ,pathnumber          ! global pathnumber
     &     ,npath,na,nb,nc,nd,ne,nf

      integer i
      real*8 in_values(nvals)
      real*4 values(nvals)


      if (nvals .le. 0) return
      do i =1,nvals
         values(i) = sngl(in_values(i))
      end do

      cdatx=cdt(1:9)
      ctimx=cdt(11:14)

      if (.not. isopen(pathoutput(pathnumber).ndx_file)) then
c--------Open the DSS file for writing
c--------exclusive write lock, works faster over NFS
c@@@         call zset('WLOCK','ON',0)
c--------set module name as data creator
         call zset('PROGRAM',dsm2_name,0)
c--------preset for a very large .dss file
         call zset('SIZE', ' ', 1000000)

         call zopen (ifltab_out(1,pathoutput(pathnumber).ndx_file),
     &        pathoutput(pathnumber).filename, iostat)
         if (iostat .gt. 0) then
            write(unit_error,'(a,a)') 'Unable to open the file ',
     &           pathoutput(pathnumber).filename
            call exit(2)
         endif
         isopen(pathoutput(pathnumber).ndx_file)=.true.
      endif

c-----write the time block
      cunits=pathoutput(pathnumber).units
      call upcase(cunits)
      call zsrts(ifltab_out(1,pathoutput(pathnumber).ndx_file),
     &     pathoutput(pathnumber).path,
     &     cdatx, ctimx, nvals, values, cunits,
     &     per_type_names(pathoutput(pathnumber).per_type), 0, istat)

      return
      end
