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
      subroutine print_outqual(istat)
      Use io_Units
      use constants
      use grid_data
      use runtime_data
      use iopath_data
c-----print the output
      use groups, only:WriteGroupMembers2File
	use rate_coeff_assignment,only:output_rate_to_file
      use dss
      use mod_readdss
      use mod_writedss
      implicit none

      include '../qual/param.inc'
      include '../qual/bltm1.inc'
      include '../qual/bltm3.inc'
      include '../qual/bltm2.inc'
 

c-----Local variables

      integer
     &     istat                ! status of call (returned)
     &     ,i                   ! index
     &     ,chan                ! channel numbers
     &     ,l                   ! constituent no.

c-----copyright notices
      write(unit_output, 805)
      write(unit_screen, 805)
 805  format(/
     &     /'Delta Simulation Model 2: A river and estuary simulation model'
     &     /'Copyright (C) 1996-2009 State of California,'
     &     /'Department of Water Resources.'
     &     /'Licensed under the GNU Public License.'     
     &     ) 

      write(unit_screen, 808) dsm2_name, dsm2_version
      write(unit_output, 808) dsm2_name, dsm2_version
 808  format(/'DSM2-',a,' Version ',a/)

c-----1D Lagrangian Transport Modeling of Water Quality


      write(unit_output, 810) crdt14
 810  format(//'   1D LAGRANGIAN TRANSPORT MODELING OF WATER QUALITY',/
     &'   -------------------------------------------------',///
     &     'Run date and time: ', a/)

      l=1
      do i=1,ntitles
         write(unit_output, '(a)') trim(title(i))
      enddo

      if (run_length .ne. ' ') then
c--------run length should be in form: '20hour' or '5day'
         Write(unit_output,900) trim(run_length),time_step
 900     format(/' Model run length: ',a/
     &        ' Time-Step=',i5,' minutes')
      else                      ! start/end char dates given
         Write(unit_output,920) run_start_date,run_end_date,time_step
 920     format(' Model run:            time'/
     &        ' Start date: ',a14/
     &        ' End   date: ',a14/
     &        ' Time-Step=',i5,' minutes')
      endif

      write(unit_output,2031) dqv
 2031 format (' Minimum dispersive',
     &     ' velocity =',f7.3,'  ft/s.')


 940  format(/' Type of simulation: ', a)
      write(unit_output,940) '11 Constituents (Conservative and Non-Conservative) '
      write(unit_output,1060)
 1060 format(////20x,'CHANNEL GEOMETRY AND INITIAL CONCENTRATION '/
     &     20x,'-------------------------------------------'//
     &     '                                                  NODE'/
     &     '  CHAN NO     (ft)                  DISP.    CONNECTIVITY      (mg/l)'/
     &     '  INT  EXT     LENGTH     MANNING    FACTOR      DOWN   UP       INITIAL CONC.'/
     &     '---------  ----------   --------   --------   ------------      ----------')

      do chan=1,nchans
c-----------upstream node
            write(unit_output,1080)chan,chan_geom(chan).chan_no,chan_geom(chan).length,
     &           chan_geom(chan).manning,chan_geom(chan).disp,
     &           node_geom(chan_geom(chan).downnode).node_id,
     &           node_geom(chan_geom(chan).upnode).node_id,
     &           (cstart(chan,l),l=1,neq)
 1080       format(i4,1x,i4,2x,i8,5x,f8.4,3x,f10.4,5x,i4,1x,i4,6x,11(1x,f8.2))
      enddo

      write(unit_output,1120)
 1120 format(////10x,'NODE GEOMETRY '/
     &     '------------------------'//
     &     '    NODE NUMBERS '/
     &     '  INT          EXT     '/
     &     '---------  ----------  ')

      do i=1,nnodes
	    write(unit_output,1140)i, node_geom(i).node_id
	end do
 1140 format(i8,2x,i8)


      write(unit_output,1400)

 1400 FORMAT(////,'                         RESERVOIRS',/
     &       '                         ----------'///
     &       '                          6'/
     &       '                         x10            (ft.)      (mg/l)'/
     &       '                        (Sqft)         BOTTOM      INITIAL'/
     &       ' NAME                    AREA           ELEV.    CONCENTRATION'/
     &       '------                ---------       --------   -------------'/)

      do i=1,nreser
         write(unit_output,1420)res_geom(i).name,res_geom(i).toparea/1.0e6,
     &        res_geom(i).botelv,(cres(i,l),l =1,neq)
 1420 format(a19,2x,f12.6,2x,f10.3,5x,15f9.3)
      enddo

      WRITE(unit_output,1600)
 1600 format(////,45x,'INPUT PATHS'/
     &     45x,'-----------'///
     &     ' NAME                TYPE         FILE NAME',
     &     '                                                DSS PATH NAME'/
     &     '-------------------- ------ ----------------------------------------------------------',
     &     '   -----------------------------------')

      do i=1,ninpaths
         if (pathinput(i).constant_value .ne. miss_val_r)then
           write(unit_output,1615)pathinput(i).name
     &        ,pathinput(i).variable,'constant'
     &        ,pathinput(i).constant_value
         else
           write(unit_output,1620)pathinput(i).name,pathinput(i).variable
     &        ,pathinput(i).filename,pathinput(i).path
         end if
 1615    format(a32,1x,a16,1x,a12,1x,f10.4)        
 1620    format(a32,1x,a16,1x,a80,1x,a50)
      enddo

      WRITE(unit_output,1610)
 1610 format(////,45x,'GROUP DEFINITIONS'/
     &     45x,'-----------'///)


c     Group memebers
	call WriteGroupMembers2File(unit_output)

	call output_rate_to_file(unit_output)

	istat=0

      return
      end
