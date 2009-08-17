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

      subroutine print_output(istat)

c-----print the output
      use IO_Units
      use Gates, ONLY: gateArray, nGate, deviceTypeString, controlTypeString
      use grid_data
      use runtime_data
      use constants
      use logging
      use iopath_data
      use common_xsect
      implicit none

      

      include '../hydrolib/network.inc'
      include '../hydrolib/netcntrl.inc'
      include '../hydrolib/chconnec.inc'

c-----Local variables

      integer
     &     istat                ! status of call (returned)
     &     ,i,j                 ! indices
     &     ,chan                ! channel numbers
     &     ,objConnectedID_external_number  ! external number for either reservoir or channel

      character
     &     objtype*4,            ! string representing object type (reservoir, channel)
     &     from_obj_type*16,to_obj_type*16,
     &     from_obj_identifier*32,to_obj_identifier*32
      
      character(LEN=32) :: scratch1 
      character(LEN=32) :: scratch2            
      character(LEN=32) :: scratch3
      character(LEN=32) :: scratch4

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
 808  format(/'DSM2-',a,' Version ',a)

      write(unit_output, 810) crdt14
 810  format('Run date and time: ', a/)


      istat=0
      do i=1,ntitles
         write(unit_output, '(a)') trim(title(i))
      enddo

      if (run_length .ne. ' ') then
c--------run length should be in form: '20hour' or '5day'
         Write(unit_output,900) trim(run_length)
 900     format(/' Model run length: ',a)
      else                      ! start/end char dates given
         Write(unit_output,920) run_start_date,run_end_date,time_step
 920     format(' Model run:            time'/
     &        ' Start date: ',a14/
     &        ' End   date: ',a14/
     &        ' Time-Step=',i5,' minutes')
      endif
c-----1D open-channel flow method
 940  format(/' Type of analysis: ', a)
      if (terms .eq. 1) then
         write(unit_output,940) 'DYNAMIC WAVE'
      elseif(terms .eq. 2) then
         write(unit_output,940) 'DIFFUSION WAVE'
      elseif(terms .eq. 3) then
         write(unit_output,940) 'KINEMATIC WAVE'
      else
         write(unit_output,940) 'Not Specified ---> DYNAMIC WAVE assumed'
      endif

      if(variabledensity) then
         write(unit_output,1020)
 1020    format(/' Variable Density Option selected (i.e. baroclinic term)')
      endif

      if(variablesinuosity) then
         write(unit_output,1040)
 1040    format(/' Variable Sinuosity Option selected')
      endif

      if (repl) then
         write(unit_output,*)
     &        'repl=true:  If two different cross-sections are'
     &        ,'assigned to two channels such that both cross-sections are adjacent'
     &        ,'to the same node and there are no other channels connected to the'
     &        ,' node, then one cross-section will be replaced with the other.'
      else
         write(unit_output,*)
     &        'repl=false:  Cross-section replacement is turned off.'
      endif

      write(unit_output,1060)
 1060 format(////20x,'CHANNEL GEOMETRY INFORMATION'/
     &     20x,'----------------------------'//
     &     '                                                  NODE'/
     &     ' CHANNEL NO     (ft)                  DISP.    CONNECTIVITY'/
     &     ' INT  EXT       LENGTH     MANNING    COEFF.      DOWN   UP  '/
     &     '---------     ----------   --------   --------   ------------ ')

      do chan=1,nchans
c-----------upstream node
            write(unit_output,1080)chan,chan_geom(chan).chan_no,chan_geom(chan).length,
     &           chan_geom(chan).manning,chan_geom(chan).disp,
     &           node_geom(chan_geom(chan).downnode).node_id,
     &           node_geom(chan_geom(chan).upnode).node_id
 1080       format(i5,1x,i4,1x,i8,5x,f8.4,3x,f8.4,5x,i4,1x,i4)
      enddo


      write(unit_output,1120)
 1120 format(////20x,'NODES '/
     &     8x,'-------------------------------------------'//
     &     '  NODE         NUMBERS '/
     &     '  INT          EXT     '/
     &     '---------  ----------  ')

      do i=1,nnodes
	    write(unit_output,1140)i, node_geom(i).node_id
	end do
 1140 format(i8,2x,i8)

      write(unit_output,1100)
 1100 format(///,8x,'GATE CONNECTION INFORMATION'/
     &     8x,'------------------------------------------------------'///
     &     '                    CONNECTED          TO        # DEVICES' /
     &     '    NAME            WATER BODY      EXT NODE    WEIR   PIPE' /
     &     '  ----------------  -------------    -------    -----  -----')

      do i=1,ngate
	   if (gateArray(i).objConnectedType.eq.obj_channel) then
	      objtype='CHAN'
            objConnectedID_external_number = chan_geom(gateArray(i).objConnectedID).chan_no
	   else if (gateArray(i).objConnectedType. eq. obj_reservoir) then
	      objtype='RES '
            objConnectedID_external_number = gateArray(i).objConnectedID
	   end if
	    
	   if (gateArray(i).inUse) then
	      write(unit_output,1160)
     &           gateArray(i).name,objtype
     &          ,objConnectedID_external_number
     &          ,node_geom(gateArray(i).node).node_id
     &          ,gateArray(i).nDevice
 1160 format(a19,1x,a4,2x,i7,4x,i5,2x,i5,2x,i5)
		end if
	end do


      write(unit_output,1200)
 1200 format(///,8x,'GATE DEVICES'/
     &     8x,'------------'///
     &     8x,'(initial installations)    '//
     &     ,77x,                                       '#OF        (ft.)     (ft.)    (ft.)       FLOW COEFF.  '/
     &     ' GATE',60x,                    'GATE        DUPLICATE  WIDTH OR   BASE               TO      FROM  '/
     &     ' NAME(DEVICE)',40x, 'TYPE       CONTROL     DEVICES    RADIUS     ELEV    HEIGHT     NODE    NODE  '/
     &     '------------',41x,  '-----      -------     -----    ----------  -------  -------    ------  ---------')
      do i=1, ngate	   
         do j=1,gateArray(i).nDevice
               call DeviceTypeString(scratch1,gateArray(i).Devices(j).structureType)
               call ControlTypeString(scratch2,gateArray(i).Devices(j).controlType)
c--------------Flow can occur through the gate
               write(unit_output,1220)
     &              gateArray(i).name,
     &                '(' //trim(gateArray(i).devices(j).name) // ')',
     &              trim(scratch1),
     &              trim(scratch2),
     &              gateArray(i).Devices(j).nDuplicate,
     &              gateArray(i).Devices(j).maxWidth,
     &              gateArray(i).Devices(j).baseElev,
     &              gateArray(i).Devices(j).height,
     &              gateArray(i).Devices(j).flowCoefToNode,
     &              gateArray(i).Devices(j).flowCoefFromNode
 1220          format(a32,1x,a16,a8,2x,a16,1x,i4,2x,f13.2,1x,f9.2,1x,2f9.2,2f9.2)
         end do
      enddo

      write(unit_output,1400)
 1400 format(/////25x,'RESERVOIRS'/
     &     28x,'----------'//
     &     '                         6'/
     &     '                       x10        (ft.)     (ft.)                Adjusted '/
     &     '                      (Sqft)     Initial   Bottom            Flow Coefficient'/
     &     ' Name                  Area       Stage     Elev.    Node   To Res.    To Chan'/
     &     '-------              ---------   --------   ------- -----   ------------------ ')
C-----Franks Tract              141.17864   5.02     -10.1     72     2000.       2000.
C-----1234567890123456789012345678901234567890123456789012345678901234567890'

      do i=1,nreser
         write(unit_output,1420)res_geom(i).name,
     &        res_geom(i).area,
     &        res_geom(i).stage,
     &        res_geom(i).botelv,
     &        res_geom(i).node_no(1),
     &        res_geom(i).coeff2res(1),res_geom(i).coeff2chan(1)
 1420    format(/a19,1x,f10.5,1x,f8.2,1x,f9.1,2x,i4,2x,f8.0,4x,f8.0,5x)
         do j=2,res_geom(i).nnodes
            write(unit_output,1440)res_geom(i).node_no(j),
     &           res_geom(i).coeff2res(j),res_geom(i).coeff2chan(j)
 1440       format(51x,i4,2x,f8.0,4x,f8.0,5x)
         enddo
      enddo

      write(unit_output,1500)
 1500 format(/////25x,'OBJ2OBJ Flow Transfers'/
     &     28x,'----------'//
     &     '                         '/
     &     ' Name                  From                  To              '/
     &     '-------              ---------   --------   --------- -----  ')

      do i=1,nobj2obj
         call obj_type_name(obj2obj(i).from_obj.obj_type,from_obj_type)
         call obj_type_name(obj2obj(i).to_obj.obj_type,to_obj_type)
         call objno_to_name(obj2obj(i).from_obj.obj_type,
     &                      obj2obj(i).from_obj.obj_no,
     &                      from_obj_identifier)
         call objno_to_name(obj2obj(i).to_obj.obj_type,
     &                      obj2obj(i).to_obj.obj_no,
     &                      to_obj_identifier)

         write(unit_output,1520)obj2obj(i).name,
     &        from_obj_type,from_obj_identifier,
     &        to_obj_type, to_obj_identifier
 1520    format(/a20,1x,a12,1x,a20,1x,a12,1x,a20)
      enddo

      if (print_level .ge. 5) then
         call geom_output
c-----todo: commented these out because they refer to hydro 
c           and the compiler for qual complains. We should really have a general
c           print_output that resides in /common, a print_outhydro here 
c           and a print_outqual (the latter exists)
c         call virt_output
c         call check_area
      endif

      WRITE(unit_output,1600)
 1600 format(////,45x,'INPUT PATHS'/
     &     45x,'-----------'///
     &     'NAME',29x,'VARIABLE',6x,'FILE NAME',70x,'DSS PATH',50x
     &     '---------------------------------------------------------------------------------------',
     &     '   ---------------------------------------')

      do i=1,ninpaths
         write(unit_output,1620)pathinput(i).name,pathinput(i).variable
     &        ,pathinput(i).filename,pathinput(i).path
 1620    format(a30,1x,a16,1x,a80,1x,a50)
      enddo

      return
      end