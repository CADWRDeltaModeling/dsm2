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



*== Public (SetBoundaryValues) ================================================

      LOGICAL FUNCTION SetBoundaryValuesFromData()
      use grid_data
      IMPLICIT NONE

*   Purpose:  Master routine for setting user-supplied boundary values as
*             well as object to object flows.

*   Argument definitions:

*   Module data:

      include 'network.inc'
      include 'netbnd.inc'

      include '../timevar/dss.inc'

*   Routines by module:

***** Local:
      LOGICAL  ReadNetworkBoundaryValues
      LOGICAL  AssignNetworkBoundaryValues
      EXTERNAL ReadNetworkBoundaryValues
      EXTERNAL AssignNetworkBoundaryValues

      integer i                 ! loop index

***** Network control:
      INTEGER  NetworkSeconds
      EXTERNAL NetworkSeconds

*   Programmed by: Lew DeLong
*   Date:          Aug   1992
*   Modified by:   Ralph Finch, Eli Ateljevich
*   Last modified: April 2004

*-----Implementation -----------------------------------------------------

c-----update external and internal flows for last time step
      do i=1,nqext
         qext(i).prev_flow=qext(i).flow
      enddo

      do i=1,nobj2obj
         obj2obj(i).prev_flow=obj2obj(i).flow
      enddo

c-----check if new data needs to be read from DSS for each interval group
c-----fill in fourpt 'boundary' array from DSS buffer

      StreamBndValue=0.0        ! initialize boundary objects and arrays to zero
      do i=1,max_qext
         qext(i).flow=0.0
      enddo

c-----update the data stored in the inputpaths
      call UpdateTimeVaryingData

c-----store the data in boundary objects
      call store_values

c-----update boundary array with stage and external flows
c-----call apply_boundary_values

      SetBoundaryValuesFromData = .TRUE.

      RETURN
      END
      
*== Public (UpdateTimeVaryingData) ================================================
	subroutine UpdateTimeVaryingData()
	use type_defs
	use iopath_data
	implicit none
*   Module data:
c      include '../fixed/defs.f'
      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'

	integer i

      if (npthsin_min15 .gt. 0) then
         call readtvd(max_inp_min,mins15,npthsin_min15,ptin_min15,
     &        datain_15min)
      endif

      if (npthsin_hour1 .gt. 0) then
         call readtvd(max_inp_hour,hrs,npthsin_hour1, ptin_hour1,
     &        datain_1hour)
      endif

      if (npthsin_day1 .gt. 0) then
         call readtvd(max_inp_day,dys,npthsin_day1,ptin_day1,
     &        datain_1day)
      endif

      if (npthsin_month1 .gt. 0) then
         call readtvd(max_inp_month,mths,npthsin_month1,ptin_month1,
     &        datain_1month)
      endif

      if (npthsin_year1 .gt. 0) then
         call readtvd(max_inp_year,yrs,npthsin_year1,ptin_year1,
     &        datain_1year)
      endif

      if (npthsin_irr .gt. 0) then
         call readtvd(max_inp_irr,irrs,npthsin_irr,ptin_irr,
     &        datain_irr)
      endif

      do i=1,ninpaths
         call get_inp_data(i) ! get input data from buffers
      end do

      end subroutine



*== Public (store_values) ================================================
      subroutine store_values
      Use Gates, only: gateArray,gate,ngate,setFree
      use IO_Units
      use grid_data
      implicit none

c-----Fill time-varying data arrays for FourPt, and fill external
c-----flow structure for tidefile

c-----common blocks

      include 'network.inc'
      include 'netbnd.inc'
      include 'netcntrl.inc'
      include 'chconnec.inc'

c-----local variables

      integer i,j

      real*8
     &     fetch_data
      real*8 install
	type(gate),pointer :: currentGate


 619  format(/'FREE_GATE value specified in time-varying input for gate ',a
     &     /'at time ',a,
     &     ' but either the free width or depth'
     &     /'were not given in the input.')

 620  format(/'Invalid value for gate operation encountered in path:'
     &     /a
     &     /'Value is: ',g10.3
     &     /'date/time: ',a)

 630  format(/'CALC mode specified in input time series for gate ',a
     &     /' device ',a,' at time ',a,
     &     /' but this gate has no operating rule affecting gate position')



c-----object-to-object flow values
      do i=1,nobj2obj
	   !fixme: ask Ralph/Parviz if they know why this if statement is here   
         !if (obj2obj(i).datasource.indx_ptr .ne. 0) then
         ! todo: dangerous!!!! multiplication is just for debug
            obj2obj(i).flow=fetch_data(obj2obj(i).datasource)
         !endif
      enddo

c-----stage boundaries
      do i=1,nstgbnd
         stgbnd(i).value=fetch_data(stgbnd(i).datasource)
	end do

c-----external flows
      do i=1,nqext
	   qext(i).flow=fetch_data(qext(i).datasource)
	end do

c-----gate controls
      do i=1,nGate
	   currentGate=>gateArray(i)
	   do j=1,currentGate.nDevice
	      currentGate.Devices(j).opCoefToNode=fetch_data(
     &         currentGate.Devices(j).op_to_node_datasource)
	      currentGate.Devices(j).opCoefFromNode=fetch_data(
     &         currentGate.Devices(j).op_from_node_datasource)
	      currentGate.Devices(j).baseElev=fetch_data(
     &         currentGate.Devices(j).elev_datasource)     
	      currentGate.Devices(j).height=fetch_data(
     &         currentGate.Devices(j).height_datasource)       
	      currentGate.Devices(j).maxWidth=fetch_data(
     &         currentGate.Devices(j).width_datasource)
            install = fetch_data(currentGate.install_datasource)
!            call setFree(currentGate,install.eq. 0.D0)
	   end do
      end do


      return
      end

c todo: gate_free, fetching data from expression


*== Public (ApplyBoundaryValues) ================================================

      subroutine ApplyBoundaryValues()
      use grid_data
      use constants
	implicit none

*   Purpose:  Move data from boundary objects to hydro arrays and data structures. 
      include 'network.inc'
      include 'netbnd.inc'
      include 'chconnec.inc'

      integer intchan,i,obj_type,node
	real(8) value

c-----object-to-object flows that involve nodes
c-----(reservoir obj2obj flows are handled in the reservoir calcs)
      do i=1,nobj2obj
c--------from a node
         if (obj2obj(i).from_obj.obj_type .eq. obj_node) then
            intchan=abs(obj2obj(i).from_obj.hydrochan) ! neg channel number -> downstream end            
c-----------note sign: from flow is subtracted
            if (obj2obj(i).from_obj.hydrochan .gt. 0) then 
              ! upstream end of channel connected to node
               StreamBndValue(intchan*2-1) =
     &              StreamBndValue(intchan*2-1) - obj2obj(i).flow
            else
              ! downstream end of channel connected to node
               StreamBndValue(intchan*2) =
     &              StreamBndValue(intchan*2) - obj2obj(i).flow
            endif
         endif

c--------to a node
         if (obj2obj(i).to_obj.obj_type .eq. obj_node) then
            intchan=abs(obj2obj(i).to_obj.hydrochan) ! - channel number denotes downstream end connected
            !todo: eli,experiment  intchan .gt. 0 is probably wrong
            intchan=node_geom(obj2obj(i).to_obj.obj_no).sumQchan
            if (intchan .gt. 0) then ! upstream end of channel connected to node
               StreamBndValue(intchan*2-1) =
     &              StreamBndValue(intchan*2-1) + obj2obj(i).flow
            else                ! downstream end of channel connected to node
               intchan = -intchan
               StreamBndValue(intchan*2) =
     &              StreamBndValue(intchan*2) + obj2obj(i).flow
            endif
         endif
      enddo

      do i=1,nqext
	   obj_type=qext(i).attach_obj_type
	   value=qext(i).flow
         if (obj_type.eq. obj_node) then
            intchan=node_geom(qext(i).attach_obj_no).sumQChan
	      if (intchan .gt. 0) then
		      ! neg channel number denotes downstream end
                StreamBndValue(intchan*2-1) = 
     &            StreamBndValue(intchan*2-1) + value
            ! source/sink or stage boundary for channel
            else                ! downstream end of channel connected to node
               intchan=-intchan
			 if (DownBoundaryCode(intchan) .eq. 2) then ! external node
                   ! flow is at exterior node, and at downstream end,
                   ! invert sign of flow for 4pt convention
                   value=-value                !fixme: is qext correct?
               endif
               StreamBndValue(intchan*2) = StreamBndValue(intchan*2) + value
            endif
         endif
	end do

      do i=1,nstgbnd
	   value=stgbnd(i).value
	      node=stgbnd(i).node
	      if (node_geom(node).nup .gt. 0) then
	          !upstream node is attached
	          intchan=node_geom(node).upstream(1)
                StreamBndValue(intchan*2-1) = value
            else
	          intchan=node_geom(node).downstream(1)
	          StreamBndValue(intchan*2) = value
            endif
	end do
      
	return
	end subroutine




      real*8 function reservoir_source_sink_prev (
     &     reservoir_no
     &     ,acct_ndx
     &     )

c-----Given a reservoir number, add the sources and sinks
c-----from previous time step and return the value.
      use grid_data
      use constants
      implicit none

c-----argument
      integer reservoir_no      ! reservoir number [INPUT]
     &     ,acct_ndx            ! accounting index, if 0 ignore [INPUT]

c-----includes

c-----local variables
      integer
     &     i                    ! loop index
     &     ,qndx                ! external/internal flow index

      reservoir_source_sink_prev=0.0

c-----external flows
      if (acct_ndx .eq. 0 .or.
     &     acct_ndx .eq. ALL_FLOWS .or.
     &     acct_ndx .eq. QEXT_FLOWS) then
         i=1
         do while (res_geom(reservoir_no).qext(i) .gt. 0) ! todo: bad substitute for nqext
            qndx=res_geom(reservoir_no).qext(i)
            reservoir_source_sink_prev=reservoir_source_sink_prev+
     &           qext(qndx).prev_flow
            i=i+1
         enddo
      endif

c-----internal flows
      if (acct_ndx .eq. 0 .or.
     &     acct_ndx .eq. ALL_FLOWS .or.
     &     acct_ndx .eq. QINT_FLOWS) then
         i=1
         do while (res_geom(reservoir_no).qinternal(i) .ne. 0)
            qndx=res_geom(reservoir_no).qinternal(i)
            if (obj2obj(qndx).from_obj.obj_type .eq. obj_reservoir
     &          .and. obj2obj(qndx).from_obj.obj_no .eq. reservoir_no) then 
            ! from this reservoir
               reservoir_source_sink_prev=reservoir_source_sink_prev -
     &              obj2obj(qndx).prev_flow
            end if
            if (obj2obj(qndx).to_obj.obj_type .eq. obj_reservoir
     &          .and. obj2obj(qndx).to_obj.obj_no .eq. reservoir_no) then             
               reservoir_source_sink_prev=reservoir_source_sink_prev +
     &              obj2obj(qndx).prev_flow
            endif
            i=i+1
         enddo
      endif

      return
      end

      REAL*8 function reservoir_source_sink (
     &     reservoir_no
     &     ,acct_ndx
     &     )

c-----Given a reservoir number, add the sources and sinks
c-----to that reservoir and return the value
      use grid_data
      use constants
      implicit none
    
c-----argument
      integer reservoir_no      ! reservoir number [INPUT]
     &     ,acct_ndx            ! accounting index, if 0 ignore [INPUT]

c-----includes

c-----local variables
      integer
     &     i                    ! loop index
     &     ,qndx                ! external/internal flow index

      reservoir_source_sink=0.0

c-----external flows
      if (acct_ndx .eq. 0 .or.
     &     acct_ndx .eq. ALL_FLOWS .or.
     &     acct_ndx .eq. QEXT_FLOWS) then
         i=1
         do while (res_geom(reservoir_no).qext(i) .gt. 0) ! todo: unclear way looping res.nqext
            qndx=res_geom(reservoir_no).qext(i)
            reservoir_source_sink=reservoir_source_sink+
     &           qext(qndx).flow
            i=i+1
         enddo
      endif

c-----internal flows
      if (acct_ndx .eq. 0 .or.
     &     acct_ndx .eq. ALL_FLOWS .or.
     &     acct_ndx .eq. QINT_FLOWS) then
         i=1
         do while (res_geom(reservoir_no).qinternal(i) .gt. 0) 
            !todo: unclear substitute for looping nqext
            qndx=res_geom(reservoir_no).qinternal(i)
            if (obj2obj(qndx).from_obj.obj_type .eq. obj_reservoir 
     &          .and. obj2obj(qndx).from_obj.obj_no .eq. reservoir_no) then 
               ! from reservoir
               reservoir_source_sink=reservoir_source_sink -
     &              obj2obj(qndx).flow
            end if
            if (obj2obj(qndx).to_obj.obj_type .eq. obj_reservoir
     &          .and. obj2obj(qndx).to_obj.obj_no .eq. reservoir_no) then             
                            ! to reservoir       
               reservoir_source_sink=reservoir_source_sink +
     &              obj2obj(qndx).flow
            endif
            i=i+1
         enddo
      endif

      return
      end

*== Public (UpstreamBoundaryValue) =====================================

      REAL*8 FUNCTION UpstreamBoundaryValue()

      IMPLICIT NONE

*   Purpose:  Return a boundary value, computed from a simple boundary
*             equation, for the upstream end of the current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netbnd.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      UpstreamBoundaryValue = StreamBndValue( 2*Branch - 1)

      RETURN
      END

*== Public (DownstreamBoundaryValue) ===================================

      REAL*8 FUNCTION DownstreamBoundaryValue()

      IMPLICIT NONE

*   Purpose:  Return a boundary value, computed from a simple boundary
*             equation, for the downstream end of the current channel.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netbnd.inc'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DownstreamBoundaryValue = StreamBndValue( 2*Branch )

      RETURN
      END
