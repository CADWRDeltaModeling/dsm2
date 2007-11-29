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


*== Private (AssignNetworkBoundaryValues) ==================================

      LOGICAL FUNCTION AssignNetworkBoundaryValues()

      IMPLICIT NONE

*   Purpose:  Assign network boundary values to be used for the end
*             of the current time step.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netbnd.inc'

      include '../input/fixed/misc.f'

*   Local Variables:
      INTEGER I, J

*   Routines by module:

***** Local:

*   Intrinsics:
      INTEGER   IABS
      INTRINSIC IABS

*   Programmed by: Lew DeLong
*   Date:          October 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      DO 100 I=1,BndValues

         J = Channel(I)

         IF(  J .NE. 0
     &        .AND.
     &        IABS(J) .LE. NumCh  ) THEN

*-----------Valid channel number.

            IF( J .GT. 0 ) THEN

*--------------Upstream end of channel.

               StreamBndValue( 2*J-1 ) = Current(I)

            ELSE

*--------------Downstream end of channel.

               StreamBndValue( -2*J ) = Current(I)

            END IF

         ELSE

*-----------Invalid channel number.

            WRITE(UNIT_ERROR,*) ' ######Error (AssignNetworkBoundaryValues)'
            WRITE(UNIT_ERROR,*) ' Channel ',IABS(J),' does not exist...'
         END IF

 100  CONTINUE

      AssignNetworkBoundaryValues = .TRUE.

      RETURN
      END

*== Public (SetUserNetBndValues) ================================================

      LOGICAL FUNCTION SetUserNetBndValues()

      IMPLICIT NONE

*   Purpose:  Set current values of user-supplied boundary values.

*   Arguments:

*   Argument definitions:

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netbnd.inc'
      include 'chconnec.inc'

      include '../input/fixed/common.f'
      include '../input/time-varying/dss.inc'
      include '../input/time-varying/readdss.inc'
      include '../input/time-varying/tide.inc'

*   Routines by module:

***** Local:
      LOGICAL  ReadNetworkBoundaryValues
      LOGICAL  AssignNetworkBoundaryValues
      EXTERNAL ReadNetworkBoundaryValues
      EXTERNAL AssignNetworkBoundaryValues

      integer i                 ! loop index
     &     ,intchan             ! hydro internal channel number

***** Network control:
      INTEGER  NetworkSeconds
      EXTERNAL NetworkSeconds

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Aug   1992
*   Modified by:   Ralph Finch
*   Last modified: June 1994
*   Version 93.01, January, 1993

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

      do i=1,2*MaxChannels
         StreamBndValue(i)=0.0
      enddo

      do i=1,max_qext
         qext(i).flow=0.0
      enddo

      do i=1,MaxChannels
         QExtBranch(i)=0.0
      enddo

      nqext=0                   ! number of external flow data

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

c-----update boundary array with stage and external flows
      call store_values

c-----object-to-object flow values
      do i=1,nobj2obj
         if (obj2obj(i).in_no .ne. 0) then
            obj2obj(i).flow=pathinput(obj2obj(i).in_no).value
         endif
      enddo

c-----object-to-object flows that involve nodes
c-----(reservoir obj2obj flows are handled in the reservoir calcs)
      do i=1,nobj2obj
c--------from a node
         if (obj2obj(i).from.object .eq. obj_node) then
            intchan=abs(obj2obj(i).from.hydrochan) ! - channel number denotes downstream end connected
c-----------note sign: from flow is subtracted
            if (obj2obj(i).from.hydrochan .gt. 0) then ! upstream end of channel connected to node
               StreamBndValue(intchan*2-1) =
     &              StreamBndValue(intchan*2-1) - obj2obj(i).flow
            else                ! downstream end of channel connected to node
               StreamBndValue(intchan*2) =
     &              StreamBndValue(intchan*2) - obj2obj(i).flow
            endif
            QExtBranch(intchan)=QExtBranch(intchan)-obj2obj(i).flow
         endif

c--------to a node
         if (obj2obj(i).to.object .eq. obj_node) then
            intchan=abs(obj2obj(i).to.hydrochan) ! - channel number denotes downstream end connected
            if (obj2obj(i).to.hydrochan .gt. 0) then ! upstream end of channel connected to node
               StreamBndValue(intchan*2-1) =
     &              StreamBndValue(intchan*2-1) + obj2obj(i).flow
            else                ! downstream end of channel connected to node
               StreamBndValue(intchan*2) =
     &              StreamBndValue(intchan*2) + obj2obj(i).flow
            endif
            QExtBranch(intchan)=QExtBranch(intchan)+obj2obj(i).flow
         endif
      enddo

      SetUserNetBndValues = .TRUE.

      RETURN
      END

      subroutine store_values

      implicit none

c-----Fill time-varying data arrays for FourPt, and fill external
c-----flow structure for tidefile

c-----common blocks

      include '../input/fixed/common.f'
      include '../input/time-varying/tide.inc'
      include 'network.inc'
      include 'netbnd.inc'
      include 'netcntrl.inc'
      include 'chconnec.inc'

c-----local variables

      logical
     &     leqner               ! hec lib function: nearly equal

      integer
     &     ptr                  ! pathname array index
     &     ,intchan             ! internal channel number
     &     ,dsm_gateno          ! DSM2 gate number
     &     ,hydro_gateno        ! Hydro gate number
     &     ,PrevGatePosition(MaxNGate) ! Previous gate position
     &     ,lnblnk              ! intrinsic

      REAL*8
     &     value                ! time-varying value
     &     ,ngate_frac          ! fraction of opening to simulate number of gates open

 619  format(/'FREE_GATE value specified in time-varying input for gate ',a
     &     /'at time ',a,
     &     ' but either the free width or depth'
     &     /'were not given in the input.')

 620  format(/'Invalid value for gate position for path:'
     &     /a
     &     /'Value is: ',g10.3
     &     /'date/time: ',a
     &     /'Possibly number of gates was not given in fixed input.')

c-----stage and external flows
      do ptr=1,ninpaths

c--------don't use input paths which are only for replacement
c--------(priority 2 or higher)
         if (pathinput(ptr).priority .gt. 1) goto 100

         call get_inp_data(ptr) ! get input data from buffers

         intchan=abs(pathinput(ptr).hydrochan) ! - channel number denotes downstream end connected
         if (intchan .ne. 0 .and.
     &        pathinput(ptr).object .eq. obj_node) then ! source/sink or stage boundary for channel
c-----------if: flow is at exterior node, and at downstream end,
c-----------invert sign of flow for 4pt convention
            value=pathinput(ptr).value
            if (pathinput(ptr).hydrochan .gt. 0) then ! upstream end of channel connected to node
               StreamBndValue(intchan*2-1) = StreamBndValue(intchan*2-1) + value
            else                ! downstream end of channel connected to node
               if (DownBoundaryCode(intchan) .eq. 2) then ! external node
                  value=-value
               endif
               StreamBndValue(intchan*2) = StreamBndValue(intchan*2) + value
            endif
            if (pathinput(ptr).data_type .eq. flow_type) then
c--------------external flow (diversion, seepage, ...)
               QExtBranch(intchan)=QExtBranch(intchan)+pathinput(ptr).value
            endif
         endif

         if (pathinput(ptr).data_type .eq. flow_type .and.
     &        pathinput(ptr).object .ne. obj_obj2obj) then
            nqext=nqext+1
c-----------fixme: this sux
            if (qext(nqext).in_no .ne. ptr) then
               write(unit_error,650) qext(nqext).in_no, ptr
 650           format(/'Software error in netbnd.f: mismatch between',
     &              /' qext(nqext).in_no ',i3,' and ptr ',i3)
               call exit(1)
            endif
            qext(nqext).flow=pathinput(ptr).value
         else if (pathinput(ptr).object .eq. obj_gate) then ! gate operation
c-----------accept timed info for either 'time' or 'calc' operation
            if (gate_geom(pathinput(ptr).object_no).oper .eq. 'time' .or.
     &          gate_geom(pathinput(ptr).object_no).oper .eq. 'calc') then
               dsm_gateno=pathinput(ptr).object_no
               hydro_gateno=hydrogates(dsm_gateno)
c--------------time-varying gate parameters
               if (pathinput(ptr).gate_param .eq. gate_ngates) then
                  gate_geom(dsm_gateno).ngates=pathinput(ptr).value
               else if (pathinput(ptr).gate_param .eq. gate_width_up) then
                  gate_geom(dsm_gateno).widthup=pathinput(ptr).value
                  WidthWeirLand(hydro_gateno)=gate_geom(dsm_gateno).widthup
               else if (pathinput(ptr).gate_param .eq. gate_width_down) then
                  gate_geom(dsm_gateno).widthdown=pathinput(ptr).value
                  WidthWeirSea(hydro_gateno)=gate_geom(dsm_gateno).widthdown
               else if (pathinput(ptr).gate_param .eq. gate_crest_elev) then
                  gate_geom(dsm_gateno).crestelev=pathinput(ptr).value
                  DepthWeirCrest(hydro_gateno)=gate_geom(dsm_gateno).crestelev
               else if (pathinput(ptr).gate_param .eq. gate_coeff_weir_up) then
                  gate_geom(dsm_gateno).coeffweirup=pathinput(ptr).value
                  GateLandCoef_Weir(hydro_gateno)=gate_geom(dsm_gateno).
     &                 coeffweirup * sqrt(2.*gravity)
               else if (pathinput(ptr).gate_param .eq. gate_coeff_weir_down) then
                  gate_geom(dsm_gateno).coeffweirdown=pathinput(ptr).value
                  GateSeaCoef_Weir(hydro_gateno)=gate_geom(dsm_gateno).
     &                 coeffweirdown * sqrt(2.*gravity)
               else if (pathinput(ptr).gate_param .eq. gate_npipes) then
                  gate_geom(dsm_gateno).npipes=pathinput(ptr).value
                  NumberofPipes(hydro_gateno)=gate_geom(dsm_gateno).npipes
               else if (pathinput(ptr).gate_param .eq. gate_pipe_rad) then
                  gate_geom(dsm_gateno).piperad=pathinput(ptr).value
                  PipeRadius(hydro_gateno)=gate_geom(dsm_gateno).piperad
               else if (pathinput(ptr).gate_param .eq. gate_pipe_elev) then
                  gate_geom(dsm_gateno).pipeelev=pathinput(ptr).value
                  DepthInvertPipe(hydro_gateno)=gate_geom(dsm_gateno).pipeelev
               else if (pathinput(ptr).gate_param .eq. gate_coeff_pipe_up) then
                  gate_geom(dsm_gateno).coeffpipeup=pathinput(ptr).value
                  GateLandCoef_Pipe(hydro_gateno)=gate_geom(dsm_gateno).
     &                 coeffpipeup * sqrt(2.*gravity)
               else if (pathinput(ptr).gate_param .eq. gate_coeff_pipe_down) then
                  gate_geom(dsm_gateno).coeffpipedown=pathinput(ptr).value
                  GateSeaCoef_Pipe(hydro_gateno)=gate_geom(dsm_gateno).
     &                 coeffpipedown * sqrt(2.*gravity)
               else if (pathinput(ptr).gate_param .eq. gate_dhopen) then
                  gate_geom(dsm_gateno).dhopen=pathinput(ptr).value
                  DeltaHOpen(hydro_gateno)=gate_geom(dsm_gateno).dhopen
               else if (pathinput(ptr).gate_param .eq. gate_velclose) then
                  gate_geom(dsm_gateno).velclose=pathinput(ptr).value
                  VelocityClose(hydro_gateno)=gate_geom(dsm_gateno).velclose
               else if (pathinput(ptr).data_type .eq. gate_type) then ! gate position
                  ngate_frac=pathinput(ptr).value /
     &                 dfloat(gate_geom(dsm_gateno).ngates)
                  if ((abs(pathinput(ptr).value -  dfloat(GATE_OPEN)) .LT. 0.0001) .or.
     &                 (ngate_frac .gt. 0.1 .and. ngate_frac .le. 1.01)) then
                     if (gate_geom(dsm_gateno).oper .eq. 'calc') then
c-----------------------calculated position gate with forced-open position
                        GateSpecialPos(hydro_gateno)=GATE_OPEN
                        NumberofGatesFraction(hydro_gateno)=ngate_frac
                     else       ! normal timed gate
                        GatePosition(hydro_gateno)=GATE_OPEN
                        NumberofGatesFraction(hydro_gateno)=ngate_frac
                     endif
                  else if ((abs(pathinput(ptr).value - dfloat(GATE_CLOSE)) .LT. 0.0001)) then
                     if (gate_geom(dsm_gateno).oper .eq. 'calc') then
c-----------------------calculated position gate with forced-close position
                        GateSpecialPos(hydro_gateno)=GATE_CLOSE
                     else       ! normal timed gate
                        GatePosition(hydro_gateno)=GATE_CLOSE
                     endif
                  else if (abs(pathinput(ptr).value - dfloat(GATE_CALC)) .LT. 0.0001) then
                     GateSpecialPos(hydro_gateno)=GATE_CALC
                     NumberofGatesFraction(hydro_gateno)=1.0
                  else if (abs(pathinput(ptr).value - dfloat(GATE_FREE)) .LT. 0.0001) then
                     GatePosition(hydro_gateno)=GATE_FREE
                     NumberofGatesFraction(hydro_gateno)=1.0
                  else          ! invalid gate position value
                     write(unit_error, 620)
     &                    pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                    pathinput(ptr).value,current_dt
                     call exit(2)
                  endif
                  if (PrevGatePosition(hydro_gateno) .ne. GatePosition(hydro_gateno)) then
                     GateOperatingTime(hydro_gateno)=julmin
                     PrevGatePosition(hydro_gateno)=GatePosition(hydro_gateno)
                  endif
               endif
            endif
         endif
 100     continue
      enddo

      return
      end

      REAL*8 function reservoir_source_sink_prev (
     &     reservoir_no
     &     ,acct_ndx
     &     )

c-----Given a reservoir number, add the sources and sinks
c-----from previous time step and return the value

      implicit none

c-----argument
      integer reservoir_no      ! reservoir number [INPUT]
     &     ,acct_ndx            ! accounting index, if 0 ignore [INPUT]

c-----includes

      include '../input/fixed/common.f'
      include '../input/time-varying/tide.inc'

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
         do while (res_geom(reservoir_no).qext(i) .ne. 0)
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
         do while (res_geom(reservoir_no).qint(i) .ne. 0)
            qndx=res_geom(reservoir_no).qint(i)
            if (obj2obj(qndx).from.object .eq. obj_reservoir) then ! from reservoir
               reservoir_source_sink_prev=reservoir_source_sink_prev -
     &              obj2obj(qndx).prev_flow
            else                ! to reservoir
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

      implicit none

c-----argument
      integer reservoir_no      ! reservoir number [INPUT]
     &     ,acct_ndx            ! accounting index, if 0 ignore [INPUT]

c-----includes

      include '../input/fixed/common.f'
      include '../input/time-varying/tide.inc'

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
         do while (res_geom(reservoir_no).qext(i) .ne. 0)
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
         do while (res_geom(reservoir_no).qint(i) .ne. 0)
            qndx=res_geom(reservoir_no).qint(i)
            if (obj2obj(qndx).from.object .eq. obj_reservoir) then ! from reservoir
               reservoir_source_sink=reservoir_source_sink -
     &              obj2obj(qndx).flow
            else                ! to reservoir
               reservoir_source_sink=reservoir_source_sink +
     &              obj2obj(qndx).flow
            endif
            i=i+1
         enddo
      endif

      return
      end

*== Public (SetUserNetBndValueIndex) ================================================

      LOGICAL FUNCTION SetUserNetBndValueIndex(N)

      IMPLICIT NONE

*   Purpose:  Set UserBndValueIndex indicating if boundary values
*             are to be supplied by user.

*   Arguments:
      INTEGER N

*   Argument definitions:
*     N - index indicating values
*          [0] - will not be supplied by user, or
*          [1] - will be supplied by user.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netbnd.inc'

*   Local Variables:

*   Routines by module:

***** Local:

*   Intrinsics:

*   Programmed by: Lew DeLong
*   Date:          Aug   1992
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( N .EQ. 1 ) THEN
         ReadBoundaryValues = .TRUE.
      ELSE
         ReadBoundaryValues = .FALSE.
      END IF

      SetUserNetBndValueIndex = .TRUE.

      RETURN
      END

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

*== Public (SetNetworkBoundaryValue) =====================================

      LOGICAL FUNCTION SetNetworkBoundaryValue(I,Value)

      IMPLICIT NONE

*   Purpose:  Set a boundary value for the upstream end
*             of current channel.

*   Arguments:
      INTEGER I
      REAL*8 Value

*   Argument definitions:
*     I - channel number, + upstream, - downstream.
*     Value - boundary value to be set.

*   Module data:
      INCLUDE 'network.inc'
      INCLUDE 'netbnd.inc'

      include '../input/fixed/misc.f'

*   Local Variables:

*   Routines by module:

*   Programmed by: Lew DeLong
*   Date:          February 1991
*   Modified by:
*   Last modified:
*   Version 93.01, January, 1993

*-----Implementation -----------------------------------------------------

      IF( I .GT. 0 ) THEN

*--------upstream end of channel.
         StreamBndValue( 2 * I - 1 ) = Value

      ELSE IF( I .LT. 0 ) THEN

*--------downstream end of channel.
         StreamBndValue( - 2 * I ) = Value

      ELSE
         WRITE(UNIT_ERROR,*) ' *** Error (SetNetworkBoundaryValue)'
         WRITE(UNIT_ERROR,*) ' Channel number = ',I
         WRITE(UNIT_ERROR,*) ' Abnormal program end.'
         CALL EXIT(1)
      END IF

      SetNetworkBoundaryValue = .TRUE.

      RETURN
      END
