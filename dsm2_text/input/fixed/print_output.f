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


      subroutine print_output(istat)

c-----print the output

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

      include '../../hydro/network.inc'
      include '../../hydro/netcntrl.inc'
      include '../../hydro/chconnec.inc'
      include '../../hydro/chnluser.inc'
      include '../../hydro/chcxrec1.inc'
      include '../../hydro/chcxrec2.inc'
      include '../time-varying/dss.inc'
      include '../time-varying/readdss.inc'
      include '../time-varying/writedss.inc'

c-----Local variables

      integer
     &     istat                ! status of call (returned)
     &     ,i,j                 ! indices
     &     ,chan                ! channel numbers
     &     ,lnblnk              ! last-non-blank intrinsic

      character
     &     maxq*8               ! flow limitation string

c-----copyright notices
      write(unit_output, 805)
      write(unit_screen, 805)
 805  format(/
     &     /'Copyright (C) 1996, 1997, 1998 State of California,'
     &     /'Department of Water Resources.'
     &     /
     &     /'Delta Simulation Model 2 (DSM2): A River, Estuary, and Land'
     &     /'numerical model.  No protection claimed in original FOURPT and'
     &     /'Branched Lagrangian Transport Model (BLTM) code written by the'
     &     /'United States Geological Survey.  Protection claimed in the'
     &     /'routines and files listed in the accompanying file "Protect.txt".'
     &     /'If you did not receive a copy of this file contact Dr. Paul'
     &     /'Hutton, below.'
     &     /
     &     /'This program is licensed to you under the terms of the GNU General'
     &     /'Public License, version 2, as published by the Free Software'
     &     /'Foundation.'
     &     /
     &     /'You should have received a copy of the GNU General Public License'
     &     /'along with this program; if not, contact Dr. Paul Hutton, below,'
     &     /'or the Free Software Foundation, 675 Mass Ave, Cambridge, MA'
     &     /'02139, USA.'
     &     /
     &     /'THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA'
     &     /'DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY'
     &     /'EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE'
     &     /'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR'
     &     /'PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA'
     &     /'DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR'
     &     /'ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR'
     &     /'CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT'
     &     /'OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR'
     &     /'BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF'
     &     /'LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT'
     &     /'(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE'
     &     /'USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH'
     &     /'DAMAGE.'
     &     /
     &     /'For more information about DSM2, contact:'
     &     /
     &     /'Dr. Paul Hutton'
     &     /'California Dept. of Water Resources'
     &     /'Division of Planning, Delta Modeling Section'
     &     /'1416 Ninth Street'
     &     /'Sacramento, CA  95814'
     &     /'916-653-5601'
     &     /'hutton@water.ca.gov'
     &     /
     &     /'or see our home page: http://wwwdelmod.water.ca.gov/'
     &     /
     &     )

      write(unit_screen, 808) dsm2_name, dsm2_version
      write(unit_output, 808) dsm2_name, dsm2_version
 808  format(/'DSM2-',a,' Version ',a/)

      write(unit_output, 810) crdt14
 810  format('Run date and time: ', a/)

      do i=1,ntitles
         write(unit_output, '(a)') title(i)(1:lnblnk(title(i)))
      enddo

      if (run_length .ne. ' ') then
c--------run length should be in form: '20hour' or '5day'
         Write(unit_output,900) run_length(1:lnblnk(run_length))
 900     format(/' Model run length: ',a)
      else                      ! start/end char dates given
         Write(unit_output,920) run_start_dt,run_end_dt,time_step
 920     format(' Model run:            time'/
     &        ' Start date: ',a14/
     &        ' End   date: ',a14/
     &        ' Time-Step=',i5,' minutes')
      endif
c-----1D open-channel flow method
 940  format(/' Type of analysis: ', a)
      if (terms .eq. 1)then
         write(unit_output,940) 'DYNAMIC WAVE'
      elseif(terms .eq. 2)then
         write(unit_output,940) 'DIFFUSION WAVE'
      elseif(terms .eq. 3)then
         write(unit_output,940) 'KINEMATIC WAVE'
      else
         write(unit_output,940) 'Not Specified ---> DYNAMIC WAVE assumed'
      endif

      if(variabledensity)then
         write(unit_output,1020)
 1020    format(/' Variable Density Option selected (i.e. baroclinic term)')
      endif

      if(variablesinuosity)then
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
     &     '  CHANNEL      (ft)                  DISP.    CONNECTIVITY'/
     &     '  ID  SEQ     LENGTH     MANNING    COEFF.      DOWN   UP       X-SECTIONS'/
     &     '---------  ----------   --------   --------   ------------      ----------')

      do chan=1,max_channels
         if (chan_geom(chan).length .gt. 0) then
c-----------upstream node
            write(unit_output,1080)chan,ext2int(chan),chan_geom(chan).length,
     &           chan_geom(chan).manning,chan_geom(chan).disp,
     &           chan_geom(chan).downnode,chan_geom(chan).upnode,
     &           chan_geom(chan).xsect(1),chan_geom(chan).xsect(2)
 1080       format(i4,1x,i4,2x,i8,5x,f8.4,3x,f8.4,5x,i4,1x,i4,6x,5(1x,i4))
         endif
      enddo

      write(unit_output,1200)
 1200 format(///,26x,'GATE INFORMATION'/
     &     26x,'----------------'///
     &     26x,'     WEIRS      '//
     &     '                               (ft.)'/
     &     '                             WEIR WIDTH      (ft.)      FLOW COEFF.'/
     &     ' NAME   OPER   CHAN   LOC     UP   DOWN    CREST ELEV    UP   DOWN'/
     &     '------  ----   ----  ----   -------------  ----------  -------------')
      do i=1, ngate
         if(gate_geom(i).npipes.eq.0.and.gate_geom(i).chan_no.ne.0)then
c-----------weir type
            if((gate_geom(i).coeffweirup.ne.0. .or.gate_geom(i).coeffweirdown.ne.0.).and.
     &           (gate_geom(i).widthup.ne.0. .or. gate_geom(i).widthdown.ne.0.))then
c--------------Flow can occur through the gate
               write(unit_output,1220)
     &              gate_geom(i).name,gate_geom(i).oper,gate_geom(i).chan_no,
     &              gate_geom(i).loc,gate_geom(i).widthup,gate_geom(i).widthdown,
     &              gate_geom(i).crestelev,
     &              gate_geom(i).coeffweirup,gate_geom(i).coeffweirdown
 1220          format(a7,1x,a4,2x,i5,2x,a4,1x,f7.1,1x,f7.1,1x,f9.2,4x,f6.3,1x,f6.3)
            else
c--------------Barrier
               write(unit_output,1240)
     &              gate_geom(i).name,gate_geom(i).oper,gate_geom(i).chan_no,
     &              gate_geom(i).loc,gate_geom(i).widthup,gate_geom(i).widthdown,
     &              gate_geom(i).crestelev
 1240          format(a7,1x,a4,2x,i5,2x,a4,1x,f7.1,1x,f7.1,1x,f9.2,7x,'BARRIER')
            endif
         endif
      enddo

      write(unit_output,1300)
 1300 format(/////
     &     26x,'    CULVERTS    '//
     &     '                                       (ft.)    (ft.)'/
     &     '                             # OF       PIPE    INVERT   FLOW COEFF.'/
     &     ' NAME   OPER   CHAN   LOC    PIPES     RADIUS    ELEV     UP   DOWN'/
     &     '------  ----   ----  ----   ------   ---------  ------   ------------')
      do i=1, ngate
         if(gate_geom(i).npipes.ne.0.and.gate_geom(i).chan_no.ne.0)then
c-----------culvert type
            if( (gate_geom(i).coeffpipeup.ne.0. .or.gate_geom(i).coeffpipedown.ne.0.)
     &           .and. gate_geom(i).piperad.ne.0.)then
c--------------Flow can occur through the gate
               write(unit_output,1320)
     &              gate_geom(i).name,gate_geom(i).oper,gate_geom(i).chan_no,
     &              gate_geom(i).loc,gate_geom(i).npipes,
     &              gate_geom(i).piperad,gate_geom(i).pipeelev,
     &              gate_geom(i).coeffpipeup,gate_geom(i).coeffpipedown
 1320          format(a7,1x,a4,2x,i5,2x,a4,3x,i4,2x,f10.2,2x,f7.2,3x,f6.3,1x,f6.3)
            else
c--------------Barrier
               write(unit_output,1340)
     &              gate_geom(i).name,gate_geom(i).oper,gate_geom(i).chan_no,
     &              gate_geom(i).loc,gate_geom(i).npipes,
     &              gate_geom(i).piperad,gate_geom(i).pipeelev
 1340          format(a7,1x,a4,2x,i5,2x,a4,3x,i4,2x,f10.2,2x,f7.2,6x,'BARRIER')
            endif
         endif
      enddo
      write(unit_output,1400)
 1400 format(/////25x,'RESERVOIRS'/
     &     25x,'----------'//
     &     '               6'/
     &     '            x10        (ft.)       (ft.)                Adjusted           (cfs)'/
     &     '           (Sqft)     Initial     Bottom            Flow Coefficient        Flow'/
     &     ' Name       Area       Stage       Elev.    Node   To Res.    To Chan    Limitation'/
     &     '------   ---------   --------    -------   -----   ------------------    ----------')
C-----FRANKS   141.17864     5.02       -10.1      72     2000.       2000.
C-----1234567890123456789012345678901234567890123456789012345678901234567890'

      do i=1,nres
         if (res_geom(i).maxq2res(1) .eq. miss_val_r) then ! no flow limitation
            maxq='  none  '
         else
            write(maxq,'(f8.0)') res_geom(i).maxq2res(1)
         endif
         write(unit_output,1420)res_geom(i).name,res_geom(i).area,res_geom(i).stage,
     &        res_geom(i).botelv,res_geom(i).node_no(1),
     &        res_geom(i).coeff2res(1),res_geom(i).coeff2chan(1),maxq
 1420    format(/a7,1x,f10.5,1x,f8.2,1x,f11.1,3x,i5,2x,f8.0,4x,f8.0,5x,a)
         do j=2,res_geom(i).nnodes
         if (res_geom(i).maxq2res(j) .eq. miss_val_r) then ! no flow limitation
            maxq='  none  '
         else
            write(maxq,'(f8.0)') res_geom(i).maxq2res(j)
         endif
            write(unit_output,1440)res_geom(i).node_no(j),
     &           res_geom(i).coeff2res(j),res_geom(i).coeff2chan(j),maxq
 1440       format(42x,i5,2x,f8.0,4x,f8.0,5x,a)
         enddo
      enddo
      write(unit_output,1500)
 1500 format(/////13x,'CROSS SECTION REPORT'/
     &     13x,'--------------------'///
     &     '                              (ft.)      (cfs)'/
     &     '         (ft.)     (ft.)     INITIAL    INITIAL'/
     &     ' XSECT   WIDTH   BOT. ELEV    STAGE       FLOW'/
     &     '------  -------  ---------   -------   ---------')
c-----1    192.0     -5.00       5.02        0.00
C-----1234567890123456789012345678901234567890123456789012345678901234567890'
      do i=1,max_xsects_tot
         if (xsect_geom(i).width .gt. 0.0)then
            write(unit_output,1520)i,xsect_geom(i).width,xsect_geom(i).botelv,
     &           xsect_geom(i).init_stage,xsect_geom(i).init_flow
 1520       format(i5,1x,f8.1,1x,f9.2,1x,f10.2,1x,f10.2)
         endif
      enddo

      if (print_level .ge. 5) then
         call geom_output
         call virt_output
         call check_dconveyance
         call check_area
      endif

      WRITE(unit_output,1600)
 1600 format(////,45x,'INPUT PATHS'/
     &     45x,'-----------'///
     &     ' NAME   TYPE         FILE NAME',
     &     '                                                DSS PATH NAME'/
     &     '------ ------ ----------------------------------------------------------',
     &     '   -----------------------------------')

      do i=1,ninpaths
         write(unit_output,1620)pathinput(i).label,pathinput(i).c_part
     &        ,pathinput(i).filename,pathinput(i).path
 1620    format(a6,1x,a6,1x,a60,1x,a40)
      enddo

      return
      end
