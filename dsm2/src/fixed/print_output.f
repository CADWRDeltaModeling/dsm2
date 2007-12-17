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

      subroutine print_output(istat)

c-----print the output
      use IO_Units
      use Gates, ONLY: gateArray, nGate, deviceTypeString, controlTypeString
      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'
      

      include '../hydrolib/network.inc'
      include '../hydrolib/netcntrl.inc'
      include '../hydrolib/chconnec.inc'

c-----Local variables

      integer
     &     istat                ! status of call (returned)
     &     ,i,j                 ! indices
     &     ,chan                ! channel numbers

      character
     &     objtype*4            ! string representing object type (reservoir, channel)              
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
     &     20x,'-------------------------------------------'//
     &     '                                                  NODE'/
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
     &     '                    CONNECTED                    # DEVICES' /
     &     '    NAME            WATER BODY       TO NODE    WEIR   PIPE' /
     &     '  ----------------  -------------    -------    -----  -----')

      do i=1,ngate
	   if (gateArray(i).objConnectedType.eq.obj_channel) then
	      objtype='CHAN'
	   else if (gateArray(i).objConnectedType. eq. obj_reservoir) then
	      objtype='RES '
	   end if
	    
	   if (gateArray(i).inUse) then
	      write(unit_output,1160)
     &           gateArray(i).name,objtype
     &          ,gateArray(i).objConnectedID
     &          ,gateArray(i).node
     &          ,gateArray(i).nDevice
 1160 format(a19,1x,a4,2x,i7,4x,i5,2x,i5,2x,i5)
		end if
	end do


      write(unit_output,1200)
 1200 format(///,8x,'GATE DEVICES'/
     &     8x,'------------'///
     &     8x,'(initial installations)    '//
     &     '                                                        #OF        (ft.)     (ft.)    (ft.)       FLOW COEFF.  '/
     &     ' GATE                                       GATE        DUPLICATE  WIDTH OR   BASE               TO      FROM  '/
     &     ' NAME(DEVICE)                    TYPE       CONTROL     DEVICES    RADIUS     ELEV    HEIGHT     NODE    NODE  '/
     &     '------------                     -----      -------     -----    ----------  -------  -------    ------  ---------')
      do i=1, ngate	   
         do j=1,gateArray(i).nDevice
c--------------Flow can occur through the gate
               write(unit_output,1220)
     &              trim(gateArray(i).name) //
     &                '(' //trim(gateArray(i).devices(j).name) // ')',
     &              trim(deviceTypeString(gateArray(i).Devices(j).structureType)),
     &              trim(controlTypeString(gateArray(i).Devices(j).controlType)),
     &              gateArray(i).Devices(j).nDuplicate,
     &              gateArray(i).Devices(j).maxWidth,
     &              gateArray(i).Devices(j).baseElev,
     &              gateArray(i).Devices(j).height,
     &              gateArray(i).Devices(j).flowCoefToNode,
     &              gateArray(i).Devices(j).flowCoefFromNode
 1220          format(a32,a6,2x,a12,1x,i4,2x,f13.2,1x,f9.2,1x,2f9.2,2f9.2)
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
         write(unit_output,1420)res_geom(i).name,res_geom(i).area,res_geom(i).stage,
     &        res_geom(i).botelv,res_geom(i).node_no(1),
     &        res_geom(i).coeff2res(1),res_geom(i).coeff2chan(1)
 1420    format(/a19,1x,f10.5,1x,f8.2,1x,f9.1,2x,i4,2x,f8.0,4x,f8.0,5x)
         do j=2,res_geom(i).nnodes
            write(unit_output,1440)res_geom(i).node_no(j),
     &           res_geom(i).coeff2res(j),res_geom(i).coeff2chan(j)
 1440       format(51x,i4,2x,f8.0,4x,f8.0,5x)
         enddo
      enddo

      if (print_level .ge. 5) then
         call geom_output
c-----todo: commented these out because they refer to hydro 
c           and the compiler for qual complains
c         call virt_output
c         call check_area
      endif

      WRITE(unit_output,1600)
 1600 format(////,45x,'INPUT PATHS'/
     &     45x,'-----------'///
     &     'NAME',29x,'TYPE',10x,'FILE NAME',70x,'DSS PATH',50x
     &     '---------------------------------------------------------------------------------------',
     &     '   ---------------------------------------')

      do i=1,ninpaths
         write(unit_output,1620)pathinput(i).name,pathinput(i).c_part
     &        ,pathinput(i).filename,pathinput(i).path
 1620    format(a30,1x,a16,1x,a80,1x,a50)
      enddo

      return
      end
