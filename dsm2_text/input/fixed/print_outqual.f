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


      subroutine print_outqual(istat)

c-----print the output

      implicit none

      include 'common.f'

c      include '../../hydro/network.inc'
      include '../../qual/param.inc'
      include '../../qual/bltm1.inc'
      include '../../qual/bltm3.inc'
      include '../../qual/bltm2.inc'
      include '../time-varying/dss.inc'
      include '../time-varying/readdss.inc'
      include '../time-varying/writedss.inc'

c-----Local variables

      integer
     &     istat                ! status of call (returned)
     &     ,i                   ! index
     &     ,chan                ! channel numbers
     &     ,lnblnk              ! last-non-blank intrinsic
     &     ,l                   ! constituent no.

c-----copyright notices
      write(unit_output, 805)
      write(unit_screen, 805)
 805  format(
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

c-----1D Lagrangian Transport Modeling of Water Quality


      write(unit_output, 810) crdt14
 810  format(//'   1D LAGRANGIAN TRANSPORT MODELING OF WATER QUALITY',/
     &'   -------------------------------------------------',///
     &     'Run date and time: ', a/)

      l=1
      do i=1,ntitles
         write(unit_output, '(a)') title(i)(1:lnblnk(title(i)))
      enddo

      if (run_length .ne. ' ') then
c--------run length should be in form: '20hour' or '5day'
         Write(unit_output,900) run_length(1:lnblnk(run_length)),time_step
 900     format(/' Model run length: ',a/
     &        ' Time-Step=',i5,' minutes')
      else                      ! start/end char dates given
         Write(unit_output,920) run_start_dt,run_end_dt,time_step
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
     &     '  CHANNEL      (ft)                  DISP.    CONNECTIVITY      (mg/l)'/
     &     '  ID  SEQ     LENGTH     MANNING    FACTOR      DOWN   UP       INITIAL CONC.'/
     &     '---------  ----------   --------   --------   ------------      ----------')

      do chan=1,max_channels
         if (chan_geom(chan).length .gt. 0) then
c-----------upstream node
            write(unit_output,1080)chan,ext2int(chan),chan_geom(chan).length,
     &           chan_geom(chan).manning,chan_geom(chan).disp,
     &           chan_geom(chan).downnode,chan_geom(chan).upnode,
     &           (cstart(ext2int(chan),l),l=1,neq)
 1080       format(i4,1x,i4,2x,i8,5x,f8.4,3x,f8.4,5x,i4,1x,i4,6x,11(1x,f8.2))
         endif
      enddo

      write(unit_output,1400)

 1400 FORMAT(////,'                         RESERVOIRS',/
     &       '                         ----------'///
     &       '               6'/
     &       '            x10            (ft.)      (mg/l)'/
     &       '           (Sqft)         BOTTOM      INITIAL'/
     &       ' NAME       AREA           ELEV.    CONCENTRATION'/
     &       '------   ---------       --------   -------------'/)

      do i=1,nres
         write(unit_output,1420)res_geom(i).name,res_geom(i).area,
     &        res_geom(i).botelv,(cres(i,l),l =1,neq)
 1420 format(/a,2x,f11.5,2x,f10.1,5x,15f9.2)
      enddo

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
