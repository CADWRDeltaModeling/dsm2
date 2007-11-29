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

      subroutine check_fixed_hydro(istat)

c-----Check the fixed input for omissions and errors before starting
c-----the model run.  Supply default values where possible.  Translate
c-----from nodes to channel numbers, and from external channel numbers
c-----to internal.  Write to FourPt arrays.

      implicit none

      include 'common.f'
      include 'common_irreg_geom.f'

      include '../../hydro/network.inc'
      include '../../hydro/netcntrl.inc'
      include '../../hydro/chconnec.inc'
      include '../../hydro/chnluser.inc'
      include '../../hydro/chcxtbl.inc'
      include '../time-varying/dss.inc'
      include '../time-varying/readdss.inc'
      include '../time-varying/writedss.inc'
      include '../time-varying/tide.inc'

c-----Local variables

      integer*4
     &     incr_intvl           ! increment julian minute by interval function

      integer
     &     istat                ! status of call (returned)
     &     ,i,j,kk,pth,nr       ! indices
     &     ,intchan,intchan2,extchan ! internal/external channels
     &     ,node,xs             ! node, xsect numbers
     &     ,neqstg              ! number of equal-stage boundaries
     &     ,nsumq               ! number of sum-of-flow boundaries
     &     ,lnblnk              ! intrinsic last non blank function
     &     ,dindex              ! function:  index of permanent data arrays
     &     ,di                  ! stores value of dindex
     &     ,veindex             ! temporary virtual elevation index
     &     ,channo              ! channel number--do loop counter
     &     ,vsecno              ! number of virt section within channel
     &     ,virtelev            ! index of the arrays in the irreg_geom_virt str
     &     ,nsec                ! number of sections based on deltax_requested
     &     ,loc,loccarr         ! character array string locator
     &     ,node2hydrochan      ! function to convert node to hydro channel
     &     ,nintnode(max_nodes) ! number of internal and external flows at nodes and reservoirs
     &     ,nintres(max_reservoirs)
     &     ,nextnode(max_nodes)
     &     ,nextres(max_reservoirs)

      integer*2 number          ! object number

      real*8
     &     totalweight          ! check quadrature weighting
     &     ,delx                ! actual distance between xsect
     &     ,dx_r                ! stores value of deltax_requested
     &     ,flow_coeff_adjust   ! adjustment factor for flow coefficients

      character cresnames(max_reservoirs)*20 ! reservoir names array
      character cinputnames(max_inputpaths)*20 ! input names array
     &     ,trans_from_names(max_translations)*20 ! vector of translation from names
      character*30 name         ! object name

      integer ii,prevchan,xsoldup,xsolddown
      integer USR, LNUM
      REAL*8    R23, R53 ,leng, distance,f1,f2
      PARAMETER (R23 = 2.0/3.0, R53 = 5.0/3.0)
      parameter(flow_coeff_adjust=0.75)

      logical firsttime(maxchannels),updefined,downdefined
      LOGICAL  InitGates,OK
      EXTERNAL InitGates

c-----statement function to calculate indices of virtual data arrays
      dindex(channo,vsecno,virtelev)
     &     =chan_index(channo) + (vsecno-1)*num_layers(channo) + virtelev-1

 602  format(/'Error opening/reading restart input file: ',a)

 605  format(/a,' date incorrect: ',a)

 607  format('Using restart file date: ',a)

 610  format(/'Quadrature weights do not add to 1.0:'
     &     /'No.  Point   Weight'
     &     /(i2,2f8.3/))

 612  format(/'Warning--Variable Density reset to false because'
     &     ' flow method is not Dynamic Wave.')

 614  format(/'Warning--Variable Sinuosity reset to true because'
     &     ' flow method is Dynamic Wave.')

 616  format(/'Warning--Variable Sinuosity reset to false because'
     &     ' flow method is Kinematic Wave.')

 630  format(/'Incorrect internal node boundary for node:',i5
     &     /'1 channel must be sum-of-flow boundary,'
     &     /'n-1 channels must be equal stage.')

 655  format(/'The following path was not found in the list of gates:'
     &     /a)

 660  format(/'No channel given: either incomplete gate specs, '
     &     /' misspelled gate name, or reservoir gate name not'
     &     /' found in reservoir names: ',a)

 670  format(/'Reservoir gate node number not found in reservoir list: ',i5)

 690  format(' Error.... Channel :',i5/
     &     '           As a minimum, X-Sections need to be specified'/
     &     '           at both ends of the channel;')
 700  format(' Error.... Channel :',i5/
     &     '           The flow area for the first level defined='/
     &     f15.5
     &     '           has to be zero. Zero Assumed')
 710  format(/'"',a,'" reservoir name: ',a,' not recognized.')

 715  format(/a,' object to object input path label not recognized.')

 720  format(/'MaxLocations too small (',i4,
     &     '), needs to be increased to'
     &     /'accomodate requested deltax (',i4,').')

 730  format(/'Too many stage boundaries, increase max_stgbnd: ',i2)

 735  format(/'Error: only one channel allowed to a fixed-stage node: ',
     &     i3)

 830  format(/'Error: too many ',a,' flows connected to ',a,' ',a)

      nquadpts=nquadpts-1

      if (time_step_intvl_hydro .ne. ' ') then
c--------time_step_intvl_hydro should be in form: '5min' or '1hour'
         time_step=incr_intvl(0,time_step_intvl_hydro,TO_BOUNDARY)
      else
         time_step=5            ! 5 minutes is default time step
      endif

c-----set FourPt values
      numch=nchans
      numuserloc=0
      timestep=0
      dt=time_step*60
      PrintLevel=print_level

      Restart_Read=io_files(hydro,io_restart,io_read).use
      Restart_Write=io_files(hydro,io_restart,io_write).use

c-----hydro binary output file interval
      if (io_files(hydro,io_tide,io_write).use) then
         HydroTimeInterval=incr_intvl(0,io_files(hydro,io_tide,io_write).
     &        interval,TO_BOUNDARY)
         if (mod(HydroTimeInterval,time_step) .EQ. 0) then
            Nsample=HydroTimeInterval/time_step
         else
            write(unit_output,'(a)') ' time_step=',time_step,' minutes'
            write(unit_output,'(a)') ' Tidefile Interval must be an'
            write(unit_output,'(a)') ' integer multiple of time_step'
            Nsample=12
            HydroTimeInterval=Nsample*time_step
            write(unit_output,'(a)') ' Tidefile Interval changed to: ',
     &           HydroTimeInterval
         endif
      endif

c-----repeating tide run
      if (repeating_tide) then
         if (io_files(hydro,io_tide,io_write).filename .eq. ' ') then
            write(unit_error,*)
     &           'Repeating tide requested, but hydro binary tide output filename blank.'
            goto 900
         endif
         if (io_files(hydro,io_restart,io_write).filename .eq. ' ') then
            write(unit_error,*)
     &           'Repeating tide requested, but hydro restart output filename blank.'
            goto 900
         endif
c--------turn off restart and tide file output until last cycle
         Restart_Write=.false.
         io_files(hydro,io_tide,io_write).use=.false.
c--------set default values
         if (max_tides .le. 1) max_tides=15
         if (repeat_stage_tol .le. 0.0) repeat_stage_tol=1.0e-3
         if (repeat_flow_tol .le. 0.0) repeat_flow_tol=2.0e-3
      endif

c-----don't use boundary equations
      do i=1,2*maxchannels+1
         eqnumber(i)=0
      enddo

c-----Quadrature Weighting
c-----supply default values if necessary
      if (nquadpts .eq. 0) then
         nquadpts=1
         quadpt(1)=0.5
         quadwt(1)=1.0
      endif
c-----check that weights total 1.0
      totalweight = 0.0
      do i=1,nquadpts
         totalweight = totalweight + quadwt(i)
      enddo
      if (abs(totalweight-1.0) .gt. 1.0e-6) then
         write(unit_error,610) (i,quadpt(i),quadwt(i),i=1,nquadpts)
         goto 900
      endif
      quadpts=nquadpts          ! FourPt variable

c-----1D open-channel flow method default
      if (
     &     terms .ne. 1 .and.
     &     terms .ne. 2 .and.
     &     terms .ne. 3
     &     ) terms=1            ! default to dynamic wave

c-----variable density and sinuousity defaults
      if (
     &     variabledensity .and.
     &     (terms .eq. 2 .or.
     &     terms .eq. 3)
     &     ) then
         variabledensity=.false.
         write(unit_error, 612)
      endif

      if (
     &     .not. variablesinuosity .and.
     &     variabledensity .and.
     &     terms .eq. 1
     &     ) then
         variablesinuosity=.true.
         write(unit_error, 614)
      endif

      if (
     &     variablesinuosity .and.
     &     terms .eq. 3
     &     ) then
         variablesinuosity=.false.
         write(unit_error, 616)
      endif

c-----Connectivity
c-----Rules:
c-----1) At least one node must be a specified-stage boundary.
c-----2) For internal nodes, 1 channel must be sum-of-flow boundary.
c-----3) For internal nodes, n-1 channels must be equal-stage boundary.
c-----4) For internal nodes, for number of connections count only
c!       stage-flow connections, not stage-stage.
c-----4) For external nodes, if no boundary type specified, assume flow.

c-----generate channel end connection types
*     FP Boundary-condition code       Meaning

*       water surface
*         1                         explicitly known
*         11                        equal to another water surface
*         31                        3-parameter relation
*         4                         self-setting (downstream only)
*
*       flow
*         2                         explicitly known
*         12                        sum of flows equal zero
*         32                        3-parameter relation

      nstgbnd=0
      do node=1,max_nodes
         if ((node_geom(node).nup + node_geom(node).ndown) .eq. 0) then
c-----------This node not connected to any channel...do nothing

         elseif ((node_geom(node).nup + node_geom(node).ndown) .eq. 1) then
c-----------external node
            if (node_geom(node).boundary_type .eq. flow_type .or.
     &           node_geom(node).boundary_type .eq. 0) then
c--------------specified-flow boundary type or none given, assume s-f
               node_geom(node).boundary_type=flow_type
               do i=1,node_geom(node).ndown
                  intchan=ext2int(node_geom(node).downstream(i))
                  downboundarycode(intchan)=2
               enddo
               do i=1,node_geom(node).nup
                  intchan=ext2int(node_geom(node).upstream(i))
                  upboundarycode(intchan)=2
               enddo
            else if (node_geom(node).boundary_type .eq. stage_type) then ! specified-stage bc
               do i=1,node_geom(node).ndown
                  intchan=ext2int(node_geom(node).downstream(i))
                  downboundarycode(intchan)=1
               enddo
               do i=1,node_geom(node).nup
                  intchan=ext2int(node_geom(node).upstream(i))
                  upboundarycode(intchan)=1
               enddo
c--------------set up stage boundary object structure
               nstgbnd=nstgbnd+1
               if (nstgbnd .gt. max_stgbnd) then
                  write(unit_error,730) max_stgbnd
                  goto 900
               endif
               stgbnd(nstgbnd).node=node
               stgbnd(nstgbnd).name=' '
c--------------search for input path with stage boundary at this node
               do i=1,ninpaths
                  if (pathinput(i).data_type .eq. stage_type .and.
     &                 pathinput(i).object .eq. obj_node .and.
     &                 pathinput(i).object_no .eq. node) then
                     stgbnd(nstgbnd).name=pathinput(i).label
                  endif
               enddo
            endif
         else                   ! internal node (multiple channels connected)
c-----------no fixed-stage nodes allowed internally
            if (node_geom(node).boundary_type .eq. stage_type) then
               write(unit_error,735) node
               goto 900
            endif
c-----------test that rules above are met; if no boundary condition set,
c-----------apply rules, if bc's are set but aren't correct, stop
            neqstg=0            ! number of bc's that are equal-stage
            nsumq=0             ! number of bc's that are sum-of-flow
            do i=1,node_geom(node).nup
               intchan=ext2int(node_geom(node).upstream(i))
               if (upboundarycode(intchan) .eq. 11) neqstg=neqstg+1
               if (upboundarycode(intchan) .eq. 12) nsumq=nsumq+1
            enddo
            do i=1,node_geom(node).ndown
               intchan=ext2int(node_geom(node).downstream(i))
               if (downboundarycode(intchan) .eq. 11) neqstg=neqstg+1
               if (downboundarycode(intchan) .eq. 12) nsumq=nsumq+1
            enddo
            if (neqstg .eq. 0 .and. nsumq .eq. 0) then
               if (node_geom(node).nup.ge.1) then
                  intchan=ext2int(node_geom(node).upstream(1))
                  upboundarycode(intchan)=12
                  do i=2,node_geom(node).nup
                     intchan=ext2int(node_geom(node).upstream(i))
                     upboundarycode(intchan)=11
                  enddo
                  do i=1,node_geom(node).ndown
                     intchan=ext2int(node_geom(node).downstream(i))
                     downboundarycode(intchan)=11
                  enddo
               else
                  intchan=ext2int(node_geom(node).downstream(1))
                  downboundarycode(intchan)=12
                  do i=2,node_geom(node).ndown
                     intchan=ext2int(node_geom(node).downstream(i))
                     downboundarycode(intchan)=11
                  enddo
               endif
            else if (neqstg .ne. 1 .and.
     &              nsumq .ne. node_geom(node).nup+node_geom(node).ndown-1) then
               write(unit_error,630) node
               goto 900
            endif
         endif
      enddo

c-----fill FourPt connection arrays
      do intchan=1,nchan_list
         extchan=int2ext(intchan)
         firsttime(intchan)=.true.
         upnumberofconnections(intchan)=0
         node=chan_geom(extchan).upnode
         if (node .gt. 0) then
            do j=1,node_geom(node).nup
               intchan2=ext2int(node_geom(node).upstream(j))
               if (upboundarycode(intchan) .ne. upboundarycode(intchan2)) then
                  upnumberofconnections(intchan)=upnumberofconnections(intchan)+1
                  upconnection((intchan-1)*maxconnectingchannels +
     &                 upnumberofconnections(intchan)) = intchan2
               endif
            enddo
            do j=1,node_geom(node).ndown
               intchan2=ext2int(node_geom(node).downstream(j))
               if (upboundarycode(intchan) .ne. downboundarycode(intchan2) .and.
     &              intchan .ne. intchan2) then
                  upnumberofconnections(intchan)=upnumberofconnections(intchan)+1
                  upconnection((intchan-1)*maxconnectingchannels +
     &                 upnumberofconnections(intchan)) = -intchan2
               endif
            enddo
         endif

         downnumberofconnections(intchan)=0
         node=chan_geom(extchan).downnode
         if (node .gt. 0) then
            do j=1,node_geom(node).ndown
               intchan2=ext2int(node_geom(node).downstream(j))
               if (downboundarycode(intchan) .ne. downboundarycode(intchan2)) then
                  downconnection((intchan-1)*maxconnectingchannels +
     &                 downnumberofconnections(intchan)) = intchan2
                  downnumberofconnections(intchan)=downnumberofconnections(intchan)+1
               endif
            enddo
            do j=1,node_geom(node).nup
               intchan2=ext2int(node_geom(node).upstream(j))
               if (downboundarycode(intchan) .ne. upboundarycode(intchan2) .and.
     &              intchan .ne. intchan2) then
                  downnumberofconnections(intchan)=downnumberofconnections(intchan)+1
                  downconnection((intchan-1)*maxconnectingchannels +
     &                 downnumberofconnections(intchan)) = intchan2
               endif
            enddo
         endif
      enddo

c-----fill the FourPt channel ID string

      do i=1,MaxLocations
         write(userlocationid(i),'(i5.5)') i
      enddo

      USR=0
      LNUM=0
      PreviousX   = -9
      PreviousBranch = 999
      PreviousH100   = 99999

c-----fill FourPt channel and related arrays and check validity
      do intchan=1,nchan_list
         extchan=int2ext(intchan)
         Lines(intchan)=2       ! means values given at two depths
         FirstTable(intchan) = USR+1
         numberofuserlocations(intchan)=0
         keepallcx(intchan) = .false. ! calculate values at junctions
         if (deltax_requested .gt. 0.) then
            dx(intchan)=deltax_requested
         else
            dx(intchan)=float(chan_geom(extchan).length)
         endif
         initialapprox(intchan)=0
         OneOverManning(intchan)=1./chan_geom(extchan).manning
         numberofuserlocations(intchan)=chan_geom(extchan).nxsect
         do j=1,chan_geom(extchan).nxsect
            numuserloc=numuserloc+1
            xs=chan_geom(extchan).xsect(j)
            userws(xs)=xsect_geom(xs).init_stage
            userq(xs)=xsect_geom(xs).init_flow
            USR = USR+1
            Datum(USR)=xsect_geom(xs).botelv
            Offset(USR) = LNUM+1
            XDistance(USR)=chan_geom(extchan).dist(j)
            write(ID(USR),'(i5.5)') xs
            if (j.eq.1) then
               upuserpointer(intchan)=xs
            elseif (j.eq.chan_geom(extchan).nxsect) then
               downuserpointer(intchan)=xs
            endif
            DO ii=1,2
               LNUM=LNUM+1
*--------------Assign hydraulic properties.
               IF (ii.eq.1) THEN
                  Depth(LNUM) = 0.
                  A(LNUM)     = 0.
                  P(LNUM)     = xsect_geom(xs).width
               ELSE
                  Depth(LNUM) = 100.
                  A(LNUM)     = 100.*xsect_geom(xs).width
                  P(LNUM)     = xsect_geom(xs).width + 200.
               ENDIF
               Width(LNUM) = xsect_geom(xs).width
               N(LNUM)     = 1./chan_geom(extchan).manning
               Bta(LNUM)   = 1.
               MA(LNUM)    = 1.
               MQ(LNUM)    = 1.
*--------------Compute one over effective n.
               IF (P(LNUM).GT.0.) THEN
                  K(LNUM)=1.486*A(LNUM)**R53/(P(LNUM)**R23*N(LNUM))
               else
                  K(LNUM)=0.0
               ENDIF

            ENDDO

            IF (USR+1.GT.MaxTables) THEN
               WRITE(unit_error,*) '***error (Check_fixed)'
               WRITE(unit_error,*) 'Maximum number of tables (',
     &              MaxTables,') exceeded.'
               goto 900
            ENDIF

            IF ((LNUM+Lines(intchan)+1) .GT. MaxLines) THEN
               WRITE(unit_error,*) '***error (Check_Fixed)'
               WRITE(unit_error,*) 'Maximum number of lines exceeded...'
               WRITE(unit_error,*) 'Cross section ',USR
               WRITE(unit_error,*) 'Current line number = ',LNUM
               goto 900
            END IF
         enddo
         LastTable(Intchan)=USR
      enddo

c-----Now process the generated irregular X-Sections

      updefined=.true.
      downdefined=.true.

      do intchan=1,nchan_list
         extchan=int2ext(intchan)
         do vsecno=1,num_virt_sec(extchan)
            if (firsttime(intchan)) then
c--------------first irregular x-section defined for this channel
c--------------This will override any previous (rectangular) specification
               firsttime(intchan)=.false.
               prevchan=intchan
               updefined=.false.
               downdefined=.false.
               lines(intchan) = num_layers(extchan) ! Number of levels defined
               NumberofUserLocations(intchan)=1
c--------------These are the x-sec # for the rectangular x-section
               xsoldup=chan_geom(extchan).xsect(1)
               xsolddown=chan_geom(extchan).xsect(chan_geom(extchan).nxsect)
            else
               NumberofUserLocations(intchan)=NumberofUserLocations(intchan)+1
            endif
            USR=USR+1
            if (USR.gt.MaxTables) then
 950           write(unit_error,*)' Error..  (check_fixed_hydro)'
               write(unit_error,*)' Too many irregular x-sections generated.'
               write(unit_error,*)' Increase Delta_x, or increase array dimension'
               write(unit_error,*)' Maxtables.   Current value=',MaxTables
               goto 900
            endif
            if (USR .gt. MaxLocations) then
               write(unit_error, 720) MaxLocations, nint(deltax_requested)
               goto 900
            endif

c-----------Datum should always be zero because min elev subtracted in readirreg
            Datum(USR)=virt_min_elev(minelev_index(extchan)+vsecno-1)
            j=NumberofUserLocations(intchan)
            Offset(USR)=LNUM+1
            leng=float(chan_geom(extchan).length)
            if (deltax_requested .eq. 0) then
               dx_r = chan_geom(extchan).length
            elseif (deltax_requested .ne. 0) then
               dx_r = deltax_requested
            endif
            if (float(chan_geom(extchan).length) .le. dx_r) then
               nsec=1
            elseif (float(chan_geom(extchan).length) .gt. dx_r) then
               nsec=int( float(chan_geom(extchan).length) /dx_r)
            endif
            delx=chan_geom(extchan).length/nsec
            distance=(vsecno-1)*delx/2
            XDistance(USR)=distance
            xs=USR
            write(ID(USR),'(i5.5)') xs
            if (XDistance(USR).eq.0.) then
               upuserpointer(intchan)=xs
               updefined=.true.
               FirstTable(intchan)=USR
               userws(xs)=userws(xsoldup)
               userq(xs)=userq(xsoldup)
            elseif (XDistance(USR).le.float(chan_geom(extchan).length)) then
               downuserpointer(intchan)=xs
               downdefined=.true.
               LastTable(intchan)=USR
               userws(xs)=userws(xsolddown)
               userq(xs)=userq(xsolddown)
            else
               f1=1. -distance/leng
               f2=1.-f1
               userws(xs)=f1*userws(xsoldup) + f2*userws(xsolddown)
               userq(xs)= f1*userq(xsoldup) + f2*userq(xsolddown)
            endif
            IF ((LNUM+Lines(intchan)+1) .GT. MaxLines) THEN
               WRITE(unit_error,*) '***error (check_fixed_hydro)'
               WRITE(unit_error,*) 'Maximum number of lines exceeded...'
               WRITE(unit_error,*) 'Current value MaxLines=',MaxLines
               WRITE(unit_error,*) 'Cross section ',USR
               WRITE(unit_error,*) 'Current line number = ',LNUM
               goto 900
            END IF

            do ii=1,num_layers(extchan)
c--------------calculate data array index for current layer
               di = dindex(extchan,vsecno,ii)
               LNUM=LNUM+1
c--------------if processing bottom layer
               veindex=elev_index(extchan)+ii-1
               if (ii.eq.1) then
                  IF (virt_area(di).ne.0.) then
                     write(unit_error,700)extchan,virt_area(di)
                     virt_area(di)=0.
                  ENDIF
                  Depth(LNUM)=0.
                  A(LNUM)=0.
               else
c-----------------virt_elevation is actually depth; it's converted in readirreg
                  Depth(LNUM)=virt_elevation(veindex)
                  A(LNUM)=virt_area(di)
               endif
               P(LNUM)=virt_wet_p(di)
               Width(LNUM)=virt_width(di)
               N(LNUM)     = 1./chan_geom(extchan).manning
               Bta(LNUM)   = 1.
               MA(LNUM)    = 1.
               MQ(LNUM)    = 1.
               IF (P(LNUM).GT.0.) THEN
                  K(LNUM)=1.486*A(LNUM)**R53/(P(LNUM)**R23*N(LNUM))
               ELSE
                  K(LNUM)=0.0
               endif
            enddo
         enddo
      enddo

c-----convert reservoir info for fourpt variables
      do i=1, MaxChannels
         ReservoirFlag(i)=0
      enddo

      do i=1,max_reservoirs
c--------channel connections to this reservoir
         if (res_geom(i).number .gt. 0) then
            Ares(res_geom(i).number)=res_geom(i).area * 1.0e6
            Yres(res_geom(i).number)=res_geom(i).stage
            Dres(res_geom(i).number)=res_geom(i).botelv
            do j=1,res_geom(i).nnodes
               node=res_geom(i).node_no(j)
               NconnectReservoir(res_geom(i).number)=
     &              NconnectReservoir(res_geom(i).number)+1
c--------------assign connection to a fourpt sum-of-flow channel
               intchan=0
               do kk=1,node_geom(res_geom(i).node_no(j)).nup
                  intchan=ext2int(node_geom(node).upstream(kk))
                  if (upboundarycode(intchan) .eq. 12) goto 501
               enddo
               do kk=1,node_geom(res_geom(i).node_no(j)).ndown
                  intchan=-ext2int(node_geom(node).downstream(kk))
                  if (downboundarycode(abs(intchan)) .eq. 12) goto 501
               enddo
 501           continue
               ResConnectingChannels(res_geom(i).number,j)=intchan
            enddo
            do j=1,NconnectReservoir(res_geom(i).number)
               ReservoirCoeff(res_geom(i).number,j,1)=res_geom(i).coeff2res(j) *
     &              sqrt(2.*gravity)*flow_coeff_adjust
               ReservoirCoeff(res_geom(i).number,j,2)=res_geom(i).coeff2chan(j) *
     &              sqrt(2.*gravity)*flow_coeff_adjust
               ReservoirFlag(abs(ResConnectingChannels(res_geom(i).number,j)))=1
            enddo
         endif
      enddo

c-----convert gate info to fourpt variables
      NumGatesOperating = 0
      NumSpecialGates = 0
      ngate=0
      do i=1,max_gates
         if (lnblnk(gate_geom(i).oper) .ne. 0 .and.
     &        gate_geom(i).oper(1:3) .ne. 'ign') then ! operating gate
            ngate=ngate+1
            NumberofGatesFraction(ngate)=1.0 ! updated in netbnd.f
            if (gate_geom(i).oper .ne. 'calc') then
               NumGatesOperating = NumGatesOperating + 1
               ListGateOperating(NumGatesOperating)=ngate
c--------------make sure that the corresponding pathinput object is gate
c--------------(might have been set to reservoir)
               do j=1,ninpaths
                  if (pathinput(j).label .eq. gate_geom(i).name .and.
     &                 pathinput(j).data_type .eq. gate_type) then
                     pathinput(j).object=obj_gate
                     pathinput(j).object_no=i
                  endif
               enddo
               if (gate_geom(i).oper(1:2) .eq. 'op') then ! gate always open
                  GatePosition(ngate)=GATE_OPEN
                  gate_geom(i).lapse=0
               elseif (gate_geom(i).oper(1:2) .eq. 'cl') then ! gate always closed
                  GatePosition(ngate)=GATE_CLOSE
                  gate_geom(i).lapse=0
               elseif (gate_geom(i).oper(1:2) .eq. 'fr') then ! gate always free
                  if (gate_geom(i).widthfree .eq. miss_val_r .or.
     &                 gate_geom(i).crestfree .eq. miss_val_r) then
                     write(unit_error, 619)
     &                    gate_geom(i).name(:lnblnk(gate_geom(i).name))
 619                 format(/'FREE operation specified in fixed input for gate ',a
     &                    ' but either the free width or depth were not given in the input.')
                     call exit(2)
                  endif
                  GatePosition(ngate)=GATE_FREE
                  gate_geom(i).lapse=0
               endif
            else if (gate_geom(i).oper .eq. 'calc') then
               DeltaHOpen(ngate)=gate_geom(i).dhopen
               VelocityClose(ngate)=gate_geom(i).velclose
               GateOperatingTime(ngate)=start_julmin
               GateSpecialPos(ngate)=GATE_CALC
               NumSpecialGates = NumSpecialGates + 1
               ListSpecialGates(NumSpecialGates)=ngate
            endif
            hydrogates(i)=ngate
            if (gate_geom(i).chan_no .le. 0) then ! gate for reservoir
c--------------which reservoir?
               do j=1,max_reservoirs
                  if (res_geom(j).name .eq. gate_geom(i).name) goto 200
               enddo
 200           continue
               if (j .gt. max_reservoirs) then
                  write(unit_error,660) gate_geom(i).name
                  goto 900
               endif
               nr=j
c--------------which node?
               do j=1,res_geom(nr).nnodes
                  if (res_geom(nr).node_no(j) .eq. gate_geom(i).node_no) goto 210
               enddo
 210           continue
               if (j .gt. res_geom(nr).nnodes) then
                  write(unit_error,670) gate_geom(i).node_no
                  goto 900
               endif
               ReservoirGate(res_geom(nr).number,j)=ngate
               GateChan(ngate)=-1
            else                ! normal channel gate
               GateChan(ngate)=ext2int(gate_geom(i).chan_no)
c--------------check for valid channel number
               if (GateChan(ngate) .le. 0) then
                  write(unit_error, *) 'Gate ',gate_geom(i).
     &                 name(:lnblnk(gate_geom(i).name)),
     &                 ' has an invalid channel number:',gate_geom(i).chan_no
                  goto 900
               endif
            endif
            GateSeaCoef_Weir(ngate)=gate_geom(i).coeffweirdown * sqrt(2.*gravity)
            GateLandCoef_Weir(ngate)=gate_geom(i).coeffweirup * sqrt(2.*gravity)
            GateLapseTime(ngate)=gate_geom(i).lapse
            WidthWeirSea(ngate)=gate_geom(i).widthdown
            WidthWeirLand(ngate)=gate_geom(i).widthup
            WidthFree(ngate)=gate_geom(i).widthfree
            DepthWeirCrest(ngate)=gate_geom(i).crestelev
            DepthFree(ngate)=gate_geom(i).crestfree
            NumberofPipes(ngate)=gate_geom(i).npipes
            PipeRadius(ngate)=gate_geom(i).piperad
            DepthInvertPipe(ngate)=gate_geom(i).pipeelev
            GateSeaCoef_Pipe(ngate)=gate_geom(i).coeffpipedown * sqrt(2.*gravity)
            GateLandCoef_Pipe(ngate)=gate_geom(i).coeffpipeup * sqrt(2.*gravity)
            if (gate_geom(i).loc .eq. 'up') then ! upstream end of channel
               GateNumber(GateChan(ngate),1)=ngate
               GateLocation(ngate)=1
            elseif (gate_geom(i).loc .eq. 'down') then ! downstream end of channel
               GateNumber(GateChan(ngate),2)=ngate
               GateLocation(ngate)=2
            endif
         endif
      enddo

C-----Modify Condition Codes for channels with gates
      OK = InitGates()

c-----fill translation name vector
      do i=1,max_translations
         trans_from_names(i)=translations(i).from_name
      enddo

      do pth=1,ninpaths
c--------convert node flow and stage inputs to a hydro channel
c--------skip if input path is just a replacement
         if (pathinput(pth).priority .gt. 1) goto 100

         if (pathinput(pth).object .eq. obj_node) then
            node=pathinput(pth).object_no
            pathinput(pth).hydrochan=node2hydrochan(node,
     &           pathinput(pth).data_type .eq. flow_type)
         endif
c--------set external flow index
         if (pathinput(pth).data_type .eq. flow_type .and.
     &        pathinput(pth).object .ne. obj_obj2obj) then ! source/sink for node
            nqext=nqext+1
            qext(nqext).obj_name=pathinput(pth).label
            qext(nqext).in_no=pth
            qext(nqext).attach.object=pathinput(pth).object
            qext(nqext).attach.obj_name=pathinput(pth).label
            qext(nqext).attach.object_no=pathinput(pth).object_no
            qext(nqext).acct_name=pathinput(pth).acct_name
            qext(nqext).mass_frac=pathinput(pth).mass_frac
c-----------for node, use node number for object name
            if (qext(nqext).attach.object .eq. obj_node) then
               write(qext(nqext).attach.obj_name,'(i3)')
     &              qext(nqext).attach.object_no
            endif
c-----------translate the path label for a reservoir
            if (qext(nqext).attach.object .eq. obj_reservoir .and.
     &           qext(nqext).attach.obj_name .ne. ' ') then
               loc=loccarr(qext(nqext).attach.obj_name,trans_from_names,
     &              max_translations,EXACT_MATCH)
               if (loc .gt. 0) then
                  qext(nqext).attach.obj_name=translations(loc).obj_name
               endif
            endif
c-----------external flow object name and accounting label empty?
            if (qext(nqext).obj_name .eq. ' ' .and.
     &           qext(nqext).acct_name .eq. ' ') then
               write(unit_error,815) 'external',
     &              obj_names(qext(nqext).attach.object)
     &              (:lnblnk(obj_names(qext(nqext).attach.object))),
     &              qext(nqext).attach.obj_name
     &              (:lnblnk(qext(nqext).attach.obj_name))
 815           format(/'Warning: 'a,' flow at ',a,' ',a,
     &              ' does not have an ID name or accounting name.')
            endif
         endif
 100     continue
      enddo

c-----convert obj2obj node flow to a hydro channel
      do i=1,nobj2obj
         if (obj2obj(i).from.object .eq. obj_node) then
            node=obj2obj(i).from.object_no
            obj2obj(i).from.hydrochan=node2hydrochan(node,.true.)
         endif
         if (obj2obj(i).to.object .eq. obj_node) then
            node=obj2obj(i).to.object_no
            obj2obj(i).to.hydrochan=node2hydrochan(node,.true.)
         endif
      enddo

c-----fill in reservoir numbers for object-to-object connections with
c-----reservoirs, and input path indices for labeled connections
      do i=1,max_reservoirs
         cresnames(i)=res_geom(i).name
      enddo

      do i=1,ninpaths
         if (pathinput(i).label .ne. ' ') then
            cinputnames(i)=pathinput(i).label
         else
            cinputnames(i)='w@e2l#rk%jsl' ! loccarr stops on blank or miss_val_c
         endif
      enddo
      if (ninpaths .lt. max_inputpaths) cinputnames(ninpaths+1)=' '

      do i=1,nobj2obj
c--------from reservoir name
         if (obj2obj(i).from.object .eq. obj_reservoir) then
            loc=loccarr(obj2obj(i).from.obj_name,cresnames,max_reservoirs,
     &           EXACT_MATCH)
            if (loc .le. 0) then
               write(unit_error, 710) 'from', obj2obj(i).from.obj_name
     &              (:lnblnk(obj2obj(i).from.obj_name))
               goto 900
            else
               obj2obj(i).from.object_no=loc
            endif
         endif
c--------to reservoir name
         if (obj2obj(i).to.object .eq. obj_reservoir) then
            loc=loccarr(obj2obj(i).to.obj_name,cresnames,max_reservoirs
     &           ,EXACT_MATCH)
            if (loc .le. 0) then
               write(unit_error, 710) 'to', obj2obj(i).to.obj_name
     &              (:lnblnk(obj2obj(i).to.obj_name))
               goto 900
            else
               obj2obj(i).to.object_no=loc
            endif
         endif

         if (obj2obj(i).label .ne. ' ') then ! find pathinput label
            loc=loccarr(obj2obj(i).label,cinputnames,max_inputpaths,
     &           EXACT_MATCH)
            if (loc .le. 0) then
               write(unit_error, 715) obj2obj(i).label
     &              (:lnblnk(obj2obj(i).label))
               goto 900
            else                ! input path label found
               obj2obj(i).in_no=loc
c--------------adopt accounting labels from input path,
c--------------if not already given in obj-to-obj input section
               if (obj2obj(i).from.acct_name .eq. ' ') then
                  obj2obj(i).from.acct_name=pathinput(loc).acct_name
               endif
               if (obj2obj(i).to.acct_name .eq. ' ') then
                  obj2obj(i).to.acct_name=pathinput(loc).acct_name
               endif
            endif
         else
c-----------internal flow value not from input path
            obj2obj(i).flow=obj2obj(i).constant_value
         endif

c--------check if all internal flows have an accounting label
         if (obj2obj(i).from.acct_name .eq. ' ') then
            write(unit_error,815) 'internal-from',
     &           obj_names(obj2obj(i).from.object)
     &           (:lnblnk(obj_names(obj2obj(i).from.object))),
     &           obj2obj(i).from.obj_name
     &           (:lnblnk(obj2obj(i).from.obj_name))
         endif
         if (obj2obj(i).to.acct_name .eq. ' ') then
            write(unit_error,815) 'internal-to',
     &           obj_names(obj2obj(i).to.object)
     &           (:lnblnk(obj_names(obj2obj(i).to.object))),
     &           obj2obj(i).to.obj_name
     &           (:lnblnk(obj2obj(i).to.obj_name))
         endif
      enddo

c-----generate an index of which internal flows
c-----correspond to which nodes and reservoirs;
      do i=1,max_nodes
         nintnode(i)=0
         nextnode(i)=0
      enddo
      do i=1,max_reservoirs
         nintres(i)=0
         nextres(i)=0
      enddo
      do i=1,nobj2obj
         number=obj2obj(i).from.object_no
         name=obj2obj(i).from.obj_name
         if (obj2obj(i).from.object .eq. obj_node) then
            if (nintnode(number) .lt. max_qobj) then
               nintnode(number)=nintnode(number)+1
            else
               write(unit_error,830) 'internal','node',
     &              name(:lnblnk(name))
               call exit(2)
            endif
            node_geom(number).qint(nintnode(number))=i
         endif
         if (obj2obj(i).from.object .eq. obj_reservoir) then
            if (nintres(number) .lt. max_qobj) then
               nintres(number)=nintres(number)+1
            else
               write(unit_error,830) 'internal','reservoir',
     &              name(:lnblnk(name))
               call exit(2)
            endif
            res_geom(number).qint(nintres(number))=i
         endif

c--------to object
         number=obj2obj(i).to.object_no
         name=obj2obj(i).to.obj_name
         if (obj2obj(i).to.object .eq. obj_node) then
            if (nintnode(number) .lt. max_qobj) then
               nintnode(number)=nintnode(number)+1
            else
               write(unit_error,830) 'internal','node',
     &              name(:lnblnk(name))
               call exit(2)
            endif
            node_geom(number).qint(nintnode(number))=i
         endif
         if (obj2obj(i).to.object .eq. obj_reservoir) then
            if (nintres(number) .lt. max_qobj) then
               nintres(number)=nintres(number)+1
            else
               write(unit_error,830) 'internal','reservoir',
     &              name(:lnblnk(name))
               call exit(2)
            endif
            res_geom(number).qint(nintres(number))=i
         endif

      enddo

c-----generate an index of which external flows
c-----correspond to which nodes and reservoirs;
      do i=1,nqext
         number=qext(i).attach.object_no
         name=qext(i).attach.obj_name
         if (qext(i).attach.object .eq. obj_node) then
            if (nextnode(number) .lt. max_qobj) then
               nextnode(number)=nextnode(number)+1
            else
               write(unit_error,830) 'external','node',
     &              name(:lnblnk(name))
               call exit(2)
            endif
            node_geom(number).qext(nextnode(number))=i
         endif
         if (qext(i).attach.object .eq. obj_reservoir) then
            if (nextres(number) .lt. max_qobj) then
               nextres(number)=nextres(number)+1
            else
               write(unit_error,830) 'external','reservoir',
     &              name(:lnblnk(name))
               call exit(2)
            endif
            res_geom(number).qext(nextres(number))=i
         endif

      enddo

      call print_output(istat)  ! echo fixed output

      return

 900  continue                  ! here for fatal error

      istat=-2
      return

      end

      integer function node2hydrochan(node,data_flow_type)
      
c-----convert a DSM2 node number to a hydro connecting channel,
c-----depending on input data type (flow or stage)

      implicit none

c-----arguments
      integer node              ! DSM2 node number [INPUT]
      logical data_flow_type    ! true if input is flow type [INPUT]

c-----include files
      include 'common.f'

      include '../../hydro/network.inc'
      include '../../hydro/netcntrl.inc'
      include '../../hydro/chconnec.inc'
      include '../../hydro/chnluser.inc'
      include '../../hydro/chcxtbl.inc'
      
c-----local variables
      integer j                 ! index

c-----flow inputs must go to a hydro channel with a flow condition code
c-----(2 or 12)
      if (node_geom(node).boundary_type .eq. stage_type) then ! specified-stage boundary condition
c--------cannot have flow to fixed stage node
         if (data_flow_type) then
            write(unit_error, 610) node
 610        format(/'Error: cannot have external or internal flow'
     &           /'at specified-stage node: ',i3)
            call exit(2)
         endif
         if (node_geom(node).ndown .gt. 0) then
            node2hydrochan=-ext2int(node_geom(node).downstream(1))
         else
            node2hydrochan=ext2int(node_geom(node).upstream(1))
         endif
      endif
      
      if (data_flow_type) then  ! node flow input
         node2hydrochan=0
c--------check upstream channel end connections to node first...
         j=1
         do while (j .le. node_geom(node).nup .and.
     &        (upboundarycode(ext2int(node_geom(node).upstream(j))) .ne. 12
     &        .and.
     &        upboundarycode(ext2int(node_geom(node).upstream(j))) .ne. 2))
            j=j+1
         enddo
         if (j .le. node_geom(node).nup) then
            node2hydrochan=ext2int(node_geom(node).upstream(j))
         else                   ! 2 or 12 is on downstream channel end to node
            j=1
            do while (j .le. node_geom(node).ndown .and.
     &           (downboundarycode(ext2int(node_geom(node).downstream(j))) .ne. 12
     &           .and.
     &           downboundarycode(ext2int(node_geom(node).downstream(j))) .ne. 2))
               j=j+1
            enddo
            if (j .le. node_geom(node).ndown) then
               node2hydrochan=-ext2int(node_geom(node).downstream(j))
            endif
         endif
      endif

      return
      end
