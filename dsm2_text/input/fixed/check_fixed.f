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

      subroutine check_fixed(istat)

c-----Check the fixed input for omissions and errors before starting
c-----the model run.  Supply default values where possible.  Translate
c-----from nodes to channel numbers, and from external channel numbers
c-----to internal.

      implicit none

      include 'common.f'
      include 'common_ptm.inc'
      include 'common_irreg_geom.f'

      include '../time-varying/dss.inc'
      include '../time-varying/readdss.inc'
      include '../time-varying/writedss.inc'
      include '../time-varying/common_tide.f'
      include '../../hydro/network.inc'
      include '../../hydro/chconnec.inc'

c-----Local variables

      logical
     &     constituent_input(max_constituent) ! true if an input path was found for constituent
     &     ,uniq_constituent    ! function to determine if this constituent is unique
     &     ,ncc                 ! function to determine if this constituent is non-conservative
     &     ,lstat               ! logical status

      integer
     &     istat                ! status of call (returned)
     &     ,ios                 ! i/o status
     &     ,i,j,k,p             ! indices
     &     ,intchan,extchan     ! internal/external channel numbers
     &     ,node,xs             ! node, xsect numbers
     &     ,nlen                ! character string length
     &     ,lnblnk              ! intrinsic last non blank function
     &     ,na,nb,nc,nd,ne,nf   ! DSS path parts
     &     ,ntides              ! number of tide cycles
     &     ,loc,loccarr         ! array index; function to find string in char array
     &     ,data_types          ! function to determine type of data (stage, flow,...)
     &     ,number              ! number prefix to DSS E part
     &     ,dsm_gateno          ! DSM2 gate number
     &     ,hydro_gateno        ! internal Hydro gate number

      integer*4
     &     cdt2jmin             ! character date/time to julian minute
     &     ,incr_intvl          ! increment julian minute by interval function
     &     ,jmin                ! julian minute

      integer
     &     holdgrp(max_chanres) ! hold group numbers
     &     ,advance             ! added to the grpindx
     &     ,adjpathout          ! adjusted pathoutput (original npathouts + ngroups)
     &     ,outindx             ! holds value of original npathouts
     &     ,itmp                ! index

      external data_types

      integer
     &     max_nc               ! max number of non-constituents
     &     ,n_required_do       ! number of required NCCs to model DO
     &     ,n_required_algae    ! number of required NCCs to model algea

      parameter(
     &     max_nc=9
     &     ,n_required_do=7
     &     ,n_required_algae=7
     &     )

      character
     &     cdist*10             ! channel distance
     &     ,modifier*32         ! DSS f_part
     &     ,ctmp*150            ! temporary string
     &     ,ca*32,cb*32,cc*32,cd*32,ce*32,cf*32 ! DSS path parts
     &     ,path*(6*32)         ! temp DSS pathname
     &     ,trans_from_names(max_translations)*32 ! vector of translation from names
     &     ,res_names(max_reservoirs)*32 ! vector of reservoir names
     &     ,const_names(max_constituent)*32 ! vector of constituent names
     &     ,diff2dates*14       ! return DSS date given start and diff
     &     ,jmin2cdt*14         ! julian minute to char function
     &     ,path_constituent*15 ! constituent name for this path
     &     ,non_constituents(max_nc)*10 ! list of inputs that are not constituents
     &     ,required_do(n_required_do)*32 ! list of NCCs required for DO
     &     ,required_algae(n_required_algae)*32 ! list of NCCs required for algae
     &     ,uniq_c_list(max_constituent)*32 ! list of unique C parts in DSS outputpaths
     &     ,interval*15         ! character part of DSS E part
     &     ,grpnum*3            ! string conversion of group number

      data
     &     constituent_input / max_constituent * .false. /
     &     ,uniq_c_list / max_constituent * ' ' /
     &     ,const_names / max_constituent * ' ' /
     &     ,non_constituents /
     &     'stage ',
     &     'flow ',
     &     'flow-net ',
     &     'pump ',
     &     'vel ',
     &     'cloud ',
     &     'dryblb ',
     &     'wind ',
     &     'atmpr '
     &     /

 601  format(/'Error opening output file:',a
     &     /'Status value: ',i3)

 602  format(/'Error opening/reading restart input file: ',a)

 605  format(/a,' date incorrect: ',a)

 603  format('Using ',a,' file date to start run: ',a)

 606  format(/'Invalid ',a,' date of ',a,' in tidefile:'
     &     /a)

 607  format(/'max_translations parameter must be set bigger;'
     &     /'Need ',i3, ' currently ',i3)

 608  format(/'Number of channels in CHANNEL section:',i4
     &     /'not equal to number given in LIST_CHAN section:',i4)

 609  format(/'The ',a,' name ''',a,''' cannot be used as a translation name.')

 610  format(/'No ',a,' with name: ',a)

 620  format(/'Flow output requested for reservoir ',a,
     &     'but no node was specified.')

 630  format(/'For reservoir ',a,' invalid node specified'
     &     /'for flow output: ',i5)

 635  format(/'Output pathname'
     &     / a /
     &     'has an incorrect channel distance: ',i6)

 640  format(/'Cross-section',i4,' at channel',i4,
     &     /'did not have an initial value entered for stage and flow.')

 642  format(/a,i4,' does not have a name, channel, node or reservoir.')

 644  format(/a,' name ',a,' does not have a translation.')

 646  format(/'Input line: '4(a,' ')/' has only a channel/distance translation.')

 649  format(/'No name, node, or channel number for this input path'
     &     ,i3)

 651  format(/'Number of input paths for ',a
     &     ,' exceed allowable limit:',i4)

 652  format(/'Number of output paths for ',a
     &     ,' exceed allowable limit:',i4)

 670  format(/'Output line: '4(a,' ')
     &     /'is given for chan/dist and node (',i3,'); node output used.')

 671  format(/'Output line: '4(a,' ')
     &     /'is not valid at a node (',i3,'); use channel/distance instead.')

 650  format(/'Unrecognized ',a,' data interval: ',i4,1x,a)

 680  format(/'Error.... Channel :',i5/
     &     'First and last X-Sections specified must'/
     &     'correspond to the two ends of the channel')

 685  format(/'Error: data syncronization requested for this path '
     &     /a/'but the time interval is not minutes, hours, day, or months.')

 690  format(/'Warning: the following input path is not being used,'
     &     /'perhaps because no output was requested of its constituent,'
     &     /'or because a constituent translation is needed.'/a)

 695  format(/'Error: Maximum number of unique accounting names exceeded:',i3)

 697  format(/'Error: 'a,' accounting name ''',a,''''
     &     /' was not given in the TYPE input section.')

 698  format(/'Error: warmup run requested but first tidefile ',
     &     'is not a repeating tide.')

 705  format(' Error...flow output at reservoir ',a,
     &     ' is at incorrect node: ',i3)

c-----check if output filename supplied, if not use default
      if (output_filename .eq. ' ') then
         output_filename='DSM2-' // dsm2_name
      endif

c-----open output file(s)
      open (
     &     unit=unit_output     ! text output for run
     &     ,file=output_filename
     &     ,iostat=ios
     &     ,err=901
     &     )

c-----adjust totals
      ninpaths=ninpaths-1
      noutpaths=noutpaths-1
      nprints=nprints-1
      nchan_list=nchan_list-1

c-----generic date in julian minutes
      jul_generic_dt=cdt2jmin(generic_dt)

c-----run start date and time can be a DSS date (e.g. 01jan1994 0100),
c-----or 'restart' (use date from restart file), or
c-----'tide' (use date from tidefile)
      if (run_start_dt .eq. ' ' .or.
     &     index(run_start_dt,'gen')) then
         run_start_dt=generic_dt
      else if (run_start_dt(1:3) .eq. 'res' .or.
     &        run_start_dt(11:13) .eq. 'res') then
c--------get run_start_dt from restart file
         if (io_files(dsm2_module,io_restart,io_read).filename
     &        .eq. ' ') then    ! no restart file specified
            write(unit_error, *)
     &           'Error-No restart filename given, but restart run start time requested.'
            goto 900
         endif
         open (
     &        unit=io_files(dsm2_module,io_restart,io_read).unit
     &        ,file=io_files(dsm2_module,io_restart,io_read).filename
     &        ,err=905
     &        )
         read(io_files(dsm2_module,io_restart,io_read).unit,
     &        '(36x,a14)',err=905) run_start_dt
c--------check for old or new restart version
         if (run_start_dt .eq. ' ') then ! new version
            read(io_files(dsm2_module,io_restart,io_read).unit,
     &           '(36x,a14)',err=905) run_start_dt
         endif
         close(io_files(dsm2_module,io_restart,io_read).unit)
         write(unit_screen,603) 'restart', run_start_dt
      else if (run_start_dt(1:3) .eq. 'tid' .or.
     &        run_start_dt(11:13) .eq. 'tid') then
         if (dsm2_module .eq. hydro) then
c-----------can't use tidefile from hydro
            write(unit_error,*) 'Error-Run start date requested from tidefile'
            goto 900
         else                   ! Qual or PTM
c-----------get run_start_dt from tidefile
            if (tide_files(1).filename .eq. ' ') then
               write(unit_error, *)
     &              'Error-no tidefiles given, but tidefile run start time requested.'
               goto 900
            endif
            call get_tidefile_dates(1)
            run_start_dt=jmin2cdt(tide_files(1).start_julmin_file)
            write(unit_screen,603) 'tide', run_start_dt
         endif
      else                      ! assume DSS style start date/time
      endif

c-----correct start date for odd minutes (not multiple of 15 minutes)
      start_julmin=cdt2jmin(run_start_dt)
      start_julmin=(start_julmin/15)*15

c-----calculate ending time if run length, rather than
c-----start/end times are given
      if (run_length .ne. ' ') then
c--------run length should be in form: '20hour' or '5day'
         run_end_dt=diff2dates(run_start_dt,run_length)
      endif                     ! start/end char dates given
      end_julmin=cdt2jmin(run_end_dt)

c-----check validity of start and end julian minutes
      if (start_julmin .ge. end_julmin) then
         write(unit_error,*) 'Starting date equal to or after ending date.'
         goto 900
      endif
      if (start_julmin .eq. miss_val_i) then
         write(unit_error,605) 'Starting',run_start_dt
         goto 900
      endif
      if (end_julmin .eq. miss_val_i) then
         write(unit_error,605) 'Ending',run_end_dt
         goto 900
      endif

c-----warning fix, until scalar variables fixed
      cont_missing=cont_missing .and. cont_bad

c-----check for minimum required input, calculate totals
      nchans=0
      do i=1,max_channels
         if (chan_geom(i).length .gt. 0)
     &        nchans=nchans+1
      enddo

      if (nchans .ne. nchan_list) then
         write(unit_error,608) nchans,nchan_list
         goto 900
      endif

      nxsects=0
      do i=1,max_xsects_tot
         if (xsect_geom(i).width .gt. 0.0)
     &        nxsects=nxsects+1
      enddo

      if (tide_cycle_length .ne. ' ') then
c--------tide_cycle_length should be in form: '1hour' or '5day'
         tide_cycle_length_mins=incr_intvl(0,tide_cycle_length,
     &        IGNORE_BOUNDARY)
      else
         tide_cycle_length_mins=25*60
      endif
      if (tide_cycle_length_mins .le. 0) tide_cycle_length_mins=25*60

c-----construct upstream and downstream nodes for DSM channels
      do intchan=1,nchans
         extchan=int2ext(intchan)
c--------upstream node
         node=chan_geom(extchan).upnode
         node_geom(node).nup=node_geom(node).nup+1
         node_geom(node).upstream(node_geom(node).nup)=extchan

c--------downstream node
         node=chan_geom(extchan).downnode
         node_geom(node).ndown=node_geom(node).ndown+1
         node_geom(node).downstream(node_geom(node).ndown)=extchan
      enddo

      nnodes=0
      do node=1,max_nodes
         if ((node_geom(node).nup + node_geom(node).ndown) .gt. 0)
     &        nnodes=nnodes+1
      enddo

c-----internal reservoir number
      nres=0
      do i=1,max_reservoirs
c--------channel connections to this reservoir
         if (res_geom(i).nnodes .gt. 0) then
            nres=nres+1
            res_geom(i).number=nres
         endif
      enddo

c-----check validity for cross-sections
      do intchan=1,nchan_list
         extchan=int2ext(intchan)
c--------for each channel, all cross-sections should have initial values
c--------for stage and flow; else error
         if (chan_geom(extchan).dist(1) .ne. 0 .and.
     &        chan_geom(extchan).dist(chan_geom(extchan).nxsect) .ne.
     &        chan_geom(extchan).length) then
            write(unit_error,680) extchan
            goto 900
         endif
         do j=1,chan_geom(extchan).nxsect
            xs=chan_geom(extchan).xsect(j)
            if (xsect_geom(xs).init_stage .eq. miss_val_r .or.
     &           xsect_geom(xs).init_flow  .eq. miss_val_r) then
               write(unit_error,640) xs,extchan
               goto 900
            endif
            chan_geom(extchan).BottomElev(j)=xsect_geom(xs).botelv
         enddo
      enddo

c-----read the irregular x-section data; assign rectangular or irregular
c-----xsects to upstream, middle, or downstream of channels; copy
c-----bottom elevations to chan_geom structure
      call readirreg

c-----fill translation and reservoir name vectors to be able to use
c-----loccarr function later
      do i=1,max_translations
         trans_from_names(i)=translations(i).from_name
      enddo
      do i=1,max_reservoirs
         res_names(i)=res_geom(i).name
      enddo

c-----generate list of unique flow and water quality accounting names
      do i=1,ntypes
         if (type_spec(i).acct_name .ne. ' ') then
            loc=loccarr(type_spec(i).acct_name,acct_names(1),max_acct_names,
     &           EXACT_MATCH)
            if (loc .gt. 0) then ! old name
               type_spec(i).acct_ndx=loc
            else                ! new name
               if (abs(loc) .lt. max_acct_names) then
                  nacct_names=nacct_names+1
                  acct_names(nacct_names)=type_spec(i).acct_name
                  type_spec(i).acct_ndx=nacct_names
               else
                  write(unit_error,695) max_acct_names
                  goto 900
               endif
            endif
         endif
      enddo

c-----assign a null accounting name for flows with no label

      if (nacct_names .lt. max_acct_names) then
         nacct_names=nacct_names+1
         acct_names(nacct_names)=miss_val_c
      else
         write(unit_error,695) max_acct_names
         goto 900
      endif

c-----translate object node names to node numbers;
c-----convert object number to name if appropriate;
c-----fill in accounting name indices for other structures,
c-----checking that the requested name was given in the accounting
c-----type structure
      do i=1,nobj2obj
         if (obj2obj(i).from.object .eq. obj_node) then
            if (obj2obj(i).from.object_no .eq. 0 .and.
     &           obj2obj(i).from.obj_name .ne. ' ') then ! translate
               ctmp=obj2obj(i).from.obj_name
               loc=loccarr(ctmp,trans_from_names,max_translations,
     &              EXACT_MATCH)
               if (loc .le. 0) then ! no translation found for object name
                  write(unit_error,644) 'From object node',ctmp(:lnblnk(ctmp))
                  goto 900
               else             ! translation found
c-----------------check that translation to object is a node
                  if (translations(loc).object .ne. obj_node) then
                     write(unit_error,687) ctmp(:lnblnk(ctmp))
 687                 format(/'Translation for ',a,
     &                    ' in obj2obj section is not a node number.')
                     goto 900
                  else          ! translation to node found
                     obj2obj(i).from.object_no=translations(loc).object_no
                  endif
               endif
            endif
c-----------store node number as name for later convenience
            write(obj2obj(i).from.obj_name,'(i3)')
     &           obj2obj(i).from.object_no
         endif
         if (obj2obj(i).from.acct_name .ne. ' ') then
            loc=loccarr(obj2obj(i).from.acct_name,acct_names(1),
     &           max_acct_names,EXACT_MATCH)
            if (loc .gt. 0) then ! old name
               obj2obj(i).from.acct_ndx=loc
            else                ! new name
               if (nacct_names .lt. max_acct_names-1) then
                  nacct_names=nacct_names+1
                  acct_names(nacct_names)=obj2obj(i).from.acct_name
                  obj2obj(i).from.acct_ndx=nacct_names
                  acct_names(nacct_names+1)=miss_val_c ! null accounting name
               else
                  write(unit_error,695) max_acct_names
                  goto 900
               endif
            endif
         endif

         if (obj2obj(i).to.object .eq. obj_node) then
            if (obj2obj(i).to.object_no .eq. 0 .and.
     &           obj2obj(i).to.obj_name .ne. ' ') then ! translate
               ctmp=obj2obj(i).to.obj_name
               loc=loccarr(ctmp,trans_from_names,max_translations,
     &              EXACT_MATCH)
               if (loc .le. 0) then ! no translation found for object name
                  write(unit_error,644) 'To object node',ctmp(:lnblnk(ctmp))
                  goto 900
               else             ! translation found
c-----------------check that translation to object is a node
                  if (translations(loc).object .ne. obj_node) then
                     write(unit_error,687) ctmp(:lnblnk(ctmp))
                     goto 900
                  else          ! translation to node found
                     obj2obj(i).to.object_no=translations(loc).object_no
                  endif
               endif
            endif
c-----------store node number as name for later convenience
            write(obj2obj(i).to.obj_name,'(i3)')
     &           obj2obj(i).to.object_no
         endif
         if (obj2obj(i).to.acct_name .ne. ' ') then
            loc=loccarr(obj2obj(i).to.acct_name,acct_names(1),max_acct_names,
     &           EXACT_MATCH)
            if (loc .gt. 0) then ! old name
               obj2obj(i).to.acct_ndx=loc
            else                ! new name
               if (nacct_names .lt. max_acct_names-1) then
                  nacct_names=nacct_names+1
                  acct_names(nacct_names)=obj2obj(i).to.acct_name
                  obj2obj(i).to.acct_ndx=nacct_names
                  acct_names(nacct_names+1)=miss_val_c ! null accounting name
               else
                  write(unit_error,695) max_acct_names
                  goto 900
               endif
            endif
         endif

      enddo

      do i=1,noutpaths
         if (pathoutput(i).source.acct_name .ne. ' ') then
            loc=loccarr(pathoutput(i).source.acct_name,acct_names(1),
     &           max_acct_names,EXACT_MATCH)
            if (loc .gt. 0) then ! old name
               pathoutput(i).source.acct_ndx=loc
            else                ! invalid name
               write(unit_error,697) 'pathoutput.source',
     &              pathoutput(i).source.acct_name
     &              (:lnblnk(pathoutput(i).source.acct_name))
               goto 900
            endif
         endif
      enddo

c-----add on reservoir, gate and obj2obj names/labels to translation
c-----structure, makes translations easier
c-----also check that translations don't use reservoir, gate, and
c-----obj2obj names

      do i=1,nreser
         loc=loccarr(res_geom(i).name,trans_from_names,max_translations,
     &        EXACT_MATCH)
         if (loc .le. 0) then   ! no existing translation found for reservoir name
c-----------append to end of translation list
            ntrans=ntrans+1
            if (ntrans.gt. max_translations) then
               write(unit_error, 607) ntrans,
     &              max_translations
               goto 900
            endif
            translations(ntrans).object=obj_reservoir
            translations(ntrans).from_name=res_geom(i).name
            translations(ntrans).obj_name=res_geom(i).name
            translations(ntrans).object_no=i
            trans_from_names(ntrans)=translations(ntrans).obj_name
         else                   ! reservoir name used as translation
            write(unit_error,609) 'reservoir',
     &           res_geom(i).name(:lnblnk(res_geom(i).name))
            goto 900
         endif
      enddo

      do i=1,ngates
         loc=loccarr(gate_geom(i).name,trans_from_names,max_translations,
     &        EXACT_MATCH)
         if (loc .le. 0) then   ! no existing translation found for gate name
c-----------append to end of translation list
            ntrans=ntrans+1
            if (ntrans.gt. max_translations) then
               write(unit_error, 607) ntrans,
     &              max_translations
               goto 900
            endif
            translations(ntrans).object=obj_gate
            translations(ntrans).from_name=gate_geom(i).name
            translations(ntrans).obj_name=gate_geom(i).name
            translations(ntrans).object_no=i
            if (gate_geom(i).loc .eq. 'up') then ! upstream
               translations(ntrans).chan_dist=0
            else                ! downstream
               translations(ntrans).chan_dist=chan_geom(gate_geom(i).chan_no).length
            endif
            trans_from_names(ntrans)=translations(ntrans).obj_name
         else                   ! gate name used as translation
c-----------gate name for reservoir ok
            if (translations(loc).object .ne. obj_reservoir) then
               write(unit_error,609) 'gate',
     &              gate_geom(i).name(:lnblnk(gate_geom(i).name))
               goto 900
            endif
         endif
      enddo

      do i=1,nobj2obj
c--------object names first...
         if (obj2obj(i).obj_name .ne. ' ') then ! don't process blank names
            loc=loccarr(obj2obj(i).obj_name,trans_from_names,max_translations,
     &           EXACT_MATCH)
            if (loc .le. 0) then ! no existing translation found for obj2obj name
c--------------append to end of translation list
               ntrans=ntrans+1
               if (ntrans.gt. max_translations) then
                  write(unit_error, 607) ntrans,
     &                 max_translations
                  goto 900
               endif
               translations(ntrans).object=obj_obj2obj
               translations(ntrans).from_name=obj2obj(i).obj_name
               translations(ntrans).obj_name=obj2obj(i).obj_name
               translations(ntrans).object_no=i
               trans_from_names(ntrans)=translations(ntrans).obj_name
            else                ! obj2obj name used as translation
               write(unit_error,609) 'obj2obj',
     &              obj2obj(i).obj_name(:lnblnk(obj2obj(i).obj_name))
               goto 900
            endif
         endif
c--------...then object labels
         if (obj2obj(i).label .ne. ' ') then ! don't process blank labels
            loc=loccarr(obj2obj(i).label,trans_from_names,max_translations,
     &           EXACT_MATCH)
            if (loc .le. 0) then ! no existing translation found for obj2obj label
c--------------append to end of translation list
               ntrans=ntrans+1
               if (ntrans.gt. max_translations) then
                  write(unit_error, 607) ntrans,
     &                 max_translations
                  goto 900
               endif
               translations(ntrans).object=obj_obj2obj
               translations(ntrans).from_name=obj2obj(i).label
               translations(ntrans).obj_name=obj2obj(i).label
               translations(ntrans).object_no=i
               trans_from_names(ntrans)=translations(ntrans).obj_name
            else                ! obj2obj label used as translation
               write(unit_error,609) 'obj2obj',
     &              obj2obj(i).label(:lnblnk(obj2obj(i).label))
               goto 900
            endif
         endif
      enddo

c-----refill translation name vector
      do i=1,max_translations
         trans_from_names(i)=translations(i).from_name
      enddo

c-----fill in object numbers in translation structure for
c-----translating to reservoirs
      do i=1,ntrans
         if (translations(i).object .eq. obj_reservoir) then
            loc=loccarr(translations(i).obj_name,res_names,max_reservoirs,
     &           EXACT_MATCH)
            if (loc .gt. 0) then
               translations(i).object_no=loc
            else
               write(unit_error,647)
 647           format(/'Invalid reservoir translation name:',a)
               goto 900
            endif
         endif
      enddo

c-----translate input and output paths from name/label to node, gate,
c-----reservoir, or obj2obj

      do i=1,ninpaths           ! input paths
         do j=1,max_path_const
            pathinput(i).const_ndx(j) = 0 ! the constituent(s) of this path, if any
         enddo
         if ( pathinput(i).object_no .eq. 0 .and.
     &        pathinput(i).label .eq. ' ') then ! no object & no translation
            write(unit_error,642) 'Path input',i
            goto 900
         endif
         if (pathinput(i).label .ne. ' ') then
c-----------translation needed to node, gate, reservoir or object-to-
c-----------object name
c-----------gate and obj2obj names/labels previously copied to
c-----------translation tables to make this section easier
            loc=loccarr(pathinput(i).label,trans_from_names,
     &           max_translations,EXACT_MATCH)
            if (loc .le. 0) then ! couldn't find the translation of the name
               write(unit_error, 644) 'Input',
     &              pathinput(i).label(1:lnblnk(pathinput(i).label))
               goto 900
            endif
c-----------translation found, fill in node/reservoir/gate name/number
c-----------check for translation to channel; if so, try to find
c-----------equivalent node
            if (translations(loc).object .eq. obj_channel) then
               if (translations(loc).chan_dist .eq. 0) then
                  node=chan_geom(translations(loc).object_no).upnode
                  if (node .gt. 0) then
                     pathinput(i).object=obj_node
                     pathinput(i).object_no=node
                  endif
               else if (translations(loc).chan_dist .eq. chan_length .or.
     &                 translations(loc).chan_dist .eq.
     &                 chan_geom(translations(loc).object_no).length) then
                  node=chan_geom(translations(loc).object_no).downnode
                  if (node .gt. 0) then
                     pathinput(i).object=obj_node
                     pathinput(i).object_no=node
                  endif
               endif
            else                ! not a channel
               pathinput(i).object=translations(loc).object
               pathinput(i).object_no=translations(loc).object_no
            endif
         endif
      enddo

      do i=1,noutpaths          ! output paths
         if ( pathoutput(i).object_no .eq. 0 .and.
     &        pathoutput(i).name .eq. ' ' .and.
     &        pathoutput(i).b_part .eq. ' ') then ! no channel, node, reservoir or ptm flux & no translation
            write(unit_error,642) 'Path output',i
            goto 900
         endif

         if (pathoutput(i).name .ne. ' ') then ! translation needed
            loc=loccarr(pathoutput(i).name,trans_from_names,
     &           max_translations,EXACT_MATCH)
            if (loc .le. 0) then ! couldn't find the translation of the name
               write(unit_error, 644) 'Output',
     &              pathoutput(i).name(1:lnblnk(pathoutput(i).name))
               goto 900
            endif
c-----------translation found, fill in channel, reservoir, or gate number
            pathoutput(i).object=translations(loc).object
            pathoutput(i).object_no=translations(loc).object_no
            pathoutput(i).chan_dist=translations(loc).chan_dist
         endif
c--------all translations done

c--------change stage output at node to channel/distance
         if (pathoutput(i).object .eq. obj_node .and.
     &        pathoutput(i).meas_type .eq. 'stage') then
            pathoutput(i).object=obj_channel
            if (node_geom(pathoutput(i).object_no).nup .gt. 0) then
               pathoutput(i).object_no=node_geom(pathoutput(i).object_no).upstream(1)
               pathoutput(i).chan_dist=0
            else
               pathoutput(i).object_no=node_geom(pathoutput(i).object_no).downstream(1)
               pathoutput(i).chan_dist=chan_length
            endif
         endif

c--------try to change flow or velocity output at node to channel/distance:
c--------must have only two channels connecting
         if (pathoutput(i).object .eq. obj_node .and.
     &        (pathoutput(i).meas_type .eq. 'flow' .or.
     &        pathoutput(i).meas_type .eq. 'vel')) then
            if (node_geom(pathoutput(i).object_no).nup .eq. 1) then
               pathoutput(i).object=obj_channel
               pathoutput(i).object_no=node_geom(pathoutput(i).object_no).upstream(1)
               pathoutput(i).chan_dist=0
            else if (node_geom(pathoutput(i).object_no).ndown .eq. 1) then
               pathoutput(i).object=obj_channel
               pathoutput(i).object_no=node_geom(pathoutput(i).object_no).downstream(1)
               pathoutput(i).chan_dist=chan_length
            else
c--------------can't have flow/vel at a node with multiple channels
               write(unit_error,671)
     &              pathoutput(i).name(1:lnblnk(pathoutput(i).name)),
     &              pathoutput(i).meas_type(1:lnblnk(pathoutput(i).meas_type)),
     &              pathoutput(i).interval(1:lnblnk(pathoutput(i).interval)),
     &              pathoutput(i).modifier(1:lnblnk(pathoutput(i).modifier)),
     &              pathoutput(i).object_no
               goto 900
            endif
         endif

c--------check reservoir output
         if (pathoutput(i).name .ne. ' ' .and.
     &        translations(loc).object .eq. obj_reservoir) then
            if (pathoutput(i).meas_type .eq. 'stage') then
               pathoutput(i).reservoir.node_no=0
            endif
c-----------check for valid reservoir name
            loc=loccarr(pathoutput(i).name,res_names,max_reservoirs,
     &           EXACT_MATCH)
            if (loc .le. 0) then ! no reservoir with that name
               write(unit_error, 610) 'reservoir',
     &              pathoutput(i).name(:lnblnk(pathoutput(i).name))
               goto 900
            else
               pathoutput(i).object_no=loc
               if (pathoutput(i).reservoir.node_no .gt. 0) then
c-----------------check that if a node was given, flow was specified
                  if (pathoutput(i).meas_type .ne. 'flow') then
                     write(unit_error, 620) pathoutput(i).name
                     goto 900
                  endif
c-----------------check for valid node number
                  if (node_geom(pathoutput(i).reservoir.node_no).nup +
     &                 node_geom(pathoutput(i).reservoir.node_no).ndown
     &                 .eq. 0) then
                     write(unit_error, 630) pathoutput(i).name,
     &                    pathoutput(i).reservoir.node_no
                     goto 900
                  endif
               endif
            endif

c-----------valid node number and Hydro node number
            if (pathoutput(i).reservoir.node_no .gt. 0) then
               k=pathoutput(i).object_no
               j=1
               do while (j .le. res_geom(k).nnodes .and.
     &              res_geom(k).node_no(j) .ne. pathoutput(i).reservoir.node_no)
                  j=j+1
               enddo
               if (j .gt. res_geom(k).nnodes) then
                  write(unit_error,705)
     &                 pathoutput(i).name(:lnblnk(pathoutput(i).name)),
     &                 pathoutput(i).reservoir.node_no
                  goto 900
               endif
               pathoutput(i).reservoir.hydro_node_no=j
            endif
         endif
      enddo

c-----create DSS input pathnames, check for sign change for each path

      npthsin_min15=0
      npthsin_hour1=0
      npthsin_day1=0
      npthsin_week1=0
      npthsin_month1=0
      npthsin_year1=0
      npthsin_irr=0

      do p=1,ninpaths
         ca=' '
         cb=' '
         cc=' '
         ce=' '
         cf=' '
c--------a part
         if (pathinput(p).a_part .ne. ' ') then
            ca=pathinput(p).a_part
         endif
c--------b part
         if (pathinput(p).b_part .ne. ' ') then
            cb=pathinput(p).b_part
         else if (pathinput(p).label .ne. ' ') then ! use label as b part
            cb=pathinput(p).label
         else if (pathinput(p).object .eq. obj_node) then ! use node as b part
            write(cb,'(i3)') pathinput(p).object_no
         else                   ! no label or node: error
            write(unit_error,649) p
            goto 900
         endif
c--------c part
         cc=pathinput(p).c_part
c--------e part
         ce=pathinput(p).e_part
         if (pathinput(p).no_intervals .eq. 1 .and.
     &        pathinput(p).e_part(1:5) .eq. '15min') then
            npthsin_min15=npthsin_min15+1
            if (npthsin_min15 .gt. max_inp_min) then
               write(unit_error, 651) '15MIN',max_inp_min
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_min15
            ptin_min15(npthsin_min15)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).e_part(1:5) .eq. '1hour') then
            npthsin_hour1=npthsin_hour1+1
            if (npthsin_hour1 .gt. max_inp_hour) then
               write(unit_error, 651) '1HOUR',max_inp_hour
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_hour1
            ptin_hour1(npthsin_hour1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).e_part(1:4) .eq. '1day') then
            npthsin_day1=npthsin_day1+1
            if (npthsin_day1 .gt. max_inp_day) then
               write(unit_error, 651) '1DAY',max_inp_day
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_day1
            ptin_day1(npthsin_day1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).e_part(1:5) .eq. '1week') then
            npthsin_week1=npthsin_week1+1
            if (npthsin_week1 .gt. max_inp_week) then
               write(unit_error, 651) '1WEEK',max_inp_week
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_week1
            ptin_week1(npthsin_week1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).e_part(1:4) .eq. '1mon') then
            npthsin_month1=npthsin_month1+1
            if (npthsin_month1 .gt. max_inp_month) then
               write(unit_error, 651) '1MON',max_inp_month
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_month1
            ptin_month1(npthsin_month1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).e_part(1:5) .eq. '1year') then
            npthsin_year1=npthsin_year1+1
            if (npthsin_year1 .gt. max_inp_year) then
               write(unit_error, 651) '1YEAR',max_inp_year
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_year1
            ptin_year1(npthsin_year1)=p
         else if (pathinput(p).e_part(:3) .eq. 'ir-') then ! irregular interval
            npthsin_irr=npthsin_irr+1
            if (npthsin_irr .gt. max_inp_irr) then
               write(unit_error, 651) 'IR-',max_inp_irr
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_irr
            ptin_irr(npthsin_irr)=p
         else                   ! unrecognized interval
            write(unit_error,650) 'input', pathinput(p).no_intervals,
     &           pathinput(p).e_part(:lnblnk(pathinput(p).e_part))
            goto 900
         endif

c--------f part
         cf=pathinput(p).f_part

         path='/'
     &        // ca // '/'      ! a part
     &        // cb // '/'      ! b part
     &        // cc // '/'      ! c part
     &        // '/'            ! d part (empty)
     &        // ce // '/'      ! e part
     &        // cf // '/'      ! f part

         pathinput(p).path=' '
         call remblk(path,pathinput(p).path,nlen) ! remove blanks within path
         call upcase(pathinput(p).path) ! convert to upper case
c--------make individual path parts
         nlen=len(pathinput(p).path)
         call zufpn(ca,na,cb,nb,cc,nc,cd,nd,ce,ne,cf,nf,
     &        pathinput(p).path,nlen,istat)

c--------check for path alternate data start date
         if (lnblnk(pathinput(p).start_dt) .ne. 0) then
            if (pathinput(p).start_dt(11:14) .eq. ' ') then
               pathinput(p).start_dt(11:14)='0000'
            endif
            pathinput(p).diff_julmin=cdt2jmin(pathinput(p).start_dt)
     &           - start_julmin
         endif

c--------check validity of a generic synchronize request:
c--------interval can be only minute, hour, day, month
         if (pathinput(p).sync) then
            call split_epart(pathinput(p).e_part, number, interval)
            if (interval .ne. '15min' .and.
     &           interval .ne. '1hour' .and.
     &           interval .ne. '1day' .and.
     &           interval .ne. '1mon') then
               write(unit_error,685)
     &              pathinput(p).path(:lnblnk(pathinput(p).path))
               goto 900
            endif
         endif

c--------set data type
         pathinput(p).data_type=data_types(pathinput(p).c_part)

c--------check for sign change on incoming values, and set flow type
         pathinput(p).sign=' '  ! no sign change

c--------set accounting types first
c--------then check for sign change by accounting type (always check all type
c--------specs to allow overriding)
c--------then finally check for sign change by pathname; this allows
c--------pathname sign changes to override sign change by accounting type

c--------set accounting types of input paths
         do j=1,ntypes
            if (type_spec(j).string .ne. ' ') then ! check by string spec
               call assign_type(type_spec(j),pathinput(p),
     &              ca,cb,cc,ce,cf,'a')
            endif
         enddo

c--------assign sign, mass fraction, value change, and data flags by
c--------accounting type, first
         do j=1,ntypes
            if (type_spec(j).string .eq. ' ' .and.
     &           type_spec(j).acct_ndx .eq. pathinput(p).acct_ndx) then
               if (type_spec(j).sign .ne. ' ') then
                  pathinput(p).sign=type_spec(j).sign
               endif
               if (type_spec(j).mass_frac .ne. miss_val_r) then
                  pathinput(p).mass_frac=type_spec(j).mass_frac
               endif
               if (type_spec(j).value_in .ne. miss_val_r) then
                  pathinput(p).value_in=type_spec(j).value_in
                  pathinput(p).value_out=type_spec(j).value_out
               endif
               if (type_spec(j).value_flag .ne. miss_val_i) then
                  pathinput(p).use_flag=type_spec(j).value_flag
               endif
            endif
         enddo

c--------assign sign, mass fraction, value change, and data flags by
c--------matching pathname string, second

         do j=1,ntypes
            if (type_spec(j).string .ne. ' ') then
               call assign_type(type_spec(j),pathinput(p),
     &              ca,cb,cc,ce,cf,'s')
               call assign_type(type_spec(j),pathinput(p),
     &              ca,cb,cc,ce,cf,'m')
               call assign_type(type_spec(j),pathinput(p),
     &              ca,cb,cc,ce,cf,'v')
               call assign_type(type_spec(j),pathinput(p),
     &              ca,cb,cc,ce,cf,'f')
            endif
         enddo
c--------check that the method of filling in missing data is specified
         if (pathinput(p).fillin .eq. miss_val_i) then
            write(unit_error,*) 'Note: ', pathinput(p).
     &           path(:lnblnk(pathinput(p).path)),
     &           ' had no FILLIN specified, LAST will be used.'
            pathinput(p).fillin=FILL_LAST
         endif
c--------check that an input DSS filename is given
         if (pathinput(p).constant_value .eq. miss_val_r .and. ! not a constant value path
     &        pathinput(p).filename .eq. ' ') then
            write(unit_error, *) 'Fatal error: ',pathinput(p).
     &           path(:lnblnk(pathinput(p).path)),
     &           ' has no input DSS filename given.'
            goto 900
         endif
c--------check for gate parameter (width, flow coeff, ...) input
         if (pathinput(p).object .eq. obj_gate .and.
     &        pathinput(p).data_type .ne. gate_type) then ! gate parameter
            ctmp=pathinput(p).c_part
c-----------check that this path's parameter is in the gate headers
            k=1
            do while (hdr_form(gates_section).fld(k) .ne. ' ' .and.
     &           k .le. hdr_form(gates_section).fldnum .and.
     &           hdr_form(gates_section).fld(k)
     &           (:lnblnk( hdr_form(gates_section).fld(k))) .ne.
     &           ctmp(:lnblnk(ctmp)))
               k=k+1
            enddo
            if (hdr_form(gates_section).fld(k)
     &           (:lnblnk( hdr_form(gates_section).fld(k))) .eq.
     &           ctmp(:lnblnk(ctmp))) then ! found a match
               dsm_gateno=pathinput(p).object_no
               hydro_gateno=hydrogates(dsm_gateno)
               if (
     &              k .eq. gate_ngates .or.
     &              k .eq. gate_width_up .or.
     &              k .eq. gate_width_down .or.
     &              k .eq. gate_crest_elev .or.
     &              k .eq. gate_coeff_weir_up .or.
     &              k .eq. gate_coeff_weir_down .or.
     &              k .eq. gate_npipes .or.
     &              k .eq. gate_pipe_rad .or.
     &              k .eq. gate_pipe_elev .or.
     &              k .eq. gate_coeff_pipe_up .or.
     &              k .eq. gate_coeff_pipe_down .or.
     &              k .eq. gate_dhopen .or.
     &              k .eq. gate_velclose
     &              ) then
                  pathinput(p).gate_param=k
               else             ! can't modify this gate field, warn and continue
                  write(unit_error,*) 'Cannot change values in path ',
     &                 pathinput(p).path
               endif
            else                ! not a recognized gate field keyword; warn and continue
               write(unit_error,*) 'Cannot change values in path ',
     &              pathinput(p).path
            endif
         endif
      enddo                     ! end pathinput loop

c-----check for alternate data replacement paths
      do p=1,ninpaths
         if (pathinput(p).priority .ne. 0) then
            do j=1,ninpaths
               if (p .ne. j .and.
     &              pathinput(p).object .eq. pathinput(j).object .and.
     &              pathinput(p).object_no .eq. pathinput(j).object_no .and.
     &              pathinput(p).c_part .eq. pathinput(j).c_part) then
c-----------------same location and C part, set order by priority
                  call chain_priority(p,j)
               endif
            enddo
         endif
      enddo

c-----this section belongs in process_fixed
      if (dsm2_module .eq. ptm) then
         if( ptm_igroup ) then
            npart_flux = noutpaths
c-----------determine the actual number of groups and put this into an array
            ngroups = 1
            do i = 1, nchanres
               itmp = group_areas(i).group
               advance = 1
               j = 1
               do while ( j .le. ngroups .and. advance .eq. 1)
                  if ( itmp .eq. holdgrp(j) ) advance = 0
                  j = j + 1
               enddo
               if ( advance .eq. 1 ) holdgrp(ngroups) = itmp
               ngroups = ngroups + advance
            enddo

c-----------fill indexing array
            do i = 1, ngroups
               revgrp(holdgrp(i))=i
            enddo

c-----------addjust group index
            ngroups = ngroups - 1
            adjpathout = ngroups + noutpaths
            outindx = noutpaths
            do while (noutpaths .lt. adjpathout)

               noutpaths = noutpaths + 1
c--------------set path parameters
               pathoutput(noutpaths).filename = io_files(ptm,io_group,io_write).filename
               pathoutput(noutpaths).meas_type = 'ptm_group'
               pathoutput(noutpaths).units = 'percent'
               pathoutput(noutpaths).interval = io_files(ptm,io_group,io_write).interval
               pathoutput(noutpaths).per_type = per_type_inst_cum
               pathoutput(noutpaths).no_intervals = 1        ! bad way to do this

               if (index(pathoutput(noutpaths).filename, '.dss') .gt. 0) then
c-----------------accumulate unique dss output filenames
                  itmp=loccarr(pathoutput(noutpaths).filename,outfilenames
     &                 ,max_dssoutfiles, EXACT_MATCH)
                  if (itmp .lt. 0) then
                     if (abs(itmp) .le. max_dssoutfiles) then
                        outfilenames(abs(itmp))=pathoutput(noutpaths).filename
                        pathoutput(noutpaths).ndx_file=abs(itmp)
                     else
                        write(unit_error,610)
     &                       'Maximum number of unique DSS output files exceeded'
                        goto 900
                     endif
                  else
                     pathoutput(noutpaths).ndx_file=itmp
                  endif
               endif

c--------------DSS a part
               
               if (pathoutput(noutpaths).a_part .ne. ' ') then
                  ca=pathoutput(noutpaths).a_part
               else             ! not explicitly given, use IEP format
                  ctmp='DSM2-' // dsm2_name(:lnblnk(dsm2_name)) // '-' //
     &                 dsm2_version
                  ca=ctmp(:lnblnk(ctmp)) // '+GROUP'
               endif
               pathoutput(noutpaths).a_part = ca

c--------------DSS b part
               if (pathoutput(noutpaths).b_part .ne. ' ') then
                  cb=pathoutput(noutpaths).b_part
               else             ! not explicitly given
                  encode( 3, '(i3)', grpnum) holdgrp(noutpaths - outindx) 
                  ctmp='GROUP-' // grpnum
                  cb=ctmp(:lnblnk(ctmp))
               endif
               pathoutput(noutpaths).b_part = cb

c--------------DSS c part
               if (pathoutput(noutpaths).c_part .ne. ' ') then
                  cc=pathoutput(noutpaths).c_part
               else             ! not explicitly given
c-----------------c part will be measurement type
                  cc=pathoutput(noutpaths).meas_type
               endif

c--------------DSS f part
               if (pathoutput(noutpaths).f_part .ne. ' ') then
                  cf=pathoutput(noutpaths).f_part
               else             ! not explicitly given, use IEP format
                  cf=' '
                  modifier=' '
                  ctmp=' '
c-----------------agency name
                  call getenv('DSM2AGENCY', ctmp)
                  if (ctmp .ne. ' ') then
                     cf=ctmp
                  endif

c-----------------optional modifier (study name, etc)
                  if (pathoutput(noutpaths).modifier .ne. ' ') then
                     modifier=pathoutput(noutpaths).modifier
                  else
                     call getenv('DSM2MODIFIER', ctmp)
                     if (ctmp .ne. ' ') then
                        modifier=ctmp
                     endif
                  endif
                  if (modifier .eq. 'runtime') then ! fill with runtime: yymmdd.hhmm
                     modifier=crdt10(1:6) // '.' // crdt10(7:10)
                  endif
                  if (modifier .eq. 'none') then
                     modifier=' '
                  endif

                  if (modifier .ne. ' ') then
                     modifier='+' // modifier
                  endif

                  cf=cf(:lnblnk(cf)) // modifier

                  path='/'
     &                 // ca // '/' ! a part
     &                 // cb // '/' ! b part
     &                 // cc // '/' ! c part
     &                 // '/'   ! d part
     &                 // pathoutput(noutpaths).interval // '/' ! e part
     &                 // cf // '/' ! f part

                  call remblk(path,pathoutput(noutpaths).path,nlen)
                  call upcase(pathoutput(noutpaths).path) ! convert to upper case
               endif
            enddo

         endif
      endif

      if (dsm2_module .eq. qual) then
c--------Conservative and nonconservative constituents.
c--------For conservative constituents (CCs), note that a 'constituent' is
c--------really each unique combination of constituent and source
c--------(either by location name, flow type, or node), while for
c--------non-conservative constituents (NCCs), each constituent type
c--------is considered unique, regardless of source, since different
c--------source tracking is not allowed for NCCs.

c--------First form a list of constituents to track from the output
c--------pathnames. Assume that any DSS C outputpath part that is not
c--------stage, flow, meteorological, or non-conservative, is a
c--------conservative constituent.  For CCs, use *only* the output paths,
c--------since there's no point in tracking a constituent in the input if
c--------the user doesn't want its output.  For NCCs, since one
c--------constituent may require others to react with, we may have to add
c--------others constituents to the tracking list.

c--------After the list is created, abort if there is not at least one
c--------input for each constituent to track.  That checking is done
c--------in check_fixed_qual

         no_of_constituent=0
         no_of_nonconserve_constituent=0
         do p=1,noutpaths
            loc=loccarr(pathoutput(p).meas_type,non_constituents,max_nc,
     &           EXACT_MATCH)
            if (loc .le. 0) then ! must be a water quality constituent
               if (pathoutput(p).source.loc_name .eq. ' ' .and.
     &              pathoutput(p).source.object_no .eq. 0 .and.
     &              pathoutput(p).source.acct_ndx .eq. 0 ) then
c-----------------no source spec given: assume from all sources
                  pathoutput(p).source.loc_name='all'
               endif

               if (uniq_constituent(p)) then
c-----------------new, unique constituent to add to tracking list
                  no_of_constituent=no_of_constituent+1
                  if (no_of_constituent .gt. max_constituent) then
                     write(unit_error, *)
     &                    'Error: too many different output constituents specified.'
                     write(unit_error, *)
     &                    'Max allowed is ',max_constituent
                     goto 900
                  endif

                  pathoutput(p).const_ndx=no_of_constituent
                  constituents(no_of_constituent).constituent=
     &                 pathoutput(p).meas_type
                  constituents(no_of_constituent).conservative=
     &                 .not. ncc(pathoutput(p).meas_type)
                  constituents(no_of_constituent).loc_name=
     &                 pathoutput(p).source.loc_name
                  constituents(no_of_constituent).object=
     &                 pathoutput(p).source.object
                  constituents(no_of_constituent).object_no=
     &                 pathoutput(p).source.object_no
                  constituents(no_of_constituent).acct_ndx=
     &                 pathoutput(p).source.acct_ndx

                  if (.not. constituents(no_of_constituent).conservative) then ! non-conservative constituent
                     no_of_nonconserve_constituent=no_of_nonconserve_constituent+1
                     nonconserve_ptr(no_of_nonconserve_constituent)=no_of_constituent
                  endif
               endif
            endif
         enddo

c--------for conservative constituents, make sure that each different
c--------chemical constituent has at least one 'all' source if a
c--------restart file is requested
         if (io_files(qual,io_restart,io_write).use) then
            j=0
            do i=1,no_of_constituent
               if (constituents(i).conservative .and.
     &              loccarr(constituents(i).constituent,const_names,
     &              no_of_constituent, EXACT_MATCH) .le. 0) then
c-----------------conservative chemical not found before
                  j=j+1
                  const_names(j)=constituents(i).constituent
c-----------------check if an 'all' source already exists for this one
                  do k=i,no_of_constituent
                     if ( (constituents(i).constituent .eq.
     &                    constituents(k).constituent)  .and.
     &                    constituents(k).loc_name .eq. 'all') then
                        goto 800
                     endif
                  enddo
c-----------------no 'all' source found, make one
                  no_of_constituent=no_of_constituent+1
                  if (no_of_constituent .ge. max_constituent) then
                     write(unit_error, *)
     &                    'Error: too many different output constituents specified.'
                     write(unit_error, *)
     &                    'Max allowed is ',max_constituent
                     goto 900
                  endif
                  constituents(no_of_constituent).constituent=
     &                 constituents(i).constituent
                  constituents(no_of_constituent).conservative=.true.
                  constituents(no_of_constituent).loc_name='all '
 800              continue      ! here to skip making 'all' source
               endif
            enddo
         endif

c--------add needed constituents for DO and algae
c--------passing slices of structures in calls, chokes
         do i=1,max_constituent
            const_names(i)=constituents(i).constituent
         enddo

c--------DO first
         loc=loccarr(nonconserve_list(ncc_do),const_names,
     &        max_constituent,EXACT_MATCH)

         if (loc .gt. 0) then   ! DO is tracked; are other required NCCs in list?
            required_do(1)=nonconserve_list(ncc_organic_n)
            required_do(2)=nonconserve_list(ncc_organic_p)
            required_do(3)=nonconserve_list(ncc_nh3)
            required_do(4)=nonconserve_list(ncc_no3)
            required_do(5)=nonconserve_list(ncc_po4)
            required_do(6)=nonconserve_list(ncc_bod)
            required_do(7)=nonconserve_list(ncc_algae)
            do i=1,n_required_do
               loc=loccarr(required_do(i),const_names, max_constituent,EXACT_MATCH)
               if (loc .le. 0) then ! required NCC is not in list; add it
                  no_of_constituent=no_of_constituent+1
                  if (no_of_constituent .ge. max_constituent) then
                     write(unit_error, *)
     &                    'Error: too many different output constituents specified.'
                     write(unit_error, *)
     &                    'Max allowed is ',max_constituent
                     goto 900
                  endif
                  constituents(no_of_constituent).constituent=required_do(i)
                  constituents(no_of_constituent).conservative=.false.
                  constituents(no_of_constituent).loc_name='all '
                  no_of_nonconserve_constituent=no_of_nonconserve_constituent+1
                  nonconserve_ptr(no_of_nonconserve_constituent)=no_of_constituent
               endif
            enddo
         endif

c--------update const_names if any constituent/s were asked for simulation
c--------which already fall within the group required for DO simulation
         do i=1,max_constituent
            const_names(i)=constituents(i).constituent
         enddo

c--------then algae
         loc=loccarr(nonconserve_list(ncc_algae),const_names,
     &        max_constituent,EXACT_MATCH)

         if (loc .gt. 0) then   ! algae is tracked; are other required NCCs in list?
            required_algae(1)=nonconserve_list(ncc_organic_n)
            required_algae(2)=nonconserve_list(ncc_organic_p)
            required_algae(3)=nonconserve_list(ncc_nh3)
            required_algae(4)=nonconserve_list(ncc_no3)
            required_algae(5)=nonconserve_list(ncc_po4)
            required_algae(6)=nonconserve_list(ncc_bod)
            required_algae(7)=nonconserve_list(ncc_do)
            do i=1,n_required_algae
               loc=loccarr(required_algae(i),const_names, max_constituent,EXACT_MATCH)
               if (loc .le. 0) then ! required NCC is not in list; add it
                  no_of_constituent=no_of_constituent+1
                  if (no_of_constituent .ge. max_constituent) then
                     write(unit_error, *)
     &                    'Error: too many different output constituents specified.'
                     write(unit_error, *)
     &                    'Max allowed is ',max_constituent
                     goto 900
                  endif
                  constituents(no_of_constituent).constituent=required_algae(i)
                  constituents(no_of_constituent).conservative=.false.
                  constituents(no_of_constituent).loc_name='all '
                  no_of_nonconserve_constituent=no_of_nonconserve_constituent+1
                  nonconserve_ptr(no_of_nonconserve_constituent)=no_of_constituent
               endif
            enddo
         endif

c--------pathinput().const_ndx keeps track of multiple source
c--------constituents which may come from the same input constituent.
c--------for non-conservative constituents, since multiple sources aren't
c--------allowed, we'll just have one for each input path.  But for
c--------conservative constituents, we may have multiple overlapping
c--------specifications
c--------this is also where constituent translations are done; e.g.
c--------DSS path is 'salinity' but we want it to be 'ec'

         do p=1,ninpaths
            pathinput(p).n_consts=0
            path_constituent=pathinput(p).c_part
            do i=1,ntrans
               if (translations(i).constituent .ne. ' ' .and.
     &              translations(i).from_name .eq. path_constituent) then
                  path_constituent=translations(i).constituent
               endif
            enddo

            do j=1,no_of_constituent
               if (path_constituent .eq. constituents(j).constituent .and.
     &              (.not. constituents(j).conservative  .or.
     &              (constituents(j).loc_name .eq. 'all' .or.
     &              (pathinput(p).label .ne. ' ' .and.
     &              pathinput(p).label(:lnblnk(pathinput(p).label)) .eq.
     &              constituents(j).loc_name(:lnblnk(constituents(j).loc_name))) .or.
     &              (pathinput(p).object .eq. constituents(j).object .and.
     &              pathinput(p).object_no .eq. constituents(j).object_no) .or.
     &              pathinput(p).acct_ndx .eq. constituents(j).acct_ndx))) then
                  pathinput(p).n_consts=pathinput(p).n_consts+1
                  pathinput(p).const_ndx(pathinput(p).n_consts) = j
                  constituent_input(j)=.true.
               endif
            enddo
c-----------generate warnings about input constituents that aren't used
            loc=loccarr(path_constituent,non_constituents,max_nc,
     &           EXACT_MATCH)
            if (pathinput(p).n_consts .eq. 0 .and. loc .le. 0) then
               write(unit_error,690)
     &              pathinput(p).path(:lnblnk(pathinput(p).path))
            endif
         enddo

c--------create pointer array of constituents from all sources
         no_all_source=0
         do i=1,no_of_constituent
            if (constituents(i).loc_name .eq. 'all') then
               no_all_source=no_all_source+1
               all_source_ptr(no_all_source)=i
            endif
         enddo
      endif

c-----do tracked constituents have inputs?
      do j=1,no_of_constituent
         if (.not. constituent_input(j)) then
            write(unit_error,631)
     &           constituents(j).constituent(:lnblnk(constituents(j).constituent)),
     &           constituents(j).loc_name(:lnblnk(constituents(j).loc_name)),
     &           acct_names(constituents(j).acct_ndx)
     &           (:lnblnk(acct_names(constituents(j).acct_ndx))),
     &           constituents(j).object_no
 631        format(/'Error:   Constituent does not have a matching input path.'/
     &           'Check input and output to make sure constituent name is not misspelled.'/
     &           'Constituent: ',a,'; source name: ',a,'; accounting name:',a,'; node: ',i3)

            goto 900
         endif
      enddo

c-----create DSS output pathnames, check for sign change for each path
c-----change channel distances to marker if == channel length
      do p=1,noutpaths
c--------remove output file, if text
         if (index(pathoutput(p).filename,'.dss') .eq. 0) then
            call unlink(pathoutput(p).filename)
         endif
c--------replace magic number channel length with correct channel length
         if (pathoutput(p).chan_dist .eq. chan_length)
     &        pathoutput(p).chan_dist=chan_geom(pathoutput(p).object_no).
     &        length
c--------DSS a part
         if (pathoutput(p).a_part .ne. ' ') then
            ca=pathoutput(p).a_part
         else                   ! not explicitly given, use IEP format
            ctmp='DSM2-' // dsm2_name(:lnblnk(dsm2_name)) // '-' //
     &           dsm2_version
            if (pathoutput(p).object .eq. obj_channel) then
               ca=ctmp(:lnblnk(ctmp)) // '+CHAN'
            else if (pathoutput(p).object .eq. obj_node) then
               ca=ctmp(:lnblnk(ctmp)) // '+NODE'
            else if (pathoutput(p).object .eq. obj_reservoir) then
               ca=ctmp(:lnblnk(ctmp)) // '+RSVR'
            else if (pathoutput(p).object .eq. obj_gate) then
               ca=ctmp(:lnblnk(ctmp)) // '+GATE'
            else if (pathoutput(p).object .eq. obj_qext) then
               ca=ctmp(:lnblnk(ctmp)) // '+QEXT'
            else if (pathoutput(p).object .eq. obj_obj2obj) then
               ca=ctmp(:lnblnk(ctmp)) // '+OBJ2OBJ'
            else if (pathoutput(p).object .eq. obj_flux) then
               ca=ctmp(:lnblnk(ctmp)) // '+FLUX'
            else if (pathoutput(p).object .eq. obj_stage) then
               ca=ctmp(:lnblnk(ctmp)) // '+STAGE'
            else
               ca=ctmp(:lnblnk(ctmp)) // '+UNK'
            endif
         endif
c--------DSS b part
         if (pathoutput(p).b_part .ne. ' ') then
            cb=pathoutput(p).b_part
         else                   ! not explicitly given
c-----------if name given, use that, else channel/dist; node number; reservoir name
            if (pathoutput(p).name .ne. ' ') then ! use translation name
               cb=pathoutput(p).name
c--------------object is a reservoir, and output is flow through a node,
c--------------then add on node number to reservoir name
               if (pathoutput(p).object .eq. obj_reservoir .and.
     &              pathoutput(p).reservoir.node_no .gt. 0) then
                  write(ctmp,'(i3)') pathoutput(p).reservoir.node_no
                  cb=cb(:lnblnk(cb)) // '-NODE' // ctmp(:lnblnk(ctmp))
               endif
            else                ! use chan/dist; node number
               if (pathoutput(p).object .eq. obj_channel) then
                  write(cdist,'(i10)') pathoutput(p).chan_dist
                  write(cb,'(i3.3,''_'',a)') pathoutput(p).object_no,cdist
               else if (pathoutput(p).object .eq. obj_node) then
                  write(cb,'(i3)') pathoutput(p).object_no
               endif
            endif
         endif

c--------DSS c part
         if (pathoutput(p).c_part .ne. ' ') then
            cc=pathoutput(p).c_part
         else                   ! not explicitly given
c-----------c part will be measurement type
            cc=pathoutput(p).meas_type
         endif

c--------DSS f part
         if (pathoutput(p).f_part .ne. ' ') then
            cf=pathoutput(p).f_part
         else                   ! not explicitly given, use IEP format
            cf=' '
            modifier=' '
            ctmp=' '
c-----------agency name
            call getenv('DSM2AGENCY', ctmp)
            if (ctmp .ne. ' ') then
               cf=ctmp
            endif

c-----------optional modifier (study name, etc)
            if (pathoutput(p).modifier .ne. ' ') then
               modifier=pathoutput(p).modifier
            else
               call getenv('DSM2MODIFIER', ctmp)
               if (ctmp .ne. ' ') then
                  modifier=ctmp
               endif
            endif
            if (modifier .eq. 'runtime') then ! fill with runtime: yymmdd.hhmm
               modifier=crdt10(1:6) // '.' // crdt10(7:10)
            endif
            if (modifier .eq. 'none') then
               modifier=' '
            endif

            if (modifier .ne. ' ') then
               modifier='+' // modifier
            endif

            cf=cf(:lnblnk(cf)) // modifier

c-----------if output is water quality constituent, identify source and
c-----------modify f part
            if (dsm2_module .eq. qual) then
               if (cf .eq. ' ') then
                  ctmp='FROM-'
               else
                  ctmp='+FROM-'
               endif
               do j=1, no_of_constituent
                  if (constituents(j).constituent .eq. pathoutput(p).meas_type) then
                     if (constituents(j).loc_name .ne. ' ' .and.
     &                    pathoutput(p).source.loc_name(:lnblnk(pathoutput(p).source.loc_name))
     &                    .eq. constituents(j).loc_name(:lnblnk(constituents(j).loc_name))) then
                        pathoutput(p).const_ndx=j
                        cf=cf(:lnblnk(cf)) // ctmp(:lnblnk(ctmp))
     &                       // constituents(j).loc_name
                     else if (constituents(j).object_no .ne. 0 .and.
     &                       pathoutput(p).source.object_no .eq. constituents(j).object_no) then
                        pathoutput(p).const_ndx=j
                        write(cf,'(a,i3.3)')
     &                       cf(:lnblnk(cf)) // ctmp(:lnblnk(ctmp))
     &                       // 'node:', constituents(j).object_no
                     else if (constituents(j).acct_ndx .ne. 0 .and.
     &                       pathoutput(p).source.acct_ndx .eq. constituents(j).acct_ndx) then
                        pathoutput(p).const_ndx=j
                        cf=cf(:lnblnk(cf)) // ctmp(:lnblnk(ctmp))
     &                       // 'type:' // acct_names(constituents(j).acct_ndx)
                     endif
                  endif
               enddo
            endif
         endif

c--------DSS e part
         if (pathoutput(p).no_intervals .eq. 1 .and.
     &        pathoutput(p).interval(1:5) .eq. '15min') then
            npthsout_min15=npthsout_min15+1
            if (npthsout_min15 .gt. max_out_min) then
               write(unit_error, 652) '15MIN',max_out_min
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_min15
            ptout_min15(npthsout_min15)=p
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:5) .eq. '1hour') then
            npthsout_hour1=npthsout_hour1+1
            if (npthsout_hour1 .gt. max_out_hour) then
               write(unit_error, 652) '1HOUR',max_out_hour
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_hour1
            ptout_hour1(npthsout_hour1)=p
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:4) .eq. '1day') then
            npthsout_day1=npthsout_day1+1
            if (npthsout_day1 .gt. max_out_day) then
               write(unit_error, 652) '1DAY',max_out_day
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_day1
            ptout_day1(npthsout_day1)=p
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:5) .eq. '1week') then
            npthsout_week1=npthsout_week1+1
            if (npthsout_week1 .gt. max_out_week) then
               write(unit_error, 652) '1WEEK',max_out_week
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_week1
            ptout_week1(npthsout_week1)=p
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:4) .eq. '1mon') then
            npthsout_month1=npthsout_month1+1
            if (npthsout_month1 .gt. max_out_month) then
               write(unit_error, 652) '1MON',max_out_month
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_month1
            ptout_month1(npthsout_month1)=p
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:5) .eq. '1year') then
            npthsout_year1=npthsout_year1+1
            if (npthsout_year1 .gt. max_out_year) then
               write(unit_error, 652) '1YEAR',max_out_year
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_year1
            ptout_year1(npthsout_year1)=p
         else                   ! unrecognized interval
            write(unit_error,650) 'output',pathoutput(p).no_intervals,
     &           pathoutput(p).interval
            goto 900
         endif

         path='/'
     &        // ca // '/'      ! a part
     &        // cb // '/'      ! b part
     &        // cc // '/'      ! c part
     &        // '/'            ! d part
     &        // pathoutput(p).interval // '/' ! e part
     &        // cf // '/'      ! f part

         call remblk(path,pathoutput(p).path,nlen)
         call upcase(pathoutput(p).path) ! convert to upper case

c--------check for valid output channel distance
         if (pathoutput(p).object .eq. obj_channel .and.
     &        (pathoutput(p).chan_dist .lt. 0 .or.
     &        pathoutput(p).chan_dist .gt. chan_geom(pathoutput(p).object_no).
     &        length)) then
            write(unit_error,635) pathoutput(p).path(:lnblnk(pathoutput(p).path)),
     &           pathoutput(p).chan_dist
            goto 900
         endif
      enddo                     ! pathoutput loop

c-----process DSS input files
c-----set some DSS parameters
      call zset('MLEVEL','',print_level)

c-----Open the DSS files for reading
      i=1
      do while (i .le. max_dssinfiles .and.
     &     infilenames(i) .ne. ' ')
         call zfname (infilenames(i), ctmp, nlen, lstat)
         if (.not. lstat) then
            write(unit_error, '(a/a/a)') 'Fatal error - DSS input file',
     &           ctmp(:nlen), 'does not exist.'
            goto 900
         endif
         call zopen (ifltab_in(1,i), infilenames(i), istat)
         if (istat .gt. 0) then
            write(unit_error, '(a,a)') 'Unable to open the file ',
     &           infilenames(i)
            goto 900
         endif
         i=i+1
      enddo

      if (dsm2_module .eq. qual .or. dsm2_module .eq. ptm) then
c--------Convert tidefile dates and times to julian minute.

c--------If no start/end date specified in input, use start/end timestamp
c--------in tidefile.
c--------If a tidefile length is exactly equal to the tidal length,
c--------and no explicit end date is given, assume the tidefile will be
c--------recycled (repeated) an integer number of times past the requested
c--------start of the next tidefile, or if this is the the last tidefile,
c--------to the end of the run.  If a tidefile is repeated, the next tidefile's
c--------starting datetime is always considered adjustable so as to start
c--------at the end of this tidefile's recycling, even if the next tidefile's
c--------starting datetime is a specific date.

         nintides=nintides-1
         if (warmup_run) nintides=1

         do i=1,nintides
            call get_tidefile_dates(i)

            repeating_tidefile=
     &           (tide_files(i).end_julmin_file-tide_files(i).start_julmin_file
     &           + tide_files(i).interval) .eq. tide_cycle_length_mins

            if (warmup_run .and. .not. repeating_tidefile) then
               write(unit_error,698)
               goto 900
            endif

c-----------start datetime
            tide_files(i).start_julmin=miss_val_i
            if (tide_files(i).start_dt .eq. ' ') then ! 'runtime': use timestamp in tidefile
               tide_files(i).start_julmin=tide_files(i).start_julmin_file -
     &              tide_files(i).interval
            endif

            if ((tide_files(i).start_dt .eq. 'last' .or. repeating_tidefile)
     &           .and. i .gt. 1) then ! start this after end of previous
               tide_files(i).start_julmin=tide_files(i-1).end_julmin
            endif

            if (.not. repeating_tidefile .and.
     &           tide_files(i).start_julmin .eq. miss_val_i) then ! use specified start datetime if not repeating
               tide_files(i).start_julmin=cdt2jmin(tide_files(i).start_dt)
            endif

            if (tide_files(i).start_julmin .eq. jul_generic_dt .or.
     &           repeating_tidefile .and. i .eq. 1) then ! 'generic': use runtime for tide start
               tide_files(i).start_julmin=start_julmin
            endif

            if (tide_files(i).start_julmin .eq. miss_val_i) then
               write(unit_error,606) 'starting',tide_files(i).start_dt,
     &              tide_files(i).filename(:lnblnk(tide_files(i).filename))
               goto 900
            endif

c-----------end datetime
c-----------a tidefile will be recycled if it is exactly equal to a tidal
c-----------length, and if it is to be used beyond its length
            if (repeating_tidefile) then ! this tidefile may be repeated, if needed
c--------------end datetime of this file will be integer multiple tidecycles
c--------------past specified end or start of next tidefile, or end of run
c--------------jmin is the maximum end datetime to use, before adjusting for tidecyles
               if (index(tide_files(i).end_dt,'len') .eq. 0) then ! use given end datetime
                  jmin=cdt2jmin(tide_files(i).end_dt)
                  if (jmin .eq. miss_val_i) then ! invalid datetime string, maybe it's a time length
                     jmin=incr_intvl(tide_files(i).start_julmin,
     &                    tide_files(i).end_dt, TO_BOUNDARY)
                     if (jmin .eq. miss_val_i) then
                        write(unit_error,606) 'ending',tide_files(i).end_dt,
     &                       tide_files(i).filename(:lnblnk(tide_files(i).filename))
                        goto 900
                     endif
                     jmin=min(jmin, end_julmin)
                  endif
               else             ! tidefile length specified
                  if (i .lt. nintides) then ! figure out next tidefile's start datetime
                     if (tide_files(i+1).start_dt .eq. 'last' .or.
     &                    tide_files(i+1).start_dt .eq. ' ' ) then ! no repeating
                        jmin=tide_files(i).start_julmin +
     &                       (tide_files(i).end_julmin_file -
     &                       tide_files(i).start_julmin_file)
                     else       ! use specified start datetime
                        jmin=cdt2jmin(tide_files(i+1).start_dt)
                     endif
                  else          ! last tidefile
                     jmin=end_julmin
                  endif
               endif
c--------------adjust for integer multiple tidecycles, or end of run
               ntides=(jmin-tide_files(i).start_julmin-1) /
     &              tide_cycle_length_mins + 1
               tide_files(i).end_julmin=min(
     &              tide_files(i).start_julmin +
     &              ntides * tide_cycle_length_mins,
     &              end_julmin)

            else                ! not a repeating tidefile
               if (index(tide_files(i).end_dt,'len') .eq. 0) then ! use given end datetime
                  tide_files(i).end_julmin=cdt2jmin(tide_files(i).end_dt)
                  if (tide_files(i).end_julmin .ne. miss_val_i) then ! valid datetime string inputted
                     tide_files(i).end_julmin=min(cdt2jmin(tide_files(i).end_dt), end_julmin)
                  else          ! invalid datetime string, maybe it's a time length
                     jmin=incr_intvl(tide_files(i).start_julmin,
     &                    tide_files(i).end_dt, TO_BOUNDARY)
                     if (jmin .eq. miss_val_i) then
                        write(unit_error,606) 'ending',tide_files(i).end_dt,
     &                       tide_files(i).filename(:lnblnk(tide_files(i).filename))
                        goto 900
                     endif
                     tide_files(i).end_julmin=min(jmin,end_julmin)
                  endif
               else             ! tidefile length specified
                  tide_files(i).end_julmin=tide_files(i).start_julmin +
     &                 (tide_files(i).end_julmin_file-
     &                 tide_files(i).start_julmin_file) +
     &                 tide_files(i).interval
               endif
            endif
         enddo
      endif

      return

 900  continue                  ! here for fatal error

      istat=-2
      return

 901  continue                  ! here for output file open error
      write(unit_error,601) output_filename, ios
      istat=-1
      return

 905  continue                  ! here for restart open/read file error
      write(unit_error,602) io_files(hydro,io_restart,io_read).filename
     &     (:lnblnk(io_files(hydro,io_restart,io_read).filename))
      istat=-1
      return

      end

      logical function ncc(chemical_name)

c-----Return true if given chemical name is a non-conservative constituent
c-----name.

      implicit none

      include 'common.f'

      integer loccarr           ! location in character array function

      character*(*) chemical_name

      external loccarr

      ncc=loccarr(chemical_name,nonconserve_list,
     &     max_constituent,EXACT_MATCH) .gt. 0

      return
      end

      logical function uniq_constituent(outpath)

c-----Check if the specifications for the outpath determine a unique
c-----constituent.  Unique for conservative means the combination
c-----of chemical constituent name, and one of source name, source
c-----flow type, or source node, are unique; for nonconservative,
c-----only the chemical name need be unique.

      implicit none

c-----argument

      integer
     &     outpath              ! pointer to outputpath structure index

      include 'common.f'

c-----local variables

      logical
     &     ncc                  ! function to determine if this constituent is non-conservative

      integer
     &     loc,loccarr          ! array index; function to find string in char array
     &     ,j,n_uniq            ! loop index, number of uniq c parts

      character*15
     &     uniq_c_list(max_constituent) ! list of unique C parts in DSS outputpaths

      data
     &     uniq_c_list / max_constituent * ' ' /
     &     ,n_uniq /0/

      save uniq_c_list,n_uniq

      loc=loccarr(pathoutput(outpath).meas_type,uniq_c_list,
     &     n_uniq,EXACT_MATCH)
      if (loc .le. 0) then      ! new chemical constituent
         n_uniq=n_uniq+1
         uniq_c_list(n_uniq)=pathoutput(outpath).meas_type
         uniq_constituent=.true.
         return
      else
         if (ncc(pathoutput(outpath).meas_type)) then
            uniq_constituent=.false.
            return
         endif
      endif

c-----not a new chemical, and conservative, now check for
c-----unique combination of source

      do j=1,no_of_constituent
         if (
     &        constituents(j).constituent .eq. pathoutput(outpath).meas_type
     &        .and.
     &        (
     &        (pathoutput(outpath).source.loc_name .ne. ' ' .and.
     &        pathoutput(outpath).source.loc_name .eq. constituents(j).loc_name)
     &        .or.
     &        (pathoutput(outpath).source.object_no .ne. 0 .and.
     &        pathoutput(outpath).source.object_no .eq. constituents(j).object_no)
     &        .or.
     &        (pathoutput(outpath).source.acct_ndx .ne. 0 .and.
     &        pathoutput(outpath).source.acct_ndx .eq. constituents(j).acct_ndx)
     &        )
     &        ) then
c-----------found same combination, not unique
            uniq_constituent=.false.
            return
         endif
      enddo

c-----didn't find same combination
      uniq_constituent=.true.
      return

      end

      subroutine assign_type (
     &     type_spec
     &     ,pathinput
     &     ,ca,cb,cc,ce,cf
     &     ,type
     &     )

c-----assign either an accounting label, or sign, mass fraction, value
c-----change or data flag to an input path

      implicit none

      include 'misc.f'
      include 'defs.f'

c-----arguments and local variables
      record /types_s/ type_spec
      record /pathinput_s/ pathinput

      character
     &     ctmp*150             ! temporary string
     &     ,ca*32,cb*32,cc*32,ce*32,cf*32 ! DSS path parts
     &     ,type*1              ! assignment type:
                                ! a: accounting label
                                ! s: sign
                                ! m: mass fraction
                                ! v: value change
                                ! f: data quality flag
      integer
     &     lnblnk               ! intrinsic last non blank function

c-----which path part to check?
      if (type_spec.part .eq. 'a') then
         ctmp=ca
      else if (type_spec.part .eq. 'b') then
         ctmp=cb
      else if (type_spec.part .eq. 'c') then
         ctmp=cc
      else if (type_spec.part .eq. 'e') then
         ctmp=ce
      else if (type_spec.part .eq. 'f') then
         ctmp=cf
      else if (type_spec.part .eq. 'p') then
         ctmp=pathinput.path
      else if (type_spec.part .eq. 'l') then
         ctmp=pathinput.label
      endif
      call locase(ctmp)
c-----exact or substring match
      if (
     &     (type_spec.match .eq. 'e' .and.
     &     type_spec.string .eq. ctmp) ! exact match
     &     .or.
     &     (type_spec.match .eq. 's' .and.
     &     index(ctmp, type_spec.string
     &     (1:lnblnk(type_spec.string))) .gt. 0) ! substring match
     &     ) then
c--------fixme: allow for overriding to nothing
         if (type .eq. 'a' .and. type_spec.acct_ndx .ne. 0) then
            pathinput.acct_name=type_spec.acct_name
            pathinput.acct_ndx=type_spec.acct_ndx
         else if (type .eq. 's' .and. type_spec.sign .ne. ' ') then
            pathinput.sign=type_spec.sign
         else if (type .eq. 'm' .and. type_spec.mass_frac .ne. miss_val_r) then
            pathinput.mass_frac=type_spec.mass_frac
         else if (type .eq. 'v' .and. type_spec.value_in .ne. miss_val_r) then
            pathinput.value_in=type_spec.value_in
            pathinput.value_out=type_spec.value_out
         else if (type .eq. 'f' .and. type_spec.value_flag .ne. miss_val_r) then
            pathinput.use_flag=type_spec.value_flag
         endif
      endif

      return
      end

      subroutine chain_priority(ndx1, ndx2)

      implicit none

c-----Create replacement priority chains for missing data in input paths.
c-----This assumes priorities are assigned consecutively by the user.

c-----arguments

      integer
     &     ndx1,ndx2            ! pathname indices of two paths to rank

c-----include files
      include 'common.f'

c-----local variables
      integer
     &     priority_diff        ! difference in priorities of the two paths
     &     ,lnblnk              ! intrinsic

 610  format(/'Error: path ',a
     &     /'and path ',a
     &     /'each have the same replacement priority',i2)

 620  format(/'Path ',a
     &     /'will replace missing values in'
     &     /'path ',a)

c-----don't bother with a no-priority path
      if (pathinput(ndx1).priority .eq. 0) return

      priority_diff=pathinput(ndx1).priority - pathinput(ndx2).priority

      if (priority_diff .eq. 0) then
         write(unit_error,610)
     &        pathinput(ndx1).path(:lnblnk(pathinput(ndx1).path)),
     &        pathinput(ndx2).path(:lnblnk(pathinput(ndx2).path)),
     &        pathinput(ndx1).priority
         call exit(2)
      endif

      if (priority_diff .eq. 1) then ! path1 replaces path2
         pathinput(ndx2).replace_path=ndx1
         if (print_level .ge. 3) then
            write(unit_screen,620)
     &           pathinput(ndx1).path(:lnblnk(pathinput(ndx1).path)),
     &           pathinput(ndx2).path(:lnblnk(pathinput(ndx2).path))
         endif
      endif

      return
      end

      integer function data_types(c_part)

c-----Return the type of data (stage, flow, etc) from the
c-----DSS C pathname part.

      implicit none

      include 'misc.f'

      character*(*) c_part      ! DSS C pathname part [INPUT]
      character*32 c_tmp        ! C part, scratch variable
      
      c_tmp=c_part
      call locase(c_tmp)

c-----flow, diversion, drainage
      if (
     &     index(c_tmp,'flow') .gt. 0 .or.
     &     index(c_tmp,'div') .gt. 0 .or.
     &     index(c_tmp,'exp') .gt. 0 .or.
     &     index(c_tmp,'pump') .gt. 0 .or.
     &     index(c_tmp,'evap') .gt. 0 .or.
     &     index(c_tmp,'release') .gt. 0 .or.
     &     index(c_tmp,'drain') .gt. 0 .or.
     &     index(c_tmp,'seep') .gt. 0 .or.
     &     index(c_tmp,'discharge') .gt. 0 .or.
     &     index(c_tmp,'spill') .gt. 0
     &     ) then
         data_types=flow_type
         return
      endif

c-----stage, elevation
      if (
     &     index(c_tmp,'stage') .gt. 0 .or.
     &     index(c_tmp,'level') .gt. 0 .or.
     &     index(c_tmp,'elev') .gt. 0 .or.
     &     index(c_tmp,'surface') .gt. 0 .or.
     &     index(c_tmp,'ws') .gt. 0
     &     ) then
         data_types=stage_type
         return
      endif

c-----gate position
      if (
     &     index(c_tmp,'gate') .gt. 0 .or.
     &     index(c_tmp,'pos') .gt. 0
     &     ) then
         data_types=gate_type
         return
      endif
      
c-----fixme: fill in water quality constituents later

c-----unknown
      data_types=unknown_type

      return

      end
