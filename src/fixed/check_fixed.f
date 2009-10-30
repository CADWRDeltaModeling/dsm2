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

      subroutine check_fixed(istat)

c-----Check the fixed input for omissions and errors before starting
c-----the model run.  Supply default values where possible.  Translate
c-----from nodes to channel numbers, and from external channel numbers
c-----to internal.
      use io_units
      
      use gates, only: gateArray, nGate
      use groups, only: groupArray, groupContains,ConvertGroupPatternsToMembers, 
     &                  GROUP_ALL,IsAllChannelReservoir,GroupTarget
      use constants
      use logging
      use common_qual
      use grid_data
      use iopath_data
      use runtime_data
      use common_xsect
      use common_tide
      use envvar
      implicit none


      include '../timevar/dss.inc'
      include '../timevar/readdss.inc'
      include '../timevar/writedss.inc'
      include '../hydrolib/network.inc'
      include '../hydrolib/chconnec.inc'
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
     &     ,nlen                ! character string length
     &     ,n_tidefiles_used    ! number of tide files used to cover simulation period
     &     ,loc,loccarr         ! array index; function to find string in char array
     &     ,data_types          ! function to determine type of data (stage, flow,...)
     &     ,replace_status
      integer*4
     &     cdt2jmin             ! character date/time to julian minute
     &     ,incr_intvl          ! increment julian minute by interval function
     &     ,jmin                ! julian minute


      integer
     &     advance             ! added to the grpindx
     &     ,adjpathout          ! adjusted pathoutput (original npathouts + ngroups)
     &     ,outindx             ! holds value of original npathouts
     &     ,itmp                ! index
      integer ext2int           ! function for converting external channel # to internal

      integer
     &     extchan
     &     ,intchan
     &     ,extupnode
     &     ,extdownnode
     &     ,intupnode
     &     ,intdownnode
     &     ,intgatenode
     &     ,extgatenode

      external data_types

      integer
     &     max_nc               ! max number of non-constituents
     &     ,n_required_do       ! number of required NCCs to model DO
     &     ,n_required_algae    ! number of required NCCs to model algea
     &     ,target_type
     &     ,target_id

      integer :: inode

      parameter(
     &     max_nc=11
     &     ,n_required_do=7
     &     ,n_required_algae=7
     &     )

      character
     &     cdist*10             ! channel distance
     &     ,modifier*32         ! DSS f_part
     &     ,ctmp*150            ! temporary string
     &     ,dsm2_agency*150     ! temp storage for
     &     ,ca*32,cb*32,cc*32,cf*32 ! DSS path parts
     &     ,path*(6*32)         ! temp DSS pathname
     &     ,res_names(max_reservoirs)*32 ! vector of reservoir names
     &     ,const_names(max_constituent)*32 ! vector of constituent names
     &     ,diff2dates*14       ! return DSS date given start and diff
     &     ,jmin2cdt*14         ! julian minute to char function
     &     ,path_constituent*16 ! constituent name for this path
     &     ,non_constituents(max_nc)*16 ! list of inputs that are not constituents
     &     ,required_do(n_required_do)*32 ! list of NCCs required for DO
     &     ,required_algae(n_required_algae)*32 ! list of NCCs required for algae
     &     ,grpnum*3            ! string conversion of group number

      data
     &     constituent_input / max_constituent * .false. /
     &     ,const_names / max_constituent * ' ' /
     &     ,non_constituents /
     &     'stage ',
     &     'flow ',
     &     'flow-net ',
     &     'flow-source ',     
     &     'pump ',
     &     'vel ',
     &     'cloud ',
     &     'dry_bulb ',
     &     'wet_bulb ',
     &     'wind ',
     &     'atm_pressure'
     &     /

 601  format(/'Error opening output file:',a
     &     /'Status value: ',i3,/'(Does the directory exist?)')

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
     &     ' but no node was specified.')

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

 685  format(/'Error: data syncronization requested for this path '
     &     /a/'but the time interval is not minutes, hours, day, or months.')

 690  format(/'Warning: the following input path is not being used,'
     &     /'perhaps because no output was requested of its constituent:'/a)

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
     &     ,buffered='NO'
     &     ,err=901
     &     )

c-----adjust totals
      nprints=nprints-1

c-----generic date in julian minutes
! eli      jul_generic_date=cdt2jmin(generic_date)

c-----run start date and time can be a DSS date (e.g. 01jan1994 0100),
c-----or 'restart' (use date from restart file), or
c-----'tide' (use date from tidefile), or derived from all_run dates

      if (run_start_date(1:7) .eq. 'restart' .or.
     &     run_start_date(11:14) .eq. 'rest') then
c--------get run_start_date from restart file
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
     &        '(36x,a14)',err=905) run_start_date
c--------check for old or new restart version
         if (run_start_date .eq. ' ') then ! new version
            read(io_files(dsm2_module,io_restart,io_read).unit,
     &           '(36x,a14)',err=905) run_start_date
         endif
         close(io_files(dsm2_module,io_restart,io_read).unit)
         write(unit_screen,603) 'restart', run_start_date
      else if (run_start_date(1:3) .eq. 'tid' .or.
     &        run_start_date(11:13) .eq. 'tid') then
         write(unit_error,*)"Start date based on tide not supported"
      else                      ! assume DSS style start date/time
      endif

c-----correct start date for odd minutes (not multiple of 15 minutes)
      start_julmin=cdt2jmin(run_start_date)
	if( start_julmin .ne. (start_julmin/15)*15) then
         write(unit_error,*)"Start time must be aligned with " //
     &     "15MIN interval(0000,0015...)"
	end if
c      start_julmin=(start_julmin/15)*15

c-----calculate ending time if run length, rather than
c-----start/end times are given
      if (run_length .ne. ' ') then
c--------run length should be in form: '20hour' or '5day'
         run_end_date=diff2dates(run_start_date,run_length)
      endif                     ! start/end char dates given
      end_julmin=cdt2jmin(run_end_date)

      if (len_trim(run_start_date) .eq. 0)then
         write(unit_error,*)'Start date missing'
         goto 900
      endif
      if (len_trim(run_end_date) .eq. 0)then
         write(unit_error,*)'End date missing'
         goto 900
      endif

c-----check validity of start and end julian minutes
      if (start_julmin .ge. end_julmin) then
         write(unit_error,"('Starting date: ',a9,
     &        ' equal to or after ending date: ',a9,'or one/both may be missing')")
     &        run_start_date,run_end_date
         goto 900
      endif
      if (start_julmin .eq. miss_val_i) then
         write(unit_error,605) 'Starting',run_start_date
         goto 900
      endif
      if (end_julmin .eq. miss_val_i) then
         write(unit_error,605) 'Ending',run_end_date
         goto 900
      endif

c-----Tidefile date to when to start writing tidefile (hydro)
      if (dsm2_module .eq. hydro) then
         if (tf_start_date .eq. ' ') then
            tf_start_julmin=start_julmin
         else
c-----------correct tf start date for odd minutes (not multiple of tidefile interval)
            tf_start_julmin=cdt2jmin(tf_start_date)
            tf_start_julmin=(tf_start_julmin/15)*15
            tf_start_julmin=max(start_julmin,tf_start_julmin) ! correct for too-soon tf start
         endif
      endif
      tf_start_date = jmin2cdt(start_julmin)

c-----warning fix, until scalar variables fixed
      cont_missing=cont_missing .and. cont_bad


c-----fixme: make sure reservoir is attached to something?

c-----assign rectangular or irregular xsects to upstream,
c-----middle, or downstream of channels;
c-----copy bottom elevations to chan_geom structure

      call process_irreg

c-----identify gate location (up- or down-stream)
c     flowDirect indicates whether gate is upstream or downstream by identifying
c     how the flow convention for the gate (waterbody to node) is oriented with the
c     flow convention for the water body (e.g., upstream to downstream)

      do i=1,ngate
         gateArray(i).flowDirection = 1.0D0 ! this initialization works for gates from reservoirs
         intchan=gateArray(i).objConnectedID
         extchan=chan_geom(intchan).chan_no
         intupnode=chan_geom(intchan).upnode
         intdownnode=chan_geom(intchan).downnode
         extupnode=node_geom(intupnode).node_ID
         extdownnode=node_geom(intdownnode).node_ID
         intgatenode=gateArray(i).node
         extgatenode=node_geom(intgatenode).node_ID
         if (gateArray(i).objConnectedType .eq. obj_channel) then
                                ! fixme: does this still work?
            if (extupnode .eq. extgatenode) then
               gateArray(i).flowDirection = -1.D0
            else if (extdownnode .eq. extgatenode) then
               gateArray(i).flowDirection = 1.D0
            else
               write(unit_error, *)
     &              'Invalid node number for gate ',
     &              trim(gateArray(i).name),
     &              ' channel #', extchan,
     &              ' node #', extgatenode
               goto 900
            endif
c-----------id must be reset to internal channel number.
            gateArray(i).objConnectedID = intchan
         endif
      enddo

c-----reservoir name vectors to be able to use
      do i=1,max_reservoirs
         res_names(i)=res_geom(i).name
      enddo
      
      inode = 0
      do i = 1, nreser
         res_geom(i).first_connect_index = inode + 1
         do j = 1,res_geom(i).nnodes
             inode = inode +1
         end do
      end do
      

      do i=1,noutpaths          ! output paths   
         if (pathoutput(i).use) then
c-----------change stage output at node to channel/distance
c-----------fixme: this
c-----------could be dangerous near gates. If upstream(1) isn't
c-----------guaranteed to be a reference channel for the gate. It is
c-----------also married to the "equal stage" compatibility condition
c-----------which wasn't always used in FourPt, isn't used in the Fisher
c-----------Delta Model (both consider equal energy instead).
c-----------Why is allowing stage at a node important
c-----------given that it is more ambiguous, redundant.
            if (pathoutput(i).obj_type .eq. obj_node)
     &           then
               if (pathoutput(i).meas_type .eq. 'stage') then
                  pathoutput(i).obj_type=obj_channel
                  if (node_geom(pathoutput(i).obj_no).nup .gt. 0) then
                     pathoutput(i).obj_no=
     &                    node_geom(pathoutput(i).obj_no).upstream(1)
                     pathoutput(i).chan_dist=0
                  else
                     pathoutput(i).obj_no=
     &                    node_geom(pathoutput(i).obj_no).downstream(1)
                     pathoutput(i).chan_dist=chan_length
                  endif
               end if
               

c--------------try to change flow or velocity output at node to channel/distance:
c--------------must have only two channels connecting

               if (pathoutput(i).meas_type .eq. 'flow' .or.
     &              pathoutput(i).meas_type .eq. 'vel') then
                  if (node_geom(pathoutput(i).obj_no).nup .eq. 1) then
                     pathoutput(i).obj_type=obj_channel
                     pathoutput(i).obj_no=node_geom(pathoutput(i).obj_no).upstream(1)
                     pathoutput(i).chan_dist=0
                  else if (node_geom(pathoutput(i).obj_no).ndown .eq. 1) then
                     pathoutput(i).obj_type=obj_channel
                     pathoutput(i).obj_no=node_geom(pathoutput(i).obj_no).downstream(1)
                     pathoutput(i).chan_dist=chan_length
                  else
c--------------------can't have flow/vel at a node with multiple channels
                     write(unit_error,671)
     &                    trim(pathoutput(i).name),
     &                    trim(pathoutput(i).meas_type),
     &                    trim(pathoutput(i).interval),
     &                    trim(pathoutput(i).modifier),
     &                    pathoutput(i).obj_no
                     goto 900
                  endif
               end if
            endif

c-----------check reservoir output
            if (pathoutput(i).name .ne. ' ' .and.
     &           pathoutput(i).obj_type .eq. obj_reservoir) then
               if (pathoutput(i).meas_type .eq. 'stage') then
                  pathoutput(i).res_node_no=0
               endif
c--------------check for valid reservoir name
               loc=loccarr(pathoutput(i).obj_name,res_names,max_reservoirs,
     &              EXACT_MATCH)
               if (loc .le. 0) then ! no reservoir with that name
                  write(unit_error, 610) 'reservoir',
     &                 trim(pathoutput(i).obj_name)
                  goto 900
               else
                  pathoutput(i).obj_no=loc
                  if (pathoutput(i).res_node_no .gt. 0) then
c--------------------check that if a node was given, flow was specified
                     if (pathoutput(i).meas_type .ne. 'flow' ) then
                        write(unit_error, 620) trim(pathoutput(i).obj_name)
                        goto 900
                     endif
c--------------------check for valid node number
                     if (node_geom(pathoutput(i).res_node_no).nup +
     &                    node_geom(pathoutput(i).res_node_no).ndown
     &                    .eq. 0) then
                        write(unit_error, 630) pathoutput(i).obj_name,
     &                       pathoutput(i).res_node_no
                        goto 900
                     endif
c                  else
c                     if( 
c     &                   pathoutput(i).meas_type .ne. 'flow-net' .and.
c     &                   pathoutput(i).meas_type .ne. 'flow-source' ) then
c                         write(unit_error, *) 
c     &                      "Only flow-net or flow-source output allowed with no node='none': ",
c     &                      trim(pathoutput(i).obj_name)
c                         goto 900
c                     endif                           
                  endif
               endif

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
         if (pathinput(p).no_intervals .eq. 1 .and.
     &        pathinput(p).interval .eq. '15min') then ! eli
            npthsin_min15=npthsin_min15+1
            if (npthsin_min15 .gt. max_inp_min) then
               write(unit_error, 651) '15MIN',max_inp_min
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_min15
            ptin_min15(npthsin_min15)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).interval(:5) .eq. '1hour') then !eli could be 1hour
            npthsin_hour1=npthsin_hour1+1
            if (npthsin_hour1 .gt. max_inp_hour) then
               write(unit_error, 651) '1HOUR',max_inp_hour
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_hour1
            ptin_hour1(npthsin_hour1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).interval(:4) .eq. '1day') then
            npthsin_day1=npthsin_day1+1
            if (npthsin_day1 .gt. max_inp_day) then
               write(unit_error, 651) '1DAY',max_inp_day
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_day1
            ptin_day1(npthsin_day1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).interval(:5) .eq. '1week') then
            npthsin_week1=npthsin_week1+1
            if (npthsin_week1 .gt. max_inp_week) then
               write(unit_error, 651) '1WEEK',max_inp_week
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_week1
            ptin_week1(npthsin_week1)=p
         else if (pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).interval(:4) .eq. '1mon') then
            npthsin_month1=npthsin_month1+1
            if (npthsin_month1 .gt. max_inp_month) then
               write(unit_error, 651) '1MON',max_inp_month
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_month1
            ptin_month1(npthsin_month1)=p
         else if ((pathinput(p).no_intervals .eq. 1 .and.
     &           pathinput(p).interval(:5) .eq. '1year') .or.
     &           pathinput(p).constant_value .ne. miss_val_r ! constant value: use 1year
     &           ) then
            pathinput(p).no_intervals=1
            pathinput(p).interval='year'
            npthsin_year1=npthsin_year1+1
            if (npthsin_year1 .gt. max_inp_year) then
               write(unit_error, 651) '1YEAR',max_inp_year
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_year1
            ptin_year1(npthsin_year1)=p
         else if (pathinput(p).interval(:3) .eq. 'ir-') then ! irregular interval
            npthsin_irr=npthsin_irr+1
            if (npthsin_irr .gt. max_inp_irr) then
               write(unit_error, 651) 'IR-',max_inp_irr
               goto 900
            endif
            pathinput(p).intvl_path=npthsin_irr
            ptin_irr(npthsin_irr)=p
         else                   ! unrecognized interval
            write(unit_error,650) 'input', pathinput(p).no_intervals,
     &           trim(pathinput(p).interval)
            goto 900
         endif
         call upcase(pathinput(p).path) ! convert to upper case
      end do

      ! Todo: ???
      dsm2_agency = ' '
      replace_status=0
      !replace_status=replace_envvars('$(DSM2AGENCY)', dsm2_agency)
      



      n_tidefiles_used = 0
      if (dsm2_module .eq. qual .or. dsm2_module .eq. ptm) then
c--------Convert tidefile dates and times to julian minute.
c--------If no start/end date specified in input, use start/end timestamp
c--------in tidefile.

         nintides=nintides-1
         if (nintides .le. 0) then
            write(unit_error, '(a)') 'No input tides given, run stopped.'
            goto 900
         endif
         do i=1,nintides
	      n_tidefiles_used = n_tidefiles_used + 1
            call get_tidefile_dates(i)


c-----------start datetime
            tide_files(i).start_julmin=miss_val_i
            if (tide_files(i).start_date .eq. ' ') then ! 'runtime': use timestamp in tidefile
               tide_files(i).start_julmin=tide_files(i).start_julmin_file
            endif

            if (tide_files(i).start_date .eq. 'last' 
     &           .and. i .gt. 1) then ! start this after end of previous
               tide_files(i).start_julmin=tide_files(i-1).end_julmin
            endif

            if (tide_files(i).start_julmin .eq. miss_val_i) then 
               ! use specified start datetime if not repeating
               tide_files(i).start_julmin=cdt2jmin(tide_files(i).start_date)
            endif

	      tide_files(i).start_julmin = max(tide_files(i).start_julmin,start_julmin)

            if (tide_files(i).start_julmin .eq. miss_val_i) then
               write(unit_error,606) 'starting',tide_files(i).start_date,
     &              trim(tide_files(i).filename)
               goto 900
            endif

c-----------end datetime
            if (index(tide_files(i).end_date,'length') .eq. 0) then 
               ! use given end datetime
               tide_files(i).end_julmin=cdt2jmin(tide_files(i).end_date)
               if (tide_files(i).end_julmin .ne. miss_val_i) then 
                  ! valid datetime string input
                  tide_files(i).end_julmin=
     &               min(cdt2jmin(tide_files(i).end_date), end_julmin)
               else          ! invalid datetime string, maybe it's a time length
                  jmin=incr_intvl(tide_files(i).start_julmin,
     &                 tide_files(i).end_date, TO_BOUNDARY)
                  if (jmin .eq. miss_val_i) then
                     write(unit_error,606) 'ending',tide_files(i).end_date,
     &                    trim(tide_files(i).filename)
                     goto 900
                  endif
                  tide_files(i).end_julmin=min(jmin,end_julmin)
               endif
            else             ! use through length of tidefile
               tide_files(i).end_julmin=tide_files(i).end_julmin_file
            endif
            if (tide_files(i).start_julmin .lt. tide_files(i).start_julmin_file 
     &          .or.
     &          tide_files(i).end_julmin .gt. tide_files(i).end_julmin_file) then
	         write(unit_error,*)"Tidefile contents do not span " //
     &             "assigned start and end dates: ", tide_files(i).filename
	         goto 900
	      end if

	      ! This exit statement allows nonexistent tidefiles to be listed
	      if (tide_files(i).end_julmin .ge. end_julmin) exit  
         enddo
c----- load header information from the first hydro tidefile
         call read_tide_head(tide_files(1).filename, .false.)
         nintides = n_tidefiles_used
	   if (nintides .gt. 1) then 
           do i=2,nintides
              if (tide_files(i).start_julmin .ne. tide_files(i-1).end_julmin) then
	           write(unit_error,*) "Tidefile dates must be ordered in time, " 
     &                 // "with no gaps or overlap in start/end dates"
	           goto 900
	        end if
	     end do
	   end if
	   if (  tide_files(1).start_julmin .gt. start_julmin 
     &      .or. tide_files(nintides).end_julmin .lt. end_julmin) then
	       write(unit_error,*)"Specified dates for tidefiles do not cover period of simulation"
c	       write(unit_error,*)"Tidefile coverage: " // 
c     &		       tide_files(1).start_date // " to " //
c     &                tide_files(nintides).end_date
	       goto 900
	    end if
      endif


c---- convert group members from patterns to actual objects&indexes
c     This must come after tidefile is loaded
	
      call ConvertGroupPatternsToMembers



      if (dsm2_module .eq. qual) then
c--------Conservative and nonconservative constituents.
c--------For conservative constituents (CCs), note that a 'constituent' is
c--------really each unique combination of constituent and source group, while for
c--------non-conservative constituents (NCCs), each constituent type
c--------is considered unique, regardless of source, since
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
            if (loc .le. 0) then ! not a "non-constituent" - must be a CC or NCC water quality constituent
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
                  constituents(no_of_constituent).name=
     &                 pathoutput(p).meas_type
                  constituents(no_of_constituent).conservative=
     &                 .not. ncc(pathoutput(p).meas_type)
                  constituents(no_of_constituent).group_ndx=
     &                 pathoutput(p).source_group_ndx

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
     &              loccarr(constituents(i).name,const_names,
     &              no_of_constituent, EXACT_MATCH) .le. 0) then
c-----------------conservative chemical not found before
                  j=j+1
                  const_names(j)=constituents(i).name
c-----------------check if an 'all' source already exists for this one
                  do k=i,no_of_constituent
                     if ( (constituents(i).name .eq. constituents(k).name)  .and.
     &                    constituents(k).group_ndx .eq. GROUP_ALL) then
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
                  constituents(no_of_constituent).name=
     &                 constituents(i).name
                  constituents(no_of_constituent).conservative=.true.
                  constituents(j).group_ndx=GROUP_ALL
 800              continue      ! here to skip making 'all' source
               endif
            enddo
         endif

c--------add needed constituents for DO and algae
c--------passing slices of structures in calls, chokes
         do i=1,max_constituent
            const_names(i)=constituents(i).name
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
                  constituents(no_of_constituent).name=required_do(i)
                  constituents(no_of_constituent).conservative=.false.
!fixme:groups is this right?
                  constituents(no_of_constituent).group_ndx = GROUP_ALL
                  no_of_nonconserve_constituent=no_of_nonconserve_constituent+1
                  nonconserve_ptr(no_of_nonconserve_constituent)=no_of_constituent
               endif
            enddo
         endif


c--------update const_names if any constituent/s were asked for simulation
c--------which already fall within the group required for DO simulation
         do i=1,max_constituent
            const_names(i)=constituents(i).name
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
                  constituents(no_of_constituent).name=required_algae(i)
                  constituents(no_of_constituent).conservative=.false.
                                !fixme:groups is this right?
                  constituents(no_of_constituent).group_ndx=GROUP_ALL
                  no_of_nonconserve_constituent=no_of_nonconserve_constituent+1
                  nonconserve_ptr(no_of_nonconserve_constituent)=no_of_constituent
               endif
            enddo
         endif

c--------pathinput().const_ndx keeps track of multiple source
c--------constituents which may come from the same input (applies only to conservative).

         do p=1,ninpaths
            pathinput(p).n_consts=0
            path_constituent=pathinput(p).variable
            do j=1,no_of_constituent  
               if (path_constituent .eq. constituents(j).name) then
                  if (
     &                 (.not. constituents(j).conservative)
     &                 .or.
     &                 (constituents(j).group_ndx .eq. GROUP_ALL)) then
                     pathinput(p).n_consts=pathinput(p).n_consts+1
                     pathinput(p).const_ndx(pathinput(p).n_consts) = j
                     constituent_input(j)=.true.
  	            else
	              call GroupTarget(pathinput(p).data_type,pathinput(p).name,
     &                               target_type,target_id)
	              if (target_id .eq. miss_val_i) then  
	                  !fixme: stage boundaries not identified in qual at this stage??
	                  call GroupTarget(obj_stage,pathinput(p).name, target_type,target_id)
	              end if
	              if (target_id .eq. miss_val_i) then
	                  write(unit_error,*) "Source group not found: ",pathinput(p).name
	                  call exit(2)
	              end if
                    if (constituents(j).group_ndx .lt. 0) then
                       write(unit_error,*) "Error with constituent group index"
                       write(unit_error,*) "Constituent index: ",j,
     &                               " Name: ", constituents(j).name, 
     &                               "Group index: ",constituents(j).group_ndx
                       call exit(2)
                    end if
                       
	              if (GroupContains(constituents(j).group_ndx,
     &                                target_type,target_id)) then
                      pathinput(p).n_consts=pathinput(p).n_consts+1
                      pathinput(p).const_ndx(pathinput(p).n_consts) = j
                      constituent_input(j)=.true.
	              end if
                  endif
               endif
            enddo
c-----------generate warnings about input constituents that aren't used
            loc=loccarr(path_constituent,non_constituents,max_nc,
     &           EXACT_MATCH)
            if (pathinput(p).n_consts .eq. 0 .and. loc .le. 0) then
               write(unit_error,690) trim(pathinput(p).path)
            endif
         enddo

c--------create index array of constituents from all sources
         no_all_source=0
         do i=1,no_of_constituent
            if (constituents(i).group_ndx .eq. GROUP_ALL) then
               no_all_source=no_all_source+1
               all_source_ptr(no_all_source)=i
            endif
         enddo
      endif

c-----do tracked constituents have inputs?
      do j=1,no_of_constituent
         if (.not. constituent_input(j)) then
            write(unit_error,631)
     &           trim(constituents(j).name)
     &           ,trim(groupArray(constituents(j).group_ndx).name)
 631        format(/'Warning:   Output constituent does not have any matching input.'/
     &           'Check input and output to make sure constituent name is not misspelled.'/
     &           'This is either an error or the constituent is totally dependent on initial condition.'/
     &           'Requested output constituent: ',a,'; source group: ',a)

         endif
      enddo

c-----create DSS output pathnames, check for sign change for each path
c-----change channel distances to marker if == channel length
      do p=1,noutpaths
         pathoutput(p).need_tmp_outfile=.false.
c--------remove output file, if text, and flag if tmp output files are needed
         if (index(pathoutput(p).filename,'.dss') .eq. 0) then
            call unlink(pathoutput(p).filename)
            need_tmp_outfiles=.true.
            pathoutput(p).need_tmp_outfile=.true.
         else                   ! .DSS file
            if (.not. dss_direct) then
               need_tmp_outfiles=.true.
               pathoutput(p).need_tmp_outfile=.true.
            endif
         endif
c--------replace magic number channel length with correct channel length
         if (pathoutput(p).chan_dist .eq. chan_length)
     &        pathoutput(p).chan_dist =
     &        chan_geom(pathoutput(p).obj_no).length

c-------replace op-to-node with op_to_node
         if (pathoutput(p).meas_type .eq. 'op-to-node')
     &       pathoutput(p).meas_type = 'op_to_node'
         if (pathoutput(p).meas_type .eq. 'op-from-node')
     &       pathoutput(p).meas_type = 'op_from_node'



c--------DSS a part
         if (pathoutput(p).a_part .ne. ' ') then
            ca=pathoutput(p).a_part
         else                   ! not explicitly given, use IEP format
c-----------ctmp='DSM2-' // trim(dsm2_name) // '-' //
c-----------&           dsm2_version
            ctmp= trim(dsm2_name) // dsm2_version
            ca=ctmp
c-----------if (pathoutput(p).obj_type .eq. obj_channel) then
c-----------ca=trim(ctmp) // '+CHAN'
c-----------else if (pathoutput(p).obj_type .eq. obj_node) then
c-----------ca=trim(ctmp) // '+NODE'
c-----------else if (pathoutput(p).obj_type .eq. obj_reservoir) then
c-----------ca=trim(ctmp) // '+RSVR'
c-----------else if (pathoutput(p).obj_type .eq. obj_gate) then
c-----------ca=trim(ctmp) // '+GATE'
c-----------else if (pathoutput(p).obj_type .eq. obj_qext) then
c-----------ca=trim(ctmp) // '+QEXT'
c-----------else if (pathoutput(p).obj_type .eq. obj_obj2obj) then
c-----------ca=trim(ctmp) // '+OBJ2OBJ'
c-----------else if (pathoutput(p).obj_type .eq. obj_flux) then
c-----------ca=trim(ctmp) // '+FLUX'
c-----------else if (pathoutput(p).obj_type .eq. obj_stage) then
c-----------ca=trim(ctmp) // '+STAGE'
c-----------else
c-----------ca=trim(ctmp) // '+UNK'
c-----------endif
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
               if (pathoutput(p).obj_type .eq. obj_reservoir .and.
     &              pathoutput(p).res_node_no .gt. 0) then
                  write(ctmp,'(i3)') node_geom(pathoutput(p).res_node_no).node_id
                  cb=trim(cb) // '-NODE' // trim(ctmp)
               endif
            else                ! use chan/dist; node number
               if (pathoutput(p).obj_type .eq. obj_channel) then
                  write(cdist,'(i10)') pathoutput(p).chan_dist
                  write(cb,'(i3.3,''_'',a)') pathoutput(p).obj_no,cdist
               else if (pathoutput(p).obj_type .eq. obj_node) then
                  write(cb,'(i3)') node_geom(pathoutput(p).obj_no).node_ID
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
            cf=trim(dsm2_agency)

c-----------optional modifier (study name, etc)
            if (pathoutput(p).modifier .ne. ' ') then
               modifier=pathoutput(p).modifier
            else
               replace_status=replace_envvars('$(DSM2MODIFIER)', ctmp)
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

            if (modifier .ne. ' ' .and.
     &           cf .ne. ' ') then
               modifier='+' // modifier
            endif

            cf=trim(cf)     // modifier

c-----------if output is water quality constituent, identify source and
c-----------modify f part
            if (dsm2_module .eq. qual) then
               if (cf .eq. ' ') then
                  ctmp='FROM-'
               else
                  ctmp='+FROM-'
               endif
               do j=1, no_of_constituent
                  if (constituents(j).name .eq. pathoutput(p).meas_type) then
                     if (pathoutput(p).source_group_ndx .eq. constituents(j).group_ndx) then
                        pathoutput(p).const_ndx=j
                        cf=trim(cf) // trim(ctmp)
     &                       // groupArray(constituents(j).group_ndx).name
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
            if (pathoutput(p).need_tmp_outfile) then
               need_tmpfile_min15=.true.
            endif
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:5) .eq. '1hour') then
            npthsout_hour1=npthsout_hour1+1
            if (npthsout_hour1 .gt. max_out_hour) then
               write(unit_error, 652) '1HOUR',max_out_hour
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_hour1
            ptout_hour1(npthsout_hour1)=p
            if (pathoutput(p).need_tmp_outfile) need_tmpfile_hour1=.true.
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:4) .eq. '1day') then
            npthsout_day1=npthsout_day1+1
            if (npthsout_day1 .gt. max_out_day) then
               write(unit_error, 652) '1DAY',max_out_day
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_day1
            ptout_day1(npthsout_day1)=p
            if (pathoutput(p).need_tmp_outfile) need_tmpfile_day1=.true.
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:5) .eq. '1week') then
            npthsout_week1=npthsout_week1+1
            if (npthsout_week1 .gt. max_out_week) then
               write(unit_error, 652) '1WEEK',max_out_week
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_week1
            ptout_week1(npthsout_week1)=p
            if (pathoutput(p).need_tmp_outfile) need_tmpfile_week1=.true.
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:4) .eq. '1mon') then
            npthsout_month1=npthsout_month1+1
            if (npthsout_month1 .gt. max_out_month) then
               write(unit_error, 652) '1MON',max_out_month
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_month1
            ptout_month1(npthsout_month1)=p
            if (pathoutput(p).need_tmp_outfile) need_tmpfile_month1=.true.
         else if (pathoutput(p).no_intervals .eq. 1 .and.
     &           pathoutput(p).interval(1:5) .eq. '1year') then
            npthsout_year1=npthsout_year1+1
            if (npthsout_year1 .gt. max_out_year) then
               write(unit_error, 652) '1YEAR',max_out_year
               goto 900
            endif
            pathoutput(p).intvl_path=npthsout_year1
            ptout_year1(npthsout_year1)=p
            if (pathoutput(p).need_tmp_outfile) need_tmpfile_year1=.true.
         else                   ! unrecognized interval
            write(unit_error,650) 'output',pathoutput(p).no_intervals,
     &           pathoutput(p).interval
            goto 900
         endif

         path='/'
     &        // trim(ca) // '/' ! a part
     &        // trim(cb) // '/' ! b part
     &        // trim(cc) // '/' ! c part
     &        // '/'            ! d part
     &        // trim(pathoutput(p).interval) // '/' ! e part
     &        // trim(cf) // '/' ! f part

         call remblk(path,pathoutput(p).path,nlen)
         call upcase(pathoutput(p).path) ! convert to upper case
         call zchkpn(trim(path),len_trim(path),istat)
         if (istat .ne. 0) then
            write(unit_error,"(a,a,a,i5)")"Illegal pathname: ",
     &           trim(path)," status: ",istat
            goto 900
         end if

c--------check for valid output channel distance
         if (pathoutput(p).obj_type .eq. obj_channel) then
            if(pathoutput(p).chan_dist .eq. -1) then
               pathoutput(p).chan_dist = chan_geom(pathoutput(p).obj_no).length
            else if(pathoutput(p).chan_dist .gt.
     &              chan_geom(pathoutput(p).obj_no).length) then
               write(unit_error,635) trim(pathoutput(p).path),
     &              pathoutput(p).chan_dist
               goto 900
            end if
         endif
      enddo                     ! pathoutput loop

c-----process DSS input files
c-----set some DSS parameters
      call zset('MLEVEL','',print_level)

c-----Open the DSS files for reading
      i=1
      do while (i .le. max_dssinfiles .and.
     &     infilenames(i) .ne. ' ')
         call zfname (trim(infilenames(i)), ctmp, nlen, lstat)
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

      return

 900  continue                  ! here for fatal error

      istat=-2
      return

 901  continue                  ! here for output file open error
      write(unit_error,601) output_filename, ios
      istat=-1
      return

 905  continue                  ! here for restart open/read file error
      write(unit_error,602) trim(io_files(hydro,io_restart,io_read).filename)
      istat=-1
      return

      end subroutine


c     Convert an external channel number to internal number
c     using a binary search. 
      integer function ext2int(extchan)
      use ifport
      use grid_data
      implicit none
      integer extchan
      ext2int=bsearchqq(loc(extchan),loc(int2ext(1)),nchans,SRT$INTEGER4)
      return
      end function

c     Compare two integers (e.g., as needed for qsort)
      integer(2) function compareInt(arg1, arg2)
      implicit none
      integer arg1,arg2
      compareInt=arg1-arg2
      return
      end function

c-----Convert an external node number to an internal one using
c     binary search
      integer function ext2intnode(extnode)
      use dflib
      use grid_data
      implicit none
      integer extnode
      ext2intnode=bsearchqq(loc(extnode),loc(nodelist(1)),
     &                      nintnodes,SRT$INTEGER4)
      if (ext2intnode .gt. 0)return
      ext2intnode=bsearchqq(loc(extnode),loc(nodelist(nintnodes+1)),
     &     (nnodes-nintnodes),SRT$INTEGER4)+nintnodes
      if (ext2intnode .lt. 0) ext2intnode = miss_val_i
      return
      end function










      logical function ncc(chemical_name)

c-----Return true if given chemical name is a non-conservative constituent
c-----name.

      use common_qual
      use constants
      implicit none

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
c-----of chemical constituent name, and one of group, source
c-----flow type, or source location, are unique; for nonconservative,
c-----only the chemical name need be unique.
      use common_qual
      use iopath_data
      use constants
      implicit none

c-----argument

      integer
     &     outpath              ! pointer to outputpath structure index

      

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
      
	uniq_constituent=.false.
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
c-----different group

      do j=1,no_of_constituent
         if (
     &        (constituents(j).name .eq. pathoutput(outpath).meas_type)
     &        .and.
     &        (pathoutput(outpath).source_group_ndx .eq. constituents(j).group_ndx)
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













