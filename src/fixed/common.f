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

c-----common blocks for DSM2 I/O

      include 'defs.f'          ! structure definitions

      include 'misc.f'          ! constants, magic numbers

c-----Program name and version number, set in the main routine
c-----of Hydro, Qual and PTM

      character*5 dsm2_name
      character*7 dsm2_version
     &     ,restart_version     ! the version of the program that produced the restart file
     &     ,tidefile_version    ! the version of the program that produced the tidefile
c     &     ,dsm2_build          ! the build number for exact build id

      common /com_c_nv/ dsm2_name
     &                 ,dsm2_version
c     &                ,dsm2_build
     &                 ,restart_version,tidefile_version

c-----runtime identification and run date/time, set in read_fixed
      integer*4
     &     dsm2_module          ! module ID
     &     ,irid                ! run ID as integer
     &     ,irdt                ! run date/time as integer: YYMMDDhhmm
      character
     &     crid*13              ! run ID as character
     &     ,crdt14*14           ! run date/time as character: DDMMMYYYY hhmm
     &     ,crdt10*10           ! run date/time as character: YYMMDDhhmm
      common /com_i_ridt/ dsm2_module, irid, irdt
      common /com_c_ridt/ crid, crdt14, crdt10

c-----input sections structure
      record /form/ hdr_form(max_sections)
      common /com_s_hdr/ hdr_form

c-----titles
      integer
     &     max_titles           ! maximum number of titles allowed
     &     ,ntitles             ! actual number of titles

      parameter (
     &     max_titles=30
     &     )

      character*80
     &     title(max_titles)

      common /com_c_titles/ title
      common /com_i_titles/ ntitles

c-----channels/nodes/cross-sections

      integer
     &     max_channels         ! maximum number of channels
     &     ,max_xsects_tot      ! maximum number of cross sections total
     &     ,max_nodes           ! maximum number of nodes
     &     ,nchans              ! actual number of channels
     &     ,nintnodes           ! actual number of internal nodes
     &     ,nnodes              ! actual number of total nodes
     &     ,nxsects             ! actual number of cross sections
     &     ,max_obj2obj         ! Maximum number of object to object connections
     &     ,nobj2obj            ! Actual number of object to object connections
     &     ,max_stgbnd          ! maximum number of stage boundaries
     &     ,nstgbnd             ! actual number of stage boundaries

      parameter (
     &     max_channels=800     ! MaxChannels should equal this
     &     ,max_xsects_tot=5*max_channels
     &     ,max_nodes=max_channels+10
     &     ,max_obj2obj=50
     &     ,max_stgbnd=5
     &     )

      record /channels_s/ chan_geom(0:max_channels)

      record /nodes_s/ node_geom(0:max_nodes)
      record /xsects_s/ xsect_geom(0:max_xsects_tot)

      real*8
     &     area_tolerance       ! max allowable ratio of virt_area(MSL) @ chan ends
     &     ,levee_slope         ! slope of levees for xsect width extrapolation

      common /com_channels/ chan_geom
      common /com_node/ node_geom
      common /com_xsect/ xsect_geom
      common /numbers_geom/ nchans, nnodes, nintnodes, nxsects

      common /com_r_channels/ area_tolerance, levee_slope

c-----reservoirs

      integer
     &     max_reservoirs       ! maximum number of reservoirs
     &     ,nreser              ! actual number of reservoirs

      parameter (
     &     max_reservoirs=30
     &     )

      record /reservoirs_s/ res_geom(0:max_reservoirs)

      common /com_s_reservoirs/ res_geom
      common /com_i_reservoirs/ nreser

c-----Node id numbers

      integer node_id(0:max_nodes)
      common /node_i_dentification/ node_id

      integer
     &     nchan_list           ! actual number of channel sequences

      integer
     &     int2ext(0:max_channels)
     &     ,resext2int(0:max_reservoirs)
     &     ,resint2ext(0:max_reservoirs)
     &     ,nodelist(0:max_nodes*2+1)

      common /com_i_dsmchan/
     &     nchan_list
     &     ,int2ext,resext2int,resint2ext
     &     ,nodelist

c-----path input (time-varying data)

      integer
     &     max_inputpaths       ! maximum number of input pathnames
     &     ,ninpaths            ! actual number of pathnames

      parameter (
     &     max_inputpaths=4200
     &     )

      logical 
     &     check_input_data     ! true to only check time-varying input data
     &     ,cont_missing        ! true to continue on missing data (use previous value)
     &     ,cont_unchecked      ! true to continue on unchecked data
     &     ,cont_question       ! true to continue on questionable data
     &     ,cont_bad            ! true to continue on bad data
     &     ,warn_missing        ! true to warn about fing data
     &     ,warn_unchecked      ! true to warn about unchecked data
     &     ,warn_question       ! true to warn about questionable data
     &     ,warn_bad            ! true to warn about bad data
     &     ,binary_output       ! true to only generate tmp binary output file w/o converting to text/DSS
     &     ,dss_direct          ! true to write directly to DSS, not using tmp binary files
     &     ,need_tmp_outfiles   ! true if tmp binary files are needed

      record /pathinput_s/ pathinput(0:max_inputpaths)
      common /com_s_inputpath/ pathinput
      common /com_i_inputpath/ ninpaths

      integer max_dssinfiles    ! max number of unique dss input files
      parameter (max_dssinfiles=40)
      character*130 infilenames(max_dssinfiles) ! unique dss input file names
      common /com_c_infile/ infilenames
      integer ifltab_in(600,max_dssinfiles) ! DSS table for each input file
      common /com_i_infile/ ifltab_in
      common /com_l_infile/ check_input_data
     &     ,cont_missing, cont_unchecked, cont_question, cont_bad
     &     ,warn_missing, warn_unchecked, warn_question, warn_bad
     &     ,binary_output,dss_direct,need_tmp_outfiles

c-----printout

      integer,parameter :: max_outputpaths=500  ! maximum number of output pathnames
      integer :: noutpaths                   ! actual number of output pathnames
      integer :: print_level                 ! diagnostic printout level

      character
     &     temp_dir*20          ! directory for temporary files

      record /pathoutput_s/ pathoutput(max_outputpaths)
      common /com_s_outputpath/ pathoutput, temp_dir
      common /com_i_outputpath/ noutpaths,print_level

      integer max_dssoutfiles   ! max number of unique dss output files
      parameter (max_dssoutfiles=10)
      character*130 outfilenames(max_dssoutfiles)
      common /com_c_outfile/ outfilenames
      integer ifltab_out(600,max_dssoutfiles) ! DSS table for each output file
      common /com_i_outfile/ ifltab_out

c-----pseudo environment variables
      integer
     &     max_envvars          ! max number of pseudo (internal) env vars

      parameter (
     &     max_envvars=100
     &     )

      record /envvars_s/ envvars(max_envvars)
      common /com_envvars/ envvars

c-----input/output file names

      integer
     &     max_iogroups         ! number of io groups (hydro, qual, ptm)
     &     ,max_file_types      ! number of types of files (restart, tide, animation,...)

      parameter (
     &     max_iogroups=3
     &     ,max_file_types=7
     &     )

      character*130
     &     output_filename      ! output filename

      record /io_files_s/ io_files(max_iogroups,max_file_types,2)

      common /com_s_io_files/ io_files
      common /com_c_io_files/ output_filename

c-----dates, timestep
c-----Note: julian minutes are minutes from 01jan1900 0000 (31dec1899 2400)
      integer
     &     max_print_dates      ! maximum number of start/stop output date/times
     &     ,nprints             ! number of start/stop output date/times
     &     ,time_step           ! time step in minutes
     &     ,prev_time_step      ! previous time step in minutes

      integer*4
     &     julmin               ! current model time in julian minutes
     &     ,prev_julmin         ! previous model time in julian minutes
     &     ,start_julmin        ! model start time, julian minutes
     &     ,end_julmin          ! model end time, julian minutes
     &     ,jul_generic_date    ! julian minute of generic_dt
     &     ,tf_start_julmin     ! (hydro) when to start writing tidefile, jul mins

      parameter (
     &     max_print_dates=10
     &     )

      character*14 current_date ! current date/time (corresponds to julmin)
     &     ,run_start_date      ! date/time of start of run
     &     ,run_end_date        ! date/time of end of run
     &     ,tf_start_date       ! (hydro) date/time of when to start writing tidefile
      character*14
     &     print_start_date(max_print_dates) ! date/time of print starts
     &     ,print_end_date(max_print_dates) ! date/time of print ends

c-----alternate method: instead of start/end dates, specify run length
c-----in form, e.g. 5day_3hour.  Model will generate end date/times.
      character*80 run_length

c-----time step
      character*80 time_step_intvl_hydro
     &     ,time_step_intvl_qual
     &     ,time_step_intvl_ptm

c-----flush output interval
      character*80 flush_intvl

c-----display time interval
      character*80 display_intvl

      common /com_c_dt/ current_date,run_start_date,run_end_date,run_length,
     &     tf_start_date,
     &     print_start_date,print_end_date,flush_intvl,display_intvl,
     &     time_step_intvl_hydro,time_step_intvl_qual,time_step_intvl_ptm

      common /com_i_dt/ nprints, julmin, prev_julmin, start_julmin,
     &     end_julmin, jul_generic_date, time_step, prev_time_step,
     &     tf_start_julmin

c-----quad points
      integer
     &     nquadpts             ! number of quadrature points

      common /com_i_quad/
     &     nquadpts

c-----hydro repeating tide, warmup run
      logical
     &     repeating_tide       ! true if repeating tide run is desired
     &     ,repeating_tidefile  ! true if tidefile is a repeating tidefile
     &     ,warmup_run          ! true if warmup run desired

      integer
     &     max_tides            ! maximum number of tide cycles
     &     ,tide_cycle_length_mins ! tide cycle length, minutes

      common /com_li_reptide/ repeating_tide, repeating_tidefile,
     &     max_tides, tide_cycle_length_mins, warmup_run

      character*80 tide_cycle_length

      common /com_c_reptide/ tide_cycle_length

      real*8
     &     repeat_stage_tol     ! repeating stage error tolerance
     &     ,repeat_flow_tol     ! repeating flow ratio error tolerance

      common /com_r_reptide/ repeat_stage_tol, repeat_flow_tol

c-----used by virtual_xsect
      real*8 deltax_requested   ! delta x to use in spatial discretization
      common /com_r_dx/ deltax_requested

c-----accounting and object names, value codes, period type names
      integer,parameter :: max_group_memberships=20 ! max number of group memberships

      character*20 obj_names(obj_null) ! names for object codes; set in read_fixed.f
      character*8 per_type_names(per_type_null) ! data type names (e.g. 'PER-AVER')

      common /list_names_c/ obj_names, per_type_names

C-----Direct object to object flow transfer
      record /obj2obj_s/ obj2obj(max_obj2obj)
      common /com_objects/ obj2obj, nobj2obj

c-----stage boundary object
      record /stgbnd_s/ stgbnd(max_stgbnd)
      common /com_stgbnd_s/ stgbnd
      common /com_stgbnd_i/ nstgbnd
