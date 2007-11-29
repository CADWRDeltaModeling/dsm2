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


c-----common blocks for DSM2 I/O

      include 'defs.f'          ! structure definitions

      include 'misc.f'          ! constants, magic numbers

c-----Program name and version number, set in the main routine
c-----of Hydro, Qual and PTM

      character*5 dsm2_name
      character*7 dsm2_version
     &     ,restart_version     ! the version of the program that produced the restart file
     &     ,tidefile_version    ! the version of the program that produced the tidefile

      common /com_c_nv/ dsm2_name, dsm2_version,restart_version,tidefile_version

c-----runtime identification and run date/time, set in read_fixed
      integer*4
     &     dsm2_module          ! module ID
     &     ,irid                ! run ID as integer
     &     ,irdt                ! run date/time as integer: YYMMDDhhmm
      character
     &     crid*6               ! run ID as character
     &     ,crdt14*14           ! run date/time as character: DDMMMYYYY hhmm
     &     ,crdt10*10           ! run date/time as character: YYMMDDhhmm
      common /com_i_ridt/ dsm2_module, irid, irdt
      common /com_c_ridt/ crid, crdt14, crdt10

c-----input sections structure
      record /form/ hdr_form(max_sections)
      integer gates_section     ! which header section is for gates
      common /com_s_hdr/ hdr_form
      common /com_i_hdr_gates/ gates_section

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
     &     ,nnodes              ! actual number of nodes
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
      common /numbers_geom/ nchans, nnodes, nxsects

      common /com_r_channels/ area_tolerance, levee_slope

c-----reservoirs

      integer
     &     max_reservoirs       ! maximum number of reservoirs
     &     ,nreser              ! actual number of reservoirs

      parameter (
     &     max_reservoirs=25
     &     )

      record /reservoirs_s/ res_geom(0:max_reservoirs)

      common /com_s_reservoirs/ res_geom
      common /com_i_reservoirs/ nreser

c-----gates

      integer
     &     max_gates            ! maximum number of gates
     &     ,ngates              ! actual number of gates

      parameter (
     &     max_gates=50
     &     )

      integer hydrogates(max_gates) ! dsm2-hydrogate numbers

      record /gates_s/ gate_geom(0:max_gates)
      common /com_s_gates/ gate_geom
      common /com_i_gates/ ngates,hydrogates

c-----Node id numbers

      integer node_id(0:max_nodes)
      common /node_i_dentification/ node_id

      integer
     &     nchan_list           ! actual number of channel sequences

      integer
     &     int2ext(0:max_channels)
     &     ,ext2int(0:max_channels)
     &     ,nodedsm2qual(0:max_nodes)
     &     ,nodequal2dsm(0:max_nodes)

      common /com_i_dsmchan/
     &     nchan_list
     &     ,int2ext,ext2int
     &     ,nodedsm2qual
     &     ,nodequal2dsm

c-----DSS interval information
      integer*4
     &     jmin_15min           ! julian minute of end-of-period for 15MIN data
     &     ,jmin_1hour
     &     ,jmin_1day
     &     ,jmin_1week
     &     ,jmin_1month
     &     ,jmin_1year
     &     ,jmin_15min_prev     ! previous value of jmin_15min
     &     ,jmin_1hour_prev
     &     ,jmin_1day_prev
     &     ,jmin_1week_prev
     &     ,jmin_1month_prev
     &     ,jmin_1year_prev

      common /dss_intvl_i/
     &     jmin_15min           ! julian minute of end-of-period for 15MIN data
     &     ,jmin_1hour
     &     ,jmin_1day
     &     ,jmin_1week
     &     ,jmin_1month
     &     ,jmin_1year
     &     ,jmin_15min_prev     ! previous value of jmin_15min
     &     ,jmin_1hour_prev
     &     ,jmin_1day_prev
     &     ,jmin_1week_prev
     &     ,jmin_1month_prev
     &     ,jmin_1year_prev

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
     &     ,warn_missing        ! true to warn about missing data
     &     ,warn_unchecked      ! true to warn about unchecked data
     &     ,warn_question       ! true to warn about questionable data
     &     ,warn_bad            ! true to warn about bad data

      record /pathinput_s/ pathinput(0:max_inputpaths)
      common /com_s_inputpath/ pathinput
      common /com_i_inputpath/ ninpaths

      integer max_dssinfiles    ! max number of unique dss input files
      parameter (max_dssinfiles=40)
      character*130 infilenames(max_dssinfiles) ! unique dss input file names
      common /com_c_infile/ infilenames
      integer ifltab_in(400,max_dssinfiles) ! DSS table for each input file
      common /com_i_infile/ ifltab_in
      common /com_l_infile/ check_input_data
     &     ,cont_missing, cont_unchecked, cont_question, cont_bad
     &     ,warn_missing, warn_unchecked, warn_question, warn_bad

c-----printout

      integer
     &     max_outputpaths      ! maximum number of output pathnames
     &     ,noutpaths           ! actual number of output pathnames
     &     ,print_level         ! diagnostic printout level

      character
     &     temp_dir*20          ! directory for temporary files

      parameter (
     &     max_outputpaths=500
     &     )

      record /pathoutput_s/ pathoutput(max_outputpaths)
      common /com_s_outputpath/ pathoutput, temp_dir
      common /com_i_outputpath/ noutpaths,print_level

      integer max_dssoutfiles   ! max number of unique dss output files
      parameter (max_dssoutfiles=10)
      character*130 outfilenames(max_dssoutfiles)
      common /com_c_outfile/ outfilenames
      integer ifltab_out(400,max_dssoutfiles) ! DSS table for each output file
      common /com_i_outfile/ ifltab_out

c-----pseudo environment variables
      integer
     &     max_envvars          ! max number of pseudo (internal) env vars

      parameter (
     &     max_envvars=50
     &     )

      record /envvars_s/ envvars(max_envvars)
      common /com_envvars/ envvars

c-----input/output file names

      integer
     &     max_iogroups         ! number of io groups (hydro, qual, ptm)
     &     ,max_file_types      ! number of types of files (restart, tide, animation,...)

      parameter (
     &     max_iogroups=3
     &     ,max_file_types=6
     &     )

      character*130
     &     output_filename      ! output filename

      record /io_files_s/ io_files(max_iogroups,max_file_types,2)

      common /com_s_io_files/ io_files
      common /com_c_io_files/ output_filename

c-----translations:
c-----location name --> chan/dist pair, node, or reservoir name

      integer
     &     max_translations     ! maximum number of translated locations
     &     ,ntrans              ! actual number of translations

      parameter (
     &     max_translations=500
     &     )

      record /trans_s/ translations(0:max_translations)
      common /com_s_translations/ translations
      common /com_i_translations/ ntrans

c-----assign type:
c-----allow user to assign types to input paths
c-----e.g. specify which paths should have sign changed on
c-----input values; or specify if an outgoing flow is a diversion,
c-----evaporation, or seepage

      integer
     &     max_types            ! maximum number of type specifications
     &     ,ntypes              ! actual number of type specs

      parameter (
     &     max_types=100
     &     )

      record /types_s/ type_spec(max_types)
      common /com_s_types/ type_spec
      common /com_i_types/ ntypes

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
     &     ,jul_generic_dt      ! julian minute of generic_dt

      parameter (
     &     max_print_dates=10
     &     )

      character*14
     &     current_dt           ! current date/time (corresponds to julmin)
     &     ,run_start_dt        ! date/time of start of run
     &     ,run_end_dt          ! date/time of end of run
     &     ,print_start_dt(max_print_dates) ! date/time of print starts
     &     ,print_end_dt(max_print_dates) ! date/time of print ends

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

      common /com_c_dt/ current_dt,run_start_dt,run_end_dt,run_length,
     &     print_start_dt,print_end_dt,flush_intvl,display_intvl,
     &     time_step_intvl_hydro,time_step_intvl_qual,time_step_intvl_ptm

      common /com_i_dt/ nprints, julmin, prev_julmin, start_julmin,
     &     end_julmin, jul_generic_dt, time_step, prev_time_step

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
      real*8 deltax_requested     ! delta x to use in spatial discretization
      common /com_r_dx/ deltax_requested

c-----Particle model


C-----Qual Parameters

      logical mass_tracking,dispersion
      integer nres_conc         ! Number of reservoirs for which initial conc. specified
      real init_conc            ! initial concentration

      common/qual_scalars/nres_conc,mass_tracking,
     &     init_conc,dispersion

      integer
     &     no_of_constituent    ! total number of conserv+nonconserv constituents
     &     ,no_of_nonconserve_constituent ! no. of nonconservative constituents
     &     ,no_all_source       ! number of constituents tracked from all sources

      record /constituents_s/ constituents(max_constituent)

      integer
     &     nonconserve_ptr(max_constituent) ! pointer back to constituents structure
     &     ,all_source_ptr(max_constituent) ! pointer back to sonstituents from all sources
     &     ,constituent_ptr(max_constituent) ! pointer to correctly transfer rate coefficients

      character*20
     &     nonconserve_list(max_constituent) ! list of nonconservative constituents

      common /constituent_param/ constituents,
     &     no_of_constituent,no_of_nonconserve_constituent,no_all_source
     &     ,nonconserve_list,nonconserve_ptr,all_source_ptr
     &     ,constituent_ptr

c-----accounting and object names, value codes, period type names
      integer
     &     max_acct_names       ! max number of accounting names
     &     ,nacct_names         ! actual number of accounting names
     &     ,max_value_codes     ! max number of value codes

      parameter (
     &     max_acct_names=20
     &     ,max_value_codes=10
     &     )

      character*10 acct_names(max_acct_names) ! names for flow accounting; from user input
      character*10 obj_names(obj_null) ! names for object codes; set in read_fixed.f
      character*8 per_type_names(per_type_null) ! data type names (e.g. 'PER-AVER')
      record /value_codes_s/ value_codes(max_value_codes) ! value_out codes and values for TYPE section

      common /list_names_c/ acct_names, obj_names, per_type_names
      common /list_names_r/ value_codes
      common /list_names_i/ nacct_names

c-----misc max values for non-conservative constituents
      integer
     &     ncoef_type
      parameter (ncoef_type=6)

c-----reaction rate coefficients
      integer num_res
      real rcoef_chan, rcoef_res_temp, rcoef_res, rcoef
      character*20 coeff_res_name

      common /rate_coeff_c_i/num_res
      common /rate_coeff_c_r/
     &     rcoef_chan(max_constituent,ncoef_type,max_channels)
     &     ,rcoef_res_temp(max_constituent,ncoef_type,max_reservoirs)
     &     ,rcoef_res(max_constituent,ncoef_type,max_reservoirs)
     &     ,rcoef(max_constituent,ncoef_type)
     &     ,coeff_res_name(max_reservoirs)

c-----qual global parameters
c-----constituents related
      real
     &     algaefract_n	 	! fraction of algae as Nitrogen
     &     ,algaefract_p        ! fraction of algae as Phosphorus
     &     ,oxy_photo 	 	! Oxygen produced per unit of algal growth
     &     ,oxy_resp            ! O2 uptake per unit of algal respiration
     &     ,oxy_nh3             ! Oxygen used in oxidiation of NH3 to NO2
     &     ,oxy_no2             ! Oxygen used in oxidiation of NO2 to NO3
     &     ,alg_chl_ratio       ! Chlorophyll to biomass ratio
     &     ,pref_factor         ! algal pref. factor for NH3
     &     ,klight_half	 	! MM half-saturation constant for light
     &     ,knit_half	 	! MM half-saturation constant for nitrogen
     &     ,kpho_half	 	! MM half-saturation const. for phosphorus
     &     ,lambda0             ! Non algal light extinction coefficient
     &     ,lambda1             ! Linear algal self shading coefficient
     &     ,lambda2             ! Nonlinear algal self shading coefficient
     &     ,alg_bod             ! algal mortality contribution to BOD

      common/global_param/
     &     algaefract_n
     &     ,algaefract_p
     &     ,oxy_photo
     &     ,oxy_resp
     &     ,oxy_nh3
     &     ,oxy_no2
     &     ,alg_chl_ratio
     &     ,pref_factor
     &     ,klight_half
     &     ,knit_half
     &     ,kpho_half
     &     ,lambda0
     &     ,lambda1
     &     ,lambda2
     &     ,alg_bod

c-----heat and temperature related
      real
     &     elev			! Basin elevation, ft
     &     ,lat			! Latitude, degrees
     &     ,long		! Longitude,degrees
     &     ,long_std_merid      ! Longitude of stand. meridian,degrees
     &     ,dust_attcoeff       ! Dust attenuation coefficient
     &     ,evapcoeff_a		! Evaporation coefficient, A
     &     ,evapcoeff_b		! Evaporation coefficient, B
     &     ,thet
     &     ,thetadj
     &     ,thettbl

      integer
     &     temp_coeff_type

      parameter (temp_coeff_type=16)

      common /temperature_param/
     &     elev
     &     ,lat
     &     ,long
     &     ,long_std_merid
     &     ,dust_attcoeff
     &     ,evapcoeff_a
     &     ,evapcoeff_b
     &     ,thet(temp_coeff_type)
     &     ,thetadj(temp_coeff_type)
     &     ,thettbl(temp_coeff_type,81)

C-----Allow direct object to object transfer
      record /obj2obj_s/ obj2obj(max_obj2obj)
      common /com_objects/ obj2obj, nobj2obj

c-----stage boundary object
      record /stgbnd_s/ stgbnd(max_stgbnd)
      common /com_stgbnd_s/ stgbnd
      common /com_stgbnd_i/ nstgbnd
