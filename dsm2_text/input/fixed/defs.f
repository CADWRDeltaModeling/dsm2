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

c-----structure definitions for DSM2

c-----yer basic object
      structure /object_s/
         integer object         ! object type of WB (e.g. res, chan, ...)
         character*32 obj_name  ! ID of particular WB (e.g. clfct, dxc, ...)
         integer object_no      ! ID number of particular WB
      end structure

c-----input section maximums
      integer
     &     max_sections         ! maximum number of different sections
     &     ,max_fields          ! maximum number of fields in a section
     &     ,max_inp_files       ! maximum number of input files allowed

      parameter (
     &     max_sections=50
     &     ,max_fields=30       ! set this larger than needed
     &     ,max_inp_files=40
     &     )

c-----special values
      integer
     &     unlimited_fields     ! denotes that a column header has from 1 to many fields,
                                ! ending with the delimiter character
      parameter (
     &     unlimited_fields=max_fields-1
     &     )

c-----valid keywords structure
      structure /form/
         integer      fldnum    ! number of fields in section
         character*30 sect      ! section name
         character*15 fld(max_fields) ! field names in section
         logical repeat         ! true if repeating field keywords allowed
      end structure

      integer
     &     max_xsects           ! maximum number of cross sections per channel
     &     ,max_cpn             ! maximum number of channels per node

      parameter (
     &     max_xsects=4
     &     ,max_cpn=4
     &     )

      structure /channels_s/
         integer length         ! channel length in feet
         real*4    manning        ! manning's N coefficient
         real*4    disp           ! dispersion coefficient
         integer xsect(max_xsects) ! channel cross section numbers
         integer dist(max_xsects) ! Distance along the channel for X-Section
         real*4    BottomElev(2)  ! Bottom Elevation of each x-section defined
         integer nxsect         ! Number of X-Sections defined for this channel
         integer upnode         ! upstream node
         integer downnode       ! downstream node
      end structure

c-----track internal and external flows at nodes and reservoirs
      integer max_qobj          ! maximum internal and external flows at objects
      parameter (max_qobj=10)

      structure /nodes_s/
         integer nup            ! number of channels connected on chan upstream end
         integer ndown          ! number of channels connected on chan downstream end
         integer boundary_type  ! type of boundary condition
         integer upstream(max_cpn) ! upstream channel numbers
         integer downstream(max_cpn) ! downstream channel numbers
         integer qint(max_qobj) ! index of internal flows at node
         integer qext(max_qobj) ! index of external flows at node
      end structure

c-----Right now this is for rectangular x-sects only.  Later will generalize.
      structure /xsects_s/
         real*8    width          ! width of channel in feet
         real*8    botelv         ! bottom stage (NGVD, feet)
         logical upstream       ! true if upstream xsect; false if the downstream
         real*8    init_stage     ! initial stage at cross section (NGVD, feet)
         real*8    init_flow      ! initial flow at cross section (cfs)
      end structure

      integer
     &     maxresnodes          ! maximum number of connecting nodes/reservoir

      parameter (
     &     maxresnodes=10
     &     )

      structure /reservoirs_s/
         character*32 name      ! reservoir name
         real*8 area              ! average top area
         real*8 botelv            ! bottom elevation wrt datum
         real*8 stage             ! stage elevation
         real*8 maxstage          ! max allowed stage
         integer nnodes         ! number of nodes connected to this reservoir
         integer node_no(maxresnodes) ! connecting node number
         real*8 coeff2res(maxresnodes) ! to reservoir flow coefficient to node
         real*8 coeff2chan(maxresnodes) ! to channel flow coefficient to node
         integer number         ! dsm2-hydro reservoir number
         real*8 maxq2res(maxresnodes) ! max inflow allowed through connection to reservoir
         integer qint(max_qobj) ! index of internal flows at reservoir
         integer qext(max_qobj) ! index of external flows at reservoir
      end structure

c-----gates

      structure /gates_s/
         character*32 name      ! name of gate
         integer chan_no        ! channel number
         integer node_no        ! node number for reservoir gate
         character*4 loc        ! location (downstream or upstream end of channel)
         integer lapse          ! lapse time in minutes for gate to fully change position
         integer ngates         ! number of operating gates in gate structure
         real*8 widthdown         ! width of gate, downstream direction of flow
         real*8 widthup           ! width of gate, upstream direction of flow
         real*8 widthfree         ! width of gate, free flow (as if no gate)
         real*8 crestelev         ! elevation of crest wrt to datum
         real*8 crestfree         ! elevation of crest wrt to datum for free flow
         real*8 coeffweirdown     ! weir flow coefficient, downstream direction
         real*8 coeffweirup       ! weir flow coefficient, upstream direction
         integer npipes         ! number of pipes in gate
         real*8 piperad           ! pipe radius
         real*8 pipeelev          ! pipe invert elevation wrt datum
         real*8 coeffpipedown     ! pipe flow coefficient, downstream direction
         real*8 coeffpipeup       ! pipe flow coefficient, upstream direction
         character*4 oper       ! operation type ('time', 'calc', 'open', 'clos')
         real*8 dhopen            ! for 'calc', open when delta stage = value
         real*8 velclose          ! for 'calc', open when velocity = value
      end structure

c-----path input (time-varying data)

      integer
     &     max_path_const       ! maximum number of constituents associated with path

      parameter (
     &     max_path_const=10
     &     )

      structure /pathinput_s/
c--------"to" tags: node or label this data will go to (a label must be
c--------translated to a node, or else be a gate or reservoir name, or
c--------be an object-to-object label)
         character*32 label     ! data path label (e.g. vernalis)
c--------"from" tags: construct the DSS pathname using a-f parts, or give
c--------IEP-style information
c--------if a-f parts are not input, these will be constructed from the
c--------IEP tags
         character*32 a_part    ! DSS A part
         character*32 b_part    ! DSS B part
         character*32 c_part    ! DSS C part
         character*32 e_part    ! DSS E part
         character*32 f_part    ! DSS F part
         character*32 ID        ! ID or study name string
         character*15 interval  ! e.g. MIN, DAY
c--------DSS filename or constant value to use
         character*130 filename ! DSS filename
         real*8 constant_value    ! constant value (instead of reading from DSS filename)
         real*8 value             ! value for this timestep
         integer*4 value_flag   ! data quality flag for this timestep
c--------other values that may be input by user
         character*14 start_dt  ! path start date and time
         integer fillin         ! how to fill in between data (first, last, interp, data)
         integer priority       ! priority to use for replacing data at a location
c--------internal indices and variables
         integer replace_path   ! index number of next replacement path
         logical replace        ! true if this value should try to be replaced
         logical sync           ! generic data that should be synched on e part
         integer object         ! object this data goes to: channel, reservoir, node, gate?
         integer object_no      ! number of labeled object
         integer data_type      ! data type: flow, gate position, stage, ..
c--------'type' section
         character*10 acct_name ! accounting type (e.g. RIM, DRAIN)
         integer acct_ndx       ! pointer to accounting name vector
         character*1 sign       ! sign to change value to (+, -, or ' ' for none)
         real*8 mass_frac         ! fraction of mass this flow takes
         real*8 value_in          ! incoming value to check
         real*8 value_out         ! outgoing value to change to
         integer*4 use_flag     ! data quality flag to use for this path
         integer gate_param     ! time-varying gate parameter
         
         integer ndx_file       ! pointer to infilename vector
         character*80 path      ! DSS pathname (constructed from a-f parts)
         integer*4 diff_julmin  ! path start time difference from run_start_dt in minutes
         integer hydrochan      ! dsm2-hydro chan this input assigned to (+ upstream end, - downstream end)
         integer const_ndx(max_path_const) ! constituent number index
         integer n_consts       ! number of constituents
         integer no_intervals   ! e.g. 1, 15
         integer intvl_path     ! path number for this interval
         integer per_type       ! period type: per-average, instantaneous, etc.
      end structure

c-----path output (time-varying data)

      structure /reserv_s/      ! reservoir sub-structure
         integer node_no        ! DSM2 node number reservoir connected to
         integer hydro_node_no  ! Hydro node number
      end structure

      structure /WaterBody_flux_type_s/
         integer object         ! object type of WB (e.g. res, chan, ...)
         character*32 obj_name  ! ID of particular WB (e.g. clfct, dxc, ...)
         integer object_no      ! ID number of particular WB
         character*10 acct_name ! accounting type (e.g. RIM, DRAIN)
         integer acct_ndx       ! pointer to accounting name vector
      end structure

c-----source of constituents
      structure /source_s/
         character*32 loc_name  ! location name (e.g. SAC, VERNALIS)
         integer object         ! object type
         integer object_no      ! object number
         character*32 obj_name  ! object name
         character*10 acct_name ! accounting type (e.g. RIM, DRAIN)
         integer acct_ndx       ! pointer to accounting name vector
      end structure

      integer
     &     max_ft_flux          ! maximum number of from-to fluxes

      parameter (
     &     max_ft_flux=10
     &     )

      structure /pathoutput_s/
         character*130 filename ! output filename
         integer ndx_file       ! pointer to outfilename
         character*80 path      ! DSS pathname
         character*32 name      ! station name (b part) for path (optional)
         integer object         ! output from which object: channel, reservoir, etc.
         integer object_no      ! object number of output
         integer chan_dist      ! distance downstream from upstream end of chan
         record /reserv_s/ reservoir ! reservoir sub-structure
         record /waterbody_flux_type_s/ flux_from(max_ft_flux)
         record /waterbody_flux_type_s/ flux_to(max_ft_flux)
         character*32 a_part    ! DSS A part
         character*32 b_part    ! DSS B part
         character*32 c_part    ! DSS C part
         character*32 e_part    ! DSS E part
         character*32 f_part    ! DSS F part
         record /source_s/ source ! source of constituents structure
         character*15 meas_type ! e.g. STAGE, VELOCITY, TDS
         integer const_ndx      ! constituent number index
         integer no_intervals   ! e.g. 1, 15
         character*15 interval  ! e.g. MIN, DAY
         character*8 units      ! e.g. cfs, feet, umhos/cm
         integer per_type       ! PER-AVER or INST-VALUE
         character*32 modifier  ! used for study name or such
         integer intvl_path     ! path number for this interval
      end structure

c-----pseudo (internal) environment variables
      structure /envvars_s/
         character*130 name
         character*130 value
      end structure

c-----input/output file names

      structure /io_files_s/
         logical use            ! .true. if restart/tide to be read/written
         integer unit           ! restart/tide read/write unit
         character*80 interval  ! interval for restart/tide writing (e.g. 1HOUR)
         character*130 filename ! restart/tide read/write filename
      end structure

c-----translations:
c-----location name --> chan/dist pair, node, or reservoir name

      structure /trans_s/
         character*32 from_name ! translate from this name
         integer object         ! translate to this type of object
         integer object_no      ! translate to this object number
         character*32 obj_name  ! translate to this object name
         integer chan_dist      ! translate to this channel distance
         character*15 constituent ! translate to this constituent name
      end structure

c-----assign type:
c-----allow user to assign types to input paths
c-----e.g. specify which paths should have sign changed on
c-----input values; or specify if an outgoing flow is a diversion,
c-----evaporation, or seepage

      structure /types_s/
         character*32 string    ! string to check for specifying type
         character*1 part       ! pathname part to check for string
         character*1 match      ! type of string match (Exact, Substring)
         character*1 sign       ! sign (+/-) to give to value
         character*10 acct_name ! accounting name (e.g. flow: diversion, seepage, ...)
         integer acct_ndx       ! pointer to accounting name vector
         real*8 mass_frac         ! fraction of mass this type takes
         real*8 value_in          ! incoming value to check
         real*8 value_out         ! outgoing value to change to
         integer*4 value_flag   ! data quality flag to use
      end structure

      structure /value_codes_s/
         character*10 code      ! value code string
         real*8 value             ! value of code
      end structure


C-----Qual Parameters

      structure /constituents_s/
         logical conservative   ! true if conservative, false if nonconservative
         character*15 constituent ! constituent name
         character*32 loc_name  ! injection location name (e.g. SAC, VERNALIS)
         integer object         ! object type
         integer object_no      ! object number
         integer acct_ndx       ! pointer to accounting name vector
      end structure

      integer
     &     max_constituent      ! Maximum number of constituents
     &     ,max_conqext         ! Maximum number of constituents for an external flow

      parameter (
     &     max_constituent=60
     &     ,max_conqext=12
     &     )

c-----external flows, for transfer in tidefile

      structure /qext_s/
         real*4 flow              ! external flow value for this timestep
         real*4 prev_flow         ! external flow value for previous timestep
         real*4 avg               ! external flow value averaged over tideblock time interval
         real*4 prev_avg          ! previous avg
         integer in_no          ! global pathnumber of flow
         integer changed_ndx    ! index of nqext_ndx of changed flows
         character*32 obj_name  ! ID of this flow (e.g. CVP, SWP, ...)
         record /object_s/ attach ! object info this flow is attached to
         character*10 acct_name ! accounting label (e.g. 'diversion', 'seepage')
         integer acct_ndx       ! pointer to accounting name vector
         real*4 mass_frac         ! fraction of mass this flow takes
      end structure

c-----object-to-object water transfer
c-----from- and to-object sub-structure
      structure /from_to_s/
         integer object         ! object type code (e.g. channel, node, or reservoir)
         character*32 obj_name  ! object name or...
         integer object_no      ! ...object number
         integer hydrochan      ! hydro channel number for node flow
         character*10 acct_name ! accounting label (e.g. 'diversion', 'seepage')
         integer acct_ndx       ! pointer to accounting name vector
         real*4 mass_frac         ! fraction of mass this flow takes
         real*4 coeff             ! Flow coefficient for stage-driven transfer
      end structure

      structure /obj2obj_s/
         character*32 obj_name  ! ID of this flow (e.g. IF, HOOD_DIV)
         record /from_to_s/ from ! From object
         record /from_to_s/ to  ! To object
         real*4 constant_value    ! fixed flow value (if given), or head diff code
         character*32 label     ! input path label
c--------following are filled in during run
         integer in_no          ! inputpath number if label given
         real*4 flow              ! flow between the two objects at this time step
         real*4 prev_flow         ! flow between the two objects at previous time step
         real*4 flow_avg          ! Average flow between the two objects
         real*4 constituent_conc(max_constituent) ! Concentration of each constituent
      end structure

c-----stage boundary object
      structure /stgbnd_s/
         integer node
         character*32 name
      end structure

c-----data value, quality flags, and timestamp object
      structure /dataqual_s/
         real*8 data
         integer*4 flag
         integer*4 julmin
      end structure
