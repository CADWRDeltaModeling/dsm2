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

      include 'defs_ptm.inc'          ! structure definitions
      include 'constants_ptm.inc'          ! constants, magic numbers

c-----common blocks for DSM2 PTM
c-----Scalars for ptm

      logical
     &     ptm_ivert            ! True to use a vertical velocity profile
     &     ,ptm_itrans          ! True to use transverse velocity profile
     &     ,ptm_iey             ! True to use transverse mixing
     &     ,ptm_iez             ! True to use vertical mixing
     &     ,ptm_iprof           ! True writes to a file locations of parts at specified timesteps and in spec. chnls
     &     ,ptm_igroup          ! True reads from a file groups of chnls & res and writes # of parts in each group
     &     ,ptm_flux_percent    ! True outputs flux as a percentage of particles
     &     ,ptm_group_percent   ! True outputs groups as a percentage of particles
     &     ,ptm_flux_cumulative ! True outputs flux as cumulative values with time

      common /com_l_ptm/
     &     ptm_ivert, ptm_itrans, ptm_iey,
     &     ptm_iez, ptm_iprof, ptm_igroup,
     &     ptm_flux_percent, ptm_group_percent,
     &     ptm_flux_cumulative

      integer
     &     ptm_random_seed
     &     ,ptm_time_step
     &     ,ptm_no_animated

      common /com_i_ptm/
     &     ptm_random_seed
     &     ,ptm_time_step
     &     ,ptm_no_animated

      real
     &     ptm_trans_constant,
     &     ptm_vonkarman,
     &     ptm_trans_a_coef,
     &     ptm_trans_b_coef,
     &     ptm_trans_c_coef,
     &     ptm_shear_vel

      common /com_r_ptm/ ptm_trans_constant, ptm_vonkarman,
     &     ptm_trans_a_coef, ptm_trans_b_coef,
     &     ptm_trans_c_coef, ptm_shear_vel

c-----Values that indicate whether a scalar was specified in the input
      integer
     &     ptm_random_seed_int,
     &     ptm_vonkarman_int,
     &     ptm_trans_constant_int,
     &     ptm_iprof_int,
     &     ptm_igroup_int,
     &     ptm_ivert_int,
     &     ptm_itrans_int,
     &     ptm_iey_int,
     &     ptm_iez_int,
     &     ptm_time_step_int,
     &     ptm_trans_a_coef_int,
     &     ptm_trans_b_coef_int,
     &     ptm_trans_c_coef_int,
     &     ptm_shear_vel_int,
     &     ptm_flux_percent_int,
     &     ptm_group_percent_int,
     &     ptm_flux_cumulative_int,
     &     ptm_no_animated_int

      common /com_r_ptmint/ ptm_random_seed_int
     &     , ptm_vonkarman_int
     &     , ptm_trans_constant_int
     &     , ptm_iprof_int
     &     , ptm_igroup_int
     &     , ptm_ivert_int
     &     , ptm_itrans_int
     &     , ptm_iey_int
     &     , ptm_iez_int
     &     , ptm_time_step_int
     &     , ptm_trans_a_coef_int
     &     , ptm_trans_b_coef_int
     &     , ptm_trans_c_coef_int
     &     , ptm_shear_vel_int
     &     , ptm_flux_percent_int
     &     , ptm_group_percent_int
     &     , ptm_flux_cumulative_int
     &     , ptm_no_animated_int

c-----part_flux structure handles particle flux output
      integer
     &     npart_flux           ! actual number of particle flux outputs requested

      common /com_i_part_flux/ npart_flux

      integer
     &     max_injection        ! Maximum Injection Locations and time of injections combined
     &     ,npartno             ! Actual No of Injection Loc and times combined
     &     ,ngroups             ! Actual No of Groups requested

      parameter (
     &     max_injection=200
     &     )

      record /part_injection_s/ part_injection(max_injection)
      common /com_s_part_no/ part_injection,npartno

      integer
     &     max_chanres          ! Maximum number of channels and Reservoirs
     &     ,nchanres            ! Actual number of channels and Reservoirs

      parameter (
     &     max_chanres=600
     &     )

      integer
     &     revgrp(0:max_chanres) ! Array hold positions of group numbers 

      record /group_areas_s/ group_areas(0:max_chanres)
      common /com_s_group/ group_areas, ngroups, revgrp
      common /com_i_group/ nchanres

