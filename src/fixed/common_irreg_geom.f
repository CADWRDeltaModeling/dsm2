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
      module common_xsect
      integer
     &     max_elevations       ! max elevations (incl. duplicates) in a channel
     &     ,max_assg_sec        ! max real sections assigned to a channel
     &     ,max_irr_xsects      ! max total irregular xsects read in
     &     ,max_layers          ! max total layers in all channels
     &     ,max_total_elevations ! max total elevations in all channels
     &     ,max_virt_xsects     ! max total virtual sec
     &     ,max_assg_virtsec    ! maximum virtual sec in a channel
      real*8
     &     max_layer_height     ! added to top of every virtual cross-section
     &     ,min_layer_elev      !
     &     ,max_dist_ratio      ! max allowable dist. for moving xsect to chan end

      parameter (
     &     max_elevations=300
     &     ,max_assg_sec=22
     &     ,max_assg_virtsec=15
     &     ,max_irr_xsects=2000
     &     ,max_layers=100000
     &     ,max_total_elevations=20000
     &     ,max_layer_height=100.0
     &     ,min_layer_elev=-100.0
     &     ,max_virt_xsects=5000
     &     ,max_dist_ratio=0.05
     &     )

      logical repl              ! if true, then readirreg will replace a
                                ! cross-section with adj sec if adj node has 2 chan
      parameter (repl = .false.)

      integer nirg              ! actual number of irregular cross sections

      type(xsect_t)
         real*8 dist_ratio      ! dist from upstr end/centerline length
         real*8 dist_actual     ! actual dist (ratio*act. chan length)
         real*8 min_elev        ! minimum elevation in the sec
         real*8 elevation(max_elevations) ! all elevations in the sec
         real*8 area(max_elevations) ! all area values in the sec
         real*8 wet_p(max_elevations) ! all wetted perimeter values in the sec
         real*8 width(max_elevations) ! all width values in the sec
         real*8 h_radius(max_elevations) ! all hydraulic radius values in the sec
         real*8 x_centroid(max_elevations) ! all x centroid values in the sec
         real*8 z_centroid(max_elevations) ! all z centroid values in the sec
         integer ID             ! irregular cross section RDB ID
         integer chan_no        ! channel number
         integer secno          ! cross-section number
         integer num_elev       ! number of elevations in the sec
      end type

      type(xsect_t) irreg_geom(max_irr_xsects)

      type(xsect_assg_t)
c--------cross-section assignment structure.  index of sec_index
c--------is the irreg. section number, which is also the index
c--------of the irreg_geom structure
         integer sec_index(max_assg_sec) ! xsect number (index of irreg_geom str)
         integer num_sec_orig   ! number of sec originally assigned
         integer num_sec_assg   ! number of sec assigned after copying
         logical original(max_assg_sec) ! true if the section is original (not copy)
         logical rect(max_assg_sec) ! true if the section is rectangular
         real*8 dist(max_assg_sec)
      end type

      type(xsect_assg_t) xsect_assg(0:max_channels)

c-----arrays for virtual cross-sections to replace structure which was to large.
c-----chan_index and num_layers will be used to calculate index of real arrays below
      integer*4
     &     chan_index(max_channels) ! index for data arrays
     &     ,num_virt_sec(max_channels) ! number of virtual sections in the chan
     &     ,num_layers(max_channels) ! number of unique elevations in the chan
     &     ,elev_index(max_channels) ! index of virt_elevation array
     &     ,minelev_index(max_virt_xsects)
      real*8
     &     virt_area(max_layers) ! all area values for all channels
     &     ,virt_wet_p(max_layers) ! all wetted perimeter values for all channels
     &     ,virt_width(max_layers) ! all width values for all channels
     &     ,virt_z_centroid(max_layers) ! all z centroid values for all channels
     &     ,virt_elevation(max_total_elevations) ! all elevation values for all chan
     &     ,virt_min_elev(max_virt_xsects)
     &     ,virt_deltax(max_channels)


      end module
