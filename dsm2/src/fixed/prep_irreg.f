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

      subroutine prep_irreg
      use grid_data
      use constants
      use common_xsect
c-----Prepare irregular cross section arrays

      implicit none

c-----local variables
      integer maxf              ! maximum number of fields in data files
      parameter (maxf=7)
      integer
     &     i
     &     ,j                   ! do loop counters

c-----initialize variables
      do i=1,max_irr_xsects
         irreg_geom(i).dist_actual=init_small_r
         irreg_geom(i).secno=init_small_i
         irreg_geom(i).num_elev=init_small_i
         irreg_geom(i).min_elev=init_big_r
         do j=1,max_elevations
            irreg_geom(i).elevation(j)=init_small_r
            irreg_geom(i).area(j)=init_small_r
            irreg_geom(i).wet_p(j)=init_small_r
            irreg_geom(i).width(j)=init_small_r
            irreg_geom(i).h_radius(j)=init_small_r
            irreg_geom(i).x_centroid(j)=init_small_r
            irreg_geom(i).z_centroid(j)=init_small_r
         enddo
      enddo

      do i=0,max_channels
         xsect_assg(i).num_sec_orig=init_small_i
         xsect_assg(i).num_sec_assg=init_small_i
         do j=1,max_assg_sec
            xsect_assg(i).sec_index(j)=init_small_i
            xsect_assg(i).original(j) = .false.
            xsect_assg(i).rect(j) = .false.
            xsect_assg(i).dist(j)=init_small_r
         enddo
      enddo

      do i=1,max_channels
         chan_index(i)=init_small_i
         num_virt_sec(i)=init_small_i
         num_layers(i)=init_small_i
         elev_index(i)=init_small_i
         virt_deltax(i)=init_small_r
      enddo

      do i=1,max_layers
         virt_area(i)=init_small_r
         virt_wet_p(i)=init_small_r
         virt_width(i)=init_small_r
         virt_z_centroid(i)=init_small_r
      enddo

      do i=1, max_total_elevations
         virt_elevation(i)=init_small_r
      enddo

      do i=1,max_virt_xsects
         virt_min_elev(i)=init_big_r
         minelev_index(i)=init_small_i
      enddo

      return
      end
