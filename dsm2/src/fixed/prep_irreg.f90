!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

subroutine prep_irreg
      use grid_data
      use constants
      use common_xsect
!-----Prepare irregular cross section arrays

      implicit none

!-----local variables
      integer maxf              ! maximum number of fields in data files
      parameter (maxf=7)
      integer i, j                   ! do loop counters

!-----initialize variables
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
