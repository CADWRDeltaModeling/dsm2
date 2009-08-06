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

      module common_xsect
      use grid_data
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

      integer :: nirg=0         ! actual number of irregular cross sections

      type cross_section_t 
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

      type(cross_section_t) irreg_geom(0:max_irr_xsects)

      type xsect_assg_t
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
