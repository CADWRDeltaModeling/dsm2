!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM)
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!> This module defines virtual xsect type and allocate array dimension by actual
!> desired size.
!>@ingroup gtm_core
module common_xsect

    !@# use common_variables, only: n_chan, chan_geom	@!# This is the original line, replaced by the next:
	use common_variables, only: n_chan !@# chan_geom is declared in module grid_data.

    !@# use gtm_precision	!@# This is the original line, replaced by the next:
	use gtm_precision, only: gtm_real, LARGEREAL, Small, half, two	!@# some variables in gtm_precision will cause conflict with DSM2's variables.

	!@# Module grid_data is required for merging.
	use grid_data, only: chan_geom, max_channels, &
		nchans, levee_slope, area_tolerance, xsect_geom, chan_dx

    implicit none
    integer :: n_irreg                         !< actual number of irregular cross sections
    integer :: max_num_elev                    !< max number of elevations in dimension
    logical :: virt_xsect_allocated = .false.  !< if true, then chan_index is allocated
    integer, allocatable :: chan_index(:)      !< starting data index for a channel
    integer, allocatable :: xsect_index(:)     !< starting xsect index for a channel
    integer, allocatable :: num_xsect_chan(:)  !< number of xsect in a channel
    integer, allocatable :: num_elev_chan(:)   !< number of elevations in a channel

    !@# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
	!@# From DSM2's common_xsect in common_irreg.geom.f
    integer:: &
        max_elevations    ! maximum virtual sec in a channel
    integer:: max_assg_sec    ! maximum virtual sec in a channel
    integer:: max_irr_xsects    ! maximum virtual sec in a channel
    integer:: max_layers    ! maximum virtual sec in a channel
    integer:: max_total_elevations    ! maximum virtual sec in a channel
    integer:: max_virt_xsects    ! maximum virtual sec in a channel
    integer:: max_assg_virtsec    ! maximum virtual sec in a channel
    real*8:: max_layer_height    ! max allowable dist. for moving xsect to chan end
    real*8:: min_layer_elev      ! max allowable dist. for moving xsect to chan end
    real*8:: max_dist_ratio      ! max allowable dist. for moving xsect to chan end

#ifdef hydro_1000
    parameter ( &
        max_elevations=1000 &
        ,max_assg_sec=300 &
        ,max_assg_virtsec=15 &
        ,max_irr_xsects=3000 &
        ,max_layers=1000000 &
        ,max_total_elevations=40000 &
        ,max_layer_height=100.0 &
        ,min_layer_elev=-100.0 &
        ,max_virt_xsects=25000 &
        ,max_dist_ratio=0.025 &
        )
#else
    parameter ( &
        max_elevations=300 &
        ,max_assg_sec=22 &
        ,max_assg_virtsec=15 &
        ,max_irr_xsects=2000 &
        ,max_layers=100000 &
        ,max_total_elevations=20000 &
        ,max_layer_height=100.0 &
        ,min_layer_elev=-100.0 &
        ,max_virt_xsects=5000 &
        ,max_dist_ratio=0.05 &
        )
#endif

    logical:: repl              ! if true, then readirreg will replace a
                              ! cross-section with adj sec if adj node has 2 chan
    parameter (repl = .false.)

    integer:: nirg=0         ! actual number of irregular cross sections

    integer :: total_virt_xsect  ! total entries in virtual xsect table

    !-----arrays for virtual cross-sections to replace structure which was to large.
    !-----chan_index and num_layers will be used to calculate index of real arrays below
    !@# chan_index is declared above as allocatable.
    !@# integer*4:: &
    !@#    chan_index(max_channels)
    integer*4:: num_virt_sec(max_channels)
    integer*4:: num_layers(max_channels)
    integer*4:: elev_index(max_channels)
    integer*4:: minelev_index(max_virt_xsects)
    real*8:: &
        virt_area(max_layers)
    real*8:: virt_wet_p(max_layers)
    real*8:: virt_width(max_layers)
    real*8:: virt_z_centroid(max_layers)
    real*8:: virt_elevation(max_total_elevations)
    real*8:: virt_min_elev(max_virt_xsects)
    real*8:: virt_deltax(max_channels)
    !@# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !> Define type for virtual xsect
    type cross_section_t
        real(gtm_real) :: min_elev                       !< minimum elevation in the sec
        real(gtm_real), allocatable :: elevation(:)      !< all elevations in the sec
        real(gtm_real), allocatable :: area(:)           !< all area values in the sec
        real(gtm_real), allocatable :: wet_p(:)          !< all wetted perimeter values in the sec
        real(gtm_real), allocatable :: width(:)          !< all width values in the sec
        integer :: ID                          !< irregular cross section RDB ID
        integer :: chan_no                     !< channel number
        integer :: vsecno                      !< virtual cross-section number
        integer :: num_elev                    !< number of elevations in the sec
        integer :: num_virt_sec                !< number of virtual xsects in the channel
        integer :: prev_elevation_index        !< cache previous elevation index for performance only

        !@# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        !@# Below: from DSM2 (module common_xsect, in common_irreg_geom.f)
        real*8 dist_ratio      ! dist from upstr end/centerline length
        real*8 dist_actual     ! actual dist (ratio*act. chan length)
        real*8 h_radius(max_elevations) ! all hydraulic radius values in the sec
        real*8 x_centroid(max_elevations) ! all x centroid values in the sec
        real*8 z_centroid(max_elevations) ! all z centroid values in the sec
        integer secno          ! cross-section number
        !@# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    end type

    type(cross_section_t), allocatable:: virt_xsect(:)
    !@# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    !@# Below: from DSM2 (module common_xsect in common_irreg_geom.f).
    type(cross_section_t):: irreg_geom(0:max_irr_xsects)

    type xsect_assg_t
        !--------cross-section assignment structure.  index of sec_index
        !--------is the irreg. section number, which is also the index
        !--------of the irreg_geom structure
        integer sec_index(max_assg_sec) ! xsect number (index of irreg_geom str)
        integer num_sec_orig   ! number of sec originally assigned
        integer num_sec_assg   ! number of sec assigned after copying
        logical original(max_assg_sec) ! true if the section is original (not copy)
        logical rect(max_assg_sec) ! true if the section is rectangular
        real*8 dist(max_assg_sec)
    end type

    type(xsect_assg_t):: xsect_assg(0:max_channels)
    !@# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    contains

    !> Allocate dimension for channel related array
    subroutine allocate_chan_virt_xsect()
        use error_handling
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        if (.not. virt_xsect_allocated) then
            allocate(chan_index(n_chan), stat = istat)
            if (istat .ne. 0 ) call gtm_fatal(message)
            chan_index = LARGEREAL
        end if
        allocate(num_xsect_chan(n_chan), num_elev_chan(n_chan), stat = istat)
        if (istat .ne. 0 ) call gtm_fatal(message)
        num_xsect_chan = LARGEREAL
        num_elev_chan = LARGEREAL
        return
    end subroutine


    !> Allocate dimension for virtual xsect array
    !> (Before allocation, call calc_virt_xsect_dimension() to obtain n_irreg, chan_index, num_xsect_chan and num_elev_chan.)
    subroutine allocate_virt_xsec_geom()
        use error_handling
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        integer :: i
        allocate(xsect_index(n_chan), stat = istat)
        allocate(virt_xsect(n_irreg), stat = istat)
        do i=1,n_irreg
            allocate(virt_xsect(i)%elevation(max_num_elev), stat = istat)
            allocate(virt_xsect(i)%area(max_num_elev), stat = istat)
            allocate(virt_xsect(i)%wet_p(max_num_elev), stat = istat)
            allocate(virt_xsect(i)%width(max_num_elev), stat = istat)
            virt_xsect(i)%elevation = LARGEREAL
            virt_xsect(i)%area = LARGEREAL
            virt_xsect(i)%wet_p = LARGEREAL
            virt_xsect(i)%width = LARGEREAL
            virt_xsect(i)%prev_elevation_index = 1
        end do
        xsect_index(1) = 1
        do i = 2, n_chan
            xsect_index(i) = xsect_index(i-1) + num_xsect_chan(i-1)
        end do
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
    end subroutine

    !@# Allocate virtual xsect array for Hydro and Qual
    subroutine allocate_virt_xsect_hq()
        implicit none
        integer :: i
        allocate(chan_index(max_channels))
        do i = 1,max_irr_xsects
            allocate(irreg_geom(i).elevation(max_elevations))
            allocate(irreg_geom(i).area(max_elevations))
            allocate(irreg_geom(i).wet_p(max_elevations))
            allocate(irreg_geom(i).width(max_elevations))
        end do
        virt_xsect_allocated = .true.
    end subroutine

    !> Deallocate virtual xsect array
    subroutine deallocate_virt_xsect()
        implicit none
        chan_index = LARGEREAL
        num_xsect_chan = LARGEREAL
        num_elev_chan = LARGEREAL
        deallocate(chan_index, num_xsect_chan, num_elev_chan)
        deallocate(xsect_index, virt_xsect)
        return
    end subroutine


    !> Calculate area of channel cross section based on given X, Z and channel number
    subroutine CxArea(area,                  &        ! return cross section area
                      X,                     &        ! distance from upstream
                      Z,                     &        ! water surface elevation
                      branch)                         ! channel no
        implicit none
        real(gtm_real), intent(in) :: Z               !< water surface elevation
        real(gtm_real), intent(in) :: X               !< distance from upstream
        integer, intent(in) :: branch                 !< channel no
        real(gtm_real), intent(out) :: area           !< CxArea
        real(gtm_real) :: virt_deltax
        integer :: vsecno_forX
        integer :: vindex
        integer :: i, si, di, ei, OK
        real(gtm_real) :: z1, z2, y1, y2, b1, b2, a1
        real(gtm_real) :: dz, slope, width
        real(gtm_real) :: wet_p, hydro_radius
        if (num_xsect_chan(branch)>1) then
            virt_deltax = chan_geom(branch)%channel_length/(num_xsect_chan(branch)-1)
        else
            virt_deltax = chan_geom(branch)%channel_length
        end if
        vsecno_forX = nint(X/virt_deltax) + 1
        si = xsect_index(branch) + vsecno_forX - 1
        vindex = virt_xsect(si)%prev_elevation_index
        do while ((Z .lt. virt_xsect(si)%elevation(vindex)) .and. (vindex .gt. 1))
            vindex = vindex - 1
        end do
        OK = 0
        do i = vindex, num_elev_chan(branch)-1
           if (OK .eq. 0) then
               if (Z.ge.virt_xsect(si)%elevation(i) .and. Z.lt.virt_xsect(si)%elevation(i+1)) then
                   OK = 1
                   ei = i
               end if
           end if
        end do
        virt_xsect(si)%prev_elevation_index=ei ! caching for performance improvement only
        if (OK .eq. 0) then
            write(*,*) 'water surface is beyond limit at branch ',branch,', X=',X,', Z=',Z
        end if
        z1 = virt_xsect(si)%elevation(ei)
        z2 = virt_xsect(si)%elevation(ei+1)
        dz = z2 - z1
        if ( abs(dz) <= Small) then
            write(*,*) 'Channel width, wet perimeter, dwetperimeter division by zero'
        endif
        slope = (Z-z2)/dz
        y1 = virt_xsect(si)%width(ei)
        y2 = virt_xsect(si)%width(ei+1)
        width = (y2-y1)*slope + y2
        a1 = virt_xsect(si)%area(ei)
        b1 = virt_xsect(si)%width(ei)
        b2 = width
        area = a1 + half * ( b1 + b2 )* (Z - z1)
        wet_p = virt_xsect(si)%wet_p(ei) + two*dsqrt(((b2-b1)*half)**2+(Z-z1)**2)
        hydro_radius = area/wet_p
        return
    end subroutine


    !> Calculate information of channel cross section based on given X, Z and channel number
    subroutine CxInfo(area,                  &        ! return cross section area
                      width,                 &        ! return channel width
                      wet_p,                 &        ! return wetted perimeter
                      depth,                 &        ! return water depth
                      X,                     &        ! distance from upstream
                      Z,                     &        ! water surface elevation
                      branch)                         ! channel no
        implicit none
        real(gtm_real), intent(in) :: Z               !< water surface elevation
        real(gtm_real), intent(in) :: X               !< distance from upstream
        integer, intent(in) :: branch                 !< channel no
        real(gtm_real), intent(out) :: area           !< CxArea
        real(gtm_real), intent(out) :: width          !< Width
        real(gtm_real), intent(out) :: wet_p          !< Wetted perimeter
        real(gtm_real), intent(out) :: depth          !< Water depth
        real(gtm_real) :: virt_deltax
        integer :: vsecno_forX
        integer :: vindex
        integer :: i, si, di, ei, OK
        real(gtm_real) :: z1, z2, y1, y2, b1, b2, a1
        real(gtm_real) :: dz, slope
        if (num_xsect_chan(branch)>1) then
            virt_deltax = chan_geom(branch)%channel_length/(num_xsect_chan(branch)-1)
        else
            virt_deltax = chan_geom(branch)%channel_length
        end if
        vsecno_forX = nint(X/virt_deltax) + 1
        si = xsect_index(branch) + vsecno_forX - 1
        vindex = virt_xsect(si)%prev_elevation_index
        do while ((Z .lt. virt_xsect(si)%elevation(vindex)) .and. (vindex .gt. 1))
            vindex = vindex - 1
        end do
        OK = 0
        do i = vindex, num_elev_chan(branch)-1
           if (OK .eq. 0) then
               if (Z.ge.virt_xsect(si)%elevation(i) .and. Z.lt.virt_xsect(si)%elevation(i+1)) then
                   OK = 1
                   ei = i
               end if
           end if
        end do
        virt_xsect(si)%prev_elevation_index=ei ! caching for performance improvement only
        if (OK .eq. 0) then
            write(*,*) 'water surface is beyond limit at branch ',branch,', X=',X,', Z=',Z
        end if
        z1 = virt_xsect(si)%elevation(ei)
        z2 = virt_xsect(si)%elevation(ei+1)
        dz = z2 - z1
        if ( abs(dz) <= Small) then
            write(*,*) 'Channel width, wet perimeter, dwetperimeter division by zero'
        endif
        slope = (Z-z2)/dz
        y1 = virt_xsect(si)%width(ei)
        y2 = virt_xsect(si)%width(ei+1)
        width = (y2-y1)*slope + y2
        a1 = virt_xsect(si)%area(ei)
        b1 = virt_xsect(si)%width(ei)
        b2 = width
        area = a1 + half * ( b1 + b2 )* (Z - z1)
        wet_p = virt_xsect(si)%wet_p(ei) + two*dsqrt(((b2-b1)*half)**2+(Z-z1)**2)
        depth = Z - (chan_geom(branch)%chan_btm_up + X/chan_geom(branch)%channel_length* &
                       (chan_geom(branch)%chan_btm_down-chan_geom(branch)%chan_btm_up))
        return
    end subroutine

end module common_xsect
