!<license>
!    Copyright (C) 2015 State of California,
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

    use common_variables, only: n_chan, chan_geom
    use gtm_precision
    implicit none  
    integer :: n_irreg                         !< actual number of irregular cross sections
    integer :: max_num_elev                    !< max number of elevations in dimension                       
    integer, allocatable :: chan_index(:)      !< starting data index for a channel
    integer, allocatable :: xsect_index(:)     !< starting xsect index for a channel
    integer, allocatable :: num_xsect_chan(:)  !< number of xsect in a channel
    integer, allocatable :: num_elev_chan(:)   !< number of elevations in a channel
    
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
    end type
    
    type(cross_section_t), allocatable:: virt_xsect(:)
    
    contains
    
    !> Allocate dimension for channel related array
    subroutine allocate_chan_virt_xsect()
        use error_handling
        implicit none
        integer :: istat = 0
        character(len=128) :: message
        allocate(chan_index(n_chan), num_xsect_chan(n_chan), num_elev_chan(n_chan), stat = istat)
        if (istat .ne. 0 )then
           call gtm_fatal(message)
        end if
        chan_index = LARGEREAL
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
                      hydro_radius,          &        ! return hydraulic radius
                      X,                     &        ! distance from upstream
                      Z,                     &        ! water surface elevation
                      branch)                         ! channel no
        implicit none
        real(gtm_real), intent(in) :: Z               !< water surface elevation
        real(gtm_real), intent(in) :: X               !< distance from upstream
        integer, intent(in) :: branch                 !< channel no
        real(gtm_real), intent(out) :: area           !< CxArea
        real(gtm_real), intent(out) :: width          !< Width
        real(gtm_real), intent(out) :: hydro_radius   !< Hydraulic radius
        real(gtm_real) :: virt_deltax
        integer :: vsecno_forX
        integer :: vindex
        integer :: i, si, di, ei, OK
        real(gtm_real) :: z1, z2, y1, y2, b1, b2, a1
        real(gtm_real) :: dz, slope
        real(gtm_real) :: wet_p
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
         
end module common_xsect
