!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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


!> Interfaces for user to provide flow and area. Replaces the connectivity with HYDRO
!> at this point 
! todo: where is its place in doxygen
module hydro_uniform_flow
    
    use gtm_precision

    real(gtm_real) :: const_flow = one                    !< Constant flow 
    real(gtm_real) :: const_area = zero                    !< Constant area
    real(gtm_real) :: reversal_time = LARGEREAL            !< Time flow direction switches   

    contains 

    !> Set constant flow and area that will be used in the no_flow hydro interface 
    subroutine set_uniform_flow_area(flow, area, reverse_time)
        use gtm_precision
        implicit none
        real(gtm_real), intent(in) :: flow                   !< Constant flow to be set
        real(gtm_real), intent(in) :: area                   !< Constant area to be set
        real(gtm_real), intent(in), optional :: reverse_time !< Time at which flow will reverse direction
        const_flow = flow
        const_area = area
        if (present(reverse_time)) then
            reversal_time = reverse_time
        else
            reversal_time = LARGEREAL
        end if
        return 
    end subroutine

    !>Simple hydrodynamic interface for constant area and constant flow
    subroutine uniform_flow_area(flow,    &
                                 flow_lo, &
                                 flow_hi, &
                                 area,    &
                                 area_lo, &
                                 area_hi, &
                                 ncell,   &
                                 time,    &
                                 dx,      &                        
                                 dt)
        use gtm_precision                   
        implicit none
        integer, intent(in) :: ncell                   !< Number of cells
        real(gtm_real), intent(in) :: time             !< Time of request
        real(gtm_real), intent(in) :: dx(ncell)        !< Spatial step
        real(gtm_real), intent(in) :: dt               !< Time step 
        real(gtm_real), intent(out) :: flow(ncell)     !< Cell and time centered flow
        real(gtm_real), intent(out) :: flow_lo(ncell)  !< Low face flow, time centered
        real(gtm_real), intent(out) :: flow_hi(ncell)  !< High face flow, time centered
        real(gtm_real), intent(out) :: area(ncell)     !< Cell center area, old time
        real(gtm_real), intent(out) :: area_lo(ncell)  !< Area lo face, time centered
        real(gtm_real), intent(out) :: area_hi(ncell)  !< Area hi face, time centered

        if (time <= reversal_time) then
            flow = const_flow
        else
            flow = minus * const_flow
        end if
    
        flow_hi = flow
        flow_lo = flow
        area = const_area
        area_lo = const_area
        area_hi = const_area

        return
    end subroutine

end module


