!> Hydrodynamics interface to be fulfilled by driver or application
!>@ingroup transport
module hydro_data_if
      !> Generic interface for fetching hydrodynamic data
      interface hydro_data
       !> Fill in hydrodynamic data.
       !> This data might be calculated from a function or provided by another module
       !> Note that continuity must be satisfied between time steps. The implementation
       !> must be provided by the driver or application 
        subroutine hydro_data8(flow,flow_lo,flow_hi,area,area_lo,area_hi,ncell,time,dt)
        use stm_precision
        implicit none
        integer, intent(in) :: ncell                   !< number of cells
        real(STM_REAL), intent(in) :: time             !< time of request "old time"
        real(STM_REAL), intent(in) :: dt               !< time step for 
        real(STM_REAL), intent(out) :: flow(ncell)     !< cell and time centered flow
        real(STM_REAL), intent(out) :: flow_lo(ncell)  !< lo face flow, time centered
        real(STM_REAL), intent(out) :: flow_hi(ncell)  !< hi face flow, time centered
        real(STM_REAL), intent(out) :: area(ncell)     !< cell center area, old time
        real(STM_REAL), intent(out) :: area_lo(ncell)  !< area lo face, time centered
        real(STM_REAL), intent(out) :: area_hi(ncell)  !< area hi face, time centered
        end subroutine
      end interface
end module