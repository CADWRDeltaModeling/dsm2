
!> Defines state variables for the transport problem
!> as well as functions to allocate them
!>@ingroup transport
module state_variables
use stm_precision
integer :: ncell  !< number of computation cells
integer :: nvar   !< number of variables

!> Concentration in the current/new time step,
!> dimensions (ncell, nvar)
real(STM_REAL),save,allocatable :: conc(:,:)

!> Concentration in the previous time step,
!> dimensions (ncell, nvar)
real(STM_REAL),save,allocatable :: conc_prev(:,:)

!> Cell-centered area
!> dimensions (ncell)
real(STM_REAL),save,allocatable :: area(:)

!> Face area on lo side of cell (so this is cell-indexed),
!> dimensions (ncell)
!> todo: we should talk about this design a bit
!> do not change this without talking to Eli
real(STM_REAL),save,allocatable :: area_lo(:)

!> Face-centered area on hi side of cell (so this is cell-indexed),
!> dimensions (ncell)
real(STM_REAL),save,allocatable :: area_hi(:)

!> face-centered flow on lo side of cell  (so this is cell-indexed),
!> dimensions (ncell)
!> todo: we should talk about this design a bit
real(STM_REAL),save,allocatable :: flow_lo(:)

!> face-centered flow on hi side of cell  (so this is cell-indexed),
!> dimensions (ncell)
real(STM_REAL),save,allocatable :: flow_hi(:)


contains

!> Allocate the state variables consistently
!> including concentration and hydrodynamics.
!> Initial value is LARGEREAL
subroutine allocate_state(a_ncell, a_nvar)
implicit none
integer, intent(in) :: a_ncell !< Number of requested cells
integer, intent(in) :: a_nvar  !< Number of constituents
ncell = a_ncell
nvar  = a_nvar
allocate(conc(ncell,nvar), conc_prev(ncell,nvar))
conc      = LARGEREAL  ! absurd value helps expose bugs  
conc_prev = LARGEREAL
allocate(area(ncell), area_lo(ncell), area_hi(ncell))
area      = LARGEREAL
area_lo   = LARGEREAL
area_hi   = LARGEREAL
allocate(flow_lo(ncell), flow_hi(ncell))
flow_lo   = LARGEREAL
flow_hi   = LARGEREAL
return
end subroutine

!> Deallocate the state variables
!> including concentration and hydrodynamics
!> and reset ncell and nvar to zero.
subroutine deallocate_state
implicit none
ncell = 0
nvar  = 0
deallocate(conc, conc_prev)
deallocate(area, area_lo, area_hi)
deallocate(flow_lo, flow_hi)
return
end subroutine





end module



