module allocation

!use stm_precision

contains

subroutine allocationdiff

implicit none


integer,parameter :: ncell = 97
!integer,parameter :: ntime = 5000
!integer,parameter:: stm_real=8
!
!
!integer :: ivar, jvar, kvar, ivar2, jvar2 
!integer:: i_case
!integer :: bc_flag_up
!integer :: bc_flag_down
!
!
!real(stm_real) :: area(ncell+1,ntime), area_plus_one(ncell+1,ntime)
!real(stm_real) :: k_s(ncell+1,ntime), k_s_plus_one(ncell+1,ntime)
!real(stm_real) :: conc(ncell), conc_plus_one(ncell)
!real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
!real(stm_real) :: dt
!real(stm_real) :: dx
!real(stm_real) :: theta
!real(stm_real) :: coef, coef_one
!real(stm_real) :: time
!real(stm_real) :: c_s_up, c_s_down
!real(stm_real) :: ax
!real(stm_real) :: flux_s_up_n, flux_s_up_n_plus_one, flux_s_down_n, flux_s_down_n_plus_one
!real(stm_real) :: conc_result(ncell, ntime)
!real(stm_real) :: conc_exact(ncell, ntime)
!real(stm_real) :: error(ncell, ntime)
!
!           
!theta = 0.5d0
!dt = 0.001d0/2.0d0
!i_case = 2
!
!if(i_case == 1)dx = 0.05d0
!if(i_case == 2)dx = 0.9d0/(ncell-1)                        !0.045d0
!if(i_case == 3) dx= 0.045d0
!if(i_case == 4) dx= 0.045d0
!
!coef = theta * dt / dx /dx
!coef_one = (1 - theta) * dt / dx / dx
return
end subroutine allocationdiff
end module allocation