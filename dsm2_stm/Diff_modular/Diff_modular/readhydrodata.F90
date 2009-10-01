module readhydrodata

use stm_precision
!use allocation

contains 
    subroutine read_hydro_area_ks(area,area_plus_one,k_s,k_s_plus_one,&
                                                                        ncell,jvar,ntime)
  integer :: ncell,ntime
integer :: ivar, jvar
    
    real(stm_real) :: area(ncell+1,ntime),area_plus_one(ncell+1,ntime),k_s(ncell+1,ntime),k_s_plus_one(ncell+1,ntime)
 !     call allocationdiff
 
    do ivar=1,ncell+1
       area(ivar,jvar) = one
       area_plus_one(ivar,jvar) = one
       k_s(ivar,jvar) = one
       k_s_plus_one(ivar,jvar) = one
    end do

    return
    end subroutine read_hydro_area_ks

end module readhydrodata