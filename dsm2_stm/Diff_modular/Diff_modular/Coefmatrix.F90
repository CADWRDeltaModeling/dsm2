module coefmatrix

use stm_precision

contains

subroutine bigk(a, b, c, d, &
                                ncell, coef, coef_one, area, area_plus_one, k_s, k_s_plus_one, conc,jvar,ntime)


  
integer :: ncell,ntime 
integer ::ivar, jvar, kvar
  
real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real) :: area(ncell+1,ntime), area_plus_one(ncell+1,ntime)
real(stm_real) :: k_s(ncell+1,ntime), k_s_plus_one(ncell+1,ntime)
real(stm_real) :: conc(0:ncell+1), conc_plus_one(0:ncell+1)
real(stm_real) :: area_aux_n, area_aux_n_plus_one
real(stm_real) :: coef, coef_one
  
 
    
   do ivar=1,ncell
    area_aux_n_plus_one = (area_plus_one(ivar,jvar) + area_plus_one(ivar+1,jvar)) / two
    area_aux_n = (area(ivar,jvar) + area(ivar+1,jvar)) / two
    b(ivar)=area_aux_n_plus_one + coef * area_plus_one(ivar+1,jvar) * k_s_plus_one(ivar+1,jvar) 
    b(ivar)=b(ivar) + coef * area_plus_one(ivar,jvar) * k_s_plus_one(ivar,jvar) 
    a(ivar)= minus * coef * area_plus_one(ivar,jvar) * k_s_plus_one(ivar,jvar)
    c(ivar)= minus * coef * area_plus_one(ivar+1,jvar) * k_s_plus_one(ivar+1,jvar)
   
       if (ivar == ncell) then 
          conc(ivar+1) = 0.0d0
       endif
!       else
!       concstar=conc(ivar+1) 
!       end if  
   
    d(ivar)=area_aux_n * conc(ivar) + coef_one * conc(ivar+1) * area(ivar+1,jvar) * k_s(ivar+1,jvar)
      d(ivar)= d(ivar) -  coef_one * conc(ivar) * area(ivar+1,jvar) * k_s(ivar+1,jvar)
    d(ivar)= d(ivar) -  coef_one * conc(ivar) * area(ivar,jvar) * k_s(ivar,jvar)
   
!        if (ivar == 1) then 
!        concstar2 =conc(ivar)
!        else
!        concstar2=conc(ivar-1)
!        end if 

       if (ivar == 1) then 
          conc(ivar-1) = 0.0d0
       endif
   
       d(ivar)= d(ivar) +  coef_one * conc(ivar-1) * area(ivar,jvar) * k_s(ivar,jvar)
   end do
         
return
end subroutine




end module coefmatrix