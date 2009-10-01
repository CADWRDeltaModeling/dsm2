module imposeboundarycondition
use stm_precision

contains
subroutine impose_bc(a, b, c, d,&
                                     bc_flag_up, bc_flag_down, ncell, c_s_up, c_s_down, coef, coef_one, area, k_s, area_plus_one, k_s_plus_one, flux_s_up_n, flux_s_up_n_plus_one, dx, conc,ntime,jvar)


integer :: ncell,ntime
integer ::jvar 
integer :: bc_flag_up
integer :: bc_flag_down
  
real(stm_real):: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real):: area(ncell+1,ntime), area_plus_one(ncell+1,ntime)
real(stm_real):: k_s(ncell+1,ntime), k_s_plus_one(ncell+1,ntime)
real(stm_real) :: c_s_up, c_s_down
real(stm_real) :: flux_s_up_n, flux_s_up_n_plus_one
real(stm_real) :: dx
real(stm_real) :: coef, coef_one
real(stm_real) :: conc(ncell), conc_plus_one(ncell)
   

   
  
   if(bc_flag_up == 1)then
     b(1) = one
     c(1) = zero
     d(ncell) = c_s_up
   end if

   if(bc_flag_up == 0)then
     b(1) = b(1)
     c(1) = c(1) - coef * area_plus_one(1,jvar) * k_s_plus_one(1,jvar)
     d(1) = d(1) - two * dx * flux_s_up_n_plus_one * coef * area_plus_one(1,jvar) * k_s_plus_one(1,jvar)
     d(1) = d(1) - two * dx * flux_s_up_n * coef_one * area(1,jvar) * k_s(1,jvar)
     d(1) = d(1) + coef_one * area(1,jvar) * k_s(1,jvar) * conc(2) 
   end if
   
   if(bc_flag_down == 1)then
     b(ncell)= one
     a(ncell)= zero
     d(ncell) = c_s_down
   end if

   if(bc_flag_down == 0)then
        
   end if
   

return
end subroutine

end module imposeboundarycondition