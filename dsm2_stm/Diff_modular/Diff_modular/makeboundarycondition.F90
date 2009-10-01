module makeboundarycondition

use stm_precision

contains
subroutine make_bc (c_s_up,c_s_down,flux_s_up_n,flux_s_up_n_plus_one,flux_s_down_n,flux_s_down_n_plus_one & 
                                                                                                    ,i_case,bc_flag_up,bc_flag_down,time,dt)

integer ::i_case,bc_flag_up,bc_flag_down

real(stm_real)::c_s_up,c_s_down,flux_s_up_n,flux_s_up_n_plus_one,time,dt,flux_s_down_n,flux_s_down_n_plus_one

if(i_case == 1)then
   bc_flag_up = one
   bc_flag_down = one
   c_s_up = zero
   c_s_down = zero
end if

if(i_case == 2)then
   bc_flag_up = zero
   bc_flag_down = one
   flux_s_up_n = two - two * pi * sin(0.05d0 * pi) * exp(minus * pi * pi / 4.0d0 * (time - dt))
   flux_s_up_n_plus_one = two - two * pi * sin(0.05d0 * pi) * exp(minus * pi * pi / 4.0d0 * time)
   c_s_down = two
end if

return
end subroutine





end module makeboundarycondition