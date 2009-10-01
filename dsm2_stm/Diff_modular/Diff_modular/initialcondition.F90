
 module initialcondition
 use stm_precision
! use allocation
 
 contains
 subroutine initial_c(conc, &
                            i_case,ncell,dx)
                               
integer :: i_case
integer :: ncell
integer :: kvar

real (stm_real) :: ax,dx
real (stm_real) :: conc(ncell)

!call allocationdiff

if(i_case == 1) then
do kvar=1, (ncell - 1) / 2 + 1
 conc(kvar) = two * (kvar - 1) * dx
end do
      
kvar_march: do kvar=(ncell - 1) / 2 + 2, ncell
 conc(kvar) = two - two * (kvar - 1) * dx
end do kvar_march



elseif(i_case == 2)then
kvar_march2: do kvar=1, ncell
 ax = (kvar - 1) * dx + 0.1d0
 conc(kvar) = two * ax + two*two * cos(0.5d0 * pi * ax)

     
end do kvar_march2

elseif (i_case == 3)then



elseif (i_case == 4) then

else
print *, 'Error! please recheck the boundary condition flag'  
end if

print *, 'Boundary condition was loaded'

return
end subroutine initial_c
 
 end module initialcondition