module matrixsolver 
 use stm_precision
contains

SUBROUTINE tridi_solver(x & 
                        ,a, b, c, d, ncell)

integer :: ncell
integer :: ivar

real(stm_real) :: a(ncell), b(ncell), c(ncell), d(ncell), x(ncell)
real(stm_real) :: gam(ncell)
real(stm_real) :: bet

            print *, "xxx in solver sub =" ,xxx

if(b(1) == 0)then
  print *, 'there is a problem here'
  stop
end if

bet = b(1)
x(1) = d(1) / bet

do ivar=2, ncell
  gam(ivar) = c(ivar - 1) / bet
  bet = b(ivar) - a(ivar) * gam(ivar)
  if(bet == 0)then
    print *, 'tridiagonal solver failed'
  end if
  x(ivar) = (d(ivar) - a(ivar) * x(ivar - 1)) / bet 
end do

do ivar= ncell-1, 1, -1
  x(ivar) = x(ivar) - gam(ivar + 1) * x(ivar + 1)
end do



return
end subroutine tridi_solver

subroutine pentadi_solver

return
end subroutine pentadi_solver


subroutine heptadi_solver

return
end subroutine heptadi_solver

subroutine sparse_solver

return
end subroutine sparse_solver


end module matrixsolver 