!> Main program unit for testing advection
!>@ingroup test
program test_transport_driver
  use fruit
  use test_gradient
  use test_extrapolate
  use test_prim_cons_conversion
  
  call init_fruit
  call test_gradient_calc
  call test_extrapolation
  call test_limiter
  call test_prim_cons_convert
  
  call fruit_summary
end program test_transport_driver