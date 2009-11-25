!> Main program unit for testing advection
!>@ingroup test
program test_transport_driver
  use fruit
  use test_gradient
  use test_extrapolate
  use test_prim_cons_conversion
  use test_uniform_flow
  use example_initial_conditions
  
  call init_fruit
  call test_gradient_calc
  call test_limiter
  call test_prim_cons_convert
  call test_example_initial_conditions
  call test_extrapolation
  call test_uniform_flow_advection
  
  call fruit_summary
end program test_transport_driver