!> Main program unit for testing advection

program test_transport_driver
  use fruit
  use test_gradient
  use test_extrapolate
  
  call init_fruit
  call test_gradient_calc
  call test_extrapolation
  call test_limiter
  
  call fruit_summary
end program test_transport_driver