!> Sample initial conditions for tests
module example_initial_conditions

contains

!> Initialize the concentration fields with gaussian plume
!> todo: abstract the part that does the shape
!> somewhere so that it is easy to write a corresponding
!> routine to verify simple advection and clarify this in the 
!> documentation
subroutine gaussian_ic  ! just an idea -- any shape is fine
end subroutine

!> Initialize the concentration fields with a step function
subroutine step_ic

end subroutine


end module