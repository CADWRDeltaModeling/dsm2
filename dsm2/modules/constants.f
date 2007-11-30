
      Module PhysicalConstants
	real*8,save :: gravity  !Acceleration due to gravity, in user specified units
	real*8,save :: sqrt2g
	real*8,parameter :: pi=3.14159265358979

      contains

	logical function verify_gravity_terms()
	use io_units
	implicit none
	verify_gravity_terms = .true.
	if(abs(gravity - 32.2) .gt. 0.1 .and. abs(gravity - 9.8) .gt. 0.1 )then
	  write(unit_error, "('a,i,a,a')") "Gravity value of ", gravity, " not expected.",
     &   " Either change units or alter model to accomodate new units."
        verify_gravity_terms= .false.
	  return
	end if
      sqrt2g=sqrt(2.D0*gravity)
      return
	end function

	End Module PhysicalConstants






