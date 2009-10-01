!> Definition of model precision, precision-related constants and special values
!>@ingroup transport
module stm_precision

!> Precision of REAL
integer, parameter :: STM_REAL=8

real(STM_REAL) :: minus = -1.D0    !< Real constant -1. properly typed
real(STM_REAL) :: zero  =  0.D0    !< Real constant  0. properly typed
real(STM_REAL) :: one   =  1.D0    !< Real constant  1. properly typed
real(STM_REAL) :: two   =  2.D0    !< Real constant  2. properly typed
real(STM_REAL) :: half   =  5.D-1  !< Real constant  0.5 properly typed
real(STM_REAL) :: fourth =  2.5D-1 !< Real constant  0.25 properly typed

!> Absurd high value, for initialization and for marking undefined
!> data in calculations. This makes bugs easier to spot.
real(STM_REAL) :: LARGEREAL = 1.23456789D8

real(stm_real),parameter :: pi=3.14159265358979323846264338327950288



end module stm_precision