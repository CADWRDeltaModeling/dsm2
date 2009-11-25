
!> Definition of model precision, precision-related constants and special values
!>@ingroup transport
module stm_precision

!> Precision of REAL
integer, parameter :: STM_REAL=8

real(STM_REAL), parameter :: minus =-1.D0     !< Real constant -1. properly typed
real(STM_REAL), parameter :: zero  =  0.D0    !< Real constant  0. properly typed
real(STM_REAL), parameter :: one   =  1.D0    !< Real constant  1. properly typed
real(STM_REAL), parameter :: two   =  2.D0    !< Real constant  2. properly typed
real(STM_REAL), parameter :: three =  2.D0    !< Real constant  3. properly typed
real(STM_REAL), parameter :: four  =  4.D0    !< Real constant  4. properly typed
real(STM_REAL), parameter :: five  =  5.D0    !< Real constant  5. properly typed
real(STM_REAL), parameter :: six   =  6.D0    !< Real constant  6. properly typed
real(STM_REAL), parameter :: seven =  7.D0    !< Real constant  7. properly typed
real(STM_REAL), parameter :: eight =  8.D0    !< Real constant  8. properly typed
real(STM_REAL), parameter :: nine  =  9.D0    !< Real constant  9. properly typed
real(STM_REAL), parameter :: ten   =  1.D1    !< Real constant  10. properly typed
real(STM_REAL), parameter :: sixteen  =  1.6D1  !< Real constant  16. properly typed
real(STM_REAL), parameter :: half     =  5.D-1  !< Real constant  0.5 properly typed
real(STM_REAL), parameter :: fourth   =  2.5D-1 !< Real constant  0.25 properly typed


!> Absurd high value, for initialization and for marking undefined
!> data in calculations. This makes bugs easier to spot.
real(STM_REAL), parameter :: LARGEREAL = 1.23456789D8 


end module