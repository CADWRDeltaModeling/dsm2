!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!> Definition of model precision, precision-related constants and special values
!>@ingroup gtm_core
module gtm_precision

   !> Precision of REAL number
   integer, parameter :: gtm_real = 8

   real(gtm_real), parameter :: minus = -1.d0          !< Real constant -1.   properly typed
   real(gtm_real), parameter :: zero  =  0.d0          !< Real constant  0.   properly typed
   real(gtm_real), parameter :: one   =  1.d0          !< Real constant  1.   properly typed
   real(gtm_real), parameter :: two   =  2.d0          !< Real constant  2.   properly typed
   real(gtm_real), parameter :: three =  3.d0          !< Real constant  3.   properly typed
   real(gtm_real), parameter :: four  =  4.d0          !< Real constant  4.   properly typed
   real(gtm_real), parameter :: five  =  5.d0          !< Real constant  5.   properly typed
   real(gtm_real), parameter :: six   =  6.d0          !< Real constant  6.   properly typed
   real(gtm_real), parameter :: seven =  7.d0          !< Real constant  7.   properly typed
   real(gtm_real), parameter :: eight =  8.d0          !< Real constant  8.   properly typed
   real(gtm_real), parameter :: nine  =  9.d0          !< Real constant  9.   properly typed
   real(gtm_real), parameter :: ten   =  1.d1          !< Real constant  10.  properly typed
   real(gtm_real), parameter :: sixteen  =  1.6d1      !< Real constant  16.  properly typed
   real(gtm_real), parameter :: sixty = 6.d1           !< Real constant  60.  properly typed
   real(gtm_real), parameter :: half     =  5.d-1      !< Real constant  0.5  properly typed
   real(gtm_real), parameter :: third = one/three      !< Real constant  1/3  properly typed
   real(gtm_real), parameter :: fourth   =  2.5d-1     !< Real constant  0.25 properly typed
   real(gtm_real), parameter :: pi = acos(-one)        !< Pi 
   real(gtm_real), parameter :: hydro_theta =  6.d-1   !< Real constant 0.6  properly typed
   
   integer, parameter :: LARGEINT = -123456789
   
   !> Missing values maker
   integer :: miss_val_i = -901
   real(gtm_real) :: miss_val_r = -901.
   character(len=1), parameter :: miss_val_c = char(1)

   !> Absurd high value, for initialization and for marking undefined
   !> data in calculations. This makes bugs easier to spot.
   real(gtm_real), parameter :: LARGEREAL = 1.23456789d8

   !> Default epsilon for testing closeness 
   real(gtm_real), parameter :: eps = 1.d-15

   !> Weaker epsilon for testing approximate equality or algorithms
   real(gtm_real), parameter :: weak_eps = 1.d-9
   
   !> Weakest epsilon for testing approximate equality
   real(gtm_real), parameter :: weakest_eps = 1.d-3
   
   !> Denominator verification
   real(gtm_real), parameter :: Small = 1.00e-6
   
   ! todo: add G = 9.80 
   ! todo: add kapa = 0.41 here or in sed variables

end module
