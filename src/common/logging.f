C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

      module logging
      integer,parameter :: LOG_ERROR = 0
      integer,parameter :: LOG_WARNING = 1
      integer,parameter :: LOG_INFO = 2
      integer,parameter :: LOG_DEBUG = 2
      integer :: print_level   ! diagnostic printout level
      
!      contains:

!      subroutine dsm2_log(level,message)
!      implicit none
!      character*(*) :: message
!      integer :: level
!      print *,"hello"
!      return 
!      end subroutine
!      
      end module
      
      