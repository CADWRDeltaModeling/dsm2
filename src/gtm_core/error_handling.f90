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

!> Routine sends the error message to logging routine 
!>@ingroup gtm_core
module error_handling

    contains
        
    !> Logs the input message as an error and stops execution with error code
    subroutine gtm_fatal(message)
        use logging, only: gtm_log, ERROR
        implicit none
        character(LEN=*), intent(in):: message      !< error message
        call gtm_log(ERROR, message)
        stop 1
        return
    end subroutine
    
    !> Verify error
    subroutine verify_error(ierror,message)
        implicit none
        integer,intent(in) :: ierror               !< error code
        character(len=*),intent(in) :: message     !< error message
        if (ierror .eq. 0) return
        write(*,"(/,a,/,'[FATAL]',1x,i)") message,ierror
        call exit(ierror)
    end subroutine

end module