!<license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
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

!> Routines to print out the logs
!>@ingroup gtm_core
module gtm_logging

    use gtm_precision
    ! todo: an initialization expression is required when using the PARAMETER attribute
    ! i added one
    integer, parameter :: ERROR = 8
    integer, parameter :: WARNING = 6
    integer, parameter :: FINE = 4
    integer, parameter :: INFO = 2
    
    integer, parameter :: LOG_DIR_LEN = 132
    character(LEN=LOG_DIR_LEN) :: log_directory = "."    
    
    ! added I/O unit
    integer :: debug_unit = 111                    !< debug print unit 
    
    interface printout
        module procedure printout
    end interface
        
    contains

    !> Set the name of the directory in which file-based logs files are kept
    subroutine set_log_directory(name)
        implicit none
        character(len=*), intent(in) :: name !< New name for log directory
        !if (len(name) .gt. LOG_DIR_LEN) call gtm_fatal("Log directory name is too long. Try relative path?")
        log_directory = " "
        log_directory = trim(name)
        return
    end subroutine

    !> Set the name of the directory to which logging files are kept
    subroutine prepend_log_directory(name)
        implicit none
        character(len=*), intent(inout) :: name !< New name for log directory
        character(len=len(name)) :: temp_name   !< Temporary name for log directory
        temp_name = name
        if ((len_trim(log_directory) + len_trim(name))  .gt. len(name)) then
        !call gtm_fatal("Log directory name is too long. Try relative path?")
        end if
        temp_name = name
        write(name,'(a,a,a)')trim(log_directory),"/",trim(name)
        return
    end subroutine    
 
    !> Prints the error message and the level of occured error
    subroutine gtm_log(level,message)
        implicit none
        ! todo: This name does not have a type, and must have an explicit type
        ! I added something just to compile
        integer, intent(in) :: level                !< Level of error
        character(LEN=*), intent(in) :: message     !< Message triggered by the error 
        !todo: do real logging
        print*,message
        return
    end subroutine
    
    !> Prints an array to file
    subroutine printout(arr,x,filename)
        implicit none
        real(gtm_real),intent(in) :: arr(:)         !< Array values
        real(gtm_real),intent(in) :: x(:)           !< X values
        character(LEN=*)          :: filename       !< Name of file to write
        !--local
        integer                   :: icell          !< Counter
        integer                   :: nx             !< Number of cells
        nx = size(arr)
        open(unit = 11, file = filename)
        do icell = 1,nx
          write(11,'(f12.4, f22.16)') x(icell), arr(icell)
        end do
        close(11)
        return
    end subroutine
    
end module