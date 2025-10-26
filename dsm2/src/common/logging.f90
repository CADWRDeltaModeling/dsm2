!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

module logging
    use stdlib_logger, only: logger_type, error_level

    implicit none
    integer, parameter :: LOG_ERROR = 0
    integer, parameter :: LOG_WARNING = 1
    integer, parameter :: LOG_INFO = 2
    integer, parameter :: LOG_DEBUG = 2
    integer:: print_level   ! diagnostic printout level

    type(logger_type) :: logger
    type(logger_type) :: stderr_logger

contains

    subroutine init_loggers()
    !! Initialize loggers for standard and error logging
    !!
    !! Sets up two loggers: one, logger, for general logging to screen and file,
    !! and another, error_logger, for error logging to error unit and screen.
    !! The error logging is sent to stderr while the general logging is sent to
    !! stdout.
    !! Both loggers are available via the module variables logger and error_logger.
        use io_units, only: unit_error, unit_screen
        integer :: unit
        call logger%add_log_unit(unit=unit_screen)
        call logger%add_log_file('dsm2.log', unit=unit)
        call stderr_logger%add_log_unit(unit=unit_error)
        call stderr_logger%add_log_unit(unit=unit)
    end subroutine init_loggers

end module logging

