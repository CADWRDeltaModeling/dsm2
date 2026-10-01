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
    integer:: oprule_log_level = -1   ! operating rule log level scalar; -1 = not set (see get_oprule_log_level)
    ! operating rule log options (SCALAR table); -1 = not set, the getters in model_interface give the defaults
    character(len=32):: oprule_log_file = ' '
    integer:: oprule_log_text = -1             ! 0 false, 1 true: also write the text log oprule_log.txt
    integer:: oprule_log_devices = -1          ! 0 false, 1 true
    integer:: oprule_log_context = -1          ! 0 false, 1 true
    real*8:: oprule_log_tol_op = -1.d0         ! op coefficient change worth a row
    real*8:: oprule_log_tol_dim = -1.d0        ! height, elevation, width change worth a row (ft)
    integer:: oprule_log_trace_interval = -1   ! steps between dense trace rows, 0 off
    real*8:: oprule_log_flush_hours = -1.d0    ! simulated hours between flushes of the log file

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

