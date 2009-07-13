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

c-----I/O unit values
      Module IO_Units

      integer, parameter :: unit_error=0     ! error messages
      integer, parameter :: unit_input=11    ! input unit
      integer, parameter :: unit_screen=6    ! output unit to screen (MUST be 6)
      integer, parameter :: unit_output=14   ! output file
      integer, parameter :: unit_text=13     ! temporary (scratch) text file output
      integer, save :: unit_hydro
      End Module IO_Units
