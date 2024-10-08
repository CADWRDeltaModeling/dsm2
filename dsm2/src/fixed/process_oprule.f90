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

subroutine process_oprule(name, action, trigger)
      use groups
      use constants
      use logging
      use io_units
      implicit none
      character*32 :: name
      character*512 :: action
      character*512 :: trigger
      character*1024 :: ruletext = " "
      logical :: parse_rule
      ruletext=" "
      call locase(name)

      write(ruletext,"(a,1x,':=',1x,a,1x,'WHEN',1x,a,';')") &
          trim(name),trim(action),trim(trigger)
      if(print_level .ge. 3) then
          write(unit_screen,"(/'Parsing rule: ',/a)"),trim(ruletext)
      end if
      if (.not. parse_rule(trim(ruletext))) then
          write(unit_error,"(/'Error parsing rule: ',/a)"),trim(ruletext)
          call exit(-3)
          return
      endif
end subroutine



subroutine process_oprule_expression(name, definition)
      use groups
      use constants
      use logging
      use io_units
      implicit none
      character*32 :: name
      character*512 :: definition, definition_text=" "
      logical :: parse_rule
      call locase(name)
      definition_text=" "

      write(definition_text,"(a,1x,':=',1x,a,';')") &
          trim(name),trim(definition)
      if(print_level .ge. 3) then
          write(unit_screen,"(/'Parsing oprule expression: ',/a)"),trim(definition)
      end if
      if (.not. parse_rule(trim(definition_text))) then
          write(unit_error,"(/'Error parsing expression: ',/a)"), trim(definition)
          call exit(-3)
          return
      endif
end subroutine

