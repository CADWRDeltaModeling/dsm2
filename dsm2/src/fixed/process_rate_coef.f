C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

      subroutine process_rate_coef(group_name,
     &                             constituent,
     &                             rate_variable,
     &                             coefficient_value)            
	use Groups, only: groupContains,IsAllChannelReservoir,groupArray
	use rate_coeff_assignment,only:assign_rate_to_group,rate_var_require_flag
	use common_qual
	use constants
	use io_units
	use logging      
      implicit none
      character*16 rate_variable,constituent
      
      character*32 group_name
      character errm*128 !todo: this is not good style      
      integer :: rate_variable_id
      integer :: constituent_id
      integer :: groupno
      integer :: istat
      integer, external :: name_to_objno
      real*8  :: coefficient_value
      integer,external :: rate_variable_code
      integer,external :: ncc_code
      
      call locase(rate_variable)
      call locase(constituent)
      rate_variable_id = rate_variable_code(rate_variable)
      if (rate_variable_id .eq. miss_val_i)then
          write (unit_error,'(a,1x,a)')"Rate variable not recognized:",
     &    trim(rate_variable)
          call exit(-3)
      end if
      constituent_id = ncc_code(constituent)
      if (constituent_id .eq. miss_val_i)then
          write(unit_error,'(a,1x,a)')"Constituent in rate coefficient assignment "//
     &    "not recognized:",constituent
          call exit(-3)
      end if
      groupno = name_to_objno(obj_group, group_name)
	if (groupno.lt.0) then
	   write(unit_error, '(a,1x,a)') 'Group in rate coefficient assignment: ' // 
     &     "not recognized: ",group_name
	   call exit(-3)
      end if
	if (not(IsAllChannelReservoir(groupArray(groupno)))) then
         write(unit_error, '(a)') 
     &                "Members of group "//group_name//
     &                " are not all channel or reservior"
         call exit(-3)
      end if
      coefficient_value=coefficient_value/24. ! convert per day to per hour
      call assign_rate_to_group(groupno,rate_variable_id,constituent_id,
     &                          coefficient_value,istat,errm)
      ! todo -- this is pretty strange style
	if (istat.lt.0) then
         write(unit_error, '(a)') errm
         call exit(-3)
      end if
      rate_var_require_flag(constituent_id,rate_variable_id)=.true.
      return
      end subroutine