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

subroutine process_group(name, id)
      use groups
      use constants
      use io_units
      implicit none
      character*32 :: name
      integer :: id
      integer :: groupno
      integer :: membertype
      integer, external :: name_to_objno
      call locase(name)
      groupno=name_to_objno(obj_group,name)
      if(groupno .eq. miss_val_i) then
         ngroup=ngroup+1
         groupArray(ngroup).id=ID
         groupArray(ngroup).name=name
	   if (ngroup .gt. MAX_GROUPS) then
	      write(unit_error,*)"Maximum number of groups exceeded"
	      call exit(-1)
         end if
      end if
      return    
end subroutine

subroutine process_group_member(groupname, &
                                     membertype, &
                                     pattern)
      use groups
      use io_units
      implicit none
      character*32 :: groupname
      integer :: membertype
      character*32 :: pattern
      integer, external :: name_to_objno
      integer :: groupNdx = miss_val_i
      integer :: npattern = miss_val_i
      call locase(groupname)
      
      if (membertype .eq. obj_group) then ! member is another group
            write(unit_error, *)"Subgroups not supported"
	      call exit(-2)
      end if
      groupNdx = name_to_objno(obj_group,groupname)
      npattern = groupArray(GroupNdx).nMemberPatterns + 1
      if (npattern .gt. MAX_MEMBER_PATTERNS) then
	   write(unit_error,*)"Maximum number of member patterns exceeded for group"
         call exit(-2)
         return
	endif      
      
      groupArray(GroupNdx).memberPatterns(npattern).obj_type=membertype
      groupArray(GroupNdx).memberPatterns(npattern).pattern= &
                                        trim(adjustl(pattern))     
      groupArray(GroupNdx).nMemberPatterns=npattern  
      return
end subroutine
      
	    
          