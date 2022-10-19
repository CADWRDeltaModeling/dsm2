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

!=================== name_to_objno=================================
! ---- This module contains functions that map object identifiers
!      and types to corresponding model object indexes and vice versa


!-----get internal object number, given object type and external (map) ID
integer function name_to_objno(ObjType, name)
      use gates, only: ngate, gateArray
      use groups, only: ngroup, groupArray
      use io_units
      use grid_data
      use constants
      use type_defs


!-----arguments
      integer*4 &
          ObjType              ! object type

      integer, external :: ext2int, ext2intnode

      character*32 :: name      ! name/identifier

!-----local variables
      integer i
      name_to_objno=miss_val_i
      name=trim(name)

      if (ObjType .eq. obj_reservoir) then
         do i = 1,nreser
            if (res_geom(i).name .eq. name) then
               name_to_objno = i
               return
            end if
         end do
      else if (ObjType .eq. obj_gate) then
         do i = 1,ngate
            !todo: trim may mess things up (eli)
            if (trim(gateArray(i).name) .eq. trim(name)) then
               name_to_objno = i
               return
            end if
         end do
      else if (ObjType .eq. obj_channel) then
         read(name,*) i
         name_to_objno = ext2int(i)
      else if (ObjType .eq. obj_node) then
         read(name,*) i
         name_to_objno = ext2intnode(i)
      else if (ObjType .eq. obj_obj2obj) then !fixme: number convention
         do i= 1,nobj2obj
            if (obj2obj(i).name .eq. name) then
               name_to_objno = i
               return
            end if
         end do
      else if (ObjType .eq. obj_qext) then
         do i=1,max_qext   !fixme this is max instead of nqext because tidefile doesnot pick up nqext
            if (qext(i).name .eq. name) then
               name_to_objno=i
               return
            end if
         end do
      else if (ObjType .eq. obj_stage) then
         do i=1,nstgbnd
            if (stgbnd(i).name .eq. name) then
               name_to_objno=i
               return
            end if
         end do
      else if (ObjType .eq. obj_group) then
         do i=0,ngroup
            if (groupArray(i).name .eq. name) then
               name_to_objno=i
               return
            end if
         end do
      else
         write(unit_error,611) ObjType, trim(name)
         call exit(3)
 611     format(/'Object type ',i4,' unrecognized. Object name:',a)
      end if

      return
end


!-----get internal object number, given object type and external (map) ID
subroutine objno_to_name(objtype, index, name)
      use gates, only: ngate, gateArray
      use groups, only: ngroup, groupArray
      use IO_Units
      use grid_data
      use constants
      use type_defs
      implicit none

!-----arguments
      integer*4 &
          ObjType              ! object type

      character*32 :: name      ! name/identifier

!-----local variables
      integer index

      name=' '
      if (ObjType .eq. obj_reservoir) then
	   name=res_geom(index).name
      else if (objtype .eq. obj_gate) then
         name=gateArray(index).name
      else if (ObjType .eq. obj_channel) then
         write(name,'(i5)') chan_geom(index).chan_no
      else if (ObjType .eq. obj_node) then
         write(name,'(i5)') node_geom(index).node_id
	   name=trim(adjustl(name))
      else if (ObjType .eq. obj_obj2obj) then !fixme: number convention
         name=obj2obj(index).name
      else if (ObjType .eq. obj_qext .or. ObjType .eq. obj_boundary_flow) then
	   name=qext(index).name
!------Jon add this line to account for sink/source object
      else if (ObjType .eq. obj_source_sink) then 
	   name=qext(index).name
      else if (ObjType .eq. obj_stage) then
	   name=stgbnd(index).name
      else if (ObjType .eq. obj_group) then
         name=groupArray(index).name
      else
         write(unit_error,611) ObjType, index
         call exit(3)
 611     format(/'Object type ',i4,' unrecognized. Object internal index:',a)
      end if


      return
end

!-----get internal object number, given object type and external (map) ID
subroutine obj_type_name(objtype, typename)
      use IO_Units
      use constants
      use type_defs
     	implicit none

!-----arguments
      integer*4 &
          ObjType              ! object type

      character*(*) :: typename


!-----local variables
      integer index

      typename=' '
      if (ObjType .eq. obj_reservoir) then
         typename='reservoir'
      else if (objtype .eq. obj_gate) then
         typename='gate'
      else if (ObjType .eq. obj_channel) then
         typename='channel'
      else if (ObjType .eq. obj_node) then
         typename='node'
      else if (ObjType .eq. obj_obj2obj) then !fixme: number convention
         typename='transfer'
      else if (ObjType .eq. obj_qext) then
         typename='qext'
      else if (ObjType .eq. obj_stage) then
         typename='stage_boundary'
      else if (ObjType .eq. obj_group) then
         typename='group'
      else if (ObjType .eq. obj_boundary_flow) then
         typename='flow_boundary'
      else if (ObjType .eq. obj_source_sink) then  !Jon add some extra line for more types
         typename='source_sink'                   !todo: Eli changed these blindly to conform to no-spaces
      else if (ObjType .eq. obj_oprule) then
         typename='operation_rule'
      else
         write(unit_error,611) ObjType
         call exit(3)
 611     format(/'Object type ',i4,' unrecognized.')
      end if
      return
end subroutine



!     get the integer type code given a character string representing the type
integer*4 function obj_type_code(objtype)
      use constants
	   implicit none
	   character*(*) :: objtype
      character*16  :: cstring
	   obj_type_code=miss_val_i
	   cstring=trim(objtype)
	   call locase(cstring)
	   if (index(cstring,"chan") .eq. 1) then 
	     obj_type_code=obj_channel
	   else if (index(cstring,"gate") .eq. 1) then
	     obj_type_code=obj_gate
	   else if (index(cstring,"res") .eq. 1) then
	     obj_type_code=obj_reservoir
	   else if (index(cstring,"node") .eq. 1) then
	     obj_type_code=obj_node	  
	   else if (index(cstring,"transfer") .eq. 1) then
	     obj_type_code=obj_obj2obj
	   else if (index(cstring,"group") .eq. 1) then
	     obj_type_code=obj_group
	   else if (index(cstring,"qext") .eq. 1) then
	     obj_type_code=obj_qext
	   else if (index(cstring,"flow_boundary") .eq. 1) then
	     obj_type_code=obj_qext
	   else if (index(cstring,"source_sink") .eq. 1) then
	     obj_type_code=obj_qext
	   else if (index(cstring,"stage") .eq. 1) then
	     obj_type_code=obj_stage
      end if
	   return
end


integer function obj_type_code2(typelabel) !todo reinvented the wheel (see above) We should figure which we want, but above is original
      use constants
      use io_units
      implicit none
      integer :: obj_type_code = 0
      character*(*) typelabel
      obj_type_code = miss_val_i
      if (typelabel .eq. "reservoir") then
         obj_type_code =obj_reservoir
      else if (typelabel .eq. "gate") then
         obj_type_code = obj_gate
      else if (typelabel .eq. "channel") then
         obj_type_code = obj_channel
      else if (typelabel .eq. "node") then
         obj_type_code = obj_node
      else if (typelabel .eq. "transfer") then
         obj_type_code = obj_obj2obj
      else if (typelabel .eq. "qext") then
         obj_type_code = obj_qext
      else if (typelabel(1:5) .eq. "stage") then
         obj_type_code = obj_stage
      else if (typelabel(1:5) .eq. "group") then
         obj_type_code = obj_group
      else if (typelabel .eq. "flow_boundary") then
         obj_type_code = obj_boundary_flow
      else if (typelabel .eq. "source_sink") then
         obj_type_code = obj_source_sink
      else if (typelabel .eq. "obj_oprule") then
         obj_type_code = obj_oprule
      else
         write(unit_error,611) typelabel
         call exit(3)
 611     format(/'Object type ',a,' unrecognized.')
      end if
      obj_type_code2 = obj_type_code
      return
end function







