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

c=================== name_to_objno=================================
c ---- This module contains functions that map object identifiers
c      and types to corresponding model object indexes and vice versa


c-----get internal object number, given object type and external (map) ID
      integer function name_to_objno(ObjType, name)
      use gates, only: ngate, gateArray
      use groups, only: ngroup, groupArray
      use io_units
      use grid_data
      use constants
      use type_defs


c-----arguments
      integer*4
     &     ObjType              ! object type

      integer, external :: ext2int, ext2intnode

      character*32 :: name      ! name/identifier

c-----local variables
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
         read(name,'(i)') i
         name_to_objno = ext2int(i)
      else if (ObjType .eq. obj_node) then
         read(name,'(i)') i
         name_to_objno = ext2intnode(i)
      else if (ObjType .eq. obj_obj2obj) then !fixme: number convention
         do i= 1,nobj2obj
            if (obj2obj(i).name .eq. name) then
               name_to_objno = i
               return
            end if
         end do
      else if (ObjType .eq. obj_qext)then
         do i=1,max_qext   !fixme this is max instead of nqext because tidefile doesnot pick up nqext
            if (qext(i).name .eq. name) then
               name_to_objno=i
               return
            end if
         end do
      else if (ObjType .eq. obj_stage)then
         do i=1,nstgbnd
            if (stgbnd(i).name .eq. name) then
               name_to_objno=i
               return
            end if
         end do
      else if (ObjType .eq. obj_group)then
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


c-----get internal object number, given object type and external (map) ID
      subroutine objno_to_name(objtype, index, name)
      use gates, only: ngate, gateArray
      use groups, only: ngroup, groupArray
      use IO_Units
      use grid_data
      use constants
      use type_defs
	implicit none

c-----arguments
      integer*4
     &     ObjType              ! object type

      character*32 :: name      ! name/identifier

c-----local variables
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
      else if (ObjType .eq. obj_qext .or. ObjType .eq. obj_boundary_flow)then
	   name=qext(index).name
c------Jon add this line to account for sink/source object
      else if (ObjType .eq. obj_source_sink)then 
	   name=qext(index).name
      else if (ObjType .eq. obj_stage)then
	   name=stgbnd(index).name
      else if (ObjType .eq. obj_group)then
         name=groupArray(index).name
      else
         write(unit_error,611) ObjType, index
         call exit(3)
 611     format(/'Object type ',i4,' unrecognized. Object internal index:',a)
      end if


      return
      end

c-----get internal object number, given object type and external (map) ID
      subroutine obj_type_name(objtype, typename)
      use IO_Units
      use constants
      use type_defs
     	implicit none

c-----arguments
      integer*4
     &     ObjType              ! object type

	character*(*) :: typename


c-----local variables
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
      else if (ObjType .eq. obj_qext)then
         typename='qext'
      else if (ObjType .eq. obj_stage)then
         typename='stage_boundary'
      else if (ObjType .eq. obj_group)then
	   typename='group'
      else if (ObjType .eq. obj_boundary_flow)then
	   typename='flow_boundary'
      else if (ObjType .eq. obj_source_sink)then  !Jon add some extra line for more types
	   typename='source/sink'
	else if (ObjType .eq. obj_oprule)then
	   typename='operation rule'
      else
         write(unit_error,611) ObjType
         call exit(3)
 611     format(/'Object type ',i4,' unrecognized.')
      end if
      return
      end






