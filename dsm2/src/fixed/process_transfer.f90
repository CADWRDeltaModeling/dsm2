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

subroutine process_transfer(ID, &
                                  TransName, &
                                  FromType, &
                                  FromObjID, &
                                  ToType, &
                                  ToObjID)

      use IO_Units
      use grid_data
      use logging
      use constants
      implicit none
      character*32 TransName, FromObjID, ToObjID
      character*16 FromType
      character*16 ToType
      integer FromObjType, ToObjType, ID
      integer, external :: ext2intnode
      integer, external :: name_to_objno
      integer, external :: obj_type_code
      call locase(FromType)
      call locase(ToType)
      call locase(TransName)
      call locase(FromObjID)
      call locase(ToObjID)
      
      
      FromObjType = obj_type_code(FromType)
      ToObjType = obj_type_code(ToType)

      nobj2obj=nobj2obj+1
      if (nobj2obj .gt. max_obj2obj) then
         write(unit_error,630) &
         'Too many object connections specified; max allowed is:' &
         ,max_obj2obj
          call exit(-1)
      endif
      obj2obj(nobj2obj).Use=.true.
      obj2obj(nobj2obj).ID=ID
      obj2obj(nobj2obj).name=TransName
      obj2obj(nobj2obj).from_obj.obj_type=FromObjType
      if (FromObjType .eq. obj_reservoir) then
         obj2obj(nobj2obj).from_obj.obj_name=FromObjID
         obj2obj(nobj2obj).from_obj.obj_no=name_to_objno(obj_reservoir,FromObjID)
      else if (FromObjType .eq. obj_node) then
         read(FromObjID,'(i10)')obj2obj(nobj2obj).from_obj.obj_no
         obj2obj(nobj2obj).from_obj.obj_no= &
        ext2intnode(obj2obj(nobj2obj).from_obj.obj_no)
         obj2obj(nobj2obj).from_obj.obj_name=FromObjID
      end if
      obj2obj(nobj2obj).to_obj.obj_type=ToObjType
      if (ToObjType .eq. obj_reservoir) then
         obj2obj(nobj2obj).to_obj.obj_name=ToObjID
         obj2obj(nobj2obj).to_obj.obj_no=name_to_objno(obj_reservoir,ToObjID)
      else if (ToObjType .eq. obj_node) then
         read(ToObjID,'(i10)')obj2obj(nobj2obj).to_obj.obj_no
         obj2obj(nobj2obj).to_obj.obj_no= &
            ext2intnode(obj2obj(nobj2obj).to_obj.obj_no)
         obj2obj(nobj2obj).to_obj.obj_name=ToObjID
      end if
      if (print_level .ge. 3) &
        write(unit_screen,'(i5,a)') nobj2obj, &
              trim(obj2obj(nobj2obj).name)
 630  format(/a,i5)
      return
end subroutine




