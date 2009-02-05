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


      subroutine load_transfers_SQL(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      use DSM2_database
      use grid_data
      use logging
      use constants
      implicit none

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables
      integer UseObj

      integer
     &     ID                   ! transfer ID
     &     ,FromObjType
     &     ,ToObjType
     &     ,name_to_objno

      character
     &     TransName*32         ! transfer name
     &     ,PrevName*32
     &     ,FromObjId*32
     &     ,ToObjID*32

      integer(SQLINTEGER_KIND):: namelen,fromobjlen,toobjlen
      integer ext2intnode

c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 

c-----Execute SQL statement
      StmtStr="SELECT Transfer_ID, Transfer.used, Transfer.Name, " //
     &     "Transfer.From_Object_Identifier, Transfer.From_Object_Type, " //
     &     "To_Object_Identifier, To_Object_Type " //
     &     "FROM Transfer INNER JOIN Model_Component ON " //
     &     "Transfer.Layer_ID = Model_Component.Component_ID " //
     &     "WHERE Model_Component.Component_Type = 'grid' " //
     &     "AND Model_Component.Model_ID = ? " //
     &     "ORDER BY Transfer.Name, Model_Component.Layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making transfer SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a)') 'Made transfer SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, TransName,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, FromObjID,
     &     loc(fromobjlen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, FromObjType,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, ToObjID,
     &     loc(toobjlen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ToObjType,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made transfer bind request'
c-----Loop to fetch records, one at a time
      nobj2obj=0
      prevname=miss_val_c
      do while (.true.)

c--------Fetch a record from the result set

         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit

         TransName=TransName(1:namelen)
         call locase(TransName)

         FromObjID=FromObjID(1:fromobjlen)
         call locase(FromObjID)

         ToObjID=ToObjID(1:toobjlen)
         call locase(ToObjID)
         

c--------use only the last version of a transfer, and skip
c--------if the transfer is marked as not-use
         if (TransName .ne. prevName .and.
     &        UseObj) then
            nobj2obj=nobj2obj+1
            if (nobj2obj .gt. max_obj2obj) then
               write(unit_error,630)
     &              'Too many object connections specified; max allowed is:'
     &              ,max_obj2obj
               istat=-1
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
               obj2obj(nobj2obj).from_obj.obj_no=
     &           ext2intnode(obj2obj(nobj2obj).from_obj.obj_no)
               obj2obj(nobj2obj).from_obj.obj_name=FromObjID
            end if
            obj2obj(nobj2obj).to_obj.obj_type=ToObjType
            if (ToObjType .eq. obj_reservoir) then
               obj2obj(nobj2obj).to_obj.obj_name=ToObjID
               obj2obj(nobj2obj).to_obj.obj_no=name_to_objno(obj_reservoir,ToObjID)
            else if (ToObjType .eq. obj_node) then
               read(ToObjID,'(i10)')obj2obj(nobj2obj).to_obj.obj_no
               obj2obj(nobj2obj).to_obj.obj_no=
     &              ext2intnode(obj2obj(nobj2obj).to_obj.obj_no)
               obj2obj(nobj2obj).to_obj.obj_name=ToObjID
            end if
            if (print_level .ge. 3)
     &           write(unit_screen,'(i5,a)') nobj2obj,
     &           trim(obj2obj(nobj2obj).name)
         endif
         PrevName=TransName
      enddo

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all transfer data',nobj2obj

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding transfer SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound transfer SQL'
      endif

 630  format(/a,i5)

      return
      end
