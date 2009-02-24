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

      subroutine load_groups_SQL(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use Groups, only: groupArray,ngroup,MAX_MEMBERS
      use io_units
      use logging
      use constants
      implicit none


c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables
      integer UseObj

      integer
     &     ID                   ! ID of group
     &     ,groupID
     &     ,counter

      character
     &     name*32,prev_name*32

      integer(SQLINTEGER_KIND):: namelen

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND),
     &     SQL_PARAM_INPUT, SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),
     &     int(0,SQLSMALLINT_KIND), ModelID, f90SQL_NULL_PTR, iRet)

c-----Execute SQL statement
c----- Change tablename from group to groups, Jon 04/04/06

      StmtStr="SELECT group_id,used,name "//
     &     "FROM groups,model_component " //
     &     "WHERE groups.layer_id = model_component.component_id "
     &     //
     &     "AND model_component.model_id = ? " //
     &     "AND model_component.component_type = 'group' " //
     &     "ORDER BY groups.name,layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making group description SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a)') 'Made group description SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Name,
     &     loc(namelen), iRet)

      if (print_level .ge. 3)
     &     write(unit_screen,'(a)') 'Made group description bind request'

c-----Loop to fetch records, one at a time
      counter=0
      prev_name=miss_val_c

      do while (.true.)
c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
         if (iRet.eq. -1) then
            write(unit_error,'(a,i5//)') 'Error in fetching group',iRet
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         end if

         name=name(:namelen)
         call locase(name)
         if (name .ne. prev_name .and. UseObj) then
            call process_group(name,id)
         end if
         prev_name=name
      enddo
      nGroup=counter

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i4/)') 'Read in all group data ', nGroup

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
            call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet) 

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding group description SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound groups SQL'
      endif

c-----load members
      do counter=1,nGroup
         groupid=groupArray(counter).id
         call load_groupmembers_sql(
     &        StmtHndl,ModelID,groupArray(counter).id, counter, istat)
	   if(istat .lt. 0)return
      end do
      istat=nGroup
      end subroutine
      
c===============================
      subroutine load_groupmembers_sql(StmtHndl, ModelID, GroupID,
     &     GroupNdx,istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use Groups, only: groupArray,ngroup,GroupMember
     &      ,GroupMemberPattern
     &     ,MAX_MEMBERS,MAX_GROUP_PATTERNS
      use io_units
      use logging
      use constants
      implicit none

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer groupID           ! id of group whose members are to be fetched
     &     ,groupNdx            ! index of group in groupArray
     &     ,istat               ! status
     &     ,ModelID              

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number


c-----local variables

      integer
     &     objType
     &     ,nmember
     &     ,n,i
     &     ,allocstat
     &     ,name_to_objno

      character*32
     &     predicate,pattern

      integer(SQLINTEGER_KIND)::
     &     objtypelen,patternlen,predicatelen

c-----Buffer for holding group members as they are processed
      Type(GroupMember), Dimension(MAX_MEMBERS) :: memberBuffer
      Type(GroupMemberPattern), Dimension(MAX_GROUP_PATTERNS) :: patternBuffer

c-----load members


      StmtStr="SELECT object_type_id, " //
     &     "identifier " //
     &     "FROM group_member "//
     &     "WHERE group_member.group_id = ? " //
     &     "ORDER BY object_type_id, identifier;"

      
c-----Prepare statement
      call f90SQLPrepare(StmtHndl, StmtStr, iRet)
c-----Bind the parameter representing group id
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND),
     &     SQL_PARAM_INPUT,SQL_F_SLONG, SQL_INTEGER,
     &     int(4,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND),
     &     groupID, f90SQL_NULL_PTR, iRet)

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, objType,
     &     loc(objtypelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, pattern,
     &     loc(patternlen), iRet)

      if (print_level .ge. 3)
     &     write(unit_screen,'(a)') 'Made group member bind request'

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error executing group member query',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      end if

c-----loop through members
      do while (.true.)
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA .or. iRet .eq. SQL_ERROR) exit
         call process_group_member(groupArray(GroupNdx).name, 
     &                             objtype, 
     &                             pattern(:patternlen))
      end do
	call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet)
      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)')
     &        'Error in querying or unbinding group member SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound group member SQL'
      endif
      
      istat=groupArray(GroupNdx).nsubgroups

      return
      end subroutine

