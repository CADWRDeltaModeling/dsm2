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

      subroutine read_input_ts_gate_sql(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use Gates
      use io_units
      use iopath_data
      use logging
      use envvar
      implicit none
      
c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character StmtStr*1000
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number
      integer(SQLINTEGER_KIND)::
     &     LocNameLen
     &     ,SubLocLen
     &     ,FileLen
     &     ,PathLen
     &     ,ParamLen

c-----local variables
      integer UseObj            ! indicates whether object is used or erased

      integer*4
     &     ID                   ! transfer ID
     &     ,Fillin              ! code for fill in type (last, none, linear)
     &     ,Sign                ! sign restriction on input
     &     ,ObjTypeID           ! object type of input data (node, gate...)
     &     ,npath,na,nb,nc,nd,ne,nf
     &     ,itmp
     &     ,counter
     &     ,loccarr             ! locate string in char array function
     &     ,nenv                ! environment var replacement
     &     ,name_to_objno       ! function to get object number
     &     ,gateNo,devNo,devType

      integer data_types
      external data_types

      real*8 ftmp
      integer :: gatendx
      real*8, external :: fetch_data
      real*8  :: fetcheddata
      logical free

      character
     &     InPath*80
     &     ,FileName*128
     &     ,Param*32
     &     ,PrevParam*32
     &     ,LocName*32
     &     ,SubLoc*32           ! Object-dependent sublocation (gate device, reservoir node connect..)
     &     ,PrevName*32
     &     ,Name*64
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,ctmp*200

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)

c-----Execute SQL statement
            StmtStr="SELECT input_series_id,used, gate, device,path," //
     &     "variable_name,fillin,input_file " //
     &     "FROM input_time_series_gate INNER JOIN model_component ON " //
     &     "input_time_series_gate.layer_id = model_component.component_id "//
     &     "WHERE model_component.model_id = ? " //
     &     "AND model_component.component_type = 'input' " //
     &     "ORDER BY gate,device,variable_name, layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making Input TS SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a)') 'Made Input TS SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, LocName,
     &     loc(LocNameLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, SubLoc,
     &     loc(SubLocLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, InPath,
     &     loc(PathLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Param,
     &     loc(ParamLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, Fillin,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, FileName,
     &     loc(FileLen), iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made Input TS bind request'
c-----Loop to fetch records, one at a time

      counter=1
      prevName=miss_val_c
	prevParam=miss_val_c
      istat=0

      do while (.true.)
c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
         if (iRet .ne. SQL_SUCCESS) then
            write(unit_error, 625) counter
 625        format(/'Invalid or Null data for input TS record ',i3)
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
            istat=-1
            return
         endif

c--------clean up name variable, replace environment variables
         ! Gate name
         locnamelen=min(32,locnamelen)
         locname=LocName(1:locnamelen)   ! preserve case for filename
         nenv=replace_envvars(LocName,ctmp)
         call locase(ctmp)
         LocName=ctmp
         call locase(LocName)         

         Subloclen=min(32,Subloclen)
         Subloc=Subloc(1:Subloclen)   ! preserve case for filename
         nenv=replace_envvars(Subloc,ctmp)
         call locase(ctmp)
         Subloc=ctmp
         call locase(Subloc)         

         write(Name,"(a23,a,a8)")trim(LocName), "-", trim(Subloc)

         Param=Param(1:ParamLen)
         nenv=replace_envvars(Param,ctmp)
         Param=ctmp
         call locase(Param)


         InPath=InPath(1:PathLen)
         nenv=replace_envvars(InPath,ctmp)
         InPath=ctmp
         call locase(InPath)

         LocName=locName(1:locnamelen)
         nenv=replace_envvars(LocName,ctmp)
         LocName=ctmp
         call locase(LocName)

         SubLoc=SubLoc(1:subloclen)
         nenv=replace_envvars(SubLoc,ctmp)
         SubLoc=ctmp
         call locase(SubLoc)

         FileName=FileName(1:filelen) ! preserve case for filename
         nenv=replace_envvars(FileName,ctmp)
         FileName=ctmp

c--------use only the highest layer version of the input, and skip
c--------if marked as not-use
         if ( (.not.(Name .eq. PrevName .and. Param .eq. PrevParam))
     &        .and. UseObj) then

            call process_input_gate(LocName,
     &                              SubLoc,
     &                              InPath,
     &                              Param,
     &                              Fillin,
     &                              Filename) 


         end if                 
         counter=counter+1
         prevName=Name
	   prevParam=Param

      end do

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all Gate TS data',ninpaths

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding Input TS SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a//)') 'Unbound Input TS SQL'
      endif

 610  format(/a)
 620  format(/a/a)
 630  format(/a,i5)

      return
      end