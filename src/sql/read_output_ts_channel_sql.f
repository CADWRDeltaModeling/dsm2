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

      subroutine load_channel_output_ts_sql(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
	use Groups, only: GROUP_ALL
      use io_units
      use logging
      use iopath_data
      use grid_data
      use envvar

      implicit none

      
c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number
      integer(SQLINTEGER_KIND):: PerOpLen, IntvlLen, NameLen
     &                           ,LocNameLen
     &                           ,SourceGroupLen, ParamLen, FileLen
      integer name_to_objno

c-----local variables
      integer UseObj
      integer*4
     &     ID                   ! transfer ID
     &     ,LocNum
     &     ,SubLoc
     &     ,ObjType
     &     ,itmp
     &     ,counter
     &     ,loccarr             ! locate string in char array function
     &     ,nenv                ! environment var replacement


      character
     &     FileName*128
     &     ,Name*32
     &     ,prevName*32
     &     ,Param*32
     &     ,PrevParam*32
     &     ,Interval*32
     &     ,PerOp*8
     &     ,LocName*32
     &     ,ctmp*200
     &     ,SourceGroup*32
     &     ,PrevSourceGroup*32

      integer ext2int
      integer get_objnumber
      external ext2int, get_objnumber

c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 



c-----Execute SQL statement

      StmtStr="SELECT out_id, name, channel, " //
     &     "distance, used, variable_name, time_interval, " //
     &     "period_op, source_group, output_file " //
     &     "FROM (output_time_series_channel INNER JOIN model_component ON " //
     &     "output_time_series_channel.layer_id = model_component.component_id) "//     
     &     "WHERE model_id = ? " //
     &     "ORDER BY name, variable_name, time_interval, period_op, source_group, layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error, '(a,i5/)') 'Error in making channel Output TS SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         istat=0
         if (print_level .ge. 3)
     &        write(unit_screen, '(a)') 'Made Output TS SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Name,
     &     loc(NameLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, LocNum,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, SubLoc,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Param,
     &     loc(ParamLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Interval,
     &     loc(IntvlLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, PerOp,
     &     loc(PerOpLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, SourceGroup,
     &     loc(SourceGroupLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, FileName,
     &     loc(FileLen), iRet)

      
      ObjType = obj_channel
      if (print_level .ge. 3) write(unit_screen, '(a)') 'Made Output TS bind request'

c-----Loop to fetch records, one at a time
      counter=1

      PrevSourceGroup=miss_val_c
      PrevName=miss_val_c
      PrevParam=miss_val_c

      do while (.true.)

c--------Fetch a record from the result set

         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
         if (iRet .ne. SQL_SUCCESS) then
            write(unit_error, 625) counter
 625        format(/'Invalid or Null data for output TS record ',i3)
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
            istat=-1
            return
         endif

c--------clean up char variables, replace environment variables
         Name=Name(1:namelen)
         nenv=replace_envvars(Name,ctmp)
         Name=ctmp
         call locase(Name)

         Param=Param(1:ParamLen)
         nenv=replace_envvars(Param,ctmp)
         Param=ctmp
         call locase(Param)
         Interval=Interval(1:IntvlLen)
         nenv=replace_envvars(Interval,ctmp)
         Interval=ctmp
         call locase(Interval)
         PerOp=PerOp(1:PerOpLen)
         nenv=replace_envvars(PerOp,ctmp)
         PerOp=ctmp
         call locase(PerOp)

         SourceGroup=SourceGroup(1:SourceGroupLen)
	   call locase(SourceGroup)

         FileName=FileName(1:FileLen) ! preserve case for filename
         nenv=replace_envvars(FileName,ctmp)
         if (len_trim(ctmp) .eq. 0) then
            write(unit_error, '(a)')'File name evaluated to blank string: ',FileName
            istat=-3
            return
         end if

         FileName=ctmp


c--------use only the last version of a path, and skip
c--------if the path is marked as not-use
         if ( .not.(
     &        Name .eq. PrevName 
     &        .and. Param .eq. PrevParam
     &        .and. PrevSourceGroup .eq. SourceGroup
     &        ) .and.
     &        UseObj) then
            call process_output_channel(Name,
     &                                  LocNum,
     &                                  SubLoc,
     &                                  Param,
     &                                  Interval,
     &                                  PerOp,
     &                                  SourceGroup,
     &                                  Filename)
         endif
 200     continue
         counter=counter+1
         prevName=Name
         PrevSourceGroup=SourceGroup
         PrevParam=Param	
      enddo

      if (print_level .ge. 2)
     &     write(unit_screen, '(a,i5/)') 'Read in all Output TS data',noutpaths

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error, '(a,i5//)') 'Error in unbinding Output TS SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen, '(a//)') 'Unbound Output TS SQL'
      endif


      istat=noutpaths


      return
      end subroutine