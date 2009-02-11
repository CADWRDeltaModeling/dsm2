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

      subroutine load_input_ts_node_SQL(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use Gates
      use io_units
      use iopath_data
      use logging
      use grid_data
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
     &     SignLen
     &     ,NameLen
     &     ,FileLen
     &     ,PathLen
     &     ,ParamLen
     &     ,RoleNameLen

c-----local variables
      integer UseObj            ! indicates whether object is used or erased

      integer*4
     &     ID                   ! transfer ID
     &     ,Fillin              ! code for fill in type (last, none, linear)
     &     ,Sign                ! sign restriction on input
     &     ,LocNum              ! object map id of input data if applicable (channel,node)
     &     ,ObjTypeID           ! object type of input data (node, gate...)
     &     ,npath,na,nb,nc,nd,ne,nf
     &     ,itmp
     &     ,counter
     &     ,loccarr             ! locate string in char array function
     &     ,nenv                ! environment var replacement

      integer data_types
      external data_types

      real*8 ftmp
      real*8, external :: fetch_data

      character
     &     InPath*80
     &     ,FileName*128
     &     ,Param*32
     &     ,PrevParam*32
     &     ,LocName*32
     &     ,PrevName*32
     &     ,RoleName*32
     &     ,Name*64
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,ctmp*200

      integer ext2intnode

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)

c-----Execute SQL statement
c-----Execute SQL statement
            StmtStr="SELECT input_series_id,used, " //
     &     "input_time_series_node.name,node, " //
     &     "path,variable_name,sign,fillin, "//
     &     "input_role_description.name,input_file " //
     &     "FROM (input_time_series_node INNER JOIN model_component ON " //
     &     "input_time_series_node.layer_id = model_component.component_id) "//
     &     "INNER JOIN input_role_description "//
     &     "ON input_time_series_node.role_id = input_role_description.role_id "//
     &     "WHERE model_component.model_id = ? " //
     &     "AND model_component.component_type = 'input' " //
     &     "ORDER BY input_time_series_node.name, layer DESC;"


      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error, '(a,i5/)') 'Error making node Input TS SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen, '(a)') 'Made Input TS SQL request'
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

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, LocNum,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, InPath,
     &     loc(PathLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Param,
     &     loc(ParamLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, Sign,
     &     loc(SignLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, Fillin,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, RoleName,
     &     loc(RoleNameLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, FileName,
     &     loc(FileLen), iRet)

      ObjTypeID = obj_node
      if (print_level .ge. 3) write(unit_screen, '(a)') 'Made Input TS bind request'
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
         namelen=min(32,namelen)
         Name=Name(1:namelen)   ! preserve case for filename
         nenv=replace_envvars(Name,ctmp)
         call locase(ctmp)
         Name=ctmp
         call locase(Name)

         Param=Param(1:ParamLen)
         nenv=replace_envvars(Param,ctmp)
         Param=ctmp
         call locase(Param)

c--------use only the highest layer version of the input, and skip
c--------if marked as not-use
         if ( (.not.(Name .eq. PrevName .and. Param .eq. PrevParam))
     &        .and. UseObj) then
            ninpaths=ninpaths+1
            if (ninpaths .gt. max_inputpaths) then
               write(unit_error,630)
     &              'Too many input paths specified; max allowed is:'
     &              ,max_inputpaths
               istat=-1
               return
            endif

c-----------clean up character variables, replace environment variables
            InPath=InPath(1:PathLen)
            nenv=replace_envvars(InPath,ctmp)
            InPath=ctmp
            call locase(InPath)

            write(LocName, '(i)')LocNum

            RoleName=RoleName(1:RoleNameLen)
            call locase(RoleName)

            FileName=FileName(1:filelen) ! preserve case for filename
            nenv=replace_envvars(FileName,ctmp)
            FileName=ctmp

            pathinput(ninpaths).name=Name
            pathinput(ninpaths).useobj=.true.
            pathinput(ninpaths).obj_name=LocName
            pathinput(ninpaths).obj_type=ObjTypeID

            if (SignLen .gt. 0) then
               if (sign .eq. -1) then
                  pathinput(ninpaths).sign = -1
               elseif (sign .eq. 1) then
                  pathinput(ninpaths).sign = 1
               else
                  write(unit_error,*)"Incorrect sign for input time series"
                  istat=-3
                  return
               end if
            else
               sign = 0
            end if

c-----------find object number given external object number 
            pathinput(ninpaths).obj_no=ext2intnode(LocNum)
            pathinput(ninpaths).obj_name=LocName
            if (FileName(:8) .eq. 'constant' .or.
     &             FileName(:8) .eq. 'CONSTANT') then
               read(InPath, '(1f10.0)') ftmp
               pathinput(ninpaths).constant_value=ftmp
               pathinput(ninpaths).variable=Param
               pathinput(ninpaths).fillin=fill_last
            else
c--------------Break up the input pathname

               pathinput(ninpaths).path=trim(InPath)
               call chrlnb(InPath, npath)
               call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne,
     &              cf, nf, InPath, npath, istat)
               if (istat .lt. 0) then
                  write(unit_error, '(/a/a)')
     &                 'Input TS: Illegal pathname', InPath
                  istat = -1
                  return
               end if

               pathinput(ninpaths).variable=Param
               call split_epart(ce,itmp,ctmp)
               if (itmp .ne. miss_val_i) then ! valid interval, parse it
                  pathinput(ninpaths).no_intervals=itmp
                  pathinput(ninpaths).interval=ctmp
               else
                  write(unit_error,610)
     &                 'Input TS: Unknown input E part or interval: ' // ce
                  write(unit_error, '(a)') 'Path: ' // trim(InPath)
                  istat=-1
                  return
               endif
               pathinput(ninpaths).filename=FileName
c--------------accumulate unique dss input filenames
               itmp=loccarr(pathinput(ninpaths).filename,infilenames,
     &              max_dssinfiles, EXACT_MATCH)
               if (itmp .lt. 0) then
                  if (abs(itmp) .le. max_dssinfiles) then
                     infilenames(abs(itmp))=pathinput(ninpaths).filename
                     pathinput(ninpaths).ndx_file=abs(itmp)
                  else
                     write(unit_error,610)
     &                    'Maximum number of unique DSS input files exceeded'
                     istat=-3
                     return
                  endif
               else
                  pathinput(ninpaths).ndx_file=itmp
               endif
               pathinput(ninpaths).fillin=Fillin
            endif
                                !fixme: the next line should probably be based on RoleName
c-----------set data type fixme:groups is this right
            if (
     &           RoleName .eq. "inflow")then
               pathinput(ninpaths).data_type=obj_boundary_flow
            else if (RoleName .eq. "source-sink")then
               pathinput(ninpaths).data_type=obj_source_sink
            else if (RoleName .eq. "stage")then
               pathinput(ninpaths).data_type=obj_stage
            end if

            if ( pathinput(ninpaths).data_type .eq. obj_stage) then
               if (pathinput(ninpaths).obj_type .eq. obj_node) then
                  node_geom(pathinput(ninpaths).obj_no).boundary_type=stage_boundary
               else
                  write(unit_error, '(a)')
     &                 'Input TS: Stage boundary must be at a node.'
               end if
            end if

            if (print_level .ge. 3)then
               write(unit_screen, '(i4,1x,a32,1x,a24,a24)') ninpaths, Name,
     &              trim(InPath(:24)),
     &              trim(FileName(:24))
            end if
         end if                 !inputpath name not equals last name processed
         counter=counter+1
         prevName=Name
	   prevParam=Param

      end do

      if (print_level .ge. 2)
     &     write(unit_screen, '(a,i5/)') 'Read in all Input TS data',ninpaths

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error, '(a,i5//)') 'Error in unbinding Input TS SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen, '(a//)') 'Unbound Input TS SQL'
      endif

 610  format(/a)
 620  format(/a/a)
 630  format(/a,i5)

      return
      end subroutine