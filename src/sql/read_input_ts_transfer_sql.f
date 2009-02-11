      subroutine load_transfer_ts_sql(StmtHndl, ModelID, istat)

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
     &     NameLen
     &     ,FileLen
     &     ,PathLen
     &     ,ParamLen

c-----local variables
      integer UseObj            ! indicates whether object is used or erased

      integer*4
     &     ID                   ! transfer ID
     &     ,Fillin              ! code for fill in type (last, none, linear)
     &     ,ObjTypeID           ! object type of input data (node, gate...)
     &     ,npath,na,nb,nc,nd,ne,nf
     &     ,itmp
     &     ,counter
     &     ,loccarr             ! locate string in char array function
     &     ,nenv                ! environment var replacement
     &     ,name_to_objno       ! function to get object number

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
     &     ,Name*64
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,ctmp*200

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)

c-----Execute SQL statement
            StmtStr="SELECT input_series_id,used,transfer, " //
     &     "path,variable_name,fillin,input_file " //
     &     "FROM input_time_series_transfer INNER JOIN model_component ON " //
     &     "input_time_series_transfer.layer_id = model_component.component_id "//
     &     "WHERE model_component.model_id = ? " //
     &     "AND model_component.component_type = 'input' " //
     &     "ORDER BY transfer, layer DESC;"


      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error making transfer input TS SQL request',iRet
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
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Name,
     &     loc(namelen), iRet)

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

      ObjTypeID = obj_obj2obj
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
         namelen=min(32,namelen)
         Name=Name(1:namelen)   ! preserve case for filename
         nenv=replace_envvars(Name,ctmp)
         call locase(ctmp)
         Name=ctmp
         call locase(Name)
         LocName = trim(Name)  ! Special case for transfer


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

            FileName=FileName(1:filelen) ! preserve case for filename
            nenv=replace_envvars(FileName,ctmp)
            FileName=ctmp

            pathinput(ninpaths).name=Name
            pathinput(ninpaths).useobj=.true.
            pathinput(ninpaths).obj_name=LocName
            pathinput(ninpaths).obj_type=ObjTypeID

c-----------find object number given external object number
            pathinput(ninpaths).obj_name=trim(LocName)
            pathinput(ninpaths).obj_no=name_to_objno(ObjTypeID,LocName)
            if (pathinput(ninpaths).obj_no .eq.miss_val_i )then
               write(unit_error,'(a,a)')
     &              'Time Series Input: ',trim(pathinput(ninpaths).name),
     &              ' attached to unrecognized object: ',LocName
               istat=-3
               return
            end if

            if (FileName(:8) .eq. 'constant' .or.
     &             FileName(:8) .eq. 'CONSTANT') then
               read(InPath,'(1f10.0)') ftmp
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
                  write(unit_error,'(a)') 'Path: ' // trim(InPath)
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

c-----------set data type
            pathinput(ninpaths).data_type=obj_obj2obj
            if (print_level .ge. 3)then
               write(unit_screen,'(i4,1x,a32,1x,a24,a24)') ninpaths, Name,
     &              trim(InPath(:24)),
     &              trim(FileName(:24))
            end if
         end if                 !inputpath name not equals last name processed
         counter=counter+1
         prevName=Name
	   prevParam=Param

      end do

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all Input TS data',ninpaths

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
      end subroutine