      subroutine load_input_ts_sql(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use Gates,only:GateArray,deviceIndex
     &     ,WEIR,PIPE,GATE_OPEN,NO_GATE_CONTROL
      use IO_Units
      implicit none

      include '../fixed/common.f'

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
     &     ,LocNameLen
     &     ,SubLocLen
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
     &     ,nenv,repl_envvars   ! environment var replacement
     &     ,name_to_objno       ! function to get object number
     &     ,gateNo,devNo,devType

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
     &     ,SubLoc*32           ! Object-dependent sublocation (gate device, reservoir node connect..)
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

      StmtStr="SELECT Input_Series_ID,Used,Input_Name,Object_Type_ID, "//
     &     "Loc_Name,Loc_Num,Sub_Loc,Path,Variable_Name, "//
     &     "Sign,Fillin,Role_Name,Input_File " //
     &     "FROM DSM2_Input_Time_Series " //
     &     "WHERE Model_ID = ? " //
     &     "ORDER BY Input_Name, Role_ID, Variable_Name, Layer DESC;"

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
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Name,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ObjTypeID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, LocName,
     &     loc(LocNameLen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, LocNum,
     &     f90SQL_NULL_PTR, iRet)

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

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made Input TS bind request'
c-----Loop to fetch records, one at a time
      ninpaths=0
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
         nenv=repl_envvars(Name,ctmp)
         call locase(ctmp)
         Name=ctmp
         call locase(Name)

         Param=Param(1:ParamLen)
         nenv=repl_envvars(Param,ctmp)
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
            nenv=repl_envvars(InPath,ctmp)
            InPath=ctmp
            call locase(InPath)

            LocName=locName(1:locnamelen)
            nenv=repl_envvars(LocName,ctmp)
            LocName=ctmp
            call locase(LocName)

            SubLoc=SubLoc(1:subloclen)
            nenv=repl_envvars(SubLoc,ctmp)
            SubLoc=ctmp
            call locase(SubLoc)

            RoleName=RoleName(1:RoleNameLen)
            call locase(RoleName)

            FileName=FileName(1:filelen) ! preserve case for filename
            nenv=repl_envvars(FileName,ctmp)
            FileName=ctmp

            pathinput(ninpaths).name=Name
            pathinput(ninpaths).use=.true.
            pathinput(ninpaths).object_name=LocName
            pathinput(ninpaths).object=ObjTypeID

            if (SignLen .gt. 0) then
               if (sign .eq. -1) then
                  pathinput(ninpaths).sign = '-'
               elseif (sign .eq. 1) then
                  pathinput(ninpaths).sign = '+'
               endif
            else
               sign = ' '
            end if

c-----------find object number given external object number
            if (ObjTypeID .eq. OBJ_NODE)then
               pathinput(ninpaths).object_no=ext2intnode(LocNum)
               pathinput(ninpaths).object_name=LocName
            else if(ObjTypeID .eq. OBJ_RESERVOIR .or.
     &              ObjTypeID .eq. OBJ_GATE .OR. 
     &              ObjTypeID .eq. OBJ_OBJ2OBJ)then
               pathinput(ninpaths).object_name=trim(LocName)
               pathinput(ninpaths).object_no=name_to_objno(ObjTypeID,LocName)
               if (pathinput(ninpaths).object_no .eq.miss_val_i )then
                  write(unit_error,'(a,a)')
     &                 'Time Series Input: ',trim(pathinput(ninpaths).name),
     &                 ' attached to unrecognized object: ',LocName
                  istat=-3
                  return
               end if
            else if(ObjTypeID .eq. OBJ_OPRULE)then
                                ! there aren't really object numbers, but this loads
                                ! defaults and prevents an error
               pathinput(ninpaths).object_name=LocName
               pathinput(ninpaths).object_no = miss_val_i

            else if(ObjTypeID .eq. OBJ_CLIMATE)then
                                ! there aren't really object numbers, but this loads
                                ! defaults and prevents an error
               pathinput(ninpaths).object_name='global'
               pathinput(ninpaths).object_no = miss_val_i  ! this could be an index into climate variables.
            else
               write(unit_error,'(a,i4,a)')'Unrecogized object type (',
     &              ObjTypeID, ')in input path named: ',LocName
               istat=-3
               return
            end if
            if (FileName(:8) .eq. 'constant' .or.
     &             FileName(:8) .eq. 'CONSTANT') then
               read(InPath,'(1f10.0)') ftmp
               pathinput(ninpaths).constant_value=ftmp
               pathinput(ninpaths).c_part=Param
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

               pathinput(ninpaths).a_part=ca
               pathinput(ninpaths).b_part=cb
               pathinput(ninpaths).c_part=Param
               call split_epart(ce,itmp,ctmp)
               if (itmp .ne. miss_val_i) then ! valid interval, parse it
                  pathinput(ninpaths).e_part=ce(1:15)
                  pathinput(ninpaths).no_intervals=itmp
                  pathinput(ninpaths).interval=ctmp
               else
                  write(unit_error,610)
     &                 'Input TS: Unknown input E part or interval: ' // ce
                  write(unit_error,'(a)') 'Path: ' // trim(InPath)
                  istat=-1
                  return
               endif
               if (cf(1:4) .eq. 'none') then
                  pathinput(ninpaths).f_part=' '
               else
                  pathinput(ninpaths).f_part=cf
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
     &           RoleName .eq. "transfer")then
               pathinput(ninpaths).data_type=obj_obj2obj
            else if (RoleName .eq. "inflow")then
               pathinput(ninpaths).data_type=obj_boundary_flow
            else if (RoleName .eq. "source-sink")then
               pathinput(ninpaths).data_type=obj_source_sink
            else if (RoleName .eq. "stage")then
               pathinput(ninpaths).data_type=obj_stage
            else if (RoleName .eq. "gate-op")then
               pathinput(ninpaths).data_type=obj_gate
            else if (RoleName .eq. "climate")then
               pathinput(ninpaths).data_type=obj_climate
            end if

            if (pathinput(ninpaths).data_type .eq. obj_gate) then
               if (subloclen.le. 0 ) then
                  if ( param(1:7) .eq. 'install')then
                     pathinput(ninpaths).gate_param=gate_install
                  else          ! device wasn't specified and not installation
                     write(unit_error,*)
     &                    'Input TS: For gate: ' // trim(LocName) //
     &                    ' a time series was entered with no device name.'
                     write(unit_error,*)
     &                    'This is only allowed if the time series variable is "install"'
                     istat=-3
                     return
                  end if
               else
                  gateNo=pathinput(ninpaths).object_no
                  devNo=deviceIndex(gateArray(gateNo),subLoc)
                  devType=gateArray(gateNo).Devices(devNo).structureType
                  if( gateNo .ne. miss_val_i .and. devNo .ne. miss_val_i)then
                     if( devType .eq. miss_val_i .and. param(1:3).eq.'pos')then
                        write(unit_error,*)  
     &                       "Time series for device position in a device with no flow control. Gate: "
     &                       // trim(LocName) //
     &                       ", Device: "// trim(subloc) // ", Parameter: " // param
                        istat=-3
                        return
                     end if
                     
                     if (param(1:3) .eq. 'pos' .and. ParamLen .eq. 3)then
                        write(unit_error,*)
     &                       "Time series variable 'pos' has been deprecated. " //
     &                       "Please use 'op' or 'position' and note 'position'" //
     &                       "is different from 'pos' in previous versions."
                        istat=-3
                        return
                     end if
                     if (param(1:8) .eq. 'position') then
                        devType=gateArray(gateNo).Devices(devNo).structureType
                        if( devType .eq. miss_val_i .or.
     &                     devType .eq. NO_GATE_CONTROL)then
                           write(unit_error,*)  
     &                          "Time series for device position in a device " //
     &                          "with no flow control. Gate: "
     &                          // trim(LocName) //
     &                          ", Device: "// trim(subloc) // ", Parameter: " // 
     &                          param
                           istat=-3
                           return
                        end if
                        pathinput(ninpaths).gate_param = gate_position
                        pathinput(ninpaths).locnum=devNo
                        
                        call datasource_from_path(
     &                       gateArray(gateNo).Devices(
     &                       devNo).pos_datasource,
     &                       ninpaths,pathinput(ninpaths))
                        gateArray(pathinput(ninpaths).object_no).Devices(
     &                       devNo).position=gateArray(gateNo).Devices(
     &                       devNo).pos_datasource.value
                        if(pathinput(ninpaths).constant_value .ne. miss_val_r)then
                           gateArray(gateNo).Devices(devNo).position
     &                          =pathinput(ninpaths).constant_value
                        end if
                     else if (param(1:2) .eq. 'op') then
                        if (param(4:5) .eq. 'to' .and. 
     &                       param(7:10) .eq. 'node')then
                           pathinput(ninpaths).gate_param = gate_op_to_node
                        else if (param(4:7) .eq. 'from' .and. 
     &                          param(9:12) .eq. 'node')then 
                           pathinput(ninpaths).gate_param = gate_op_from_node
                        else
                           pathinput(ninpaths).gate_param = gate_operation
                        end if

                        
                        if( pathinput(ninpaths).gate_param .eq. gate_operation 
     &                       .or. 
     &                       pathinput(ninpaths).gate_param .eq. gate_op_to_node) 
     &                       then ! bidirectional or to_node
                           call datasource_from_path(
     &                          gateArray(gateNo).Devices(
     &                          devNo).op_to_node_datasource,
     &                          ninpaths,pathinput(ninpaths))
                           gateArray(pathinput(ninpaths).object_no).Devices(
     &                          devNo).opCoefToNode=fetch_data(
     &                            gateArray(gateNo).Devices(
     &                          devNo).op_to_node_datasource)

                        endif

                        if( pathinput(ninpaths).gate_param .eq. gate_operation 
     &                       .or.
     &                       pathinput(ninpaths).gate_param .eq. gate_op_from_node) 
     &                       then !bidirectional or from_node
                           call datasource_from_path(
     &                          gateArray(gateNo).Devices(
     &                          devNo).op_from_node_datasource,
     &                          ninpaths,pathinput(ninpaths))
                           gateArray(pathinput(ninpaths).object_no).Devices(
     &                          devNo).opCoefFromNode=fetch_data(
     &                          gateArray(
     &                          pathinput(ninpaths).object_no
     &                          ).Devices(devNo).op_from_node_datasource)

                        end if
c     !fixme: what about constant value stuff??
c     gateArray(gateNo).Devices(devNo).height
c     &                   =gateArray(gateNo).Devices(
c     &                    devNo).pos_datasource.value
                     else
                        write(unit_error,*)  
     &                       "Unknown time series parameter. Gate: " 
     &                       // trim(LocName) //
     &                       ", Device: "// trim(subloc) //
     &                       ", Parameter: " // param
                        istat=-3
                        return
                     end if 
                  else
                     write(unit_error,'(a/a/a/a/)')
     &                    'Input TS: Input attached to unrecognized device',
     &                    '  Gate: ' // trim(LocName),'  Device: ' // trim(subLoc),
     &                    '  Path: ' // trim(pathinput(ninpaths).path)
                  end if        ! device was recognized
               end if           ! subloclen >0 (ie, both gate and device given)
            end if              ! input is a gate
            if ( pathinput(ninpaths).data_type .eq. obj_stage) then
               if (pathinput(ninpaths).object .eq. obj_node) then
                  node_geom(pathinput(ninpaths).object_no).boundary_type=stage_boundary
               else
                  write(unit_error, '(a)')
     &                 'Input TS: Stage boundary must be at a node.'
               end if
            end if

            pathinput(ninpaths).priority=0
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
      end


