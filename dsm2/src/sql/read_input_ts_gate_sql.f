      subroutine load_gate_ts_sql(StmtHndl, ModelID, istat)

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

            pathinput(ninpaths).name=Name
            pathinput(ninpaths).useobj=.true.
            pathinput(ninpaths).obj_name=LocName
            pathinput(ninpaths).obj_type=ObjTypeID
            sign = 0
            pathinput(ninpaths).sign = sign

            ObjTypeID = obj_gate ! todo: check this?
            pathinput(ninpaths).obj_name=trim(LocName)
            pathinput(ninpaths).obj_no=name_to_objno(ObjTypeID,LocName)
            if (pathinput(ninpaths).obj_no .eq.miss_val_i )then
               write(unit_error,'(a,a)')
     &            'Gate Time Series Input: ',trim(pathinput(ninpaths).name),
     &            ' attached to unrecognized object: ',LocName
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
            !fixme: the next line should probably be based on RoleName
c-----------set data type fixme:groups is this right
            pathinput(ninpaths).data_type=obj_gate

            if (pathinput(ninpaths).data_type .eq. obj_gate) then
               if (subloclen.le. 0 ) then
                  if ( param(1:7) .eq. 'install')then
                     pathinput(ninpaths).gate_param=gate_install
                     pathinput(ninpaths).locnum=devNo
                     gatendx=pathinput(ninpaths).obj_no
                     call datasource_from_path(
     &                          gateArray(gatendx).install_datasource,
     &                          ninpaths,
     &                          pathinput(ninpaths))
                     fetcheddata=fetch_data(
     &                            gateArray(gatendx).install_datasource)
                     free = fetcheddata .eq. 0.
                     call setFree(gateArray(gatendx), free)
                  else   ! device wasn't specified and not installation
                     write(unit_error,*)
     &                    'Input TS: For gate: ' // trim(LocName) //
     &                    ' a time series was entered with no device name.'
                     write(unit_error,*)
     &                    'This is only allowed if the time series variable is "install"'
                     istat=-3
                     return
                  end if
               else
                  gateNo=pathinput(ninpaths).obj_no
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
     &                    "Time series variable 'pos' has been deprecated. "
                        istat=-3
                        return
                     end if
                     if (param(1:8) .eq. 'position') then
c                        write(unit_error,*)
c     &                  "Time series variable 'position' has been deprecated. " //
c     &                  "Please use 'height','elev' or 'width' to manipulate the" //
c     &                  "position directly. Use 'height' for a radial " //
c     &                  "gate and elev for a bottom-operated gate. "//
c     &                  "The last version of HYDRO that accepts position "//
c     &                  "without a warning was 7.8."   
                     
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
                        gateArray(pathinput(ninpaths).obj_no).Devices(
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
                           gateArray(pathinput(ninpaths).obj_no).Devices(
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
                           gateArray(pathinput(ninpaths).obj_no).Devices(
     &                          devNo).opCoefFromNode=fetch_data(
     &                          gateArray(
     &                          pathinput(ninpaths).obj_no
     &                          ).Devices(devNo).op_from_node_datasource)

                        end if
                     else if (param(1:5) .eq. 'width') then
                        pathinput(ninpaths).gate_param = gate_width
                        pathinput(ninpaths).locnum=devNo
                        call datasource_from_path(
     &                          gateArray(gateNo).Devices(
     &                          devNo).width_datasource,
     &                          ninpaths,pathinput(ninpaths))
                           gateArray(pathinput(ninpaths).obj_no).Devices(
     &                          devNo).maxWidth=fetch_data(
     &                          gateArray(
     &                          pathinput(ninpaths).obj_no
     &                          ).Devices(devNo).width_datasource)
                     else if (param(1:6) .eq. 'height') then
                        pathinput(ninpaths).gate_param = gate_height
                        pathinput(ninpaths).locnum=devNo
                        call datasource_from_path(
     &                          gateArray(gateNo).Devices(
     &                          devNo).height_datasource,
     &                          ninpaths,pathinput(ninpaths))
                           gateArray(pathinput(ninpaths).obj_no).Devices(
     &                          devNo).height=fetch_data(
     &                          gateArray(
     &                          pathinput(ninpaths).obj_no
     &                          ).Devices(devNo).height_datasource)
                     else if (param(1:4) .eq. 'elev') then
                        pathinput(ninpaths).gate_param = gate_elev
                        pathinput(ninpaths).locnum=devNo
                        call datasource_from_path(
     &                          gateArray(gateNo).Devices(
     &                          devNo).elev_datasource,
     &                          ninpaths,pathinput(ninpaths))
                           gateArray(pathinput(ninpaths).obj_no).Devices(
     &                          devNo).baseElev=fetch_data(
     &                          gateArray(
     &                          pathinput(ninpaths).obj_no
     &                          ).Devices(devNo).elev_datasource)

                     else
                        write(unit_error,*)  
     &                       "Unknown time series parameter. Gate: " 
     &                       // trim(LocName) //
     &                       ", Device: "// trim(subloc) //
     &                       ", Parameter: " // param
                        istat=-3
                        return
                     end if  ! parameter
                  else
                     write(unit_error,'(a/a/a/a/)')
     &                    'Input TS: Input attached to unrecognized device',
     &                    '  Gate: ' // trim(LocName),'  Device: ' // trim(subLoc),
     &                    '  Path: ' // trim(pathinput(ninpaths).path)
                  end if        ! device was recognized
               end if           ! subloclen >0 (ie, both gate and device given)
            end if              ! input is a gate
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