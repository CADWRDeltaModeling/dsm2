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

      subroutine set_database_name(name)
      use dsm2_database
      implicit none
      character(LEN=32) name
  	database_name=trim(name)
      ServerName=trim(name)
      end subroutine

      subroutine set_model_name(name)
      use dsm2_database
      implicit none
      character(LEN=32) name
      model_name=trim(name)
      end subroutine
          

      subroutine read_sql(istat)
      use dsm2_database
      use groups, only: ConvertGroupPatternsToMembers
      use runtime_data
      use iopath_data
      implicit none

      integer :: istat          ! status
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) :: ModelID ! ModelID for run
      integer(DBASE_ID_KIND) :: HydroID ! Corresponding Hydro model id 
                                             ! (may be same as ModelID)
      call get_statement_handle(StmtHndl, istat)
      call get_model_id(ModelID,istat)
      call get_hydro_model_id(HydroID, istat)
      if (istat .lt. 0) goto 901
c-----Convert model name to model id
      
c-----Load data
      ! todo: disabled
      ! call load_scalar_SQL(StmtHndl, ModelID, istat)
      if (istat .lt. 0) goto 901
      !call load_channels_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901
      !call load_channel_xsects_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901
      !call load_xsect_layers_SQL(StmtHndl, istat)
      if (istat .lt. 0) goto 901

      ! todo: disabled
      !call load_reservoirs_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901
      ! todo: disabled
      !call load_reservoir_connections_SQL(StmtHndl, istat)
      if (istat .lt. 0) goto 901
      !call load_gates_SQL(StmtHndl, HydroID, istat)
      !if (istat .lt. 0) goto 901
      !if (istat .gt. 0) then    ! gates are included
      !   call load_gate_devices_SQL(StmtHndl, istat)
      !   if (istat .lt. 0) goto 901
      !end if
      ! todo: disabled
      !call load_transfers_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901

      if( dsm2_name .eq. 'Hydro' .or. dsm2_name .eq. 'Qual')then
          call read_input_ts_node_sql(StmtHndl, ModelID, istat)
          !if (istat .lt. 0) goto 901
          call read_input_ts_reservoir_sql(StmtHndl, ModelID, istat) 
          !if (istat .lt. 0) goto 901
      end if
      
      if( dsm2_name .eq. 'Hydro')then
          call read_input_ts_gate_sql(StmtHndl, ModelID, istat)
          !if (istat .lt. 0) goto 901
          !call read_input_ts_transfer_sql(StmtHndl, ModelID, istat) 
          !if (istat .lt. 0) goto 901          
          !todo: disabled
          !call read_input_ts_oprule_sql(StmtHndl, ModelID, istat) 
      end if
      if( dsm2_name .eq. 'Qual')then
          !call read_input_ts_climate_sql(StmtHndl, ModelID, istat) 
          !if (istat .lt. 0) goto 901
      end if

      !todo: disable
      !call load_groups_sql(StmtHndl, ModelID,istat)
      if (istat .lt. 0) goto 901

      if( dsm2_name .eq. 'Qual')
     &    call load_rate_coeffs_SQL(StmtHndl, ModelID,istat)
      if (istat .lt. 0) goto 901
      
      !todo: disable
      if( .false.)then 
      if( dsm2_name .eq. 'Hydro' .or. dsm2_name .eq. 'Qual')then
         !call load_output_ts_SQL(StmtHndl, ModelID, istat)
	   !if (istat .lt. 0) goto 901
	   !call load_channel_output_ts_SQL(StmtHndl, ModelID, istat)
	   !if (istat .lt. 0) goto 901
         !call load_node_output_ts_SQL(StmtHndl, ModelID, istat)
	   !if (istat .lt. 0) goto 901	   
	   !call load_reservoir_output_ts_SQL(StmtHndl, ModelID, istat)
	   !if (istat .lt. 0) goto 901
	   !call load_gate_output_ts_SQL(StmtHndl, ModelID, istat)
	   !if (istat .lt. 0) goto 901	   	   
	end if
      end if

      if ( (dsm2_name .eq. 'Hydro') .and.
     &     (.not. (io_files(hydro,io_restart,io_read).use)) ) then
                                ! No restart. Read from database.
         call load_channel_initcond(StmtHndl, ModelID, 'stage', istat)
         call load_channel_initcond(StmtHndl, ModelID, 'flow', istat)
         call load_reservoir_initcond(StmtHndl, ModelID, 'stage', istat)
         if (istat .lt. 0) goto 901

      end if
      istat=0

 901  continue
      

      return
      end

      subroutine load_scalar_SQL(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      use logging
      Use PhysicalConstants
      use dsm2_database
      use constants
      use runtime_data
      use iopath_data
      use common_qual
      use common_ptm
      use envvar

      implicit none

      include '../hydrolib/network.inc'
      include '../hydrolib/netcntrl.inc'
      include '../hydrolib/chconnec.inc'



c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables
      integer
     &     counter
     &     ,itmp
     &     ,err_status


      character
     &     Param*32             ! parameter
     &     ,prev_param*32       ! previous parameter name
     &     ,Value*32            ! parameter value
     &     ,ctmp*20


      integer(SQLINTEGER_KIND):: scalarlen, vallen

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)

c-----defaults

 610  format(/'Unrecognized Parameter in SCALAR section:'
     &     /a,a)
 615  format(/'Theta must be between 0.5 and 1.0:',f5.2)
 620  format(/a, ' ',a/a)
 630  format(/a)

c-----Execute SQL statement
      StmtStr="SELECT Model_Parameter_Description.Name,Parameter_Value " //
     &     "FROM Model_Parameter_Values,Model_Parameter_Description,Model_Component " //
     &     "WHERE Model_Parameter_Values.Model_Parameter_ID = Model_Parameter_Description.Model_Parameter_ID " //
     &     "AND Layer_ID = Model_Component.Component_ID " //
     &     "AND Model_Component.Component_Type = 'param' " //
     &     "AND Model_Component.Model_ID=? " //
     &     "ORDER BY Name, Layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error, '(a,i5/)') 'Error in making scalar SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen, '(a)') 'Made scalar SQL request'
      endif

c-----Bind variables to columns in result set      !fixme: layer needs to be bound
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Param,
     &     loc(scalarlen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Value,
     &     loc(vallen), iRet)

      if (print_level .ge. 3) write(unit_screen, '(a)') 'Made scalar bind request'
c-----Loop to fetch records, one at a time
      prev_param=miss_val_c
      counter=0
      do while (.true.)

c--------Fetch a record from the result set
         Param=' '
         Value=' '
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit

         Param=Param(1:scalarlen)
         call locase(Param)
         err_status=replace_envvars(Param,ctmp)
         call locase(ctmp)
         Param=ctmp
         

c--------use only the last version of a parameter
         if (Param .ne. prev_param) then
           Value=Value(1:vallen)
           call locase(Value)
           err_status=replace_envvars(Value,ctmp)
           Value=ctmp

           call process_scalar(Param, Value)   
                  
         endif 
         
         if (print_level .ge. 3)
     &        write(unit_screen, '(a,t20,a)')
     &        trim(Param),trim(Value)

         prev_param=Param
      
      enddo

      if (print_level .ge. 2)
     &     write(unit_screen, '(a,i5/)') 'Read in all scalar data', counter

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error, '(a,i5//)') 'Error in unbinding scalar SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen, '(a//)') 'Unbound scalar SQL'
      endif

      return

c-----char-to-value conversion errors

 810  continue
      write(unit_error, 620) 'Conversion error on field ',
     &     trim(Param), trim(Value)

      istat=-2

 900  continue                  ! fatal error

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error, '(a,i5//)') 'error in unbinding scalar SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen, '(a//)') 'Unbound scalar SQL'
      endif

      if (counter .eq. 0) then
         write(unit_error,*)"No parameters found for model "// model_name //
     &        ". Parameters are the first thing the model " //
     &        "looks for -- so this could be a parameter " //
     &        "or a more basic problem with the simulation or model. Unless you " //
     &        "are doing something very unusual, any checking errors that follow are "//
     &        "probably bogus."
      end if 

      return
      endsubroutine

      subroutine process_scalar_SQL(Param, Value, istat)

      Use PhysicalConstants
      use IO_Units
      use logging
      use constants
      use runtime_data
      use iopath_data
      use common_qual
      use common_ptm
      use envvar
      use dsm2_database

      implicit none

      include '../hydrolib/network.inc'
      include '../hydrolib/netcntrl.inc'
      include '../hydrolib/chconnec.inc'

      integer, intent(inout) :: istat !status
      integer                  :: itmp
      character*32 field_names(1) !kc to match text input
      character, intent(in)    :: Value*20 ! parameter value
      character, intent(in)    :: Param*20 ! parameter
      character                :: ctmp*20  ! local
      !integer, intent(out)     :: err_status  ! to follow the "GOTO"
                                              ! statmemnet in the old code. 
      !err_status = 0



        !include '../test/include_merged.f'
        call process_scalar(Param, Value, istat)

      
  
      endsubroutine
               
      
      integer*4 function sqlTime2JulMin(time)
c-----convert sql timestamp structure into a julian minute
      use f90SQLStructures
      use grid_data
      implicit none
      type(TIMESTAMP_STRUCT) :: time

      integer*4
     &     julday               ! days since 31dec1899
     &     ,minute              ! minutes past midnight
     &     ,iymdjl              ! DSS function (m-d-y to julian)

      if ( time%year .eq. 0 .and. time%month .eq. 0) then
                                ! return smallest integer (minute is just an arbitrary integer used as a ref. in HUGE)
         sqlTime2JulMin=-HUGE(minute)
      else
         julday = iymdjl(Int2(time%year),Int2(time%month),Int2(time%day))
         minute = time%hour*60 + time%minute
         sqlTime2JulMin=julday*24*60 + minute
      end if
      return
      end

      integer*4 function get_objnumber(ObjType, dbID)
c-----get object number, given object type and ID from RDBMS.
c     The ID is an autonumbered key field used
c     by the database. It is not the external map number and it
c     is not the internal model number
      use IO_Units
      use gates, only: nGate, gateArray
      use groups, only: nGroup, groupArray
      use grid_data

c-----arguments
      integer*4
     &     ObjType              ! object type
     &     ,dbID                ! ID from RDBMS

c-----local variables
      integer i

      get_objnumber=miss_val_i
      if (ObjType .eq. obj_node) then
         get_objnumber=dbID
      else if (ObjType .eq. obj_reservoir) then
         do i=1, nreser
            if (res_geom(i).ID .eq. dbID) then
               get_objnumber=i
               exit
            endif
         enddo
      else if (ObjType .eq. obj_channel) then
         do i=1,nchans
            if (chan_geom(i).ID .eq. dbID) then
               get_objnumber=i
               exit
            endif
         enddo
      else if (ObjType .eq. obj_gate) then
         do i = 1,nGate
            if (gateArray(i).ID .eq. dbID) then
               get_objnumber = i
               exit
            end if
         end do
      else if (ObjType .eq. obj_obj2obj) then !fixme: number convention
         do i= 1,max_obj2obj
            if (obj2obj(i).ID .eq. dbID) then
               get_objnumber = i
               exit
            end if
         end do
      else if (ObjType .eq. obj_group) then
         do i= 1,nGroup
            if (groupArray(i).ID .eq. dbID) then
               get_objnumber = i
               exit
            end if
         end do

      else

         write(unit_error,610) ObjType, dbID
 610     format(/'Error in get_objnumber: unrecognized object type:',i3,
     &        '  ID:',i6)
      endif
      return
      end function






      subroutine ShowDiags(HndlType,Hndl)

c-----This subroutine prints error diagnostics

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      implicit none


      integer(SQLHANDLE_KIND)::Hndl
      integer(SQLSMALLINT_KIND)::HndlType

      character(len=6):: SqlState
      character(len= SQL_MAX_MESSAGE_LENGTH)::Msg
      integer(SQLINTEGER_KIND)::NativeError
      integer(SQLSMALLINT_KIND):: iDiag, MsgLen
      integer(SQLRETURN_KIND):: DiagRet

      iDiag = 1
      DiagRet=SQL_SUCCESS
      do while (DiagRet .eq. SQL_SUCCESS)
         call f90SQLGetDiagRec(HndlType, Hndl, iDiag, SqlState,
     &        NativeError, Msg, MsgLen, DiagRet)
         write(unit_error, '(a5,1x,i8,1x,a)')
     &        SqlState, NativeError, Msg(1:MsgLen)
         iDiag=iDiag+1
      enddo

      return
      end subroutine
