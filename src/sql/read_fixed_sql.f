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
      call load_scalar_SQL(StmtHndl, ModelID, istat)
      if (istat .lt. 0) goto 901

      call load_channels_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901
      call load_channel_xsects_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901
      call load_xsect_layers_SQL(StmtHndl, istat)
      if (istat .lt. 0) goto 901

      call load_reservoirs_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901
      call load_reservoir_connections_SQL(StmtHndl, istat)
      if (istat .lt. 0) goto 901

      call load_gates_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901
      if (istat .gt. 0) then    ! gates are included
         call load_gate_devices_SQL(StmtHndl, istat)
         if (istat .lt. 0) goto 901
      end if

      call load_obj2obj_SQL(StmtHndl, HydroID, istat)
      if (istat .lt. 0) goto 901

      if( dsm2_name .eq. 'Hydro' .or. dsm2_name .eq. 'Qual')
     &      call load_input_ts_SQL(StmtHndl, ModelID, istat)
      if (istat .lt. 0) goto 901

      call load_groups_sql(StmtHndl, ModelID,istat)
      if (istat .lt. 0) goto 901

      if( dsm2_name .eq. 'Qual')
     &    call load_rate_coeffs_SQL(StmtHndl, ModelID,istat)
      if (istat .lt. 0) goto 901

	

      if( dsm2_name .eq. 'Hydro' .or. dsm2_name .eq. 'Qual')
     &   call load_output_ts_SQL(StmtHndl, ModelID, istat)
      
	if (istat .lt. 0) goto 901

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
     &     ,repl_envvars        ! replace env variable function
     &     ,itmp

      character
     &     Param*20             ! parameter
     &     ,prev_param*20       ! previous parameter name
     &     ,Value*20            ! parameter value
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
 620  format(/a,' ',a/a)
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
         write(unit_error,'(a,i5/)') 'Error in making scalar SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a)') 'Made scalar SQL request'
      endif

c-----Bind variables to columns in result set      !fixme: layer needs to be bound
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Param,
     &     loc(scalarlen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, Value,
     &     loc(vallen), iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made scalar bind request'
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

         Value=Value(1:vallen)
         call locase(Value)

         counter=counter+1

c--------use only the last version of a parameter
         if (Param .ne. prev_param) then
            if (Param .eq. 'run_start_date') then
               run_start_date(1:9)=Value(1:9)

            else if (Param .eq. 'run_start_time') then
               run_start_date(11:14)=Value(1:4)

            else if (Param .eq. 'run_end_date') then
               run_end_date(1:9)=Value(1:9)

            else if (Param .eq. 'run_end_time') then
               run_end_date(11:14)=Value(1:4)

            else if (Param .eq. 'run_length') then
               run_length=Value
            else if (Param .eq. 'database') then
!fixme: how can we possibly get here and not know the database and model_name??
               if (database_name .eq. miss_val_c)ServerName=Value
               ServerName=Value
            else if (Param .eq. 'model_name') then
               if (model_name .eq. miss_val_c)model_name=Value

            else if (Param .eq. 'print_start_date') then
               nprints=nprints+1
               print_start_date(nprints)(1:9)=Value(1:9)

            else if (Param .eq. 'print_start_time') then
               print_start_date(nprints)(11:14)=Value(1:4)

            else if (Param .eq. 'flush_output') then
               flush_intvl=Value

            else if (Param .eq. 'binary_output') then
               itmp=repl_envvars(Value,ctmp)
               read(ctmp,'(l2)', err=810) binary_output

            else if (Param .eq. 'dss_direct') then
               itmp=repl_envvars(Value,ctmp)
               read(ctmp,'(l2)', err=810) dss_direct

            else if (Param .eq. 'hydro_time_step') then
               time_step_intvl_hydro=Value

            else if (Param .eq. 'qual_time_step') then
               time_step_intvl_qual=Value

            else if (Param .eq. 'ptm_time_step') then
               ptm_time_step_int=1
               time_step_intvl_ptm=Value

            else if (Param .eq. 'mass_tracking') then
               read(Value,'(l2)', err=810) mass_tracking

            else if (Param .eq. 'init_conc') then
               read(Value,'(f10.0)', err=810) init_conc

            else if (Param .eq. 'dispersion') then
               read(Value,'(l2)', err=810) dispersion

c--------------global rates for non-conserative const.

            else if (Param .eq. 'algaefract_n') then
               read(Value,'(f8.3)', err=810) algaefract_n

            else if (Param .eq. 'algaefract_p') then
               read(Value,'(f8.3)', err=810) algaefract_p

            else if (Param .eq. 'oxy_photo') then
               read(Value,'(f8.3)', err=810) oxy_photo

            else if (Param .eq. 'oxy_resp') then
               read(Value,'(f8.3)', err=810) oxy_resp

            else if (Param .eq. 'oxy_nh3') then
               read(Value,'(f8.3)', err=810) oxy_nh3

            else if (Param .eq. 'oxy_no2') then
               read(Value,'(f8.3)', err=810) oxy_no2

            else if (Param .eq. 'alg_chl_ratio') then
               read(Value,'(f8.3)', err=810) alg_chl_ratio

            else if (Param .eq. 'pref_factor') then
               read(Value,'(f8.3)', err=810) pref_factor

            else if (Param .eq. 'klight_half') then
               read(Value,'(f8.3)', err=810) klight_half

            else if (Param .eq. 'knit_half') then
               read(Value,'(f8.3)', err=810) knit_half

            else if (Param .eq. 'kpho_half') then
               read(Value,'(f8.3)', err=810) kpho_half

            else if (Param .eq. 'lambda0') then
               read(Value,'(f8.2)', err=810) lambda0

            else if (Param .eq. 'lambda1') then
               read(Value,'(f8.4)', err=810) lambda1

            else if (Param .eq. 'lambda2') then
               read(Value,'(f8.4)', err=810) lambda2
	      
		  else if (Param .eq. 'alg_bod' ) then
               read(Value,'(f8.4)', err=810) alg_bod 

c--------------heat and temperature related parameters

            else if (Param .eq. 'elev') then
               read(Value,'(f8.2)', err=810) elev

            else if (Param .eq. 'lat') then
               read(Value,'(f8.2)', err=810) lat

            else if (Param .eq. 'long') then
               read(Value,'(f8.2)', err=810) longitude

            else if (Param .eq. 'long_std_merid') then
               read(Value,'(f8.2)', err=810) long_std_merid

            else if (Param .eq. 'dust_attcoeff') then
               read(Value,'(f8.2)', err=810) dust_attcoeff

            else if (Param .eq. 'evapcoeff_a') then
               read(Value,'(f10.5)', err=810) evapcoeff_a

            else if (Param .eq. 'evapcoeff_b') then
               read(Value,'(f10.5)', err=810) evapcoeff_b

            else if (Param .eq. 'temp_bod_decay') then
               read(Value,'(f8.3)', err=810) thet(temp_bod_decay)

            else if (Param .eq. 'temp_bod_set') then
               read(Value,'(f8.3)', err=810) thet(temp_bod_set)

            else if (Param .eq. 'temp_reaer') then
               read(Value,'(f8.3)', err=810) thet(temp_reaer)

            else if (Param .eq. 'temp_do_ben') then
               read(Value,'(f8.3)', err=810) thet(temp_do_ben)

            else if (Param .eq. 'temp_orgn_decay') then
               read(Value,'(f8.3)', err=810) thet(temp_orgn_decay)

            else if (Param .eq. 'temp_orgn_set') then
               read(Value,'(f8.3)', err=810) thet(temp_orgn_set)

            else if (Param .eq. 'temp_nh3_decay') then
               read(Value,'(f8.3)', err=810) thet(temp_nh3_decay)

            else if (Param .eq. 'temp_nh3_ben') then
               read(Value,'(f8.3)', err=810) thet(temp_nh3_ben)

            else if (Param .eq. 'temp_no2_decay') then
               read(Value,'(f8.3)', err=810) thet(temp_no2_decay)

            else if (Param .eq. 'temp_orgp_decay') then
               read(Value,'(f8.3)', err=810) thet(temp_orgp_decay)

            else if (Param .eq. 'temp_orgp_set') then
               read(Value,'(f8.3)', err=810) thet(temp_orgp_set)

            else if (Param .eq. 'temp_po4_ben') then
               read(Value,'(f8.3)', err=810) thet(temp_po4_ben)

            else if (Param .eq. 'temp_alg_grow') then
               read(Value,'(f8.3)', err=810) thet(temp_alg_grow)

            else if (Param .eq. 'temp_alg_resp') then
               read(Value,'(f8.3)', err=810) thet(temp_alg_resp)

            else if (Param .eq. 'temp_alg_set') then
               read(Value,'(f8.3)', err=810) thet(temp_alg_set)

	      else if (Param .eq. 'temp_alg_die') then
               read(Value,'(f8.3)', err=810) thet(temp_alg_die)

            else if (Param .eq. 'display_intvl') then
               display_intvl=Value

            else if (Param .eq. 'deltax') then
c--------------keyword 'length' means use channel length for each delta x
               if (index(Value, 'len') .eq. 0) then
                  read(Value,'(f10.0)', err=810) deltax_requested
               else
                  deltax_requested=0.0
               endif

            else if (Param .eq. 'levee_slope') then
               read(Value,'(f10.0)', err=810) levee_slope

            else if (Param .eq. 'theta') then
               read(Value,'(f10.0)', err=810) theta
               if (
     &              theta .lt. 0.5 .or.
     &              theta .gt. 1.0
     &              ) then
                  write(unit_error, 615) theta
                  istat=-1
                  goto 900
               endif

            else if (Param .eq. 'terms') then
               terms=0
               if (Value(1:3) .eq. 'dyn') then
                  terms=1       ! dynamic wave
               else if (Value(1:3) .eq. 'dif') then
                  terms=2       ! diffusion wave
               else if (Value(1:3) .eq. 'kin') then
                  terms=3       ! kinematic wave
               else
                  write(unit_error, 620)
     &                 'Unrecognized value for solution method:',
     &                 trim(Value),
     &                 'Should be dynamic, diffusion, or kinematic.'
                  istat=-1
                  goto 900
               endif

            else if (Param .eq. 'vardensity') then
               read(Value,'(l2)', err=810) variabledensity
               if (variabledensity .and. terms .ne. 1) then
                  variabledensity=.false.
                  write(unit_error, 630)
     &                 'Warning: Variable Density allowed only with dynamic wave.'
               endif

            else if (Param .eq. 'varsinuosity') then
               read(Value,'(l2)', err=810) variablesinuosity
               if (variablesinuosity .and. terms .eq. 3) then
                  variablesinuosity=.false.
                  write(unit_error, 630)
     &                 'Warning: variable sinuosity not allowed with kinematic wave.'
               endif

            else if (Param .eq. 'gravity') then
               read(Value,'(f10.0)', err=810) gravity

            else if (Param .eq. 'toleranceq') then
               read(Value,'(f10.0)', err=810) toleranceq

            else if (Param .eq. 'tolerancez') then
               read(Value,'(f10.0)', err=810) tolerancez

            else if (Param .eq. 'maxiter') then
               read(Value,'(i5)', err=810) maxiterations

            else if (Param .eq. 'luinc') then
               read(Value,'(i5)', err=810) luinc

            else if (Param .eq. 'printlevel') then
               if(print_level .eq. miss_val_i) then
                                ! setting this over the text value will cause
                                ! funny echoed output, so set only if not initialized
                  read(Value,'(i5)', err=810) print_level
               end if

            else if (Param .eq. 'temp_dir') then
               temp_dir=Value

            else if (Param .eq. 'checkdata') then
               read(Value,'(l2)', err=810) check_input_data

            else if (Param .eq. 'cont_missing') then
               read(Value,'(l2)', err=810) cont_missing

            else if (Param .eq. 'cont_unchecked') then
               read(Value,'(l2)', err=810) cont_unchecked

            else if (Param .eq. 'cont_question') then
               read(Value,'(l2)', err=810) cont_question

            else if (Param .eq. 'cont_bad') then
               read(Value,'(l2)', err=810) cont_bad

            else if (Param .eq. 'warn_missing') then
               read(Value,'(l2)', err=810) warn_missing

            else if (Param .eq. 'warn_unchecked') then
               read(Value,'(l2)', err=810) warn_unchecked

            else if (Param .eq. 'warn_question') then
               read(Value,'(l2)', err=810) warn_question

            else if (Param .eq. 'warn_bad') then
               read(Value,'(l2)', err=810) warn_bad

            else if (Param .eq. 'ptm_ivert') then
               ptm_ivert_int=1
               read(Value,'(l2)', err=810) ptm_ivert

            else if (Param .eq. 'ptm_itrans') then
               ptm_itrans_int=1
               read(Value,'(l2)', err=810) ptm_itrans

            else if (Param .eq. 'ptm_iey') then
               ptm_iey_int=1
               read(Value,'(l2)', err=810) ptm_iey

            else if (Param .eq. 'ptm_iez') then
               ptm_iez_int=1
               read(Value,'(l2)', err=810) ptm_iez

            else if (Param .eq. 'ptm_flux_percent') then
               ptm_flux_percent_int=1
               read(Value,'(l2)', err=810) ptm_flux_percent

            else if (Param .eq. 'ptm_group_percent') then
               ptm_group_percent_int=1
               read(Value,'(l2)', err=810) ptm_group_percent

            else if (Param .eq. 'ptm_flux_cumulative') then
               ptm_flux_cumulative_int=1
               read(Value,'(l2)', err=810) ptm_flux_cumulative

            else if (Param .eq. 'ptm_random_seed') then
               ptm_random_seed_int=1
               read(Value,'(i5)', err=810) ptm_random_seed

            else if (Param .eq. 'ptm_no_animated') then
               ptm_no_animated_int=1
               read(Value,'(i5)', err=810) ptm_no_animated

            else if (Param .eq. 'ptm_trans_constant') then
               ptm_trans_constant_int=1
               read(Value,'(f7.4)', err=810) ptm_trans_constant

            else if (Param .eq. 'ptm_vert_constant') then
               ptm_vert_constant_int=1
               read(Value,'(f7.4)', err=810) ptm_vert_constant

            else if (Param .eq. 'ptm_iprof') then
               ptm_iprof_int=1
               read(Value,'(l2)', err=810) ptm_iprof

            else if (Param .eq. 'ptm_trans_a_coef') then
               ptm_trans_a_coef_int=1
               read(Value,'(f7.4)', err=810) ptm_trans_a_coef

            else if (Param .eq. 'ptm_trans_b_coef') then
               ptm_trans_b_coef_int=1
               read(Value,'(f7.4)', err=810) ptm_trans_b_coef

            else if (Param .eq. 'ptm_trans_c_coef') then
               ptm_trans_c_coef_int=1
               read(Value,'(f7.4)', err=810) ptm_trans_c_coef

            else if (Param .eq. 'ptm_shear_vel') then
	         write(unit_error,610)"ptm_shear_vel not used in this version of PTM"
               istat=-2
	         goto 900
            else if (Param .eq. 'repeating_tide') then
	         write(unit_error,610)"repeating tide is deprecated"
	         goto 900	         
            else if (Param .eq. 'warmup_run') then
	         write(unit_error,610)"repeating tide is deprecated"
	         goto 900	         
            else
               write(unit_error,610), Param, Value
               istat=-1
               goto 900
            endif
         endif
         if (print_level .ge. 3)
     &        write(unit_screen,'(a,t20,a)')
     &        trim(Param),trim(Value)
         prev_param=Param
      enddo

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all scalar data', counter

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding scalar SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a//)') 'Unbound scalar SQL'
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
         write(unit_error,'(a,i5//)') 'error in unbinding scalar SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a//)') 'Unbound scalar SQL'
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
      end

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

c     get the integer type code given a character string representing the type
      integer*4 function obj_type_code(objtype)
      use constants
	implicit none
	character*(*) :: objtype
      character*32  :: cstring
	obj_type_code=miss_val_i
	cstring=trim(objtype)
	call locase(cstring)
	if (index(cstring,"chan") .eq. 1)then 
	  obj_type_code=obj_channel
	else if (index(cstring,"gate") .eq. 1)then
	  obj_type_code=obj_gate
	else if (index(cstring,"res") .eq. 1)then
	  obj_type_code=obj_reservoir
	else if (index(cstring,"transfer") .eq. 1)then
	  obj_type_code=obj_obj2obj
	else if (index(cstring,"group") .eq. 1)then
	  obj_type_code=obj_group
	else if (index(cstring,"qext") .eq. 1)then
	  obj_type_code=obj_qext
	else if (index(cstring,"stage") .eq. 1)then
	  obj_type_code=obj_stage
      end if
	return
	end





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
         write(unit_error,'(a5,1x,i8,1x,a)')
     &        SqlState, NativeError, Msg(1:MsgLen)
         iDiag=iDiag+1
      enddo

      return
      end subroutine
