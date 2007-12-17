      subroutine read_operating_rules(istat)
      use dsm2_database
      use IO_Units
	implicit none
	integer :: istat
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) :: ModelID           ! which ModelID to select

	call get_statement_handle(StmtHndl, istat)
	call get_model_id(ModelID, istat)
	call load_oprules_sql(StmtHndl,ModelID, istat)

	return
	end subroutine

	
	subroutine load_oprules_sql(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      implicit none

      include '../fixed/common.f'

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
     &     ID
     &     ,actionID            ! Action when trigger is set off
     &     ,counter
     &     ,allocstat
     &     ,ext2int
     &     ,name_to_objno       ! Function for converting object name to internal number
     &     ,nenv,repl_envvars   ! environment var replacement
     &     ,deflen

      character
     &     name*32,prev_name*32
     &     ,definitiontext*255,triggertext*255
     &     ,ctmp*512

      integer(SQLINTEGER_KIND):: namelen,definelen,triggerlen
	logical parse_rule

c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 

c-----Execute SQL statement
      StmtStr="SELECT expression_id, expression.name, definition "//
     &     "FROM expression,model_component " //
     &     "WHERE expression.layer_id = model_component.component_id " //
     &     "AND model_component.model_id = ? " //
     &     "AND model_component.component_type = 'oprule' " //
     &     "ORDER BY expression.name,layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making operating rule SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a)') 'Made operating rule SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, name,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, definitiontext,
     &     loc(definelen), iRet)

      if (print_level .ge. 3)
     &     write(unit_screen,'(a)') 'Made operating rule bind request'

c-----Loop to fetch records, one at a time
      counter=0
      prev_name=miss_val_c

      do while (.true.)
c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
         if (iRet.eq. -1) then
            write(unit_error,'(a,i5//)') 'Error in fetching operating rule SQL',iRet
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         end if
         name=name(1:namelen)
         call locase(name)
	   if (name .ne. prev_name ) then ! use highest layer, if UseObj=T
	      definitiontext=definitiontext(1:definelen)
            nenv=repl_envvars(definitiontext,ctmp)
	      definitiontext=ctmp
	      ctmp=" "
		  write(ctmp,"(a,1x,':=',1x,a,';')")
     &        name(1:namelen),definitiontext(1:len_trim(definitiontext))
            if (parse_rule(trim(ctmp)))then
	         if(print_level .ge. 3) then
			    write(unit_screen,"(/'Parsed expression: ',/a)"),trim(ctmp)
	         end if
            else
               write(unit_error,"(/'Error parsing expression: ',/a)"), trim(ctmp)
	         istat=-3
	         return
	      endif
	      counter=counter+1
         endif
         prev_name=name
      enddo

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i4/)')
     &      'Read in all operating rule expressions', counter

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 
     &     'Error in unbinding operating rule expression cursor',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound gate operating rule SQL'
      endif



c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 

c-----Execute SQL statement
      StmtStr="SELECT oprule_id, used, name, oprule_action, oprule_trigger "//
     &     "FROM operating_rule,model_component " //
     &     "WHERE operating_rule.layer_id = model_component.component_id " //
     &     "AND model_component.model_id = ? " //
     &     "AND model_component.component_type = 'oprule' " //
     &     "ORDER BY operating_rule.name,layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making operating rule SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a)') 'Made operating rule SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, name,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, definitiontext,
     &     loc(definelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, triggertext,
     &     loc(triggerlen), iRet)

      if (print_level .ge. 3)
     &     write(unit_screen,'(a)') 'Made operating rule bind request'

c-----Loop to fetch records, one at a time
      counter=0
      prev_name=miss_val_c

      do while (.true.)
c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
         if (iRet.eq. -1) then
            write(unit_error,'(a,i5//)') 'Error in fetching operating rule SQL',iRet
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         end if
         name=name(1:namelen)
         call locase(name)
	   ctmp=" "
         if (name .ne. prev_name .and. UseObj) then ! use highest layer, if UseObj=T
	      definitiontext=definitiontext(1:definelen)
            triggertext=triggertext(1:triggerlen)
            nenv=repl_envvars(definitiontext,ctmp)
	      definitiontext=ctmp
            nenv=repl_envvars(triggertext,ctmp)
	      triggertext=ctmp
	      ctmp=" "
		  write(ctmp,"(a,1x,':=',1x,a,1x,'WHEN',1x,a,';')")
     &        trim(name),trim(definitiontext),trim(triggertext)
	      if(print_level .ge. 3) then
		    write(unit_screen,"(/'Parsing rule: ',/a)"),trim(ctmp)
	      end if
	      if (parse_rule(trim(ctmp)))then

            else
               write(unit_error,"(/'Error parsing rule: ',/a)"), trim(ctmp)
	         istat=-3
	         return
	      endif
	      counter=counter+1
         endif
         prev_name=name
      enddo

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i4/)') 'Read in all gate operating rule data', counter

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding operating rule SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound gate operating rule SQL'
      endif


      return
      end subroutine
