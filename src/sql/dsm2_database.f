      module dsm2_database
      use f90SQLConstants
      use f90SQL
      use IO_Units
      use DFLIB
      use DFPORT

      implicit none

      integer, parameter :: DBASE_ID_KIND = 4 ! int used for dbase keys 
      character*80 database_name
      character*32 sim_name
      character*48 model_name
      character*48 hydro_model_name

      integer(DBASE_ID_KIND) :: ModelID       ! which ModelID to select
      integer(DBASE_ID_KIND) :: HydroModelID  ! which ModelID to select

c-----f90SQL variables
      integer(SQLHANDLE_KIND):: EnvHndl = 0
	integer(SQLHANDLE_KIND):: ConnHndl = 0
	integer(SQLHANDLE_KIND):: StmtHndl = 0
      logical :: dbase_open = .false.

c-----f90SQL variables
      character(len=255):: ServerName
	character(len=255):: UsrNameStr = 'dsmtwo'
	character(len=255):: PasswordStr = 'User2Dmin'
c	character(len=255):: UsrNameStr = 'SYSDBA'
c	character(len=255):: PasswordStr = 'User123'

      private StmtHndl,ConnHndl,EnvHndl
	private simple_error_message
	private ModelID
      
	contains


      subroutine init_database(istat)
	implicit none
	integer istat
	call open_database(istat)
	if (istat .ne. 0) call simple_error_message( 
     &   "Error opening database " // ServerName // " from init_database", .false.)
      call load_model_id(StmtHndl, model_name,ModelId,istat)
	if (istat .ne. 0) call simple_error_message( 
     &   "Error obtaining model_id for model " // model_name 
     &    // " (in init_database)", .false.)

	return
	end subroutine

      subroutine simple_error_message(message, fail)
	  use IO_Units
        implicit none
	  logical fail
	  character*(*) :: message
	  write(unit_error,'(a)') message
	  call exit(3)
      end subroutine


      subroutine get_statement_handle(handle, istat)
	use IO_Units
	implicit none
	integer istat
	integer(SQLHANDLE_KIND) handle
c	if (.not. dbase_open) then
c	   call simple_error_message(
c     &    "Request for dbase statement handle made with database not open",
c     &     .false.)
c	   handle=0
c	   istat=3
c	else 
         handle = StmtHndl
	   istat=0
c	end if
	return
	end subroutine

      subroutine get_model_id(model_id, istat)
	use IO_Units
	implicit none
	integer(DBASE_ID_KIND) model_id 
	integer istat
	if (.not. dbase_open)then
	   call simple_error_message(
     &       "Request for model_id made with database not open", .false.)
         model_id = 0
	   istat = 3
	else   
	   model_id = ModelID
	   istat = 0
	end if
	return
	end subroutine

      subroutine get_hydro_model_id(model_id, istat)
	use IO_Units
	implicit none
	integer(DBASE_ID_KIND) model_id 
	integer istat
	if (.not. dbase_open)then
	   call simple_error_message(
     &       "Request for hydro_model_id made with database not open", .false.)
         model_id = 0
	   istat = 3
	else   
	   model_id = HydroModelID
	   istat = 0
	end if
	return
	end subroutine


      subroutine open_database(istat)      
      implicit none
	include '../fixed/common.f'
      integer(SQLRETURN_KIND) iret
      integer istat
c-----allocate an environment handle
      call f90SQLAllocHandle(
     &           SQL_HANDLE_ENV, 
     &           SQL_NULL_HANDLE, 
     &           EnvHndl, 
     &           iRet)

      if (iRet.ne.SQL_SUCCESS) then
         istat=1
         return
      endif

c-----Set ODBC version we will be using (3.x in this case)
      call f90SQLSetEnvAttr(EnvHndl, 
     &     SQL_ATTR_ODBC_VERSION, 
     &     SQL_OV_ODBC3,
     &     iRet)

c-----Allocate a connection handle
      call f90SQLAllocHandle(SQL_HANDLE_DBC, 
     &     EnvHndl, ConnHndl, iRet)

c-----make connection to selected data source
      ServerName=database_name  !'DSM2 Input Database'

      call f90SQLConnect(
     &	     ConnHndl,
     &         trim(ServerName),
     &         trim(UsrNameStr), 
     &         trim(PasswordStr), 
     &         iRet)

c-----print*,ServerName
c-----call f90SQLDriverConnect(ConnHndl,SQL_NULL_HANDLE,trim(ServerName),
c     &      OutConnectionString, OutConnectionStringLength, DriverCompletion, iRet)

      if (iRet.ne.SQL_SUCCESS .and. iRet.ne.SQL_SUCCESS_WITH_INFO) then
         write(unit_error,'(a)') 'Error in making SQL connection'
         call ShowDiags( SQL_HANDLE_DBC, ConnHndl )
         istat=2
         return
      else
         if (print_level .ge. 2)
     &        write(unit_screen,'(a)') 'Made SQL connection'
      endif

c-----Allocate statement handle
      call f90SQLAllocHandle(SQL_HANDLE_STMT, ConnHndl, StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         istat=3
         return
      endif
      dbase_open=.true.
	return
	end subroutine

	subroutine close_database()
         implicit none
         integer(SQLRETURN_KIND) iret

	   !todo: error handling
         call f90SQLDisconnect(ConnHndl, iRet)
         call f90SQLFreeHandle(SQL_HANDLE_DBC, ConnHndl, iRet)
         dbase_open=.false.
	   return
	end subroutine

      subroutine load_model_id(StmtHndl,ModelName,ModelID,istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      implicit none
      include '../fixed/common.f'

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) ModelID           ! which ModelID to select
     &     ,istat               ! status
      character*48 ModelName    ! name of model

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables
      integer
     &     modID,hydroModID

      character
     &     modName*48           ! Model name as string
      character
     &     hydroModName*48           ! Model name as string

c-----defaults

c-----Execute SQL statement
      StmtStr="SELECT x0.model_id, x0.name, x1.model_id, x1.name " //
     &     "FROM model_definition x0, model_definition x1 " //
     &     "WHERE x0.name = '" // ModelName // "'" //
     &     "AND x0.simulation_id = x1.simulation_id "//
     &     "AND x1.computer_model LIKE 'hydro';"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making scalar SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a)') 'Made modelID SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, modID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, modName,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, hydroModID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, hydroModName,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made ModelID bind request'

c-----Fetch a record from the result set. The database already guarantees at most one

      call f90SQLFetch(StmtHndl,iRet)
      if (iRet .eq. SQL_NO_DATA) then
         write(unit_error,117) ModelName
         istat=-3
         return
      end if
      ModelID=modID
	HydroModelID=hydroModID

      if (print_level .ge. 3)
     &     write(unit_screen,'(a/)') 'Converted model name to ID'

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding modelID SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a//)') 'Unbound modelID SQL'
      endif

 117  format(/'Unrecognized model name:'
     &     /a,a)
      return
      end subroutine
	end module







