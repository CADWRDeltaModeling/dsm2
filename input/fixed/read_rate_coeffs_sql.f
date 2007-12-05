



      subroutine load_rate_coeffs_SQL(StmtHndl, ModelID, istat)
c      subroutine load_rate_coeffs_SQL(istat) 


      use IO_Units
c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use DSM2_database
	use Groups, only: groupContains,IsAllChannelReservoir,groupArray
	use rate_coeff_assignment,only:assign_rate_to_group,rate_var_require_flag
      implicit none

      include 'common.f'
	include 'common_qual.inc'

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number


c-----External Functions
      integer,external :: name_to_objno

c-----local variables


      integer
     &     rate_variable_id,pre_rate_variable_id
     &     ,constituent_id,pre_constituent_id

      real*4
     &     coefficient_value

      character
     &     group_name*32
     &     ,prev_group_name*32        ! previous reservoir name

      character errm*128 !error message

      integer(SQLINTEGER_KIND):: namelen =0

	integer ncoeff,  !number of coefficients
     &        counter,
     &       groupno

      integer i


c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 

c-----Execute SQL statement
      StmtStr="SELECT group_name, rate_variable_id, constituent_id, coefficient_value " //
     &     "FROM rate_coefficient INNER JOIN model_component ON " //
     &     "rate_coefficient.Layer_ID = Model_Component.Component_ID " //
     &     "WHERE model_component.model_id = ? " //
     &     "ORDER BY model_component.layer,group_name,rate_variable_id,constituent_id;"
c    &     "ORDER BY rate_variable_id,constituent_id,Model_Component.Layer DESC;"
      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making rate_coefficient SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-1
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a)') 'Made rate_coefficient SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
	call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, group_name,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, rate_variable_id,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, constituent_id,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_FLOAT, coefficient_value,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made rate_coefficient bind request'
c-----Loop to fetch records, one at a time
      ncoeff=0
      prev_group_name=miss_val_c
	pre_rate_variable_id=miss_val_i
      pre_constituent_id=miss_val_i
      counter=0
      do while (.true.)
c--------Fetch a record from the result set
         group_name=' '
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit

         group_name=group_name(1:namelen)
	   group_name=trim(group_name)
         call locase(group_name)

c--------use only the last version of a rate
c--------
c         if ((group_name .ne. prev_group_name) .or.
c     &       (rate_variable_id.or.pre_rate_variable_id).or.
c     &        (constituent_id.ne.pre_constituent_id)) then

            ncoeff=ncoeff+1
	      groupno = name_to_objno(obj_group, group_name)
	      if (groupno.lt.0) then
	           istat=-3
                 write(unit_error, '(a)') 'Group not recognized: ' // group_name
	           return
	      end if
		  if (not(IsAllChannelReservoir(groupArray(groupno)))) then
                 istat=-3
	           write(unit_error, '(a)') 'Members of group'//group_name//" are not all channel or reservior"
	           return

	      end if
            coefficient_value=coefficient_value/24. ! convert per day to per hour
            call assign_rate_to_group(groupno,rate_variable_id,constituent_id,
     &		   coefficient_value,istat,errm)
	      if (istat.lt.0) then
               write(unit_error, '(a)') errm
			 return
	      end if

            rate_var_require_flag(constituent_id,rate_variable_id)=.true.
c        end if
c        prev_group_name=group_name
c	   pre_rate_variable_id=rate_variable_id
c        pre_constituent_id=constituent_id
         counter=counter+1

      end do

      if (counter .eq. 0) then
         write(unit_error, '(a)') 'Note: no rate coefficient records retrieved.'
      endif
      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all rate coefficient data ', ncoeff

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding rate coefficeint SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a//)') 'Unbound rate coefficient SQL'
      endif

 630  format(/a,i5)

      return
      end