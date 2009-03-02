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


      subroutine load_reservoirs_SQL(StmtHndl, ModelID, istat)
      use IO_Units
c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use DSM2_database
      use grid_data
      use logging
      implicit none


c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) ModelID           ! which ModelID to select
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables

      integer UseObj

      integer
     &     ID
     &     ,counter

      real*4
     &     reser_area
     &     ,reser_botelv

      character
     &     reser_name*32
     &     ,prev_name*32        ! previous reservoir name

      integer(SQLINTEGER_KIND):: namelen =0

c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 

c-----Execute SQL statement
      StmtStr="SELECT Reservoir_ID, Used, Name, Area, Bottom_Elev " //
     &     "FROM Reservoir INNER JOIN Model_Component ON " //
     &     "Reservoir.Layer_ID = Model_Component.Component_ID " //
     &     "WHERE Model_Component.Component_Type = 'grid' " //
     &     "AND Model_Component.Model_ID = ? " //
     &     "ORDER BY reservoir.name, model_component.layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making reservoir SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-1
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a)') 'Made reservoir SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, reser_name,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_FLOAT, reser_area,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_FLOAT, reser_botelv,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made reservoir bind request'
c-----Loop to fetch records, one at a time
      nreser=0
      prev_name=miss_val_c
      counter=0
      do while (.true.)
c--------Fetch a record from the result set
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit

         reser_name=reser_name(1:namelen)
         call locase(reser_name)

c--------use only the last version of a reservoir, and skip
c--------if the reservoir is marked as not-use
         if (reser_name .ne. prev_name .and.
     &        UseObj) then
            call process_reservoir(id,
     &                             reser_name(1:namelen),
     &                             dble(reser_area),
     &                             dble(reser_botelv))
         endif
         prev_name=reser_name
         counter=counter+1
      enddo

      if (counter .eq. 0) then
         write(unit_error, '(a)') 'Note: no reservoir records retrieved.'
      endif
      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all reservoir data', nreser

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding reservoir SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a//)') 'Unbound reservoir SQL'
      endif

      return
      end

c=================================================================================

      subroutine load_reservoir_connections_SQL(StmtHndl, istat)
      use IO_Units
c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use grid_data
      use logging

      implicit none
      include '../hydrolib/network.inc'

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
     &     ,istat               ! status

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet=0
      integer(SQLSMALLINT_KIND)::ColNumber=0 ! SQL table column number

c-----local variables

      integer
     &      resno               ! reservoir number
     &     ,con_node            ! connecting node number
     &     ,nn                  ! connecting node number
     &     ,resID
     &     ,counter

      real*4
     &     rescon_incoef
     &     ,rescon_outcoef

      character
     &     ResIDStr*12          ! ResID as string

      integer ext2intnode

c-----prepare statement including a parameter for resIDStr

      StmtStr="SELECT " //
     &     "Connected_Node_Number, " //
     &     "In_Coef, Out_Coef " //
     &     "FROM Reservoir_Connections " //
     &     "WHERE Reservoir_ID = ?" // " " //
     &     "ORDER BY Reservoir_ID, Connected_Node_Number;"

      call f90SQLPrepare(StmtHndl, StmtStr, iRet) 
      call f90SQLBindParameter(StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(0,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), 
     &     resID, f90SQL_NULL_PTR, iRet)

c-----Bind variables to columns in result set
      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, con_node,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_FLOAT, rescon_incoef,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_FLOAT, rescon_outcoef,
     &     f90SQL_NULL_PTR, iRet)

c-----loop over reservoirs, adding connecting nodes from
c-----ReservoirConnections table
      do resno=1,nreser
         if (res_geom(resno).inUse) then
            if (print_level .ge. 3)
     &           write(unit_screen,'(a,a)') 'For reservoir ',
     &           trim(res_geom(resno).name)
            write(ResIDStr,'(i12)') res_geom(resno).ID
            resID=res_geom(resno).id

c-----------Execute SQL statement

            call f90SQLExecDirect(StmtHndl, StmtStr,iRet)
            if (iRet.ne.SQL_SUCCESS) then
               write(unit_error,'(a,a/a,i5)')
     &              'For reservoir ', res_geom(resno).name,
     &              'error in making reservoir connection SQL request ',iRet
               call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
               istat=-1
               return
            endif

c-----------Loop to fetch records, one at a time
            counter=0
            do while (.true.)
c--------------Fetch a record from the result set
               call f90SQLFetch(StmtHndl,iRet)
               if (iRet .eq. SQL_NO_DATA) exit
               call process_reservoir_connection(res_geom(resno).name,
     &                                           con_node,
     &                                           dble(rescon_incoef),
     &                                           dble(rescon_outcoef))
      

               counter=counter+1
            enddo
            call f90SQLFreeStmt(StmtHndl,SQL_CLOSE, iRet) 
         endif
         res_geom(resno).nConnect=res_geom(resno).nnodes
      enddo
      
      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,a/a,i5)')
     &        'For reservoir ',
     &        trim(res_geom(resno).name),
     &        ' error in unbinding reservoir connection SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      endif

      if (print_level .ge. 2)
     &     write(unit_screen,'(a/)') 'Read in all reservoir connection data'

      return
      end subroutine
