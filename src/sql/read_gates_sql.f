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

      subroutine load_gates_SQL(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use Gates, only: gateArray,nGate, MAX_GATES
      use IO_Units
      use DSM2_database
      use logging
      use constants
      use grid_data
      implicit none


c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
      integer(DBASE_ID_KIND) ModelID           ! which ModelID to select
     &     ,istat               ! status ( < 0 means error, >= 0 is #gates)

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber=0 ! SQL table column number
      integer(SQLINTEGER_KIND):: objidlen,namelen

c-----local variables

      integer UseObj

      integer
     &     ID
     &     ,LayerID
     &     ,ObjConnType         ! connected to channel, reservoir, etc.
     &     ,NodeConn            ! node connected to
     &     ,name_to_objno       ! function to get object number
     &     ,channo
     &     ,resno
     &     ,counter
     &     ,i

      character
     &     name*32
     &     ,prev_name*32
     &     ,ObjConnID*32        ! name of reservoir, number of channel
     &     ,channoStr*10

      integer ext2intnode

c-----Bind the parameter representing ModelID
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet)
c-----Execute SQL statement
      StmtStr="SELECT Gate_ID, used, Name, Obj_Connected_Type, " //
     &     "Obj_Connected_Identifier, Node_Connected, Layer_ID " //
     &     "FROM Gate INNER JOIN Model_Component ON " //
     &     "Gate.Layer_ID = Model_Component.Component_ID " //
     &     "WHERE Model_Component.Component_Type = 'grid' " //
     &     "AND Model_Component.Model_ID = ? " //
     &     "ORDER BY Gate.Name, Model_Component.Layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making gate SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-1
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a)') 'Made gate SQL request'
      endif

c-----Bind variables to columns in result set
      ObjConnID=" "
      channoStr=" "
      name=" "
      ColNumber=0

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, name,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ObjConnType,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, ObjConnID,
     &     loc(objidlen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, NodeConn,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, LayerID,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3)
     &     write(unit_screen,'(a)') 'Made gate bind request'
c-----Loop to fetch records, one at a time
      prev_name=miss_val_c
      nGate=0
      counter=0
      do while (.true.)

c--------Fetch a record from the result set

         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit
         if (iRet .ne. SQL_SUCCESS) then
            write(unit_error, 625) name
 625        format(/'Invalid or Null data for gate record ',a)
            istat=-1
            return
         endif

         name=name(:namelen)
         call locase(name)
         
         objConnID=objConnID(:objidlen)

c--------use only the last version of a gate, and skip
c--------if the gate is marked as not-use
         if (name .ne. prev_name .and.
     &        UseObj) then
            call process_gate(id,
     &                        useObj,
     &                        name,
     &                        ObjConnType,
     &                        ObjConnID,
     &                        nodeConn)
         endif
         prev_name=name
         counter=counter+1
      enddo
      
      if (counter .eq. 0) then
         write(unit_error, '(a)') 'Note: no gate records retrieved.'
      endif
      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all gate data', ngate

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding gate SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a//)') 'Unbound gate SQL'
      endif


      istat = ngate

      return
      end

c==============================================================================


      subroutine load_gate_devices_SQL(StmtHndl, istat)

c-----load f90SQL modules
      use Gates, only: gateArray,maxNGate,
     &     PIPE,WEIR,MAX_DEV,GATE_OPEN,GATE_CLOSE,
     &     UNIDIR_TO_NODE,UNIDIR_FROM_NODE
      use f90SQLConstants
      use f90SQLStructures
      use f90SQL
      use io_units
      use logging
      
      implicit none

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
     &     ,istat               

c-----f90SQL variables
      character(len=1000)::StmtStr
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number
      integer(SQLINTEGER_KIND):: namelen
      type(TIMESTAMP_STRUCT) :: time

c-----local variables

      integer
     &     gateID               ! gate ID
     &     ,gateno              ! counter for gates
     &     ,devno
     &     ,nduplicate           ! number of dublicate structures
     &     ,struct_type          ! type of structure (weir,pipe)
     &     ,control_type         ! flow control device (type of gate)
     &     ,count
     &     ,ndx,i,nw
     &     ,nout
     &     ,default_op
     &     ,get_objnumber       ! function to get object number   

      real*8
     &     max_width
     &     ,base_elev,height
     &     ,CFfrom,CFto
     &     ,from_op,to_op

      character*32 :: name = " "
      character*32 :: gatename = " "

      
c-----Create query statement. All devices of a particular type are fetched at
c-----once. 

      ndx=1
      StmtStr="SELECT  gate_device.name, structure_type, control_type, " //
     &     "nduplicate, max_width, base_elev, " //
     &     "height,flow_coef_from_node, flow_coef_to_node,default_op " //
     &     "FROM gate_device,gate " //
     &     "WHERE gate_device.gate_ID = gate.gate_id AND gate.gate_ID = ? "//
     &     "ORDER BY gate_device.name;"

      call f90SQLPrepare(StmtHndl, StmtStr, iRet) 
c      call f90SQLBindParameter(StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
c     &     SQL_F_CHAR, SQL_VARCHAR, int(32,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), 
c     &     gatename, f90SQL_NULL_PTR, iRet)

      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     gateID, f90SQL_NULL_PTR, iRet)

c-----Bind variables to columns in result set

         ColNumber=0

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, name,
     &     loc(namelen), iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, struct_type,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, control_type,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, nduplicate,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, max_width,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, base_elev,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, height,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, CFfrom,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_DOUBLE, CFto,
     &     f90SQL_NULL_PTR, iRet)

         ColNumber=ColNumber+1
         call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, default_op,
     &     f90SQL_NULL_PTR, iRet)

      count=1

      do gateno=1,maxNGate
c-----------Execute SQL statement
         gatename=gateArray(gateno).name
         gateID=gateArray(gateno).ID
         
	   call f90SQLExecDirect(StmtHndl, StmtStr,iRet)
         if (iRet.ne.SQL_SUCCESS) then
            write(unit_error,'(a,a/a,i5)')
     &        'For gate ', name,
     &        'error in making gate device SQL request ',iRet
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
            istat=-1
            return
         endif
	   gateArray(gateNo).ndevice=0
c-----Loop to fetch records, one at a time
         do while (.true.)
c--------Fetch a record from the result set and append it to gateWeirHistory, 

            call f90SQLFetch(StmtHndl,iRet) 
            if (iRet .eq. SQL_NO_DATA) exit
            name=name(1:namelen)
            call locase(name)              
            call process_gate_device(gatename,
     &                               name,
     &                               struct_type,
     &                               control_type,
     &                               nduplicate,
     &                               max_width,
     &                               base_elev,
     &                               height,
     &                               CFfrom,
     &                               CFto,
     &                               default_op)
	      count=count + 1
         enddo

         call f90SQLCloseCursor (StmtHndl, iRet)
         if (iRet.ne.SQL_SUCCESS) then
            write(unit_error,'(a,a,i5)') 'For gate ', gateArray(gateNo).name,
     &           'error in unbinding gate device SQL',iRet
            call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
            istat=-3
            return
         endif
      end do

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5)')
     &        'Error in unbinding gate weir SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      endif

      if (print_level .ge. 2)
     &     write(unit_screen,'(a/)') 'Read in all gate device data'

      return
      end subroutine




