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
         reser_name=' '
         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit

         reser_name=reser_name(1:namelen)
	   reser_name=trim(reser_name)
         call locase(reser_name)

c--------use only the last version of a reservoir, and skip
c--------if the reservoir is marked as not-use
         if (reser_name .ne. prev_name .and.
     &        UseObj) then
            nreser=nreser+1
            if (nreser .gt. max_reservoirs) then
               write(unit_error,630)
     &              'Reservoir number too high; max allowed is:',
     &              max_reservoirs
               istat=-1
               return
            endif
            res_geom(nreser).id=ID
            res_geom(nreser).inUse=.true.
            res_geom(nreser).name=trim(reser_name)
            res_geom(nreser).area=reser_area
            res_geom(nreser).botelv=reser_botelv
            if (print_level .ge. 3)
     &           write(unit_screen,'(i5,1x,a)')
     &           nreser,trim(res_geom(nreser).name)
c                resext2int(resno)=nreser    !todo: resext2int
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

 630  format(/a,i5)

      return
      end

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
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number

c-----local variables

      integer
     &     connID               ! connection ID
     &     ,resno               ! reservoir number
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
     &     "Connection_ID, Connected_Node_Number, " //
     &     "In_Coef, Out_Coef " //
     &     "FROM Reservoir_Connections " //
     &     "WHERE Reservoir_ID = ?" // " " //
     &     "ORDER BY Reservoir_ID, Connected_Node_Number;"

      call f90SQLPrepare(StmtHndl, StmtStr, iRet) 
      call f90SQLBindParameter(StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(0,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), 
     &     resID, f90SQL_NULL_PTR, iRet)

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, connID,
     &     f90SQL_NULL_PTR, iRet)

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

c--------------no duplicate or deleted connections are allowed; create a new
c--------------reservoir instead
               res_geom(resno).nnodes=res_geom(resno).nnodes+1
	         if (res_geom(resno).nnodes .gt. MaxResConnectChannel)then
                    write(unit_error,*) 'Number of reservoir connections for ',
     &               res_geom(resno).name, ' exceeds maximum of ',
     &               MaxResConnectChannel
                     istat=-1
                  return
               endif	                   
               nn=res_geom(resno).nnodes
               res_geom(resno).isNodeGated(nn)=.false.
                                ! fixme check that only gated or reservoir connection, not both
               res_geom(resno).node_no(nn)=ext2intnode(con_node)
               res_geom(resno).coeff2res(nn)=rescon_incoef
               res_geom(resno).coeff2chan(nn)=rescon_outcoef
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
      end

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
      integer(SQLSMALLINT_KIND)::ColNumber ! SQL table column number
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
	   name=trim(name)
         call locase(name)

c--------use only the last version of a gate, and skip
c--------if the gate is marked as not-use
         if (name .ne. prev_name .and.
     &        UseObj) then
            ngate=ngate+1
            if (ngate .gt. max_gates) then
               write(unit_error,630)
     &              'Too many gates specified; max allowed is:' ,max_gates
               istat=-1
               return
            endif
            gateArray(ngate).ID = ID
            gateArray(ngate).inUse=useObj
            gateArray(ngate).name=name
            gateArray(ngate).objConnectedType = ObjConnType
            gateArray(ngate).node=ext2intnode(NodeConn)
	      gateArray(ngate).install_datasource.source_type=const_data
	      gateArray(ngate).install_datasource.indx_ptr=0 !fixme: is this is OK?
	      gateArray(ngate).install_datasource.value=1.            
            ObjConnID=ObjConnID(1:objidlen)
            call locase(ObjConnID)
            if ( (ObjConnType .eq. OBJ_CHANNEL) ) then
               read(ObjConnID,'(I10)')channo
               gateArray(ngate).objConnectedID	=channo
            else if(ObjConnType .eq. OBJ_RESERVOIR) then
               resno=name_to_objno(ObjConnType, objConnID)
               gateArray(ngate).objConnectedID=resno
	         do i=1,res_geom(resno).nnodes
	             if (res_geom(resno).node_no(i) .eq. gateArray(ngate).node) then
	                write(unit_error,627)trim(name),trim(res_geom(resno).name),
     &                  node_geom(gateArray(ngate).node).node_ID
 627	                format('Gate ',a, ' attached from reservoir ', a, ' to node ',
     &                  i5, /'conflicts with a gate or reservoir connection ' /
     &                  'defined between the same reservoir and node. ' /
     &                  'Use a single gate or reservoir connection.')     
                      call exit(1)
	             end if
	         end do
               res_geom(resno).nnodes=res_geom(resno).nnodes+1
               res_geom(resno).node_no(res_geom(resno).nnodes)=gateArray(ngate).node
               res_geom(resno).isNodeGated(res_geom(resno).nnodes)=.true.
	         gateArray(ngate).subLocation=res_geom(resno).nnodes
            else
               write(unit_error,628) name
 628           format(/'Gate ',a,' connected to an object that is not supported')
            end if

            gateArray(ngate).flowDirection=0.D0 ! fixme: depends on location upstream or down.
            if (print_level .ge. 3)
     &           write(unit_screen,'(i5,1x,a,2i10)')
     &           ngate,
     &           trim(gateArray(ngate).name),
     &           gateArray(ngate).ID,LayerID
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

 630  format(/a,i5)
      istat = ngate

      return
      end

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
      use constants
      
      implicit none

c-----arguments
      integer(SQLHANDLE_KIND):: StmtHndl
     &     ,istat               ! status

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

      character*32
     &     name
     &     ,gatename

      
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
            name=name(1:namelen)
            call locase(name)  
            if (iRet .eq. SQL_NO_DATA) exit
            gateArray(gateNo).ndevice=gateArray(gateNo).ndevice+1
	      devNo=gateArray(gateNo).ndevice
            if (devNo .gt. MAX_DEV)then
           ! too many devices at one gate
              write(unit_error, '(/a)')
     &        'Maximum number of weirs exceeded for gate:',
     &         gateArray(gateNo).name
               istat=-3
              return
	      end if

            gateArray(gateNo).devices(devNo).name=name(1:namelen)
            gateArray(gateNo).devices(devNo).structureType=struct_type
            gateArray(gateNo).devices(devNo).controlType=control_type
            gateArray(gateNo).devices(devNo).nduplicate=nduplicate
            gateArray(gateNo).devices(devNo).maxWidth=max_width
            gateArray(gateNo).devices(devNo).baseElev=base_elev
            gateArray(gateNo).devices(devNo).height=height

            gateArray(gateNo).devices(devNo).flowCoefFromNode=CFfrom
            gateArray(gateNo).devices(devNo).flowCoefToNode=CFto
	      
	
		  if( default_op .eq. GATE_CLOSE)then
	         to_op=0.
			 from_op=0.
	      else if(default_op .eq. GATE_OPEN)then
	         to_op=1.
			 from_op=1.
	      else if(default_op .eq. UNIDIR_TO_NODE)then
	         to_op=1.
	         from_op=0.
	      else if(default_op .eq. UNIDIR_FROM_NODE)then
	         to_op=0.
	         from_op=1.0
	      else
		     write (unit_error,"('Unrecognized default operation for gate',1x,
     &                   a,' device ',a)")gatename,name
               istat=-3
	         return
	      end if
	      gateArray(gateNo).devices(devNo).op_to_node_datasource.source_type=const_data
	!fixme: is this next line OK?
	      gateArray(gateNo).devices(devNo).op_to_node_datasource.indx_ptr=0
	      gateArray(gateNo).devices(devNo).op_to_node_datasource.value=to_op

	      gateArray(gateNo).devices(devNo).op_from_node_datasource.source_type=const_data
	      gateArray(gateNo).devices(devNo).op_from_node_datasource.indx_ptr=0    !fixme: is this OK?
	      gateArray(gateNo).devices(devNo).op_from_node_datasource.value=from_op

	      gateArray(gateNo).devices(devNo).width_datasource.source_type=const_data
	      gateArray(gateNo).devices(devNo).width_datasource.indx_ptr=0    !fixme: is this OK?
	      gateArray(gateNo).devices(devNo).width_datasource.value=max_width
	      gateArray(gateNo).devices(devNo).height_datasource.source_type=const_data
	      gateArray(gateNo).devices(devNo).height_datasource.indx_ptr=0    !fixme: is this OK?
	      gateArray(gateNo).devices(devNo).height_datasource.value=height
	      gateArray(gateNo).devices(devNo).elev_datasource.source_type=const_data
	      gateArray(gateNo).devices(devNo).elev_datasource.indx_ptr=0    !fixme: is this OK?
	      gateArray(gateNo).devices(devNo).elev_datasource.value=base_elev

	      gateArray(gateNo).devices(devNo).pos_datasource.source_type=const_data
	      gateArray(gateNo).devices(devNo).pos_datasource.indx_ptr=0 !fixme: is this is OK?
	      gateArray(gateNo).devices(devNo).pos_datasource.value=miss_val_r


	      
	      
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


      subroutine load_obj2obj_SQL(StmtHndl, ModelID, istat)

c-----load f90SQL modules
      use f90SQLConstants
      use f90SQL
      use IO_Units
      use DSM2_database
      use grid_data
      use logging
      use constants
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
     &     ID                   ! transfer ID
     &     ,FromObjType
     &     ,ToObjType
     &     ,name_to_objno

      character
     &     TransName*32         ! transfer name
     &     ,PrevName*32
     &     ,FromObjId*32
     &     ,ToObjID*32

      integer(SQLINTEGER_KIND):: namelen,fromobjlen,toobjlen
      integer ext2intnode

c-----Bind the parameter representing ModelID	
      call f90SQLBindParameter (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_PARAM_INPUT,
     &     SQL_F_SLONG, SQL_INTEGER, int(4,SQLUINTEGER_KIND),  int(0,SQLSMALLINT_KIND),
     &     ModelID, f90SQL_NULL_PTR, iRet) 

c-----Execute SQL statement
      StmtStr="SELECT Transfer_ID, Transfer.used, Transfer.Name, " //
     &     "Transfer.From_Object_Identifier, Transfer.From_Object_Type, " //
     &     "To_Object_Identifier, To_Object_Type " //
     &     "FROM Transfer INNER JOIN Model_Component ON " //
     &     "Transfer.Layer_ID = Model_Component.Component_ID " //
     &     "WHERE Model_Component.Component_Type = 'grid' " //
     &     "AND Model_Component.Model_ID = ? " //
     &     "ORDER BY Transfer.Name, Model_Component.Layer DESC;"

      call f90SQLExecDirect(StmtHndl, StmtStr,iRet)

      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5/)') 'Error in making transfer SQL request',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3) write(unit_screen,'(a)') 'Made transfer SQL request'
      endif

c-----Bind variables to columns in result set
      ColNumber=1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ID,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, UseObj,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, TransName,
     &     loc(namelen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, FromObjID,
     &     loc(fromobjlen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, FromObjType,
     &     f90SQL_NULL_PTR, iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_CHAR, ToObjID,
     &     loc(toobjlen), iRet)

      ColNumber=ColNumber+1
      call f90SQLBindCol(StmtHndl, ColNumber, SQL_F_SLONG, ToObjType,
     &     f90SQL_NULL_PTR, iRet)

      if (print_level .ge. 3) write(unit_screen,'(a)') 'Made transfer bind request'
c-----Loop to fetch records, one at a time
      nobj2obj=0
      prevname=miss_val_c
      do while (.true.)

c--------Fetch a record from the result set

         call f90SQLFetch(StmtHndl,iRet)
         if (iRet .eq. SQL_NO_DATA) exit

         TransName=TransName(1:namelen)
         call locase(TransName)

         FromObjID=FromObjID(1:fromobjlen)
         call locase(FromObjID)

         ToObjID=ToObjID(1:toobjlen)
         call locase(ToObjID)
         

c--------use only the last version of a transfer, and skip
c--------if the transfer is marked as not-use
         if (TransName .ne. prevName .and.
     &        UseObj) then
            nobj2obj=nobj2obj+1
            if (nobj2obj .gt. max_obj2obj) then
               write(unit_error,630)
     &              'Too many object connections specified; max allowed is:'
     &              ,max_obj2obj
               istat=-1
            endif
            obj2obj(nobj2obj).Use=.true.
            obj2obj(nobj2obj).ID=ID
            obj2obj(nobj2obj).name=TransName
            obj2obj(nobj2obj).from_obj.obj_type=FromObjType
            if (FromObjType .eq. obj_reservoir) then
               obj2obj(nobj2obj).from_obj.obj_name=FromObjID
               obj2obj(nobj2obj).from_obj.obj_no=name_to_objno(obj_reservoir,FromObjID)
            else if (FromObjType .eq. obj_node) then
               read(FromObjID,'(i10)')obj2obj(nobj2obj).from_obj.obj_no
               obj2obj(nobj2obj).from_obj.obj_no=
     &           ext2intnode(obj2obj(nobj2obj).from_obj.obj_no)
               obj2obj(nobj2obj).from_obj.obj_name=FromObjID
            end if
            obj2obj(nobj2obj).to_obj.obj_type=ToObjType
            if (ToObjType .eq. obj_reservoir) then
               obj2obj(nobj2obj).to_obj.obj_name=ToObjID
               obj2obj(nobj2obj).to_obj.obj_no=name_to_objno(obj_reservoir,ToObjID)
            else if (ToObjType .eq. obj_node) then
               read(ToObjID,'(i10)')obj2obj(nobj2obj).to_obj.obj_no
               obj2obj(nobj2obj).to_obj.obj_no=
     &              ext2intnode(obj2obj(nobj2obj).to_obj.obj_no)
               obj2obj(nobj2obj).to_obj.obj_name=ToObjID
            end if
            if (print_level .ge. 3)
     &           write(unit_screen,'(i5,a)') nobj2obj,
     &           trim(obj2obj(nobj2obj).name)
         endif
         PrevName=TransName
      enddo

      if (print_level .ge. 2)
     &     write(unit_screen,'(a,i5/)') 'Read in all transfer data',nobj2obj

      call f90SQLFreeStmt(StmtHndl,SQL_UNBIND, iRet)
      call f90SQLCloseCursor (StmtHndl, iRet)
      if (iRet.ne.SQL_SUCCESS) then
         write(unit_error,'(a,i5//)') 'Error in unbinding transfer SQL',iRet
         call ShowDiags(SQL_HANDLE_STMT, StmtHndl)
         istat=-3
         return
      else
         if (print_level .ge. 3)
     &        write(unit_screen,'(a//)') 'Unbound transfer SQL'
      endif

 630  format(/a,i5)

      return
      end



c-----Order nodes in node_geom in a way that is compatible
c-----with hydro and qual. The function also changes 
c-----chan_geom.upnode and chan_geom.downnode from external to internal
      logical function order_nodes()
      use grid_data
      implicit none
      integer nn,n,node
      integer intchan
      integer ext2intnode
      integer compareInt
      external compareInt
      order_nodes=.true.
c     compile list of all nodes and sort them in numerical order

      nn=0

      do intchan=1,nchans
         if (chan_geom(intchan).inUse) then
            nn=nn+1
            nodelist(nn)=chan_geom(intchan).upnode
            nn=nn+1
            nodelist(nn)=chan_geom(intchan).downnode
         end if
      end do
      ! now sort
      call qsort (nodelist(1), nn, int4(4), compareInt) 
      
      node=nn
c     add internal nodes to node_geom, in order
      n=0
      do nn=1,node
         if (nodelist(nn) .ne. miss_val_i .and. ! not junk
     &        nodelist(nn) .ne. node_geom(n).node_ID .and. ! not already done
     &        (nodelist(nn) .eq. nodelist(nn+1) .or. ! internal (repeated in nodelist)
     &        nodelist(nn) .eq. nodelist(nn-1))) then
            n=n+1
            node_geom(n).node_ID=nodelist(nn)
         end if
      end do
      nintnodes=n

c     add external nodes to node_geom, in order
      do nn=1,node
         if (nodelist(nn) .ne. miss_val_i .and. ! not junk
     &        nodelist(nn) .ne. node_geom(n).node_ID .and. ! not already handled
     &        nodelist(nn) .ne. nodelist(nn+1) .and. ! external
     &        nodelist(nn) .ne. nodelist(nn-1)) then
            n=n+1
            node_geom(n).node_ID=nodelist(nn)
         end if
      end do      
      nnodes=n

c-----now repair nodelist to reflect new order
      nodelist=miss_val_i      
      do n=1,nnodes
         nodelist(n)=node_geom(n).node_ID
      end do

c-----add network connectivity of nodes and channels to node_geom and chan_geom
      do intchan=1,nchans
c--------upstream node
         node=ext2intnode(chan_geom(intchan).upnode)
         node_geom(node).nup=node_geom(node).nup+1
         node_geom(node).upstream(node_geom(node).nup)=intchan
         chan_geom(intchan).upnode=node
c--------downstream node
         node=ext2intnode(chan_geom(intchan).downnode)
         node_geom(node).ndown=node_geom(node).ndown+1
         node_geom(node).downstream(node_geom(node).ndown)=intchan
         chan_geom(intchan).downnode=node
      enddo

      return
      end function
