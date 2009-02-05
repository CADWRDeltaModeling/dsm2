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




