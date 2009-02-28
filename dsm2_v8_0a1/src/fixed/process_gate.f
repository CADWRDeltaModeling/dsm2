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
      subroutine process_gate(id,
     &                        useObj,
     &                        name,
     &                        ObjConnType,
     &                        ObjConnID,
     &                        nodeConn)
      use Gates, only: gateArray,nGate, MAX_GATES
      use IO_Units
      use DSM2_database
      use logging
      use constants
      use grid_data
      implicit none
      integer
     &     ID
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

      logical :: useObj
      integer, external :: ext2intnode
          
            ngate=ngate+1
            if (ngate .gt. max_gates) then
               write(unit_error,630)
     &              'Too many gates specified; max allowed is:' ,max_gates
               call exit(-1)
               return
            endif
 630        format(/a,i5)     
            gateArray(ngate).ID = ID
            gateArray(ngate).inUse=useObj
            gateArray(ngate).name=trim(name)
            gateArray(ngate).objConnectedType = ObjConnType
            gateArray(ngate).node=ext2intnode(NodeConn)
	      gateArray(ngate).install_datasource.source_type=const_data
	      gateArray(ngate).install_datasource.indx_ptr=0 !fixme: is this is OK?
	      gateArray(ngate).install_datasource.value=1.            
            ObjConnID=trim(ObjConnID)
            call locase(ObjConnID)           
            if ( (ObjConnType .eq. OBJ_CHANNEL) ) then
               channo=name_to_objno(ObjConnType, objConnID)
               gateArray(ngate).objConnectedID=channo
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
     &           write(unit_screen,'(i5,1x,a,i10)')
     &           ngate,
     &           trim(gateArray(ngate).name),
     &           gateArray(ngate).ID


      return
      end subroutine

c================================================================

      subroutine process_gate_device(
     &                               gatename,
     &                               name,
     &                               struct_type,
     &                               control_type,
     &                               nduplicate,
     &                               max_width,
     &                               base_elev,
     &                               height,
     &                               cffrom,
     &                               cfto,
     &                               default_op)
      use Gates, only: gateArray,maxNGate,
     &     PIPE,WEIR,MAX_DEV,GATE_OPEN,GATE_CLOSE,
     &     UNIDIR_TO_NODE,UNIDIR_FROM_NODE
      use io_units
      use constants
      
      implicit none

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
      
      integer,external :: name_to_objno   

      real*8
     &     max_width
     &     ,base_elev,height
     &     ,CFfrom,CFto
     &     ,from_op,to_op

      character*32
     &     name
     &     ,gatename
            
            gateNo = name_to_objno(obj_gate,gatename)
            
            gateArray(gateNo).ndevice=gateArray(gateNo).ndevice+1
	      devNo=gateArray(gateNo).ndevice
            if (devNo .gt. MAX_DEV)then
           ! too many devices at one gate
              write(unit_error, '(/a)')
     &        'Maximum number of weirs exceeded for gate:',
     &         gateArray(gateNo).name
               call exit(-3)
              return
	      end if
            gateArray(gateNo).devices(devNo).name=trim(name)
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
               call exit(-3)
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




      return
      end subroutine
