      subroutine init_fixed_data(filename)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      character*(*) filename
      integer i
      include 'version.inc'
      
      call read_ptm(filename)
c-----temp
      nnodes=0
      
      do i=1,max_nodes
         if (node_geom(i).nup + node_geom(i).ndown .gt. 0)
     &        nnodes=nnodes+1
      enddo
c----- fill up stage boundary information
      nStageBoundaries = 0
      do i=1,max_nodes
         if ( node_geom(i).boundary_type .eq. stage_type ) then
            nStageBoundaries = nStageBoundaries + 1
            stageBoundary(nStageBoundaries).attach.object = obj_node
            stageBoundary(nStageBoundaries).attach.object_no = i
            stageBoundary(nStageBoundaries).acct_name = 'stage'
         endif
      enddo
c-----endtemp
c----- update node and waterbody fixed information
      call updateNodeInfo()
      call updateWBInfo()
c----- update flux info
      call updateFluxInfo()
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_unique_id_for_channel(localIndex)
      implicit none
      integer localIndex, get_unique_id_for_channel
      get_unique_id_for_channel = localIndex
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_unique_id_for_reservoir(localIndex)
      implicit none
      integer localIndex, get_unique_id_for_reservoir
      integer get_maximum_number_of_channels
      get_unique_id_for_reservoir  = localIndex 
     &     + get_maximum_number_of_channels()
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_unique_id_for_stage_boundary(localIndex)
      implicit none
      integer localIndex, get_unique_id_for_stage_boundary
      integer get_maximum_number_of_channels
     &     , get_maximum_number_of_reservoirs
       get_unique_id_for_stage_boundary = localIndex 
     &     + get_maximum_number_of_channels()
     &     + get_maximum_number_of_reservoirs()
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_unique_id_for_boundary(localIndex)
      implicit none
      integer localIndex, get_unique_id_for_boundary
      integer get_maximum_number_of_channels
     &     , get_maximum_number_of_reservoirs
     &     , get_maximum_number_of_stage_boundaries
      get_unique_id_for_boundary  = localIndex 
     &     + get_maximum_number_of_channels()
     &     + get_maximum_number_of_reservoirs()
     &     + get_maximum_number_of_stage_boundaries()
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_unique_id_for_conveyor(localIndex)
      implicit none
      integer localIndex, get_unique_id_for_conveyor
      integer get_maximum_number_of_channels
     &     , get_maximum_number_of_reservoirs
     &     , get_maximum_number_of_stage_boundaries
     &     , get_maximum_number_of_boundary_waterbodies
      get_unique_id_for_conveyor  = localIndex 
     &     + get_maximum_number_of_channels()
     &     + get_maximum_number_of_reservoirs()
     &     + get_maximum_number_of_stage_boundaries()
     &     + get_maximum_number_of_boundary_waterbodies()
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine updateFluxInfo
      implicit none
      include '../../input/fixed/common.f'
      include '../../input/fixed/common_ptm.inc'
      include '../../input/time-varying/common_tide.f'
      include 'ptmLocal.inc'
      integer getWaterbodyUniqueId, getStageWaterbodyForNode
      integer i, j, id, uniqId
      byte wbtype
c-----
      nFlux = noutpaths-ngroups
      do i=1, noutpaths
         if (pathoutput(i).meas_type .eq. 'ptm_flux       ') then
            j=1
            wbType = pathoutput(i).flux_from(j).object
            do while( wbType .ne. 0 )
               flux(i).typeInArray(j) = wbType
               if ( pathoutput(i).flux_from(j).acct_ndx .gt. 0 ) then
                  flux(i).accountTypeInArray(j) =
     &                 pathoutput(i).flux_from(j).acct_ndx
               else
                  id = pathoutput(i).flux_from(j).object_no
                  if ( id .ne. 0 ) then
                     if ( wbType .eq. 8 ) then
                        uniqId = getStageWaterbodyForNode(id)
                     else
                        uniqId = getWaterbodyUniqueId(wbType, id)
                     endif
                     flux(i).inArray(j) = uniqId
                  else
                     flux(i).accountTypeInArray(j) = alltypes ! key for 'all'
                  endif
               endif
               j=j+1
               wbType = pathoutput(i).flux_from(j).object
            enddo
            flux(i).numberIncoming = j-1

            j=1
            wbType = pathoutput(i).flux_to(j).object
            do while( wbType .ne. 0 )
               flux(i).typeOutArray(j) = wbType
               if ( pathoutput(i).flux_to(j).acct_ndx .gt. 0 ) then
                  flux(i).accountTypeOutArray(j) =
     &                 pathoutput(i).flux_to(j).acct_ndx
               else
                  id = pathoutput(i).flux_to(j).object_no
                  if ( id .ne. 0 ) then
                     if ( wbType .eq. 8 ) then
                        uniqId = getStageWaterbodyForNode(id)
                     else
                        uniqId = getWaterbodyUniqueId(wbType, id)
                     endif
                     flux(i).outArray(j) = uniqId
                  else
                     flux(i).accountTypeOutArray(j) = alltypes ! key for 'all'
                  endif
               endif
               j=j+1
               wbType = pathoutput(i).flux_to(j).object
            enddo
            flux(i).numberOutgoing = j-1
         endif
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine updateWBInfo
      implicit none
      include '../../input/fixed/common.f'
      include '../../input/fixed/common_ptm.inc'
      include '../../input/time-varying/tide.inc'
      include 'ptmLocal.inc'
c----- functions
      integer get_unique_id_for_channel
     &     , get_unique_id_for_reservoir
     &     , get_unique_id_for_stage_boundary
     &     , get_unique_id_for_boundary
     &     , get_unique_id_for_conveyor
      
      integer get_maximum_number_of_channels
     &     , get_maximum_number_of_reservoirs
     &     , get_maximum_number_of_stage_boundaries
     &     , get_maximum_number_of_boundary_waterbodies
     &     , get_maximum_number_of_conveyors
      integer get_internal_node_id_for_unique_ids
c----- locals
      integer i,j,id, numNodes, objId

c----- begin
c----- update channel info
      do i=1, get_maximum_number_of_channels()
         id = get_unique_id_for_channel(i)
         wb(id).type = obj_channel
         wb(id).localIndex = i
         wb(id).globalIndex = id
         wb(id).numberOfNodes = 2
         wb(id).group = 0
         wb(id).node(1) = chan_geom(i).upnode
         wb(id).node(2) = chan_geom(i).downnode
      enddo
c----- update reservoir info
      do i=1, get_maximum_number_of_reservoirs()
         id = get_unique_id_for_reservoir(i)
         wb(id).type = obj_reservoir
         wb(id).localIndex = i
         wb(id).globalIndex = id
         wb(id).numberOfNodes = res_geom(i).nnodes
         wb(id).group = 0
         do j=1, res_geom(i).nnodes
            wb(id).node(j) = res_geom(i).node_no(j)
         enddo
         j=1
         do while(res_geom(i).qint(j) .ne. 0)
            wb(id).numberOfNodes = wb(id).numberOfNodes + 1
            numNodes = wb(id).numberOfNodes
            objId = res_geom(i).qint(j)
            objId = get_unique_id_for_conveyor( objId )
            wb(id).node(numNodes) = 
     &           get_internal_node_id_for_unique_ids( id, objId )
            j = j + 1
         enddo
         j=1
         do while(res_geom(i).qext(j) .ne. 0)
            wb(id).numberOfNodes = wb(id).numberOfNodes + 1
            numNodes = wb(id).numberOfNodes
            objId = res_geom(i).qext(j)
            objId = get_unique_id_for_boundary( objId )
            wb(id).node(numNodes) = 
     &           get_internal_node_id_for_unique_ids( id, objId )
            j = j + 1
         enddo
      enddo
c----- update stage boundary info 
      do i=1, get_maximum_number_of_stage_boundaries()
         id = get_unique_id_for_stage_boundary(i)
         wb(id).type = obj_qext
         wb(id).localIndex = i
         wb(id).globalIndex = id
         wb(id).numberOfNodes = 1
         wb(id).group = 0
         wb(id).node(1) = stageBoundary(i).attach.object_no
      enddo
c----- update boundary info
      do i=1, get_maximum_number_of_boundary_waterbodies()
         id = get_unique_id_for_boundary(i)
         wb(id).type = obj_qext
         wb(id).acctType = qext(i).acct_ndx
         wb(id).localIndex = i
         wb(id).globalIndex = id
         wb(id).numberOfNodes = 1
         wb(id).group = 0
         if ( qext(i).attach.object .eq. obj_node ) then
            wb(id).node(1) = qext(i).attach.object_no
         else if ( qext(i).attach.object .eq. obj_reservoir ) then
            objId = qext(i).attach.object_no
            objId = get_unique_id_for_reservoir( objId )
            wb(id).node(1) = get_internal_node_id_for_unique_ids( 
     &           objId
     &           , id )
         else
c-----------write(*,*) ' External types connection to type: ' ,
c-----------&           qext(i).object, ' not handled '
         endif
      enddo
c----- update conveyor info
      do i=1, get_maximum_number_of_conveyors()
         id = get_unique_id_for_conveyor(i)
         wb(id).type = obj_obj2obj
         wb(id).acctType = obj2obj(i).from.acct_ndx
         wb(id).localIndex = i
         wb(id).globalIndex = id
         wb(id).numberOfNodes = 2
         wb(id).group = 0
         if ( obj2obj(i).from.object .eq. obj_node ) then
            wb(id).node(1) = obj2obj(i).from.object_no
         else if ( obj2obj(i).from.object .eq. obj_reservoir ) then
            objId = obj2obj(i).from.object_no
            objId = get_unique_id_for_reservoir(objId)
            wb(id).node(1) = get_internal_node_id_for_unique_ids( 
     &           objId
     &           , id )
         else
c-----------write(*,*) ' Internal types connection from type: ' ,
c-----------&           obj2obj(i).from.object, ' not handled '
         endif
         if ( obj2obj(i).to.object .eq. obj_node ) then
            wb(id).node(2) = obj2obj(i).to.object_no
         else if ( obj2obj(i).to.object .eq. obj_reservoir ) then
            objId = obj2obj(i).to.object_no
            objId = get_unique_id_for_reservoir(objId)
            wb(id).node(2) = get_internal_node_id_for_unique_ids( 
     &           objId
     &           , id )
         else
c-----------write(*,*) ' Internal types connection to type: ' ,
c-----------&           obj2obj(i).to.object, ' not handled '
         endif
      enddo
c----- update group info
      j=1
      do while (group_areas(j).group .ne. 0)
c      do i=1, get_maximum_number_of_group_elements()
         if ( group_areas(j).object .eq. 'chan' ) then         
            id = get_unique_id_for_channel(group_areas(j).number)
            if ( wb(id).group .ne. 0 ) write(unit_screen, 590) id
c            wb(id).group = group_areas(j).group
            wb(id).group = revgrp(group_areas(j).group)
         elseif ( group_areas(j).object .eq. 'res' ) then
            id = get_unique_id_for_reservoir(group_areas(j).number)
            if ( wb(id).group .ne. 0 ) write(unit_screen, 590) id
c            wb(id).group = group_areas(j).group
            wb(id).group = revgrp(group_areas(j).group)
         else
            write(*,*) ' Internal types connection to type: ' ,
     &           group_areas(j).group, ' not handled '
         endif
         j = j + 1
      enddo
 590  format('Warning -  Waterbody ',i4,' defined for more than one group!'/)
c----- end
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine updateNodeInfo
      implicit none
      include '../../input/fixed/common.f'
      include '../../input/time-varying/tide.inc'
      include 'ptmLocal.inc'
      integer i, j, k, nodeId, nUp, nDown, conveyorId, nnId, objId, qId
      integer get_unique_id_for_reservoir,
     &     get_unique_id_for_boundary, get_unique_id_for_conveyor,
     &     get_unique_id_for_stage_boundary
      integer get_maximum_number_of_reservoirs
c----- get node geom info
      nodeId = 0
      do i=1, max_nodes
         nodeId=nodeId+1
c-------- channel info at node
         if ( node_geom(i).nup + node_geom(i).ndown .gt. 0) then
            nodes(nodeId).id = nodeId
            nUp = node_geom(i).nup
            nDown = node_geom(i).ndown
            nodes(nodeId).nwbs = nUp + nDown
            do j=1,nUp
               nodes(nodeId).wbs(j) = node_geom(i).upstream(j)
            enddo
            do j=nUp+1, nUp + nDown
               nodes(nodeId).wbs(j) = node_geom(i).downstream(j-nUp)
            enddo
         endif
c-------- add external flows at node
         j=1
         do while(node_geom(i).qext(j) .gt. 0 .and. j .le. max_qobj )
            nodes(i).nwbs = nodes(i).nwbs+1
            qId = node_geom(i).qext(j)
            nodes(i).wbs(nodes(i).nwbs) = 
     &           get_unique_id_for_boundary(qId)
            j = j + 1
         enddo
c-------- add internal flows at node
         j=1
         do while(node_geom(i).qint(j) .gt. 0 .and. j .le. max_qobj )
            nodes(i).nwbs = nodes(i).nwbs+1
            qId = node_geom(i).qint(j)
            nodes(i).wbs(nodes(i).nwbs) = 
     &           get_unique_id_for_conveyor(qId)
            j = j + 1
         enddo
      enddo                     !end loop for node_geom structure
c----- add reservoirs
      do j=1, get_maximum_number_of_reservoirs()
         do k=1, res_geom(j).nnodes
            nnId = res_geom(j).node_no(k)
            nodes(nnId).nwbs = nodes(nnId).nwbs+1
            nodes(nnId).wbs(nodes(nnId).nwbs) = 
     &           get_unique_id_for_reservoir(j)
         enddo
      enddo
c-------- add stage boundaries
      do j=1, nStageBoundaries
         nnId = stageBoundary(j).attach.object_no
         nodes(nnId).nwbs = nodes(nnId).nwbs + 1
         nodes(nnId).wbs(nodes(nnId).nwbs) = 
     &        get_unique_id_for_stage_boundary(j)
      enddo
c-----create internal nodes info. These are connections between
c-----waterbodies not explicitly connected through nodes. 
c-----check external flows connected to a waterbody (ie. not a node)
      do i=1, nqext
         if ( qext(i).attach.object .eq. obj_channel ) then
            nodeId = nodeId + 1
            nodes(nodeId).id = nodeId
            nodes(nodeId).nwbs = 2
            nodes(nodeId).wbs(1) = qext(i).attach.object_no
            nodes(nodeId).wbs(2) = get_unique_id_for_boundary(i)
         elseif( qext(i).attach.object .eq. obj_reservoir ) then
            nodeId = nodeId + 1
            nodes(nodeId).id = nodeId
            nodes(nodeId).nwbs = 2
            objId = qext(i).attach.object_no
            nodes(nodeId).wbs(1) = 
     &           get_unique_id_for_reservoir(objId)
            nodes(nodeId).wbs(2) = get_unique_id_for_boundary(i)
c----------- add nodes to reservoir if not present
c-----------add_node_to_reservoir(nodeId)
         else if( qext(i).attach.object .eq. obj_node ) then
!             nnId = qext(i).object_no 
!             nodes(nnId).nwbs = nodes(nnId).nwbs + 1
!             nodes(nnId).wbs(nodes(nnId).nwbs) = 
!      &           get_unique_id_for_boundary(i)
         else
                                ! do nothing
         endif
      enddo
c-----check internal flows connected between waterbodies ( only reservoirs
c-----for now )
      conveyorId = 0
      do i=1, nobj2obj
c--------get global unique id for this internal flow
         conveyorId = get_unique_id_for_conveyor(i)
c--------from object
         if( obj2obj(i).from.object .eq. obj_reservoir) then
            nodeId = nodeId+1
            nodes(nodeId).id = nodeId
            objId = obj2obj(i).from.object_no
            nodes(nodeId).nwbs = 2
            nodes(nodeId).wbs(1) = 
     &           get_unique_id_for_reservoir(objId)
            nodes(nodeId).wbs(2) = conveyorId
c----------- add nodes to reservoir if not present
c-----------add_node_to_reservoir(nodeId)
         else if( obj2obj(i).from.object .eq. obj_node ) then
!             nnId = obj2obj(i).from.object_no 
!             nodes(nnId).nwbs = nodes(nnId).nwbs + 1
!             nodes(nnId).wbs(nodes(nnId).nwbs) = 
!      &           get_unique_id_for_conveyor(i)
         endif
c--------to object
         if( obj2obj(i).to.object .eq. obj_reservoir) then
            nodeId = nodeId+1
            nodes(nodeId).id = nodeId
            nodes(nodeId).nwbs = 2
            objId = obj2obj(i).to.object_no
            nodes(nodeId).wbs(1) =  
     &           get_unique_id_for_reservoir(objId)
            nodes(nodeId).wbs(2) = conveyorId
         else if( obj2obj(i).to.object .eq. obj_node ) then
!             nnId = obj2obj(i).to.object_no 
!             nodes(nnId).nwbs = nodes(nnId).nwbs + 1
!             nodes(nnId).wbs(nodes(nnId).nwbs) = 
!      &           get_unique_id_for_conveyor(i)
         endif
      enddo
      return 
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_waterbodies()
      implicit none
      integer get_number_of_waterbodies
      integer get_number_of_channels
      integer get_number_of_reservoirs
      integer get_number_of_boundary_waterbodies
      integer get_number_of_stage_boundaries
      integer get_number_of_conveyors
      include '../../input/fixed/common.f'
      get_number_of_waterbodies = 
     &     get_number_of_channels() 
     &     + get_number_of_reservoirs()
     &     + get_number_of_stage_boundaries()
     &     + get_number_of_boundary_waterbodies()
     &     + get_number_of_conveyors()
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_channels()
      implicit none
      integer get_number_of_channels
      include '../../input/fixed/common.f'
      get_number_of_channels = nchans
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_reservoirs()
      implicit none
      integer get_number_of_reservoirs
      include '../../input/fixed/common.f'
      get_number_of_reservoirs = nreser
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_nodes()
      implicit none
      integer get_number_of_nodes
c-----integer get_maximum_number_of_pumps
      include '../../input/fixed/common.f'
      include '../../input/time-varying/tide.inc'
      integer i
      get_number_of_nodes = nnodes 
c----- create internal nodes for object to object flows
c----- which are not nodes.
      do i=1,nqext
         if(qext(i).attach.object .ne. obj_node) then
            get_number_of_nodes = get_number_of_nodes + 1
         endif
      enddo
c----- do the same for internal flows or conveyors
      do i=1,nobj2obj
         if(obj2obj(i).from.object .ne. obj_node) then
            get_number_of_nodes = get_number_of_nodes + 1
         endif
         if(obj2obj(i).to.object .ne. obj_node) then
            get_number_of_nodes = get_number_of_nodes + 1
         endif
      enddo
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_xsections()
      implicit none
      integer get_number_of_xsections
      include '../../input/fixed/common.f'
      get_number_of_xsections = nxsects
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_diversions()
      implicit none
      integer get_number_of_diversions
      include '../../input/fixed/common.f'
      include '../../input/time-varying/tide.inc'
c-----Number of diversions = 0
      get_number_of_diversions = 0
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_pumps()
      implicit none
      integer get_number_of_pumps
      include '../../input/fixed/common.f'
      include '../../input/time-varying/tide.inc'
c-----pumping from a reservoir is the same as a diversion?
      get_number_of_pumps = 0
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_conveyors()
      implicit none
      integer get_number_of_conveyors
      include '../../input/fixed/common.f'
c-----internal flows
      get_number_of_conveyors = nobj2obj
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_boundary_waterbodies()
      implicit none
      integer get_number_of_boundary_waterbodies
      include '../../input/fixed/common.f'
      include '../../input/time-varying/tide.inc'
      include 'ptmLocal.inc'
      get_number_of_boundary_waterbodies = nqext
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_stage_boundaries()
      implicit none
      integer get_number_of_stage_boundaries
      include '../../input/fixed/common.f'
      include '../../input/time-varying/tide.inc'
      include 'ptmLocal.inc'
      get_number_of_stage_boundaries = nStageBoundaries
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_channel_groups()
      implicit none
      integer get_number_of_channel_groups
      include '../../input/fixed/common.f'
      include '../../input/fixed/common_ptm.inc'
      include '../../input/time-varying/tide.inc'
      include 'ptmLocal.inc'
      get_number_of_channel_groups = ngroups
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_waterbodies()
      implicit none
      integer get_maximum_number_of_waterbodies
      integer get_maximum_number_of_channels
      integer get_maximum_number_of_reservoirs
      integer get_maximum_number_of_diversions
      integer get_maximum_number_of_pumps
      integer get_maximum_number_of_boundary_waterbodies
      integer get_maximum_number_of_stage_boundaries
      integer get_maximum_number_of_conveyors

      get_maximum_number_of_waterbodies = 
     &     get_maximum_number_of_channels() +
     &     get_maximum_number_of_reservoirs() +
     &     get_maximum_number_of_diversions() +
     &     get_maximum_number_of_pumps() +
     &     get_maximum_number_of_stage_boundaries() +
     &     get_maximum_number_of_boundary_waterbodies() +
     &     get_maximum_number_of_conveyors()
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_channels()
      implicit none
      integer get_maximum_number_of_channels
      include '../../input/fixed/common.f'
      get_maximum_number_of_channels = max_channels
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_reservoirs()
      implicit none
      integer get_maximum_number_of_reservoirs
      include '../../input/fixed/common.f'
      get_maximum_number_of_reservoirs = max_reservoirs
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_conveyors()
      implicit none
      integer get_maximum_number_of_conveyors
      include '../../input/fixed/common.f'
      get_maximum_number_of_conveyors = max_obj2obj
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_nodes()
      implicit none
      integer get_maximum_number_of_nodes
c-----integer get_maximum_number_of_pumps
      integer get_maximum_number_of_conveyors
      include '../../input/fixed/common.f'
      get_maximum_number_of_nodes = max_nodes 
     &     + 2*get_maximum_number_of_conveyors()
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_xsections()
      implicit none
      integer get_maximum_number_of_xsections
      include '../../input/fixed/common.f'
      get_maximum_number_of_xsections = max_xsects_tot
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_reservoir_nodes()
      implicit none
      integer get_maximum_number_of_reservoir_nodes
      include '../../input/fixed/common.f'
      get_maximum_number_of_reservoir_nodes = maxresnodes
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_diversions()
      implicit none
      integer get_maximum_number_of_diversions
      include '../../input/fixed/common.f'
c      get_maximum_number_of_diversions = max_nodes
      get_maximum_number_of_diversions = 0
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_pumps()
      implicit none
      integer get_maximum_number_of_pumps
      include '../../input/fixed/common.f'
c      get_maximum_number_of_pumps = max_reservoirs
      get_maximum_number_of_pumps = 0
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_boundary_waterbodies()
      implicit none
      integer get_maximum_number_of_boundary_waterbodies
      include '../../input/fixed/common.f'
      include '../../input/time-varying/tide.inc'
      get_maximum_number_of_boundary_waterbodies = max_qext
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_stage_boundaries()
      implicit none
      integer get_maximum_number_of_stage_boundaries
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      get_maximum_number_of_stage_boundaries = maxStageBoundaries
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_maximum_number_of_group_elements()
      implicit none
      integer get_maximum_number_of_group_elements
      include '../../input/fixed/common_ptm.inc'
      get_maximum_number_of_group_elements = max_chanres
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_channel_length(i)
      implicit none
      integer get_channel_length
      include '../../input/fixed/common.f'
      integer i
      get_channel_length = chan_geom(i).length
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_channel_number_of_nodes(i)
      implicit none
      include '../../input/fixed/common.f'
      integer get_channel_number_of_nodes
      integer i
      get_channel_number_of_nodes = 2
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_channel_number_of_xsections(i)
      implicit none
      include '../../input/fixed/common.f'
      integer get_channel_number_of_xsections
      integer i
      get_channel_number_of_xsections = chan_geom(i).nxsect
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_channel_node_array(i, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      integer nodeArray(50)
      integer i
      nodeArray(1) = chan_geom(i).upnode
      nodeArray(2) = chan_geom(i).downnode
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_channel_xsection_ids(i, xSectionIds)
      implicit none
      include '../../input/fixed/common.f'
      integer xSectionIds(50)
      integer get_channel_number_of_xsections
      integer i
      integer xid, nXs
      nXs = get_channel_number_of_xsections(i)
      do xid=1,nXs
         xSectionIds(xid) = chan_geom(i).xsect(xid)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_channel_xsection_distances(i, xSectionDistances)
      implicit none
      include '../../input/fixed/common.f'
      real xSectionDistances(50)
      integer i, get_channel_number_of_xsections
      integer xid, nXs
      nXs = get_channel_number_of_xsections(i)
      do xid=1,nXs
         xSectionDistances(xid) = chan_geom(i).xsect(xid)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_reservoir_area(reservoirNumber)
      implicit none
      include '../../input/fixed/common.f'
      real get_reservoir_area
      integer reservoirNumber
      get_reservoir_area = res_geom(reservoirNumber).area
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_reservoir_bottom_elevation(reservoirNumber)
      implicit none
      include '../../input/fixed/common.f'
      real get_reservoir_bottom_elevation
      integer reservoirNumber
      get_reservoir_bottom_elevation = 
     &     res_geom(reservoirNumber).botelv
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_reservoir_name(reservoirNumber, name)
      implicit none
      include '../../input/fixed/common.f'
      character*(*) name
      integer reservoirNumber, lastNonBlank
      integer lnblnk
      name = res_geom(reservoirNumber).name
      lastNonBlank = lnblnk(res_geom(reservoirNumber).name)
      name = name(1:lastNonBlank) // char(0)
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_reservoir_number_of_nodes(reservoirNumber)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer reservoirNumber,get_reservoir_number_of_nodes
      integer uniqId, get_unique_id_for_reservoir
      uniqId = get_unique_id_for_reservoir(reservoirNumber)
      get_reservoir_number_of_nodes = wb(uniqId).numberOfNodes
      return
      end
!       get_reservoir_number_of_nodes = 
!      &     res_geom(reservoirNumber).nnodes
! c----- add external flow connections
!       i = 0
!       do while( res_geom(reservoirNumber).qext(i) .ne. 0 
!      &     .and. i .le. max_qobj )
!          get_reservoir_number_of_nodes = 
!      &        get_reservoir_number_of_nodes + 1  
!          i = i + 1
!       enddo
! c----- add internal flow connection nodes
!       i=0
!       do while( res_geom(reservoirNumber).qint(i) .ne. 0 
!      &     .and. i .le. max_qobj )
!          get_reservoir_number_of_nodes = 
!      &        get_reservoir_number_of_nodes + 1  
!          i = i + 1
!       enddo
!       return 
!       end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_reservoir_node_array(reservoirNumber, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer reservoirNumber, nodeArray(50)
      integer i, uniqId, get_unique_id_for_reservoir
      uniqId = get_unique_id_for_reservoir(reservoirNumber)
      do i = 1, wb(uniqId).numberOfNodes
         nodeArray(i) = wb(uniqId).node(i)
      enddo
      return 
      end
! c----- number of nodes ( for a check )
!       nNodes = get_reservoir_number_of_nodes(reservoirNumber)
! c----- add nodes from reservoir structure
!       do nodeId=1,res_geom(reservoirNumber).nnodes
!          nodeArray(nodeId) = res_geom(reservoirNumber).node_no(i)
!       enddo
! c----- add nodes for external flows
!       i=0
!       do while( res_geom(reservoirNumber).qext(i) .ne. 0 ) 
!          nodeId = nodeId + 1
!          qId = res_geom(reservoirNumber).qext(i)
!          extId = 
!      &        get_unique_id_for_boundary( qId )
!          resId = 
!      &        get_unique_id_for_reservoir(reservoirNumber)
!          nodeArray(nodeId) = 
!      &        get_internal_node_id_for_unique_ids(extId, resId)
!          i = i + 1
!       enddo
! c----- add nodes for internal flows
!       i=0
!       do while( res_geom(reservoirNumber).qint(i) .ne. 0 ) 
!          nodeId = nodeId + 1
!          qId = res_geom(reservoirNumber).qint(i)
!          extId = 
!      &        get_unique_id_for_conveyor( qId )
!          resId = 
!      &        get_unique_id_for_reservoir(reservoirNumber)
!          nodeArray(nodeId) = 
!      &        get_internal_node_id_for_unique_ids(extId, resId)
!          i = i + 1
!       enddo
! c----- check that nodeId matches number of nodes
!       if ( nodeId .ne. nNodes ) then
!          write(*,*) 'Warning: # nodes in reservoir', reservoirNumber,
!      &        ' dont match those calculated in updateNode function'
!       endif
!       return 
!       end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_internal_node_id_for_unique_ids( id1, id2 )
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer get_internal_node_id_for_unique_ids
      integer id1, id2
      integer id
      logical found
      found = .false.
      id = max_nodes+1
      do while( .not. found .and. id .le. maxNodes)
         if ( nodes(id).wbs(1) .eq. id1 .and. nodes(id).wbs(2) .eq. id2)
     &        found = .true.
         id = id + 1
      enddo
      id = id - 1
      get_internal_node_id_for_unique_ids = id
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_diversion_number_of_nodes(diversionNumber)
      implicit none
      include '../../input/fixed/common.f'
      integer diversionNumber,get_diversion_number_of_nodes
      if ( diversionNumber .le. max_nodes) then
         if ( node_geom(diversionNumber).nup +  
     &        node_geom(diversionNumber).ndown .gt. 0 ) then 
            get_diversion_number_of_nodes = 1
         else
            get_diversion_number_of_nodes = -1
         endif
      else
         get_diversion_number_of_nodes = 1
      endif
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_diversion_node_array(diversionNumber, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      integer get_diversion_number_of_nodes
      integer diversionNumber, nodeArray(50)
      integer i, nn
      nn = get_diversion_number_of_nodes(diversionNumber)
      do i=1,nn
         nodeArray(i) = diversionNumber
      enddo
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_pump_number_of_nodes(pumpNumber)
      implicit none
      include '../../input/fixed/common.f'
      integer pumpNumber,get_pump_number_of_nodes
      if(res_geom(pumpNumber).area .gt. 0.0) then
         get_pump_number_of_nodes = 1
      else
         get_pump_number_of_nodes = -1
      endif

      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_pump_node_array(pumpNumber, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      integer get_pump_number_of_nodes, get_maximum_number_of_diversions
      integer pumpNumber, nodeArray(50)
      integer i, nn
      nn = get_pump_number_of_nodes(pumpNumber)
      do i=1,nn
         nodeArray(i) = 
     &        get_maximum_number_of_diversions() + pumpNumber
      enddo
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_boundary_waterbody_number_of_nodes(Number)
      implicit none
      integer Number,get_number_of_boundary_waterbodies,
     &     get_boundary_waterbody_number_of_nodes
      if (Number .lt. get_number_of_boundary_waterbodies()) then
         get_boundary_waterbody_number_of_nodes = 1
      else
         get_boundary_waterbody_number_of_nodes = 0
      endif
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_boundary_waterbody_node_array(
     &     number, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer get_unique_id_for_boundary
      integer number, nodeArray(50)
      integer i, uniqId
c----- check with nodes to get waterbody with unique matching id
      uniqId = get_unique_id_for_boundary(number)
      do i=1, wb(uniqId).numberOfNodes
         nodeArray(i) = wb(uniqId).node(i)
      enddo
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_stage_boundary_number_of_nodes(Number)
      implicit none
      integer Number,get_number_of_stage_boundaries,
     &     get_stage_boundary_number_of_nodes
      if (Number .lt. get_number_of_stage_boundaries()) then
         get_stage_boundary_number_of_nodes = 1
      else
         get_stage_boundary_number_of_nodes = -1
      endif
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_stage_boundary_node_array(
     &     number, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer get_stage_boundary_number_of_nodes
      integer number, nodeArray(50)
      nNodes = 
     &     get_stage_boundary_number_of_nodes(
     &     number)
      if ( nNodes .ne. 1 ) then
         nodeArray(1) = 0
         return
      endif
      nodeArray(1) = stageBoundary(number).attach.object_no
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_conveyor_number_of_nodes(number)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer number, get_conveyor_number_of_nodes
      integer get_unique_id_for_conveyor,id
      id = get_unique_id_for_conveyor(number)
      get_conveyor_number_of_nodes = wb(id).numberOfNodes
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_conveyor_node_array(
     &     conveyorNumber, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer conveyorNumber
c-----integer get_node_for_conveyor
      integer nodeArray(50), id
      integer get_unique_id_for_conveyor, numberOfNodes, i
      id = get_unique_id_for_conveyor(conveyorNumber)
      numberOfNodes = wb(id).numberOfNodes
      if ( numberOfNodes .eq. 2 ) then
         do i=1, wb(id).numberOfNodes
            nodeArray(i) = wb(id).node(i)
         enddo
      endif
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
!       function get_node_for_boundary_waterbody(index)
!       implicit none
!       integer get_node_for_boundary_waterbody, index
!       include '../../input/fixed/common.f'
!       include '../../input/time-varying/tide.inc'
!       include 'ptmLocal.inc'
!       integer i, nBoundary
!       i=1
!       if ( index .le. nqext ) then
!          if ( qext(index).object .eq. obj_node ) then
!             get_node_for_boundary_waterbody = qext(index).object_no
!          else
!             get_node_for_boundary_waterbody = 0
!          endif
!       else ! node with stage boundary type
!       endif

!       nBoundary = 0
!       get_node_for_boundary_waterbody = -1
!       do while ( i .lt. transNumber .and. nBoundary .ne. index) 
!          if ( translationInfo(i).type .eq. obj_qext) then
!             nBoundary = nBoundary + 1
!          endif
!          i = i + 1
!       enddo
!       if (nBoundary .eq. index) then
!         get_node_for_boundary_waterbody = 
!      &        translationInfo(i-1).nodeNumber
!       endif
!       return
!       end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_waterbodies_for_node(nodeNumber)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer get_number_of_waterbodies_for_node
      integer nodeNumber
      get_number_of_waterbodies_for_node = nodes(nodeNumber).nwbs
      return 
      end
!       if (nodeNumber .le. max_nodes) then
!          get_number_of_waterbodies_for_node = 
!      &        node_geom(nodeNumber).nup + node_geom(nodeNumber).ndown
! !     &        + 1
!          numberOfBoundaries = 0
! !          do i=1, get_number_of_boundary_waterbodies()
! !             if (nodeNumber .eq. get_node_for_boundary_waterbody(i)) then
! !                numberOfBoundaries = numberOfBoundaries + 1
! !             endif
! !          enddo
! !          get_number_of_waterbodies_for_node = 
! !      &        get_number_of_waterbodies_for_node + numberOfBoundaries
!       else
!          nn = nodeNumber - max_nodes
!          if( nn .eq. )
! !         get_number_of_waterbodies_for_node = 2
!       endif
!       return 
!       end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_boundary_type_for_node(nodeNumber, name)
      implicit none
      include '../../input/fixed/common.f'
      character*(*) name
      integer nodeNumber, lastNonBlank
c-----integer lnblnk
c-----lastNonBlank = lnblnk(node_geom(nodeNumber).boundary_type)
      name=''
      lastNonBlank=1
      if ( nodeNumber .le. max_nodes) then
         if (node_geom(nodeNumber).boundary_type .eq. stage_type ) then
            name = 'STAGE'
         end if
      endif
      name = name(1:lastNonBlank) // char(0)
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_waterbody_id_array_for_node(nodeNumber, array)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer nodeNumber, array(50)
      integer i
      if ( nodeNumber .gt. maxNodes) goto 999
      do i=1, nodes(nodeNumber).nwbs
         array(i) = nodes(nodeNumber).wbs(i)
      enddo
 999  return 
      end
!       if(nodeNumber .le. max_nodes) then 
!          do i=1, node_geom(nodeNumber).nup
!             array(i) = node_geom(nodeNumber).upstream(i)
!          enddo
!          do i=1,node_geom(nodeNumber).ndown
!             array(i+node_geom(nodeNumber).nup) 
!      &           = node_geom(nodeNumber).downstream(i)
!          enddo
! !         array(node_geom(nodeNumber).ndown + 1) = nodeNumber
!       else
!          array(1) = nodeNumber - max_nodes 
!      &        + get_maximum_number_of_channels()
!          array(2) = nodeNumber - max_nodes 
!      &        + get_maximum_number_of_channels()
!      &        + get_maximum_number_of_reservoirs()
!      &        + get_maximum_number_of_diversions()
!       endif
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_xsection_number_of_elevations()
      implicit none
      include '../../input/fixed/common.f'
      integer get_xsection_number_of_elevations
      get_xsection_number_of_elevations = 2 ! regular sections yet
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_xsection_widths(number, array)
      implicit none
      include '../../input/fixed/common.f'
      integer number
      real array(50)
      array(1) = xsect_geom(number).width ! regular sections
      array(2) = xsect_geom(number).width ! regular sections
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine get_xsection_elevations(number, array)
      implicit none
      include '../../input/fixed/common.f'
      integer number
      real array(50)
      array(1) = xsect_geom(number).botelv
      array(2) = 100
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_xsection_areas(number, array)
      implicit none
      include '../../input/fixed/common.f'
      integer number
      real array(50)
      array(1) = -1
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_xsection_minimum_elevation(number)
      implicit none
      include '../../input/fixed/common.f'
      real get_xsection_minimum_elevation
      integer number
      get_xsection_minimum_elevation = xsect_geom(number).botelv
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_particle_boolean_inputs(array)
      implicit none
      include '../../input/fixed/common_ptm.inc'
      integer array(50)
      array(1)=ptm_ivert
      array(2)=ptm_itrans
      array(3)=ptm_iey
      array(4)=ptm_iez
      array(5)=ptm_iprof
      array(6)=ptm_igroup
      array(7)=ptm_flux_percent
      array(8)=ptm_group_percent
      array(9)=ptm_flux_cumulative
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_particle_float_inputs(array)
      implicit none
      include '../../input/fixed/common_ptm.inc'
      real array(50)
      array(1)=ptm_random_seed
      array(2)=ptm_trans_constant
      array(3)=ptm_vert_constant
      array(4)=ptm_trans_a_coef
      array(5)=ptm_trans_b_coef
      array(6)=ptm_trans_c_coef
      array(7)=ptm_no_animated
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_particle_number_of_injections()
      implicit none
      include '../../input/fixed/common_ptm.inc'
      integer get_particle_number_of_injections
      get_particle_number_of_injections = npartno
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_particle_injection_nodes(array)
      implicit none
      include '../../input/fixed/common_ptm.inc'
      integer array(50)
      integer i
      if (npartno .gt. 50) 
     &     write(*,*) 'Extend LEN1 in fixedData.h to ', npartno 
      do i=1,npartno
         array(i) = part_injection(i).node
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_particle_number_of_particles_injected(array)
      implicit none
      include '../../input/fixed/common_ptm.inc'
      integer array(50)
      integer i
      if (npartno .gt. 50) 
     &     write(*,*) 'Extend LEN1 in fixedData.h to ', npartno 
      do i=1,npartno
         array(i) = part_injection(i).nparts
      enddo  
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_particle_injection_start_julmin(array)
      implicit none
      include '../../input/fixed/common_ptm.inc'
      integer array(50)
      integer i
      if (npartno .gt. 50) 
     &     write(*,*) 'Extend LEN1 in fixedData.h to ', npartno 
      do i=1,npartno
         array(i) = part_injection(i).start_julmin
      enddo  
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_particle_injection_length_julmin(array)
      implicit none
      include '../../input/fixed/common_ptm.inc'
      integer array(50)
      integer i
      if (npartno .gt. 50) 
     &     write(*,*) 'Extend LEN1 in fixedData.h to ', npartno 
      do i=1,npartno
         array(i) = part_injection(i).length_julmin
      enddo  
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_fluxes()
      implicit none
      integer get_number_of_fluxes
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      get_number_of_fluxes = nFlux
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_incoming(index)
      implicit none
      integer get_number_incoming
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer index
      get_number_incoming = flux(index).numberIncoming
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_outgoing(index)
      implicit none
      integer get_number_outgoing
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer index
      get_number_outgoing = flux(index).numberOutgoing
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_flux_incoming(index, array)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer array(50), index
      integer i
      do i=1,flux(index).numberIncoming
         array(i) = flux(index).inArray(i)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_flux_outgoing(index, array)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer array(50), index
      integer i
      do i=1,flux(index).numberOutgoing
         array(i) = flux(index).outArray(i)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_flux_incoming_type(index, array)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer array(50), index
      integer i
      do i=1,flux(index).numberIncoming
         array(i) = flux(index).typeInArray(i)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_flux_outgoing_type(index, array)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer array(50), index
      integer i
      do i=1,flux(index).numberOutgoing
         array(i) = flux(index).typeOutArray(i)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_flux_incoming_account_type(index, array)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer array(50), index
      integer i
      do i=1,flux(index).numberIncoming
         array(i) = flux(index).accountTypeInArray(i)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_flux_outgoing_account_type(index, array)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer array(50), index
      integer i
      do i=1,flux(index).numberOutgoing
         array(i) = flux(index).accountTypeOutArray(i)
      enddo
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++

      function get_model_start_time()
      implicit none
      include '../../input/fixed/common.f'
      integer get_model_start_time
      get_model_start_time = start_julmin
      return
      end

c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_model_end_time()
      implicit none
      include '../../input/fixed/common.f'
      integer get_model_end_time
      get_model_end_time = end_julmin
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_model_ptm_time_step()
      implicit none
      include '../../input/fixed/common_ptm.inc'
      integer get_model_ptm_time_step
      get_model_ptm_time_step = ptm_time_step
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_display_interval()
      implicit none
      include '../../input/fixed/common.f'
      integer*4 get_display_interval
      integer*4 incr_intvl
      get_display_interval = incr_intvl(0,display_intvl,IGNORE_BOUNDARY)
      return 
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_animation_filename(array)
      implicit none
      include '../../input/fixed/common.f'
      character*(*) array
      integer lnblnk
      array = io_files(ptm,io_animation,io_write).filename
      array = array(1:lnblnk(array)) // char(0)
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_model_animation_output_interval()
      implicit none
      include '../../input/fixed/common.f'
      integer get_model_animation_output_interval
      integer mins
      character*80 intvl
      intvl = io_files(ptm,io_animation,io_write).interval
      call CharIntvl2Mins(intvl, mins)
      get_model_animation_output_interval = mins
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_behavior_filename(array)
      implicit none
      include '../../input/fixed/common.f'
      character*(*) array
      integer lnblnk
      array = io_files(ptm,io_behavior,io_read).filename
      array = array(1:lnblnk(array)) // char(0)
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_trace_filename(array)
      implicit none
      include '../../input/fixed/common.f'
      character*(*) array
      integer lnblnk
      array = io_files(ptm,io_trace,io_write).filename
      array = array(1:lnblnk(array)) // char(0)
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_model_trace_output_interval()
      implicit none
      include '../../input/fixed/common.f'
      integer get_model_trace_output_interval
      integer mins
      character*80 intvl
      intvl = io_files(ptm,io_trace,io_write).interval
      call CharIntvl2Mins(intvl, mins)
      get_model_trace_output_interval = mins
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_restart_output_filename(array)
      implicit none
      include '../../input/fixed/common.f'
      character*(*) array
      integer lnblnk
      array = io_files(ptm,io_restart,io_write).filename
      array = array(1:lnblnk(array)) // char(0)
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_restart_output_interval()
      implicit none
      include '../../input/fixed/common.f'
      integer get_restart_output_interval, mins
      character*80 intvl
      intvl = io_files(ptm,io_restart,io_write).interval
      call CharIntvl2Mins(intvl, mins)
      get_restart_output_interval= mins
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_restart_input_filename(array)
      implicit none
      include '../../input/fixed/common.f'
      character*(*) array
      integer lnblnk
      array = io_files(ptm,io_restart,io_read).filename
      array = array(1:lnblnk(array)) // char(0)
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_waterbody_type( id )
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer id, type, get_waterbody_type
      type = wb(id).type
      if ( type .eq. obj_channel ) then
         type = 100
      else if ( type .eq. obj_reservoir ) then
         type = 101
      else if ( type .eq. obj_qext ) then
         type = 102
      else if ( type .eq. obj_obj2obj ) then
         type = 103
      else
         type = -1
      endif
      get_waterbody_type = type
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_local_id_for_waterbody( id )
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer id, get_local_id_for_waterbody
      get_local_id_for_waterbody = wb(id).localIndex
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_number_of_nodes_for_waterbody( id )
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer id, get_number_of_nodes_for_waterbody
      get_number_of_nodes_for_waterbody = wb(id).numberOfNodes
      return
      end
c-----++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine get_node_array_for_waterbody(id, nodeArray)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
      integer i,id, nodeArray(50)
      do i=1,wb(id).numberOfNodes
         nodeArray(i) = wb(id).node(i)
      enddo
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function getWaterbodyUniqueId(wbtype, id)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
c-----
      integer getWaterbodyUniqueId
      integer get_unique_id_for_channel
     &     , get_unique_id_for_reservoir
     &     , get_unique_id_for_stage_boundary
     &     , get_unique_id_for_boundary
     &     , get_unique_id_for_conveyor
      integer wbId, posId,id
      byte wbtype
      posId = abs(id)
      if (wbtype .eq. obj_channel) then
         wbId = get_unique_id_for_channel(posId)
      else if (wbtype .eq. obj_reservoir) then
         wbId = get_unique_id_for_reservoir(posId)
      else if (wbtype .eq. obj_qext) then
         wbId = get_unique_id_for_boundary(posId)
      else if (wbtype .eq. obj_obj2obj) then
         wbId = get_unique_id_for_conveyor(posId)
      else if (wbtype .eq. obj_stage) then
         wbId = get_unique_id_for_stage_boundary(posId)
      endif
      getWaterbodyUniqueId = sign(wbId, id)
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function getStageWaterbodyForNode(id)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
c-----
      integer getStageWaterbodyForNode, id
      integer wbId, i, uniqId
      integer get_unique_id_for_stage_boundary
      do i = 1, nStageBoundaries
         uniqId = get_unique_id_for_stage_boundary(i)
         if ( wb(uniqId).node(1) .eq. id ) then
            wbId = uniqId
         endif
      enddo
      getStageWaterbodyForNode = wbId
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_waterbody_accounting_type(id)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
c-----
      integer get_waterbody_accounting_type, id
      integer type
      type = wb(id).acctType
      get_waterbody_accounting_type = type
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_waterbody_object_type(id)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
c-----

      integer get_waterbody_object_type, id
      integer type
      type = wb(id).type
      get_waterbody_object_type = type
      return
      end
c-----+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function get_waterbody_group(id)
      implicit none
      include '../../input/fixed/common.f'
      include 'ptmLocal.inc'
c-----
      integer get_waterbody_group, id
      integer group
      group = wb(id).group
      get_waterbody_group = group
      return
      end
