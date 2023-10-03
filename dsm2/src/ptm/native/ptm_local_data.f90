!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.

!    The Delta Simulation Model 2 (DSM2) is free software:
!    you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.

!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

      module ptm_local
      use grid_data
      integer max_wbs, max_nodes_per_wb, max_groups
      parameter ( max_wbs = 2000 &
           , max_nodes_per_wb = 10 &
           , max_groups = 25 &
           )

      integer, parameter :: max_wbs_per_node = max_cpn + max_qobj
      integer, parameter :: maxNodesPTM = max_nodes +500

      type wbData
      sequence
         integer type              ! type of waterbody ( obj_channel, obj_reservoir...)
         integer localIndex   ! local index within type
         integer globalIndex  ! global index (unique)
         integer numberOfNodes ! number of nodes
         integer node(max_nodes_per_wb) ! node indices mapping
         integer group        ! waterbody group number - zero for no group
         real flowToNode(max_nodes_per_wb) ! flow to node indexed by local node index
         character*32 name ! channel name="channel", name="reservior" boundary name=qext.name
      end type

      type nodeData
      sequence
         integer type         ! type ( internal / external for now )
         integer id           ! global index (unique)
         integer nwbs         ! number of waterbodies
         integer wbs(max_wbs_per_node) ! waterbody indices ( unique mapping )
      end type

      type translationStructure
      sequence
         character*40 name
         integer type
         integer nodeNumber
         integer reservoirNumber
      end type

      type fluxStructure
      sequence
         integer inIndex
         integer outIndex
         integer inType
         integer outType
         integer nodeId
         real fluxOut
      end type

      type groupOutStructure
      sequence
         integer groupNdx
         real value
      end type

!-----External flows
      real, parameter :: theta = 0.6
!     real, parameter :: theta = 0.5
      real qNodeDiversion(max_nodes), qReservoirPumping(max_reservoirs)
      real reservoirVolume(max_reservoirs)

!----- stage boundaries
      integer maxStageBoundaries
      parameter( maxStageBoundaries = 5 )
      integer nStageBoundaries
      type(qext_t) stageBoundary(maxStageBoundaries)

!---- waterbodies
      type(wbData) wb(max_wbs)
!----- nodes ( current memory estimate ~ 44kb )


!----- upto 500 internal nodes assumed == about 250 internal flows
      type(nodeData) nodes(maxNodesPTM)

!-----translations
      integer transNumber

	integer, parameter :: max_types = 100 !fixme: Eli to compile

      type(translationStructure) translationInfo(max_types)


      integer nFlux
      type(fluxStructure) flux(max_ft_flux)


      integer ngroup_output

	type(groupOutStructure) :: groupOut(max_group_out)
      end module
