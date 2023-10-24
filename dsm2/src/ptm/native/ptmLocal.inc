!    Copyright (C) 1996, 2009 State of California, Department of Water
!    Resources.
!
!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
!    numerical model.  No protection claimed in original FOURPT and
!    Branched Lagrangian Transport Model (BLTM) code written by the
!    United States Geological Survey.  Protection claimed in the
!    routines and files listed in the accompanying file "Protect.txt".
!    If you did not receive a copy of this file contact
!    Tara Smith, below.
!
!    This program is licensed to you under the terms of the GNU General
!    Public License, version 2, as published by the Free Software
!    Foundation.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, contact Tara Smith, below,
!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
!    02139, USA.
!
!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
!    DAMAGE.
!
!    For more information about DSM2, contact:
!
!    Tara Smith
!    California Dept. of Water Resources
!    Division of Planning, Delta Modeling Section
!    1416 Ninth Street
!    Sacramento, CA  95814
!    916-653-9885
!    tara@water.ca.gov
!
!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/

!-----$Id: ptmLocal.inc,v 1.2.6.6 2006/10/17 22:43:46 qshu Exp $
!-----External flows
      real, parameter :: theta = 0.5
      real qNodeDiversion(max_nodes), qReservoirPumping(max_reservoirs)
      real reservoirVolume(max_reservoirs)
      common /ptmexternalflows/ qNodeDiversion, qReservoirPumping, &
           reservoirVolume
!----- stage boundaries
      integer*2 maxStageBoundaries
      parameter( maxStageBoundaries = 5 )
      integer*2 nStageBoundaries
      type(qext_t) stageBoundary(maxStageBoundaries)
      common /stg_bound/ stageBoundary, nStageBoundaries
!----- waterbodies ( curret memory estimate ~ 136 kb)
      integer*2 max_wbs, max_nodes_per_wb, max_groups
!-----&     max_channels + max_reservoirs +
!     &     maxStageBoundaries + max_qext + maxobj2obj
      parameter ( max_wbs = 2000 &
           , max_nodes_per_wb = 10 &
           , max_groups = 25 &
           )
      structure /wbData/
         byte type              ! type of waterbody ( obj_channel, obj_reservoir...)
         integer*2 localIndex   ! local index within type
         integer*2 globalIndex  ! global index (unique)
         integer*2 numberOfNodes ! number of nodes
         integer*2 node(max_nodes_per_wb) ! node indices mapping
         integer*2 group        ! waterbody group number - zero for no group
         real flowToNode(max_nodes_per_wb) ! flow to node indexed by local node index
      end structure
!      integer*2 ngroups           ! number of waterbody groups
      record /wbData/ wb(max_wbs)
      common /wbInfo/ wb
!----- nodes ( current memory estimate ~ 44kb )
      integer*2 max_wbs_per_node, maxNodesPTM !todo: added _ Eli to compile
      parameter( max_wbs_per_node = max_cpn + max_qobj, &
           maxNodesPTM = max_nodes +500)
      structure /nodeData/
         byte type              ! type ( internal / external for now )
         integer*2 id           ! global index (unique)
         integer*2 nwbs         ! number of waterbodies
         integer*2 wbs(max_wbs_per_node) ! waterbody indices ( unique mapping )
      end structure
!----- upto 500 internal nodes assumed == about 250 internal flows
      record /nodeData/ nodes(maxNodesPTM)
      common /nodeInfo/ nodes
!-----translations
      integer transNumber
      structure /translationStructure/
         character*40 name
         byte type
         integer nodeNumber
         integer reservoirNumber
      end structure
	  integer, parameter :: max_types = 100 !fixme: Eli to compile
      record /translationStructure/ translationInfo(max_types)
      common /com_name_translations/ transNumber, translationInfo

      structure /fluxStructure/
         integer inIndex &
              ,outIndex &
              ,inType &
              ,outType &
              ,nodeId
         real fluxOut
      end structure

      structure /groupOutStructure/
         integer groupNdx
         real value
      end structure



      integer nFlux
      record /fluxStructure/ flux(max_ft_flux)

      common /com_s_flux/ flux, nFlux

      integer ngroup_output

	  record /groupOutStructure/ groupOut(max_group_out)
	  common /com_s_group/ groupOut,ngroup_output
