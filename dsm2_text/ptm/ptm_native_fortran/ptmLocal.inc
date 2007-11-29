C!    Copyright (C) 1996 State of California, Department of Water
C!    Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!
C!    For more information about DSM2, contact:
C!
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

c-----$Id: ptmLocal.inc,v 1.2 2000/08/07 17:12:21 miller Exp $
c-----External flows
      real qNodeDiversion(max_nodes), qReservoirPumping(max_reservoirs)
      real reservoirVolume(max_reservoirs)
      common /ptmexternalflows/ qNodeDiversion, qReservoirPumping, 
     &     reservoirVolume
c----- stage boundaries
      integer*2 maxStageBoundaries
      parameter( maxStageBoundaries = 5 )
      integer*2 nStageBoundaries
      record /qext_s/ stageBoundary(maxStageBoundaries)
      common /stg_bound/ stageBoundary, nStageBoundaries
c----- waterbodies ( curret memory estimate ~ 136 kb)
      integer*2 max_wbs, max_nodes_per_wb, max_groups
c-----&     max_channels + max_reservoirs + 
c     &     maxStageBoundaries + max_qext + maxobj2obj
      parameter ( max_wbs = 2000
     &     , max_nodes_per_wb = 10 
     &     , max_groups = 25
     &     )
      structure /wbData/
         byte type              ! type of waterbody ( obj_channel, obj_reservoir...)
         byte acctType          ! accounting type of waterbody
         integer*2 localIndex   ! local index within type 
         integer*2 globalIndex  ! global index (unique)
         integer*2 numberOfNodes ! number of nodes
         integer*2 node(max_nodes_per_wb) ! node indices mapping
         integer*2 group        ! waterbody group number - zero for no group
         real flowToNode(max_nodes_per_wb) ! flow to node indexed by local node index
      end structure
c      integer*2 ngroups           ! number of waterbody groups
      real groupOut(max_groups)   ! group out array
      record /wbData/ wb(max_wbs)
      common /wbInfo/ wb, groupOut
c----- nodes ( current memory estimate ~ 44kb )
      integer*2 max_wbs_per_node, maxNodes
      parameter( max_wbs_per_node = max_cpn + max_qobj, 
     &     maxNodes = max_nodes +500)
      structure /nodeData/
         byte type              ! type ( internal / external for now )
         integer*2 id           ! global index (unique)
         integer*2 nwbs         ! number of waterbodies 
         integer*2 wbs(max_wbs_per_node) ! waterbody indices ( unique mapping )
      end structure
c----- upto 500 internal nodes assumed == about 250 internal flows
      record /nodeData/ nodes(maxNodes)
      common /nodeInfo/ nodes
c-----translations
      integer transNumber
      structure /translationStructure/
         character*40 name
         byte type
         integer nodeNumber
         integer reservoirNumber
      end structure
      record /translationStructure/ translationInfo(max_types)
      common /com_name_translations/ transNumber, translationInfo
      
      structure /fluxStructure/
         integer inArray(max_ft_flux)
     &        , typeInArray(max_ft_flux)
     &        , numberIncoming
     &        , outArray(max_ft_flux)
     &        , typeOutArray(max_ft_flux)
     &        , accountTypeInArray(max_ft_flux)
     &        , accountTypeOutArray(max_ft_flux)
     &        , numberOutgoing
     &        , nodeId
         real fluxOut
      end structure

      integer nFlux
      record /fluxStructure/ flux(max_ft_flux)
      
      common /com_s_flux/ flux, nFlux
