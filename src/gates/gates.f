C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
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

c-----Module: Gates

      Module Gates

      Implicit None

      include '../hydrolib/network.inc'
      include '../fixed/misc.f'
	include '../fixed/defs.f'
                                ! Maximums for pre-dimensioned arrays
      integer, parameter :: MAX_DEV=10 ! Max no. of devices per gate
      integer, parameter :: MAX_GATES=MaxNGate ! Max no. of gates

      Integer, save :: nGate    ! Actual number of gates calc'd at run time

c---- Constants for structureType
      integer, parameter :: WEIR =1
	integer, parameter :: PIPE =2
c---- Constants for controlType
	integer, parameter :: NO_GATE_CONTROL = 512
	integer, parameter :: GATED_FROM_TOP = 1028
	integer, parameter :: GATED_FROM_BOTTOM = 2056

c-----gate operations/states      
      integer, parameter :: GATE_OPEN =1  ! device fully open to flow
      integer, parameter :: GATE_CLOSE =0 ! device fully closed to flow
      integer, parameter :: GATE_FREE =10 ! all devices removed, open channel flow
	integer, parameter :: UNIDIR_TO_NODE = 20   ! device open to flow to node, closed from node
	integer, parameter :: UNIDIR_FROM_NODE = 40 ! device open to flow from node, closed to node

c-----constants for flow coeff direction
	integer, parameter :: FLOW_COEF_TO_NODE = 1
	integer, parameter :: FLOW_COEF_FROM_NODE = -1



      
      TYPE GateDevice       !Keep variables in natural alignment.
	real*8 :: flow        ! fixme: initialization
                                       ! time of installation
      real*8 :: position = miss_val_r  ! position (in physical units) of any gate control
	                      ! e.g., height of a radial gate or
	                      ! crest elevation in a gate with adjustable bottom
      real*8 :: maxWidth        ! width or pipe radius
      real*8 :: baseElev        ! invert or crest or bottom elevation wrt datum
	real*8 :: height          ! height or maximum aperture height
      real*8 :: flowCoefToNode   ! flow coefficient (physical) in node direction
      real*8 :: flowCoefFromNode ! flow coeff from node to water body
      real*8 :: opCoefToNode   = 1. ! time varying coefficient between [0,1] fixme: default?
      real*8 :: opCoefFromNode = 1. !  0 = closed, 1 = fully open
      logical*4 :: dummy         ! filler to maintain an even number of
                                 ! *4 byte variables to maintain alignment
	integer*4 :: structureType ! type of gate structure (pipe, weir). 
	                           ! See structureType constants above for acceptable values
	integer*4 :: controlType   ! type of flow control (e.g. gated from top)
	                            ! See controlType constants above for acceptable values
      integer*4 :: nDuplicate = 0 ! number of identical structures treated as one device
      integer*4 :: gate         ! index of gate in which device appears (fixme: why?)
	integer*4 :: calcRow      ! Row (equation) in which gate device equation is expressed
      character :: name*32=' ' ! index of device in gate     
      record /datasource_s/ pos_datasource  ! datasource that controls 
	                       ! (via time series) the position of the gate control
	record /datasource_s/ op_to_node_datasource   !datasource that controls the 
	                                              !operating coefficient of the
	record /datasource_s/ op_from_node_datasource ! device in the direction indicated
      END TYPE

!Variables are in natural (8byte) alignment.
      TYPE Gate          
      real*8  flowDirection        ! orientation of flow. (+1.D0) if downstream/pos flow is from 
                                   ! gate's connected object to node. (-1.D0) otherwise.
      real*8  flow
      real*8  flowPrev
      integer :: ID 
                                     !  see note above on alignment
      logical*4 :: inUse   = .false. ! use gate? Used to indicate that the gate is not even
                                     !  included in the model
	logical*4 :: free    = .false. ! are all devices considered uninstalled? If so this gate,
	                               ! for the moment, will act like a simple node connection
	                               ! between two channels.
      integer*4 :: objConnectedType ! Object Type to which gate is connected (e.g. obj_channel,obj_reservoir)
      integer*4 :: objConnectedID   ! Internal number of object (channel, reservoir) to which gate is attached
      integer*4 :: objCompPoint     ! Index of computation point at water body
      integer*4 :: subLocation      ! location within objConnectedID:
                                    ! For reservoirs: index of associated reservoir connection
                                    ! For channels:   extremity (+1 for upstream, -1 for downstream) 
      integer*4 :: node             ! Node to which gate is attached
      integer*4 :: nodeCompPoint    ! Index of (stage reference) computation point at node
      integer*4 :: nDevice = 0      ! number of devices in gate structure
      Type(GateDevice),dimension(MAX_DEV) :: devices ! Array of devices in gate
      character :: name*32 = ' '   ! name of gate
      End Type

! fixme is NTotalDevice needed? When is it initialized?
      Integer, save :: NTotalDevice = 0

      Type (Gate), dimension(MaxNGate), target, save :: gateArray

      contains  !!========================================================!

*== Public (GateInit) ===================================================
      
      subroutine setFree( inGate, isfree)
c-----Sets or unsets a gate to the GATE_FREE condition, which means that the
c-----gate has been physically uninstalled and the connection reverts to
c-----an "equal stage" internal boundary condition. This routine takes care
c-----of the clean-up required in order to make the transition.
c-----use Gates, only: Gate, GATE_OPEN
      implicit none
	include '../fixed/common.f'
      Type(Gate) :: inGate
      logical*4 :: isfree
      integer i
      ! fixme: is this correct?? 
      if (isfree .and. .not. inGate.free)then ! gate is non-redundantly free
         do i= 1,inGate.nDevice
            inGate.Devices(i).flow=0.D0 ! fixme: this may be unacceptable?
         end do
      else if ((.not. isfree) .and. inGate.free)then 
	      ! gate is non-redundantly installed
      end if
      inGate.free=isfree
      return
      end subroutine


*== Public (gateIndex) =========
c     Given a gate name, obtain its internal index in gateArray or
c     miss_val_i if the name is not a gate name in the current model. 
c     Element-by-element search, so this routine is expensive. 
      integer function gateIndex(gateName)
	implicit none
	include '../fixed/misc.f'
	integer igate
	character :: gateName*32
	character :: lowerName*32
	lowerName=gateName
	call locase(lowerName)
	do igate=1,nGate
	  if(lowerName .eq. gateName) then
	    gateIndex=igate
	    return
	  end if
      end do
	gateIndex=miss_val_i
	return
	end function

*== Public (deviceIndex) =======
c     Given a gate, converts a character name of a device into the index
c     of the device. If the gate does not contain the device name, miss_val_i
c     is returned. Element-by-element search, so this routine is expensive. 
      integer function deviceIndex(parentgate,devName)
      implicit none
      include '../fixed/misc.f'
      Type(Gate) parentgate
      Character :: devName*32
      Character :: parname*32=' '
      integer idev
      do idev=1,parentgate.nDevice
         parname=parentgate.devices(idev).name
         call locase(parname)
         if (parname .eq. devName)then
            deviceIndex=idev
            return
         end if
      end do
      deviceIndex=miss_val_i
      return
      end function
      
	character*32 function deviceTypeString(devType)
      use IO_Units
	implicit none
      integer devType
	if (devType .eq. WEIR)then
	  deviceTypeString = "weir"
	else if (devType .eq. PIPE) then 
	  deviceTypeString = "pipe"
      else 
	  write(unit_error,'(a)')
     &  "Gate device type not recognized (in function deviceTypeString)"
        call exit(3)
      end if
	return
	end function

	character*32 function controlTypeString(controlType)
      use IO_Units
	implicit none
      integer controlType
	if (controlType .eq. GATED_FROM_TOP) then
	   controlTypeString = "gated from top"
	else if (controlType .eq. NO_GATE_CONTROL) then
	   controlTypeString = "no control"
	else if (controlType .eq. GATED_FROM_BOTTOM) then
	   controlTypeString = "gated from bottom"
	else if (controlType .eq. UNIDIR_TO_NODE) then
	   controlTypeString = "unidir to node"
	else if (controlType .eq. UNIDIR_FROM_NODE) then
	   controlTypeString = "unidir from node"
      else 
	  write(unit_error,'(a)')"Gate control type not recognized "//
     &"(in function controlTypeString)"
        call exit(3)
      end if
	return
	end function

c===== interprets the "position" variable and applies it to the correct
c      gate parameter      
	subroutine ApplyDevicePosition(device)
	use io_units
      implicit none
	type(GateDevice) device
      if (device.position .eq. miss_val_r) then
	   ! fixme: cheap way out, no  warning
	   return
	end if
      if (device.structureType .eq. PIPE .or. device.controlType 
     &      .eq. NO_GATE_CONTROL) then
         return ! no gate control yet for pipes	   
      else if (device.structureType .eq. WEIR)then
         if (device.controlType .eq. GATED_FROM_TOP)then
	      device.height = device.position
         else if(device.controlType .eq. GATED_FROM_BOTTOM)then
	      device.baseElev = device.position
         end if
      else 
	  write(unit_error,'(a)')"Gate device type not recognized "//
     &     "(in function ApplyDevicePosition)"
        call exit(3)
      end if
	return
	end subroutine



      End Module Gates
