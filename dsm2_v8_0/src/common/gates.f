C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    The Delta Simulation Model 2 (DSM2) is free software: 
C!    you can redistribute it and/or modify
C!    it under the terms of the GNU General Public License as published by
C!    the Free Software Foundation, either version 3 of the License, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public License for more details.

C!    You should have received a copy of the GNU General Public License
C!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
C!</license>

c-----Module: Gates

      Module Gates
      use type_defs
      use constants
      Implicit None

      include '../hydrolib/network.inc'
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
	integer, parameter :: FLOW_COEF_TO_FROM_NODE = 0


      
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
      type(datasource_t) height_datasource  ! datasource that controls
      type(datasource_t) width_datasource  ! datasource that controls
      type(datasource_t) elev_datasource  ! datasource that controls                    
      type(datasource_t) pos_datasource  ! datasource that controls 
	                       ! (via time series) the position of the gate control
	type(datasource_t) op_to_node_datasource   ! datasource that controls op
	type(datasource_t) op_from_node_datasource ! in the direction indicated

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
	type(datasource_t) install_datasource ! in the direction indicated
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
      Type(Gate) :: inGate
      logical*4 :: isfree
      integer i
      ! fixme: is this correct?? 
      if (isfree .and. .not. inGate.free) then ! gate is non-redundantly free
         do i= 1,inGate.nDevice
            inGate.Devices(i).flow=0.D0 ! fixme: this may be unacceptable?
         end do
      else if ((.not. isfree) .and. inGate.free) then 
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
      use constants
	implicit none
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
      Type(Gate) parentgate
      Character :: devName*32
      Character :: parname*32=' '
      integer idev
      do idev=1,parentgate.nDevice
         parname=parentgate.devices(idev).name
         call locase(parname)
         if (parname .eq. devName) then
            deviceIndex=idev
            return
         end if
      end do
      deviceIndex=miss_val_i
      return
      end function
      
	subroutine deviceTypeString(typeString,devType)
      use IO_Units
	implicit none
	character*32, intent(out) :: TypeString
      integer, intent(in) :: devType
      TypeString = " "
	if (devType .eq. WEIR) then
	  TypeString = "weir"
	else if (devType .eq. PIPE) then 
	  TypeString = "pipe"
      else 
	  write(unit_error,'(a,i)')
     &  "Gate device type not recognized (in function deviceTypeString):",
     &  devType
        call exit(3)
      end if
	return
	end subroutine

	subroutine controlTypeString(typeString,controlType)
      use IO_Units
	implicit none
	character*32, intent(out) :: TypeString	
      integer, intent(in) :: controlType
	if (controlType .eq. GATED_FROM_TOP) then
	   typeString = "gated from top"
	else if (controlType .eq. NO_GATE_CONTROL) then
	   typeString = "no control"
	else if (controlType .eq. GATED_FROM_BOTTOM) then
	   typeString = "gated from bottom"
	else if (controlType .eq. UNIDIR_TO_NODE) then
	   typeString = "unidir to node"
	else if (controlType .eq. UNIDIR_FROM_NODE) then
	   typeString = "unidir from node"
      else 
	  write(unit_error,'(a)')"Gate control type not recognized "//
     &                          "(in function controlTypeString)"
        call exit(3)
      end if
	return
	end subroutine

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
      else if (device.structureType .eq. WEIR) then
         if (device.controlType .eq. GATED_FROM_TOP) then
	      device.height = device.position
         else if(device.controlType .eq. GATED_FROM_BOTTOM) then
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
