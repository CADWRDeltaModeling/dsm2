!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software: 
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>

subroutine process_channel_ic(channel, &
                              dist, &
                              stage, &
                              flow)
      
      use io_units
      use logging
      use grid_data
      use envvar
      use network
      use chinitcd
      implicit none

      character*8 dist
      integer :: channel 
      integer :: intchan,extchan
      integer :: distance
      real*8 :: stage
      real*8 :: flow
      integer, external :: ext2int
      integer,save :: k = 0
!--------use only the last version of the initial condition
      call locase(dist)
      extchan = channel
      intchan=ext2int(extchan)
      if (intchan .le. 0) then ! non-existant channel
          write(unit_error,*)"Attempt to initialize unused channel ignored."
	    write(unit_error,*)"Channel: ",extchan
      else
          FirstLocation(intchan) = k
      !end if
      
      k=k+1
     
      if (dist .eq. "length") then
          distance=chan_geom(intchan).length
      else 
          read(dist,*)distance
      end if
      
      NUserInitLocations(intchan)=NUserInitLocations(intchan)+1
      InitialX(k)=dble(distance)
      InitialWS(k)=stage
      InitialQ(k)=flow
      end if
      return
end subroutine

subroutine process_reservoir_ic(resname, &
                                stage)
      use io_units
      use logging
      use grid_data
      use envvar
      use constants
      use network
      use chconnec

      implicit none

      character*32 resname
      real*8   :: stage
      integer :: resno
      integer, save :: name_to_objo
      integer,save :: k = 0
      integer, external :: name_to_objno
!--------use only the last version of the initial condition
      call locase(resname)
      resno=name_to_objno(obj_reservoir, resname)
      if (resno .eq. miss_val_i) then ! reservoir doesn't exist for init val
          write(unit_error, '(a)') 'Reservoir ' &
           // trim(resname) // ' does not exist to load initial conditions.'
      else
          YRes(resno)=stage
      end if 
      return
end subroutine

       
