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


c-----multiple tidefiles for Qual, PTM
      module common_tide
      use type_defs
      use constants
      use grid_data
       
     
      integer max_tide_files    ! maximum number of tidefiles to use
     &     ,nintides            ! actual number of tidefiles
     &     ,dim_res_tf,dim_chan_tf ! reservoir and channel array dimensions used in tidefile
     &     ,n_res_tf,n_chan_tf  ! reservoir and channels used in tidefile
     &     ,current_tidefile

      parameter (max_tide_files=12)
      character*21 chead


      type(tidefile_t) :: tide_files(0:max_tide_files)

      real*4
     &      Ychan(Max_Channels,2)      ! Depth at both ends of a channel
     &     ,Achan(Max_Channels,2)     ! Flow Area at both ends of a channel
     &     ,YchanPrev(Max_Channels,2) ! Depth at both ends of a channel
     &     ,AchanPrev(Max_Channels,2) ! Flow Area at both ends of a channel
     &     ,Achan_Avg(Max_Channels)   ! Average Flow Area in a channel (Used for volume calculations)
     &     ,Qchan(Max_Channels,2)     ! Average flow at both ends of a channel
     &     ,Qresv(Max_Reservoirs,Maxresnodes) ! Flows in and out of all the junctions in reservoirs
     &     ,Eresv(Max_Reservoirs)     ! Stage in all the reservoirs
     &     ,TempQExtAv(1000)    ! HDF5 Temporary holder

      integer*4 TideTime        ! julian minute timestamp from tidefile
      integer*4 next_hydro_interval

      real*8   QChNet(Max_Channels)
      integer TidefileWriteInterval,NSample 

      end module
