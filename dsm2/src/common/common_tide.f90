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

!-----multiple tidefiles for Qual, PTM
module common_tide
    use constants
    use array_limits
    use grid_data
    use gates_data, only: MAX_GATES, MAX_DEV

    implicit none

    integer :: max_tide_files
    integer :: nintides
    integer :: dim_res_tf
    integer :: dim_chan_tf
    integer :: n_res_tf
    integer :: n_chan_tf
    integer :: current_tidefile

    parameter(max_tide_files=12)
    character*21 :: chead

    type(tidefile_t):: tide_files(0:max_tide_files)

    real*4 :: &
        Zchan(2, Max_Channels)             ! HDF5 Temporary holder
    real*4 :: Hchan(2, Max_Channels)             ! HDF5 Temporary holder
    real*4 :: Achan(2, Max_Channels)             ! HDF5 Temporary holder
    real*4 :: HchanPrev(2, Max_Channels)             ! HDF5 Temporary holder
    real*4 :: AchanPrev(2, Max_Channels)             ! HDF5 Temporary holder
    real*4 :: Achan_Avg(Max_Channels)             ! HDF5 Temporary holder
    real*4 :: Qchan(2, Max_Channels)             ! HDF5 Temporary holder
    real*4 :: Eresv(Max_Reservoirs)             ! HDF5 Temporary holder
    real*4 :: TempQExtAv(1000)             ! HDF5 Temporary holder
    real*4 :: Qcp(MAX_LOCATIONS)          ! Flow at computation points
    real*4 :: Zcp(MAX_LOCATIONS)          ! Stage at computation points

    ! Flows in and out of all the reservoir-node connections.
    ! Dimension will be one per reservoir connection
    real*4, allocatable :: Qresv(:)
    real*4, allocatable :: inst_Qresv(:)

    real*4 :: inst_qext(max_qext)
    real*4 :: inst_obj2obj(max_obj2obj)
    real*4 :: inst_device_flow(MAX_GATES, MAX_DEV)

    integer*4 :: TideTime        ! julian minute timestamp from tidefile
    integer*4 :: next_hydro_interval

    real*8 :: QChNet(Max_Channels)
    integer :: TidefileWriteInterval
    integer :: NSample

end module
