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

subroutine process_tide(new_tidefile,recycle_tidefile, &
    first_used_tidefile,current_tidefile,tide_block_no)
    implicit none
    real :: current_tidefile
    real :: first_used_tidefile
    integer :: new_tidefile
    real :: recycle_tidefile
    real :: tide_block_no
    !-----This is a dummy routine to keep the linker happy for Hydro.
    return
end

subroutine check_tidefile(dim_res,dim_chan,n_res,n_chan, &
    tidefile)
    implicit none
    real :: dim_chan
    real :: dim_res
    integer :: n_chan
    integer :: n_res
    !-----This is a dummy routine to keep the linker happy for Hydro.
    character*(*) tidefile
    return
end
