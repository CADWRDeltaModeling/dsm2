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

module qual_rate_chanres
    use qual_param
    use bltm
contains
      subroutine rate_chanres (num)

!-----This subroutine decides whether to use rate coeffs for reservoirs
!-----or for channels: if chan_res =1, channel; if chan_res=2, reservoir
      use common_qual
      implicit none

!-----local variables
      integer  i, ii, j, k, num

!-----depth comput.  for channels may need to be changed to correspond to
!-----irregular geometry
      if (chan_res .eq. 1) then
         depth = a(mx)/w(mx)
         vel = q(mx)/a(mx)

      elseif (chan_res .eq. 2) then
         depth = resdepth
         vel = 0.2
      endif

      do ii = 1, no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)
         k = constituent_ptr(i)
         do j = 1, ncoef_type
            if (chan_res .eq. 1) then
               rcoef(i,j)=rcoef_chan(k,j,num)
            else if (chan_res .eq. 2) then
               rcoef(i,j)=rcoef_res(k,j,num)
            else
               print*,' error... wrong value for chan_res sent to'
               print*,'          subroutine rate_chanres'
            endif
         enddo
      enddo
      return
      end
end module qual_rate_chanres