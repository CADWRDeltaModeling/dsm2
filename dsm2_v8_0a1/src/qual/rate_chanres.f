C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>


      subroutine rate_chanres (num)

c-----This subroutine decides whether to use rate coeffs for reservoirs
c-----or for channels: if chan_res =1, channel; if chan_res=2, reservoir
      use common_qual
      implicit none
      include 'param.inc'
      include 'bltm3.inc'
      include 'kinetic1.inc'

c-----local variables
      integer  i, ii, j, k, num

c-----depth comput.  for channels may need to be changed to correspond to
c-----irregular geometry
      if (chan_res. eq. 1) then
         depth = a(mx)/w(mx)     
         vel = q(mx)/a(mx)

      elseif (chan_res. eq. 2) then
         depth = resdepth
         vel = 0.2
      endif

      do ii = 1, no_of_nonconserve_constituent
         i = nonconserve_ptr(ii)
         k = constituent_ptr(i)
         do j = 1, ncoef_type
            if (chan_res. eq. 1) then
               rcoef(i,j)=rcoef_chan(k,j,num)
            else if (chan_res. eq. 2) then
               rcoef(i,j)=rcoef_res(k,j,num)
            else
               print*,' error... wrong value for chan_res sent to'
               print*,'          subroutine rate_chanres'
            endif
         enddo
      enddo
      return
      end
