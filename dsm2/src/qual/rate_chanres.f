C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.

c!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
c!    numerical model.  No protection claimed in original FOURPT and
c!    Branched Lagrangian Transport Model (BLTM) code written by the
c!    United States Geological Survey.  Protection claimed in the
c!    routines and files listed in the accompanying file "Protect.txt".
c!    If you did not receive a copy of this file contact Dr. Paul
c!    Hutton, below.
c!
c!    This program is licensed to you under the terms of the GNU General
c!    Public License, version 2, as published by the Free Software
c!    Foundation.
c!
c!    You should have received a copy of the GNU General Public License
c!    along with this program; if not, contact Dr. Paul Hutton, below,
c!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
c!    02139, USA.
c!
c!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
c!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
c!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
c!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
c!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
c!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
c!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
c!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
c!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
c!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
c!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
c!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
c!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
c!    DAMAGE.
c!
c!    For more information about DSM2, contact:
c!
c!    Dr. Paul Hutton
c!    California Dept. of Water Resources
c!    Division of Planning, Delta Modeling Section
c!    1416 Ninth Street
c!    Sacramento, CA  95814
c!    916-653-5601
c!    hutton@water.ca.gov
c!
c!    or see our home page: http://wwwdelmod.water.ca.gov/

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
