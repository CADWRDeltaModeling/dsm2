!!    Copyright (C) 1996, 1997, 1998 State of California,
!!    Department of Water Resources.
!!
!!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
!!    numerical model.  No protection claimed in original FOURPT and
!!    Branched Lagrangian Transport Model (BLTM) code written by the
!!    United States Geological Survey.  Protection claimed in the
!!    routines and files listed in the accompanying file "Protect.txt".
!!    If you did not receive a copy of this file contact Dr. Paul
!!    Hutton, below.
!!
!!    This program is licensed to you under the terms of the GNU General
!!    Public License, version 2, as published by the Free Software
!!    Foundation.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with this program; if not, contact Dr. Paul Hutton, below,
!!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
!!    02139, USA.
!!
!!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
!!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
!!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
!!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
!!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
!!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
!!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
!!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
!!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
!!    DAMAGE.
!!
!!    For more information about DSM2, contact:
!!
!!    Dr. Paul Hutton
!!    California Dept. of Water Resources
!!    Division of Planning, Delta Modeling Section
!!    1416 Ninth Street
!!    Sacramento, CA  95814
!!    916-653-5601
!!    hutton@water.ca.gov
!!
!!    or see our home page: http://wwwdelmod.water.ca.gov/

      program wrt_tmp

!-----Write temporary files from FourPt (perhaps left from model crash)
!-----to permanent files.

!-----Filenames will be on command line.

      use IO_Units
      USE DFLIB                 !! <NT>
      use dss
      use readdss
      use writedss
      implicit none

      include '../fixed/common.f'
!-----local variables

      integer &
          i                    ! loop index &
          ,n_args              ! number of command line args &
          ,iargc               ! number of command line args intrinsic

      integer*2 i2

 610  format('Warning--',i3,' command line arguments specified, 6 used.')

      n_args=iargc()
      if (n_args .eq. 0) then
         write(unit_error,'(a)') &
             'Must specify temporary files on command line.'
         goto 900
      endif

      if (n_args .gt. 6) then
         write(unit_error, 610) n_args
         n_args=6
      endif

      do i2=1,6
         scratch_file_array(i2)=miss_val_c
      enddo

      do i2=1, n_args
         call getarg(i2,scratch_file_array(i2))
      enddo

      call zset('MLEVEL','',2)
      call wrt_outpaths

      call exit(0)

 900  continue

      call exit(2)

      end
