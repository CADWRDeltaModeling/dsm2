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

      subroutine read_tide_flow(desired_time,new_tidefile,
     &     first_used_tidefile,tidefile,
     &     tide_block_no,process_data)

c-----Read the time-varying flow values from a DSM2-Hydro
c-----binary or HDF5 tidefile.  This routine assumes that the correct
c-----tidefile is being used for the desired time specified, however,
c-----it will search within the tidefile for the correct tide block.

      use IO_Units
      use hdfvars

      implicit none

      include '../fixed/common.f'
      include 'common_tide.f'
      include 'tide.inc'

c-----arguments

      integer*4 desired_time     ! desired time of tide block, in julian minutes (INPUT)
      logical new_tidefile      ! true if new tidefile (INPUT)
     &     ,process_data        ! check and process tide data
      integer  tidefile         ! current tidefile number that is being used (INPUT)
     &     ,first_used_tidefile ! number of first tidefile that was used
     &     ,tide_block_no       ! tide block number used (INPUT & RETURNED)

c-----local variables

      integer, external :: GetCurrentTideTime
      integer, external :: SetHDF5ToTime

         
	if (GetCurrentTideTime() .lt. desired_time)then
	  ! flow values must be updated
	  TideTime=SetHDF5ToTime(desired_time)
	   call ReadDataFromHDF5(TideTime)

         if (process_data) then
            call process_tide(new_tidefile,
     &           first_used_tidefile,current_tidefile,tide_block_no)
         endif
      endif
      return


      end




