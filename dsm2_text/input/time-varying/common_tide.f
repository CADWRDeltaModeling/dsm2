C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/


c-----multiple tidefiles for Qual, PTM
      
      integer max_tide_files    ! maximum number of tidefiles to use
     &     ,nintides            ! actual number of tidefiles
     &     ,dim_res_tf,dim_chan_tf ! reservoir and channel array dimensions used in tidefile
     &     ,n_res_tf,n_chan_tf  ! reservoir and channels used in tidefile
      parameter (max_tide_files=300)


      structure /tide_file_str/
         character*14 start_dt  ! when to start using this tidefile (date and time)
         character*30 end_dt    ! when to quit using this tidefile (date and time, or time length (e.g. 5day_3hour))
         integer*4 start_julmin_file ! file timestamp start
         integer*4 end_julmin_file ! file timestamp end
         integer*4 start_julmin ! when to start using this tidefile (wrt tidefile date)
         integer*4 end_julmin   ! when to quit using this tidefile (wrt tidefile date)
         integer ntideblocks    ! number of tideblocks
         integer interval       ! minutes between tideblocks
         character*150 filename ! tidefile name
      end structure
      record /tide_file_str/ tide_files(0:max_tide_files)
      common /com_tide_files/ tide_files,nintides,dim_res_tf,dim_chan_tf,
     &     n_res_tf,n_chan_tf

      real*4
     &     Ychan                ! Depth at both ends of a channel
     &     ,Achan               ! Flow Area at both ends of a channel
     &     ,Achan_Avg           ! Average Flow Area in a channel (Used for volume calculations)
     &     ,Qchan               ! Average flow at both ends of a channel
     &     ,Qresv               ! Flows in and out of all the junctions in reservoirs
     &     ,Eresv               ! Stage in all the reservoirs

      integer*4 TideTime        ! julian minute timestamp from tidefile
c-----update changes to ../../ptm/common_tide_type.hc
      common /tide_file_hydrology/Ychan(Max_Channels,2),
     &     Achan(Max_Channels,2),Achan_Avg(Max_Channels),
     &     Qchan(Max_Channels,2),Qresv(Max_Reservoirs,Maxresnodes),
     &     Eresv(Max_Reservoirs),TideTime



