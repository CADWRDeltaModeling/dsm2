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


c-----miscellaneous values

c-----particle passing between waterbodies (flux output)
      integer
     &     ptm_from_wb
     &     ,ptm_to_wb
     &     ,ptm_interval
     &     ,ptm_filename
     &     ,ptm_modifier
     &     ,b_part

      parameter (
     &     ptm_from_wb=1
     &     ,ptm_to_wb=2
     &     ,ptm_interval=3
     &     ,ptm_filename=4
     &     ,ptm_modifier=5
     &     ,b_part=6
     &     )

c-----magic numbers for particle injection
      integer
     &     partno_node
     &     ,partno_nparts
     &     ,partno_slength
     &     ,partno_length
     &     ,partno_sdate
     &     ,partno_stime
     &     ,partno_edate
     &     ,partno_etime
     &     ,partno_type

      parameter (
     &     partno_node=1
     &     ,partno_nparts=2
     &     ,partno_slength=3
     &     ,partno_length=4
     &     ,partno_sdate=5
     &     ,partno_stime=6
     &     ,partno_edate=7
     &     ,partno_etime=8
     &     ,partno_type=9
     &     )

c-----magic numbers for locations of particles in chnl groups
      integer
     &     group_object
     &     ,group_chnlno
     &     ,group_num

      parameter (
     &     group_object=1
     &     ,group_chnlno=2
     &     ,group_num=3
     &     )

c-----magic number for 'all' keyword in flux output
      integer alltypes
      parameter (alltypes = -100)

c-----magic numbers for profile of particle distribution option

      integer
     &     partdist_chan
     &     ,partdist_slength
     &     ,partdist_sdate
     &     ,partdist_stime

      parameter (
     &     partdist_chan=1
     &     ,partdist_slength=2
     &     ,partdist_sdate=3
     &     ,partdist_stime=4
     &     )

      integer 
     &     unit_binary

      parameter (
     &     unit_binary=3
     &     )
