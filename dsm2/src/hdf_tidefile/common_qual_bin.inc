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

c-----Qual binary for PTM

      integer max_num_const     ! maximum number of constituents
      integer max_qual_nodes    ! maximum number of nodes
      parameter (max_num_const = 10)
      parameter (max_qual_nodes = 650)

      structure /qual_bin_file_str/
         integer*4 start_julmin_file ! file timestamp start
         integer*4 end_julmin_file ! file timestamp end
         integer*4 start_julmin ! when to start using this tidefile (wrt tidefile date)
         integer*4 end_julmin   ! when to quit using this tidefile (wrt tidefile date)
         integer interval       ! minutes between tideblocks
         character*14 start_date ! file timestamp start
         character*14 end_date  ! file timestamp end
         character*14 constituent(max_num_const) ! name of quality constituent
         character*150 filename ! tidefile name
         character*7 version    ! binary file version
      end structure
      record /qual_bin_file_str/ qual_bin_file
      common /com_qual_bin_files/ qual_bin_file

      real*4
     &     Qnode                ! array of quality constituents for node

      integer*4
     &     QualTime             ! julian minute timestamp from qual binary file

      integer
     &     numnode              ! number of used nodes
     &     ,neq                 ! number of conservative constituents
     &     ,qual2node(0:max_qual_nodes)

      common /qual_bin_data/ Qnode(0:max_qual_nodes,max_num_const),
     &     numnode,neq,qual2node,QualTime
