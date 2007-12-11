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

      subroutine process_tide(new_tidefile,recycle_tidefile,
     &     first_used_tidefile,current_tidefile,tide_block_no)
c-----This is a dummy routine to keep the linker happy for Hydro.
      return
      end

      subroutine check_tidefile(dim_res,dim_chan,n_res,n_chan,
     &     tidefile)
c-----This is a dummy routine to keep the linker happy for Hydro.
      character*(*) tidefile
      return
      end
