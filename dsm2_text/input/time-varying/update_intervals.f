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

      subroutine update_intervals

c-----Update the julian minute values representing the end of each
c-----DSS interval.

      implicit none

      include '../fixed/common.f'

      integer*4
     &     incr_intvl          ! increment julian minute by interval function

      jmin_15min=incr_intvl(julmin,'15MIN',NEAREST_BOUNDARY)
      jmin_15min_prev=incr_intvl(julmin,'-15MIN',NEAREST_BOUNDARY)

      jmin_1hour=incr_intvl(julmin,'1HOUR',NEAREST_BOUNDARY)
      jmin_1hour_prev=incr_intvl(julmin,'-1HOUR',NEAREST_BOUNDARY)

      jmin_1day=incr_intvl(julmin,'1DAY',NEAREST_BOUNDARY)
      jmin_1day_prev=incr_intvl(julmin,'-1DAY',NEAREST_BOUNDARY)

      jmin_1week=incr_intvl(julmin,'1WEEK',NEAREST_BOUNDARY)
      jmin_1week_prev=incr_intvl(julmin,'-1WEEK',NEAREST_BOUNDARY)

      jmin_1month=incr_intvl(julmin,'1MON',NEAREST_BOUNDARY)
      jmin_1month_prev=incr_intvl(julmin,'-1MON',NEAREST_BOUNDARY)

      jmin_1year=incr_intvl(julmin,'1YEAR',NEAREST_BOUNDARY)
      jmin_1year_prev=incr_intvl(julmin,'-1YEAR',NEAREST_BOUNDARY)

      return
      end
