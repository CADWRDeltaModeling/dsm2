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

      integer
     &     max_inp_min
     &     ,max_inp_hour
     &     ,max_inp_day
     &     ,max_inp_week
     &     ,max_inp_month
     &     ,max_inp_year
     &     ,max_inp_irr
     &     ,max_out_min
     &     ,max_out_hour
     &     ,max_out_day
     &     ,max_out_week
     &     ,max_out_month
     &     ,max_out_year
     &     ,mins15
     &     ,hrs
     &     ,dys
     &     ,wks
     &     ,mths
     &     ,yrs
     &     ,irrs
     &     ,maxinpsize

      parameter (
     &     max_inp_min = 20     ! maximum input paths for 15minute intervals
     &     ,max_inp_hour = 20   ! maximum input paths for hour intervals
     &     ,max_inp_day = 33    ! maximum input paths for day intervals
     &     ,max_inp_week = 10   ! maximum input paths for week intervals
     &     ,max_inp_month = 3000 ! maximum input paths for month intervals
     &     ,max_inp_year = 10   ! maximum input paths for year intervals
     &     ,max_inp_irr = 150   ! maximum input paths for irregular intervals
     &     ,max_out_min = 200   ! maximum output paths for 15minute intervals
     &     ,max_out_hour = 200  ! maximum output paths for hour intervals
     &     ,max_out_day = 200   ! maximum output paths for day intervals
     &     ,max_out_week = 10   ! maximum output paths for week intervals
     &     ,max_out_month = 200 ! maximum output paths for month intervals
     &     ,max_out_year = 10   ! maximum output paths for year intervals
c-----each of the following should be 2 or greater, and they must be
c-----larger than the longest repeating length, e.g. at least 25 hours
c-----for a 19 year repeating mean tide; 12 months for monthly data
c-----that varies by month but not by year.
     &     ,mins15 = 4*24*20    ! number of values in a 15MIN interval (20 days worth)
     &     ,hrs = 24*20         ! 20 days of hourly values
     &     ,dys = 35            ! NOTE: if you change these so that mins15 is no longer
     &     ,wks = 5             ! the longest length block, you must resize outdata_arr in wrt_outpaths.f
     &     ,mths = 13
     &     ,yrs = 3
     &     ,irrs = 10
     &     ,maxinpsize = max(mins15,hrs,dys,wks,mths,yrs,irrs)
     &     )

