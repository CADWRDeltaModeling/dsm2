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

c-----Define arrays to be used as buffers for data values, data
c-----quality flags from DSS, and julian minute timestamp.
c-----Use one array for each kind of data interval for DSS.

      type(dataqual_t)
     &     datain_15min(mins15,max_inp_min) ! 15 minute data
     &     ,datain_1hour(hrs,max_inp_hour) ! hourly data
     &     ,datain_1day(dys,max_inp_day) ! daily data
     &     ,datain_1week(wks,max_inp_week) ! weekly data
     &     ,datain_1month(mths,max_inp_month) ! monthly data
     &     ,datain_1year(yrs,max_inp_year) ! yearly data
     &     ,datain_irr(irrs,max_inp_irr) ! irregular data block

      common /dssinbuffers_s/
     &     datain_15min
     &     ,datain_1hour
     &     ,datain_1day
     &     ,datain_1week
     &     ,datain_1month
     &     ,datain_1year
     &     ,datain_irr

c-----pointer back to location in global pathinput structure
      integer
     &     ptin_min15(max_inp_min)
     &     ,ptin_hour1(max_inp_hour)
     &     ,ptin_day1(max_inp_day)
     &     ,ptin_week1(max_inp_week)
     &     ,ptin_month1(max_inp_month)
     &     ,ptin_year1(max_inp_year)
     &     ,ptin_irr(max_inp_irr)

c-----number of paths in each buffer group
      integer
     &     npthsin_min15
     &     ,npthsin_hour1
     &     ,npthsin_day1
     &     ,npthsin_week1
     &     ,npthsin_month1
     &     ,npthsin_year1
     &     ,npthsin_irr

      common /dssinbuffers_i/
     &     ptin_min15
     &     ,ptin_hour1
     &     ,ptin_day1
     &     ,ptin_week1
     &     ,ptin_month1
     &     ,ptin_year1
     &     ,ptin_irr
     &     ,npthsin_min15
     &     ,npthsin_hour1
     &     ,npthsin_day1
     &     ,npthsin_week1
     &     ,npthsin_month1
     &     ,npthsin_year1
     &     ,npthsin_irr
