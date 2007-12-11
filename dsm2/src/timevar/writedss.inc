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

c-----Define arrays to be used as buffers for data from DSS.
c-----Use one array for each kind of data interval for DSS.

c-----scratch file unit numbers
      integer
     &     unit_min15
     &     ,unit_hour1
     &     ,unit_day1
     &     ,unit_week1
     &     ,unit_month1
     &     ,unit_year1

      parameter (
     &     unit_min15=31
     &     ,unit_hour1=32
     &     ,unit_day1=33
     &     ,unit_week1=34
     &     ,unit_month1=35
     &     ,unit_year1=36
     &     )

c-----output data buffers
c-----the zero'th location is used to store the value from the
c-----previous model time step, for interpolation with odd time steps
      real*8
     &     dataout_minutes15(0:mins15,max_out_min) ! 15 minute data for one day
     &     ,dataout_hours(0:hrs,max_out_hour) ! hourly data for one month
     &     ,dataout_days(0:dys,max_out_day) ! daily data for one year
     &     ,dataout_weeks(0:wks,max_out_week) ! weekly data for one decade
     &     ,dataout_months(0:mths,max_out_month) ! monthly data for one decade
     &     ,dataout_years(0:yrs,max_out_year) ! yearly data for one century

      common /dssoutbuffers_r/  dataout_minutes15, dataout_hours,
     &     dataout_days, dataout_weeks, dataout_months, dataout_years

c-----pointer back to location in pathoutput structure
      integer*4
     &     ptout_min15(max_out_min)
     &     ,ptout_hour1(max_out_hour)
     &     ,ptout_day1(max_out_day)
     &     ,ptout_week1(max_out_week)
     &     ,ptout_month1(max_out_month)
     &     ,ptout_year1(max_out_year)

c-----the starting date/time of each block of data, in julian minutes
      integer*4
     &     julstout_minutes15
     &     ,julstout_hours1
     &     ,julstout_days1
     &     ,julstout_weeks1
     &     ,julstout_months1
     &     ,julstout_years1

c-----the number of values that went into an average value over the
c-----time interval
      integer
     &     nave_min15(max_out_min)
     &     ,nave_hour1(max_out_hour)
     &     ,nave_day1(max_out_day)
     &     ,nave_week1(max_out_week)
     &     ,nave_month1(max_out_month)
     &     ,nave_year1(max_out_year)

c-----number of paths in each buffer group
      integer
     &     npthsout_min15
     &     ,npthsout_hour1
     &     ,npthsout_day1
     &     ,npthsout_week1
     &     ,npthsout_month1
     &     ,npthsout_year1

c-----whether each buffer group needs to write to a tmp binary file
      logical
     &     need_tmpfile_min15
     &     ,need_tmpfile_hour1
     &     ,need_tmpfile_day1
     &     ,need_tmpfile_week1
     &     ,need_tmpfile_month1
     &     ,need_tmpfile_year1

c-----scratch output filenames
      character
     &     scratch_file_array*80(6)

      common /dssoutbuffers_i/
     &     ptout_min15
     &     ,ptout_hour1
     &     ,ptout_day1
     &     ,ptout_week1
     &     ,ptout_month1
     &     ,ptout_year1
     &     ,julstout_minutes15
     &     ,julstout_hours1
     &     ,julstout_days1
     &     ,julstout_weeks1
     &     ,julstout_months1
     &     ,julstout_years1
     &     ,nave_min15
     &     ,nave_hour1
     &     ,nave_day1
     &     ,nave_week1
     &     ,nave_month1
     &     ,nave_year1
     &     ,npthsout_min15
     &     ,npthsout_hour1
     &     ,npthsout_day1
     &     ,npthsout_week1
     &     ,npthsout_month1
     &     ,npthsout_year1
     &     ,need_tmpfile_min15
     &     ,need_tmpfile_hour1
     &     ,need_tmpfile_day1
     &     ,need_tmpfile_week1
     &     ,need_tmpfile_month1
     &     ,need_tmpfile_year1

      common /dssoutbuffers_c/
     &     scratch_file_array
