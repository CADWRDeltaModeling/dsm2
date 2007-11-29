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

      subroutine readdss (
     &     pathnumber
     &     ,jmin
     &     ,inpaths_dim,block_dim
     &     ,indata
     &     ,per_type
     &     )

c-----Buffer time series data from DSS for model input

      implicit none

      include '../fixed/common.f'
      include 'dss.inc'

c-----arguments

      integer
     &     pathnumber           ! global pathnumber [INPUT]
     &     ,inpaths_dim         ! input paths array dimension [INPUT]
     &     ,block_dim           ! data block array dimension [INPUT]

      integer*4
     &     jmin                 ! date/time for start of data [INPUT]

      record /dataqual_s/
     &     indata(block_dim,inpaths_dim) ! data, flags, and julmin structure array [OUTPUT]

      character
     &     per_type*(*)         ! type of DSS data: PER-AVER or INST-VAL [OUTPUT]

c-----local variables

      logical
     &     lflags               ! true if data flags should be retrieved
     &     ,lfread              ! true if data flags were retrieved

      integer
     &     nvals ,istat
     &     ,pn_intvl            ! pathnumber for data block
     &     ,npath,na,nb,nc,nd,ne,nf
     &     ,i
     &     ,juls,istime,jule,ietime ! start/end date/time
     &     ,itimes(irrs)        ! irregular date/times of values relative to jbdate
     &     ,jbdate              ! julian base date of irregular data
     &     ,kheadu              ! number of headers to retrieve
     &     ,nheadu              ! number of headers actually retrieved
     &     ,inflag              ! flag to retrieve values before/after time block
     &     ,iofset              ! time offset in minutes
     &     ,icomp               ! compression method

      integer*4
     &     jul_jbdate           ! jbdate in julian minutes
     &     ,cdt2jmin            ! char date to julian minute function
     &     ,ihm2m,inctim        ! DSS functions
     &     ,nmins               ! number of minutes in this data interval
     &     ,flags(maxinpsize)   ! 32-bit data flags
     &     ,incr_intvl          ! increment julian minute by interval function
     &     ,jmin2               ! test used for jmin

      real*4 values(maxinpsize) ! data values
     &     ,headu               ! data headers

      character
     &     csdt*14              ! date/time for start of data
     &     ,csdate*9            ! starting date of data block
     &     ,cstime*4            ! starting time of data block
     &     ,cedt*14             ! nominal ending datetime of data block
     &     ,cunits*8
     &     ,ca*32, cb*32, cc*32, cd*32, ce*32, cf*32
     &     ,jmin2cdt*14         ! julian min to char date/time function

      data lflags /.true./      ! get data flags
     &     ,kheadu /0/          ! don't get data headers
     &     ,inflag /3/          ! retrieve values before & after time block
     &     ,icomp /-1/          ! no compression

 610  format(/'Error on data read: ',a
     &     /' ',a,' to ',a,' istat=',i2)
 620  format(/'Invalid ',a,' date: ',a)

c-----Break up the pathnames
      call chrlnb(pathinput(pathnumber).path,npath)
      call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne,
     &     cf, nf, pathinput(pathnumber).path, npath, istat)

c-----convert starting date/time to the standard interval
      istat=1                   ! get minutes in interval, given E part
      call zgintl(nmins,ce,nvals,istat)
      if (istat .eq. 0) then    ! regular time-series convention
c--------jmin must be set to an interval end...if in the middle of an
c--------interval, use the previous end, else if at an end, use that
         jmin2=incr_intvl(jmin,ce,NEAREST_BOUNDARY)
         if (jmin2 .ne. jmin) then ! not at interval end
            jmin=incr_intvl(jmin,'-'//ce,NEAREST_BOUNDARY)
         endif
      endif

      csdt=jmin2cdt(jmin)
      csdate=csdt(1:9)
      cstime=csdt(11:14)
c-----julian day/minute for start of data time
      call datjul(csdate, juls, istat)
      istime=ihm2m(cstime)
      if (istat .ne. 0 .or. istime .eq. -1) then
         write(unit_error,620) 'starting',csdt
         call exit(2)
      endif

      pn_intvl=pathinput(pathnumber).intvl_path

      nvals=block_dim

c-----Read the time block
      if (pathinput(pathnumber).interval(1:3) .ne. 'ir-') then ! regular time series
         call zrrtsx(ifltab_in(1,pathinput(pathnumber).ndx_file),
     &        pathinput(pathnumber).path, csdate, cstime,
     &        nvals, values, flags, lflags, lfread, cunits, per_type, headu,
     &        kheadu, nheadu, iofset, icomp, istat)

         cedt=jmin2cdt(cdt2jmin(csdt)+nvals*nmins) ! nominal end of data block

         if (istat .gt. 10) then
            write(unit_error, 610) pathinput(pathnumber).path(1:npath),
     &           csdt,cedt,istat
            call exit(2)
         endif

         do i=1, nvals
c-----------set julian minute for each data value
            istat=inctim(nmins, 0, i-1, juls, istime, jule, ietime)
            indata(i,pn_intvl).julmin=jule*24*60+ietime
c-----------fill data array
            indata(i,pn_intvl).data=values(i)
c-----------data quality flags, use from DSS file or user-specified value?
            if (pathinput(pathnumber).use_flag .eq. miss_val_i) then
               indata(i,pn_intvl).flag=flags(i)
            else
               indata(i,pn_intvl).flag=pathinput(pathnumber).use_flag
            endif
         enddo
      else                      ! irregular time series
c--------julian day/minute for end of data:
c--------use end of run adjusted for data offset, but if repeating
c--------tide, use data start + tide cycle length
         if (.not. repeating_tide) then
            jule=(end_julmin+pathinput(pathnumber).diff_julmin)/(24*60)
            ietime=end_julmin+pathinput(pathnumber).diff_julmin-jule*24*60
         else
            jule=(jmin+tide_cycle_length_mins)/(24*60)
            ietime=jmin+tide_cycle_length_mins-jule*24*60
         endif
c--------zritsx doesn't initialize flags to zero
         do i=1,maxinpsize
            flags(i)=0
         enddo
         call zritsx(ifltab_in(1,pathinput(pathnumber).ndx_file),
     &        pathinput(pathnumber).path,
     &        juls, istime, jule, ietime,
     &        itimes, values, irrs, nvals, jbdate,
     &        flags, lflags, lfread, cunits, per_type,
     &        headu, kheadu, nheadu, inflag, istat)

         jul_jbdate=jbdate*24*60
         cedt=jmin2cdt(itimes(max(nvals,1))+jul_jbdate)

         if (istat .gt. 10) then
            write(unit_error, 610) pathinput(pathnumber).path(1:npath),
     &           csdt,cedt,istat
            call exit(2)
         endif

         do i=1, block_dim
            indata(i,pn_intvl).julmin=end_julmin+1
            indata(i,pn_intvl).data=-901.0
            indata(i,pn_intvl).flag=0
         enddo

         do i=1, nvals
            indata(i,pn_intvl).julmin=itimes(i)+jul_jbdate
            indata(i,pn_intvl).data=values(i)
c-----------data quality flags, use from DSS file or user-specified value?
            if (pathinput(pathnumber).use_flag .eq. miss_val_i) then
               indata(i,pn_intvl).flag=flags(i)
            else
               indata(i,pn_intvl).flag=pathinput(pathnumber).use_flag
            endif
         enddo
c--------if less values returned than block_dim, fill in remainder of data
c--------array with last value so readtvd doesn't keep re-reading data.
         if (nvals .gt. 0) then
            do i=nvals+1, block_dim
               indata(i,pn_intvl).julmin=indata(nvals,pn_intvl).julmin
               indata(i,pn_intvl).data=indata(nvals,pn_intvl).data
               indata(i,pn_intvl).flag=indata(nvals,pn_intvl).flag
            enddo
         endif
      endif

      call upcase(per_type)

      return
      end
