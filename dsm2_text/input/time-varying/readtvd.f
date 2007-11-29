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


      subroutine readtvd (
     &     inpaths_dim,block_dim
     &     ,npaths
     &     ,inpath_ptr
     &     ,indata
     &     )

      implicit none

c-----Retrieve time-varying data from DSS, if necessary, and then
c-----process further (interpolate, fillin missing, ...)

c-----common blocks

      include '../fixed/common.f'
      include '../../hydro/network.inc'
      include '../../hydro/netbnd.inc'
      include '../../hydro/chconnec.inc'

c-----subroutine arguments

      integer
     &     inpaths_dim          ! input paths array dimension
     &     ,block_dim           ! data block array dimension

      integer
     &     npaths               ! number of input paths for this interval
     &     ,inpath_ptr(inpaths_dim) ! pointer array to global input pathnames

      record /dataqual_s/
     &     indata(block_dim,inpaths_dim) ! raw input data structure array

c-----local variables

      record /dataqual_s/
     &     last_value(max_inputpaths) ! last good value for path
     &     ,tmpval

      logical check_dataqual    ! function to test quality of data
     &     ,interpolate_value   ! true to interpolate this path's final value

      integer
     &     ptr                  ! path pointer to all paths
     &     ,i                   ! path index to paths for this interval
     &     ,lnm                 ! last non-missing value index
     &     ,ndx,ndx2            ! array index to use for data now, and for interpolated value
     &     ,ndx_next,ndx_prev_curr ! array indices of data forward, and back of or at, current time step
     &     ,last_ndx_next(max_inputpaths) ! last timestep's ndx_next
     &     ,bufndx_nosync,bufndx_sync ! functions to return index in data buffer corresponding to a julian minute
     &     ,bufndx_next_nosync,bufndx_next_sync ! variables to hold this interval's bufndx
     &     ,getndx              ! get final data buffer index to use
     &     ,lnblnk              ! intrinsic last non-blank
     &     ,nmins_intvl         ! nominal number of minutes in a path's interval
     &     ,find_miss           ! function to find first missing value in data
     &     ,nvals               ! number of values in a regular-interval time block
     &     ,istat               ! status

      integer*4
     &     jul_next,jul_prev_curr ! julian minutes of data forward, and back of or at, current time step
     &     ,jul,jul2            ! julian minute corresponding to ndx, ndx2
     &     ,js_data             ! julian minute to start new data
     &     ,jm_next             ! current julian minute to next boundary
     &     ,cdt2jmin            ! character date/time to julian minute
     &     ,timediff_dat        ! time difference between two data points
     &     ,timediff_val        ! time difference between current time and data point
     &     ,incr_intvl          ! increment time interval function

      REAL*8 val1,val2            ! values used for interpolating

      character*14 datetime1,datetime2,jmin2cdt
      character*5 current_sync  ! synchronize string for model time
      character*8 per_type      ! per-aver or inst-val
      character*32 ce           ! DSS E part

      record /dataqual_s/ indata_tmp

      data last_ndx_next /max_inputpaths * 1/
      save last_ndx_next,last_value
      external jmin2cdt

 610  format(/a,' data in path:'/a
     &     /' At data time: ',a,' model time: ',a
     &     /' Using replacement value of ',1p g10.3,' from ',a,
     &     /' or a lower priority path')
 615  format(/'Missing or rejected data for interpolation in path:'/a
     &     /' At data time: ',a,' model time: ',a
     &     /' Using replacement value of ',1p g10.3,' from ',a)
 611  format(/'Missing or rejected data in path:'
     &     /' ',a
     &     /' at data time: ',a,' model time: ',a
     &     /' Cannot continue.')
 613  format(/'Software error in readtvd: Bad ndx_prev_curr for path:'
     &     /' ',a
     &     /' at data time: ',a,' model time: ',a
     &     /' Cannot continue.')
 620  format(/'Error in reading time-varying data:'
     &     /'Current time is ',a,'; earliest data time for '/a
     &     /'is ',a)
 625  format(/'Error in reading time-varying data:'
     &     /'Current time is ',a,'; all data times for '/a
     &     /' are before this time.')
 626  format(/'Error in reading time-varying data:'
     &     /'Current time is ',a,'; data synchronization request for '/a
     &     /' could not be satisfied.')
 630  format(/'Unrecognized data period type: ',a
     &     /' for path: ',a)

c-----get synchronize string for current model time, in case there is
c-----a synchronize pathinput
      if (pathinput(inpath_ptr(1)).interval(:3) .ne. 'ir-') then ! regular interval data
         jm_next=incr_intvl(julmin, pathinput(inpath_ptr(1)).e_part,
     &        nearest_boundary)
         datetime1=jmin2cdt(jm_next)
         call get_intvl(datetime1, pathinput(inpath_ptr(1)).e_part,
     &        current_sync)
      endif

c-----Check if new data needs to be read from DSS to arrays
      do i=1,npaths
         ptr=inpath_ptr(i)
         pathinput(ptr).replace=.false.
         if (pathinput(ptr).constant_value .eq. -901.) then ! get value from dss file
            if ( (julmin+pathinput(ptr).diff_julmin .ge.
     &           indata(block_dim,i).julmin .and. .not. pathinput(ptr).sync) .or.
     &           prev_julmin .eq. start_julmin) then
               js_data=julmin+pathinput(ptr).diff_julmin
               call readdss(ptr,js_data,inpaths_dim,block_dim,indata,
     &              per_type)
               if (per_type .eq. ' ') then ! none, assume instantaneous
                  pathinput(ptr).per_type=per_type_inst_val
               else if (per_type .eq. per_type_names(per_type_inst_val)) then
                  pathinput(ptr).per_type=per_type_inst_val
               else if (per_type .eq. per_type_names(per_type_per_aver)) then
                  pathinput(ptr).per_type=per_type_per_aver
               else if (per_type .eq. per_type_names(per_type_inst_cum)) then
                  pathinput(ptr).per_type=per_type_inst_cum
               else if (per_type .eq. per_type_names(per_type_per_cum)) then
                  pathinput(ptr).per_type=per_type_per_cum
               else
                  write(unit_error,630) per_type,
     &                 pathinput(ptr).path(:lnblnk(pathinput(ptr).path))
                  call exit(2)
               endif
            endif
         endif
      enddo

c-----force initial calculation of buffer indices
      bufndx_next_sync=-1
      bufndx_next_nosync=-1

      do i=1,npaths
         ptr=inpath_ptr(i)

         if (pathinput(ptr).constant_value .ne. miss_val_r) then ! use constant value
            pathinput(ptr).value=pathinput(ptr).constant_value
            tmpval.data=pathinput(ptr).value
            tmpval.flag=pathinput(ptr).value_flag
            call set_dataqual(tmpval,GOOD_DATA)
            pathinput(ptr).value_flag=tmpval.flag
            goto 100
         endif

c--------check for warmup run
         if (warmup_run) then
c-----------warmup run means:
c-----------Set all input paths to use the first value for all values,
c-----------except for those that have a generic date and are equal in
c-----------length to the tidecycle length
c-----------For qual, use only first tidefile; if not repeating, use
c-----------only first time's values from tidefile
            if (pathinput(ptr).start_dt .ne. generic_dt) then
               pathinput(ptr).fillin=fill_first
            else                ! generic data; equal tidal cycle length?
               lnm=find_miss(indata, i, block_dim, inpaths_dim) - 1
               istat=1
               ce=pathinput(ptr).e_part
               call upcase(ce)
               call zgintl(nmins_intvl, ce, nvals, istat)
               if (lnm*nmins_intvl .ne. tide_cycle_length_mins) then
                  pathinput(ptr).fillin=fill_first
               endif
            endif
         endif

c--------use value from DSS file

c--------should this path's value be interpolated?
c--------don't interpolate if not requested or gate values
         interpolate_value=(pathinput(ptr).fillin .eq. fill_interp .or.
     &        (pathinput(ptr).fillin .eq. fill_bydata .and.
     &        (pathinput(ptr).per_type .eq. per_type_inst_val .or.
     &        pathinput(ptr).per_type .eq. per_type_inst_cum))) .and.
     &        index(pathinput(ptr).path, 'GATE') .eq. 0

c--------if this path has a different start date offset than the previous
c--------path, force recalculation of buffer indices
         if (i .gt. 1 .and.
     &        pathinput(inpath_ptr(i-1)).diff_julmin .ne.
     &        pathinput(ptr).diff_julmin) then
            bufndx_next_sync=-1
            bufndx_next_nosync=-1
         endif

c--------ndx_next is index in dss buffer for data forward of current
c--------time step; depends on whether data is to be synced or not
c--------calculate this once each for synchronized and non-synchronized
c--------paths, for regular data; for irregular, calc for every path

         if (pathinput(ptr).sync) then ! synchronized data
            if (bufndx_next_sync .eq. -1 .or.
     &           pathinput(ptr).interval(:3) .eq. 'ir-') then
               ndx_next=bufndx_sync(indata, i, current_sync,
     &              pathinput(ptr).e_part, last_ndx_next(ptr), block_dim,
     &              inpaths_dim)
               bufndx_next_sync=ndx_next
            else
               ndx_next=bufndx_next_sync
            endif
         else
            if (bufndx_next_nosync .eq. -1 .or.
     &           pathinput(ptr).interval(:3) .eq. 'ir-') then
               ndx_next=bufndx_nosync(indata, julmin+pathinput(ptr).
     &              diff_julmin, i, last_ndx_next(ptr),
     &              block_dim, inpaths_dim)
               bufndx_next_nosync=ndx_next
            else
               ndx_next=bufndx_next_nosync
            endif
         endif

         if (ndx_next .eq. -1) then
c-----------either all irregular data for this path is before current time,
c-----------or synchronize didn't work
            if (.not. pathinput(ptr).sync) then ! not a synch request
c--------------if the 'last' value is wanted, finding newer data doesn't matter
               if (interpolate_value) then
                  write(unit_error,625) current_dt,pathinput(ptr).path
     &                 (:lnblnk(pathinput(ptr).path))
                  call exit(2)
               else             ! simply use last data available
                  ndx_next=block_dim ! readdss.f copies last value to end of buffer
               endif
            else                ! sync request
               write(unit_error,626) current_dt,pathinput(ptr).path
     &              (:lnblnk(pathinput(ptr).path))
               call exit(2)
            endif
         endif
         jul_next=indata(ndx_next,i).julmin

c--------if interpolation wanted, but next value is bad (and not generic
c--------data), turn off interpolation and try to replace this value
c--------later
         if (interpolate_value .and.
     &        (check_dataqual(indata(ndx_next,i),MISS_OR_REJ_DATA) .and.
     &        pathinput(ptr).start_dt .ne. generic_dt)) then
            interpolate_value=.false.
            pathinput(ptr).replace=.true.
         endif

c--------fixme: check this if statement
         if (ndx_next .eq. 1 .and.
     &        pathinput(ptr).interval(:3) .eq. 'ir-') then ! all irregular data for this path is after current time
c-----------generate error if no other priority path, else perhaps
c-----------lesser priority path has data
            if (pathinput(ptr).priority .eq. 0) then
               datetime1=jmin2cdt(indata(1,i).julmin)
               write(unit_error,620) current_dt,pathinput(ptr).path
     &              (:lnblnk(pathinput(ptr).path)),datetime1
               call exit(2)
            endif
            pathinput(ptr).replace=.true. ! later try to replace this
            call set_dataqual(indata(ndx_next,i),REJECT_DATA)
            ndx_next=2
         endif

c--------index in dss buffer for data at previous or current time step
         if (ndx_next .ge. 2) then
            ndx_prev_curr=ndx_next-1
         else if (pathinput(ptr).start_dt .eq. generic_dt) then ! previous index wraps backward for generic data
            ndx_prev_curr=1
            do while (ndx_prev_curr .lt. block_dim .and.
     &           .not. check_dataqual(indata(ndx_prev_curr+1,i),MISSING_DATA))
               ndx_prev_curr=ndx_prev_curr+1
            enddo
         else                   ! this shouldn't happen
            datetime1=jmin2cdt(indata(ndx_next,i).julmin)
            write(unit_error,613)
     &           pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &           datetime1,current_dt
            call exit(2)
         endif
c--------julian minute of previous or current data value
         jul_prev_curr=indata(ndx_prev_curr,i).julmin

c--------ndx points to which data value to use
         ndx=getndx(julmin, jul_next, jul_prev_curr, ndx_next,
     &        ndx_prev_curr, pathinput(ptr).per_type, interpolate_value)

         indata_tmp=indata(ndx,i) ! in case indata missing value is replaced later

c--------initialize last_value to use for missing data
         if (prev_julmin .eq. start_julmin) then
            last_value(ptr).data=miss_val_r
            call set_dataqual(last_value(ptr),REJECT_DATA)
         endif

c--------for interpolated value, need second value
         if (interpolate_value) then
            if (ndx .eq. ndx_next) then
               ndx2=ndx_prev_curr
            else
               ndx2=ndx_next
            endif
            jul=indata(ndx,i).julmin
            jul2=indata(ndx2,i).julmin
            timediff_dat=jul2-jul
            timediff_val=julmin - (jul-pathinput(ptr).diff_julmin)
            tmpval=indata(ndx2,i)
            val1=indata(ndx,i).data
            val2=indata(ndx2,i).data
         endif
         jul_next=indata(ndx_next,i).julmin
         jul_prev_curr=indata(ndx_prev_curr,i).julmin

c--------check for rewind of generic data
c--------missing value means start over for generic, except
c--------for synchronized data
         if (check_dataqual(indata(ndx,i),MISSING_DATA) .and.
     &        prev_julmin .ne. start_julmin .and.
     &        pathinput(ptr).start_dt .eq. generic_dt) then
            js_data=cdt2jmin(pathinput(ptr).start_dt)
c-----------synchronized data should not hit a missing value
            if (pathinput(ptr).sync) then
               write(unit_error,626) current_dt,pathinput(ptr).path
     &              (:lnblnk(pathinput(ptr).path))
               call exit(2)
            endif
c-----------update the offset between generic time and current time
            if (.not. repeating_tide) ! repeating tide recycles julmin
     &           pathinput(ptr).diff_julmin=jul_generic_dt - julmin
c-----------new buffer indices
            ndx_next=2
            ndx_prev_curr=1

            jul_next=indata(ndx_next,i).julmin
            jul_prev_curr=indata(ndx_prev_curr,i).julmin
            ndx=getndx(julmin, jul_next, jul_prev_curr, ndx_next,
     &           ndx_prev_curr, pathinput(ptr).per_type,
     &           interpolate_value)
            if (interpolate_value) then
               tmpval=indata(2,i)
               val1=indata(1,i).data
               val2=indata(2,i).data
            endif
         endif

c--------check for questionable, missing, or rejected data
         if (check_dataqual(indata(ndx,i),QUESTION_DATA) .or.
     &        check_dataqual(indata(ndx,i),MISS_OR_REJ_DATA)) then ! bad data...
            datetime1=jmin2cdt(jul_prev_curr)
c-----------continue if user requests it and good data is available
c-----------to fill in; or continue if the path is part of a priority list
c-----------(a last check will be made for bogus data just before use)
            if ( (cont_question .or. cont_missing) .and.
     &           (.not. check_dataqual(last_value(ptr),QUESTION_DATA) .and.
     &           .not. check_dataqual(last_value(ptr),MISS_OR_REJ_DATA)) .or.
     &           pathinput(ptr).priority .ge. 1) then
               if (pathinput(ptr).priority .le. 1 .and.
     &              warn_question .and.
     &              check_dataqual(indata(ndx,i),QUESTION_DATA) .and.
     &              .not. check_dataqual(last_value(ptr),MISS_OR_REJ_DATA)) then
                  datetime2=jmin2cdt(last_value(ptr).julmin)
                  write(unit_screen, 610)
     &                 'Questionable',
     &                 pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                 datetime1,current_dt,last_value(ptr).data,
     &                 datetime2
               endif
               if (pathinput(ptr).priority .le. 1 .and.
     &              warn_missing .and.
     &              check_dataqual(indata(ndx,i),MISS_OR_REJ_DATA) .and.
     &              .not. check_dataqual(last_value(ptr),MISS_OR_REJ_DATA)) then
                  datetime2=jmin2cdt(last_value(ptr).julmin)
                  write(unit_screen, 610)
     &                 'Missing or rejected',
     &                 pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                 datetime1,current_dt,last_value(ptr).data,
     &                 datetime2
               endif
               pathinput(ptr).replace=.true. ! later try to replace this
               indata(ndx,i)=last_value(ptr)
               if (interpolate_value) then
                  val1=indata(ndx,i).data
               endif
            else                ! don't continue on quest/miss/rej data, and no replacement data available
               if (pathinput(ptr).priority .eq. 0) then
                  write(unit_error, 611)
     &                 pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                 datetime1,current_dt
                  call exit(2)
               endif
            endif
         endif

c--------check for missing data of other index
         if (interpolate_value .and. check_dataqual(tmpval,MISS_OR_REJ_DATA)) then
c-----------if generic date, missing value means recycle to first value
c-----------assume that the full range of data can fit into a data block
            if (pathinput(ptr).start_dt .eq. generic_dt) then
               val2=indata(1,i).data
            else                ! not generic
               datetime1=jmin2cdt(jul2)
c--------------continue if user requests it and good data is available
c--------------to fill in, or the path is for replacement only
               if ( (cont_missing .and.
     &              .not. check_dataqual(last_value(ptr),MISS_OR_REJ_DATA)) .or.
     &              pathinput(ptr).priority .ge. 1) then
                  pathinput(ptr).replace=.true. ! later try to replace this
                  val2=last_value(ptr).data
                  if (pathinput(ptr).priority .le. 1 .and.
     &                 warn_missing) then
                     datetime2=jmin2cdt(last_value(ptr).julmin)
                     write(unit_screen, 615)
     &                    pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                    datetime1,current_dt,last_value(ptr).data,
     &                    datetime2
                  endif
               else
                  write(unit_error, 611)
     &                 pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &                 datetime1,current_dt
                  call exit(2)
               endif
            endif
         endif
         last_value(ptr)=indata(ndx,i) ! in case we wish to replace missing data

         if (interpolate_value) then
c-----------interpolate to end of time step
            pathinput(ptr).value= val1 + (val2-val1) *
     &           float(timediff_val) / float(timediff_dat)
            pathinput(ptr).value_flag=indata(ndx,i).flag
         else                   ! don't interpolate
            pathinput(ptr).value=indata(ndx,i).data
            pathinput(ptr).value_flag=indata(ndx,i).flag
         endif

         if (pathinput(ptr).start_dt .ne. generic_dt) then ! kluge upon kluge
            indata(ndx,i)=indata_tmp
         endif

 100     continue

c--------change sign if desired
         if (pathinput(ptr).sign .eq. '-') then
            pathinput(ptr).value=-abs(pathinput(ptr).value)
         else if (pathinput(ptr).sign .eq. '+') then
            pathinput(ptr).value=abs(pathinput(ptr).value)
         endif
c--------change value if desired
         if (pathinput(ptr).value_in .eq. pathinput(ptr).value)
     &        pathinput(ptr).value=pathinput(ptr).value_out

         last_ndx_next(ptr)=ndx_next

      enddo

      return
      end

      integer function bufndx_nosync(indata, jm, path, last_ndx,
     &     max_v, max_paths)

c-----Find index in julian minute array that is less than
c-----target julian minute.

      implicit none

      include '../fixed/defs.f'
      include '../fixed/misc.f'

c-----arguments and local variables
      integer
     &     last_ndx             ! bufndx value from last timestep
     &     ,max_v               ! max number of data and time values
     &     ,max_paths           ! max number of paths
     &     ,i                   ! loop index
     &     ,path                ! path index

      record /dataqual_s/
     &     indata(max_v,max_paths) ! input data structure array

      integer*4
     &     jm                   ! current julian minute

      do i=1, max_v
         if (indata(i,path).julmin .gt. jm) then
            bufndx_nosync=i
            return
         endif
      enddo

      bufndx_nosync=-1          ! all data is old

      return
      end

      integer function bufndx_sync(indata, path, sync_str, e_part,
     &     last_ndx, max_v, max_paths)

c-----Find index in julian minute array that matches the DSS part to
c-----synchronize with the current time

      implicit none

      include '../fixed/defs.f'
      include '../fixed/misc.f'

c-----arguments and local variables
      integer
     &     last_ndx             ! bufndx value from last timestep
     &     ,max_v               ! max number of data and time values
     &     ,max_paths           ! max number of paths
     &     ,i                   ! loop index
     &     ,path                ! path index

      record /dataqual_s/
     &     indata(max_v,max_paths) ! input data structure array

      character*(*)
     &     sync_str*(*)         ! string from current model to synchronize with
     &     ,e_part              ! synchronize on e_part in data time

      character*14
     &     jmv_cdt*14           ! character dates for jmv_cdt
     &     ,jmin2cdt            ! julian minute to character function

      character*5
     &     jmv_intvl            ! interval strings for jmv_cdt

c-----check last timestep's value, probably still good
      jmv_cdt=jmin2cdt(indata(last_ndx,path).julmin)
      call get_intvl(jmv_cdt, e_part, jmv_intvl)
      if (sync_str .eq. jmv_intvl) then
         bufndx_sync=last_ndx
         return
      else
         do i=1, max_v
            jmv_cdt=jmin2cdt(indata(i,path).julmin)
            call get_intvl(jmv_cdt, e_part, jmv_intvl)
            if (sync_str .eq. jmv_intvl) then
               bufndx_sync=i
               return
            endif
         enddo
      endif

      bufndx_sync=-1            ! couldn't synchronize

      return
      end

      integer function getndx(julmin, jul_next, jul_prev_curr,
     &     ndx_next, ndx_prev_curr, per_type, interpolated)

c-----Return either next or previous data index as the base index to
c-----use for correct data for this timestep.

      implicit none

      include '../fixed/misc.f'

      logical interpolated      ! true if this path's value is to be interpolated

      integer*4
     &     julmin               ! current julian minute
     &     ,jul_next,jul_prev_curr ! julian minutes of data forward, and back of or at, current time step

      integer
     &     ndx_next,ndx_prev_curr ! array indices of data forward, and back of or at, current time step
     &     ,per_type            ! per-average, instantaneous, etc.

c-----for instantaneous values, use previous or current,
c-----whether interpolated or not;
c-----for period average values, use next or current if
c-----not interpolated, use previous if interpolated
c-----fixme: for interpolated period average, really the
c-----other ndx to use should change midway thru the time period
      getndx=-9999
c-----always use prev_curr index if current time and data time are equal
      if (julmin .eq. jul_prev_curr) then
         getndx=ndx_prev_curr
      else
         if (per_type .eq. per_type_inst_val .or.
     &        per_type .eq. per_type_inst_cum) then ! instantaneous
            getndx=ndx_prev_curr
         else if (per_type .eq. per_type_per_aver .or.
     &           per_type .eq. per_type_per_cum) then ! period average
            if (.not. interpolated) then
               getndx=ndx_next
            else
               getndx=ndx_prev_curr
            endif
         endif
      endif

      return
      end

      integer function find_miss(indata, path, max_v, max_paths)

c-----Find first missing value in data vector for path

      implicit none

      include '../fixed/misc.f'
      include '../fixed/defs.f'

      logical check_dataqual    ! function to test whether data is 'missing'

      integer
     &     max_v                ! max number of data and time values
     &     ,max_paths           ! max number of paths
     &     ,path                ! path index

      record /dataqual_s/
     &     indata(max_v,max_paths) ! input data structure array

      do find_miss=1, max_v
         if (check_dataqual(indata(find_miss,path),MISS_OR_REJ_DATA)) return
      enddo

      find_miss=max_v

      return
      end

      logical function check_dataqual(value,qualflag)

c-----Check the quality of data.

      implicit none

      include '../fixed/misc.f'
      include '../fixed/defs.f'

      record /dataqual_s/ value ! data value to be tested [INPUT]
      integer qualflag          ! type of quality flag to check [INPUT]
      logical
     &     btest                ! external bit checking function

      if (qualflag .eq. SCREENED_DATA) then
         check_dataqual=btest(value.flag,SCREENED_BIT) .or.
     &        value.data .eq. -901. .or. ! missing data is considered as screened
     &        value.data .eq. -902.
      else if (qualflag .eq. GOOD_DATA) then
         check_dataqual=btest(value.flag,GOOD_BIT)
      else if (qualflag .eq. MISSING_DATA) then
         check_dataqual=value.data .eq. -901. .or.
     &        value.data .eq. -902. .or.
     &        btest(value.flag,MISSING_BIT)
      else if (qualflag .eq. QUESTION_DATA) then
         check_dataqual=btest(value.flag,QUESTION_BIT)
      else if (qualflag .eq. REJECT_DATA) then
         check_dataqual=btest(value.flag,REJECT_BIT)
      else if (qualflag .eq. MISS_OR_REJ_DATA) then
         check_dataqual=value.data .eq. -901. .or.
     &        value.data .eq. -902. .or.
     &        btest(value.flag,MISSING_BIT) .or.
     &        btest(value.flag,REJECT_BIT)
      else                      ! unknown incoming flag
         write(unit_error,*) 'Software error in check_dataqual; ',
     &        'unknown qualflag value: ', qualflag
      endif

      return
      end

      subroutine set_dataqual(value,qualflag)

c-----Set the quality data flags.

      implicit none

      include '../fixed/misc.f'
      include '../fixed/defs.f'

      record /dataqual_s/ value ! data value to be set [INPUT, OUTPUT]
      integer qualflag          ! type of quality flag to check [INPUT]

      value.flag=and(value.flag,0)
      value.flag=ibset(value.flag,SCREENED_BIT)
      if (qualflag .eq. GOOD_DATA) then
         value.flag=ibset(value.flag,GOOD_BIT)
      else if (qualflag .eq. MISSING_DATA) then
         value.data=miss_val_r
         value.flag=ibset(value.flag,MISSING_BIT)
      else if (qualflag .eq. QUESTION_DATA) then
         value.flag=ibset(value.flag,QUESTION_BIT)
      else if (qualflag .eq. REJECT_DATA) then
         value.flag=ibset(value.flag,REJECT_BIT)
      else                      ! unknown incoming flag
         write(unit_error,*) 'Software error in set_dataqual; ',
     &        'unknown qualflag value: ', qualflag
      endif

      return
      end

      subroutine find_replacement(ndx)

c-----Find a replacement input path value for the one at ndx.

      implicit none

c-----subroutine args
      integer ndx               ! input path index to replace value of

c-----include files
      include '../fixed/common.f'

c-----local variables
      logical
     &     check_dataqual       ! check data quality function

      integer
     &     p                    ! pathname index
     &     ,replace_ndx         ! replacing path index
     &     ,lnblnk              ! intrinsic

      record /dataqual_s/ pathvalue ! so as to use missing value function

 610  format(1p/'Path ',a
     &     /'value of ',g10.3
     &     /'replaced with path ',a
     &     /'value of ',g10.3,' at time ',a)

      p=ndx
      do while (pathinput(p).replace_path .gt. 0)
         replace_ndx=pathinput(p).replace_path
         pathvalue.data=pathinput(replace_ndx).value
         pathvalue.flag=pathinput(replace_ndx).value_flag
         if (pathinput(replace_ndx).replace .or. ! reject replacements that were not original data...
     &        check_dataqual(pathvalue,MISS_OR_REJ_DATA) .or.
     &        check_dataqual(pathvalue,QUESTION_DATA)) goto 100

         if (print_level .ge. 2)
     &        write(unit_screen,610)
     &        pathinput(ndx).path(:lnblnk(pathinput(ndx).path)),
     &        pathinput(ndx).value,
     &        pathinput(replace_ndx).path(:lnblnk(pathinput(ndx).path)),
     &        pathinput(replace_ndx).value,
     &        current_dt
         pathinput(ndx).value=pathinput(replace_ndx).value
         pathinput(ndx).value_flag=pathinput(replace_ndx).value_flag
         return

 100     continue
c--------that path didn't work, try next
         p=replace_ndx
      enddo

      return
      end

      subroutine get_inp_data(ptr)

c-----Get input data from buffers for computations

      implicit none

c-----common blocks

      include '../fixed/common.f'

      integer
     &     ptr                  ! pathname array index
     &     ,lnblnk              ! intrinsic

      logical
     &     check_dataqual       ! function checks quality of data

      record /dataqual_s/ dataqual

 610  format(/'No replacement path given for '
     &     /a
     &     /' however bad value encountered at model time ',a)
 612  format(/'Error in get_inp_data: Missing data in path:'
     &     /' ',a
     &     /' at model time: ',a
     &     /' Cannot continue.')
 613  format(/a/a/'at model time: ',a)

c-----check for replacement of bad value
      if (pathinput(ptr).replace) then
         dataqual.data=pathinput(ptr).value
         dataqual.flag=pathinput(ptr).value_flag
         if (pathinput(ptr).replace_path .gt. 0) then
            call find_replacement(ptr)
         else if ((.not. cont_question .and.
     &           check_dataqual(dataqual,QUESTION_DATA)) .or.
     &           check_dataqual(dataqual,MISS_OR_REJ_DATA)) then
c-----------either questionable data and user doesn't want to use it;
c-----------or missing or rejected data found
            write(unit_error,610)
     &           pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &           current_dt
            call exit(2)
         endif
      endif

c-----last check for missing data
      dataqual.data=pathinput(ptr).value
      dataqual.flag=pathinput(ptr).value_flag
      if (check_dataqual(dataqual,MISS_OR_REJ_DATA)) then
         write(unit_error, 612)
     &        pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &        current_dt
         call exit(2)
      endif

c-----warning msgs about questionable or unscreened data;
c-----check to continue run
      if (.not. check_dataqual(dataqual,SCREENED_DATA)) then
         if (warn_unchecked .or. .not. cont_unchecked) then
            write(unit_error,613) 'Warning: unchecked data: ',
     &           pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &           current_dt
         endif
         if (.not. cont_unchecked) then
            write(unit_error,*) 'Fatal error.'
            call exit(2)
         else if (warn_unchecked) then
            write(unit_error,*) 'Using current value.'
         endif
      endif

      if (check_dataqual(dataqual,QUESTION_DATA)) then
         if (warn_question .or. .not. cont_question) then
            write(unit_error,613) 'Warning: questionable data: ',
     &           pathinput(ptr).path(:lnblnk(pathinput(ptr).path)),
     &           current_dt
         endif
         if (.not. cont_question) then
            write(unit_error,*) 'Fatal error.'
            call exit(2)
         else if (warn_question) then
            write(unit_error,*) 'Using current value.'
         endif
      endif

c-----use this value for all time steps?
      if (pathinput(ptr).fillin .eq. fill_first) then
         pathinput(ptr).constant_value=pathinput(ptr).value
      endif

      return
      end
