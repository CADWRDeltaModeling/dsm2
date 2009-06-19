C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>


      integer*4 function incr_intvl(jmins, e_part, boundary)

c!    Given a julian minute corresponding to a DSS character date
c!    (e.g. 05JUN1993 0510) and a DSS E part (e.g. 1HOUR, 2MON)
c!    char string, return the next interval in julian minutes
c!    according to the boundary flag:

c!    TO_BOUNDARY:
c!    to the time boundary of the interval, times number of intervals
c!    e.g. 05JUN1993 0600, 30JUL1993 2400

c!    IGNORE_BOUNDARY:
c!    increment number of intervals, ignoring boundary
c!    e.g. 05JUN1993 0610, 05AUG1993 0510

c!    NEAREST_BOUNDARY:
c!    to the nearest boundary, ignoring number of intervals
c!    e.g. 05JUN1993 0600, 30JUN1993 2400

c!    If the e_part starts with `-', then the interval is backwards
c!    in time, e.g. for -1HOUR, -2MON:
c!    TO_BOUNDARY:	05JUN1993 0500, 30APR1993 2400
c!    IGNORE_BOUNDARY:	05JUN1993 0410, 05APR1993 0510
c!    NEAREST_BOUNDARY:	05JUN1993 0500, 31MAY1993 2400

c!    If TO_BOUNDARY is requested, and jmins is already on the boundary,
c!    then jmins will still be incremented by the interval requested.
c!    But if NEAREST_BOUNDARY is requested, and jmins is on a boundary,
c!    then jmins will be returned for a positive interval; the previous
c!    boundary will be returned for a negative interval.   

      use constants
      implicit none


      logical
     &     on_boundary          ! true if already on interval boundary
     &     ,keepit              ! statement function

      character
     &     e_part*(*)           ! DSS style interval [INPUT]
     &     ,e_part_tmp*80       ! temporary e_part
     &     ,interval*80         ! DSS interval (e.g. HOUR)

      integer*4
     &     jmins                ! starting julian minute [INPUT]
     &     ,iymdjl              ! DSS function
     &     ,nom_mins            ! nominal number of minutes in an interval

      integer
     &     boundary             ! how to handle boundary [INPUT]
     &     ,number              ! number (e.g. 1, 15)
     &     ,juls,jule           ! starting and ending julian day
     &     ,istime,ietime       ! starting and ending minutes past midnight
     &     ,jliymd              ! convert julian day to year,month,day DSS function
     &     ,inctim              ! DSS function to increment time
     &     ,idaywk              ! DSS function to return day of week
     &     ,dayofweek           ! day of week index
     &     ,istat               ! return status
     &     ,iy,imon,id,ih,imin  ! integer year, month, day, hour, minute
     &     ,i                   ! loop index
     &     ,ibegf(10)           ! beginning position of each field in line
     &     ,ilenf(10)           ! length of each field in line
     &     ,idelmt(10)          ! type of delimiter for each field
     &     ,idelmp(10)          ! position in delimiter string of delimiter
     &     ,itbl(128)           ! needed by findlm routine
     &     ,nfields             ! number of fields found
     &     ,len                 ! returns declared length of string
     &     ,lens                ! length of string
     &     ,number_sign         ! sign of number of units (+ or -)
     &     ,sign                ! intrinsic
     &     ,nvals               ! number of values in DSS data block

c-----statement function to see whether to keep this new date,
c-----or to increment further
      keepit(boundary,number_sign,on_boundary) =
     &     boundary .eq. NEAREST_BOUNDARY .and. (
     &     (number_sign .gt. 0 .and. on_boundary) .or.
     &     (number_sign .lt. 0 .and. .not. on_boundary) )

c-----could be multiple intervals (e.g. 3DAY 5HOUR); process
c-----each individually

c-----use spaces, tabs, and underscore as delimiters
      e_part_tmp=e_part

      call setdlm(3,' ',1,0,itbl) ! don't use string delimiters
      call setdlm(2, '	 _',1,3,itbl) ! space, tab, and underscore
      nfields=10
      lens=len(e_part_tmp)
      call findlm(e_part_tmp,1,lens,nfields,ibegf,ilenf,idelmt,idelmp,itbl)

      call locase(e_part_tmp)

      jule=jmins/(24*60)        ! julian days
      ietime=mod(jmins,24*60)   ! minutes past midnight

      do i=1,nfields

         juls=jule
         istime=ietime

c--------Separate the number of units from the units (e.g. 3 MON).
c--------For irregular data (IR-*) the unit is 1.

c--------note: 01JAN1993 0000 == 31DEC1992 2400

         call split_epart(e_part_tmp(ibegf(i):ibegf(i)+ilenf(i)),number,
     &        interval)
         number_sign=sign(1,number)
         if (boundary .eq. NEAREST_BOUNDARY) number=number_sign
         on_boundary=.false.

         istat=1                ! get nominal number of minutes given e part
         call upcase(interval)
         call zgintl(nom_mins, interval, nvals, istat)
         call locase(interval)

         if (boundary .eq. IGNORE_BOUNDARY) then
            istat=inctim(nom_mins, 0, number, juls, istime, jule, ietime)
         else                   ! respect boundary
            istat=jliymd(juls,iy,imon,id) ! get integer year, month, day
            ih=istime/60
            imin=mod(istime,60)

            if (index(interval,'decade') .ne. 0) then
               if (mod(iy,10) .eq. 0 .and. imon .eq. 1 .and.
     &              id .eq. 1 .and. ih .eq. 0 .and. imin .eq. 0)
     &              on_boundary=.true. ! jmins on decade boundary
               iy=(iy/10)*10    ! e.g. 1993 -> 1990, 1990 -> 1990
               imon=1
               id=1
               ih=0
               imin=0
c--------------now back to same or previous decade boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=iymdjl(iy,imon,id)
                  ietime=0
                  goto 100
               else
                  juls=iymdjl(iy,imon,id)
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,
     &                 ietime)
               endif
            else if (index(interval,'year') .ne. 0) then
               if (imon .eq. 1 .and. id .eq. 1 .and. ih .eq. 0 .and.
     &              imin .eq. 0) on_boundary=.true. ! jmins on year boundary
               imon=1
               id=1
               ih=0
               imin=0
c--------------now back to same or previous year boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=iymdjl(iy,imon,id)
                  ietime=0
                  goto 100
               else
                  juls=iymdjl(iy,imon,id)
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,
     &                 ietime)
               endif
            else if (index(interval,'mon') .ne. 0) then
               if (id .eq. 1 .and. ih .eq. 0 .and.
     &              imin .eq. 0) on_boundary=.true. ! jmins on month boundary
               id=1
               ih=0
               imin=0
c--------------now back to same or previous month boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=iymdjl(iy,imon,id)
                  ietime=0
                  goto 100
               else
                  juls=iymdjl(iy,imon,id)
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,
     &                 ietime)
               endif
            else if (index(interval,'week') .ne. 0) then
               dayofweek=idaywk(juls)
               if (dayofweek .eq. 1 .and. ih .eq. 0 .and. imin .eq. 0)
     &              on_boundary=.true. ! jmins on week boundary
               juls=juls-dayofweek+1
               istat=jliymd(juls,iy,imon,id) ! get integer year, month, day
               ih=0
               imin=0
c--------------now back to same or previous week boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=0
                  goto 100
               else
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,
     &                 ietime)
               endif
            else if (index(interval,'day') .ne. 0) then
               if (ih .eq. 0 .and. imin .eq. 0)
     &              on_boundary=.true. ! jmins on day boundary
               ih=0
               imin=0
c--------------now back to same or previous day boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=0
                  goto 100
               else
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,
     &                 ietime)
               endif
            else if (index(interval,'hour') .ne. 0) then
               if (imin .eq. 0) on_boundary=.true. ! jmins on hour boundary
               imin=0
c--------------now back to same or previous hour boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=ih*60
                  goto 100
               else
                  istime=ih*60
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,
     &                 ietime)
               endif
            else if (index(interval,'min') .ne. 0) then
               if (mod(imin,15) .eq. 0) on_boundary=.true. ! jmins on 15min boundary
               if (interval(:5) .eq. '15min') imin=(imin/15)*15
c--------------now back to same or previous 15min boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=ih*60 + imin
                  goto 100
               else
                  istime=ih*60 + imin
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,
     &                 ietime)
               endif
            else
               incr_intvl=miss_val_i
               return
            endif
         endif

 100     continue

      enddo

      incr_intvl=jule*24*60 + ietime

      return

      end

      subroutine split_epart(e_part,number,interval)

c-----Split a DSS E part into its interval (e.g. HOUR, DECADE)
c-----and number of intervals (e.g. 1, 5).
c-----Assume that every interval is unitary (e.g. 6HOUR means
c-----six 1HOUR intervals, not a single interval of 6HOUR).
c-----However 15MIN means a single 15MIN interval.
      use constants
      implicit none

      character e_part*(*)      ! DSS E part [INPUT]
     &     ,interval*(*)        ! DSS interval [RETURN]
     &     ,e_part_tmp*80       ! temporary e part
     &     ,char_list*12        ! list of chars to scan

      integer number            ! number of intervals [RETURN]
     &     ,ielen               ! length of e_part
     &     ,ipos2               ! which char found in iscan
     &     ,ilast               ! position of last digit in e_part
     &     ,iscan               ! DSS char scan function

      data char_list /'0123456789+-'/

      e_part_tmp=e_part
      call locase(e_part_tmp)

      if (e_part_tmp(:3) .eq. 'ir-') then ! irregular interval
         number=1
         interval=e_part_tmp
         return
      else
         ielen=len(e_part_tmp)
         ilast=iscan(e_part_tmp, ielen, -ielen, char_list, 1, 10, ipos2)
c--------handle e.g. 'hour' (w/o number) correctly
         if (ilast .eq. 0) then
            number=1
         else
            read(e_part_tmp(:ilast),'(i5)', err=600) number
         endif
      endif
      interval=e_part_tmp(ilast+1:)
c-----check for valid interval
      if (
     &     index(interval,'min') .gt. 0 .or.
     &     index(interval,'hour') .gt. 0 .or.
     &     index(interval,'day') .gt. 0 .or.
     &     index(interval,'week') .gt. 0 .or.
     &     index(interval,'mon') .gt. 0 .or.
     &     index(interval,'year') .gt. 0 .or.
     &     index(interval,'dec') .gt. 0
     &     ) then
c--------for minutes, treat 15MIN intervals as unit
         if (mod(number,15) .eq. 0 .and. interval(:3) .eq. 'min') then
            number=number/15
            interval='15min'
         else
            interval='1'//interval
         endif

         return
      endif

 600  continue                  ! here for error getting number of intervals
      number=miss_val_i
      interval=' '
      return

      end

      subroutine get_intvl(cdatx, e_part, cdate_intvl)

c-----Given a character date/time string (e.g. 05JAN1996 0530), and a
c-----DSS E part interval (e.g. 1HOUR, 1MONTH), return the
c-----corresponding portion from the date.
      use constants
      implicit none


c-----subroutine arguments

      character*(*)
     &     cdatx                ! date/time string [IN]
     &     ,e_part              ! DSS E part interval [IN]
     &     ,cdate_intvl         ! date/time portion corresponding to interval [OUT]

c-----local variables

      integer
     &     number               ! number prefix of E part

      character*15
     &     interval             ! E part minus number prefix

      call split_epart(e_part, number, interval)

      if (interval .eq. '15MIN' .or.
     &     interval .eq. '15min') then
         cdate_intvl=cdatx(13:14)
      else if (interval .eq. '1HOUR' .or.
     &        interval .eq. '1hour') then
         cdate_intvl=cdatx(11:12)
      else if (interval .eq. '1DAY' .or.
     &        interval .eq. '1day') then
         cdate_intvl=cdatx(1:2)
      else if (interval .eq. '1MON' .or.
     &        interval .eq. '1mon') then
         cdate_intvl=cdatx(3:5)
      else if (interval .eq. '1YEAR' .or.
     &        interval .eq. '1year') then
         cdate_intvl=cdatx(6:9)
      else                      ! couldn't find specified interval
         cdate_intvl=miss_val_c
      endif

      return
      end

      integer*4 function cdt2jmin(cdatx)

c-----Convert from character date/time to julian minute
      use constants
      implicit none

      character*(*) cdatx       ! character date/time (e.g. 05JUN1983 0510) (input)

      integer*4
     &     julday               ! days since 31dec1899
     &     ,minute              ! minutes past midnight
     &     ,ihm2m               ! DSS function
     &     ,ierror              ! error flag

      call datjul(cdatx(1:9), julday, ierror)
      if (cdatx(11:14) .eq. ' ') then ! assume empty time means 0000
         minute=0
      else
         minute=ihm2m(cdatx(11:14))
      endif
      if (ierror .ne. 0 .or. minute .eq. -1) then
         cdt2jmin=miss_val_i
         goto 900
      endif

      cdt2jmin=julday*24*60 + minute
      return

 900  continue
      return

      end

      character*14 function jmin2cdt(julmin)

c-----Convert from julian minute to character date/time
      use constants
      implicit none

      integer*4 julmin          ! minutes since 31dec1899 2400

      integer*4
     &     julday               ! days since 31dec1899
     &     ,minute              ! minutes past midnight
     &     ,ndate               ! number of characters in date
     &     ,m2ihm               ! DSS function
     &     ,itime               ! integer time
     &     ,jtmp,itmp           ! temporary julday & minutes

      jmin2cdt='              '
      jtmp=julmin/(24*60)       ! julday
      itmp=mod(julmin,24*60)    ! minutes past midnight
      call datcll(jtmp,itmp,julday,minute)

      call juldat(julday, 104, jmin2cdt(1:9), ndate)
      itime=m2ihm(minute,jmin2cdt(11:14))

      if (itime .eq. -1) then
         jmin2cdt(1:1)=miss_val_c
      endif

      return

      end

      character*19 function jmin2iso(julmin)

c-----Convert from julian minute to character date/time
c     in ISO compliant format yyyy-mmm-dd hh:mm::ss
c     with no military time conversion (and 00:00 is 
c     always used instead of 2400)
      implicit none

      integer*4 julmin          ! minutes since 31dec1899 2400
	integer*4 julday
	integer*4 minute
	integer*4 y,m,d,ihr,imin
      character*14 jmin2cdt

      julday=julmin/(24*60)       ! julday
      minute=mod(julmin,24*60)    ! minutes past midnight

	call jliymd(julday,y,m,d)
      ihr=minute/60
	imin=mod(minute,60)
	write(jmin2iso,231)y,m,d,ihr,imin
 231  format (i4,'-',i2.2,'-',i2.2,' ',i2.2,':',i2.2,':00')
      return
      end
      character*80 function dates2diff(cdate1,cdate2)

c-----Given 2 DSS date/times, return the difference between them
c-----in terms of years, months, days, hour, and minutes

      implicit none

      character*(*)
     &     cdate1,cdate2        ! earliest and latest date/time pair

      character*3
     &     cyears               ! number of years difference
     &     ,cdays               ! number of days difference
     &     ,chours              ! number of hours difference
     &     ,cmins               ! number of minutes difference

      integer*4
     &     jmin1,jmin2          ! julian minute for cdate1 and cdate2
     &     ,jdiff               ! difference
     &     ,cdt2jmin            ! convert from char date to julian minute

      integer
     &     nyears               ! number of years difference
     &     ,ndays               ! number of days difference
     &     ,nhours              ! number of hours difference
     &     ,nmins               ! number of minutes difference

      jmin1=cdt2jmin(cdate1)
      jmin2=cdt2jmin(cdate2)

      jdiff=jmin2-jmin1

      nyears=jdiff/(365*24*60)
      jdiff=jdiff-nyears*(365*24*60)
      ndays=jdiff/(24*60)
      jdiff=jdiff-ndays*(24*60)
      nhours=jdiff/60
      nmins=jdiff-nhours*60

      write(cyears,'(i3.3)') nyears
      write(cdays,'(i3.3)') ndays
      write(chours,'(i3.3)') nhours
      write(cmins,'(i3.3)') nmins

      dates2diff=
     &     cyears // 'years ' //
     &     cdays // 'days ' //
     &     chours // 'hours ' //
     &     cmins // 'mins '

      return
      end

      character*14 function diff2dates(start_date, cintvls)

c-----Given a character DSS start date, and a character interval,
c-----return the DSS character end date.
      use constants
      implicit none


      character*(*)
     &     start_date           ! DSS start date
     &     ,cintvls             ! list of intervals (e.g. 1DAY 2HOUR 15MIN)

      character*14 jmin2cdt

      integer*4 cdt2jmin, incr_intvl

      external incr_intvl,cdt2jmin

      diff2dates=jmin2cdt(incr_intvl(cdt2jmin(start_date),cintvls,
     &     IGNORE_BOUNDARY))

      return
      end

      integer function loccarr(cstring,carr,dim_carr,exact)

c-----Locate a string in a character array, given array dimension.
c-----Returns array index if found.  If not found return negative
c-----value of first blank array index. Input logical exact controls
c-----whether exact or substring match requested.

      implicit none

      logical exact             ! if true, exact match, else substring match

      integer dim_carr          ! dimension of carr

      character*(*)
     &     cstring              ! input string
     &     ,carr(dim_carr)      ! input character array

      integer i                 ! loop index
     &     ,lstring             ! length of nonblank part of cstring
     &     ,lnblnk              ! last nonblank function
      
      i=1
      if (exact) then           ! exact match
         do while (i .le. dim_carr)
            if (carr(i) .eq. cstring .or. carr(i) .eq. ' ') exit
            i=i+1
         enddo
      else                      ! substring match
         lstring=lnblnk(cstring)
         do while (i .le. dim_carr) 
            if (carr(i)(:lstring) .eq. cstring(:lstring) 
     &          .or. 
     &          carr(i) .ne. ' ')exit
            i=i+1
         enddo
      endif

      if (i .gt. dim_carr) then
         loccarr=0
      else if (carr(i) .eq. ' ') then
         loccarr=-i
      else
         loccarr=i
      endif

      return
      end

      character*200 function get_substring(cstring,delimiter)

c-----Given a string of delimiter separated substrings, return
c-----the first substring, and remove the first substring
c-----from the main string.  For whitespace separated substrings,
c-----set delimiter to a single blank (' ').

      implicit none

      integer ndx1,ndx2         ! delimiter indices
     &     ,lnblnk              ! last non-blank intrinsic

      character*(*) cstring     ! main string [INPUT and OUTPUT]
     &     ,delimiter*1         ! single character delimiter [INPUT]

      if (delimiter .eq. ' ') then
c--------delimiter is blank: look for and discard multiple blanks
         ndx1=1
         do while(cstring(ndx1:ndx1) .eq. delimiter)
            ndx1=ndx1+1         ! ndx1 will be first nonblank character
         enddo
         ndx2=index(cstring(ndx1:),delimiter)+ndx1-1 ! ndx2 is last nonblank character in substring
      else                      ! delimiter is not a blank
c--------look for delimiter, if none, for last non-blank
         ndx1=1
         ndx2=index(cstring,delimiter)-1
      endif

      if (ndx2 .eq. -1) ndx2=lnblnk(cstring)
      if (ndx2 .ge. 1) then
         get_substring=cstring(ndx1:ndx2)
         cstring=cstring(ndx2+2:len(cstring))
      else                      ! empty main string
         get_substring=' '
         cstring=' '
      endif

      return
      end



      logical function lscan(string,chars)

c-----Scan STRING for occurrence of any characters in CHARS.

      implicit none

      character*(*)
     &     string               ! string to search in [INPUT]
     &     ,chars               ! single characters to look for [INPUT]

      integer
     &     i                    ! loop index

      lscan=.false.
      
      do i=1,len(chars)
         if (index(string,chars(i:i)) .gt. 0) then
            lscan=.true.
            return
         endif
      enddo

      return
      end

      subroutine obj2obj_direc(flow,obj2obj,from_object,to_object)

c-----Given a flow value and an instance of an obj2obj array element,
c-----return the from (Q>0) and the to object (Q<0), depending
c-----on the flow direction (positive or negative).

      use type_defs
      use constants
      implicit none


c-----args

      real*4 flow                 ! flow value
      type(obj2obj_t) obj2obj ! the obj2obj array element
      type(from_to_t) :: from_object,to_object ! the from and to substructure

      if (flow .ge. 0) then
         from_object=obj2obj.from_obj
         to_object=obj2obj.to_obj
      else
         from_object=obj2obj.to_obj
         to_object=obj2obj.from_obj
      endif

      return
      end

      logical function version_fn(current_version, test_version)

c-----Return true if test_version is 'newer' or same as current_version;
c-----false if older than current_version, or either current or test are blank

      use constants
      implicit none


c-----arguments

      character*(*)
     &     current_version      ! current version number string
     &     ,test_version        ! test version number string

      integer dot_group         ! index of which dot group we're testing
     &     ,current_dot_number  ! dot number of current version
     &     ,test_dot_number     ! dot number of test version
     &     ,get_dot_number_fn   ! function to get dot number from version string
     &     ,lnblnk              ! intrinsic

c-----test for empty version numbers
      version_fn=.false.
      if (lnblnk(current_version) .le. 0) return
      if (lnblnk(test_version) .le. 0) return

c-----The version number has a major and minor number, separated by a
c-----dot.  Each number should be treated as an integer.  Each number
c-----will be referred to as a dot-number, with leftward numbers having
c-----precedence.

c-----If the test_version major number is greater than the
c-----current_version major number, the test_version is more recent.
c-----If the test_version major number is the less than the
c-----current_version major number, the test_version is less recent.
c-----If the test_version major number is the same as the
c-----current_version major number, the minor numbers must be compared,
c-----similar to the major number testing described above.

c-----loop over each dot-number, comparing left-to-right, exiting when
c-----similar dot numbers differ, or when the end is reached.

      dot_group=1
      current_dot_number=get_dot_number_fn(current_version,dot_group)
      test_dot_number=get_dot_number_fn(test_version,dot_group)
      do while (current_dot_number .ne. miss_val_i .and.
     &     test_dot_number .ne. miss_val_i)
         if (current_dot_number .gt. test_dot_number) then
            version_fn=.true.
            return
         endif
         if (current_dot_number .lt. test_dot_number) then
            version_fn=.false.
            return
         endif
         dot_group=dot_group+1
         current_dot_number=get_dot_number_fn(current_version,dot_group)
         test_dot_number=get_dot_number_fn(test_version,dot_group)
      enddo
      version_fn=.true.
      return

      end

      integer function get_dot_number_fn(version,dot_group)

c-----Given a dotted version number string (e.g. '5.67.23'), and a dot
c-----group (e.g. 2, counting from left), return the version number for
c-----that group number (e.g. 67).
      use constants
      implicit none


c-----arguments
      
      character*(*) version     ! version number string [IN]
      integer dot_group         ! dot group [IN]

c-----local variables
      
      integer
     &     version_len          ! length of version char string
     &     ,max_fields          ! max dot fields

      parameter (
     &     max_fields=10
     &     )

      character*10 dot_char     ! string corresponding to dot number at the dot grouping

c-----DSS subroutine variables
      integer
     &     ibegf(max_fields)    ! beginning position of each field in line
     &     ,ilenf(max_fields)   ! length of each field in line
     &     ,idelmt(max_fields)  ! type of delimiter for each field
     &     ,idelmp(max_fields)  ! position in delimiter string of delimiter
     &     ,itbl(128)           ! needed by findlm routine; for most inputs
     &     ,nfields             ! number of delimited fields found

c-----use dot as delimiter
      call setdlm(3,' ',1,0,itbl) ! don't use string delimiters
      call setdlm(2,' ',1,0,itbl) ! don't use type 2 delimiters
      call setdlm(1,'.',1,1,itbl) ! set dot as delimiter

      version_len=len(version)
      call findlm(version,1,version_len,nfields,ibegf,ilenf,idelmt,
     &     idelmp,itbl)

      if (dot_group .le. 0 .or.
     &     dot_group .gt. nfields) then ! illegal dot group requested
         get_dot_number_fn=miss_val_i
         return
      endif
      
      dot_char=version(ibegf(dot_group):ibegf(dot_group)+ilenf(dot_group)-1)
      read(dot_char,'(i10)') get_dot_number_fn
      return

      end

      subroutine get_command_args(init_input_file, SimName)
c-----get optional starting input file from command line,
c-----then from environment variables,
c-----then default
      use runtime_data
      implicit none

c-----arguments
      character SimName*(*)     ! ModelID in RDB

      character
     &     init_input_file*(*)  ! initial input file on command line [optional]


c-----local variables
      logical
     &     exst                 ! true if file exists

      integer
     &     iarg                 ! argument index
     &     ,lnblnk

      character*150 CLA         ! command line args

      call getarg(1,CLA)
      if (lnblnk(CLA) .eq. 0 .or.
     &     CLA(:2) .eq. "-v" .or.
     &     CLA(:2) .eq. "-V" .or.
     &     CLA(:2) .eq. "-h" .or.
     &     CLA(:2) .eq. "-H") then ! print version, usage, quit
         print *, 'DSM2-' // trim(dsm2_name) // ' ', dsm2_version
         print *, 'Usage: ' // trim(dsm2_name) // ' input-file '
         call exit(1)
      else                      ! command line arg
c--------check arg(s) if valid filename, ModelID
         iarg=1
         do while (CLA .ne. ' ' .and.
     &        iarg .le. 2)
            inquire (file=CLA, exist=exst)
            if (exst) then
               init_input_file=CLA
            else                ! not a file, is it Model Name?
               SimName=CLA
            endif
            iarg=iarg+1
            call getarg(iarg,CLA)
         enddo
      endif

      return

 900  continue
      print *, 'Could not find file or ModelID: ',trim(CLA)
      call exit(1)

      end

      logical function binarytf_fn(filename)

c-----Determine if FILENAME is a Fortran binary tidefile,
c-----an HDF5 tidefile, or unknown (fatal error)

      Use IO_Units
      
      implicit none

      character*(*)
     &     filename             ! filename string to test [INPUT]
      integer index             ! intrinsic function

      if (index(filename,'.hdf') .gt. 0 .or.
     &     index(filename,'.HDF') .gt. 0 .or.
     &     index(filename,'.H5') .gt. 0 .or.
     &     index(filename,'.h5') .gt. 0 ) then
         binarytf_fn=.false.       ! not a Fortran binary file
         return
      else if (index(filename,'.htf') .gt. 0 .or.
     &        index(filename,'.HTF') .gt. 0 .or.
     &        index(filename,'.bin') .gt. 0 .or.
     &        index(filename,'.BIN') .gt. 0 ) then
         binarytf_fn=.true.        ! is a Fortran binary file
         return
      else                      ! cannot determine filetype
         write(unit_error,610) filename
 610     format(/'Fatal error: cannot determine tidefile type: ',a)
      endif
      end function



      integer function fillin_code(fillin)
      use constants
      implicit none
      character*(*) fillin
      fillin_code = miss_val_i
      call locase(fillin)
      if (fillin .eq. "last") then 
          fillin_code =  fill_last
      elseif (fillin .eq. "linear") then
          fillin_code =  fill_interp
      endif
      return
      end function