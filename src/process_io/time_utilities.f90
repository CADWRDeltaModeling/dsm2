!<license>
!    Copyright (C) 2013 State of California,
!    Department of Water Resources.
!    This file is part of DSM2-GTM.
!
!    The Delta Simulation Model 2 (DSM2) - General Transport Model (GTM) 
!    is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    DSM2 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!</license>

!> Routines for time functionalities
!>@ingroup process_io	
module time_utilities

    use gtm_precision
    use error_handling
    contains
    
    !> Convert from character date/time to julian minute
    integer*4 function cdt2jmin(cdatx)
        use common_dsm2_vars, only: miss_val_i
        implicit none
        character*(*) :: cdatx        ! character date/time (e.g. 05JUN1983 0510) (input)
        integer*4 :: julday,   &      ! days since 31dec1899
                     minute,   &      ! minutes past midnight
                     ihm2m,    &      ! DSS function
                     ierror           ! error flag
        call datjul(cdatx(1:9), julday, ierror)
        if (cdatx(11:14) .eq. ' ') then ! assume empty time means 0000
             minute = 0
        else
             minute = ihm2m(cdatx(11:14))
        endif
        if (ierror .ne. 0 .or. minute .eq. -1) then
             cdt2jmin = miss_val_i
             goto 900
        endif
        cdt2jmin = julday*24*60 + minute
        return
 900    continue
        return
    end function
   
    !> Convert from Julian minute to character date/time
    character*14 function jmin2cdt(julmin)
        use common_dsm2_vars, only: miss_val_c
        implicit none
        integer*4 :: julmin             ! minutes since 31dec1899 2400
        integer*4 :: julday,      &     ! days since 31dec1899
                     minute,      &     ! minutes past midnight
                     ndate,       &     ! number of characters in date
                     m2ihm,       &     ! DSS function
                     itime,       &     ! integer time
                     jtmp,itmp          ! temporary julday & minutes
        jmin2cdt = '              '
        jtmp = julmin/(24*60)       ! julday
        itmp = mod(julmin,24*60)    ! minutes past midnight
        call datcll(jtmp,itmp,julday,minute)
        call juldat(julday, 104, jmin2cdt(1:9), ndate)
        itime = m2ihm(minute,jmin2cdt(11:14))
        if (itime .eq. -1) then
           jmin2cdt(1:1) = miss_val_c
        endif
        return
    end function

    !> Given a julian minute corresponding to a DSS character date
    !> (e.g. 05JUN1993 0510) and a DSS E part (e.g. 1HOUR, 2MON)
    !> char string, return the next interval in julian minutes
    !> according to the boundary flag:
    !> TO_BOUNDARY:
    !> to the time boundary of the interval, times number of intervals
    !> e.g. 05JUN1993 0600, 30JUL1993 2400
    !> IGNORE_BOUNDARY:
    !> increment number of intervals, ignoring boundary
    !> e.g. 05JUN1993 0610, 05AUG1993 0510
    !> NEAREST_BOUNDARY:
    !> to the nearest boundary, ignoring number of intervals
    !> e.g. 05JUN1993 0600, 30JUN1993 2400

    !> If the e_part starts with `-', then the interval is backwards
    !> in time, e.g. for -1HOUR, -2MON:
    !> TO_BOUNDARY:	05JUN1993 0500, 30APR1993 2400
    !> IGNORE_BOUNDARY:	05JUN1993 0410, 05APR1993 0510
    !> NEAREST_BOUNDARY:	05JUN1993 0500, 31MAY1993 2400

    !> If TO_BOUNDARY is requested, and jmins is already on the boundary,
    !> then jmins will still be incremented by the interval requested.
    !> But if NEAREST_BOUNDARY is requested, and jmins is on a boundary,
    !> then jmins will be returned for a positive interval; the previous
    !> boundary will be returned for a negative interval.   
    integer*4 function incr_intvl(jmins, e_part, boundary)
    
      use common_dsm2_vars, only: miss_val_i, IGNORE_BOUNDARY, NEAREST_BOUNDARY
    
      implicit none
      
      logical :: on_boundary,     & ! true if already on interval boundary
                 keepit             ! statement function
      character :: e_part*(*),    & ! DSS style interval [INPUT]
                   e_part_tmp*80, & ! temporary e_part
                   interval*80      ! DSS interval (e.g. HOUR)
      integer*4 :: jmins,         & ! starting julian minute [INPUT]
                   iymdjl,        & ! DSS function
                   nom_mins         ! nominal number of minutes in an interval
      integer :: boundary,        & ! how to handle boundary [INPUT]
                 number,          & ! number (e.g. 1, 15)
                 juls,jule,       & ! starting and ending julian day
                 istime,ietime,   & ! starting and ending minutes past midnight
                 jliymd,          & ! convert julian day to year,month,day DSS function
                 inctim,          & ! DSS function to increment time
                 idaywk,          & ! DSS function to return day of week
                 dayofweek,       & ! day of week index
                 istat,           & ! return status
                 iy,imon,id,ih,imin, & ! integer year, month, day, hour, minute
                 i,               & ! loop index
                 ibegf(10),       & ! beginning position of each field in line
                 ilenf(10),       & ! length of each field in line
                 idelmt(10),      & ! type of delimiter for each field
                 idelmp(10),      & ! position in delimiter string of delimiter
                 itbl(128),       & ! needed by findlm routine
                 nfields,         & ! number of fields found
                 len,             & ! returns declared length of string
                 lens,            & ! length of string
                 number_sign,     & ! sign of number of units (+ or -)
                 sign,            & ! intrinsic
                 nvals              ! number of values in DSS data block

      ! statement function to see whether to keep this new date,
      ! or to increment further
       keepit(boundary,number_sign,on_boundary) =            &
            boundary .eq. NEAREST_BOUNDARY .and. (           &
            (number_sign .gt. 0 .and. on_boundary) .or.      &
            (number_sign .lt. 0 .and. .not. on_boundary) )

      !--could be multiple intervals (e.g. 3DAY 5HOUR); process
      !--each individually

      !---use spaces, tabs, and underscore as delimiters
      e_part_tmp = e_part

      call setdlm(3,' ',1,0,itbl) ! don't use string delimiters
      call setdlm(2, '	 _',1,3,itbl) ! space, tab, and underscore
      nfields=10
      lens=len(e_part_tmp)
      call findlm(e_part_tmp,1,lens,nfields,ibegf,ilenf,idelmt,idelmp,itbl)

      call locase(e_part_tmp)

      jule = jmins/(24*60)        ! julian days
      ietime = mod(jmins,24*60)   ! minutes past midnight

      do i=1,nfields
      
         juls=jule
         istime=ietime

         !---Separate the number of units from the units (e.g. 3 MON).
         !---For irregular data (IR-*) the unit is 1.

         !---note: 01JAN1993 0000 == 31DEC1992 2400

         call split_epart(e_part_tmp(ibegf(i):ibegf(i)+ilenf(i)),number,   &
              interval)
         number_sign=sign(1,number)
         if (boundary .eq. NEAREST_BOUNDARY) number = number_sign
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
               if (mod(iy,10) .eq. 0 .and. imon .eq. 1 .and.      &
                    id .eq. 1 .and. ih .eq. 0 .and. imin .eq. 0)  &
                    on_boundary=.true. ! jmins on decade boundary
               iy=(iy/10)*10    ! e.g. 1993 -> 1990, 1990 -> 1990
               imon=1
               id=1
               ih=0
               imin=0
               !----now back to same or previous decade boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=iymdjl(iy,imon,id)
                  ietime=0
                  goto 100
               else
                  juls=iymdjl(iy,imon,id)
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,  &
                       ietime)
               endif
            else if (index(interval,'year') .ne. 0) then
               if (imon .eq. 1 .and. id .eq. 1 .and. ih .eq. 0 .and.     &
                    imin .eq. 0) on_boundary=.true. ! jmins on year boundary
               imon=1
               id=1
               ih=0
               imin=0
               !------now back to same or previous year boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=iymdjl(iy,imon,id)
                  ietime=0
                  goto 100
               else
                  juls=iymdjl(iy,imon,id)
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,  &
                       ietime)
               endif
            else if (index(interval,'mon') .ne. 0) then
               if (id .eq. 1 .and. ih .eq. 0 .and.                       &
                    imin .eq. 0) on_boundary=.true. ! jmins on month boundary
               id=1
               ih=0
               imin=0
               !-----now back to same or previous month boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=iymdjl(iy,imon,id)
                  ietime=0
                  goto 100
               else
                  juls=iymdjl(iy,imon,id)
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,   &
                       ietime)
               endif
            else if (index(interval,'week') .ne. 0) then
               dayofweek=idaywk(juls)
               if (dayofweek .eq. 1 .and. ih .eq. 0 .and. imin .eq. 0)    &
                    on_boundary=.true. ! jmins on week boundary
               juls=juls-dayofweek+1
               istat=jliymd(juls,iy,imon,id) ! get integer year, month, day
               ih=0
               imin=0
               !-----now back to same or previous week boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=0
                  goto 100
               else
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,   &
                       ietime)
               endif
            else if (index(interval,'day') .ne. 0) then
               if (ih .eq. 0 .and. imin .eq. 0)                           &
                    on_boundary=.true. ! jmins on day boundary
               ih=0
               imin=0
               !-----now back to same or previous day boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=0
                  goto 100
               else
                  istime=0
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,   &
                       ietime)
               endif
            else if (index(interval,'hour') .ne. 0) then
               if (imin .eq. 0) on_boundary=.true. ! jmins on hour boundary
               imin=0
               !----now back to same or previous hour boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=ih*60
                  goto 100
               else
                  istime=ih*60
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,   &
                       ietime)
               endif
            else if (index(interval,'min') .ne. 0) then
               if (mod(imin,15) .eq. 0) on_boundary=.true. ! jmins on 15min boundary
               if (interval(:5) .eq. '15min') imin=(imin/15)*15
               !----now back to same or previous 15min boundary
               if (keepit(boundary,number_sign,on_boundary)) then
                  jule=juls
                  ietime=ih*60 + imin
                  goto 100
               else
                  istime=ih*60 + imin
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,   &
                       ietime)
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
    end function


    !> Routine to determine offset and buffer length to read HDF file
    subroutine check_runtime(offset, num_buffers, memlen,          &
                             time_buffer,                          &                               
                             run_start_jmin, run_end_jmin,         &
                             hdf_start_jmin, hdf_end_jmin,         &
                             hdf_time_interval)              
          implicit none
          integer, intent(out) :: offset                  !< time offset to read hydro tidefile
          integer, intent(out) :: num_buffers             !< number of total buffer blocks
          integer, allocatable, intent(out) :: memlen(:)  !< length of memory for each buffer
          integer, intent(in) :: time_buffer              !< time buffer length
          integer, intent(in) :: hdf_start_jmin           !< hydro tidefile start julian minutes
          integer, intent(in) :: hdf_end_jmin             !< hydro tidefile end Julian minutes
          integer, intent(in) :: hdf_time_interval        !< hydro tidefile time interval
          integer, intent(in) :: run_start_jmin           !< GTM start Julian miniutes
          integer, intent(in) :: run_end_jmin             !< GTM end Julian miniutes
          integer :: remainder, i                         ! local variables
          integer :: istat = 0                            ! error handling for allocation
          offset = LARGEINT
          num_buffers = LARGEINT
          remainder = LARGEINT
          if (run_start_jmin < hdf_start_jmin) then
              write(*,*) "HDF file time range:",jmin2cdt(hdf_start_jmin),"-",jmin2cdt(hdf_end_jmin)          
              call gtm_fatal("GTM starting time should be within HDF file time range.")
          else if (run_end_jmin > hdf_end_jmin) then
              write(*,*) "HDF file time range:",jmin2cdt(hdf_start_jmin),"-",jmin2cdt(hdf_end_jmin)          
              call gtm_fatal(" GTM ending time should be within HDF file time range.")
          else 
              offset = (run_start_jmin - hdf_start_jmin)/hdf_time_interval
              num_buffers = int((run_end_jmin-run_start_jmin)/hdf_time_interval/time_buffer) + 1
              remainder = mod((run_end_jmin-run_start_jmin)/hdf_time_interval, time_buffer)
              allocate(memlen(num_buffers), stat = istat)
              if (num_buffers>1) then
                  do i = 1, num_buffers - 1
                      memlen(i) = time_buffer
                  end do
              end if
              memlen(num_buffers) = remainder              
          end if
          return  
    end subroutine
    
    !> Split a DSS E part into its interval (e.g. HOUR, DECADE)
    !> and number of intervals (e.g. 1, 5).
    !> Assume that every interval is unitary (e.g. 6HOUR means
    !> six 1HOUR intervals, not a single interval of 6HOUR).
    !> However 15MIN means a single 15MIN interval.      
    subroutine split_epart(e_part,number,interval)
        use common_dsm2_vars, only: miss_val_i
        implicit none
        character :: e_part*(*),      &  ! DSS E part [INPUT]
                     interval*(*),    &  ! DSS interval [RETURN]
                     e_part_tmp*80,   &  ! temporary e part
                     char_list*12        ! list of chars to scan

        integer :: number,            &  ! number of intervals [RETURN]
                   ielen,             &  ! length of e_part
                   ipos2,             &  ! which char found in iscan
                   ilast,             &  ! position of last digit in e_part
                   iscan                 ! DSS char scan function

        data char_list /'0123456789+-'/

        e_part_tmp = e_part
        call locase(e_part_tmp)

        if (e_part_tmp(:3) .eq. 'ir-') then ! irregular interval
           number = 1
           interval = e_part_tmp
           return
        else
           ielen = len(e_part_tmp)
           ilast = iscan(e_part_tmp, ielen, -ielen, char_list, 1, 10, ipos2)
           !---handle e.g. 'hour' (w/o number) correctly
           if (ilast .eq. 0) then
              number = 1
           else
              read(e_part_tmp(:ilast),'(i5)', err=600) number
           endif
        endif
        interval = e_part_tmp(ilast+1:)
        !-----check for valid interval
        if (                                     &
            index(interval,'min') .gt. 0 .or.    &
            index(interval,'hour') .gt. 0 .or.   &
            index(interval,'day') .gt. 0 .or.    &
            index(interval,'week') .gt. 0 .or.   &
            index(interval,'mon') .gt. 0 .or.    &
            index(interval,'year') .gt. 0 .or.   &
            index(interval,'dec') .gt. 0         &
            ) then
       !----for minutes, treat 15MIN intervals as unit
           if (mod(number,15) .eq. 0 .and. interval(:3) .eq. 'min') then
              number = number/15
              interval = '15min'
           else
              interval = '1'//interval
           endif
           return
        endif
 600    continue                  ! here for error getting number of intervals
        number = miss_val_i
        interval = ' '
        return
    end subroutine
      
      
    !> Parse time interval string into real number in unit of minutes
    subroutine get_time_intvl_min(time_intvl, time_intvl_str)
        implicit none
        character*(*), intent(in) :: time_intvl_str   !< time interval string read from input specification file
        real(gtm_real), intent(out) :: time_intvl     !< converted time interval in minutes
        character(len=20) :: time_intvl_tmp
        character :: char_list*12                     ! list of chars to scan
        integer :: number,                         &  ! number of intervals [RETURN]
                   ielen,                          &  ! length of e_part
                   ipos2,                          &  ! which char found in iscan
                   ilast,                          &  ! position of last digit in e_part
                   iscan,                          &  ! DSS char scan function
                   out_tmp

        data char_list /'0123456789+-'/
      
        time_intvl = LARGEINT
        call locase(time_intvl_str)
        ielen = len(time_intvl_str)
        ilast = iscan(time_intvl_str, ielen, -ielen, char_list, 1, 10, ipos2)
        time_intvl_tmp = time_intvl_str(1:ilast)
        read(time_intvl_tmp,'(i)', err=610) out_tmp
        time_intvl = real(out_tmp)
  
        if (index(time_intvl_str,'min') .gt. 0) time_intvl = time_intvl
        if (index(time_intvl_str,'hour') .gt. 0) time_intvl = time_intvl*sixty
        if (index(time_intvl_str,'day') .gt. 0) time_intvl = time_intvl*sixty*twentyfour
        if (index(time_intvl_str,'week') .gt. 0) time_intvl = time_intvl*sixty*twentyfour*seven   ! not recommend for such a coarse time interval
        if (index(time_intvl_str,'mon') .gt. 0) time_intvl = time_intvl*60*24*30                  ! not recommend for such a coarse time interval, todo: using 30 is not right
        if (index(time_intvl_str,'year') .gt. 0) time_intvl = time_intvl*60*24*365                ! not recommend for such a coarse time interval, todo: using 365 is not right   
        return
610     write(*,*) "Error in parsing time interval string", time_intvl_str     
        return         
    end subroutine  


    !> Given a character date/time string (e.g. 05JAN1996 0530), and a
    !> DSS E part interval (e.g. 1HOUR, 1MONTH), return the
    !> corresponding portion from the date.      
    subroutine get_intvl(cdatx, e_part, cdate_intvl)
 
      use common_dsm2_vars, only: miss_val_c
      implicit none

      character*(*), intent(in) :: cdatx        ! date/time string [IN]
      character*(*), intent(in) :: e_part       ! DSS E part interval [IN]
      character*(*), intent(out) :: cdate_intvl ! date/time portion corresponding to interval [OUT]

      integer :: number                        ! number prefix of E part

      character*15 :: interval                 ! E part minus number prefix

      call split_epart(e_part, number, interval)

      if (interval .eq. '15MIN' .or. interval .eq. '15min') then
         cdate_intvl=cdatx(13:14)
      else if (interval .eq. '1HOUR' .or. interval .eq. '1hour') then
         cdate_intvl=cdatx(11:12)
      else if (interval .eq. '1DAY' .or. interval .eq. '1day') then
         cdate_intvl=cdatx(1:2)
      else if (interval .eq. '1MON' .or. interval .eq. '1mon') then
         cdate_intvl=cdatx(3:5)
      else if (interval .eq. '1YEAR' .or. interval .eq. '1year') then
         cdate_intvl=cdatx(6:9)
      else                      ! couldn't find specified interval
         cdate_intvl=miss_val_c
      endif

      return
    end subroutine
      
    !> Routine to get number of partition in time
    subroutine get_npartition_t(npart_t, orig_time_interv, gtm_time_interv)
          implicit none
          integer, intent(out) :: npart_t
          integer, intent(in) :: orig_time_interv
          real(gtm_real), intent(in) :: gtm_time_interv
          if (orig_time_interv < gtm_time_interv) then
              call gtm_fatal("GTM runtime interval should be smaller or equal than DSM2 hydro output time interval.")
          else
              npart_t = orig_time_interv/gtm_time_interv
              if((int(npart_t)-npart_t).ne.zero)then
                  call gtm_fatal("HDF time interval/GTM runtime interval does not yield an integer.")
              end if
          end if
          return
    end subroutine
      
end module