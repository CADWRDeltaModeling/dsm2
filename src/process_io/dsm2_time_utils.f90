

!> Routines for time functionalities
!>@ingroup process_io	
module dsm2_time_utils

    use gtm_precision
    use error_handling
    contains

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
    subroutine incr_intvl2(incr_intv, jmins, e_part, boundary)
    
      use common_dsm2_vars, only: miss_val_i, IGNORE_BOUNDARY, NEAREST_BOUNDARY
    
      implicit none
      
      real(gtm_real), intent(out) :: incr_intv  !> incremented interval
      character, intent(in) :: e_part*(*)       !> DSS style interval
      real(gtm_real), intent(in) :: jmins       !> starting julian minute
      integer, intent(in) :: boundary           !> how to handle boundary
      logical :: on_boundary,     & ! true if already on interval boundary
                 keepit             ! statement function
                 

      character :: e_part_tmp*80, & ! temporary e_part
                   interval*80      ! DSS interval (e.g. HOUR)

      integer*4 :: iymdjl,        & ! DSS function
                   nom_mins         ! nominal number of minutes in an interval
      real(gtm_real) :: number      ! number (e.g. 1, 15)                  
      real(gtm_real) :: number_sign ! sign of number of units (+ or -)
      real(gtm_real) :: ietime      ! starting and ending minutes past midnight
      integer :: juls,jule,       & ! starting and ending julian day
                 istime,          & ! starting and ending minutes past midnight
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
      ietime = mod(jmins,dble(24*60))   ! minutes past midnight

      do i=1,nfields
      
         juls=jule
         istime=ietime

         !---Separate the number of units from the units (e.g. 3 MON).
         !---For irregular data (IR-*) the unit is 1.

         !---note: 01JAN1993 0000 == 31DEC1992 2400

         call split_epart(e_part_tmp(ibegf(i):ibegf(i)+ilenf(i)),number,   &
              interval)
         number_sign=sign(one,number)
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
               !if (interval(:5) .eq. '15min') imin=(imin/15)*15
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
               if ((mod(number,one).ne.zero).or. (jule<0) ) then   ! to take care of non-integer time interval
                  jule = 0
                  ietime = number
               end if     
            else
               incr_intv = miss_val_i
               return
            endif
         endif
 100     continue
      enddo
      incr_intv = jule*24.d0*60.d0 + ietime
      return
    end subroutine


    subroutine incr_intvl(incr_intv, jmins, e_part, boundary)
    
      use common_dsm2_vars, only: miss_val_i, IGNORE_BOUNDARY, NEAREST_BOUNDARY
    
      implicit none
      
      real(gtm_real), intent(out) :: incr_intv  !> incremented interval
      character, intent(in) :: e_part*(*)       !> DSS style interval
      real(gtm_real), intent(in) :: jmins       !> starting julian minute
      integer, intent(in) :: boundary           !> how to handle boundary
      logical :: on_boundary,     & ! true if already on interval boundary
                 keepit             ! statement function
      integer :: incr_intv_int
      integer :: jmins_int           

      character :: e_part_tmp*80, & ! temporary e_part
                   interval*80      ! DSS interval (e.g. HOUR)

      integer*4 :: iymdjl,        & ! DSS function
                   nom_mins         ! nominal number of minutes in an interval
      real(gtm_real) :: number      ! number (e.g. 1, 15)                  
      integer :: number_sign        ! sign of number of units (+ or -)
      integer :: ietime             ! starting and ending minutes past midnight
      integer :: juls,jule,       & ! starting and ending julian day
                 istime,          & ! starting and ending minutes past midnight
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
                 sign,            & ! intrinsic
                 nvals              ! number of values in DSS data block
                 
       jmins_int = int(jmins)

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

      jule = jmins_int/(24*60)            ! julian days
      ietime = mod(jmins_int,24*60)   ! minutes past midnight

      do i=1,nfields
      
         juls=jule
         istime=ietime

         !---Separate the number of units from the units (e.g. 3 MON).
         !---For irregular data (IR-*) the unit is 1.

         !---note: 01JAN1993 0000 == 31DEC1992 2400

         call split_epart(e_part_tmp(ibegf(i):ibegf(i)+ilenf(i)),number,   &
              interval)
         number_sign=sign(one,number)
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
               call keep_this_date(keepit,boundary,number_sign,on_boundary,NEAREST_BOUNDARY)
               if (keepit) then
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
               call keep_this_date(keepit,boundary,number_sign,on_boundary,NEAREST_BOUNDARY)
               if (keepit) then
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
               call keep_this_date(keepit,boundary,number_sign,on_boundary,NEAREST_BOUNDARY)
               if (keepit) then
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
               call keep_this_date(keepit,boundary,number_sign,on_boundary,NEAREST_BOUNDARY)
               if (keepit) then
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
               call keep_this_date(keepit,boundary,number_sign,on_boundary,NEAREST_BOUNDARY)
               if (keepit) then
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
               call keep_this_date(keepit,boundary,number_sign,on_boundary,NEAREST_BOUNDARY)
               if (keepit) then
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
               !if (interval(:5) .eq. '15min') imin=(imin/15)*15
               !----now back to same or previous 15min boundary
               call keep_this_date(keepit,boundary,number_sign,on_boundary,NEAREST_BOUNDARY)
               if (keepit) then
                  jule=juls
                  ietime=ih*60 + imin
                  goto 100
               else
                  istime=ih*60 + imin
                  istat=inctim(nom_mins, 0, number, juls, istime, jule,   &
                       ietime)
               endif
               if ((mod(number,one).ne.zero).or. (jule<0) ) then   ! to take care of non-integer time interval
                  jule = 0
                  ietime = number
               end if     
            else
               incr_intv = miss_val_i
               return
            endif
         endif
 100     continue
      enddo
      incr_intv_int = jule*24*60 + ietime
      incr_intv = dble(incr_intv_int)
      return
    end subroutine

    !> subroutine to see whether to keep this new date,
    !> or to increment further
    subroutine keep_this_date(keepit, boundary, number_sign, on_boundary, NEAREST_BOUNDARY)
        implicit none
        logical, intent(out) :: keepit            !> keep this new date or to increment further
        integer, intent(in) :: boundary           !> how to handle boundary
        integer, intent(in) :: number_sign        !> sign of number of units (+ or -)
        logical, intent(in) :: on_boundary        !> true if already on interval boundary
        integer, intent(in) :: NEAREST_BOUNDARY   !> nearest boundary
        keepit = boundary .eq. NEAREST_BOUNDARY .and. (  &
                (number_sign .gt. 0 .and. on_boundary) .or.      &
                (number_sign .lt. 0 .and. .not. on_boundary) )
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
        character, intent(in) :: e_part*(*)    ! DSS E part
        character, intent(out) :: interval*(*) ! DSS interval
        real(gtm_real), intent(out) :: number  ! number of intervals        
        character :: e_part_tmp*80,         &  ! temporary e part
                     char_list*13              ! list of chars to scan
        integer :: ielen,                   &  ! length of e_part
                   ipos2,                   &  ! which char found in iscan
                   ilast,                   &  ! position of last digit in e_part
                   iscan                       ! DSS char scan function
        data char_list /'0123456789+-.'/        
        integer :: ilast2, number_tmp          
        
        e_part_tmp = e_part
        call locase(e_part_tmp)

        if (e_part_tmp(:3) .eq. 'ir-') then ! irregular interval
           number = 1
           interval = e_part_tmp
           return
        else
           ielen = len(e_part_tmp)
           ilast = iscan(e_part_tmp, ielen, -ielen, char_list, 1, 10, ipos2)
           ilast2 = scan(e_part_tmp, '.')
           !---handle e.g. 'hour' (w/o number) correctly
           if (ilast .eq. 0) then
              number = 1
           else
              if (ilast2==0) then
                  read(e_part_tmp(:ilast),'(i)', err=600) number_tmp
                  number = dble(number_tmp)
              else
                  read(e_part_tmp(:ilast),'(f)', err=600) number
              end if    
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
           !if (mod(number,fifteen) .eq. 0 .and. interval(:3) .eq. 'min') then
           !   number = number/fifteen
           !   interval = '15min'
           !else
              interval = '1'//interval
           !endif
           return
        endif
 600    continue                  ! here for error getting number of intervals
        number = miss_val_i
        interval = ' '
        return
    end subroutine
 

    !> Given a character date/time string (e.g. 05JAN1996 0530), and a
    !> DSS E part interval (e.g. 1HOUR, 1MONTH), return the
    !> corresponding portion from the date.      
    subroutine get_intvl(cdatx, e_part, cdate_intvl)
 
      use common_variables, only: miss_val_c
      implicit none

      character*(*), intent(in) :: cdatx        ! date/time string [IN]
      character*(*), intent(in) :: e_part       ! DSS E part interval [IN]
      character*(*), intent(out) :: cdate_intvl ! date/time portion corresponding to interval [OUT]
      real(gtm_real) :: number                  ! number prefix of E part
      character*15 :: interval                  ! E part minus number prefix

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
    
            
end module    
    