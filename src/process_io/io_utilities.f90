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
!>@ingroup process_io
module io_utilities

    contains

    !> get optional starting input file from command line,
    !> then from environment variables,
    !> then default
    subroutine get_command_args(init_input_file)   !todo: I haven't tested echo_only functionality
      use common_dsm2_vars
      implicit none
      character init_input_file*(*)  ! initial input file on command line [optional]

      !-----local variables
      logical :: exst,     &            ! true if file exists
                 echo_only
      integer :: iarg                   ! argument index
      character*150 :: CLA              ! command line args
      echo_only = .false.      
      call getarg(1,CLA)
      if (len_trim(CLA) .eq. 0) then ! print version, usage, quit
         print *, 'DSM2-' // trim(dsm2_name) // ' ', dsm2_version
         print *, 'Usage: ' // trim(dsm2_name) // ' input-file '
         call exit(1)
      elseif ( CLA(:2) .eq. "-v" .or.   &
               CLA(:2) .eq. "-V" .or.   &
               CLA(:2) .eq. "-h" .or.   &
               CLA(:2) .eq. "-H") then ! print version and subversion, usage, quit
         print *, 'DSM2-' // trim(dsm2_name) // ' ', trim(dsm2_version) // '  Subversion: ', trim(svn_build)
         print *, 'Usage: ' // trim(dsm2_name) // ' input-file '
         call exit(1)
      else                      ! command line arg
      !---check arg(s) if valid filename, ModelID
         iarg=1
         do while (CLA .ne. ' ' .and.      &
              iarg .le. 2)
            inquire (file=CLA, exist=exst)
            if (exst) then
               init_input_file=CLA
            else              ! not a file, is it Model Name?
               if(CLA(:2) .eq. "-e" .or. CLA(:2) .eq. "-E")then
                   echo_only = .true.
               else
                   write(unit_error,*)"Launch file not found: ",trim(CLA)
                   call exit(-3)
               end if
            endif
            iarg=iarg+1
            call getarg(iarg,CLA)
         enddo
      endif
      return
 900  continue
      print *, 'Could not find file or ModelID: ',trim(CLA)
      call exit(1)
    end subroutine

    
    !> fill in code for last or linear
    integer function fillin_code(fillin)
      use common_dsm2_vars, only: miss_val_i, fill_last, fill_interp
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


    !> Locate a string in a character array, given array dimension.
    !> Returns array index if found.  If not found return negative
    !> value of first blank array index. Input logical exact controls
    !> whether exact or substring match requested.
    integer function loccarr(cstring, carr, dim_carr, exact)
      implicit none
      logical :: exact                    ! if true, exact match, else substring match
      integer :: dim_carr                 ! dimension of carr
      character*(*) :: cstring,         & ! input string
                       carr(dim_carr)     ! input character array
      integer :: i,                     & ! loop index
                 lstring,               & ! length of nonblank part of cstring
                 lnblnk                   ! last nonblank function
      i = 1
      if (exact) then                     ! exact match
         do while (i .le. dim_carr)
            if (carr(i) .eq. cstring .or. carr(i) .eq. ' ') exit
            i = i + 1
         enddo
      else                      ! substring match
         lstring=lnblnk(cstring)
         do while (i .le. dim_carr) 
            if (carr(i)(:lstring) .eq. cstring(:lstring)     &
                .or.                                         &
                carr(i) .ne. ' ')exit
            i = i + 1
         enddo
      endif
      if (i .gt. dim_carr) then
         loccarr = 0
      else if (carr(i) .eq. ' ') then
         loccarr = -i
      else
         loccarr = i
      endif
      return
    end function


    !> Split a DSS E part into its interval (e.g. HOUR, DECADE)
    !> and number of intervals (e.g. 1, 5).
    !> Assume that every interval is unitary (e.g. 6HOUR means
    !> six 1HOUR intervals, not a single interval of 6HOUR).
    !> However 15MIN means a single 15MIN interval.
    subroutine split_epart(e_part,number,interval)
      use common_dsm2_vars, only: miss_val_i
      implicit none
      character :: e_part*(*),        &  ! DSS E part [INPUT]
                   interval*(*),      &  ! DSS interval [RETURN]
                   e_part_tmp*80,     &  ! temporary e part
                   char_list*12          ! list of chars to scan
      integer :: number,              &  ! number of intervals [RETURN]
                 ielen,               &  ! length of e_part
                 ipos2,               &  ! which char found in iscan
                 ilast,               &  ! position of last digit in e_part
                 iscan                   ! DSS char scan function

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
         !----handle e.g. 'hour' (w/o number) correctly
         if (ilast .eq. 0) then
            number=1
         else
            read(e_part_tmp(:ilast),'(i5)', err=600) number
         endif
      endif
      interval=e_part_tmp(ilast+1:)
      !-----check for valid interval
      if ( index(interval,'min') .gt. 0 .or.    &
           index(interval,'hour') .gt. 0 .or.   &
           index(interval,'day') .gt. 0 .or.    &
           index(interval,'week') .gt. 0 .or.   &
           index(interval,'mon') .gt. 0 .or.    &
           index(interval,'year') .gt. 0 .or.   &
           index(interval,'dec') .gt. 0         &
          ) then
      !-----for minutes, treat 15MIN intervals as unit
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
    end subroutine
    
end module    