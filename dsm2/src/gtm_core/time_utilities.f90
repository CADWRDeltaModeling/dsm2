!<license>
!    Copyright (C) 2017 State of California,
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
!>@ingroup gtm_core
module time_utilities

    use gtm_precision
    use error_handling
    contains

    !> Convert from character date/time to julian minute
    integer*4 function cdt2jmin(cdatx)
        use common_variables, only: miss_val_i
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
        use common_variables, only: miss_val_c
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


    !> Convert from julian minute to character date/time
    !> in ISO compliant format yyyy-mmm-dd hh:mm::ss
    !> with no military time conversion (and 00:00 is
    !> always used instead of 2400)
    character*19 function jmin2iso(julmin)
      implicit none
      integer*4 :: julmin   !< minutes since 31dec1899 2400
	  integer*4 :: julday
	  integer*4 :: minute
	  integer*4 :: y,m,d,ihr,imin
      character*14 :: jmin2cdt

      julday = julmin/(24*60)       ! julday
      minute = mod(julmin,24*60)    ! minutes past midnight

	  call jliymd(julday,y,m,d)
      ihr = minute/60
	  imin = mod(minute,60)
	  write(jmin2iso,231)y,m,d,ihr,imin
 231  format (i4,'-',i2.2,'-',i2.2,' ',i2.2,':',i2.2,':00')
      return
    end function


     !> Routine to determine offset and buffer length to read HDF file
    subroutine check_runtime(num_blocks, memlen,                   &
                             time_buffer, hdf_time_interval,       &
                             hdf_start_jmin, hdf_end_jmin,         &
                             gtm_start_jmin, gtm_end_jmin)
          implicit none
          integer, intent(out) :: num_blocks              !< number of total buffer blocks
          integer, allocatable, intent(out) :: memlen(:)  !< length of memory for each buffer
          integer, intent(in) :: time_buffer              !< time buffer length
          integer, intent(in) :: hdf_start_jmin           !< hydro tidefile start julian minutes
          integer, intent(in) :: hdf_end_jmin             !< hydro tidefile end Julian minutes
          integer, intent(in) :: hdf_time_interval        !< hydro tidefile time interval
          real(gtm_real), intent(in) :: gtm_start_jmin    !< gtm tidefile start julian minutes
          real(gtm_real), intent(in) :: gtm_end_jmin      !< gtm tidefile end Julian minutes
          integer :: remainder, i                         ! local variables
          integer :: istat = 0                            ! error handling for allocation
          num_blocks = LARGEINT
          remainder = LARGEINT
          if (int(gtm_start_jmin) < hdf_start_jmin) then
              write(*,*) "HDF file time range:",jmin2cdt(hdf_start_jmin),"-",jmin2cdt(hdf_end_jmin)
              call gtm_fatal("GTM starting time should be within HDF file time range.")
          else if (int(gtm_start_jmin) > hdf_end_jmin) then
              write(*,*) "HDF file time range:",jmin2cdt(hdf_start_jmin),"-",jmin2cdt(hdf_end_jmin)
              call gtm_fatal("GTM ending time should be within HDF file time range.")
          else
              num_blocks = int((hdf_end_jmin - hdf_start_jmin)/hdf_time_interval/time_buffer) + 1
              remainder = mod((hdf_end_jmin - hdf_start_jmin)/hdf_time_interval, time_buffer)
              allocate(memlen(num_blocks), stat = istat)
              if (num_blocks>1) then
                  do i = 1, num_blocks - 1
                      memlen(i) = time_buffer
                  end do
              end if
              memlen(num_blocks) = remainder
          end if
          return
    end subroutine

    !> Subroutine to return current block number and slice in the block
    subroutine get_loc_in_hydro_buffer(iblock,            &
                                       slice_in_block,    &
                                       time_in_slice,     &
                                       current_time,      &
                                       start_hydro_block, &
                                       memory_buffer,     &
                                       hdf_time_interval, &
                                       gtm_time_interval)
        use gtm_logging, only:debug_unit
        implicit none
        integer, intent(in) :: start_hydro_block        !< offset to start reading hydro tidefile
        integer, intent(in) :: memory_buffer            !< memory buffer
        integer, intent(in) :: hdf_time_interval        !< hydro tidefile time interval
        real(gtm_real), intent(in) :: current_time      !< current julian time
        real(gtm_real), intent(in) :: gtm_time_interval !< gtm time interval
        integer, intent(out) :: iblock                  !< block index
        integer, intent(out) :: slice_in_block          !< slice in block
        real(gtm_real), intent(out) :: time_in_slice           !< time in slice

        iblock = int((current_time - dble(start_hydro_block))/dble(hdf_time_interval)/dble(memory_buffer)) + 1

        slice_in_block = int((current_time - dble(start_hydro_block))/dble(hdf_time_interval) - (dble(iblock)-one)*dble(memory_buffer)) + 1

        time_in_slice = current_time - dble(start_hydro_block) - ((dble(iblock)-one)*dble(memory_buffer)+(dble(slice_in_block)-one))*dble(hdf_time_interval)

        return
    end subroutine


    !> Parse time interval string into real number in unit of minutes
    subroutine get_time_intvl_min(time_intvl,      &
                                  time_intvl_str)
        implicit none
        character*(*), intent(in) :: time_intvl_str   !< time interval string read from input specification file
        real(gtm_real), intent(out) :: time_intvl     !< converted time interval in minutes
        character(len=20) :: time_intvl_tmp
        character :: char_list*13                     ! list of chars to scan
        real(gtm_real) :: number                      ! number of intervals [RETURN]
        real(gtm_real) :: out_tmp
        integer :: ielen,                          &  ! length of e_part
                   ipos2,                          &  ! which char found in iscan
                   ilast,                          &  ! position of last digit in e_part
                   iscan                              ! DSS char scan function
        data char_list /'0123456789+-.'/
        integer :: idot, readint

        time_intvl = LARGEINT
        call locase(time_intvl_str)
        ielen = len(time_intvl_str)
        ilast = iscan(time_intvl_str, ielen, -ielen, char_list, 1, 10, ipos2)
        time_intvl_tmp = time_intvl_str(1:ilast)
        idot = scan(time_intvl_tmp,".")
        if (idot==0) then
            read(time_intvl_tmp,*, err=610) readint
            time_intvl = real(readint)
        else
            read(time_intvl_tmp,*, err=610) time_intvl
        endif
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


    !> Routine to get number of partition in time
    subroutine get_npartition_t(npart_t,            &
                                orig_time_interv,   &
                                gtm_time_interv)
          implicit none
          integer, intent(out) :: npart_t                 !< number of time steps between two hydro time steps
          integer, intent(in) :: orig_time_interv         !< Hydrotidefile time interval
          real(gtm_real), intent(in) :: gtm_time_interv   !< GTM runtime interval
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