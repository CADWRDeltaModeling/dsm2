!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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
module time_util

    use gtm_precision
    use error_handling
    contains
    
    !> Convert from character date/time to julian minute
    subroutine cdt2jmin(julmin, cdatx)
        implicit none
        character(len=*), intent(in) :: cdatx     !< character date/time (e.g. 05JUN1983 0510) (input)
        integer, intent(out) :: julmin            !< julian minute
        integer*4 :: julday  &                    ! days since 31dec1899
                    ,minute  &                    ! minutes past midnight
                    ,ihm2m                        ! DSS function
        integer*4 :: ierror                       ! error flag
        call datjul(cdatx(1:9), julday, ierror)
        if (cdatx(11:14) .eq. ' ') then           ! assume empty time means 0000
            minute = 0
        else
            minute = ihm2m(cdatx(11:14))
        endif
        if (ierror .ne. 0 .or. minute .eq. -1) then
            julmin = miss_val_i
            goto 900
        endif
        julmin = julday*24*60 + minute
        return
900     continue
    end subroutine
   
    !> Convert from Julian minute to character date/time
    subroutine jmin2cdt(cdt, julmin)
        implicit none
        integer*4, intent(in) ::  julmin           !< minutes since 31dec1899 2400
        character(len=14), intent(out) :: cdt      !< character date/time
        integer*4 ::  julday    &                  ! days since 31dec1899
                     ,minute    &                  ! minutes past midnight
                     ,ndate     &                  ! number of characters in date
                     ,m2ihm     &                  ! DSS function
                     ,itime     &                  ! integer time
                     ,jtmp,itmp                    ! temporary julday & minutes
        cdt = '              '
        jtmp = julmin/(24*60)       ! julday
        itmp = mod(julmin,24*60)    ! minutes past midnight
        call datcll(jtmp,itmp,julday,minute)
        call juldat(julday, 104, cdt(1:9), ndate)
        itime = m2ihm(minute,cdt(11:14))
        if (itime .eq. -1) then
            cdt(1:1) = miss_val_c
        endif
        return
      end subroutine

      !> Routine to determine offset and buffer length to read HDF file
      subroutine check_runtime(offset, num_buffers, memlen,          &
                               time_buffer,                          &                               
                               gtm_start_cdt, gtm_end_cdt,           &
                               hdf_start_jmin, hdf_end_jmin,         &
                               hdf_time_interval)              
          implicit none
          integer, intent(out) :: offset                  !< time offset to read hydro tidefile
          integer, intent(out) :: num_buffers             !< number of total buffer blocks
          integer, allocatable, intent(out) :: memlen(:)  !< length of memory for each buffer
          integer, intent(in) :: time_buffer              !< time buffer length
          integer, intent(in) :: hdf_start_jmin           !< hydro tidefile start julian minutes
          integer, intent(in) :: hdf_end_jmin             !< hydro tidefile end Julian minutes
          integer, intent(in) :: hdf_time_interval        
          character(len=14), intent(in) :: gtm_start_cdt  !< GTM start character date/time
          character(len=14), intent(in) :: gtm_end_cdt    !< GTM end character date/time
          integer :: gtm_start_jmin                       !< GTM starting Julian miniutes
          integer :: gtm_end_jmin                         !< GTM ending Julian miniutes
          integer :: remainder, i                         ! local variables
          integer :: istat = 0                            ! error handling for allocation
          offset = LARGEINT
          num_buffers = LARGEINT
          remainder = LARGEINT
          call cdt2jmin(gtm_start_jmin, gtm_start_cdt)
          call cdt2jmin(gtm_end_jmin, gtm_end_cdt)
          if (gtm_start_jmin < hdf_start_jmin) then
              call gtm_fatal("GTM starting time should be within HDF file time range.")
          else if (gtm_end_jmin > hdf_end_jmin) then
              call gtm_fatal(" GTM ending time should be within HDF file time range.")
          else 
              offset = (gtm_start_jmin - hdf_start_jmin)/hdf_time_interval
              num_buffers = int((gtm_end_jmin-gtm_start_jmin)/hdf_time_interval/time_buffer) + 1
              remainder = mod((gtm_end_jmin-gtm_start_jmin)/hdf_time_interval, time_buffer)
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
      
      !> Routine to get number of partition in time
      subroutine get_npartition_t(npart_t, orig_time_interv, gtm_time_interv)
          implicit none
          integer, intent(out) :: npart_t
          integer, intent(in) :: orig_time_interv
          integer, intent(in) :: gtm_time_interv
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