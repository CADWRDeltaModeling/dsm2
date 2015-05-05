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


!> Define arrays to be used as buffers for data values, data
!> quality flags from DSS, and julian minute timestamp.
!> Use one array for each kind of data interval for DSS.
!> This module is borrowed from DSM2/time_var project with 
!> slight modification. This module was originally developed
!> by Ralph Finch in 1996.
!>@ingroup process_io
module gtm_dss_read

    use gtm_dss

    contains
        
    !> Buffer time series data from DSS for model input    
    subroutine readdss (pathnumber,  & 
                        jmin,        &
                        inpaths_dim, &
                        block_dim,   &
                        indata,      &
                        per_type)
       use common_dsm2_vars
       use time_utilities
       use dsm2_time_utils
       implicit none
       !-----arguments
       integer, intent(in) :: pathnumber      ! global pathnumber [INPUT] 
       integer, intent(in) :: inpaths_dim     ! input paths array dimension [INPUT]
       integer, intent(in) :: block_dim       ! data block array dimension [INPUT]
       real(gtm_real) :: jmin                 ! date/time for start of data [INPUT] 

       type(dataqual_t), intent(out) :: indata(block_dim, inpaths_dim) ! data, flags, and julmin structure array [OUTPUT]

       character, intent(out) :: per_type*(*)         ! type of DSS data: PER-AVER or INST-VAL [OUTPUT] 

       !-----local variables
       logical :: lflags, &           ! true if data flags should be retrieved 
                  lfread              ! true if data flags were retrieved

       integer :: nvals ,istat,            &
                  pn_intvl,                & ! pathnumber for data block 
                  npath,na,nb,nc,nd,ne,nf, &
                  i,                       &
                  juls,istime,jule,ietime, & ! start/end date/time
                  itimes(irrs),            & ! irregular date/times of values relative to jbdate
                  jbdate,                  & ! julian base date of irregular data
                  kheadu,                  & ! number of headers to retrieve 
                  nheadu,                  & ! number of headers actually retrieved 
                  inflag,                  & ! flag to retrieve values before/after time block 
                  iofset,                  & ! time offset in minutes 
                  icomp                      ! compression method

       integer*4 :: jul_jbdate,            & ! jbdate in julian minutes 
                    ihm2m,inctim,          & ! DSS functions 
                    nmins,                 & ! number of minutes in this data interval 
                    flags(maxinpsize)        ! 32-bit data flags 
       real(gtm_real):: jmin2                ! test used for jmin

       real*4 :: values(maxinpsize)        & ! data values 
                 ,headu                      ! data headers

       character :: csdt*14,     &           ! date/time for start of data 
                    csdate*9,    &           ! starting date of data block 
                    cstime*4,    &           ! starting time of data block 
                    cedt*14,     &           ! nominal ending datetime of data block 
                    cunits*8,    &
                    ca*32, cb*32, cc*32, cd*32, ce*32, cf*32 !

       data lflags /.true./ &     ! get data flags 
           ,kheadu /0/ &         ! don't get data headers 
           ,inflag /3/ &         ! retrieve values before & after time block
           ,icomp /-1/          ! no compression

 610   format(/'Error on data read: ',a &
           /' ',a,' to ',a,' istat=',i8)
 620   format(/'Invalid ',a,' date: ',a)

       !-----Break up the pathnames
       call chrlnb(pathinput(pathnumber)%path,npath)
       call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne, &
            cf, nf, pathinput(pathnumber)%path, npath, istat)

       !-----convert starting date/time to the standard interval
       istat=1                   ! get minutes in interval, given E part
       call zgintl(nmins,ce,nvals,istat)
       if (istat .eq. 0) then    ! regular time-series convention
       !--------jmin must be set to an interval end...if in the middle of an
       !--------interval, use the previous end, else if at an end, use that
          call incr_intvl(jmin2, jmin,ce,NEAREST_BOUNDARY)
          if (jmin2 .ne. jmin) then ! not at interval end
             call incr_intvl(jmin, jmin,'-'//ce,NEAREST_BOUNDARY)
          endif
       endif

       csdt=jmin2cdt(int(jmin))
       csdate=csdt(1:9)
       cstime=csdt(11:14)
       !-----julian day/minute for start of data time
       call datjul(csdate, juls, istat)
       istime=ihm2m(cstime)
       if (istat .ne. 0 .or. istime .eq. -1) then
          write(unit_error,620) 'starting',csdt
          call exit(2)
       endif

       pn_intvl=pathinput(pathnumber)%intvl_path

       nvals=block_dim

       !-----Read the time block
       if (pathinput(pathnumber)%interval(1:3) .ne. 'ir-') then ! regular time series

          call zrrtsx(ifltab_in(1,pathinput(pathnumber)%ndx_file), &
              pathinput(pathnumber)%path, csdate, cstime, &
              nvals, values, flags, lflags, lfread, cunits, per_type, headu, &
              kheadu, nheadu, iofset, icomp, istat)

          cedt=jmin2cdt(cdt2jmin(csdt)+nvals*nmins) ! nominal end of data block

          if (istat .gt. 10) then
             write(unit_error, 610) pathinput(pathnumber)%path(1:npath), &
                 csdt,cedt,istat
             call exit(2)
          endif

          do i=1, nvals
             !-----------set julian minute for each data value
             istat=inctim(nmins, 0, i-1, juls, istime, jule, ietime)
             indata(i,pn_intvl)%julmin=jule*24*60+ietime
             !-----------fill data array
             indata(i,pn_intvl)%data=values(i)
             !-----------data quality flags, use from DSS file or user-specified value?
             indata(i,pn_intvl)%flag=flags(i)
          enddo
       else                      ! irregular time series
          !--------julian day/minute for end of data:
          !--------use end of run adjusted for data offset
          jule=(end_julmin+pathinput(pathnumber)%diff_julmin)/(24*60)
          ietime=end_julmin+pathinput(pathnumber)%diff_julmin-jule*24*60

          !--------zritsx doesn't initialize flags to zero
          do i=1,maxinpsize
             flags(i)=0
          enddo
          call zritsx(ifltab_in(1,pathinput(pathnumber)%ndx_file), &
               pathinput(pathnumber)%path, &
               juls, istime, jule, ietime, &
               itimes, values, irrs, nvals, jbdate, &
               flags, lflags, lfread, cunits, per_type, &
               headu, kheadu, nheadu, inflag, istat)

          jul_jbdate=jbdate*24*60
          cedt=jmin2cdt(itimes(max(nvals,1))+jul_jbdate)

          if (istat .gt. 10) then
             write(unit_error, 610) pathinput(pathnumber)%path(1:npath), &
                 csdt,cedt,istat
             call exit(2)
          endif

          do i=1, block_dim
             indata(i,pn_intvl)%julmin=end_julmin+1
             indata(i,pn_intvl)%data=-901.0
             indata(i,pn_intvl)%flag=0
          enddo

          do i=1, nvals
             indata(i,pn_intvl)%julmin=itimes(i)+jul_jbdate
             indata(i,pn_intvl)%data=values(i)
             !-----------data quality flags, use from DSS file or user-specified value?
             indata(i,pn_intvl)%flag=flags(i)
          enddo
          !--------if less values returned than block_dim, fill in remainder of data
          !--------array with last value so readtvd doesn't keep re-reading data.
          if (nvals .gt. 0) then
             do i=nvals+1, block_dim
                indata(i,pn_intvl)%julmin=indata(nvals,pn_intvl)%julmin
                indata(i,pn_intvl)%data=indata(nvals,pn_intvl)%data
                indata(i,pn_intvl)%flag=indata(nvals,pn_intvl)%flag
             enddo
          endif
       endif

       call upcase(per_type)

       return
    end subroutine

end module
