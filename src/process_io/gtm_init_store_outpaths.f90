!<license>
!    Copyright (C) 2015 State of California,
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
!> This module is used to initialize DSS output paths
!>@ingroup process_io  
module gtm_init_store_outputs

    use gtm_dss_write

    contains
  
   !> Initialization for storing time-varying output data in temporary
   !> files.
    subroutine gtm_init_store_outpaths(istat)

        use gtm_precision, only: miss_val_c
        use common_variables
        use common_dsm2_vars, only: temp_dir, pathoutput, NEAREST_BOUNDARY, &
                                    per_type_names, gtm_start_jmin
        use dsm2_time_utils, only: incr_intvl
        
        implicit none

        !-----local variables
        character :: tmp_dir*50,     &     ! scratch file directory
                     dir_arr(10)*30, &     ! list of possible scratch directories
                     dsm2mod*20,     &     ! dsm2modifier
                     tmp_file*80,    &     ! scratch file name
                     ctmp*100              ! scratch variable

        integer :: istat, &                ! file status 
                   ptr,   &                ! global pointer for pathouput 
                   i,     &                ! loop index 
                   npaths                  ! path count 
        character*1 :: backslash = '\'
        character*13 :: crid               ! run date/time as character: YYMMDDhhmm
        integer :: tmp_incr_intvl
        integer :: time_intv
        integer :: start_julmin

        call get_npathout        
        start_julmin = int(gtm_start_jmin)

        !-----For each interval with output data, create a scratch file
        !-----and write header info.

        !-----scratch directory; if not specified in user input, try
        !-----plausible locations
        dir_arr(1) = temp_dir
        dir_arr(2) = '/tmp'
        dir_arr(4) = 'c:' // backslash // 'temp'
        dir_arr(5) = 'c:' // backslash // 'tmp'
        dir_arr(6) = 'd:' // backslash // 'temp'
        dir_arr(7) = 'd:' // backslash // 'tmp'
        dir_arr(8) = '.'
        dir_arr(9) = miss_val_c     ! array list must end with this
        call getdir(ptr,dir_arr)
        if (ptr .ne. 0) then
            tmp_dir = dir_arr(ptr)
        else
 605        format(/a,(/a))
            write(unit_error,605) 'Could not find a valid directory in this list:', &
                                 (dir_arr(ptr),ptr=1,8)
            goto 901
        endif

        ctmp = trim(crid) // '.bin'

        call incr_intvl(julstout_minutes15, start_julmin,'15min', NEAREST_BOUNDARY)
        if (need_tmpfile_min15) then
            call mkfilename(tmp_dir, 'tmp_min15-' // trim(ctmp), tmp_file)
            scratch_file_array(1) = tmp_file
            open(unit=unit_min15,    &
                 file=tmp_file,      &
                 form='unformatted', &
                 buffered='yes',     &
                 buffercount=50,     &
                 iostat=istat,       &
                 err=901)
            !--------count number of paths to write
            npaths = 0
            do i = 1, npthsout_min15
                ptr = ptout_min15(i)
                if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
            enddo
            write (unit=unit_min15) npaths

            do i = 1, npthsout_min15
                ptr = ptout_min15(i)
                !-----only write those paths that need tmp file output
                if (pathoutput(ptr)%need_tmp_outfile) &
                    write (unit=unit_min15) ptr,pathoutput(ptr), &
                    per_type_names(pathoutput(ptr)%per_type)
            enddo
        else
             scratch_file_array(1) = miss_val_c
        endif

        call incr_intvl(julstout_hours1, start_julmin,'1hour', &
                        NEAREST_BOUNDARY)
        if (need_tmpfile_hour1) then
            call mkfilename(tmp_dir, 'tmp_hour1-' // trim(ctmp), tmp_file)
            scratch_file_array(2) = tmp_file
            open(unit=unit_hour1,    &
                 file=tmp_file,      &
                 form='unformatted', &
                 buffered='yes',     &
                 buffercount=50,     &
                 iostat=istat,       &
                 err=901)
            !--------count number of paths to write
            npaths = 0
            do i = 1, npthsout_hour1
                ptr = ptout_hour1(i)
                if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
            enddo
            write (unit=unit_hour1) npaths

            do i=1,npthsout_hour1
                ptr=ptout_hour1(i)
                !-----------only write those paths that need tmp file output
                if (pathoutput(ptr)%need_tmp_outfile) &
                    write (unit=unit_hour1) ptr,pathoutput(ptr), &
                    per_type_names(pathoutput(ptr)%per_type)
            enddo
        else
            scratch_file_array(2) = miss_val_c
        endif

        call incr_intvl(julstout_days1, start_julmin,'1day', &
             NEAREST_BOUNDARY)
        if (need_tmpfile_day1) then
            call mkfilename(tmp_dir, 'tmp_day1-' // trim(ctmp), tmp_file)
            scratch_file_array(3)=tmp_file
            open(unit=unit_day1,     &
                 file=tmp_file,      &
                 form='unformatted', &
                 buffered='yes',     &
                 buffercount=10,     &
                 iostat=istat,       &
                 err=901)
            !--------count number of paths to write
            npaths = 0
            do i = 1, npthsout_day1
                ptr = ptout_day1(i)
                if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
            enddo
            write (unit=unit_day1) npaths

            do i = 1, npthsout_day1
                ptr = ptout_day1(i)
                !-----------only write those paths that need tmp file output
                if (pathoutput(ptr)%need_tmp_outfile) &
                write (unit=unit_day1) ptr,pathoutput(ptr), &
                per_type_names(pathoutput(ptr)%per_type)
            enddo
        else
            scratch_file_array(3)=miss_val_c
        endif

        call incr_intvl(julstout_weeks1, start_julmin,'1week', &
             NEAREST_BOUNDARY)  
        if (need_tmpfile_week1) then
            call mkfilename(tmp_dir, 'tmp_week1-' // trim(ctmp), tmp_file)
            scratch_file_array(4) = tmp_file
            open(unit=unit_week1,    &
                 file=tmp_file,      &
                 form='unformatted', &
                 iostat=istat,       &
                 err=901)
            !--------count number of paths to write
            npaths = 0
            do i = 1, npthsout_week1
                ptr = ptout_week1(i)
                if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
            enddo
            write (unit=unit_week1) npaths

            do i = 1, npthsout_week1
                ptr = ptout_week1(i)
                !-----------only write those paths that need tmp file output
                if (pathoutput(ptr)%need_tmp_outfile) &
                    write (unit=unit_week1) ptr,pathoutput(ptr), &
                    per_type_names(pathoutput(ptr)%per_type)
            enddo
        else
            scratch_file_array(4) = miss_val_c
        endif

        call incr_intvl(julstout_months1, start_julmin,'1month', &
             NEAREST_BOUNDARY)
        if (need_tmpfile_month1) then
            call mkfilename(tmp_dir, 'tmp_month1-' // trim(ctmp), tmp_file)
                 scratch_file_array(5) = tmp_file
            open(unit=unit_month1,   &
                 file=tmp_file,      &
                 form='unformatted', &
                 iostat=istat,       &
                 err=901)
            !--------count number of paths to write
            npaths = 0
            do i = 1, npthsout_month1
                ptr = ptout_month1(i)
                if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
            enddo
            write (unit=unit_month1) npaths

            do i = 1, npthsout_month1
                ptr = ptout_month1(i)
                !-----------only write those paths that need tmp file output
                if (pathoutput(ptr)%need_tmp_outfile) &
                    write (unit=unit_month1) ptr,pathoutput(ptr), &
                          per_type_names(pathoutput(ptr)%per_type)
            enddo
        else
            scratch_file_array(5) = miss_val_c
        endif

        call incr_intvl(julstout_years1, start_julmin,'1year', &
             NEAREST_BOUNDARY)
        if (need_tmpfile_year1) then
            call mkfilename(tmp_dir, 'tmp_year1-' // trim(ctmp), tmp_file)
            scratch_file_array(6) = tmp_file
            open(unit=unit_year1,    &
                 file=tmp_file,      &
                 form='unformatted', &
                 iostat=istat,       &
                 err=901)
            !--------count number of paths to write
            npaths = 0
            do i = 1, npthsout_year1
                ptr = ptout_year1(i)
                if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
            enddo
            write (unit=unit_year1) npaths

            do i = 1, npthsout_year1
                ptr = ptout_year1(i)
                !-----------only write those paths that need tmp file output
                if (pathoutput(ptr)%need_tmp_outfile) &
                    write (unit=unit_year1) ptr,pathoutput(ptr), &
                    per_type_names(pathoutput(ptr)%per_type)
            enddo
        else
            scratch_file_array(6) = miss_val_c
        endif
        
        return
        
901     continue                  ! scratch file open error

        write(unit_error, "('Error opening binary scratch file',1x,a)")tmp_file
        call exit(2)
        return
    end subroutine


    !> get number of path for each output period
    subroutine get_npathout
        use common_variables, only: unit_error
        use common_dsm2_vars, only: noutpaths, pathoutput, dsm2_name
        implicit none
       
        character :: ca*32,cb*32,cc*32,cf*32 ! DSS path parts
        character :: path*(6*32)             ! temp DSS pathname              
        integer :: p, nlen
        integer :: istat = 0
       
        do p = 1, noutpaths
            if (pathoutput(p).no_intervals .eq. 1 .and.           &
                pathoutput(p).interval(1:5) .eq. '15min') then
                npthsout_min15=npthsout_min15+1
                if (npthsout_min15 .gt. max_out_min) then
                    write(unit_error, 652) '15MIN',max_out_min
                    goto 900
                endif
                pathoutput(p).intvl_path=npthsout_min15
                ptout_min15(npthsout_min15)=p
                if (pathoutput(p).need_tmp_outfile) then
                    need_tmpfile_min15=.true.
                endif
            else if (pathoutput(p).no_intervals .eq. 1 .and.    &
                    pathoutput(p).interval(1:5) .eq. '1hour') then
                npthsout_hour1=npthsout_hour1+1
                if (npthsout_hour1 .gt. max_out_hour) then
                    write(unit_error, 652) '1HOUR',max_out_hour
                    goto 900
                endif
                pathoutput(p).intvl_path=npthsout_hour1
                ptout_hour1(npthsout_hour1)=p
                if (pathoutput(p).need_tmp_outfile) need_tmpfile_hour1=.true.
            else if (pathoutput(p).no_intervals .eq. 1 .and.     &
                     pathoutput(p).interval(1:4) .eq. '1day') then
                npthsout_day1=npthsout_day1+1
                if (npthsout_day1 .gt. max_out_day) then
                   write(unit_error, 652) '1DAY',max_out_day
                   goto 900
                endif
                pathoutput(p).intvl_path=npthsout_day1
                ptout_day1(npthsout_day1)=p
                if (pathoutput(p).need_tmp_outfile) need_tmpfile_day1=.true.
                else if (pathoutput(p).no_intervals .eq. 1 .and.     &
                         pathoutput(p).interval(1:5) .eq. '1week') then
                    npthsout_week1=npthsout_week1+1
                if (npthsout_week1 .gt. max_out_week) then
                    write(unit_error, 652) '1WEEK',max_out_week
                    goto 900
                endif
                pathoutput(p).intvl_path=npthsout_week1
                ptout_week1(npthsout_week1)=p
                if (pathoutput(p).need_tmp_outfile) need_tmpfile_week1=.true.
            else if (pathoutput(p).no_intervals .eq. 1 .and.      &
                        pathoutput(p).interval(1:4) .eq. '1mon') then
                        npthsout_month1=npthsout_month1+1
               if (npthsout_month1 .gt. max_out_month) then
                   write(unit_error, 652) '1MON',max_out_month
                   goto 900
               endif
               pathoutput(p).intvl_path=npthsout_month1
               ptout_month1(npthsout_month1)=p
               if (pathoutput(p).need_tmp_outfile) need_tmpfile_month1=.true.
            else if (pathoutput(p).no_intervals .eq. 1 .and.      &
                     pathoutput(p).interval(1:5) .eq. '1year') then
                     npthsout_year1=npthsout_year1+1
                if (npthsout_year1 .gt. max_out_year) then
                    write(unit_error, 652) '1YEAR',max_out_year
                    goto 900
                endif
                pathoutput(p).intvl_path=npthsout_year1
                ptout_year1(npthsout_year1)=p
                if (pathoutput(p).need_tmp_outfile) need_tmpfile_year1=.true.
            else                   ! unrecognized interval
                write(unit_error,650) 'output',pathoutput(p).no_intervals,  &
                    pathoutput(p).interval
                goto 900
            endif

            ! define DSS pathname
            if (pathoutput(p).a_part .ne. ' ') then
                ca = pathoutput(p).a_part
            else 
                ca = trim(dsm2_name)
            endif
            if (pathoutput(p).b_part .ne. ' ') then
                cb = pathoutput(p).b_part
            else 
                cb = pathoutput(p).name
            endif
            if (pathoutput(p).c_part .ne. ' ') then
                cc = pathoutput(p).c_part
            else         
                cc = pathoutput(p).meas_type
            endif
            if (pathoutput(p).f_part .ne. ' ') then
                cf = pathoutput(p).f_part
            else          
                if (pathoutput(p).modifier .ne. ' ') then
                   cf = pathoutput(p).modifier
                else
                   cf = 'NONE'
                endif
            end if

            path = '/'  &
                   // trim(ca) // '/'                     &! a part 
                   // trim(cb) // '/'                     &! b part
                   // trim(cc) // '/'                     &! c part
                   // '/'                                 &! d part
                   // trim(pathoutput(p).interval) // '/' &! e part
                   // trim(cf) // '/'                      ! f part

            ! define units for each output path
            if (pathoutput(p).c_part .eq. 'ec') then
                pathoutput(p).units = 'umhos/cm'
            else 
                pathoutput(p).units = 'mg/l'
            endif

            call remblk(path,pathoutput(p).path,nlen)
            call upcase(pathoutput(p).path) ! convert to upper case
            call zchkpn(trim(path),len_trim(path),istat)
            !if (istat .ne. 0) then
            !   write(unit_error,"(a,a,a,i5)")"Illegal pathname: ",  &
            !       trim(path)," status: ",istat
            !   goto 900
            !end if
        end do 

650     format(/'Unrecognized ',a,' data interval: ',i4,1x,a)        
652     format(/'Number of output paths for ',a,' exceed allowable limit:',i4)
900     return
    end subroutine       
      
      
    !> Find a usable directory from the given list; return the
    !> array index of the one to use.  The list must end with
    !> miss_val_c.
    subroutine getdir(getdir_out, dir_arr)
        implicit none
        integer, intent(out) :: getdir_out
        character*(*), intent(in) :: dir_arr(*)  ! list of directory names to try
        integer :: ndx,         &                ! directory array index &
                   nlen,        &                ! character length &
                   statarr(13), &                ! file status array &
                   stat,        &                ! file status intrinsic function &
                   istat,       &                ! file status value &
                   lnblnk                        ! intrinsic function
        ndx = 1
        do while (dir_arr(ndx) .ne. miss_val_c)
            nlen=lnblnk(dir_arr(ndx))
            if (nlen .eq. 0) goto 100
            istat=stat(dir_arr(ndx),statarr)
            if (istat .eq. 0) then ! this directory name ok
                getdir_out = ndx
                return
            endif
 100        continue
            ndx = ndx + 1
        enddo
        getdir_out = 0
        return
    end subroutine


    !> Make a full filename (directory + filename) from directory name
    !> and filename.
    subroutine mkfilename(dir,    &
                          file,   &
                          dirfile)
        implicit none
        !-----arguments
        character*(*) :: dir,     &          ! directory name &
                         file,    &          ! filename &
                         dirfile             ! directory+filename

        !-----local variables
        integer :: nlen,   &             ! length of character string &
                   ndx,    &             ! array index &
                   lnblnk, &             ! intrinsic &
                   index                 ! intrinsic

        character :: dirchar              ! directory delimiter (/ or \)
        character*1 :: backslash = '\'
        
        nlen=lnblnk(dir)
        !-----try to find / or \ in directory name
        ndx=index(dir,'/')
        if (ndx .gt. 0) then      ! unix
           dirchar=dir(ndx:ndx)
        else
           ndx=index(dir,backslash)
           if (ndx .gt. 0) then   ! pc
              dirchar=dir(ndx:ndx)
           else                   ! unknown
              dirchar='/'
           endif
        endif
        !-----directory name must end in either / or \ before
        !-----appending filename
        if (dir(nlen:nlen) .ne. dirchar) then
           nlen=nlen+1
           dir(nlen:nlen)=dirchar
        endif
        dirfile=dir(:nlen) // file
        return
    end subroutine

end module