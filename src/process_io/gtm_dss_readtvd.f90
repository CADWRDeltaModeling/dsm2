

!> Retrieve time-varying data from DSS, if necessary, process
!> further (interpolate, fillin missing, ...)
!> This module is borrowed from DSM2/time_var project with 
!> slight modification. This module was originally developed
!> by Ralph Finch in 1996.
!>@ingroup process_io
module gtm_dss_readtvd

    contains
    
    !> Retrieve time-varying data from DSS, if necessary, and then
    !> process further (interpolate, fillin missing, ...)
    subroutine readtvd (indata,      &
                        jmin,        & 
                        prev_jmin,   &
                        inpaths_dim, & 
                        block_dim,   &
                        npaths,      &
                        inpath_ptr) 
        use gtm_dss, only: last_value, last_ndx_next,     &
                           allocate_last_value, deallocate_last_value
        use gtm_dss_read
        use common_dsm2_vars
        use time_utilities
       
        implicit none
        integer, intent(in) :: jmin                     !< current julmin  
        integer, intent(in) :: prev_jmin                !< previous time step julmin
        integer, intent(in) :: inpaths_dim              !< data block array dimension
        integer, intent(in) :: block_dim                !< data block array dimension
        integer, intent(in) :: npaths                   !< number of total input paths, including all time intervals
        integer, intent(in) :: inpath_ptr(inpaths_dim)  !< pointer array to global input pathnames
        type(dataqual_t), intent(out) :: &
                       indata(block_dim,inpaths_dim)    !< raw input data structure array

        !-----local variables
       
        type(dataqual_t):: tmpval
        logical :: interpolate_value                    ! true to interpolate this path's final value
        integer :: ptr                                  ! status
        integer :: i                                    ! status
        integer :: lnm                                  ! status
        integer :: ndx                                  ! status
        integer :: ndx2                                 ! status
        integer :: ndx_next                             ! status
        integer :: ndx_prev_curr                        ! status
        integer :: bufndx_sync                          ! status
        integer :: bufndx_next_nosync                   ! status
        integer :: bufndx_next_sync                     ! status
        integer :: nmins_intvl                          ! status
        integer :: nvals                                ! status
        integer :: istat                                ! status

        integer*4 :: jul_next                           ! increment time interval function
        integer*4 :: jul_prev_curr                      ! increment time interval function
        integer*4 :: jul                                ! increment time interval function
        integer*4 :: jul2                               ! increment time interval function
        integer*4 :: js_data                            ! increment time interval function
        integer*4 :: jm_next                            ! increment time interval function
        integer*4 :: timediff_dat                       ! increment time interval function
        integer*4 :: timediff_val                       ! increment time interval function
 
        double precision :: val1                        ! values used for interpolating
        double precision :: val2                        ! values used for interpolating
 
        character*14 :: datetime1
        character*14 :: datetime2
        character*5 :: current_sync                     ! synchronize string for model time
        character*8 :: per_type                         ! per-aver or inst-val
        character*32 :: ce                              ! DSS E part

        type(dataqual_t)::  indata_tmp
 
        per_type_names(per_type_per_aver) = 'PER-AVER'
        per_type_names(per_type_per_cum) = 'PER-CUM'
        per_type_names(per_type_per_min) = 'PER-MIN'
        per_type_names(per_type_per_max) = 'PER-MAX'
        per_type_names(per_type_inst_val) = 'INST-VAL'
        per_type_names(per_type_inst_cum) = 'INST-CUM'
        per_type_names(per_type_null) = ' '
       
        call allocate_last_value(n_inputpaths)

 610    format(/a,' data in path:'/a &
              /' At data time: ',a,' model time: ',a &
              /' Using replacement value of ',1p g10.3,' from ',a, &
              /' or a lower priority path')
 615    format(/'Missing or rejected data for interpolation in path:'/a &
              /' At data time: ',a,' model time: ',a &
              /' Using replacement value of ',1p g10.3,' from ',a)
 611    format(/'Missing or rejected data in path:' &
              /' ',a &
              /' at data time: ',a,' model time: ',a &
              /' Cannot continue.')
 613    format(/'Software error in readtvd: Bad ndx_prev_curr for path:' &
              /' ',a &
              /' at data time: ',a,' model time: ',a &
              /' Cannot continue.')
 620    format(/'Error in reading time-varying data:' &
              /'Current time is ',a,'; earliest data time for '/a &
              /'is ',a/'If this is an irregular series, note that HEC-DSS does not look back to ' &
              /'previous DSS time blocks (the previous decade if the E Part is IR-DECADE). The workaround' &
              /'for this problem is to repeat the prior data point on the first step of the decade/year' &
              )
 625    format(/'Error in reading time-varying data:' &
              /'Current time is ',a,'; all data times for '/a &
              /' are before this time.')
 626    format(/'Error in reading time-varying data:' &
              /'Current time is ',a,'; data synchronization request for '/a &
              /' could not be satisfied.')
 630    format(/'Unrecognized data period type: ',a &
              /' for path: ',a)

        !-----Check if new data needs to be read from DSS to arrays
        do i = 1,inpaths_dim
            ptr=inpath_ptr(i)
            pathinput(ptr)%replace=.false.
            if (pathinput(ptr)%constant_value == miss_val_r) then ! get value from dss file
               if ( (jmin+pathinput(ptr)%diff_julmin >= &
                   indata(block_dim,i)%julmin ) .or. &
                   prev_jmin == start_julmin) then
                   js_data=jmin+pathinput(ptr)%diff_julmin
                   call readdss(ptr,js_data,inpaths_dim,block_dim,indata, &
                        per_type)
                   if (per_type == ' ') then ! none, assume instantaneous
                      pathinput(ptr)%per_type=per_type_inst_val
                   else if (per_type == per_type_names(per_type_inst_val)) then
                      pathinput(ptr)%per_type=per_type_inst_val
                   else if (per_type == per_type_names(per_type_per_aver)) then
                      pathinput(ptr)%per_type=per_type_per_aver
                   else if (per_type == per_type_names(per_type_inst_cum)) then
                      pathinput(ptr)%per_type=per_type_inst_cum
                   else if (per_type == per_type_names(per_type_per_cum)) then
                      pathinput(ptr)%per_type=per_type_per_cum
                   else
                      write(unit_error,630) per_type, &
                           trim(pathinput(ptr)%path)
                      call exit(2)
                   endif
               endif
            endif
         enddo

         !-----force initial calculation of buffer indices
         bufndx_next_sync=-1
         bufndx_next_nosync=-1
         ndx_next = -1
         do i=1,inpaths_dim
            ptr=inpath_ptr(i)
            if (pathinput(ptr)%constant_value /= miss_val_r) then ! use constant value
               pathinput(ptr)%value=pathinput(ptr)%constant_value
               tmpval%data=pathinput(ptr)%value
               tmpval%flag=pathinput(ptr)%value_flag
               call set_dataqual(tmpval,good_data)
               pathinput(ptr)%value_flag=tmpval%flag
               goto 100
            endif

            !----use value from DSS file
            !----should this path's value be interpolated?
            !----don't interpolate if not requested or gate values
            !         if (
            !     &        pathinput(ptr)%fillin .eq. fill_interp
            !     &        .and. pathinput(ptr)%per_type .ne. per_type_inst_val) then
            !         print*,pathinput(ptr)%name, " is weird"
            !         end if
            interpolate_value=(pathinput(ptr)%fillin == fill_interp .or. &
               (pathinput(ptr)%fillin == fill_bydata .and. &
               (pathinput(ptr)%per_type == per_type_inst_val .or. &
               pathinput(ptr)%per_type == per_type_inst_cum))) .and. &
               index(pathinput(ptr)%path, 'GATE') == 0

            !----if this path has a different start date offset than the previous
            !----path, force recalculation of buffer indices
            if (i > 1) then
               if( &
                 pathinput(inpath_ptr(i-1))%diff_julmin /= &
                 pathinput(ptr)%diff_julmin) then
                 bufndx_next_sync=-1
                 bufndx_next_nosync=-1
               end if
            endif
  
            !----ndx_next is index in dss buffer for data forward of current
            !----time step; depends on whether data is to be synced or not
            !----calculate this once each for synchronized and non-synchronized
            !----paths, for regular data; for irregular, calc for every path
         
            if (bufndx_next_nosync == -1 .or. &
                 pathinput(ptr)%interval(:3) == 'ir-') then
                 ndx_next=bufndx_nosync(indata, jmin+pathinput(ptr)%diff_julmin, &
                 i, last_ndx_next(ptr), block_dim, inpaths_dim)
                 bufndx_next_nosync = ndx_next
            else
                 ndx_next=bufndx_next_nosync
            endif

            if (ndx_next == -1) then
               !--------------if the 'last' value is wanted, finding newer data doesn't matter
               if (interpolate_value) then
                  write(unit_error,625) trim(current_date),trim(pathinput(ptr)%path)
                  call exit(2)
               else             ! simply use last data available
                  ndx_next=block_dim ! readdss.f copies last value to end of buffer
               endif
            endif
            jul_next=indata(ndx_next,i)%julmin

            !----if interpolation wanted, but next value is bad (and not generic
            !----data), turn off interpolation and try to replace this value
            !----later
            if (interpolate_value .and. &
               (check_dataqual(indata(ndx_next,i),miss_or_rej_data) .and. &
                pathinput(ptr)%start_date /= generic_date)) then
                interpolate_value = .false.
                pathinput(ptr)%replace = .true.
            endif

            !----fixme: check this if statement
            if (ndx_next == 1 .and. &
                pathinput(ptr)%interval(:3) == 'ir-') then
                ! all irregular data for this path is after current time
                datetime1=jmin2cdt(indata(1,i)%julmin)
                write(unit_error,620) trim(current_date),trim(pathinput(ptr)%path)
                call exit(2)
            endif

            !----index in dss buffer for data at previous or current time step
            if (ndx_next >= 2) then
                ndx_prev_curr=ndx_next-1
            else                   ! this shouldn't happen
                datetime1=jmin2cdt(indata(ndx_next,i)%julmin)
                write(unit_error,613) &
                   trim(pathinput(ptr)%path), &
                datetime1,current_date
                call exit(2)
            endif
            !----julian minute of previous or current data value
            jul_prev_curr = indata(ndx_prev_curr,i)%julmin

            !----ndx points to which data value to use
            ndx = getndx(jmin, jul_next, jul_prev_curr, ndx_next, &
                  ndx_prev_curr, pathinput(ptr)%per_type, interpolate_value)

            indata_tmp=indata(ndx,i) ! in case indata missing value is replaced later

            !----initialize last_value to use for missing data
            if (prev_jmin == start_julmin) then
                last_value(ptr)%data=miss_val_r
                call set_dataqual(last_value(ptr),reject_data)
            endif

            !----for interpolated value, need second value
            if (interpolate_value) then
               if (ndx == ndx_next) then
                   ndx2=ndx_prev_curr
               else
                   ndx2=ndx_next
               endif
               jul=indata(ndx,i)%julmin
               jul2=indata(ndx2,i)%julmin
               timediff_dat=jul2-jul
               timediff_val=jmin - (jul-pathinput(ptr)%diff_julmin)
               tmpval=indata(ndx2,i)
               val1=indata(ndx,i)%data
               val2=indata(ndx2,i)%data
            endif
            jul_next=indata(ndx_next,i)%julmin
            jul_prev_curr=indata(ndx_prev_curr,i)%julmin

            !----check for questionable, missing, or rejected data
            if (check_dataqual(indata(ndx,i),question_data) .or. &
                check_dataqual(indata(ndx,i),miss_or_rej_data)) then ! bad data...
                datetime1=jmin2cdt(jul_prev_curr)
               !-----continue if user requests it and good data is available
               !-----to fill in; or continue if the path is part of a priority list
               !-----(a last check will be made for bogus data just before use)
               if ( (cont_question .or. cont_missing) .and. &
                   (.not. check_dataqual(last_value(ptr),question_data) .and. &
                   .not. check_dataqual(last_value(ptr),miss_or_rej_data))  &
                   ) then
                  if (warn_question .and. &
                      check_dataqual(indata(ndx,i),question_data) .and. &
                      .not. check_dataqual(last_value(ptr),miss_or_rej_data)) then
                      datetime2=jmin2cdt(last_value(ptr)%julmin)
                      write(unit_screen, 610) &
                         'Questionable', &
                         trim(pathinput(ptr)%path), &
                         datetime1,current_date,last_value(ptr)%data, &
                         datetime2
                  endif
                  if ( warn_missing .and. &
                      check_dataqual(indata(ndx,i),miss_or_rej_data) .and. &
                      .not. check_dataqual(last_value(ptr),miss_or_rej_data)) then
                      datetime2=jmin2cdt(last_value(ptr)%julmin)
                      write(unit_screen, 610) &
                          'Missing or rejected', &
                         trim(pathinput(ptr)%path), &
                         datetime1,current_date,last_value(ptr)%data, &
                         datetime2
                  endif
                  pathinput(ptr)%replace=.true. ! later try to replace this
                  indata(ndx,i)=last_value(ptr)
                  if (interpolate_value) then
                     val1=indata(ndx,i)%data
                  endif
               else                ! don't continue on quest/miss/rej data, and no replacement data available
                     write(unit_error, 611) &
                         trim(pathinput(ptr)%path), &
                        datetime1,current_date
                     call exit(2)
               endif
            endif

            !----check for missing data of other index
            if (interpolate_value .and. check_dataqual(tmpval,miss_or_rej_data)) then
            !-----if generic date, missing value means recycle to first value
            !-----assume that the full range of data can fit into a data block
               if (pathinput(ptr)%start_date == generic_date) then
                  val2=indata(1,i)%data
               else                ! not generic
                  datetime1=jmin2cdt(jul2)
                  !------continue if user requests it and good data is available
                  !------to fill in, or the path is for replacement only
                  if ( (cont_missing .and. &
                      .not. check_dataqual(last_value(ptr),miss_or_rej_data))  &
                     ) then
                     pathinput(ptr)%replace=.true. ! later try to replace this
                     val2=last_value(ptr)%data
                     if (warn_missing) then
                        datetime2=jmin2cdt(last_value(ptr)%julmin)
                        write(unit_screen, 615) &
                            trim(pathinput(ptr)%path), &
                            datetime1,current_date,last_value(ptr)%data, &
                            datetime2
                     endif
                  else
                     write(unit_error, 611) &
                         trim(pathinput(ptr)%path), &
                         datetime1,current_date
                     call exit(2)
                  endif
               endif
            endif
            last_value(ptr)=indata(ndx,i) ! in case we wish to replace missing data
 
            if (interpolate_value) then
               !---interpolate to end of time step
               pathinput(ptr)%value= val1 + (val2-val1) * &
                   float(timediff_val) / float(timediff_dat)
               pathinput(ptr)%value_flag=indata(ndx,i)%flag
            else                   ! don't interpolate
               pathinput(ptr)%value=indata(ndx,i)%data
               pathinput(ptr)%value_flag=indata(ndx,i)%flag
            endif

            if (pathinput(ptr)%start_date /= generic_date) then ! kluge upon kluge
               indata(ndx,i)=indata_tmp
            endif
  
 100        continue
 
            !---change sign if desired 
            if (pathinput(ptr)%sign == -1) then
               pathinput(ptr)%value=-pathinput(ptr)%value
            else if (pathinput(ptr)%sign == 1) then
               pathinput(ptr)%value=pathinput(ptr)%value
            endif
            !---change value if desired
            if (pathinput(ptr)%value_in == pathinput(ptr)%value) &
                pathinput(ptr)%value=pathinput(ptr)%value_out
  
            last_ndx_next(ptr)=ndx_next
  
         enddo

         call deallocate_last_value
       
         return
    end subroutine


    !> Find index in julian minute array that is less than
    !> target julian minute.
    integer function bufndx_nosync(indata, jm, path, last_ndx, &
          max_v, max_paths)
        use common_dsm2_vars, only: dataqual_t
        implicit none

        !-----arguments and local variables
        integer :: last_ndx                 ! path index
        integer :: max_v                    ! path index
        integer :: max_paths                ! path index
        integer :: i                        ! path index
        integer :: path                     ! path index

        type(dataqual_t) :: indata(max_v,max_paths) ! input data structure array

        integer*4 :: jm                     ! current julian minute

        do i=1, max_v
           if (indata(i,path)%julmin > jm) then
              bufndx_nosync = i
              return
           endif
        enddo

        bufndx_nosync = -1                  ! all data is old

        return
    end function
    

    !> Find index in julian minute array that matches the DSS part to
    !> synchronize with the current time
    integer function bufndx_sync(indata, path, sync_str, e_part, &
          last_ndx, max_v, max_paths)
          
        use common_dsm2_vars, only: dataqual_t
        use time_utilities
      
        implicit none

        !-----arguments and local variables
        integer :: last_ndx                 ! path index
        integer :: max_v                    ! path index
        integer :: max_paths                ! path index
        integer :: i                        ! path index
        integer :: path                     ! path index

        type(dataqual_t) :: indata(max_v,max_paths) ! input data structure array

        character*(*) :: sync_str*(*)       ! synchronize on e_part in data time
        character*(*) :: e_part             ! synchronize on e_part in data time
        character*14 ::  jmv_cdt*14         ! julian minute to character function  

        character*5 ::  jmv_intvl           ! interval strings for jmv_cdt

        !-----check last timestep's value, probably still good
        jmv_cdt=jmin2cdt(indata(last_ndx,path)%julmin)
        call get_intvl(jmv_cdt, e_part, jmv_intvl)
        if (sync_str == jmv_intvl) then
           bufndx_sync = last_ndx
           return
        else
           do i=1, max_v
              jmv_cdt=jmin2cdt(indata(i,path)%julmin)
              call get_intvl(jmv_cdt, e_part, jmv_intvl)
              if (sync_str == jmv_intvl) then
                 bufndx_sync = i
                 return
              endif
           enddo
        endif

        bufndx_sync = -1                      ! couldn't synchronize

        return
    end function
      
      
    !> Return either next or previous data index as the base index to
    !> use for correct data for this timestep.
    integer function getndx(jmin, jul_next, jul_prev_curr, &
            ndx_next, ndx_prev_curr, per_type, interpolated)
 
        use common_dsm2_vars
        use time_utilities
        implicit none

        logical :: interpolated         ! true if this path's value is to be interpolated

        integer*4 :: jmin               ! julian minutes of data forward, and back of or at, current time step
        integer*4 :: jul_next           ! julian minutes of data forward, and back of or at, current time step
        integer*4 :: jul_prev_curr      ! julian minutes of data forward, and back of or at, current time step

        integer :: ndx_next             ! per-average, instantaneous, etc.
        integer :: ndx_prev_curr        ! per-average, instantaneous, etc.
        integer :: per_type             ! per-average, instantaneous, etc.

        !---for instantaneous values, use previous or current,
        !---whether interpolated or not;
        !---for period average values, use next or current if
        !---not interpolated, use previous if interpolated
        !---fixme: for interpolated period average, really the
        !---other ndx to use should change midway thru the time period
        getndx = -9999
        !-----always use prev_curr index if current time and data time are equal
        if (jmin == jul_prev_curr) then
            getndx=ndx_prev_curr
        else
            if (per_type == per_type_inst_val .or. &
                per_type == per_type_inst_cum) then ! instantaneous
                getndx=ndx_prev_curr
            elseif (per_type == per_type_per_aver .or. &
                per_type == per_type_per_cum) then ! period average
                if (.not. interpolated) then
                   getndx=ndx_next
                else
                   getndx=ndx_prev_curr
                endif
            endif
        endif
        return
    end function


    !> Find first missing value in data vector for path
    integer function find_miss(indata, path, max_v, max_paths)
        use common_dsm2_vars
        implicit none 
        integer :: max_v                            ! path index
        integer :: max_paths                        ! path index
        integer :: path                             ! path index

        type(dataqual_t) :: indata(max_v,max_paths) ! input data structure array

        do find_miss = 1, max_v
           if (check_dataqual(indata(find_miss,path),miss_or_rej_data)) return
        enddo

        find_miss = max_v

        return
    end function
    
    
    !> Check the quality of data.
    logical function check_dataqual(value, qualflag)
        use common_dsm2_vars
        implicit none
        type(dataqual_t) :: value          ! data value to be tested [INPUT]
        integer :: qualflag                ! type of quality flag to check [INPUT]
        logical :: btest                   ! external bit checking function

        if (qualflag == screened_data) then
           check_dataqual=btest(value%flag,screened_bit) .or. &
               value%data == -901. .or. &  ! missing data is considered as screened &
               value%data == -902.
        else if (qualflag == good_data) then
           check_dataqual=btest(value%flag,good_bit)
        else if (qualflag == missing_data) then
           check_dataqual=value%data == -901. .or. &
               value%data == -902. .or. &
               btest(value%flag,missing_bit)
        else if (qualflag == question_data) then
           check_dataqual=btest(value%flag,question_bit)
        else if (qualflag == reject_data) then
           check_dataqual=btest(value%flag,reject_bit)
        else if (qualflag == miss_or_rej_data) then
           check_dataqual=value%data == -901. .or. &
               value%data == -902. .or. &
               btest(value%flag,missing_bit) .or. &
               btest(value%flag,reject_bit)
        else                                ! unknown incoming flag
           write(unit_error,*) 'Software error in check_dataqual; ', &
               'unknown qualflag value: ', qualflag
        endif
        return
    end function


    !> Set the quality data flags.
    subroutine set_dataqual(value, qualflag)
        use common_dsm2_vars
        implicit none
  
        type(dataqual_t):: value     ! data value to be set [INPUT, OUTPUT]
        integer :: qualflag          ! type of quality flag to check [INPUT]

        value%flag = and(value%flag,0)
        value%flag = ibset(value%flag,screened_bit)
        if (qualflag == good_data) then
           value%flag = ibset(value%flag,good_bit)
        else if (qualflag == missing_data) then
           value%data=miss_val_r
           value%flag = ibset(value%flag,missing_bit)
        else if (qualflag == question_data) then
           value%flag = ibset(value%flag,question_bit)
        else if (qualflag == reject_data) then
           value%flag = ibset(value%flag,reject_bit)
        else                        ! unknown incoming flag
           write(unit_error,*) 'Software error in set_dataqual; ', &
               'unknown qualflag value: ', qualflag
        endif
      return
    end subroutine


    !> Get input data from buffers for computations
    subroutine get_inp_data(ptr)
        use common_dsm2_vars
        implicit none  
        integer :: ptr                  ! pathname array index
        type(dataqual_t) :: dataqual

 610    format(/'No replacement path given for ' &
               /a &
               /' however bad value encountered at model time ',a)
 612    format(/'Error in get_inp_data: Missing data in path/file:' &
               /' ',a &
               /' ',a &
               /' at model time: ',a &
               /' Cannot continue.')
 613    format(/a/a/'at model time: ',a)

        !----last check for missing data
        dataqual%data=pathinput(ptr)%value
        dataqual%flag=pathinput(ptr)%value_flag
        if (check_dataqual(dataqual,miss_or_rej_data)) then
           write(unit_error, 612) &
               trim(pathinput(ptr)%path),trim(pathinput(ptr)%filename), &
               current_date
           call exit(2)
        endif

        !----warning msgs about questionable or unscreened data;
        !----check to continue run
        if (.not. check_dataqual(dataqual,screened_data)) then
           if (warn_unchecked .or. .not. cont_unchecked) then
              write(unit_error,613) 'Warning: unchecked data: ', &
                  trim(pathinput(ptr)%path), &
                  current_date
           endif
           if (.not. cont_unchecked) then
              write(unit_error,*) 'Fatal error.'
              !call exit(2)        !todo:: temporarily comment out by ehsu. Why it gets here? need to ask Ralph.
           else if (warn_unchecked) then
              write(unit_error,*) 'Using current value.'
           endif
        endif

        if (check_dataqual(dataqual,question_data)) then
           if (warn_question .or. .not. cont_question) then
              write(unit_error,613) 'Warning: questionable data: ', &
                  trim(pathinput(ptr)%path), &
                  current_date
           endif
           if (.not. cont_question) then
              write(unit_error,*) 'Fatal error.'
              call exit(2)
           else if (warn_question) then
              write(unit_error,*) 'Using current value.'
           endif
        endif

        !----use this value for all time steps?
        if (pathinput(ptr)%fillin == fill_first) then
            pathinput(ptr)%constant_value=pathinput(ptr)%value
        endif
      
        return
    end subroutine

end module
