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
!> This is to store output data periodically to temporary files.
!>@ingroup process_io
module gtm_store_outpath

    use gtm_precision
    use gtm_dss_write

    contains
    
    !> Write output data periodically to temporary files.
    !> Initialize the temporary files first with init_store_outpaths.
    !> This routine does not allow outputs mixed with different output time intervals.
    subroutine gtm_store_outpaths(lflush,          &
                                  runtime_julmin,  &
                                  runtime_step,    &
                                  vals)
                                  
        use dsm2_time_utils, only: incr_intvl
        use common_dsm2_vars, only: NEAREST_BOUNDARY, noutpaths
        
        implicit none
        
        logical, intent(in) :: lflush                 !< true to force data flush to scratch files
        integer, intent(in) :: runtime_julmin         !< runtime julmin minute
        integer, intent(in) :: runtime_step           !< runtime step
        real(gtm_real), intent(in) :: vals(noutpaths) !< values for each output path
        logical :: lupdate                            ! true to update value arrays
        integer :: last_update                        ! julian minute of last update
        
        !-----the storage index pointer for each block of data
        integer :: ndx_minutes15, &
                   ndx_hours1,    &
                   ndx_days1,     &
                   ndx_weeks1,    &
                   ndx_months1,   &
                   ndx_years1

        save ndx_minutes15, ndx_hours1, ndx_days1, &
             ndx_weeks1, ndx_months1, ndx_years1,  &
             last_update

        data  ndx_minutes15 /1/, ndx_hours1 /1/, ndx_days1 /1/, &
              ndx_weeks1 /1/, ndx_months1 /1/, ndx_years1 /1/,  &
              last_update /0/
              
        integer :: jmin_15min, jmin_15min_prev,   &
                   jmin_1hour, jmin_1hour_prev,   &
                   jmin_1day, jmin_1day_prev,     &
                   jmin_1week, jmin_1week_prev,   &
                   jmin_1month, jmin_1month_prev, &
                   jmin_1year, jmin_1year_prev
   
        lupdate = .not. (runtime_julmin .eq. last_update)

        if (npthsout_min15 .gt. 0) then
            call incr_intvl(jmin_15min,runtime_julmin,'15MIN',NEAREST_BOUNDARY)
            call incr_intvl(jmin_15min_prev,runtime_julmin,'-15MIN',NEAREST_BOUNDARY)
            call gtm_store_outpaths_gen(max_out_min,    &
                 mins15,npthsout_min15,ptout_min15,     &
                 julstout_minutes15,ndx_minutes15,      &
                 jmin_15min,jmin_15min_prev,nave_min15, &
                 dataout_minutes15,unit_min15,          &
                 lflush,lupdate,need_tmpfile_min15,     &
                 runtime_julmin,runtime_step,vals)
        endif

        if (npthsout_hour1 .gt. 0) then
            call incr_intvl(jmin_1hour,runtime_julmin,'1HOUR',NEAREST_BOUNDARY)
            call incr_intvl(jmin_1hour_prev,runtime_julmin,'-1HOUR',NEAREST_BOUNDARY)
            call gtm_store_outpaths_gen(max_out_hour,   &
                 hrs,npthsout_hour1,ptout_hour1,        &
                 julstout_hours1,ndx_hours1,            &
                 jmin_1hour,jmin_1hour_prev,nave_hour1, &
                 dataout_hours,unit_hour1,              &
                 lflush,lupdate,need_tmpfile_hour1,     &
                 runtime_julmin,runtime_step,vals)
        endif

        if (npthsout_day1 .gt. 0) then
            call incr_intvl(jmin_1day,runtime_julmin,'1DAY',NEAREST_BOUNDARY)
            call incr_intvl(jmin_1day_prev,runtime_julmin,'-1DAY',NEAREST_BOUNDARY)
            call gtm_store_outpaths_gen(max_out_day,    &
                 dys,npthsout_day1,ptout_day1,          &
                 julstout_days1,ndx_days1,              &
                 jmin_1day,jmin_1day_prev,nave_day1,    &
                 dataout_days,unit_day1,                &
                 lflush,lupdate,need_tmpfile_day1,      &
                 runtime_julmin,runtime_step,vals)
        endif

        if (npthsout_week1 .gt. 0) then
            call incr_intvl(jmin_1week,runtime_julmin,'1WEEK',NEAREST_BOUNDARY)
            call incr_intvl(jmin_1week_prev,runtime_julmin,'-1WEEK',NEAREST_BOUNDARY)
            call gtm_store_outpaths_gen(max_out_week,   &
                 wks,npthsout_week1,ptout_week1,        &
                 julstout_weeks1,ndx_weeks1,            &
                 jmin_1week,jmin_1week_prev,nave_week1, &
                 dataout_weeks,unit_week1,              &
                 lflush,lupdate,need_tmpfile_week1,     &
                 runtime_julmin,runtime_step,vals)
        endif

        if (npthsout_month1 .gt. 0) then
            call incr_intvl(jmin_1month,runtime_julmin,'1MON',NEAREST_BOUNDARY)
            call incr_intvl(jmin_1month_prev,runtime_julmin,'-1MON',NEAREST_BOUNDARY)
            call gtm_store_outpaths_gen(max_out_month,     &
                 mths,npthsout_month1,ptout_month1,        &
                 julstout_months1,ndx_months1,             &
                 jmin_1month,jmin_1month_prev,nave_month1, &
                 dataout_months,unit_month1,               &
                 lflush,lupdate,need_tmpfile_month1,       &
                 runtime_julmin,runtime_step,vals)
        endif

        if (npthsout_year1 .gt. 0) then
            call incr_intvl(jmin_1year,runtime_julmin,'1YEAR',NEAREST_BOUNDARY)
            call incr_intvl(jmin_1year_prev,runtime_julmin,'-1YEAR',NEAREST_BOUNDARY)
            call gtm_store_outpaths_gen(max_out_year,      &
                 yrs,npthsout_year1,ptout_year1,           &
                 julstout_years1,ndx_years1,               &
                 jmin_1year,jmin_1year_prev,nave_year1,    &
                 dataout_years,unit_year1,                 &
                 lflush,lupdate,need_tmpfile_year1,        &
                 runtime_julmin,runtime_step,vals)
        endif
 
        last_update = runtime_julmin

        return
    end subroutine


    !> General store outpaths. This fills output buffer arrays and
    !> writes temp file.
    subroutine gtm_store_outpaths_gen(outpaths_dim,   &
                                      block_dim,      &
                                      npaths,         &
                                      outpath_ptr,    &
                                      jul_start,      &
                                      store_ndx,      &
                                      jmin_eop,       &
                                      jmin_eop_prev,  &
                                      nave_intvl,     &
                                      outdata_arr,    &
                                      unit,           &
                                      lflush,         &
                                      lupdate,        &
                                      need_tmpfile,   &
                                      runtime_julmin, &
                                      runtime_step,   &
                                      vals)

        use common_dsm2_vars, only: noutpaths, pathoutput, prev_julmin,      &
                                    start_julmin, end_julmin,             &
                                    per_type_per_aver, per_type_per_cum,  &
                                    per_type_per_min, per_type_per_max,   &
                                    per_type_inst_val, per_type_inst_cum, &
                                    per_type_null
 
        use time_utilities, only: jmin2cdt
 
        implicit none

        !-----subroutine arguments

        integer, intent(in) :: outpaths_dim                !< output paths array dimension
        integer, intent(in) :: block_dim                   !< data block array dimension
        integer, intent(in) :: npaths                      !< number of output paths for this interval
        integer, intent(in) :: outpath_ptr(outpaths_dim)   !< pointer array to output pathnames
        integer, intent(inout) :: store_ndx                !< end array index for output buffers (data blocks)
        integer, intent(inout) :: nave_intvl(outpaths_dim) !< number of values in the interval average
        integer, intent(inout) :: jul_start                !< julian minute of start of data for this path         
        integer, intent(in) :: unit                        !< write unit number     
        integer, intent(in) :: jmin_eop                    !< julian minute of end-of-period for this interval 
        integer, intent(in) :: jmin_eop_prev               !< previous value of jmin_eop                 
        integer, intent(in) :: runtime_julmin              !< runtime julimn
        integer, intent(in) :: runtime_step                !< runtime step        
        real(gtm_real), intent(in) :: vals(noutpaths)      !< runtime values
        logical, intent(in) :: lflush                      !< true to force data flush to scratch files 
        logical, intent(in) :: lupdate                     !< true to update value arrays 
        logical, intent(in) :: need_tmpfile                !< true if tmp file is needed for this group                
        real(gtm_real), intent(out) :: outdata_arr(0:block_dim,outpaths_dim) !< output data array

        !-----local variables
        logical :: lnewndx,         &   ! this julmin first one in time interval 
                   lendndx              ! this julmin last one in time interval
        integer :: i,j,             &   ! array indices 
                   ptr,             &   ! array pointer 
                   nvals                ! number of values to write to disk
        real(gtm_real) :: val,      &   ! output value 
                          get_output    ! function to get the output value for each DSM2 module
        character(len=14) :: ctmp       ! temp string
      
        !-----data will be stored at end-of-period; for example, for 1HOUR:
        !-----from 05JUN1994 0101 to 05JUN1994 0200 (inclusive) all pertains
        !-----to the 05JUN1994 0200 time block.
        lnewndx = .false.
        if (runtime_julmin .ne. start_julmin .and. &
            (runtime_julmin-jmin_eop_prev .le. runtime_step)) then   ! current model time just crossed a data time interval
            lnewndx = .true.
            store_ndx = store_ndx + 1
        endif

        lendndx = .false.
        if (runtime_julmin+runtime_step .gt. jmin_eop) lendndx = .true.

        if (.not. lupdate) goto 100

        !-----put value into output buffer
        do i = 1, npaths
            ptr = outpath_ptr(i)    
            val = vals(ptr)
            if (pathoutput(ptr)%per_type .eq. per_type_per_aver .or. &
                pathoutput(ptr)%per_type .eq. per_type_per_cum) then ! period average
                outdata_arr(store_ndx,i) = outdata_arr(store_ndx,i)*dble(nave_intvl(i)) + val
                if (lnewndx) nave_intvl(i) = 0
                nave_intvl(i) = nave_intvl(i) + 1
                outdata_arr(store_ndx,i) = outdata_arr(store_ndx,i)/dble(nave_intvl(i))
            else if (pathoutput(ptr)%per_type .eq. per_type_per_min) then ! period minimum
                outdata_arr(store_ndx,i)=min(outdata_arr(store_ndx,i),val)
            else if (pathoutput(ptr)%per_type .eq. per_type_per_max) then ! period maximum
                outdata_arr(store_ndx,i)=max(outdata_arr(store_ndx,i),val)
            else if (pathoutput(ptr)%per_type .eq. per_type_inst_val) then ! instantaneous, no averaging
                if (runtime_julmin .eq. jmin_eop) then ! at end of DSS interval, no interpolation needed
                    outdata_arr(store_ndx,i) = val
                else if (lnewndx .and. &
                         runtime_julmin .gt. prev_julmin .and. & ! skip recycled julmin &
                         prev_julmin .ne. jmin_eop_prev) then    ! just crossed interval, interpolate between time steps
                    outdata_arr(store_ndx-1,i) = ( (val-outdata_arr(0,i)) * &
                       dble(jmin_eop-(jmin_eop-jmin_eop_prev)-prev_julmin)) / &
                       (runtime_julmin-prev_julmin) + outdata_arr(0,i)
                endif
            else if (pathoutput(ptr)%per_type .eq. per_type_inst_cum) then ! instantaneous cumulative value
                outdata_arr(store_ndx,i) = val
            endif
            outdata_arr(0,i) = val ! outdata_arr(0,X) stores value of previous time step
        enddo
 100    continue

        !-----if flush request, or the output buffer will overflow and it's the
        !-----end of a DSS time interval, then write to temporary file
        if (lflush .or. (lendndx .and. store_ndx .eq. block_dim)) then
            if (.not. lendndx .or. &
                (runtime_julmin .eq. end_julmin)) then ! last value incomplete, don't write out
                nvals = store_ndx - 1
            else                   ! last value is complete
                nvals = store_ndx
            endif
            ctmp = jmin2cdt(int(jul_start)) ! date/time of start of data block
            if (need_tmpfile) then
                write(unit) nvals   ! number of values written
                write(unit) ctmp
            endif
            do i=1,npaths
                ptr = outpath_ptr(i)
                if (pathoutput(ptr)%need_tmp_outfile) then
                    write(unit) ptr,(outdata_arr(j,i), j=1,nvals)
                else
                    call writedss(ptr, ctmp, outdata_arr(1,i), nvals)
                endif
                if (.not. lendndx) then ! move incomplete value to start of array
                    outdata_arr(1,i)=outdata_arr(store_ndx,i)
                    store_ndx = 1
                else
                    outdata_arr(1,i) = zero
                    store_ndx=0
                endif
                do j=2,block_dim
                    outdata_arr(j,i) = zero
                enddo
            enddo
            !--------set julian minute of start of next data block
            jul_start = jul_start+(jmin_eop-jmin_eop_prev)*nvals
        endif
        return
    end subroutine

end module