!!<license>
!!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!!    Department of Water Resources.
!!    This file is part of DSM2.

!!    The Delta Simulation Model 2 (DSM2) is free software:
!!    you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.

!!    DSM2 is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.

!!    You should have received a copy of the GNU General Public License
!!    along with DSM2.  If not, see <http://www.gnu.org/licenses>.
!!</license>
subroutine store_outpaths_gen( &
    outpaths_dim, &
    block_dim, &
    npaths, &
    outpath_ptr, &
    jul_start, &
    store_ndx, &
    jmin_eop, &
    jmin_eop_prev, &
    nave_intvl, &
    outdata_arr, &
    unit, &
    lflush, &
    lupdate, &
    need_tmpfile &
    )

!-----General store outpaths.  This fills output buffer arrays and
!-----writes temp file.
    use runtime_data
    use iopath_data
    use mod_writedss
    use utilities, only: jmin2cdt
    implicit none

!-----subroutine arguments

    integer &
        outpaths_dim, &        ! output paths array dimension &
        block_dim           ! data block array dimension

    integer &
        npaths, &              ! number of output paths for this interval &
        outpath_ptr(outpaths_dim), & ! pointer array to output pathnames &
        store_ndx, &          ! end array index for output buffers (data blocks) &
        nave_intvl(outpaths_dim), & ! number of values in the interval average &
        unit                ! write unit number

    integer*4 &
        jul_start, &           ! julian minute of start of data for this path &
        jmin_eop, &           ! julian minute of end-of-period for this interval &
        jmin_eop_prev       ! previous value of jmin_eop

    REAL*8 &
        outdata_arr(0:block_dim, outpaths_dim) ! output data array

    logical &
        lflush, &              ! true to force data flush to scratch files &
        lupdate, &            ! true to update value arrays &
        need_tmpfile        ! true if tmp file is needed for this group

!-----local variables

    logical &
        lnewndx, &             ! this julmin first one in time interval &
        lendndx             ! this julmin last one in time interval

    integer &
        i, j, &                 ! array indices &
        ptr, &                ! array pointer &
        nvals               ! number of values to write to disk

    REAL*8 &
        value               ! output value &
    ! , get_output          ! function to get the output value for each DSM2 module
    REAL*8, external :: get_output

    character &
        ctmp*14             ! temp string

!-----data will be stored at end-of-period; for example, for 1HOUR:
!-----from 05JUN1994 0101 to 05JUN1994 0200 (inclusive) all pertains
!-----to the 05JUN1994 0200 time block.

    lnewndx = .false.
    if (julmin .ne. start_julmin .and. &
        (julmin - jmin_eop_prev .le. time_step)) then
!--------current model time just crossed a data time interval
        lnewndx = .true.
        store_ndx = store_ndx + 1
    end if

    lendndx = .false.
    if (julmin + time_step .gt. jmin_eop) lendndx = .true.

    if (.not. lupdate) goto 100

!-----put value into output buffer
    do i = 1, npaths
        ptr = outpath_ptr(i)
        value = get_output(ptr)  ! get the desired output variable for each DSM2 module
        if (pathoutput(ptr)%per_type .eq. per_type_per_aver .or. &
            pathoutput(ptr)%per_type .eq. per_type_per_cum) then ! period average
            outdata_arr(store_ndx, i) = outdata_arr(store_ndx, i) &
                                        *float(nave_intvl(i)) + value
            if (lnewndx) nave_intvl(i) = 0
            nave_intvl(i) = nave_intvl(i) + 1
            outdata_arr(store_ndx, i) = outdata_arr(store_ndx, i) &
                                        /float(nave_intvl(i))
        else if (pathoutput(ptr)%per_type .eq. per_type_per_min) then ! period minimum
            outdata_arr(store_ndx, i) = min(outdata_arr(store_ndx, i), value)
        else if (pathoutput(ptr)%per_type .eq. per_type_per_max) then ! period maximum
            outdata_arr(store_ndx, i) = max(outdata_arr(store_ndx, i), value)
        else if (pathoutput(ptr)%per_type .eq. per_type_inst_val) then ! instantaneous, no averaging
            if (julmin .eq. jmin_eop) then ! at end of DSS interval, no interpolation needed
                outdata_arr(store_ndx, i) = value
            else if (lnewndx .and. &
                     julmin .gt. prev_julmin .and. & ! skip recycled julmin &
                     prev_julmin .ne. jmin_eop_prev) then ! just crossed interval, interpolate between time steps
                outdata_arr(store_ndx - 1, i) = ((value - outdata_arr(0, i))* &
                                                 float(jmin_eop - (jmin_eop - jmin_eop_prev) - prev_julmin))/ &
                                                (julmin - prev_julmin) + outdata_arr(0, i)
            end if
        else if (pathoutput(ptr)%per_type .eq. per_type_inst_cum) then ! instantaneous cumulative value
            outdata_arr(store_ndx, i) = value
        end if
        outdata_arr(0, i) = value ! outdata_arr(0,X) stores value of previous time step
    end do

100 continue

!-----if flush request, or the output buffer will overflow and it's the
!-----end of a DSS time interval, then write to temporary file
    if (lflush .or. &
        (lendndx .and. store_ndx .eq. block_dim)) then
        if (.not. lendndx .or. &
            (julmin .eq. end_julmin)) then ! last value incomplete, don't write out
            nvals = store_ndx - 1
        else                   ! last value is complete
            nvals = store_ndx
        end if
        ctmp = jmin2cdt(jul_start) ! date/time of start of data block
        if (need_tmpfile) then
            write (unit) nvals   ! number of values written
            write (unit) ctmp
        end if
        do i = 1, npaths
            ptr = outpath_ptr(i)
            if (pathoutput(ptr)%need_tmp_outfile) then
                write (unit) ptr, (outdata_arr(j, i), j=1, nvals)
            else
                call writedss(ptr, ctmp, outdata_arr(1, i), nvals)
            end if

            if (.not. lendndx) then ! move incomplete value to start of array
                outdata_arr(1, i) = outdata_arr(store_ndx, i)
                store_ndx = 1
            else
                outdata_arr(1, i) = 0.0
                store_ndx = 0
            end if

            do j = 2, block_dim
                outdata_arr(j, i) = 0.0
            end do
        end do
!--------set julian minute of start of next data block
        jul_start = jul_start + (jmin_eop - jmin_eop_prev)*nvals
    end if

    return
end

subroutine store_outpaths(lflush)

!-----Write output data periodically to temporary files.
!-----Initialize the temporary files first with init_store_outpaths.
    use runtime_data
    use constants
    use dss
    use intervals
    use mod_writedss
    use utilities, only: incr_intvl
    implicit none

    logical &
        lflush, &              ! true to force data flush to scratch files &
        lupdate             ! true to update value arrays

    integer*4 &
        last_update          ! julian minute of last update

!-----the storage index pointer for each block of data
    integer &
        ndx_minutes15, &
        ndx_hours1, &
        ndx_days1, &
        ndx_weeks1, &
        ndx_months1, &
        ndx_years1

    save ndx_minutes15, ndx_hours1, ndx_days1, &
        ndx_weeks1, ndx_months1, ndx_years1, &
        last_update

    data ndx_minutes15/1/, ndx_hours1/1/, ndx_days1/1/, &
        ndx_weeks1/1/, ndx_months1/1/, ndx_years1/1/, &
        last_update/0/

    lupdate = .not. (julmin .eq. last_update)

    if (npthsout_min15 .gt. 0) then
        jmin_15min = incr_intvl(julmin, '15MIN', NEAREST_BOUNDARY)
        jmin_15min_prev = incr_intvl(julmin, '-15MIN', NEAREST_BOUNDARY)
        call store_outpaths_gen(max_out_min, &
                                mins15, npthsout_min15, ptout_min15, &
                                julstout_minutes15, ndx_minutes15, &
                                jmin_15min, jmin_15min_prev, nave_min15, &
                                dataout_minutes15, unit_min15, &
                                lflush, lupdate, need_tmpfile_min15)
    end if

    if (npthsout_hour1 .gt. 0) then
        jmin_1hour = incr_intvl(julmin, '1HOUR', NEAREST_BOUNDARY)
        jmin_1hour_prev = incr_intvl(julmin, '-1HOUR', NEAREST_BOUNDARY)
        call store_outpaths_gen(max_out_hour, &
                                hrs, npthsout_hour1, ptout_hour1, &
                                julstout_hours1, ndx_hours1, &
                                jmin_1hour, jmin_1hour_prev, nave_hour1, &
                                dataout_hours, unit_hour1, &
                                lflush, lupdate, need_tmpfile_hour1)
    end if

    if (npthsout_day1 .gt. 0) then
        jmin_1day = incr_intvl(julmin, '1DAY', NEAREST_BOUNDARY)
        jmin_1day_prev = incr_intvl(julmin, '-1DAY', NEAREST_BOUNDARY)
        call store_outpaths_gen(max_out_day, &
                                dys, npthsout_day1, ptout_day1, &
                                julstout_days1, ndx_days1, &
                                jmin_1day, jmin_1day_prev, nave_day1, &
                                dataout_days, unit_day1, &
                                lflush, lupdate, need_tmpfile_day1)
    end if

    if (npthsout_week1 .gt. 0) then
        jmin_1week = incr_intvl(julmin, '1WEEK', NEAREST_BOUNDARY)
        jmin_1week_prev = incr_intvl(julmin, '-1WEEK', NEAREST_BOUNDARY)
        call store_outpaths_gen(max_out_week, &
                                wks, npthsout_week1, ptout_week1, &
                                julstout_weeks1, ndx_weeks1, &
                                jmin_1week, jmin_1week_prev, nave_week1, &
                                dataout_weeks, unit_week1, &
                                lflush, lupdate, need_tmpfile_week1)
    end if

    if (npthsout_month1 .gt. 0) then
        jmin_1month = incr_intvl(julmin, '1MON', NEAREST_BOUNDARY)
        jmin_1month_prev = incr_intvl(julmin, '-1MON', NEAREST_BOUNDARY)
        call store_outpaths_gen(max_out_month, &
                                mths, npthsout_month1, ptout_month1, &
                                julstout_months1, ndx_months1, &
                                jmin_1month, jmin_1month_prev, nave_month1, &
                                dataout_months, unit_month1, &
                                lflush, lupdate, need_tmpfile_month1)
    end if

    if (npthsout_year1 .gt. 0) then
        jmin_1year = incr_intvl(julmin, '1YEAR', NEAREST_BOUNDARY)
        jmin_1year_prev = incr_intvl(julmin, '-1YEAR', NEAREST_BOUNDARY)
        call store_outpaths_gen(max_out_year, &
                                yrs, npthsout_year1, ptout_year1, &
                                julstout_years1, ndx_years1, &
                                jmin_1year, jmin_1year_prev, nave_year1, &
                                dataout_years, unit_year1, &
                                lflush, lupdate, need_tmpfile_year1)
    end if

    last_update = julmin

    return
end

