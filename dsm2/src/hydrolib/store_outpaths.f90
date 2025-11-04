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
module mod_store_outpaths
    implicit none

    abstract interface
        real*8 function get_output_ptr(ptr)
            integer :: ptr
        end function get_output_ptr
    end interface
    procedure(get_output_ptr), pointer :: get_output => null()

contains
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
    need_tmpfile)

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

      subroutine init_store_outpaths(istat, fn_get_output)

!-----Initialization for storing time-varying output data in temporary
!-----files.
          use IO_Units
          use iopath_data
          use runtime_data
          use constants
          use dss
          use mod_writedss
          use utilities, only: incr_intvl
          implicit none

          procedure(get_output_ptr), intent(in), pointer :: fn_get_output
!-----local variables
          character &
              tmp_dir*50, &          ! scratch file directory &
              dir_arr(10)*30, &     ! list of possible scratch directories &
              dsm2mod*20, &         ! dsm2modifier &
              tmp_file*80, &        ! scratch file name &
              ctmp*100            ! scratch variable

          integer &
              istat, &               ! file status &
              ptr, &                ! global pointer for pathouput &
              i, &                  ! loop index &
              npaths             ! path count &

          get_output => fn_get_output

!-----For each interval with output data, create a scratch file
!-----and write header info.

!-----scratch directory; if not specified in user input, try
!-----plausible locations
          dir_arr(1) = temp_dir
          dir_arr(2) = '/tmp'
          dir_arr(4) = 'c:'//backslash//'temp'
          dir_arr(5) = 'c:'//backslash//'tmp'
          dir_arr(6) = 'd:'//backslash//'temp'
          dir_arr(7) = 'd:'//backslash//'tmp'
          dir_arr(8) = '.'
          dir_arr(9) = miss_val_c     ! array list must end with this
          ptr = getdir(dir_arr)
          if (ptr .ne. 0) then
              tmp_dir = dir_arr(ptr)
          else
605           format(/a, '(/a)')
              write (unit_error, 605) 'Could not find a valid directory in this list:', &
                  (dir_arr(ptr), ptr=1, 8)
              goto 901
          end if

          ctmp = trim(crid)//'.bin'

          julstout_minutes15 = incr_intvl(start_julmin, '15min', &
                                          NEAREST_BOUNDARY)
          if (need_tmpfile_min15) then
              call mkfilename(tmp_dir, 'tmp_min15-'//trim(ctmp), tmp_file)
              scratch_file_array(1) = tmp_file
              open ( &
                  unit=unit_min15, &
                  file=tmp_file, &
                  form='unformatted', &
                  buffered='yes', &
                  buffercount=50, &
                  iostat=istat, &
                  err=901 &
                  )
!--------count number of paths to write
              npaths = 0
              do i = 1, npthsout_min15
                  ptr = ptout_min15(i)
                  if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
              end do
              write (unit=unit_min15) npaths

              do i = 1, npthsout_min15
                  ptr = ptout_min15(i)
!-----------only write those paths that need tmp file output
                  if (pathoutput(ptr)%need_tmp_outfile) &
                      write (unit=unit_min15) ptr, pathoutput(ptr), &
                      per_type_names(pathoutput(ptr)%per_type)
              end do
          else
              scratch_file_array(1) = miss_val_c
          end if

          julstout_hours1 = incr_intvl(start_julmin, '1hour', &
                                       NEAREST_BOUNDARY)
          if (need_tmpfile_hour1) then
              call mkfilename(tmp_dir, 'tmp_hour1-'//trim(ctmp), tmp_file)
              scratch_file_array(2) = tmp_file
              open ( &
                  unit=unit_hour1, &
                  file=tmp_file, &
                  form='unformatted', &
                  buffered='yes', &
                  buffercount=50, &
                  iostat=istat, &
                  err=901 &
                  )
!--------count number of paths to write
              npaths = 0
              do i = 1, npthsout_hour1
                  ptr = ptout_hour1(i)
                  if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
              end do
              write (unit=unit_hour1) npaths

              do i = 1, npthsout_hour1
                  ptr = ptout_hour1(i)
!-----------only write those paths that need tmp file output
                  if (pathoutput(ptr)%need_tmp_outfile) &
                      write (unit=unit_hour1) ptr, pathoutput(ptr), &
                      per_type_names(pathoutput(ptr)%per_type)
              end do
          else
              scratch_file_array(2) = miss_val_c
          end if

          julstout_days1 = incr_intvl(start_julmin, '1day', &
                                      NEAREST_BOUNDARY)
          if (need_tmpfile_day1) then
              call mkfilename(tmp_dir, 'tmp_day1-'//trim(ctmp), tmp_file)
              scratch_file_array(3) = tmp_file
              open ( &
                  unit=unit_day1, &
                  file=tmp_file, &
                  form='unformatted', &
                  buffered='yes', &
                  buffercount=10, &
                  iostat=istat, &
                  err=901 &
                  )
!--------count number of paths to write
              npaths = 0
              do i = 1, npthsout_day1
                  ptr = ptout_day1(i)
                  if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
              end do
              write (unit=unit_day1) npaths

              do i = 1, npthsout_day1
                  ptr = ptout_day1(i)
!-----------only write those paths that need tmp file output
                  if (pathoutput(ptr)%need_tmp_outfile) &
                      write (unit=unit_day1) ptr, pathoutput(ptr), &
                      per_type_names(pathoutput(ptr)%per_type)
              end do
          else
              scratch_file_array(3) = miss_val_c
          end if

          julstout_weeks1 = incr_intvl(start_julmin, '1week', &
                                       NEAREST_BOUNDARY)
          if (need_tmpfile_week1) then
              call mkfilename(tmp_dir, 'tmp_week1-'//trim(ctmp), tmp_file)
              scratch_file_array(4) = tmp_file
              open ( &
                  unit=unit_week1, &
                  file=tmp_file, &
                  form='unformatted', &
                  iostat=istat, &
                  err=901 &
                  )
!--------count number of paths to write
              npaths = 0
              do i = 1, npthsout_week1
                  ptr = ptout_week1(i)
                  if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
              end do
              write (unit=unit_week1) npaths

              do i = 1, npthsout_week1
                  ptr = ptout_week1(i)
!-----------only write those paths that need tmp file output
                  if (pathoutput(ptr)%need_tmp_outfile) &
                      write (unit=unit_week1) ptr, pathoutput(ptr), &
                      per_type_names(pathoutput(ptr)%per_type)
              end do
          else
              scratch_file_array(4) = miss_val_c
          end if

          julstout_months1 = incr_intvl(start_julmin, '1month', &
                                        NEAREST_BOUNDARY)
          if (need_tmpfile_month1) then
              call mkfilename(tmp_dir, 'tmp_month1-'//trim(ctmp), tmp_file)
              scratch_file_array(5) = tmp_file
              open ( &
                  unit=unit_month1, &
                  file=tmp_file, &
                  form='unformatted', &
                  iostat=istat, &
                  err=901 &
                  )
!--------count number of paths to write
              npaths = 0
              do i = 1, npthsout_month1
                  ptr = ptout_month1(i)
                  if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
              end do
              write (unit=unit_month1) npaths

              do i = 1, npthsout_month1
                  ptr = ptout_month1(i)
!-----------only write those paths that need tmp file output
                  if (pathoutput(ptr)%need_tmp_outfile) &
                      write (unit=unit_month1) ptr, pathoutput(ptr), &
                      per_type_names(pathoutput(ptr)%per_type)
              end do
          else
              scratch_file_array(5) = miss_val_c
          end if

          julstout_years1 = incr_intvl(start_julmin, '1year', &
                                       NEAREST_BOUNDARY)
          if (need_tmpfile_year1) then
              call mkfilename(tmp_dir, 'tmp_year1-'//trim(ctmp), tmp_file)
              scratch_file_array(6) = tmp_file
              open ( &
                  unit=unit_year1, &
                  file=tmp_file, &
                  form='unformatted', &
                  iostat=istat, &
                  err=901 &
                  )
!--------count number of paths to write
              npaths = 0
              do i = 1, npthsout_year1
                  ptr = ptout_year1(i)
                  if (pathoutput(ptr)%need_tmp_outfile) npaths = npaths + 1
              end do
              write (unit=unit_year1) npaths

              do i = 1, npthsout_year1
                  ptr = ptout_year1(i)
!-----------only write those paths that need tmp file output
                  if (pathoutput(ptr)%need_tmp_outfile) &
                      write (unit=unit_year1) ptr, pathoutput(ptr), &
                      per_type_names(pathoutput(ptr)%per_type)
              end do
          else
              scratch_file_array(6) = miss_val_c
          end if

          return

901       continue                  ! scratch file open error
          write (unit_error, "('Error opening binary scratch file',1x,a)") tmp_file
          call exit(2)

          return
      end

      integer function getdir(dir_arr)

!-----Find a usable directory from the given list; return the
!-----array index of the one to use.  The list must end with
!-----miss_val_c.
          use constants
          implicit none

!-----argument
          character*(*) dir_arr(*)  ! list of directory names to try

!-----local variables
          integer &
              ndx, &                 ! directory array index &
              nlen, &               ! character length &
              statarr(13), &        ! file status array &
              stat, &               ! file status intrinsic function &
              istat, &              ! file status value &
              lnblnk               ! intrinsic function

          ndx = 1
          do while (dir_arr(ndx) .ne. miss_val_c)
              nlen = lnblnk(dir_arr(ndx))
              if (nlen .eq. 0) goto 100
              istat = stat(dir_arr(ndx), statarr)
              if (istat .eq. 0) then ! this directory name ok
                  getdir = ndx
                  return
              end if
100           continue
              ndx = ndx + 1
          end do

          getdir = 0
          return
      end

      subroutine mkfilename( &
          dir, &
          file, &
          dirfile &
          )

!-----Make a full filename (directory + filename) from directory name
!-----and filename.
          use constants
          implicit none

!-----arguments
          character*(*) &
              dir, &                 ! directory name &
              file, &               ! filename &
              dirfile             ! directory+filename

!-----local variables
          integer &
              nlen, &                ! length of character string &
              ndx, &                ! array index &
              lnblnk, &             ! intrinsic &
              index               ! intrinsic

          character &
              dirchar              ! directory delimiter (/ or \)

          nlen = lnblnk(dir)
!-----try to find / or \ in directory name
          ndx = index(dir, '/')
          if (ndx .gt. 0) then      ! unix
              dirchar = dir(ndx:ndx)
          else
              ndx = index(dir, backslash)
              if (ndx .gt. 0) then   ! pc
                  dirchar = dir(ndx:ndx)
              else                   ! unknown
                  dirchar = '/'
              end if
          end if
!-----directory name must end in either / or \ before
!-----appending filename
          if (dir(nlen:nlen) .ne. dirchar) then
              nlen = nlen + 1
              dir(nlen:nlen) = dirchar
          end if

          dirfile = dir(:nlen)//file

          return
      end

    real*8 function get_output_hydro(ptr) result(fn_result)

    !-----Get the desired output variable from the particular DSM module
    use gates_data, only: gateArray
    use IO_Units
    use grid_data
    use iopath_data
    use constants
    use chconnec
    use netbnd, only: reservoir_source_sink
    use channel_schematic, only: StreamEndNode
    use chstatus, only: &
        GlobalStreamFlow, &  ! Hydro function to return flow &
        GlobalStreamSurfaceElevation! Hydro function to return stage &
    use tidefile, only: ChannelVelocity     ! Hydro function to return velocity
    implicit none

    !-----arguments

    integer &
        ptr                  ! output path pointer

    !-----global variables

    !-----local variables

    integer &
        intchan, &            ! internal channel numbers &
        nodeup, nodedown, &    ! Hydro upstream and downstream 'node' number &
        hydrores, &          ! Hydro reservoir number
        ngpoints, &
        i                   ! loop index

    integer node1, node2    !up and down global comp. node
    real*8 &
        val_x, &              ! interpolated value statement function &
        val_up, val_down, &    ! value at upstream and downstream end of chan &
        reach_dist, &          ! distance in a reach (not channel) &
        reach_len, &          ! reach length &
        Q_interp, &         !interpolated discharge &
        Z_interp            !interpolated stage
    !-----statement function to interpolate value along channel
    val_x(val_up, val_down, reach_dist, reach_len) = val_up - (val_up &
                                                               - val_down)*(reach_dist/reach_len)

    if (pathoutput(ptr)%obj_type == obj_channel) then ! output is from channel

        intchan = pathoutput(ptr)%obj_no
        nodedown = -StreamEndNode(-intchan)
        nodeup = StreamEndNode(intchan)
        ngpoints = nodedown - nodeup + 1
        if (ngpoints < 2) then
            write (unit_error, 901) chan_geom(intchan)%chan_no, ngpoints
901         format(' Error in output specification in channel:', i6/ &
                   ' Number of grid points=', i6)
            call exit(2)
        end if
        !         closest_node=int(dfloat(nodeup)+dfloat(pathoutput(ptr)%chan_dist)/
        !     &        dfloat(chan_geom(intchan)%length)*(dfloat(nodedown)-
        !     &        dfloat(nodeup))+0.5)
        node1 = int(dfloat(nodeup) + dfloat(pathoutput(ptr)%chan_dist)/ &
                    dfloat(chan_geom(intchan)%length)*(dfloat(nodedown) - &
                                                       dfloat(nodeup)))
        if (node1 == nodedown) node1 = node1 - 1
        node2 = node1 + 1
        reach_len = dfloat(chan_geom(intchan)%length)/(dfloat(nodedown) - &
                                                       dfloat(nodeup))
        reach_dist = dfloat(pathoutput(ptr)%chan_dist) - reach_len*(node1 - nodeup)

        if (node1 < nodeup .or. node2 > nodedown .or. &
            pathoutput(ptr)%chan_dist > chan_geom(intchan)%length) then
            write (unit_error, 902) chan_geom(intchan)%chan_no, &
                pathoutput(ptr)%chan_dist, &
                chan_geom(intchan)%length
902         format('Error in output specification for channel=', i6/ &
                   'output specified for distance=', i10/ &
                   'channel length=', i10)
            call exit(2)
        end if
        if (pathoutput(ptr)%meas_type == 'stage') then
            fn_result = val_x( &
                         globalStreamSurfaceElevation(node1), &
                         globalStreamSurfaceElevation(node2), &
                         reach_dist, &
                         reach_len)
        else if (pathoutput(ptr)%meas_type(1:3) == 'vel') then
            fn_result = ChannelVelocity(intchan, dfloat(pathoutput(ptr)%chan_dist))
        else if (pathoutput(ptr)%meas_type == 'flow') then
            fn_result = val_x( &
                         globalStreamFlow(node1), &
                         globalStreamFlow(node2), &
                         reach_dist, &
                         reach_len)
        end if
        !else if (pathoutput(ptr)%obj_type .eq. obj_qext) then
        !   fn_result=qext(pathoutput(ptr)%obj_no)
    else if (pathoutput(ptr)%obj_type == obj_reservoir) then ! output is from reservoir
        hydrores = pathoutput(ptr)%obj_no
        nodeup = pathoutput(ptr)%res_node_no
        if (nodeup > 0) then ! flow to node
            do i = 1, res_geom(hydrores)%nnodes
                if (res_geom(hydrores)%node_no(i) == nodeup) then
                    fn_result = -qres(hydrores, i)
                end if
            end do
            !fn_result=-qres(hydrores,nodeup) ! + qres: flow from res to chan
        else if (pathoutput(ptr)%meas_type == 'stage') then ! stage of reservoir
            fn_result = yres(hydrores)
        else if (pathoutput(ptr)%meas_type == 'flow-net') then ! net flow in/out of reservoir
            fn_result = reservoir_source_sink(pathoutput(ptr)%obj_no, ALL_FLOWS)
            do i = 1, res_geom(hydrores)%nnodes
                fn_result = fn_result - qres(hydrores, i)
            end do
        else if (pathoutput(ptr)%meas_type == 'flow-source') then ! net source in/out of reservoir
            fn_result = reservoir_source_sink(pathoutput(ptr)%obj_no, QEXT_FLOWS)
        else if (pathoutput(ptr)%meas_type == 'pump') then ! net pumping out of reservoir
            fn_result = reservoir_source_sink(pathoutput(ptr)%obj_no, ALL_FLOWS)
        end if

    else if (pathoutput(ptr)%obj_type == obj_gate) then
        if (pathoutput(ptr)%meas_type == 'pos') then
            ! //@todo: 'pos' is deprecated
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%opCoefToNode
        else if (pathoutput(ptr)%meas_type == 'flow') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%flow
        else if (pathoutput(ptr)%meas_type == 'device-flow') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%flow
        else if (pathoutput(ptr)%meas_type == 'op-to-node' .or. &
                 pathoutput(ptr)%meas_type == 'op_to_node') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%opCoefToNode
        else if (pathoutput(ptr)%meas_type == 'op-from-node' .or. &
                 pathoutput(ptr)%meas_type == 'op_from_node') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%opCoefFromNode
        else if (pathoutput(ptr)%meas_type == 'position') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%position
        else if (pathoutput(ptr)%meas_type == 'install') then
            fn_result = 1.0
            if (gateArray(pathoutput(ptr)%obj_no)%free) fn_result = 0.
        else if (pathoutput(ptr)%meas_type == 'height') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%height
        else if (pathoutput(ptr)%meas_type == 'elev') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%baseElev
        else if (pathoutput(ptr)%meas_type == 'width') then
            fn_result = gateArray(pathoutput(ptr)%obj_no)%Devices( &
                         pathoutput(ptr)%gate_device)%maxWidth
        else
            fn_result = miss_val_r
        end if
    end if

    return
end

end module mod_store_outpaths