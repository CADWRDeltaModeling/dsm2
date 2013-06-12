      
      
module hdf_util_qual      
    use hdf5, only: h5open_f, h5close_f

    contains

    subroutine testapi
!---- hdf5 api on
      call h5open_f(ierror)
      call verify_error(ierror,"Program error -- opening hdf5 API")
      call buffer_input_tidefile()      ! process tidefile name(s)
!----- load header information from the first hydro tidefile
!      this assures that names of qext and stage boundaries are available
      call read_tide_head(tide_files(1).filename, .false.)
      prev_julmin=0
      julmin=start_julmin
      current_date=jmin2cdt(julmin)
         call InitHDF5MemoryDims()
!--------start time loop for checking boundary data

         prev_julmin=julmin
         julmin=julmin+time_step
         current_date=jmin2cdt(julmin)

         do while (julmin .le. end_julmin)

            if (julmin .ge. next_display) then
 611           format('Starting data-check for time: ',a)
               write(unit_screen,611) current_date
               next_display=incr_intvl(next_display,display_intvl,  &
                   TO_BOUNDARY)
            endif

            call read_boundary_values
!-----------Read the Hydro tidefile
            call read_mult_tide

            prev_julmin=julmin
            julmin=julmin+time_step
            current_date=jmin2cdt(julmin)
         enddo 
      end subroutine
      
      subroutine buffer_input_tidefile()      
      use input_storage_fortran
      use constants
      use io_units
      use runtime_data
      use common_tide
      implicit none
      integer :: nitem
      integer :: icount
      character*128 iofile
      integer :: ierror = 0

      character*(16) :: sdate,edate
      integer*4, external :: obj_type_code
      integer :: i
      integer :: n_tidefiles_used

      nitem = tidefile_buffer_size()
      if (nitem .eq. 0)then
          write(unit_error,*)"No input tidefiles listed."
          call exit(-3)
      end if
      do icount = 1,nitem
         call tidefile_query_from_buffer(icount,sdate,edate,iofile,ierror)
         call process_tidefile(sdate,edate,iofile)
      end do
      print *,"Number of tidefiles: ", nitem

      ! Check the numbering and order of tidefiles
      n_tidefiles_used = 0
      if (nintides .le. 0) then
         write(unit_error, '(a)') 'No input tides given, run stopped.'
         call exit(-3)
      endif
      do i=1,nintides
	   n_tidefiles_used = n_tidefiles_used + 1
	   ! This exit statement allows nonexistent tidefiles to be listed
	   if (tide_files(i).end_julmin .ge. end_julmin) exit  
      enddo
      nintides = n_tidefiles_used
	if (nintides .gt. 1) then 
        do i=2,nintides
           if (tide_files(i).start_julmin .ne. tide_files(i-1).end_julmin) then
	        write(unit_error,*) "Tidefile dates must be ordered in time, "    &
                   // "with no gaps or overlap in start/end dates"
	        call exit(-3)
	     end if
	  end do
	end if

!-----make sure run dates are spanned
      if (dsm2_module .eq. qual .or. dsm2_module .eq. ptm) then
	  if (  tide_files(1).start_julmin .gt. start_julmin   &
        .or. tide_files(nintides).end_julmin .lt. end_julmin) then
	    write(unit_error,*)"Specified dates for tidefiles do not cover period of simulation"
	    call exit(-3)
	  end if
	end if
	! todo: eli
	tide_files(1).start_julmin = start_julmin    
      return
      end subroutine
      
      subroutine read_mult_tide

!-----Read multiple Hydro tidefiles.  Each tidefile can have multiple
!-----tides.  A tide is flow data averaged between the last and
!-----this timestamp for each tide.
!-----Determine if data is available in current or new tidefile;
!-----read tidefile if necessary and store data.
      use io_units
      use logging
      use common_tide
      use runtime_data
      use iopath_data
      implicit none

!-----local variables
      character    &
          filenm*150    &       ! current tidefile name
          ,jmin2cdt*14         ! julian minute to char function

      integer ::     i                    ! loop indices

     
      integer, save :: prev_read_tidetime = miss_val_i     
      integer, save :: first_used_tidefile = miss_val_i
      integer, save :: prev_tidefile = miss_val_i

      logical   &
          new_tidefile  &       ! true if new tidefile
          ,foundtime


      integer, external :: GetCurrentTideTime
      integer, external :: SetHDF5ToTime

      external jmin2cdt

      foundtime = .false.
      do i=max(current_tidefile,1),nintides     
         if (julmin .ge. tide_files(i).start_julmin .and.   &
             julmin .le. tide_files(i).end_julmin) then
            new_tidefile=current_tidefile .ne. i
	      if (new_tidefile .and. current_tidefile .ne. miss_val_i)  &
              call CloseHDF5()
	      prev_tidefile=current_tidefile
            current_tidefile=i
            if (first_used_tidefile .eq. miss_val_i) then
		     first_used_tidefile=i
            end if
            foundtime = .true.
	      exit
         endif
      enddo
      if (.not. foundtime) then
610     format(/'Unable to find a tidefile for current time: ',a)
        write(unit_error,610) current_date
        call exit(2)
      end if

      filenm=tide_files(current_tidefile).filename
      if (new_tidefile) then
	   !@todo: why do we read_tide_head? We want some specific dated info, 
	   !       but mostly this is just dangerous. 
	   !       It would be better to have a smaller read of any
	   !       info that actually changes between tidefiles.
         call read_tide_head(filenm, .true.)
         if (print_level.ge.1) then
            write(unit_screen,922) trim(filenm),  current_date
            write(unit_output,922) trim(filenm), current_date
 922        format(/'Opened a new tidefile: ',/a     &
                /' model time: ',a)
         endif
         ! When a transition occurs at t0, the old tidefile is read for the
	   ! time step that reads t0. Here, we are about to read/calculate
	   ! t1. Need to reload the data from t0 as if it came from the new
	   ! tidefile, and then reconcile any differences in ProcessTide.
         call ReadDataFromHDF5(tide_files(current_tidefile).start_julmin) 
	   call process_tide(new_tidefile,first_used_tidefile,current_tidefile)
         prev_read_tidetime = getCurrentTideTime() ! forces a second read based on julmin
         
      endif 
      new_tidefile = .false.
!-----read tide flows
	if (julmin .gt. prev_read_tidetime ) then
	  ! flow values must be updated, rather than reused
	   call ReadDataFromHDF5(julmin)
	   prev_read_tidetime = getCurrentTideTime()
         call process_tide(new_tidefile,first_used_tidefile,current_tidefile)
      end if


	return
      end subroutine
            
end module


