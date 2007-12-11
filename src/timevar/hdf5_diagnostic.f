      subroutine VerifyHDF5(error,message)
	use io_units
	use hdfvars
	implicit none

	integer error
	character*(*)message

	if (error .ne. 0)then
        write(unit_error,1212) ,message,error, hdf5point
	  call exit(2)
      end if
1212  format(/"Verify HDF failed: ",a,/"Error: ",i4,/"HDF point: ",i12)
	return
	end subroutine



      subroutine ReportOpenData()
	use io_units
	use hdfvars
	use qextvars
	use inclvars
      implicit none
      integer(HID_T)    :: loc_id      ! File or group identifier 
      character(LEN=60) :: obj_name        ! Name of the group
      integer :: nmembers            ! Number of members in the group
      integer :: hdferr 
	integer :: idx
	integer :: obj_type
	
	if (.not.(mod(hdf5point,12) .lt. 2)) goto 999
	
  	call h5gn_members_f(file_id, "hydro/data", nmembers, hdferr)
      write(unit_screen,*)"HDFPoint: ", hdf5point," Number data members open: ",nmembers

	do idx=0,(nmembers-1)
        call h5gget_obj_info_idx_f(file_id, "hydro/data", idx, 
     &                             obj_name, obj_type, hdferr)

	  write(unit_screen,"(a,i5)")trim(obj_name), obj_type

	end do

999	return
	end subroutine


