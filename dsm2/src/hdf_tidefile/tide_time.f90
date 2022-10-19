!***********************************************************************
!***********************************************************************

integer function GetCurrentTideTime()

	use hdfvars
	use inclvars

	implicit none

	GetCurrentTideTime=h5_time_start + (hdf5point)*h5_time_interval

	return
end function

!***********************************************************************
!***********************************************************************
integer function calcHDF5NumberOfTimeIntervals() result (ans)

	use hdfvars
	use inclvars
	use runtime_data
	use common_tide

	implicit none
	integer span
	span = end_julmin - tf_start_julmin
	ans = span/TideFileWriteInterval
	
	if (mod(span,TideFileWriteInterval) .eq. 0) then
	  ans = ans+1
	end if
	return 
	
end function




!***********************************************************************
!***********************************************************************

integer function getHDF5TimeLength() result (ans)

	use hdfvars
	use inclvars

	implicit none

	integer getHDF5TimeInterval
	integer getHDF5NumberOfTimeIntervals

	ans = (getHDF5NumberOfTimeIntervals() - 1) * getHDF5TimeInterval()

	return 
	
end function

!***********************************************************************
!***********************************************************************

integer function getHDF5EndTime() result (time)

	use hdfvars

	implicit none

	integer getHDF5StartTime
	integer getHDF5TimeLength

	time = getHDF5StartTime() + getHDF5TimeLength()

	return 
	
end function

!***********************************************************************
!***********************************************************************

integer function getHDF5IndexForTideTime(time) result (index)

	use hdfvars

	implicit none

	integer, intent(in) :: time

	integer,external :: getHDF5StartTime
	integer,external :: getHDF5TimeInterval

	index = ((time - h5_time_start) / getHDF5TimeInterval())

	return 
	
end function

!***********************************************************************
!***********************************************************************

integer function getHDF5IndexAtOrBeyondTime(tidetime) result(index)

	use hdfvars

	implicit none

	integer, intent(in) :: tidetime

	integer, external :: getHDF5StartTime
	integer, external :: getHDF5TimeInterval
	integer :: reltime
	integer :: intvl

	reltime = tidetime - h5_time_start
	intvl = getHDF5TimeInterval()

	if(mod(reltime,intvl).eq.0) then
	  index = reltime/intvl
	else
	  index = (reltime/intvl)+1
      endif
	return 
end function




!**********************************************************************
!**********************************************************************
subroutine getTimeAttributes(file_id, &
                                   hdf_start_julmin, &
                                   hdf_end_julmin, &
                                   hdf_interval, &
                                   hdf_ntime)
      use hdf5
      use h5lt
      implicit none
      integer(HID_T) :: file_id
      integer :: hdf_start_julmin
      integer :: hdf_end_julmin
      integer :: hdf_interval
      integer :: hdf_ntime
      integer, dimension(1) :: hdf5_read_buffer      
      integer        :: error   ! HDF5 Error flag

      call h5ltget_attribute_int_f(file_id,"hydro", &
                "Start time", &
                hdf5_read_buffer, error)
      call verify_error(error, "Reading start time from hdf5 file")
      hdf_start_julmin = hdf5_read_buffer(1)
      call h5ltget_attribute_int_f(file_id,"hydro", &
                "Time interval", &
                hdf5_read_buffer, error)
      hdf_interval = hdf5_read_buffer(1)
      call h5ltget_attribute_int_f(file_id,"hydro", &
                "Number of intervals", &
                hdf5_read_buffer, error)
      hdf_ntime = hdf5_read_buffer(1)
      hdf_end_julmin=hdf_start_julmin+(hdf_ntime-1)*hdf_interval
      return
end subroutine

!***********************************************************************
!***********************************************************************

integer function getHDF5StartTime() RESULT (time)
      ! read the start time from the HDF5 file 

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use common_tide

      implicit none


      integer(HID_T) :: attr_id ! Attribute identifier 
      integer(HID_T) :: atype_id ! Attribute Dataspace identifier 
      integer        :: error   ! HDF5 Error flag
      integer(HSIZE_T), dimension(7) :: a_data_dims
      
      integer, save :: temptime
      integer getHDF5TimeInterval

      integer,save :: prev_tidefile = 0

	if (.not. (current_tidefile .eq. prev_tidefile)) then
          a_data_dims(1) = 1
        call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
        call h5aopen_name_f(hydro_id,"Start time",attr_id,error)
        call h5aread_f(attr_id, atype_id, temptime, a_data_dims, error)
        call h5aclose_f(attr_id, error)
      end if

      time = temptime

      return
end

!***********************************************************************
!***********************************************************************

integer function getHDF5TimeInterval() RESULT (out)

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use common_tide
      implicit none

      integer(HID_T) :: attr_id ! Attribute identifier 
      integer(HID_T) :: atype_id ! Attribute Dataspace identifier 
      integer        :: error   ! HDF5 Error flag
      integer(HSIZE_T), dimension(7) :: a_data_dims
      integer,save :: prev_tidefile = 0
      integer, save :: interval

	if (.not. (current_tidefile .eq. prev_tidefile)) then
	   a_data_dims(1) = 1

         call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
         call h5aopen_name_f(hydro_id,"Time interval",attr_id,error)
         call h5aread_f(attr_id, atype_id, interval, a_data_dims, error)
         call h5aclose_f(attr_id, error)
	   prev_tidefile = current_tidefile
      end if
	out=interval

    
      return
end

!***********************************************************************
!***********************************************************************

integer function getHDF5NumberOfTimeIntervals() RESULT (out)

      use HDF5                  ! HDF5 This module contains all necessary modules 
      use hdfvars
      use common_tide
      implicit none

      integer(HID_T) :: attr_id ! Attribute identifier 
      integer(HID_T) :: atype_id ! Attribute Dataspace identifier 
      integer        :: error   ! HDF5 Error flag
      integer(HSIZE_T), dimension(7) :: a_data_dims
      integer,save :: prev_tidefile = 0
      integer, save :: number


	if (.not. (current_tidefile .eq. prev_tidefile)) then
          a_data_dims(1) = 1
        call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
        call h5aopen_name_f(hydro_id,"Number of intervals",attr_id,error)
        call h5aread_f(attr_id, atype_id, number, a_data_dims, error)
        call h5aclose_f(attr_id, error)
      end if
      out = number

      return
end



!***********************************************************************
!***********************************************************************

integer function SetHDF5ToTime(tidetime) result(out)
!----- Purpose: sets the tidefile to the first row that is at or
!      beyond the input tidetime
	use hdfvars
	implicit none
	integer tidetime,tempndx
	integer,external :: getHDF5IndexAtOrBeyondTime
	integer,external :: GetCurrentTideTime
	tempndx=getHDF5IndexAtOrBeyondTime(tidetime)
      hdf5point=tempndx

	out=GetCurrentTideTime()
	return
end








