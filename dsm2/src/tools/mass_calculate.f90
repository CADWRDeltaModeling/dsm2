!<license>
!    Copyright (C) 2017 State of California,
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

!>@ingroup tools
module mass_calculate

    contains
    
    !> Routine to create restart file from gtm tide file
    subroutine calculate_mass(outfile,           &
                              gtm_tidefile,      &
                              start_time,        &
                              end_time,          &
                              exclude_channels)
        use gtm_precision
        use error_handling
        use hdf5
        use h5lt
        use time_utilities        
        implicit none
        character*(*), intent(in) :: outfile              !< output text file
        character*(*), intent(in) :: gtm_tidefile         !< gtm tidefile name
        character*(*), intent(in) :: start_time           !< start time
        character*(*), intent(in) :: end_time             !< end time
        character*(*), intent(in) :: exclude_channels     !< exclude channels list filename
        integer(HID_T) :: gtm_file_id                     ! HDF5 File identifier
        integer(HID_T) :: geom_id                         ! geom folder identifier
        integer(HID_T) :: output_id                       ! output folder identifier
        integer(HID_T) :: data_id                         ! local group identifier
        integer(HID_T) :: dset_id                         ! local dataset identifier
        integer(HID_T) :: dataspace                       ! Dataspace identifier
        integer(HID_T) :: memspace                        ! memspace identifier        
        integer(HID_T) :: hdf5_access_plist               ! HDF5 property identifier
	    integer(SIZE_T) :: rddc_nelmts                    ! HDF5 property identifier 
        integer(SIZE_T) :: rddc_nbytes                    ! HDF5 property identifier
	    integer :: nelmts                                 ! HDF5 property identifier
	    real :: rdcc_w0                                   ! HDF5 property identifier
        integer(HSIZE_T), dimension(3) :: count           ! local variable
        integer(HSIZE_T), dimension(3) :: offset          ! local variable
        integer(HSIZE_T), dimension(3) :: offset_out      ! local variable      
        integer(HSIZE_T), dimension(3) :: dimsm           ! local variable
        integer(HSIZE_T), dimension(3) :: data_dims       ! local datasets dimensions
        integer :: memrank = 3                            ! memory rank
        integer :: jmin, file_unit
        logical :: file_exists
        integer,dimension(1) :: hdf_dummy_integer      
        character(10) :: hdf_dummy_char        
        real(gtm_real),dimension(1) :: hdf_dummy_real
        integer :: time_offset, buffer_size        
               
        integer(HID_T) :: chan_dset_id                    ! Dataset identifier
        integer(HID_T) :: chan_dt_id                      ! Memory datatype identifier
        integer(HID_T) :: dt1_id, dt2_id, dt3_id, dt4_id  ! Memory datatype identifier
        integer(HSIZE_T), dimension(1) :: chan_data_dims  ! Datasets dimensions
        integer(SIZE_T) :: typesize                       ! Size of the datatype
        integer(SIZE_T) :: type_size                      ! Size of the datatyp
        integer(SIZE_T):: chan_offset                     ! Member's offset        
        
        !> Define channel type to store channel related arrays
        type channel_t                                    !< channel between hydro nodes
            integer :: channel_num                       !< actual channel number in DSM2 grid
            integer :: chan_no                           !< index channel number
            integer :: start_cell                        !< starting cell
            integer :: end_cell                          !< ending cell
        end type
        type(channel_t), allocatable :: chan_geom(:)
              
        integer :: error, i, j, k, h
        real(gtm_real) :: gtm_time_interval, gtm_start_jmin
        real(gtm_real) :: jmin_start, jmin_end
        integer :: nchan, nvar
        real(gtm_real), parameter ::  unit_conversion = 28.3168d0/1000000000000.d0  ! cubit foot*mg/l--> kton
        real, allocatable :: init(:,:,:)        ! output array
        real, allocatable :: budget(:,:)
        integer :: n_exclude
        integer, allocatable :: chan_exclude(:)   
        integer, allocatable :: chan_exclude_no(:)   
        real(gtm_real) :: total, exclude_chan
        integer :: char_index
        real(gtm_real) :: hdf_interval
        character(3) :: start_month
        integer :: start_year
        integer :: months(12), skip
        real(gtm_real), allocatable :: month_budget(:,:,:)
        
        jmin_start = cdt2jmin(start_time)
        jmin_end = cdt2jmin(end_time)
        file_unit = 131
        error = 0 
        start_month = start_time(3:5)
        read(start_time(6:9),*,iostat=error) start_year
        if (mod(start_year+1,4).eq.0) then
            months = (/31,30,31,31,29,31,30,31,30,31,31,30/)
        else
            months = (/31,30,31,31,28,31,30,31,30,31,31,30/)
        end if      

        inquire(file=gtm_tidefile, exist=file_exists)
        if (file_exists) then
            call h5open_f(error)             
           	! set up stdio to allow for buffered read/write          	
	        call h5pcreate_f(H5P_FILE_ACCESS_F, hdf5_access_plist, error)      
	        call h5pget_cache_f(hdf5_access_plist, nelmts,                &
                                rddc_nelmts, rddc_nbytes, rdcc_w0, error)
	        rddc_nbytes = 8000000
	        call h5pset_cache_f(hdf5_access_plist, nelmts, rddc_nelmts,   &
                                rddc_nbytes, rdcc_w0,error)
	        call verify_error(error,"Cache set")
	        
            call h5fopen_f (gtm_tidefile, H5F_ACC_RDONLY_F, gtm_file_id, error)
            call verify_error(error, "opening tidefile") 
            call h5gopen_f(gtm_file_id, "geometry", geom_id, error)       
            call h5ltget_attribute_double_f(geom_id, ".",    &
                    "gtm_time_interval", hdf_dummy_real, error)
            gtm_time_interval = hdf_dummy_real(1)
            call h5ltget_attribute_double_f(geom_id, ".",    &
                    "gtm_start_jmin", hdf_dummy_real, error)
            gtm_start_jmin = hdf_dummy_real(1)
            call h5ltget_attribute_int_f(geom_id, ".",    &
                    "n_chan", hdf_dummy_integer, error)
            nchan = hdf_dummy_integer(1)            
            call h5ltget_attribute_int_f(geom_id, ".",    &
                    "n_var", hdf_dummy_integer, error)
            nvar = hdf_dummy_integer(1)              
            call verify_error(error, "Reading attribute from hdf5 file")            

            !---read channel geometry
            allocate(chan_geom(nchan))
            chan_data_dims(1) = nchan
            call h5dopen_f(geom_id, "channel", chan_dset_id, error) 
            call h5tcopy_f(H5T_NATIVE_INTEGER, chan_dt_id, error)
            typesize = 4
            call h5tset_size_f(chan_dt_id, typesize, error)
            call h5tget_size_f(chan_dt_id, type_size, error)
            chan_offset = 0   
            call h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
            call h5tinsert_f(dt1_id, "chan_no", chan_offset, chan_dt_id, error)    
            call h5dread_f(chan_dset_id, dt1_id, chan_geom%chan_no, chan_data_dims, error) 
                           
            call h5tcreate_f(H5T_COMPOUND_F, type_size, dt2_id, error)
            call h5tinsert_f(dt2_id, "channel_num", chan_offset, chan_dt_id, error)    
            call h5dread_f(chan_dset_id, dt2_id, chan_geom%channel_num, chan_data_dims, error) 

            call h5tcreate_f(H5T_COMPOUND_F, type_size, dt3_id, error)
            call h5tinsert_f(dt3_id, "start_cell", chan_offset, chan_dt_id, error)    
            call h5dread_f(chan_dset_id, dt3_id, chan_geom%start_cell, chan_data_dims, error) 
            
            call h5tcreate_f(H5T_COMPOUND_F, type_size, dt4_id, error)
            call h5tinsert_f(dt4_id, "end_cell", chan_offset, chan_dt_id, error)    
            call h5dread_f(chan_dset_id, dt4_id, chan_geom%end_cell, chan_data_dims, error) 
                      
            call h5tclose_f(chan_dt_id, error)
            call h5tclose_f(dt1_id, error)
            call h5tclose_f(dt2_id, error)
            call h5tclose_f(dt3_id, error)
            call h5tclose_f(dt4_id, error)
            call h5dclose_f(chan_dset_id, error)  
            call h5gclose_f(geom_id, error)
            
            !---read mass budget
            call h5gopen_f(gtm_file_id, "output", output_id, error)
            call h5dopen_f(output_id, "channel budget", dset_id, error)
            call h5dget_space_f(dset_id, dataspace, error)
            if (error.ne.0) call gtm_fatal("budget table is not available!")             

            call h5ltget_attribute_string_f(dset_id, ".","interval",hdf_dummy_char,error)
            char_index = index(hdf_dummy_char,"min")
            read(hdf_dummy_char(1:char_index-1),*,iostat=error) hdf_interval
            buffer_size  = (jmin_end - jmin_start)/hdf_interval
            time_offset = (jmin_start - gtm_start_jmin)/hdf_interval
            allocate(init(nchan, nvar, buffer_size))  
            allocate(budget(nchan, nvar))
            allocate(month_budget(12,nchan,nvar))
            init = 0  
            budget = 0.0
            dimsm = (/nchan,nvar,buffer_size/)
            offset = (/0, 0, time_offset/)
            count = (/nchan,nvar,buffer_size/) 
            offset_out = (/0,0,0/)       
            call h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, offset, count, error)
            call h5screate_simple_f(memrank, dimsm, memspace, error)
            call h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, offset_out, count, error)
            data_dims(1) = nchan
            data_dims(2) = nvar
            data_dims(3) = buffer_size
            call h5dread_f(dset_id, H5T_NATIVE_REAL, init(:,:,:), data_dims, error, memspace, dataspace) 
            
            if (exclude_channels.ne.'na') then
                open(801,file = exclude_channels)
                read(801,*) n_exclude
                allocate(chan_exclude(n_exclude))
                allocate(chan_exclude_no(n_exclude))
                do i = 1, n_exclude
                    read(801,*) chan_exclude(i)
                    do j = 1, nchan
                        if(chan_exclude(i).eq.chan_geom(j)%channel_num) then
                            chan_exclude_no(i) = chan_geom(j)%chan_no
                        end if    
                    end do
                end do
                close(801)
            end if   

            ! calculate monthly budget for each channel
            skip = 0
            month_budget = zero
            do h = 1, 12
                do  i = 1, nvar
                    do j = 1, nchan
                        do k = 1, months(h)*(24*60/hdf_interval)
                            month_budget(h,j,i) = month_budget(h,j,i) + init(j,i,skip+k)*unit_conversion
                        end do
                    end do
                end do
                skip = skip +  months(h)*(24*60/hdf_interval)
            end do
            
            ! calculate annual budget for each channel
            do  i = 1, nvar
                do j = 1, nchan
                    do k = 1, buffer_size
                        budget(j,i) = budget(j,i) + init(j,i,k)*unit_conversion
                    end do
                end do
            end do
            
            ! to calculate the total sediment load 
            total = 0
            exclude_chan = 0
            do i = 1, nvar
                do j = 1, nchan
                    total = total + budget(j,i)
                end do    
                do j = 1, n_exclude
                    exclude_chan = exclude_chan + budget(chan_exclude_no(j),i)
                end do
            enddo
            total = total - exclude_chan
            
            open(file_unit, file=outfile)           
            write(file_unit,'(a20,f20.3)') "Annual Sediment Budget =", total 
            do j = 1, nchan
                write(file_unit,'(i10,3f10.4)') chan_geom(j)%channel_num,budget(j,1),budget(j,2),budget(j,3)
            end do
            write(file_unit,*) "Monthly Sediment Budget:"
            do j = 1, nchan
                write(file_unit,'(i10,12f10.4)') chan_geom(j)%channel_num,(month_budget(k,j,1),k=1,12)
            end do
            
            call h5sclose_f(dataspace, error)
            call h5sclose_f(memspace, error)
            call h5dclose_f(dset_id, error)
            call h5gclose_f(output_id, error)
            call h5fclose_f(gtm_file_id, error)        
            call h5pclose_f(hdf5_access_plist, error)                   
            call h5close_f(error)   
            close(file_unit)                    
        else
            call gtm_fatal(gtm_tidefile//" is not a valid tidefile under working directory!")  
        end if
        deallocate(init)
        deallocate(budget)
        deallocate(month_budget)
        deallocate(chan_geom)
        deallocate(chan_exclude)
        return  
    end subroutine
    
end module    
    
