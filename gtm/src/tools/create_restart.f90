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
module create_restart

    contains
    
    !> Routine to create restart file from gtm tide file
    subroutine create_restart_file(outfile,           &
                                   gtm_tidefile,      &
                                   time_selected)
        use gtm_precision
        use error_handling
        use hdf5
        use h5lt
        use time_utilities        
        implicit none
        character*(*), intent(in) :: outfile              !< output text file
        character*(*), intent(in) :: gtm_tidefile         !< gtm tidefile name
        character(len=14), intent(in) :: time_selected    !< time selected
        integer(HID_T) :: gtm_file_id                     ! HDF5 File identifier
        integer(HID_T) :: geom_id
        integer(HID_T) :: output_id
        integer(HID_T) :: data_id                         ! local group identifier
        integer(HID_T) :: dset_id                         ! local dataset identifier
        integer(HID_T) :: dataspace                       ! Dataspace identifier
        integer(HID_T) :: memspace                        ! memspace identifier        
        integer(HSIZE_T), dimension(3) :: count           ! local variable
        integer(HSIZE_T), dimension(3) :: offset          ! local variable
        integer(HSIZE_T), dimension(3) :: offset_out      ! local variable      
        integer(HSIZE_T), dimension(3) :: data_dims       ! local datasets dimensions
        integer(HSIZE_T), dimension(3) :: dimsm           ! local variable
        integer :: memrank = 3                            ! memory rank                       
        integer :: jmin, file_unit
        logical :: file_exists
        integer,dimension(1) :: hdf_dummy_integer        
        integer :: error, i, j
        real(gtm_real) :: gtm_time_interval, gtm_start_jmin
        real(gtm_real),dimension(1) :: hdf_dummy_real
        integer :: ncell, nvar
        integer :: time_offset, buffer_size               
        real(gtm_real), allocatable :: init(:,:,:)        ! output array
        
        file_unit = 131
        jmin = cdt2jmin(time_selected)
        inquire(file=gtm_tidefile, exist=file_exists)
        if (file_exists) then        
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
                    "n_cell", hdf_dummy_integer, error)
            ncell = hdf_dummy_integer(1)            
            call h5ltget_attribute_int_f(geom_id, ".",    &
                    "n_var", hdf_dummy_integer, error)
            nvar = hdf_dummy_integer(1)              
            call verify_error(error, "Reading attribute from hdf5 file")            
            call h5gclose_f(geom_id, error)
            
            call h5gopen_f(gtm_file_id, "output", output_id, error)
            call h5dopen_f(output_id, "cell concentration", dset_id, error) 
            call h5dget_space_f(dset_id, dataspace, error)

            buffer_size  = 1
            time_offset = (jmin-gtm_start_jmin)/gtm_time_interval             
            allocate(init(nvar, ncell, buffer_size))  
            init = LARGEREAL          
            dimsm = (/nvar, ncell, buffer_size/)
            offset = (/0, 0,time_offset/)
            count = (/nvar, ncell, buffer_size/) 
            offset_out = (/0,0,0/)       
            call h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, offset, count, error)
            call h5screate_simple_f(memrank, dimsm, memspace, error)
            call h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, offset_out, count, error)
            data_dims(1) = nvar
            data_dims(2) = ncell
            data_dims(3) = buffer_size
            call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, init, data_dims, error, memspace, dataspace) !todo::not sure why INTEGER works while DOUBLE not.
            
            call h5sclose_f(dataspace, error)
            call h5sclose_f(memspace, error)
            call h5dclose_f(dset_id, error)
            call h5gclose_f(output_id, error)
            call h5fclose_f(gtm_file_id, error)        
            
            open(file_unit, file=outfile)
            write(file_unit,*) jmin2cdt(jmin)
            write(file_unit,*) jmin 
            write(file_unit,*) nvar, "/n_var"
            write(file_unit,*) ncell, "/n_cell"
            do i = 1, ncell
                write(file_unit,*) (init(j,i,1), j =1,nvar)
            end do              
            close(file_unit)                    
        else
            call gtm_fatal(gtm_tidefile//" is not a valid tidefile under working directory!")  
        end if    

        return  
    end subroutine
    
end module    
    
