

!> This module contains unit tests for gtm_hdf_write module, 
!> which mainly is used to print out time series output into 
!> HDF file.
!>@ingroup test_process_io
module ut_gtm_hdf_write

    use fruit
 
    contains
    
    !> Main routines to call all unit tests for writing data into HDF file
    !>                          
    !>                           |<--------------------CHANNEL 1------------------->|                   
    !>  comp: 7             comp:3,5,6                  comp:2                comp: 1
    !>  dsm2: 54              dsm2:52                                         dsm2: 51 
    !>   O=========5500'=========O========5000'===========O=========5000'===========O 
    !>                            \\
    !>                             \\
    !>                              =========6000'=========O dsm2:53, comp:4
    !>   |<-----CHANNEL 2--------->|<-----CHANNEL 2------->|
    !> 
    subroutine test_gtm_hdf_write()
        use hdf5   
        use common_variables
        use gtm_hdf_write
        implicit none
        integer(HID_T) :: file_id, geom_id
        character*128 :: hdf_name                   ! name of qual hdf5 file
        integer :: err
        logical :: h5_file_exists
        
        hdf_name = "gtm_out_hdf_test_geom.h5"
	    ! check to see if file exists
     	inquire (file = hdf_name, exist = h5_file_exists)

    	if (h5_file_exists) then
	         write(unit_error,920) trim(hdf_name)
 920         format(' File already exists... deleting existing file :: ', a )
	    endif        
	    
        call h5open_f(err)
	    call h5fcreate_f(hdf_name, H5F_ACC_TRUNC_F, file_id, err)
        call create_geometry_group(geom_id, file_id)
        
        n_chan = 3
        n_segm = 4
        n_node = 4
        n_comp = 7
        n_resv = 0
        n_qext = 0
        n_conn = 6
        n_boun = 2
        call allocate_channel_property
        call allocate_reservoir_property
        call allocate_qext_property
        call allocate_comp_pt_property
        chan_geom(1)%chan_no = 1
        chan_geom(1)%up_node = 51
        chan_geom(1)%down_node = 52
        chan_geom(1)%channel_num = 1
        chan_geom(1)%channel_length = 10000
        chan_geom(2)%chan_no = 2
        chan_geom(2)%up_node = 53
        chan_geom(2)%down_node = 52
        chan_geom(2)%channel_num = 2
        chan_geom(2)%channel_length = 6000       
        chan_geom(3)%chan_no = 3
        chan_geom(3)%up_node = 52
        chan_geom(3)%down_node = 54
        chan_geom(3)%channel_num = 3
        chan_geom(3)%channel_length = 5500          
        comp_pt(1)%comp_index = 1
        comp_pt(2)%comp_index = 2
        comp_pt(3)%comp_index = 3
        comp_pt(4)%comp_index = 4
        comp_pt(5)%comp_index = 5
        comp_pt(6)%comp_index = 6
        comp_pt(7)%comp_index = 7
        comp_pt(1)%chan_no = 1
        comp_pt(2)%chan_no = 1
        comp_pt(3)%chan_no = 1
        comp_pt(4)%chan_no = 2
        comp_pt(5)%chan_no = 2
        comp_pt(6)%chan_no = 3
        comp_pt(7)%chan_no = 3
        comp_pt(1)%distance = 0
        comp_pt(2)%distance = 5000
        comp_pt(3)%distance = 10000
        comp_pt(4)%distance = 0
        comp_pt(5)%distance = 6000        
        comp_pt(6)%distance = 0
        comp_pt(7)%distance = 5500
        
        call assign_chan_comppt        
        call assign_segment
        call get_dsm2_network_info
        
        call write_segment_info(geom_id)
        call write_channel_info(geom_id)
        call write_dsm2_network_info(geom_id)
        call write_connection_info(geom_id)
        call h5gclose_f(geom_id, err)
        call h5fclose_f(file_id, err)
        call h5close_f(err)   
        call deallocate_geometry
        return
    end subroutine    

end module