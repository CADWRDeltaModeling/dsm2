
module gtm_subs

    contains
    

    !> Serve debug purpose: write geom info to text file
    subroutine write_geom_to_text()
        use gtm_logging
        use common_variables
        implicit none
        integer :: i, j
        write(debug_unit,'(2a10,a15)') "segm_no","chan_no","segm_length"
        write(debug_unit,'(2i10,f15.2)') &
             (segm(i)%segm_no, segm(i)%chan_no, segm(i)%length, i=1,n_segm)
        write(debug_unit,'(4a10)') "conn_no","chan_no","cell_no","up_down"
        write(debug_unit,'(4i10)')       &
             (conn(i)%conn_no, conn(i)%chan_no, conn(i)%cell_no, conn(i)%conn_up_down, i=1,n_conn)
        return
    end subroutine
    
    
    !> Write GTM grid info into GTM tidefile
    subroutine write_grid_to_tidefile(file_id)
        use hdf5
        use gtm_hdf_write
        use common_variables
        implicit none
        integer(HID_T), intent(in) :: file_id
        integer(HID_T) :: geom_id
        integer :: err
        call create_geometry_group(geom_id, file_id)
        call write_attributes_gtm(geom_id)
        call write_segment_info(geom_id)
        call write_channel_info(geom_id)
        call write_reservoir_info(geom_id)
        call write_qext_info(geom_id)
        call write_connection_info(geom_id)
        call write_dsm2_node_info(geom_id)
        call h5gclose_f(geom_id, err) 
        return
    end subroutine    
    
end module    