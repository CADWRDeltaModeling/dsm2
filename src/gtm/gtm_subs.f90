
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
        do i = 1, n_junc
            write(debug_unit,'(a10, i10)') "DSM2_node:", junc(i)%dsm2_node_no
            do j = 1, junc(i)%n_conn_cells
                write(debug_unit,'(2i10)') junc(i)%cell_no(j), junc(i)%up_down(j)
            end do
        end do    
        write(debug_unit,'(3a10)') "DSM2_node","cell_no","up_down"
        do i = 1, n_boun
            write(debug_unit,'(3i10)') bound(i)%dsm2_node_no, bound(i)%cell_no, bound(i)%up_down
        end do     
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
        integer :: error
        call create_geometry_group(geom_id, file_id)
        call write_segment_info(geom_id, n_segm, segm)
        call write_channel_info(geom_id, n_chan, chan_geom)
        call write_junction_info(geom_id, n_junc, junc)
        call write_boundary_info(geom_id, n_boun, bound)
        call write_connection_info(geom_id, n_conn, conn)
        call h5gclose_f(geom_id, error) 
        return
    end subroutine    
    
end module    