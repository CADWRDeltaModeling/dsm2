
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


    !> assign value to dsm2_node(:)%node_conc, pathinput(:)%i_node, pathinput(:)%i_var
    subroutine assign_node_ts()
        use common_variables, only : n_node, dsm2_node, n_var, constituents
        use common_dsm2_vars, only : n_inputpaths, pathinput    
        implicit none
        integer :: i, j
        do i = 1, n_inputpaths
            do j = 1, n_var
                call locase(pathinput(i)%variable)
                call locase(constituents(j)%name)
                if (trim(pathinput(i)%variable) .eq. trim(constituents(j)%name)) then
                    pathinput(i)%i_var = constituents(j)%conc_no
                end if
            end do
            do j = 1, n_node 
                if (pathinput(i)%obj_no .eq. dsm2_node(j)%dsm2_node_no) then
                    pathinput(i)%i_node = j
                    dsm2_node(j)%node_conc = 1
                end if        
            end do
        end do
        return
    end subroutine        
    
end module    