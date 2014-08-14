
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
        call write_dsm2_network_info(geom_id)
        call h5gclose_f(geom_id, err) 
        return
    end subroutine    


    !> assign value to dsm2_network(:)%node_conc, pathinput(:)%i_node, pathinput(:)%i_var
    subroutine assign_node_ts()
        use common_variables, only : n_node, dsm2_network, n_var, constituents
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
                if (pathinput(i)%obj_no .eq. dsm2_network(j)%dsm2_node_no) then
                    pathinput(i)%i_node = j
                    dsm2_network(j)%node_conc = 1
                end if        
            end do
        end do
        return
    end subroutine        

    !> check the flow mass balance at DSM2 network
    subroutine flow_mass_balance_check(ncell, nqext, nresv_conn, flow_lo, flow_hi, qext_flow, resv_flow) 
        use gtm_precision
        use common_variables, only : n_node, dsm2_network
        implicit none
        integer, intent(in) :: ncell
        integer, intent(in) :: nqext
        integer, intent(in) :: nresv_conn
        real(gtm_real), intent(in) :: flow_lo(ncell)
        real(gtm_real), intent(in) :: flow_hi(ncell)
        real(gtm_real), intent(in) :: qext_flow(nqext)
        real(gtm_real), intent(in) :: resv_flow(nresv_conn)        
        real(gtm_real) :: flow_chk
        integer :: i, j
        
        do i = 1, n_node
            flow_chk = zero
            do j = 1, dsm2_network(i)%n_conn_cell
                if (dsm2_network(i)%up_down(j) == 0 ) then  !upstream
                    !write(11,*) dsm2_network(i)%dsm2_node_no, dsm2_network(i)%up_down(j), flow_hi(dsm2_network(i)%cell_no(j))
                    flow_chk = flow_chk + flow_hi(dsm2_network(i)%cell_no(j))
                else  
                    !write(11,*) dsm2_network(i)%dsm2_node_no, dsm2_network(i)%up_down(j), flow_lo(dsm2_network(i)%cell_no(j))
                    flow_chk = flow_chk + minus*flow_lo(dsm2_network(i)%cell_no(j))
                end if
            end do 
            do j = 1, dsm2_network(i)%n_qext 
                 !write(11,*) dsm2_network(i)%dsm2_node_no, qext_flow(dsm2_network(i)%qext_no(j)),"qext_flow"
                 flow_chk = flow_chk + qext_flow(dsm2_network(i)%qext_no(j))
            end do
            if (dsm2_network(i)%reservoir_no.ne.0) then 
                  !write(11,*) dsm2_network(i)%dsm2_node_no, resv_flow(dsm2_network(i)%resv_conn_no),"resv_flow"
                  flow_chk = flow_chk + resv_flow(dsm2_network(i)%resv_conn_no)
            end if                   
            if (abs(flow_chk) .gt. weakest_eps) write(11,*) dsm2_network(i)%dsm2_node_no, flow_chk, "*******" 
        end do        
    end subroutine
    
end module    