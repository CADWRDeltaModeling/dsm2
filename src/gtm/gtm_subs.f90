
module gtm_subs

    contains
    
    !> subroutine to print out time series for selected cells into a text file
    subroutine get_select_cell(out_cell,            &  !< output cells
                               n_out_cell,          &  !< number of output cells
                               chan_num,            &  !< total number of cells 
                               x_dist)                 !< input file name
        use gtm_precision
        use common_variables, only: n_chan, n_segm, chan_geom, segm
        implicit none            
        integer, intent(out) :: out_cell(n_out_cell)
        integer, intent(in) :: n_out_cell
        integer, intent(in) :: chan_num(n_out_cell)
        real(gtm_real), intent(in) :: x_dist(n_out_cell)
        integer :: i, j, k, chan_no
        do i = 1, n_out_cell
            do j = 1, n_chan
                if (chan_num(i).eq.chan_geom(j)%channel_num) then
                    chan_no = chan_geom(j)%chan_no
                    do k = 1, n_segm
                        if (segm(k)%chan_no .eq. chan_no) then
                            if ((x_dist(i).ge.segm(k)%up_distance).and.(x_dist(i).lt.segm(k)%down_distance)) then
                                out_cell(i) = segm(k)%start_cell_no + int((x_dist(i)-segm(k)%up_distance)/(segm(k)%length/segm(k)%nx))
                                goto 10    
                            end if     
                            if (x_dist(i).eq.segm(k)%down_distance) then
                                out_cell(i) = segm(k)%start_cell_no + segm(k)%nx - 1
                                goto 10    
                            end if                                                  
                        end if
                    end do
                end if
            end do                
10      enddo
        return
    end subroutine
    
    
    !> temporary output cells for spot checking
    subroutine output_cell_arr(out_cell)
        use gtm_precision
        implicit none
        integer, parameter:: n_out_cell = 39
        integer, intent(out) :: out_cell(n_out_cell)        
        type output_t
            character*16 :: name
            integer :: chan_num
            real(gtm_real) :: distance
        end type
        type(output_t) :: out(n_out_cell)

        out(1)%name = 'RFAL008' 
        out(2)%name = 'RMID015_144' 
        out(3)%name = 'RMID015_145' 
        out(4)%name = 'RMID023' 
        out(5)%name = 'RMID027' 
        out(6)%name = 'RMID040' 
        out(7)%name = 'RMKL019' 
        out(8)%name = 'ROLD014' 
        out(9)%name = 'ROLD024' 
        out(10)%name = 'ROLD046' 
        out(11)%name = 'ROLD047'
        out(12)%name = 'ROLD059' 
        out(13)%name = 'RSAC054'
        out(14)%name = 'RSAC075'
        out(15)%name = 'RSAC081' 
        out(16)%name = 'RSAC092' 
        out(17)%name = 'RSAC139' 
        out(18)%name = 'RSAC142' 
        out(19)%name = 'RSAC155' 
        out(20)%name = 'RSAN007'
        out(21)%name = 'RSAN018' 
        out(22)%name = 'RSAN032'
        out(23)%name = 'RSAN037'
        out(24)%name = 'RSAN058' 
        out(25)%name = 'RSAN072'
        out(26)%name = 'RSAN087'
        out(27)%name = 'RSAN112' 
        out(28)%name = 'RSMKL008' 
        out(29)%name = 'SLCCH016' 
        out(30)%name = 'SLPPR000' 
        out(31)%name = 'SLPPR003' 
        out(32)%name = 'SLRCK005' 
        out(33)%name = 'SLSBT011' 
        out(34)%name = 'SLTRM004' 
        out(35)%name = 'YOLO' 
        out(36)%name = 'VERNALIS' 
        out(37)%name = 'SAC'
        out(38)%name = 'GEORG_SL' 
        out(39)%name = 'RIO'
        out(1)%chan_num = 276 
        out(2)%chan_num = 144 
        out(3)%chan_num = 145 
        out(4)%chan_num = 135 
        out(5)%chan_num = 133 
        out(6)%chan_num = 126 
        out(7)%chan_num = 357 
        out(8)%chan_num = 117 
        out(9)%chan_num = 106 
        out(10)%chan_num = 80 
        out(11)%chan_num = 79 
        out(12)%chan_num = 71 
        out(13)%chan_num = 441 
        out(14)%chan_num = 437 
        out(15)%chan_num = 436 
        out(16)%chan_num = 434 
        out(17)%chan_num = 417 
        out(18)%chan_num = 417 
        out(19)%chan_num = 412 
        out(20)%chan_num = 52      
        out(21)%chan_num = 83 
        out(22)%chan_num = 349 
        out(23)%chan_num = 42 
        out(24)%chan_num = 20 
        out(25)%chan_num = 10 
        out(26)%chan_num = 6 
        out(27)%chan_num = 17 
        out(28)%chan_num = 344 
        out(29)%chan_num = 402 
        out(30)%chan_num = 268 
        out(31)%chan_num = 269 
        out(32)%chan_num = 206 
        out(33)%chan_num = 385 
        out(34)%chan_num = 310 
        out(35)%chan_num = 398 
        out(36)%chan_num = 17 
        out(37)%chan_num = 410 
        out(38)%chan_num = 366       
        out(39)%chan_num = 430
        out(1)%distance = 5480.d0        
        out(2)%distance = 838.d0 
        out(3)%distance = 2114.d0 
        out(4)%distance = 4427.d0 
        out(5)%distance = 3641.d0 
        out(6)%distance = 3951.d0 
        out(7)%distance = 0.d0 
        out(8)%distance = 0.d0 
        out(9)%distance = 2718.d0 
        out(10)%distance = 1431.d0 
        out(11)%distance = 2766.d0 
        out(12)%distance = 3116.d0 
        out(13)%distance = 5398.d0 
        out(14)%distance = 11108.d0 
        out(15)%distance = 5733.d0 
        out(16)%distance = 435.d0 
        out(17)%distance = 1718.d0 
        out(18)%distance = 4400.d0 
        out(19)%distance = 4623.d0 
        out(20)%distance = 366.d0 
        out(21)%distance = 4213.d0 
        out(22)%distance = 9672.d0 
        out(23)%distance = 286.d0 
        out(24)%distance = 2520.d0 
        out(25)%distance = 9400.d0 
        out(26)%distance = 7902.d0 
        out(27)%distance = 4744.d0 
        out(28)%distance = 7088.d0 
        out(29)%distance = 0.d0 
        out(30)%distance = 4440.d0 
        out(31)%distance = 13650.d0 
        out(32)%distance = 0.d0 
        out(33)%distance = 2273.d0 
        out(34)%distance = 540.d0 
        out(35)%distance = 0.d0 
        out(36)%distance = 0.d0 
        out(37)%distance = 0.d0 
        out(38)%distance = 0.d0
        out(39)%distance = 9684.d0         
        call get_select_cell(out_cell, n_out_cell, out(:)%chan_num, out(:)%distance)
        return
    end subroutine    
    
    
    !> Write concentration for output cells
    subroutine print_out_cell_conc(c, nc, out_cell, nout)
        use gtm_precision
        implicit none
        integer, intent(in) :: nc
        integer, intent(in) :: nout
        real(gtm_real), intent(in) :: c(nc)
        integer, intent(in) :: out_cell(nout)
        integer :: i
        write(101,'(39f10.0)') (c(out_cell(i)),i=1,nout)        
        return
    end subroutine   
    
       
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
                    write(11,*) dsm2_network(i)%dsm2_node_no, dsm2_network(i)%up_down(j), flow_hi(dsm2_network(i)%cell_no(j))
                    flow_chk = flow_chk + flow_hi(dsm2_network(i)%cell_no(j))
                else  
                    write(11,*) dsm2_network(i)%dsm2_node_no, dsm2_network(i)%up_down(j), flow_lo(dsm2_network(i)%cell_no(j))
                    flow_chk = flow_chk + minus*flow_lo(dsm2_network(i)%cell_no(j))
                end if
            end do 
            do j = 1, dsm2_network(i)%n_qext 
                 write(11,*) dsm2_network(i)%dsm2_node_no, qext_flow(dsm2_network(i)%qext_no(j)),"qext_flow"
                 flow_chk = flow_chk + qext_flow(dsm2_network(i)%qext_no(j))
            end do
            if (dsm2_network(i)%reservoir_no.ne.0) then 
                  write(11,*) dsm2_network(i)%dsm2_node_no, resv_flow(dsm2_network(i)%resv_conn_no),"resv_flow"
                  flow_chk = flow_chk + resv_flow(dsm2_network(i)%resv_conn_no)
            end if        
            if (abs(flow_chk) .gt. weakest_eps) write(11,*) dsm2_network(i)%dsm2_node_no, "*******"            
            write(11,*) dsm2_network(i)%dsm2_node_no, flow_chk
        end do        
    end subroutine
    
    
    !> calculate area for memory buffer
    subroutine get_area_for_buffer(hyd_area, hyd_ws, ncomp, buffer)
        use gtm_precision
        use common_variables, only: comp_pt
        use common_xsect
        implicit none
        integer, intent(in) :: ncomp                   !< number of computational points
        integer, intent(in) :: buffer                  !< size of memory buffer
        real(gtm_real), intent(in) :: hyd_ws(ncomp,buffer)    !< hydro water surface         
        real(gtm_real), intent(out) :: hyd_area(ncomp,buffer) !< hydro calculated area
        real(gtm_real) :: x
        integer :: branch
        integer :: i, j
        do i = 1, ncomp
            branch = comp_pt(i)%chan_no
            x = comp_pt(i)%distance        
            do j = 1, buffer                
                call CxArea(hyd_area(i,j), x, hyd_ws(i,j), branch)
            end do
        end do
        return
    end subroutine
      
end module    