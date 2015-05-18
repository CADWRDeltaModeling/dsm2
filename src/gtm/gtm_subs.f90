!<license>
!    Copyright (C) 2015 State of California,
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
!>@ingroup gtm_driver
module gtm_subs

    use gtm_precision
    
    contains

    !> subroutine to get output time series for selected output points
    !> This will update pathoutput%out_cell, calc_option, x_from_lo_face
    subroutine get_output_channel  
        use common_dsm2_vars, only: noutpaths, pathoutput
        implicit none       
        integer :: out_cell(noutpaths)    !< output cells
        integer :: calc_option(noutpaths) !< calculation option of interpolation by using u/s cell or d/s cell
        integer :: chan_num(noutpaths)                 !< channcel number
        real(gtm_real) :: x_from_lo_face(noutpaths)    !< distance from lo face of the cell
        real(gtm_real) :: x_dist(noutpaths)        
        integer :: i
        do i = 1, noutpaths
            chan_num(i) = pathoutput(i)%channo
            x_dist(i) = dble(pathoutput(i)%distance)
        enddo
        call get_select_cell_with_x(pathoutput(:)%out_chan_cell,  &
                                    pathoutput(:)%x_from_lo_face, &
                                    pathoutput(:)%calc_option,    &      
                                    noutpaths, chan_num, x_dist)
        return
    end subroutine
        
    !> subroutine to get the output cell information 
    !> (cell_id, distance from lo face of cell, and calculation option)
    !> cal1(slope of i and i+1): u/s boundary, d/s of junction, x close to hi face
    !> cal2(slope of i and i-1): d/s boundary, u/s of junction, x close to lo face    
    subroutine get_select_cell_with_x(out_cell,            &
                                      x_from_lo_face,      &
                                      calc_option,         &
                                      n_out_cell,          &
                                      chan_num,            &
                                      x_dist)              
        
        use common_variables, only: n_chan, n_segm, chan_geom, segm, cell
        
        implicit none            
        
        integer, intent(out) :: out_cell(n_out_cell)               !< output cells
        integer, intent(out) :: calc_option(n_out_cell)            !< calculation option
        integer, intent(in) :: n_out_cell                          !< number of output cells
        integer, intent(in) :: chan_num(n_out_cell)                !< channel number
        real(gtm_real), intent(out) :: x_from_lo_face(n_out_cell)  !< distance from lo face in that out cell
        real(gtm_real), intent(inout) :: x_dist(n_out_cell)        !< distance from upstream node read from inp file
        integer :: i, j, k, chan_no
        
        do i = 1, n_out_cell
            do j = 1, n_chan
                if (chan_num(i).eq.chan_geom(j)%channel_num) then 
                    chan_no = chan_geom(j)%chan_no
                    if (x_dist(i).eq.LARGEINT .or. x_dist(i).eq.LARGEREAL) then
                        x_dist(i) = dble(chan_geom(j)%channel_length)
                    end if 
                    do k = 1, n_segm
                        if (segm(k)%chan_no .eq. chan_no) then
                            if ((x_dist(i).ge.segm(k)%up_distance).and.(x_dist(i).lt.segm(k)%down_distance)) then
                                out_cell(i) = segm(k)%start_cell_no + int((x_dist(i)-segm(k)%up_distance)/(segm(k)%length/segm(k)%nx))
                                x_from_lo_face(i) = x_dist(i)-segm(k)%up_distance - (segm(k)%length/segm(k)%nx)*     &
                                                    int((x_dist(i)-segm(k)%up_distance)/(segm(k)%length/segm(k)%nx))
                                if ((cell(out_cell(i))%up_cell.le.0) .or. (x_from_lo_face(i).ge.half*cell(out_cell(i))%dx) &
                                     .and. (cell(out_cell(i))%down_cell.gt.0) ) then
                                    calc_option(i) = 1
                                elseif ((cell(out_cell(i))%down_cell.le.0) .or. (x_from_lo_face(i).lt.half*cell(out_cell(i))%dx) &
                                     .and. (cell(out_cell(i))%up_cell.gt.0) ) then
                                    calc_option(i) = 2
                                else
                                    calc_option(i) = 0
                                end if    
                                goto 10    
                            end if     
                            if (x_dist(i).eq.segm(k)%down_distance) then
                                out_cell(i) = segm(k)%start_cell_no + segm(k)%nx - 1
                                x_from_lo_face(i) = segm(k)%length/segm(k)%nx
                                if (cell(out_cell(i))%down_cell.gt.0) then 
                                    calc_option(i) = 1
                                else
                                    calc_option(i) = 2
                                end if        
                                goto 10    
                            end if                                                  
                        end if
                    end do
                end if
            end do                
10      enddo
        return
    end subroutine
    

    !> subroutine to print out time series for selected cells into a text file
    subroutine get_select_cell(out_cell,            &
                               n_out_cell,          &
                               chan_num,            &
                               x_dist)               
        
        use common_variables, only: n_chan, n_segm, chan_geom, segm
        
        implicit none            
        
        integer, intent(out) :: out_cell(n_out_cell)        !< output cells
        integer, intent(in) :: n_out_cell                   !< number of output cells
        integer, intent(in) :: chan_num(n_out_cell)         !< channel no
        real(gtm_real), intent(inout) :: x_dist(n_out_cell) !< distance from upstream node read from inp file
        integer :: i, j, k, chan_no
        
        do i = 1, n_out_cell
            do j = 1, n_chan
                if (chan_num(i).eq.chan_geom(j)%channel_num) then 
                    chan_no = chan_geom(j)%chan_no
                    if (x_dist(i).eq.LARGEINT .or. x_dist(i).eq.LARGEREAL) then
                        x_dist(i) = dble(chan_geom(j)%channel_length)
                    end if 
                    do k = 1, n_segm
                        if (segm(k)%chan_no .eq. chan_no) then
                            if ((x_dist(i).ge.segm(k)%up_distance).and.(x_dist(i).lt.segm(k)%down_distance)) then
                                out_cell(i) = segm(k)%start_cell_no + &
                                              int((x_dist(i)-segm(k)%up_distance)/(segm(k)%length/segm(k)%nx))
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


    !> get output channel values
    subroutine get_output_channel_vals(vals,           &
                                       conc,           &
                                       ncell,          &
                                       nvar)
                                       
        use common_variables, only: cell
        use common_dsm2_vars, only: noutpaths, pathoutput        
        
        implicit none
        
        integer, intent(in) :: nvar                            !< number of constituents
        integer, intent(in) :: ncell                           !< number of cells
        real(gtm_real), intent(in) :: conc(ncell, nvar)        !< concentration
        real(gtm_real), intent(out) :: vals(noutpaths,nvar)     !< output requested values
        integer :: i, icell, down_cell, up_cell
        
        do i = 1, noutpaths
            vals(i,:) = zero
            icell = pathoutput(i)%out_chan_cell
            if (pathoutput(i)%calc_option.eq.1) then       ! calculate the slope by icell and downstream cell
                down_cell = cell(icell)%down_cell
                vals(i,:) = conc(icell,:)+(conc(down_cell,:)-conc(icell,:))*      &
                            (pathoutput(i)%x_from_lo_face-half*cell(icell)%dx)/cell(icell)%dx
            elseif (pathoutput(i)%calc_option.eq.2) then   ! calculate the slope by icell and upstream cell
                up_cell = cell(icell)%up_cell
                vals(i,:) = conc(icell,:)+(conc(icell,:)-conc(up_cell,:))*        &
                            (pathoutput(i)%x_from_lo_face-half*cell(icell)%dx)/cell(icell)%dx               
            else                            
                vals(i,:) = conc(icell,:)
            end if
            where (vals(i,:) .le. zero) vals(i,:) = conc(icell,:)
        end do
        return
    end subroutine                 
                     
    
    !> Write concentration for output cells to a text file
    subroutine print_out_cell_conc(file_id,   &
                                   c,         &
                                   nc,        &
                                   out_cell,  &
                                   nout)
        implicit none
        integer, intent(in) :: file_id           !< text file id
        integer, intent(in) :: nc                !< concentration
        integer, intent(in) :: nout              !< number of output cells
        real(gtm_real), intent(in) :: c(nc)      !< number of cells
        integer, intent(in) :: out_cell(nout)    !< output cell no
        integer :: i
        write(file_id,'(<nout>f10.0)') (c(out_cell(i)),i=1,nout)    
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