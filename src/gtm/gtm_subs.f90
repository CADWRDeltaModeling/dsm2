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
! subroutines to support gtm main driver
!>@ingroup gtm_driver
module gtm_subs

    use gtm_precision
    
    contains
    
    !> get the properties for top survey elevation
    subroutine get_survey_top(wet_perim,   &
                              elevation,   &
                              ncell)
        use common_variables, only: chan_geom, n_chan, dx_arr
        use common_xsect
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(out) :: wet_perim(ncell)
        real(gtm_real), intent(out) :: elevation(ncell)
        integer :: ichan, icell
        integer :: chan_no, elev_no
        real(gtm_real) :: area, width, depth, X
        
        do ichan = 1, n_chan
            chan_no = chan_geom(ichan)%chan_no
            elev_no = virt_xsect(xsect_index(chan_no))%num_elev-1
            do icell = chan_geom(chan_no)%start_cell, chan_geom(chan_no)%end_cell
                X = half*dx_arr(icell) + dx_arr(icell)*(icell-chan_geom(chan_no)%start_cell)
                elevation(icell) = virt_xsect(xsect_index(chan_no))%elevation(elev_no)
                call CxInfo(area, width, wet_perim(icell), depth, X, elevation(icell), chan_no)
            end do   
        end do
        return
    end subroutine       

    !> print out final stage in a text file as a initial file format
    subroutine print_last_stage(cdtdate,        &
                                intdate,        &
                                out_conc,       &
                                out_conc_resv,  &
                                ncell,          &
                                nresv,          &
                                nvar)                                
        use common_variables, only : constituents, resv_geom    
        implicit none
        character(len=14), intent(in) :: cdtdate
        integer, intent(in) :: intdate
        integer, intent(in) :: ncell
        integer, intent(in) :: nresv
        integer, intent(in) :: nvar
        real(gtm_real) :: out_conc(ncell,nvar)
        real(gtm_real) :: out_conc_resv(nresv,nvar)
        integer :: ncol
        integer :: a(nvar)
        character*16 :: c(nvar)
        integer :: i, j        
        ncol = 0
        a = 0
        c = ''
        do i = 1, nvar
            if (constituents(i)%simulate) then
                ncol = ncol + 1
                a(ncol) = i
                c(ncol) = constituents(i)%name
            end if
        end do             
        open(801,file="init.txt")
        write(801,*) cdtdate, "/time"
        write(801,*) intdate, "/julmin"
        write(801,*) ncol, "/n_column"
        write(801,*) ncell, "/n_cell"
        write(801,'(a32,<ncol>a32)') "cell_no", (c(j),j=1,ncol)
        do i = 1, ncell
            write(801,'(i32,<ncol>f32.16)') i, (out_conc(i,a(j)),j=1,ncol) 
        end do
        write(801,*) nresv, "/n_resv"
        write(801,'(a32,<ncol>a32)') "reservoir_name", (c(j),j=1,ncol)
        do i = 1, nresv
            write(801,'(a32,<ncol>f32.16)') resv_geom(i)%name, (out_conc_resv(i,a(j)),j=1,ncol) 
        end do
        close(801)
        return
    end subroutine    

    !> Read initial values
    !> This will update variables n_var and constituents if detecting extra constituents
    !> in initial condition file.
    subroutine read_init_values(init_c,            &
                                init_r,            &
                                init_conc,         &
                                file_exists,       &
                                restart_file_name)
        use common_variables, only: n_var, n_cell, n_resv, constituents, constituents_tmp
        use read_init
        implicit none
        real(gtm_real), allocatable, intent(out) :: init_c(:,:)
        real(gtm_real), allocatable, intent(out) :: init_r(:,:)
        real(gtm_real), intent(in) :: init_conc
        logical, intent(in) :: file_exists
        character(len=:), allocatable, intent(in) :: restart_file_name
        character(len=32) :: name(10)
        integer :: nadd, i     
        
        if (file_exists==.true.) then 
            call check_init_file(nadd, name, restart_file_name)
            if (nadd .gt. 0) n_var = n_var + nadd
            allocate(constituents(n_var))
            allocate(init_c(n_cell,n_var))
            allocate(init_r(n_resv,n_var))               
            do i = 1, nadd
                constituents(i)%conc_no = i
                constituents(i)%name = name(i)
            enddo
            constituents(nadd+1:n_var) = constituents_tmp(1:n_var-nadd)             
            call read_init_file(init_c, init_r, restart_file_name)
        else
            allocate(constituents(n_var))
            allocate(init_c(n_cell,n_var))
            allocate(init_r(n_resv,n_var))             
            constituents = constituents_tmp
            if (init_conc .ne. LARGEREAL) then
                init_c = init_conc
                init_r = init_conc
            else    
                init_c = zero
                init_r = zero            
            end if                         
        endif 
        deallocate(constituents_tmp)
        return
    end subroutine
    

    !> subroutine to get output time series for selected output points
    !> This will update pathoutput%out_cell, calc_option, x_from_lo_face
    subroutine get_output_channel  
        use common_dsm2_vars, only: noutpaths, pathoutput
        use common_variables, only: resv_geom, n_resv
        implicit none       
        integer :: out_cell(noutpaths)              !< output cells
        integer :: calc_option(noutpaths)           !< calculation option of interpolation by using u/s cell or d/s cell
        integer :: chan_num(noutpaths)              !< channcel number
        real(gtm_real) :: x_from_lo_face(noutpaths) !< distance from lo face of the cell
        real(gtm_real) :: x_dist(noutpaths)        
        integer :: i, j
        do i = 1, noutpaths
            chan_num(i) = pathoutput(i)%no
            x_dist(i) = dble(pathoutput(i)%distance)
            if (pathoutput(i)%obj_type.eq.2) then
                do j = 1, n_resv
                    if (trim(resv_geom(j)%name).eq.trim(pathoutput(i)%name)) then
                        pathoutput(i)%no = resv_geom(j)%resv_no
                    end if
                end do
            end if
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
                                out_cell(i) = segm(k)%start_cell_no +  &
                                              int((x_dist(i)-segm(k)%up_distance)/(segm(k)%length/segm(k)%nx))
                                x_from_lo_face(i) = x_dist(i)-segm(k)%up_distance - (segm(k)%length/segm(k)%nx)*     &
                                                    int((x_dist(i)-segm(k)%up_distance)/(segm(k)%length/segm(k)%nx))
                                if ((cell(out_cell(i))%up_cell.le.0) .or.                     &
                                    (x_from_lo_face(i).ge.half*cell(out_cell(i))%dx) .and.    &
                                    (cell(out_cell(i))%down_cell.gt.0) ) then
                                    calc_option(i) = 1
                                elseif ((cell(out_cell(i))%down_cell.le.0) .or.                &
                                        (x_from_lo_face(i).lt.half*cell(out_cell(i))%dx) .and. &
                                        (cell(out_cell(i))%up_cell.gt.0) ) then
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


    !> assign i_var to outpath
    subroutine assign_ivar_to_outpath()
        use common_variables, only: n_var, constituents, n_resv, resv_geom
        use common_dsm2_vars, only: noutpaths, pathoutput  
        implicit none
        integer :: i, j, k
        do i = 1, noutpaths
            do j = 1, n_var
                if (trim(pathoutput(i).c_part).eq.trim(constituents(j)%name))then
                    pathoutput(i)%i_var = constituents(j)%conc_no
                end if
                if (pathoutput(i).obj_type.eq.2) then
                    do k = 1, n_resv
                        call locase(resv_geom(k)%name)
                        if (trim(resv_geom(k)%name).eq.trim(pathoutput(i)%b_part)) then
                             pathoutput(i)%no = resv_geom(k)%resv_no
                        end if
                    end do        
                end if                
            end do
        end do                
        return
    end subroutine            


    !> get output channel values
    subroutine get_output_channel_vals(vals,           &
                                       conc,           &
                                       ncell,          &
                                       conc_resv,      &
                                       nresv,          &
                                       nvar)
                                       
        use common_variables, only: cell
        use common_dsm2_vars, only: noutpaths, pathoutput        
        implicit none
        integer, intent(in) :: nvar                            !< number of constituents
        integer, intent(in) :: ncell                           !< number of cells
        integer, intent(in) :: nresv                           !< number of reservoirs        
        real(gtm_real), intent(in) :: conc(ncell, nvar)        !< concentration
        real(gtm_real), intent(in) :: conc_resv(nresv, nvar)   !< concentration        
        real(gtm_real), intent(out) :: vals(noutpaths)         !< output requested values
        integer :: i, icell, down_cell, up_cell
        
        do i = 1, noutpaths
            vals(i) = zero
            if (pathoutput(i)%obj_type.eq.1) then   !channel
                icell = pathoutput(i)%out_chan_cell
                if (pathoutput(i)%calc_option.eq.1) then           ! calculate the slope by icell and downstream cell
                    down_cell = cell(icell)%down_cell
                    vals(i) = conc(icell,pathoutput(i)%i_var)+                                       &
                             (conc(down_cell,pathoutput(i)%i_var)-conc(icell,pathoutput(i)%i_var))*  &
                             (pathoutput(i)%x_from_lo_face-half*cell(icell)%dx)/cell(icell)%dx
                elseif (pathoutput(i)%calc_option.eq.2) then       ! calculate the slope by icell and upstream cell
                    up_cell = cell(icell)%up_cell
                    vals(i) = conc(icell,pathoutput(i)%i_var)+                                       &
                             (conc(icell,pathoutput(i)%i_var)-conc(up_cell,pathoutput(i)%i_var))*    &
                             (pathoutput(i)%x_from_lo_face-half*cell(icell)%dx)/cell(icell)%dx               
                else                            
                    vals(i) = conc(icell,pathoutput(i)%i_var)
                end if
                if (vals(i) .le. zero) vals(i) = conc(icell,pathoutput(i)%i_var) ! to avoid extrapolation unstability
            elseif (pathoutput(i)%obj_type.eq.2) then !reservoir
                vals(i) = conc_resv(pathoutput(i)%no, pathoutput(i)%i_var)
            end if
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


    !> assign value to dsm2_network(:)%node_conc, pathinput(:)%i_no, pathinput(:)%i_var
    subroutine assign_node_ts()
        use common_variables, only : n_node, dsm2_network, dsm2_network_extra,     &
                                     n_var, constituents, qext, n_resv, resv_geom, &
                                     n_sediment, n_node_ts
        use common_dsm2_vars, only : n_inputpaths, pathinput, obj_reservoir
        implicit none
        integer :: i, j, k, st
        
        do i = 1, n_node_ts
            do j = 1, n_var
                call locase(pathinput(i)%variable)
                call locase(constituents(j)%name)
                if (trim(pathinput(i)%variable) .eq. trim(constituents(j)%name)) then
                    pathinput(i)%i_var = constituents(j)%conc_no
                end if
            end do
            if (pathinput(i)%obj_type.eq.obj_reservoir) then
                do j = 1,n_resv
                    if (trim(resv_geom(j)%name) .eq. trim(pathinput(i)%obj_name)) then
                        pathinput(i)%obj_no = resv_geom(j)%resv_no
                        pathinput(i)%i_no = resv_geom(j)%resv_no
                        do k = 1, resv_geom(j)%n_qext
                            if (trim(resv_geom(j)%qext_name(k)).eq.trim(pathinput(i)%name)) then
                                resv_geom(j)%qext_path(k,pathinput(i)%i_var) = i
                                if (trim(pathinput(i)%variable).eq.'ssc') then
                                    do st = 1, n_sediment
                                        resv_geom(j)%qext_path(k,n_var-n_sediment+st) = i
                                    end do                        
                                end if                                
                            end if    
                        end do                        
                    end if
                end do              
            else
                do j = 1, n_node 
                    if (pathinput(i)%obj_no .eq. dsm2_network(j)%dsm2_node_no) then
                        pathinput(i)%i_no = j
                    end if        
                end do
                do j = 1, dsm2_network_extra(pathinput(i)%i_no)%n_qext
                    if (trim(qext(dsm2_network_extra(pathinput(i)%i_no)%qext_no(j))%name) .eq. trim(pathinput(i)%name)) then
                        dsm2_network_extra(pathinput(i)%i_no)%qext_path(j,pathinput(i)%i_var) = i
                        if (trim(pathinput(i)%variable).eq.'ssc') then
                            do st = 1, n_sediment
                                dsm2_network_extra(pathinput(i)%i_no)%qext_path(j,n_var-n_sediment+st) = i
                            end do                        
                        end if
                    end if            
                end do
            end if
        end do
        return
    end subroutine        

    !> assign value to group_var for input time series
    subroutine assign_input_ts_group_var
        use common_variables
        use common_dsm2_vars, only: pathinput
        implicit none        
        integer :: ts_var_code
        integer :: i, j, k, m, p, ivar, c
        integer :: n_input_ts_type
        integer :: temp, ivar_temp, io
        integer :: chan_chk(n_chan), print_status
        character*100 :: error_out
        
        allocate(input_ts(n_input_ts))
        do i = 1, n_input_ts
            call ts_var_string_to_code(ts_var_code, pathinput(n_node_ts+i)%variable)
            input_ts(i)%input_ts_id = i
            input_ts(i)%ts_var_code = ts_var_code
            input_ts(i)%pathinput_id = n_node_ts+i  
            do j = 1, n_group
                if (trim(pathinput(n_node_ts+i)%name) .eq. trim(group(j)%name)) then          
                    input_ts(i)%group_id = group(j)%id
                end if
            end do    
            if (i.eq.1 .and. n_ts_var.eq.0)then
                n_ts_var = 1
                ts_code(n_ts_var) = ts_var_code
                ts_name(n_ts_var) = pathinput(n_node_ts+i)%variable
                code_to_ts_id(ts_var_code) = n_ts_var
            else
                do j = 1, n_ts_var
                    if (ts_var_code.eq.ts_code(j)) then
                        exit
                    else
                        if (j.eq.n_ts_var) then
                            n_ts_var = n_ts_var + 1
                            ts_code(n_ts_var) = ts_var_code
                            ts_name(n_ts_var) = pathinput(n_node_ts+i)%variable
                            code_to_ts_id(ts_var_code) = n_ts_var                        
                        end if
                    end if
                end do           
            end if 
        end do         
        
        allocate(ts(n_ts_var,n_cell))
        print_status = 0 
        do i = 1, n_ts_var
            chan_chk = 0
            do j = 1, n_input_ts                
                if (ts_code(i) .eq. input_ts(j)%ts_var_code) then
                    do k = 1, group(input_ts(j)%group_id)%n_members
                        if (group(input_ts(j)%group_id)%member_pattern_code(k) .eq. obj_channel) then
                            read(group(input_ts(j)%group_id)%member_name(k),'(i)',iostat=io) temp
                            do m = 1, n_chan
                                if (temp.eq. chan_geom(m)%channel_num) then
                                    chan_chk(m) = 1
                                    do p = chan_geom(m)%start_cell, chan_geom(m)%end_cell
                                        ts(i,p) = input_ts(j)%pathinput_id                                                 
                                    end do
                                end if
                            end do
                        elseif (group(i)%member_pattern_code(j) .eq. obj_reservoir) then
                    
                        else
                            write(*,*) "this is neither channel nor reservoir"
                        end if           
                    end do
                end if
            end do
            ! check if all the channels are assigned
            do m = 1, n_chan
                if (chan_chk(m) .eq. 0) then
                    print_status = 1
                    write(*,'(a24,i6,a21,a20)') "warning: Channel Number: ",chan_geom(m)%channel_num," is not assigned for ",ts_name(i)
                end if
            end do        
        end do
        if (print_status .eq. 1) call exit(-1)
        return
    end subroutine      

    !> Check sediment bed time series input
    subroutine check_sediment_bed_ts_input()
        use common_variables
        use error_handling
        implicit none
        if (code_to_ts_id(ts_var_temp) == 0) call gtm_fatal("Time series for Temperature is not specified!!!! It is needed for Sediment Bed Module.")
        return
    end subroutine
    
    !> check if the mercury related time series inputs are specified
    subroutine check_mercury_ts_input()
        use common_variables
        use error_handling
        implicit none
        if (code_to_ts_id(ts_var_do) == 0) call gtm_fatal("Time series for DO is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_ph) == 0) call gtm_fatal("Time series for PH is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_so4) == 0) call gtm_fatal("Time series for SO4 is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_temp) == 0) call gtm_fatal("Time series for Temperature is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_ipar) == 0) call gtm_fatal("Time series for IPAR is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_iuva) == 0) call gtm_fatal("Time series for IUVA is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_iuvb) == 0) call gtm_fatal("Time series for IUVB is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_rgm_air) == 0) call gtm_fatal("Time series for RGM_AIR is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_hg0_air) == 0) call gtm_fatal("Time series for Hg0_Air is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_mehg_air) == 0) call gtm_fatal("Time series for MeHg_Air is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_precip) == 0) call gtm_fatal("Time series for Precipitation is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_wet_hgii) == 0) call gtm_fatal("Time series for Wet_HgII is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_dry_hgii) == 0) call gtm_fatal("Time series for Dry_HgII is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_wet_mehg) == 0) call gtm_fatal("Time series for Wet_MeHg is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_dry_mehg) == 0) call gtm_fatal("Time series for Dry_MeHg is not specified!!!! It is needed for Mercury Module.")
        if (code_to_ts_id(ts_var_dgm_ratio) == 0) call gtm_fatal("Time series for DGM_Ratio is not specified!!!! It is needed for Mercury Module.")
        return
    end subroutine
    

    !> check the flow mass balance at DSM2 network
    subroutine flow_mass_balance_check(ncell,      &
                                       nqext,      &
                                       nresv_conn, &
                                       flow_lo,    &
                                       flow_hi,    &
                                       qext_flow,  &
                                       resv_flow) 
        use common_variables, only : n_node, dsm2_network, dsm2_network_extra
        implicit none
        integer, intent(in) :: ncell                        !< number of cells
        integer, intent(in) :: nqext                        !< number of external flows
        integer, intent(in) :: nresv_conn                   !< number of reservoir connections
        real(gtm_real), intent(in) :: flow_lo(ncell)        !< flow at lo face
        real(gtm_real), intent(in) :: flow_hi(ncell)        !< flow at hi face
        real(gtm_real), intent(in) :: qext_flow(nqext)      !< external flows
        real(gtm_real), intent(in) :: resv_flow(nresv_conn) !< reservoir flows
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
            do j = 1, dsm2_network_extra(i)%n_qext 
                 write(11,*) dsm2_network_extra(i)%dsm2_node_no, qext_flow(dsm2_network_extra(i)%qext_no(j)),"qext_flow"
                 flow_chk = flow_chk + qext_flow(dsm2_network_extra(i)%qext_no(j))
            end do
            if (dsm2_network_extra(i)%reservoir_no.ne.0) then 
                  write(11,*) dsm2_network_extra(i)%dsm2_node_no, resv_flow(dsm2_network_extra(i)%resv_conn_no),"resv_flow"
                  flow_chk = flow_chk + resv_flow(dsm2_network_extra(i)%resv_conn_no)
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
        integer, intent(in) :: ncomp                          !< number of computational points
        integer, intent(in) :: buffer                         !< size of memory buffer
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
    
    !> print out erosion flux and deposition flux 
    subroutine print_erosion_deposition(erosion,           &
                                        deposition,        &
                                        time,              &
                                        nvar,              &
                                        ncell)
        implicit none
        integer, intent(in) :: nvar
        integer, intent(in) :: ncell
        character*14, intent(in) :: time
        real(gtm_real), intent(in) :: erosion(ncell,nvar)
        real(gtm_real), intent(in) :: deposition(ncell,nvar)
        character*4 :: out_name(16)
        integer :: out_cell(16)
        integer :: i, j
        real(gtm_real) :: unit_convert
        unit_convert = 0.3048d0*0.3048d0*1000000.0d0   ! kg/m^2--> mg/ft^2
        out_name = ['CCH','DWS','GEO','JPT','LIB','LPS','MID','MIN','MLD','MOK','NFM','OLD','RIO','SFM','STK','UCS']
        out_cell = [ 2285, 2272, 1913,  490, 2369, 1630,  737, 2121, 2607, 1807, 1862,  555, 2516, 1712,  158, 2351]
        write(802,'(a14,32(a1,f12.9))') time,(',',erosion(out_cell(i),1)*unit_convert,            &
                                            ',',deposition(out_cell(i),1)*unit_convert,i=1,16)
        write(803,'(a14,32(a1,f12.9))') time,(',',erosion(out_cell(i),2)*unit_convert,            &
                                            ',',deposition(out_cell(i),2)*unit_convert,i=1,16)
        return
    end subroutine    
      
end module    