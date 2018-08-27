module sb_subs
    !use gtm_precision
    use sb_common
    !use gtm_subs, only: get_survey_top
    !use common_variables, only: chan_geom, segm
    use hdf_util !, only: get_int_attribute_from_hdf5, n_chan, n_xsect, n_comp
    use common_xsect
    implicit none
    contains
    
    subroutine get_command_args_sb(init_input_file)
        !args
        character init_input_file*(*) 
        !local
        character*150 :: cla
        logical :: exst
        integer :: stat
        
        if (iargc().NE.3) then
            write(*,*) "Incorrect number of command line arguments"
            write(*,*) "    arg 1 - hydro tidefile"
            write(*,*) "    arg 2 - gtm_dx (approx. cell length ft)"
            write(*,*) "    arg 3 - n_zone (no. of sediment zones)"
            call exit(1)
        end if
        call getarg(1,cla)
        inquire (file=CLA, exist=exst)
        if (exst) then
            init_input_file=Trim(cla)
            print *, 'Hydro tidefile found - ' , Trim(cla)
        else
            write(*,*) "File not found - " // cla
            call exit(1)
        end if
        
        call getarg(2,cla)
       
        read( cla, *, iostat = stat )  gtm_dx
        if (stat.ne.0) then
            write(*,*) 'invalid argument for gtm_dx'
            call exit(1)
        else
            print *, 'gtm_dx = ' , gtm_dx !trim(cla)
        end if
        call getarg(3,cla)
        read( cla, * , iostat = stat)  n_zone
        if (stat.ne.0) then
            write(*,*) 'invalid argument for n_zone'
            call exit(1)
        else
            print *, 'n_zone = ' , n_zone
        end if
        
        
    end subroutine get_command_args_sb
    
    subroutine assign_cells()
        integer :: ii
        integer :: jj
        integer :: kk
        integer :: cell_no
        integer :: cell_count       !for checking
        real(gtm_real) :: length
        real(gtm_real) :: width
        real(gtm_real) :: depth
        real(gtm_real) :: area
        real(gtm_real) :: X
        real(gtm_real) :: midpt
        !real(gtm_real), allocatable, dimension(:) :: max_elev
        !real(gtm_real), allocatable, dimension(:) :: min_elev
        real(gtm_real) :: dy_elev
        real(gtm_real) :: elev
        integer        :: mrk
        real(gtm_real) :: temp
        cell_count = 0
        !allocate (top_wet_p(n_cell))
       ! allocate (top_elev(n_cell))
        !allocate (max_elev(n_cell))
        !allocate (min_elev(n_cell))
        
        write(*,*) "processing sediment bed zones"
        
        call get_survey_top(top_wet_p, top_elev, n_cell)
        
        do ii = 1, n_segm
            length =   segm(ii)%length/segm(ii)%nx
            cell_no = segm(ii)%start_cell_no - 1
            
            do jj = 1, segm(ii)%nx
                cell_count = cell_count + 1
                cell_no = cell_no + 1
                sedcell(cell_no)%cell_no = cell_no
                sedcell(cell_no)%chan_no = segm(ii)%chan_no
                sedcell(cell_no)%up_comp_no = segm(ii)%up_comppt
                sedcell(cell_no)%down_comp_no = segm(ii)%down_comppt
                sedcell(cell_no)%length = length
                if (jj == 1) then
                    sedcell(cell_no)%up = segm(ii)%up_distance
                else
                    sedcell(cell_no)%up = sedcell(cell_no-1)%down
                end if
                if (jj == segm(ii)%nx) then
                    sedcell(cell_no)%down = segm(ii)%down_distance
                else
                    sedcell(cell_no)%down = sedcell(cell_no)%up + sedcell(cell_no)%length
                end if
                
                midpt = (sedcell(cell_no)%up + sedcell(cell_no)%down)/two
                                                            !interpolate
                sedcell(cell_no)%max_elev = elevation(sedcell(cell_no)%up_comp_no)%max +  (midpt - segm(ii)%up_distance)*( elevation(sedcell(cell_no)%down_comp_no)%max -  elevation(sedcell(cell_no)%up_comp_no)%max)/(segm(ii)%down_distance - segm(ii)%up_distance)
                sedcell(cell_no)%min_elev = elevation(sedcell(cell_no)%up_comp_no)%min +  (midpt - segm(ii)%up_distance)*( elevation(sedcell(cell_no)%down_comp_no)%min -  elevation(sedcell(cell_no)%up_comp_no)%min)/(segm(ii)%down_distance - segm(ii)%up_distance)
                
                X = half*dx_arr(cell_no) + dx_arr(cell_no)*(cell_no-chan_geom(segm(ii)%chan_no)%start_cell) !todo:check
                 
                    ! if (min_elev(ii) >= top_elev(ii)) then      !apparently the sediment bed is always inundated in some channels ???
                    !     call CxInfo(area, sedcell(cell_no)%width(1), sedcell(cell_no)%wet_p(1), depth, X, top_elev(cell_no), segm(ii)%chan_no)
                    !dy_elev = (top_elev(cell_no) - min_elev(cell_no))/(n_zone - 1)  !this had problems
                
                dy_elev = ( sedcell(cell_no)%max_elev - sedcell(cell_no)%min_elev)/(n_zone - 1)
                   
                if (cell_no.eq.1591) then
                    cell_no=1591  
                end if
                
                do kk = 1, n_zone
                    sedcell(cell_no)%elev(kk) = sedcell(cell_no)%min_elev + (kk-1)*dy_elev                     
                    call CxInfo(area, sedcell(cell_no)%width(kk), sedcell(cell_no)%wet_p(kk), depth, X, sedcell(cell_no)%elev(kk), segm(ii)%chan_no)
                    if (kk.gt.1) then
                        if (sedcell(cell_no)%wet_p(kk)<= sedcell(cell_no)%wet_p(kk-1)) then
                            write (*,'(A28,1x,I4,2x,A5,1x,I1)') 'cell wetted perimeter error:', cell_no,'zone:',kk
                            temp = sedcell(cell_no)%wet_p(kk-1)
                            sedcell(cell_no)%wet_p(kk-1) = sedcell(cell_no)%wet_p(kk)
                            sedcell(cell_no)%wet_p(kk) = temp       !todo: discuss with En_Ching
                        endif
                    end if
                    !sedcell(cell_no)%wet_p(kk) = area/sedcell(cell_no)%wet_p(kk)  !todo what was I trying to do here dhh hydraulic radius ????????
                end do
            end do
        end do
       
                
               
        
        !deallocate (min_elev)
        !deallocate (max_elev)
        !ii = segm(1)%segm_no
        return
    end subroutine

    subroutine get_survey_top(wet_perim,   &
                              elev,   &
                              ncell)
        use common_variables, only: chan_geom, n_chan, dx_arr
        use common_xsect
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(out) :: wet_perim(ncell)
        real(gtm_real), intent(out) :: elev(ncell)
        integer :: ichan, icell
        integer :: chan_no, elev_no
        real(gtm_real) :: area, width, depth, X
        
        do ichan = 1, n_chan
            chan_no = chan_geom(ichan)%chan_no
            elev_no = virt_xsect(xsect_index(chan_no))%num_elev-1
            do icell = chan_geom(chan_no)%start_cell, chan_geom(chan_no)%end_cell
                X = half*dx_arr(icell) + dx_arr(icell)*(icell-chan_geom(chan_no)%start_cell)
                elev(icell) = virt_xsect(xsect_index(chan_no))%elevation(elev_no)
                call CxInfo(area, width, wet_perim(icell), depth, X, elev(icell), chan_no)
            end do   
        end do
        return
    end subroutine 
                   
    subroutine fill_sed_hdf_array()
        integer:: ii
        integer:: jj
        integer:: count
        
        count = zero
        do ii=1, n_cell
            do jj = 1, n_zone
                count = count + 1
                sed_hdf(count)%cell = ii
                sed_hdf(count)%zone = jj
                sed_hdf(count)%elev = sedCell(ii)%elev(jj)
                sed_hdf(count)%cell_wet_p = sedCell(ii)%wet_p(jj)
                if (jj==1) then
                    sed_hdf(count)%zone_wet_p = sedCell(ii)%wet_p(jj)
                else
                    sed_hdf(count)%zone_wet_p = sedCell(ii)%wet_p(jj) - sedCell(ii)%wet_p(jj-1)
                end if
                sed_hdf(count)%width = sedCell(ii)%width(jj)
            end do
        end do
    
    
    end subroutine
    
end module sb_subs