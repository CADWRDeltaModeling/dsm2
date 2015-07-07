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
!> Main program for General Transport Model
!>@ingroup gtm_driver
program gtm
    
    use gtm_precision
    use error_handling
    use gtm_logging
    use common_variables
    use common_xsect   
    use common_dsm2_vars
    use common_dsm2_qual   
    use process_gtm_input
    use io_utilities  
    use time_utilities
    use read_init
    use gtm_hdf_write    
    use gtm_hdf_ts_write    
    use gtm_dss
    use gtm_dss_open
    use gtm_dss_main      
    use hydro_data_tidefile
    use interpolation
    use gtm_network 
    use dsm2_gtm_network
    use hydro_data
    use hydro_data_network
    use state_variables
    use state_variables_network
    use primitive_variable_conversion
    use gradient_adjust
    use boundary_concentration
    use advection
    use diffusion
    use advection_network
    use diffusion_network
    use dispersion_coefficient    
    use source_sink
    use boundary_advection 
    use boundary_diffusion       
    use gtm_subs
    use dsm2_time_utils, only: incr_intvl
    use boundary_diffusion_network
    use gtm_init_store_outputs
    use gtm_store_outpath
    use klu

    implicit none
    
    real(gtm_real), allocatable :: cfl(:)
    real(gtm_real), allocatable :: disp_coef_lo(:)           !< Low side constituent dispersion coef. at new time
    real(gtm_real), allocatable :: disp_coef_hi(:)           !< High side constituent dispersion coef. at new time
    real(gtm_real), allocatable :: disp_coef_lo_prev(:)      !< Low side constituent dispersion coef. at old time
    real(gtm_real), allocatable :: disp_coef_hi_prev(:)      !< High side constituent dispersion coef. at old time
    real(gtm_real), allocatable :: init_c(:,:)
    real(gtm_real), allocatable :: init_r(:,:)   
     
    real(gtm_real) :: theta = half                           !< Crank-Nicolson implicitness coeficient
    real(gtm_real) :: constant_dispersion
    integer :: nt    
    integer :: up_comp, down_comp    
    integer :: ok
    
    ! local variables to obtain time series data from HDF5 file
    integer :: time_offset
    integer :: i, j, ibuffer, start_buffer, icell
    integer :: iblock, slice_in_block, t_index
    integer :: time_step_int = 0
    real(gtm_real) :: t_in_slice
    real(gtm_real) :: time
    real(gtm_real) :: current_time
    real(gtm_real) :: new_current_time
    real(gtm_real) :: gtm_hdf_time_intvl
    real(gtm_real) :: time_in_slice
    integer :: offset, num_blocks, jday
    integer, allocatable :: memlen(:)
    integer :: runtime_hydro_start, runtime_hydro_end
    integer :: current_block = 0
    integer :: current_slice = 0
    integer :: time_index_in_gtm_hdf
    integer :: next_output_flush   ! next time to flush output
      
    procedure(hydro_data_if), pointer :: fill_hydro => null()   ! Hydrodynamic pointer to be filled by the driver
    logical, parameter :: limit_slope = .true.                ! Flag to switch on/off slope limiter  
    !real(gtm_real), parameter :: constant_decay = 5.0d-5
    real(gtm_real), parameter :: constant_decay = zero
    logical :: debug_interp = .false.    
    logical :: sub_time_step = .true.                         ! flag to turn on/off sub time stepping
    integer, parameter :: max_num_sub_ts = 20                 ! maximum number of sub time step within GTM time step    
    
    character(len=130) :: init_input_file                     ! initial input file on command line [optional]
    character(len=24) :: restart_file_name
    character(len=14) :: cdt
    character(len=9) :: prev_day

    ! variables for sub time stepping 
    real(gtm_real) :: sub_gtm_time_step                       ! sub time step for GTM when max CFL > 1
    real(gtm_real) :: max_cfl                                 ! max_cfl = maxval(cfl)
    integer :: sub_st                                         ! number of sub time step within gtm time step
    integer :: prev_sub_ts                                    ! previous number of sub time step (used to decide if deallocate and allocate is needed.)
    integer :: ceil_max_cfl                                   ! ceiling integer of max_cfl
    integer :: tmp_int
    integer :: ierror
    integer :: istat = 0
    
    ! local variables for input time series
    integer :: n_bound_ts
    integer, allocatable :: bound_index(:), path_index(:)
    
    real(gtm_real) :: flow_chk
    
    ! for specified output locations
    real(gtm_real), allocatable :: vals(:,:)
    
    integer :: st, k, n_st     ! temp index

    n_var = 1
    
    call h5open_f(ierror)
    call verify_error(ierror, "opening hdf interface")   
    
    !
    !----- Read input specification from *.inp text file -----
    !
    call get_command_args(init_input_file)
    call read_input_text(init_input_file)                  ! read input specification text
    call opendss(ifltab_in, n_dssfiles, indssfiles)        ! open all input dss files
    call opendss(ifltab_out, n_outdssfiles, outdssfiles)   ! open all output dss files    
    open(debug_unit, file = "gtm_debug_unit.txt")          ! debug output text file
    
    write(*,*) "Process DSM2 geometry info...."    
    call hdf5_init(hydro_hdf5)                         
    call dsm2_hdf_geom
    call write_geom_to_text

    call check_runtime(num_blocks, memlen,                     & 
                       memory_buffer, hydro_time_interval,     & 
                       hydro_start_jmin, hydro_end_jmin,       &
                       gtm_start_jmin, gtm_end_jmin)   
    
    allocate(vals(noutpaths,n_var))    
    call get_cell_info    
    call get_output_channel

    !
    !----- allocate array for interpolation -----     
    !
    nt = npartition_t + 1
    if (apply_diffusion) then
        if (disp_coeff.ne.LARGEREAL) then
            disp_arr = disp_coeff
        end if        
        dispersion_coef => assign_dispersion_coef
    end if      
    
    allocate(constituents(n_var))    
    call allocate_state_hydro(n_comp, n_resv, n_resv_conn, n_qext, n_tran, n_cell)
    call allocate_hydro_ts 
    call allocate_network_tmp(npartition_t)
    call allocate_state(n_cell, n_var)
    call allocate_state_network(n_resv, n_resv_conn, n_qext, n_tran, n_var)
    allocate(init_c(n_cell,n_var))
    allocate(init_r(n_resv,n_var))
    allocate(linear_decay(n_var))
    allocate(cfl(n_cell))    
    allocate(disp_coef_lo(n_cell), disp_coef_hi(n_cell))
    allocate(disp_coef_lo_prev(n_cell), disp_coef_hi_prev(n_cell))
    
    write(*,*) "You need to have ",n_cell," number of cells in initial file"
    
    linear_decay = constant_decay
    
    constituents(1)%conc_no = 1
    constituents(1)%name = "EC"
    call assign_node_ts
    allocate(node_conc(n_node, n_var))
    allocate(prev_node_conc(n_node, n_var))
    node_conc = LARGEREAL    
    prev_node_conc = LARGEREAL 

    call incr_intvl(tmp_int, 0,gtm_io(3,2)%interval,1)
    gtm_hdf_time_intvl = dble(tmp_int)
    call init_qual_hdf(qual_hdf,             &
                       gtm_io(3,2)%filename, &
                       n_cell,               &
                       n_resv,               &
                       n_var,                &
                       int(gtm_start_jmin),  &
                       int(gtm_end_jmin),    &
                       gtm_io(3,2)%interval)
    
    if (trim(gtm_io(3,2)%filename) .ne. "") then
        call write_input_to_hdf5(qual_hdf%file_id)
        call write_grid_to_tidefile(qual_hdf%file_id)
    end if
                       
    !
    !----- point to interface -----
    !
    fill_hydro => gtm_flow_area
    fill_hydro_network => gtm_network_data
    compute_source => no_source
    !compute_source => linear_decay_source 
    adjust_gradient => adjust_differences_network               ! adjust gradients for DSM2 network
    boundary_conc => assign_boundary_concentration              ! assign boundary concentration    
    advection_boundary_flux => bc_advection_flux_network        ! adjust flux for DSM2 network
    boundary_diffusion_flux => network_boundary_diffusive_flux
    boundary_diffusion_network_matrix => network_diffusion_sparse_matrix
    
    !boundary_diffusion_flux => neumann_zero_diffusive_flux
    !boundary_diffusion_matrix => neumann_zero_diffusion_matrix
    
    call set_dispersion_arr(disp_arr, n_cell)
    
    write(*,*) "Process time series...."
    write(debug_unit,"(16x,3000i8)") (i, i = 1, n_cell) 
 
    prev_day =  "01JAN1000"       ! to initialize for screen printing only

    restart_file_name = trim(gtm_io(1,1)%filename) 
    if (restart_file_name.eq.'') then  
        init_c = 200.d0
        init_r = 200.d0    
    else
        call read_init_file(init_c, init_r, restart_file_name, n_cell, n_resv, n_var)
    end if

    if (n_cell .gt. 0) then
        if (init_c(1,1) .ne. LARGEREAL) then
            conc = init_c
        else    
            conc = init_conc
        end if    
    end if
    if (n_resv .gt. 0) then
        if (init_r(1,1) .ne. LARGEREAL) then
            conc_resv = init_r
        else    
            conc_resv = init_conc
        end if      
    end if
    
    if (apply_diffusion)then
        call dispersion_coef(disp_coef_lo,         &
                             disp_coef_hi,         &
                             flow,                 &
                             flow_lo,              &
                             flow_hi,              &
                             area,                 &
                             area_lo,              &
                             area_hi,              &                             
                             gtm_start_jmin,       &
                             dx_arr,               &
                             gtm_time_interval,    &
                             n_cell,               &
                             n_var) 
        call network_diffusion_sparse_geom(n_cell)
        k_common = klu_fortran_init()
        k_symbolic = klu_fortran_analyze(n_cell, aap, aai, k_common)
        call klu_fortran_free_numeric(k_numeric, k_common)
        k_numeric = klu_fortran_factor(aap, aai, aax, k_symbolic, k_common)                                
    else
        disp_coef_lo = LARGEREAL
        disp_coef_hi = LARGEREAL            
    end if
    disp_coef_lo_prev = disp_coef_lo
    disp_coef_hi_prev = disp_coef_hi

    write(debug_unit,'(2a6,a10,11a24)') "cell","var","dx","grad","grad_lo","grad_hi","area", &
                     "mass_prev","flow_lo","flow_hi","conc_lo","conc_hi","flux_lo","flux_hi"
    
    prev_sub_ts = 1
    start_julmin = int(gtm_start_jmin)
    end_julmin = int(gtm_end_jmin)    
    call incr_intvl(next_output_flush, start_julmin, flush_intvl, TO_BOUNDARY)
    call gtm_init_store_outpaths(istat)
    
    do current_time = gtm_start_jmin, gtm_end_jmin, gtm_time_interval
        
        time_step_int = int((current_time-gtm_start_jmin)/gtm_time_interval) + 1 
         
        !---print out processing date on screen
        cdt = jmin2cdt(int(current_time))   ! this function only for screen status printout. 
        if (cdt(1:9) .ne. prev_day) then    ! Rough integer is fine.
            write(*,*) cdt(1:9)
            prev_day =  cdt(1:9)
        end if
                
        !---read hydro data from hydro tidefile
        call get_loc_in_hydro_buffer(iblock, slice_in_block, t_in_slice, current_time, hydro_start_jmin, &
                                     memory_buffer, hydro_time_interval, gtm_time_interval)

        if (iblock .ne. current_block) then ! check if need to read new block into buffer
            current_block = iblock
            current_slice = 0
            time_offset = memory_buffer*(iblock-1)
            call dsm2_hdf_ts(time_offset, memlen(iblock))
            call get_area_for_buffer(hydro_area, hydro_ws, n_comp, memory_buffer)
            if (prev_comp_flow(1)==LARGEREAL) then                             !if (slice_in_block .eq. 1) then
                call dsm2_hdf_slice(prev_comp_flow, prev_comp_ws, prev_hydro_resv, prev_hydro_resv_flow, &
                     prev_hydro_qext, prev_hydro_tran, n_comp, n_resv, n_resv_conn, n_qext, n_tran, time_offset-1)
            end if
        end if

        !--- interpolate flow and water surface between computational points
        if (slice_in_block .ne. current_slice) then  ! check if need to interpolate for new hydro time step
            n_st = npartition_t + 1
            if (prev_sub_ts .ne. 1) then
                call deallocate_network_tmp
                call allocate_network_tmp(npartition_t)
            end if   
            call interp_network(npartition_t, slice_in_block, n_comp, prev_comp_flow, prev_comp_ws, n_cell, prev_flow_cell_lo, prev_flow_cell_hi) 
            call interp_network_ext(npartition_t, slice_in_block, prev_hydro_resv, prev_hydro_resv_flow, &
                                    prev_hydro_qext, prev_hydro_tran)              
            ! to determine if sub time step is required based on CFL number
            call fill_hydro(flow,          &
                            flow_lo,       &
                            flow_hi,       &
                            area,          &
                            area_lo,       &
                            area_hi,       &
                            n_cell,        &
                            dble(1),       &
                            dx_arr,        &
                            gtm_time_interval)
            cfl = flow/area*(gtm_time_interval*sixty)/dx_arr
            max_cfl = maxval(cfl)
            ceil_max_cfl = ceiling(max_cfl) 
            if ((max_cfl .gt. one).and.(sub_time_step)) then
                if (ceil_max_cfl .gt. max_num_sub_ts) then   ! try to avoid exceeding max_num_sub_ts
                    ceil_max_cfl = max_num_sub_ts                    
                end if
                sub_gtm_time_step = gtm_time_interval/dble(ceil_max_cfl)
                write(*,'(f15.0,f5.2,i5,f10.4)') current_time, max_cfl, ceil_max_cfl, sub_gtm_time_step                             
                call deallocate_network_tmp
                call allocate_network_tmp(npartition_t*ceil_max_cfl)         
                n_st = npartition_t*ceil_max_cfl + 1
                call interp_network(npartition_t*ceil_max_cfl, slice_in_block, n_comp, prev_comp_flow, prev_comp_ws, n_cell, prev_flow_cell_lo, prev_flow_cell_hi) 
                call interp_network_ext(npartition_t*ceil_max_cfl, slice_in_block, prev_hydro_resv,    &
                                        prev_hydro_resv_flow, prev_hydro_qext, prev_hydro_tran)                                      
                prev_sub_ts = ceil_max_cfl
            end if                                     
            prev_comp_flow(:) = hydro_flow(:,slice_in_block)  ! keep track of prev_* to avoid index error at t_index=1
            prev_comp_ws(:) = hydro_ws(:,slice_in_block)      
            prev_hydro_resv(:) = hydro_resv_height(:,slice_in_block)
            prev_hydro_resv_flow(:) = hydro_resv_flow(:,slice_in_block)
            prev_hydro_qext(:) = hydro_qext_flow(:,slice_in_block)
            prev_hydro_tran(:) = hydro_tran_flow(:,slice_in_block)
            prev_flow_cell_lo(:) = flow_mesh_lo(n_st,:)
            prev_flow_cell_hi(:) = flow_mesh_hi(n_st,:)            
            current_slice = slice_in_block            
        end if
        
        if (sub_time_step) then
            sub_st = ceil_max_cfl
            sub_gtm_time_step = gtm_time_interval/dble(ceil_max_cfl)
        else
            sub_st = 1
            sub_gtm_time_step = gtm_time_interval
        end if
                                  
        do st = 1, sub_st
            t_index = int(t_in_slice/sub_gtm_time_step) + st
            new_current_time = current_time + dble(st-1)*sub_gtm_time_step
            
            !---get time series for boundary conditions, this will update pathinput(:)%value; 
            !---rounded integer is fine since DSS does not take care of precision finer than 1minute anyway.
            call get_inp_value(int(new_current_time),int(new_current_time-sub_gtm_time_step))
            do i = 1, n_inputpaths
                node_conc(pathinput(i)%i_node, pathinput(i)%i_var) = pathinput(i)%value   
            end do   
                        
            call fill_hydro(flow,          &
                            flow_lo,       &
                            flow_hi,       &
                            area,          &
                            area_lo,       &
                            area_hi,       &
                            n_cell,        &
                            dble(t_index), &
                            dx_arr,        &
                            sub_gtm_time_step)  
        
            call fill_hydro_network(resv_height,  &
                                    resv_flow,    &
                                    qext_flow,    &
                                    tran_flow,    &
                                    n_resv,       &
                                    n_resv_conn,  & 
                                    n_qext,       &
                                    n_tran,       &
                                    dble(t_index))                               
      
            !if (t_index.eq.1) then
            !    call flow_mass_balance_check(n_cell, n_qext, n_resv_conn, flow_lo, flow_hi, qext_flow, resv_flow) 
            !end if    
            ! assign initial hydro data                           
            if ((new_current_time .eq. gtm_start_jmin).or.(area_prev(1) .eq. LARGEREAL)) then
                call prim2cons(mass_prev, conc, area, n_cell, n_var)
                area_prev = area
                area_lo_prev = area_lo
                area_hi_prev = area_hi          
                prev_resv_height = resv_height
                prev_resv_flow = resv_flow
                prev_qext_flow = qext_flow
                prev_tran_flow = tran_flow
                prev_conc_resv = conc_resv
            end if
           
            !----- advection and source/sink -----        
            call advect_network(mass,                         &
                                mass_prev,                    &  
                                flow,                         &
                                flow_lo,                      &
                                flow_hi,                      &
                                area,                         &
                                area_prev,                    & 
                                area_lo,                      &
                                area_hi,                      &
                                n_cell,                       &
                                n_var,                        &
                                dble(new_current_time)*sixty, &
                                sub_gtm_time_step*sixty,      &
                                dx_arr,                       &
                                limit_slope)   
            where (mass.lt.zero) mass = zero                               
            call cons2prim(conc, mass, area, n_cell, n_var)          
            conc_prev = conc
            prev_conc_resv = conc_resv
            !write(108,*) new_current_time
            !write(108,'(16x,6a10)') "3591","3561","3570","2400","2413","2414"
            !write(108,'(a10,i6,6f10.2)') "FLOW",time_step_int,flow(2591),flow(3561),flow_hi(3570),flow_lo(2400),flow_lo(2413),flow_hi(2414)   ! to check junction #381
            !write(108,'(a10,i6,6f10.2)') "AREA",time_step_int,area(2897),area(2899),area(2900),area(2909),area(2989),area(2996),area(3007)               ! to check junction #381
            !write(108,'(a10,i6,6f10.2)') "VEL",time_step_int,flow(2897)/area(2897),flow(2899)/area(2899),flow(2900)/area(2900),flow(2899)/area(2899),flow(2900)/area(2900),flow(2909)/area(2909)               ! to check junction #381            
            !write(108,'(a10,i6,6f10.3)') "ADVECT",time_step_int,conc(3591,1),conc(3561,1),conc(3570,1),conc(2400,1),conc(2413,1),conc(2414,1)  ! to check junction #381
            !--------- Diffusion ----------
            if (apply_diffusion) then
                call dispersion_coef(disp_coef_lo,         &
                                     disp_coef_hi,         &
                                     flow,                 &
                                     flow_lo,              &
                                     flow_hi,              &
                                     area,                 &
                                     area_lo,              &
                                     area_hi,              &                             
                                     gtm_start_jmin,       &
                                     dx_arr,               &
                                     gtm_time_interval,    &
                                     n_cell,               &
                                     n_var)             
                call diffuse_network(conc,                         &
                                     conc_prev,                    &
                                     area,                         &
                                     area_prev,                    &
                                     area_lo,                      &
                                     area_hi,                      &
                                     area_lo_prev,                 &
                                     area_hi_prev,                 &
                                     disp_coef_lo,                 &  
                                     disp_coef_hi,                 &
                                     disp_coef_lo_prev,            &  
                                     disp_coef_hi_prev,            &
                                     n_cell,                       &
                                     n_var,                        &
                                     dble(new_current_time)*sixty, &
                                     theta,                        &
                                     sub_gtm_time_step*sixty,      &
                                     dx_arr)
            end if 
            !write(108,'(a10,i6,6f10.3)') "DISP",time_step_int,conc(3591,1),conc(3561,1),conc(3570,1),conc(2400,1),conc(2413,1),conc(2414,1) ! to chcek junction #391
                        
            call prim2cons(mass,conc,area,n_cell,n_var)
            mass_prev = mass
            conc_prev = conc
            prev_conc_resv = conc_resv
            area_prev = area
            area_lo_prev = area_lo
            area_hi_prev = area_hi    
            disp_coef_lo_prev = disp_coef_lo
            disp_coef_hi_prev = disp_coef_hi 
            prev_resv_height = resv_height
            prev_resv_flow = resv_flow
            prev_qext_flow = qext_flow
            prev_tran_flow = tran_flow            
            prev_node_conc = node_conc
            node_conc = LARGEREAL 
        end do ! end of loop of sub time step

        !---- print output to DSS file -----
        call get_output_channel_vals(vals, conc, n_cell, n_var)
        if (int(current_time) .ge. next_output_flush .or. current_time.eq.gtm_end_jmin) then
            call incr_intvl(next_output_flush, next_output_flush, flush_intvl,TO_BOUNDARY)
            call gtm_store_outpaths(.true.,int(current_time),int(gtm_time_interval), vals)
        else
            call gtm_store_outpaths(.false.,int(current_time),int(gtm_time_interval), vals)
        endif
        
        !if (time_step_int.eq.442) then
        !   apply_diffusion = .false.
        !end if
        !
        !----- print output to hdf5 file -----
        !                     
        if (mod(current_time-gtm_start_jmin,gtm_hdf_time_intvl)==zero) then
            time_index_in_gtm_hdf = (current_time-gtm_start_jmin)/gtm_hdf_time_intvl
            call write_qual_hdf(qual_hdf,                     &
                                conc,                         &
                                n_cell,                       &
                                n_var,                        &
                                time_index_in_gtm_hdf)     
            if (n_resv .gt. 0) then
                call write_qual_hdf_resv(qual_hdf,            & 
                                         conc_resv,           & 
                                         n_resv,              &
                                         n_var,               &  
                                         time_index_in_gtm_hdf)                                
            end if
            if (debug_print==.true.) then                                                 
                call write_qual_hdf_ts(qual_hdf%cell_flow_id, &
                                       flow,                  & 
                                       n_cell,                &
                                       time_index_in_gtm_hdf)
                call write_qual_hdf_ts(qual_hdf%cell_area_id, &
                                       area,                  & 
                                       n_cell,                &
                                       time_index_in_gtm_hdf)                               
                call write_qual_hdf_ts(qual_hdf%cell_cfl_id,  &
                                       cfl,                   & 
                                       n_cell,                &
                                       time_index_in_gtm_hdf)                                             
            end if            
        end if                           
        prev_julmin = int(current_time)         
        
    end do  ! end of loop for gtm time step
    if (n_dssfiles .ne. 0) then
        call zclose(ifltab_in)  !!ADD A global to detect if dss is opened
        deallocate(ifltab_in) 
    end if
    if (n_outdssfiles .ne. 0) then
        call zclose(ifltab_out)  !!ADD A global to detect if dss is opened
        deallocate(ifltab_out) 
    end if    
    if (apply_diffusion)then
        call klu_fortran_free(k_symbolic, k_numeric, k_common)
        call deallocate_geom_arr
    end if
    deallocate(disp_coef_arr)
    deallocate(pathinput)
    deallocate(pathoutput)
    deallocate(node_conc)   
    deallocate(constituents)
    deallocate(init_c)
    deallocate(init_r)
    deallocate(vals)
    call deallocate_datain             
    call deallocate_geometry
    call deallocate_state
    call deallocate_state_network
    call deallocate_network_tmp
    call deallocate_state_hydro
    call deallocate_hydro_ts
    call close_qual_hdf(qual_hdf)         
    call hdf5_close
    close(debug_unit)
    write(*,*) '-------- Normal program end -------'
    call exit(0)   
end program

