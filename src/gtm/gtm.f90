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
    use hydro_data
    use hydro_data_network
    use state_variables
    use state_variables_network
    use primitive_variable_conversion
    use gradient
    use gradient_adjust
    use advection
    use diffusion
    use dispersion_coefficient    
    use source_sink
    use boundary_advection 
    use boundary_diffusion  
    use boundary_advection_network     
    use boundary_diffusion_network    
    use boundary_concentration        
    use gtm_subs
    use dsm2_time_utils, only: incr_intvl
    use gtm_init_store_outputs
    use gtm_store_outpath
    use klu
    ! extended modules
    use turbidity
    use suspended_sediment
    use sediment_variables
    use sediment_bed    
    use mercury_state_variables
    use mercury_fluxes

    implicit none
    
    real(gtm_real), allocatable :: cfl(:)
    real(gtm_real), allocatable :: disp_coef_lo(:)           !< Low side constituent dispersion coef. at new time
    real(gtm_real), allocatable :: disp_coef_hi(:)           !< High side constituent dispersion coef. at new time
    real(gtm_real), allocatable :: disp_coef_lo_prev(:)      !< Low side constituent dispersion coef. at old time
    real(gtm_real), allocatable :: disp_coef_hi_prev(:)      !< High side constituent dispersion coef. at old time
    real(gtm_real), allocatable :: explicit_diffuse_op(:,:)   
    real(gtm_real), allocatable :: init_c(:,:)
    real(gtm_real), allocatable :: init_r(:,:)   
    real(gtm_real), allocatable :: manning(:)     
    real(gtm_real) :: theta = half                           !< Crank-Nicolson implicitness coeficient
    real(gtm_real) :: constant_dispersion   
    
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
    integer :: next_output_flush 
    
    ! variables for sub time stepping 
    logical :: sub_time_step = .true.                         ! flag to turn on/off sub time stepping
    integer, parameter :: max_num_sub_ts = 30                 ! maximum number of sub time step within GTM time step    
    real(gtm_real) :: sub_gtm_time_step                       ! sub time step for GTM when max CFL > 1
    real(gtm_real) :: max_cfl                                 ! max_cfl = maxval(cfl)
    real(gtm_real) :: flow_chk                                ! flow check
    integer :: sub_st                                         ! number of sub time step within gtm time step
    integer :: prev_sub_ts                                    ! previous number of sub time step (used to decide if deallocate and allocate is needed.)
    integer :: ceil_max_cfl                                   ! ceiling integer of max_cfl
    integer :: tmp_int
    integer :: ierror
    integer :: nt     
    integer :: istat = 0  
        
    ! for specified output locations
    real(gtm_real), allocatable :: vals(:)  
    logical :: file_exists
    integer :: st, p, n_st, chan_no
    integer :: ivar
    real(gtm_real) :: start, finish    
    
    procedure(hydro_data_if), pointer :: fill_hydro => null() ! Hydrodynamic pointer to be filled by the driver
    logical, parameter :: limit_slope = .true.                ! Flag to switch on/off slope limiter  
    logical :: debug_interp = .false.    
    logical :: use_gtm_hdf = .false.
    logical :: apply_diffusion_prev = .false.                 ! Calculate diffusive operatore from previous time step
    character(len=130) :: init_input_file                     ! initial input file on command line [optional]
    character(len=:), allocatable :: restart_file_name  
    character(len=14) :: cdt
    character(len=9) :: prev_day
    character(len=32) :: name(10)
    integer :: nadd, ok
    real(gtm_real), allocatable :: input_time_series(:,:) 
    
    ! variables for sediment module
    real(gtm_real), allocatable :: diameter(:,:)
    real(gtm_real), allocatable :: fall_velocity(:,:)
    real(gtm_real), allocatable :: critical_shear(:,:)
    real(gtm_real), allocatable :: particle_reynolds(:,:)
    real(gtm_real), allocatable :: erosion(:,:)
    real(gtm_real), allocatable :: deposition(:,:)
    real(gtm_real), allocatable :: available_bed(:,:)       
    integer :: isediment = 0
    ! variables for turbidity module
    real(gtm_real), allocatable :: turbidity_decay(:)
    real(gtm_real), allocatable :: turbidity_settle(:)
    ! variables for mercury module
    real(gtm_real), allocatable :: conc_mercury(:,:)
    real(gtm_real), allocatable :: source_mercury(:,:)
    real(gtm_real), allocatable :: area_wet(:)
    integer :: ec_ivar, doc_ivar
    integer :: nlayers = 2
    integer :: rkstep = 2
        
    open(unit_error, file="error.log")
    !open(debug_unit, file = "gtm_debug_unit.txt")          ! debug output text file
    
    !----- Start of DSM2-GTM Program  -----    
    call cpu_time(start)
    call h5open_f(ierror)
    call verify_error(ierror, "opening hdf interface")   
    
    !----- Read input specification from *.inp text file -----
    call get_command_args(init_input_file)
    call read_input_text(init_input_file)                  ! read input specification text
    call opendss(ifltab_in, n_dssfiles, indssfiles)        ! open all input dss files
    call opendss(ifltab_out, n_outdssfiles, outdssfiles)   ! open all output dss files        
        
    write(*,*) "Process DSM2 geometry info...."    
    call hdf5_init(hydro_hdf5)                         
    call dsm2_hdf_geom
    !call write_geom_to_text

    call check_runtime(num_blocks, memlen,                     & 
                       memory_buffer, hydro_time_interval,     & 
                       hydro_start_jmin, hydro_end_jmin,       &
                       gtm_start_jmin, gtm_end_jmin)  
    
    ! assigen the initial concentration to cells and reservoirs    
    restart_file_name = trim(gtm_io(1,1)%filename) 
    inquire(file=gtm_io(1,1)%filename, exist=file_exists)
    if (file_exists==.true.) then 
        call check_init_file(nadd, name, restart_file_name)
        if (nadd .gt. 0) n_var = n_var + nadd
        allocate(constituents(n_var))
        do i = 1, nadd
            constituents(i)%conc_no = i
            constituents(i)%name = name(i)
        enddo
        constituents(nadd+1:n_var) = constituents_tmp(1:n_var-nadd)
        allocate(init_c(n_cell,n_var))
        allocate(init_r(n_resv,n_var))                
        call read_init_file(init_c, init_r, restart_file_name)
    else
        allocate(constituents(n_var))
        constituents = constituents_tmp
        allocate(init_c(n_cell,n_var))
        allocate(init_r(n_resv,n_var))  
        if (init_conc .ne. LARGEREAL) then
            init_c = init_conc
            init_r = init_conc
        else    
            init_c = zero
            init_r = zero            
        end if                         
    endif       
                       
    call assign_ivar_to_outpath    
    allocate(vals(noutpaths))    
    call get_cell_info    
    call get_output_channel
    nt = npartition_t + 1
    
    !----- assign dispersion coefficient and the pointer to function call
    if (apply_diffusion) then
        if (disp_coeff.ne.LARGEREAL) then
            disp_arr = disp_coeff
        end if        
        dispersion_coef => adjust_dispersion_coef_with_velocity !assign_dispersion_coef
    end if 
    
    !----- allocate array for interpolation -----         
    call allocate_state_hydro(n_comp, n_resv, n_resv_conn, n_qext, n_tran, n_cell)
    call allocate_hydro_ts 
    call allocate_network_tmp(npartition_t)
    call allocate_state(n_cell, n_var)
    call allocate_state_network(n_resv, n_resv_conn, n_qext, n_tran, n_node, n_cell, n_var)
    allocate(explicit_diffuse_op(n_cell,n_var))
    allocate(linear_decay(n_var))
    allocate(cfl(n_cell))    
    allocate(disp_coef_lo(n_cell), disp_coef_hi(n_cell))
    allocate(disp_coef_lo_prev(n_cell), disp_coef_hi_prev(n_cell))
    allocate(source_term_by_cell(n_cell, n_var))    
    
    write(*,*) "You need to have ",n_cell," number of cells in initial file."
    
    call assign_node_ts
    if (trim(gtm_io(3,2)%filename).ne.'') use_gtm_hdf = .true.
    if (use_gtm_hdf) then
        use_gtm_hdf = .true.    
        call incr_intvl(tmp_int, 0,gtm_io(3,2)%interval,1)
        gtm_hdf_time_intvl = dble(tmp_int)
        call init_gtm_hdf(gtm_hdf,              &
                          gtm_io(3,2)%filename, &
                          n_cell,               &
                          n_chan,               &
                          n_resv,               &
                          n_var,                &
                          int(gtm_start_jmin),  &
                          int(gtm_end_jmin),    &
                          gtm_io(3,2)%interval)
        call write_input_to_hdf5(gtm_hdf%file_id)
        call write_grid_to_tidefile(gtm_hdf%file_id)
    end if
                
    !----- point to interface -----
    fill_hydro_info => hydro_info
    fill_hydro_network => gtm_network_data
    compute_source => set_source_term_by_cell
    conc_gradient => difference_network
    adjust_gradient => adjust_differences_network               ! adjust gradients for DSM2 network
    boundary_conc => assign_boundary_concentration              ! assign boundary concentration    
    advection_boundary_flux => bc_advection_flux_network        ! adjust flux for DSM2 network
    boundary_diffusion_flux => network_boundary_diffusive_flux
    boundary_diffusion_matrix => network_diffusion_sparse_matrix_zero_at_junctions
    source_term_by_cell = zero 
    
    call set_dispersion_arr(disp_arr, n_cell)
    write(*,*) "Process time series...."
    prev_day = "01JAN1000"                                      ! to initialize for screen printing only

    ! for sediment module
    allocate(diameter(n_cell,n_sediment))  
    allocate(fall_velocity(n_cell,n_sediment)) 
    allocate(particle_reynolds(n_cell,n_sediment))
    allocate(critical_shear(n_cell,n_sediment))
    allocate(erosion(n_cell,n_sediment))
    allocate(deposition(n_cell,n_sediment))
    allocate(available_bed(n_cell,n_sediment))
    call assign_group_static_variables
    call assign_input_ts_group_var
    allocate(input_time_series(n_ts_var,n_cell)) 
       
    do i = 1, n_sediment
        call check_group_channel(ncc_ssc,sediment_coef_start+i-1)     
        diameter(:,i) = group_var_cell(ncc_ssc,sediment_coef_start+i-1,:)
        call get_sediment_properties(fall_velocity(:,i), particle_reynolds(:,i), &
                                     critical_shear(:,i), diameter(:,i), n_cell)
    end do
    !> Assigning group variables for turbidity module
    allocate(turbidity_decay(n_cell),turbidity_settle(n_cell))
    turbidity_decay(:) = group_var_cell(ncc_turbidity,decay,:)
    turbidity_settle(:) = group_var_cell(ncc_turbidity,settle,:)
    !> for mercury module
    if (run_mercury) then
        call allocate_mercury(n_cell,nlayers)
        allocate(conc_mercury(n_cell,n_mercury), source_mercury(n_cell,n_mercury))
        allocate(area_wet(n_cell))
        call check_mercury_ts_input
        call constituent_name_to_ivar(ec_ivar, 'ec')  
        call constituent_name_to_ivar(doc_ivar, 'ec')  ! todo: change 'ec' to 'doc' when doc model setup is ready
    end if
    
    ! initialize concentrations
    conc = init_c
    conc_prev = init_c        
    conc_resv = init_r 
    conc_resv_prev = init_r       
    prev_conc_stip = zero
    mass_prev = zero
    
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
        disp_coef_lo = zero !LARGEREAL
        disp_coef_hi = zero !LARGEREAL            
    end if
    disp_coef_lo_prev = disp_coef_lo
    disp_coef_hi_prev = disp_coef_hi

    prev_sub_ts = 1
    start_julmin = int(gtm_start_jmin)
    end_julmin = int(gtm_end_jmin)    
    call incr_intvl(next_output_flush, start_julmin, flush_intvl, TO_BOUNDARY)
    call gtm_init_store_outpaths(istat)
    
    ! start marching throught the time steps for simulation
    do current_time = gtm_start_jmin, gtm_end_jmin, gtm_time_interval
        
        time_step_int = int((current_time-gtm_start_jmin)/gtm_time_interval) + 1 
         
        !---print out processing date on screen
        cdt = jmin2cdt(int(current_time))   ! this function only used for screen status printout
        if (cdt(1:9) .ne. prev_day) then    ! Rough integer is fine, only for screen printing
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
            !call get_area_for_buffer(hydro_area, hydro_ws, n_comp, memory_buffer) ! can be deleted later
            if (prev_comp_flow(1)==LARGEREAL) then                             !if (slice_in_block .eq. 1) then
                call dsm2_hdf_slice(prev_comp_flow, prev_comp_ws, prev_hydro_resv, prev_hydro_resv_flow,   &
                                    prev_hydro_qext, prev_hydro_tran, n_comp, n_resv, n_resv_conn, n_qext, &
                                    n_tran, time_offset+slice_in_block-2)
            end if
        end if

        !--- interpolate flow and water surface between computational points
        if (slice_in_block .ne. current_slice) then  ! check if need to interpolate for new hydro time step
            n_st = npartition_t + 1
            if (prev_sub_ts .ne. 1) then
                call deallocate_network_tmp
                call allocate_network_tmp(npartition_t)
            end if
            call interp_network_linear(npartition_t, slice_in_block, n_comp, prev_comp_flow, &
                 prev_comp_ws, n_cell, prev_flow_cell_lo, prev_flow_cell_hi) 
            call interp_network_ext(npartition_t, slice_in_block, prev_hydro_resv,           &
                 prev_hydro_resv_flow, prev_hydro_qext, prev_hydro_tran)              
            ! to determine if sub time step is required based on CFL number
            call fill_hydro_info(flow,          &
                                 flow_lo,       &
                                 flow_hi,       &
                                 area,          &
                                 area_lo,       &
                                 area_hi,       &
                                 width,         &
                                 hydro_radius,  &
                                 depth,         &
                                 n_cell,        &
                                 dble(1),       &
                                 dx_arr,        &
                                 gtm_time_interval)
                            
            cfl = abs(flow/area)*(gtm_time_interval*sixty)/dx_arr
            max_cfl = maxval(cfl)
            ceil_max_cfl = ceiling(max_cfl) 
            if ((max_cfl .gt. one).and.(sub_time_step)) then
                if (ceil_max_cfl .gt. max_num_sub_ts) then  
                    ceil_max_cfl = max_num_sub_ts
                    write(*,*) "exceed max number of sub timestep. consider to decrease the cell size."
                end if
                sub_gtm_time_step = gtm_time_interval/dble(ceil_max_cfl)                          
                call deallocate_network_tmp
                call allocate_network_tmp(npartition_t*ceil_max_cfl)         
                n_st = npartition_t*ceil_max_cfl + 1
                call interp_network_linear(npartition_t*ceil_max_cfl, slice_in_block, n_comp,    &
                     prev_comp_flow, prev_comp_ws, n_cell, prev_flow_cell_lo, prev_flow_cell_hi) 
                call interp_network_ext(npartition_t*ceil_max_cfl, slice_in_block,               &
                     prev_hydro_resv, prev_hydro_resv_flow, prev_hydro_qext, prev_hydro_tran)                                      
                prev_sub_ts = ceil_max_cfl
            end if                                                 
            prev_comp_flow(:) = hydro_flow(:,slice_in_block)
            prev_comp_ws(:) = hydro_ws(:,slice_in_block)     
            prev_hydro_resv(:) = hydro_resv_height(:,slice_in_block)
            prev_hydro_resv_flow(:) = hydro_resv_flow(:,slice_in_block)
            prev_hydro_qext(:) = hydro_qext_flow(:,slice_in_block)
            prev_hydro_tran(:) = hydro_tran_flow(:,slice_in_block)
            prev_flow_cell_lo(:) = flow_mesh_lo(n_st,:)
            prev_flow_cell_hi(:) = flow_mesh_hi(n_st,:)            
            current_slice = slice_in_block            
        end if

        do i = 1, n_ts_var   !use ts_name(i) to get the variable name
            input_time_series(i,:) = pathinput(ts(i,:))%value
        end do        
        
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
            !---rounded integer is fine since DSS does not take care of precision finer than 1 minute anyway.
            call get_inp_value(int(new_current_time),int(new_current_time-sub_gtm_time_step))
                              
            call fill_hydro_info(flow,          &
                                 flow_lo,       &
                                 flow_hi,       &
                                 area,          &
                                 area_lo,       &
                                 area_hi,       &
                                 width,         &
                                 hydro_radius,  &
                                 depth,         &
                                 n_cell,        &
                                 dble(1),       &
                                 dx_arr,        &
                                 gtm_time_interval)
        
            call fill_hydro_network(resv_height,  &
                                    resv_flow,    &
                                    qext_flow,    &
                                    tran_flow,    &
                                    n_resv,       &
                                    n_resv_conn,  & 
                                    n_qext,       &
                                    n_tran,       &
                                    dble(t_index))                               
      
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
            end if
            cfl = flow/area*(gtm_time_interval*sixty)/dx_arr           


            !--------------- Source Terms Implementation --------------
            ! Sedimentation process       
            do ivar = 1, n_var
                if (trim(constituents(ivar)%use_module)=='sediment') then
                    isediment = ivar-(n_var-n_sediment)
                    available_bed(:,isediment) = erosion(:,isediment)
                    call suspended_sediment_flux(source_term_by_cell(:,ivar),    &  
                                                 erosion(:,isediment),           &  !unit:kg/m2/s
                                                 deposition(:,isediment),        &  !unit:kg/m2/s
                                                 conc(:,ivar),                   &
                                                 flow,                           &
                                                 area,                           &
                                                 width,                          &
                                                 hydro_radius,                   & 
                                                 depth,                          &
                                                 mann_arr,                       &
                                                 diameter(:,isediment),          &
                                                 fall_velocity(:,isediment),     &
                                                 critical_shear(:,isediment),    &
                                                 particle_reynolds(:,isediment), & 
                                                 dx_arr,                         &
                                                 sub_gtm_time_step*sixty,        &
                                                 n_cell,                         &
                                                 available_bed(:,isediment))                  
                elseif (trim(constituents(ivar)%name)=='turbidity') then
                    call turbidity_source(source_term_by_cell(:,ivar),           & 
                                          conc(:,ivar),                          &
                                          turbidity_decay,                       &
                                          turbidity_settle,                      &
                                          sub_gtm_time_step*sixty,               &
                                          n_cell)
                end if
            end do
            area_wet = dx_arr*width
            temperature = input_time_series(code_to_ts_id(ts_var_temp),:)
            call sediment_bed_main(erosion,                 &
                                   deposition,              &
                                   temperature,             &
                                   area_wet,                &   
                                   sub_gtm_time_step*sixty, & 
                                   rkstep,                  &
                                   nlayers,                 &
                                   n_cell,                  &
                                   n_sediment)             
            ! call mercury module 
            if (run_mercury) then    
                conc_mercury(:,1:n_mercury) = conc(:,mercury_ivar(1:n_mercury))
                call mercury_source(source_mercury,                                      & !< source/sink to GTM
                                    conc_mercury,                                        & !< GTM results from previous step
                                    area_wet,                                            & !< hydrodynamic data from Hydro
                                    depth,                                               & !< water depth
                                    n_cell,                                              & !< number of cells
                                    n_sediment,                                          & !< number of sediments
                                    n_mercury,                                           & !< number of mercury constituents
                                    conc(:,n_var-n_sediment+1:n_var),                    & !< sediment concentration 
                                    conc(:,ec_ivar),                                     & 
                                    conc(:,doc_ivar),                                    &
                                    input_time_series(code_to_ts_id(ts_var_do),:),       & ! DO
                                    input_time_series(code_to_ts_id(ts_var_ph),:),       & ! pH
                                    input_time_series(code_to_ts_id(ts_var_so4),:),      & ! SO4
                                    input_time_series(code_to_ts_id(ts_var_temp),:),     & ! Temperature
                                    input_time_series(code_to_ts_id(ts_var_ipar),:),     & ! ipar
                                    input_time_series(code_to_ts_id(ts_var_iuva),:),     & ! iuva
                                    input_time_series(code_to_ts_id(ts_var_iuvb),:),     & ! iuvb
                                    input_time_series(code_to_ts_id(ts_var_rgm_air),:),  & ! rgm_atm
                                    input_time_series(code_to_ts_id(ts_var_hg0_air),:),  & ! Hg0_atm
                                    input_time_series(code_to_ts_id(ts_var_mehg_air),:), & ! MeHg_atm
                                    input_time_series(code_to_ts_id(ts_var_precip),:),   & ! precip 
                                    input_time_series(code_to_ts_id(ts_var_wet_hgii),:), & ! wetdep_HgII
                                    input_time_series(code_to_ts_id(ts_var_dry_hgii),:), & ! drydep_HgII
                                    input_time_series(code_to_ts_id(ts_var_wet_mehg),:), & ! wetdep_MeHg
                                    input_time_series(code_to_ts_id(ts_var_dry_mehg),:), & ! drydep_MeHg
                                    input_time_series(code_to_ts_id(ts_var_rct_if),:),   & ! dgm_ratio
                                    input_time_series(code_to_ts_id(ts_var_rct_water),:),& ! rct_interface
                                    input_time_series(code_to_ts_id(ts_var_solid_in),:), & ! rct_water
                                    input_time_series(code_to_ts_id(ts_var_vol_frac),:))   ! vol_frac                
                source_term_by_cell(:,mercury_ivar(1:n_mercury)) = source_mercury(:,1:n_mercury)
            end if
                     
            !------------------ End of Source Terms Implementation -----------------         
          
            if (apply_diffusion_prev) then   ! take dispersion term for advection calculation
                boundary_diffusion_flux => network_boundary_diffusive_flux_prev
                call explicit_diffusion_operator(explicit_diffuse_op,          &
                                                 conc_prev,                    &
                                                 area_lo_prev,                 &
                                                 area_hi_prev,                 &
                                                 disp_coef_lo_prev,            &  
                                                 disp_coef_hi_prev,            &
                                                 n_cell,                       &
                                                 n_var,                        &
                                                 dble(new_current_time)*sixty, &
                                                 dx_arr,                       &
                                                 sub_gtm_time_step*sixty)
            else !omit dispersion term in advection calculation
                explicit_diffuse_op = zero
            end if    

            !----- advection and source/sink -----        
            call advect(mass,                         &
                        mass_prev,                    &  
                        flow,                         &
                        flow_lo,                      &
                        flow_hi,                      &
                        area,                         &
                        area_prev,                    & 
                        area_lo,                      &
                        area_hi,                      &
                        explicit_diffuse_op,          &                             
                        n_cell,                       &
                        n_var,                        &
                        dble(new_current_time)*sixty, &
                        sub_gtm_time_step*sixty,      &
                        dx_arr,                       &
                        limit_slope)   
            where (mass.lt.zero) mass = zero                               
            call cons2prim(conc, mass, area, n_cell, n_var)

            !--------- Diffusion ----------
            if (apply_diffusion) then
                boundary_diffusion_flux => network_boundary_diffusive_flux            
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
                call diffuse(conc,                         &
                             conc_prev,                    &
                             mass,                         &                                     
                             area,                         &
                             flow_lo,                      &
                             flow_hi,                      &                                     
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
                             dx_arr,                       &
                             .true.)        
                call prim2cons(mass,conc,area,n_cell,n_var)                
            end if       
            
            ! add all sediment to ssc
            if (ssc_index .ne. 0) then
                do i = 1, n_cell
                    conc(i,ssc_index) = zero
                    do ivar = n_var-n_sediment+1,n_var
                        conc(i,ssc_index) = conc(i,ssc_index) + conc(i,ivar)
                    end do
                end do
                do i = 1, n_resv
                    conc_resv(i,ssc_index) = zero
                    do ivar = n_var-n_sediment+1,n_var
                        conc_resv(i,ssc_index) = conc_resv(i,ssc_index) + conc_resv(i,ivar)
                    end do
                end do                
            end if    
                                    
            mass_prev = mass
            conc_prev = conc
            conc_resv_prev = conc_resv
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
            prev_conc_stip = conc_stip            
            node_conc = LARGEREAL
        
        end do ! end of loop of sub time step

        !---- print output to DSS file -----
        call get_output_channel_vals(vals, conc, n_cell, conc_resv, n_resv, n_var)
        if (int(current_time) .ge. next_output_flush .or. current_time .eq. gtm_end_jmin) then
            call incr_intvl(next_output_flush, next_output_flush, flush_intvl, TO_BOUNDARY)
            call gtm_store_outpaths(.true.,int(current_time),int(gtm_time_interval), vals)
        else
            call gtm_store_outpaths(.false.,int(current_time),int(gtm_time_interval), vals)
        endif
        
        !----- print output to hdf5 file -----
        !                     
        if (use_gtm_hdf) then
            if (mod(current_time-gtm_start_jmin,gtm_hdf_time_intvl)==zero) then
                time_index_in_gtm_hdf = (current_time-gtm_start_jmin)/gtm_hdf_time_intvl
                if (hdf_out .eq. 'channel') then
                    call write_gtm_chan_hdf(gtm_hdf,               &
                                            conc,                  &
                                            n_chan,                &
                                            n_cell,                &
                                            n_var,                 &
                                            time_index_in_gtm_hdf)                
                else
                    call write_gtm_hdf(gtm_hdf,                    &
                                       conc,                       &
                                       n_cell,                     &
                                       n_var,                      &
                                       time_index_in_gtm_hdf)
                    !write(501,'(a15,<n_cell>(a1,f8.3))') cdt(1:9),((',',conc(i,1)),i=1,n_cell)
                end if
                if (n_resv .gt. 0) then
                    call write_gtm_hdf_resv(gtm_hdf,               & 
                                            conc_resv,             & 
                                            n_resv,                &
                                            n_var,                 &  
                                            time_index_in_gtm_hdf)                                
                end if
                if (debug_print==.true.) then                                                 
                    call write_gtm_hdf_ts(gtm_hdf%cell_flow_id,    &
                                          flow_hi,                 & 
                                          n_cell,                  &
                                          time_index_in_gtm_hdf)
                    call write_gtm_hdf_ts(gtm_hdf%cell_area_id,    &
                                          area_hi,                 & 
                                          n_cell,                  &
                                          time_index_in_gtm_hdf)                               
                    call write_gtm_hdf_ts(gtm_hdf%cell_cfl_id,     &
                                          cfl,                     & 
                                          n_cell,                  &
                                          time_index_in_gtm_hdf)                                             
                end if            
            end if
        end if
                                       
        prev_julmin = int(current_time)         
        
    end do  ! end of loop for gtm time step
    
    call print_last_stage(jmin2cdt(int(current_time)),int(current_time),conc,conc_resv,n_cell,n_resv,n_var)
    
    !----- deallocate memories and close file units -----
    if (n_dssfiles .ne. 0) then
        call zclose(ifltab_in)   !!ADD A global to detect if dss is opened
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
    if (use_gtm_hdf) then
        call close_gtm_hdf(gtm_hdf)         
        call hdf5_close
    end if    
    deallocate(disp_coef_arr)
    deallocate(pathinput)
    deallocate(pathoutput)
    deallocate(constituents)
    deallocate(init_c)
    deallocate(init_r)
    deallocate(vals)
    deallocate(source_term_by_cell)
    deallocate(diameter)
    deallocate(fall_velocity)
    deallocate(particle_reynolds)
    deallocate(critical_shear)
    deallocate(erosion)
    deallocate(deposition)
    deallocate(available_bed)
    deallocate(input_ts, ts, input_time_series)
    deallocate(turbidity_decay, turbidity_settle)
    if (run_mercury) then
        deallocate(conc_mercury, source_mercury, area_wet)
        call deallocate_mercury
    end if
    call deallocate_datain             
    call deallocate_geometry
    call deallocate_state
    call deallocate_state_network
    call deallocate_network_tmp
    call deallocate_state_hydro
    call deallocate_hydro_ts
    !close(debug_unit)
    close(unit_error)
    write(*,*) '-------- Normal program end -------'
    call cpu_time(finish)
    write(*,*) "Total CPU Time = ",finish - start," seconds." 
    call exit(0)   
end program

