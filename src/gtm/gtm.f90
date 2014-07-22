!<license>
!    Copyright (C) 2013 State of California,
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
! 
!> Main program for General Transport Model
!>@ingroup driver
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
    use state_variables
    use primitive_variable_conversion
    use advection
    use diffusion
    use dispersion_coefficient    
    use source_sink
    use boundary_advection 
    use boundary_diffusion        
    use gtm_subs
    use dsm2_time_utils, only: incr_intvl

    implicit none
    
    integer :: nt
    real(gtm_real), allocatable :: prev_comp_flow(:)
    real(gtm_real), allocatable :: prev_comp_ws(:)
    real(gtm_real), allocatable :: flow_arr(:), ws_arr(:)
    real(gtm_real), allocatable :: cfl(:)
    real(gtm_real), allocatable :: disp_coef_lo(:)      !< Low side constituent dispersion coef. at new time
    real(gtm_real), allocatable :: disp_coef_hi(:)      !< High side constituent dispersion coef. at new time
    real(gtm_real), allocatable :: disp_coef_lo_prev(:) !< Low side constituent dispersion coef. at old time
    real(gtm_real), allocatable :: disp_coef_hi_prev(:) !< High side constituent dispersion coef. at old time
    real(gtm_real) :: theta = half                      !< Crank-Nicolson implicitness coeficient
    real(gtm_real), parameter :: constant_dispersion = 0  !6000.d0
    
    real(gtm_real) :: total_flow_volume_change, total_area_volume_change
    real(gtm_real) :: avga_volume_change, diff
    integer :: up_comp, down_comp    
    integer :: time_offset
    integer :: i, j, ibuffer, start_buffer, icell
    integer :: iblock, slice_in_block, t_index
    real(gtm_real) :: time
    real(gtm_real) :: current_time
    real(gtm_real) :: gtm_hdf_time_intvl
    integer :: offset, num_blocks, jday
    integer, allocatable :: memlen(:)
    procedure(hydro_data_if), pointer :: fill_hydro => null()   ! Hydrodynamic pointer to be filled by the driver
    logical, parameter :: limit_slope = .false.                 ! Flag to switch on/off slope limiter  
    real(gtm_real), allocatable :: init_c(:,:)
    !real(gtm_real), parameter :: constant_decay = 5.0d-5
    real(gtm_real), parameter :: constant_decay = zero
    character(len=130) :: init_input_file                       ! initial input file on command line [optional]
    character(len=24) :: restart_file_name
    character(len=14) :: cdt
    character(len=9) :: prev_day
    integer :: runtime_hydro_start, runtime_hydro_end
    integer :: current_block = 0
    integer :: current_slice = 0
    integer :: time_index_in_gtm_hdf

    integer :: ierror
    logical :: debug_interp = .false.

    integer :: n_bound_ts
    integer, allocatable :: bound_index(:), path_index(:)
    real(gtm_real), allocatable :: node_conc(:,:)
    
    logical :: apply_diffusion = .false.

    n_var = 1
    
    call h5open_f(ierror)
    call verify_error(ierror, "opening hdf interface")   
    
    !
    !----- Read input specification from *.inp text file -----
    !
    call get_command_args(init_input_file)
    call read_input_text(init_input_file)                  ! read input specification text
    call opendss(ifltab_in, n_dssfiles, indssfiles)        ! open all dss files
    open(debug_unit, file = "gtm_debug_unit.txt")          ! debug output text file
    
    write(*,*) "Process DSM2 geometry info...."    
    call hdf5_init(hydro_hdf5)                         
    call dsm2_hdf_geom
    call write_geom_to_text

    call check_runtime(num_blocks, memlen,                     & 
                       memory_buffer, hydro_time_interval,     & 
                       hydro_start_jmin, hydro_end_jmin,       &
                       gtm_start_jmin, gtm_end_jmin)   
                           
    !
    !----- allocate array for interpolation -----     
    !
    nt = npartition_t + 1
    allocate(prev_comp_flow(n_comp))
    allocate(prev_comp_ws(n_comp))
    allocate(constituents(n_var))
    allocate(flow_arr(n_comp), ws_arr(n_comp))
    call allocate_hydro_ts()  
    call allocate_network_tmp()
    call allocate_state(n_cell, n_var)
    call allocate_state_resv(n_resv, n_var)
    allocate(init_c(n_cell,n_var))
    allocate(linear_decay(n_var))
    allocate(cfl(n_cell))    
    allocate(disp_coef_lo(n_cell), disp_coef_hi(n_cell))
    allocate(disp_coef_lo_prev(n_cell), disp_coef_hi_prev(n_cell))
    
    prev_comp_flow = LARGEREAL
    prev_comp_ws = LARGEREAL
    
    linear_decay = constant_decay
    
    constituents(1)%conc_no = 1
    constituents(1)%name = "EC"
    
    gtm_hdf_time_intvl = incr_intvl(zero,gtm_io(3,2)%interval,1)
    call init_qual_hdf(qual_hdf,             &
                       gtm_io(3,2)%filename, &
                       n_cell,               &
                       n_resv,               &
                       n_var,                &
                       gtm_start_jmin,       &
                       gtm_end_jmin,         &
                       gtm_io(3,2)%interval)
    
    if (trim(gtm_io(3,2)%filename) .ne. "") then
        call write_input_to_hdf5(qual_hdf%file_id)
        call write_grid_to_tidefile(qual_hdf%file_id)
    end if

    
    do i = 1, n_inputpaths
        do j = 1, n_var
            call locase(pathinput(i)%variable)
            call locase(constituents(j)%name)
            if (trim(pathinput(i)%variable) .eq. trim(constituents(j)%name)) then
                pathinput(i)%i_var = constituents(j)%conc_no
            end if
        end do
        do j = 1, n_node
            if (pathinput(i)%obj_no==dsm2_node(j)%dsm2_node_no) then
                pathinput(i)%i_node = j
            end if        
        end do
    end do

    
    allocate(node_conc(n_node, n_var))
    node_conc = zero
                       
    !
    !----- point to interface -----
    !
    fill_hydro => gtm_flow_area
    dispersion_coef => constant_dispersion_coef
    compute_source => no_source
    !compute_source => linear_decay_source
    advection_boundary_flux => bc_fixup_advection_flux
    boundary_diffusion_flux => neumann_zero_diffusive_flux
    boundary_diffusion_matrix => neumann_zero_diffusion_matrix
    
    call set_constant_dispersion(constant_dispersion)
    
    write(*,*) "Process time series...."
    write(debug_unit,"(16x,3000i8)") (i, i = 1, n_cell) 
 
    prev_day =  "01JAN1000"       ! to initialize for screen printing only

    restart_file_name = trim(gtm_io(1,1)%filename)   
    call read_init_file(init_c, restart_file_name, n_cell, n_var)
    if (init_c(1,1) .ne. LARGEREAL) then
        conc = init_c
    else    
        conc = init_conc
    end if    

    if (apply_diffusion)then
        call dispersion_coef(disp_coef_lo,         &
                             disp_coef_hi,         &
                             flow,                 &
                             flow_lo,              &
                             flow_hi,              &
                             gtm_start_jmin,       &
                             dx_arr,               &
                             gtm_time_interval,    &
                             n_cell,               &
                             n_var) 
    else
        disp_coef_lo = LARGEREAL
        disp_coef_hi = LARGEREAL            
    end if
    disp_coef_lo_prev = disp_coef_lo
    disp_coef_hi_prev = disp_coef_hi    

    write(debug_unit,'(2a6,a10,11a24)') "cell","var","dx","grad","grad_lo","grad_hi","area","mass_prev","flow_lo","flow_hi","conc_lo","conc_hi","flux_lo","flux_hi"
        
    do current_time = gtm_start_jmin, gtm_end_jmin, gtm_time_interval
        
        !---print out processing date on screen
        cdt = jmin2cdt(int(current_time))   ! this function only for screen status printout. Rough integer is fine.
        if (cdt(1:9) .ne. prev_day) then 
            write(*,*) cdt(1:9)
            prev_day =  cdt(1:9)
        end if
        
        !---get time series for boundary conditions
        call get_inp_value(int(current_time),int(current_time-gtm_time_interval))   ! this will update pathinput(:)%value; rounded integer is fine since DSS does not take care of precision finer than 1minute anyway.
        
        do i = 1, n_inputpaths
            node_conc(pathinput(i)%i_node, pathinput(i)%i_var) = pathinput(i)%value   
        end do    
        
        !---read hydro data from hydro tidefile
        call get_loc_in_hydro_buffer(iblock, slice_in_block, t_index, current_time, hydro_start_jmin, &
                                     memory_buffer, hydro_time_interval, gtm_time_interval)

        if (iblock .ne. current_block) then ! check if need to read new block into buffer
            current_block = iblock
            current_slice = 0
            time_offset = memory_buffer*(iblock-1)
            call dsm2_hdf_ts(time_offset, memlen(iblock))
            if (prev_comp_flow(1)==LARGEREAL) then                             !if (slice_in_block .eq. 1) then
                call dsm2_hdf_slice(prev_comp_flow, prev_comp_ws, n_comp, time_offset-1)
            end if
        end if

        !--- interpolate flow and water surface between computational points
        if (slice_in_block .ne. current_slice) then  ! check if need to interpolate for new hydro time step
            call interp_network(npartition_t, slice_in_block, n_comp, prev_comp_flow, prev_comp_ws)
            prev_comp_flow(:) = hydro_flow(:,slice_in_block)
            prev_comp_ws(:) = hydro_ws(:,slice_in_block)       
            current_slice = slice_in_block
        end if
                
        call fill_hydro(flow,     &
                        flow_lo,  &
                        flow_hi,  &
                        area,     &
                        area_lo,  &
                        area_hi,  &
                        n_cell,   &
                        dble(t_index),  &
                        dx_arr,   &
                        gtm_time_interval)         
                           
        if (current_time == gtm_start_jmin) then
            call prim2cons(mass_prev, conc, area, n_cell, n_var)
            area_prev = area
            area_lo_prev = area_lo
            area_hi_prev = area_hi
        end if
        
        !
        !----- call advection and source -----
        !
        call advect(mass,                     &
                    mass_prev,                &  
                    flow,                     &
                    flow_lo,                  &
                    flow_hi,                  &
                    area,                     &
                    area_prev,                & 
                    area_lo,                  &
                    area_hi,                  &
                    n_cell,                   &
                    n_var,                    &
                    dble(current_time)*sixty, &
                    gtm_time_interval*sixty,  &
                    dx_arr,                   &
                    n_node,                   &
                    dsm2_node,                &
                    node_conc,                &
                    .true.)     
        call cons2prim(conc, mass, area, n_cell, n_var)
        cfl = flow/area*(gtm_time_interval*sixty)/dx_arr
        
        conc_prev = conc
        if (apply_diffusion) then
            call dispersion_coef(disp_coef_lo,     &
                                 disp_coef_hi,     &
                                 flow,             &
                                 flow_lo,          &
                                 flow_hi,          &
                                 current_time,     &
                                 dx_arr,           &
                                 gtm_time_interval,&
                                 n_cell,           &
                                 n_var)                                   
            call diffuse(conc,                     &
                         conc_prev,         &
                         area,              &
                         area_prev,         &
                         area_lo,           &
                         area_hi,           &
                         area_lo_prev,      &
                         area_hi_prev,      &
                         disp_coef_lo,      &  
                         disp_coef_hi,      &
                         disp_coef_lo_prev, &  
                         disp_coef_hi_prev, &
                         n_cell,            &
                         n_var,             &
                         current_time,      &
                         theta,             &
                         gtm_time_interval, &
                         dx_arr)
        end if
        call prim2cons(mass,conc,area,n_cell,n_var)
        
        !
        !----- print output to hdf5 file -----
        !              
        if (mod(current_time-gtm_start_jmin,gtm_hdf_time_intvl)==zero) then
            time_index_in_gtm_hdf = (current_time-gtm_start_jmin)/gtm_hdf_time_intvl
            call write_qual_hdf(qual_hdf,                 &
                                conc,                     &
                                n_cell,                   &
                                n_var,                    &
                                time_index_in_gtm_hdf)     
            if (debug_print==.true.) then                                                 
                call write_qual_hdf_ts(qual_hdf%cell_flow_id, &
                                       flow,                  & 
                                       n_cell,                &
                                       time_index_in_gtm_hdf)
                call write_qual_hdf_ts(qual_hdf%cell_area_id, &
                                       area,                  & 
                                       n_cell,                &
                                       time_index_in_gtm_hdf)                               
                call write_qual_hdf_ts(qual_hdf%cell_cfl_id, &
                                       cfl,                  & 
                                       n_cell,                &
                                       time_index_in_gtm_hdf)                                             
            end if
        end if                           
        mass_prev = mass
        area_prev = area                 
    end do
    if (n_dssfiles .ne. 0) then
        call zclose(ifltab_in)  !!ADD A global to detect if dss is opened
        deallocate(ifltab_in) 
    end if   
    deallocate(pathinput)
    deallocate(node_conc)   
    deallocate(constituents)             
    call deallocate_datain             
    call deallocate_geometry
    call deallocate_state
    call deallocate_state_resv
    call deallocate_network_tmp
    call deallocate_hydro_ts
    call close_qual_hdf(qual_hdf)         
    call hdf5_close
    close(debug_unit)
    pause    
end program

