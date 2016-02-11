!<license>
!    Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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

!> Testing advection of mass which is subjected to a tidal boundary with varying dx cells
!>@ingroup test_transport
module test_converge_hydro_interpolation

    use gtm_precision
    !----- module variables
    ! todo: make the names more meaningful
    integer, parameter  :: nconc = 2                              !< Number of constituents
    integer, parameter  :: nstep_base = 128*2                     !< Number of time steps in finer discritization
    integer, parameter  :: nx_base    = 256*2                     !< Number of spatial discritization in finer mesh 
    real(gtm_real),parameter :: origin = zero                     !< Left hand side of the channel
    real(gtm_real),parameter :: domain_length = 204800.d0/two     !< Domain Length in meter
    real(gtm_real),parameter :: amplitude = half                  !< Tidal amplitude in meter    
    real(gtm_real),parameter :: gravity = 9.80d0                  !< Gravitational acceleration in m/s^2
    real(gtm_real),parameter :: depth = 16.0d0                    !< Channel depth in meter
    real(gtm_real),parameter :: sec_per_hr = 60.d0*60.d0          !< Convert factor of hour to second 
    real(gtm_real),parameter :: m2_period = 12.4d0*sec_per_hr     !< M2 tidal period 
    !real(gtm_real),parameter :: m2_period = 16.0d0*sec_per_hr     !< M2 tidal period 
    real(gtm_real),parameter :: freq=two*pi/m2_period             !< Frequency of tidal oscillation
    real(gtm_real),parameter :: dye_length = domain_length/six    !< Length of cosine distribution of mass
    real(gtm_real),parameter :: dye_center = domain_length/two    !< Center of cosine distribution of mass
    real(gtm_real),parameter :: ic_gaussian_sd = domain_length/32.d0   !< Standard deviation of initial values 
    real(gtm_real),parameter :: ic_center = domain_length/two     !< Center of initial condition
    real(gtm_real),parameter :: total_time = m2_period            !< Total time of the test
    real(gtm_real),parameter :: start_time = zero                 !< Starts at zero
    real(gtm_real),parameter :: const_tidal_decay_rate = 1.552749d-5 !< Decay rate (set in the way it reduces half of the mass after one tidal period)
    real(gtm_real) :: flow_mesh_lo_tmp(nstep_base+1,nx_base) !2001=memory_buffer*4+1
    real(gtm_real) :: flow_mesh_hi_tmp(nstep_base+1,nx_base) !156=n_segment*4
    real(gtm_real) :: area_mesh_lo_tmp(nstep_base+1,nx_base)
    real(gtm_real) :: area_mesh_hi_tmp(nstep_base+1,nx_base)

    contains

    !todo: this test could be much less weird-looking if the tidal excursion moved toward the domain
    !      rather than into the domain.
    !      the tidal range is very short, so the discretization of the plume is fairly coarse despite the apparently  
    !      coarse discretization
    !todo: to have an approximation in mind the tide hight is about one meter in Antioch where the water depth is ?? 

    !> Tests the convergence of error rate in advection of mass which is exposed a tidal boundary 
    subroutine test_advection_reaction_hydro_interp(verbose)

        use hydro_data
        use boundary_advection
        use boundary_diffusion
        use gaussian_init_boundary_condition
        use source_sink
        use test_convergence_transport
        use diffusion
        use dispersion_coefficient
     
        implicit none
        procedure(hydro_data_if),pointer :: interp_hydro          !< The pointer points to tidal flow data
        logical :: verbose                                        !< Flag for showing the detail on the screen
        logical :: detail_printout=.true.                         !< Flag for printing out the details
        real(gtm_real) :: fine_initial_condition(nx_base,nconc)   !< initial condition at finest resolution
        real(gtm_real) :: fine_solution(nx_base,nconc)            !< reference solution at finest resolution
        real(gtm_real) :: solution_center = ic_center             !< Center of final solution 
        real(gtm_real) :: solution_gaussian_sd = ic_gaussian_sd   !< Standard deviation of final values
        real(gtm_real) :: tidal_ar_decay_rate                     !< Tidal decay rate
        character(LEN=64) :: label                                !< Test name label
        real(gtm_real) :: acceptance_ratio(3)                     !< Acceptance ratio
        real(gtm_real) :: dx(nx_base)
        integer :: i

        ! define spatial varying dx here
        dx = domain_length/dble(nx_base)

        acceptance_ratio = [2.9, 2.9, 2.9]
 
        interp_hydro=> interp_hydro_data
        ! do not remove it 
        !tidal_hydro=> tidal_flow_cell_average ! this flow generator is NOT mass conservative but it is cell averaged
        advection_boundary_flux => zero_advective_flux !todo: move this so it isn't hardwired
        boundary_diffusion_flux => no_diffusion_flux
        boundary_diffusion_matrix => no_diffusion_matrix

        call set_constant_dispersion(zero)

        tidal_ar_decay_rate = zero
        compute_source => no_source

        call fill_hydro_network(4)

        label = 'advection_tidal_gaussian_interp' 

        ! load the initial values and reference final values to feed the test routine
        call initial_fine_solution_tidal_gaussian_interp(fine_initial_condition, &
                                                          fine_solution,          &
                                                          nx_base,                &
                                                          nconc,                  &
                                                          origin,                 &
                                                          domain_length,          &
                                                          dx,                     &
                                                          ic_gaussian_sd,         &
                                                          solution_gaussian_sd,   &
                                                          ic_center,              &
                                                          solution_center,        &        
                                                          tidal_ar_decay_rate)

        ! The general subroutine which gets the fine initial and reference values from the privious subroutine and 
        ! compute the norms, after each step coarsen the values and repeat computation.
        ! at the end  calculates the ratio of the norms and prints a log 
        call test_convergence(label,                             &
                              interp_hydro,                      &
                              zero_advective_flux,               &
                              no_diffusion_flux,                 &
                              no_diffusion_matrix,               &
                              no_source,                         &
                              domain_length,                     &
                              total_time,                        &
                              start_time,                        &
                              fine_initial_condition,            &
                              fine_solution,                     &            
                              nstep_base,                        &
                              nx_base,                           &
                              nconc,                             &
                              dx,                                &
                              verbose,                           &
                              .true.,                            &
                              acceptance_ratio)
                      
        label = "advection_tidal_sinusoidal_interp" 
        ! load the initial values and reference final values to feed the test routine
        call initial_fine_solution_tidal_sinusoidal_interp(fine_initial_condition, &
                                                            fine_solution,          &
                                                            nx_base,                &
                                                            nconc,                  &
                                                            domain_length,          &
                                                            dx,                     &
                                                            dye_center,             &
                                                            dye_length,             &
                                                            tidal_ar_decay_rate)

        ! The general subroutine which gets the fine initial and reference values from the privious subroutine and 
        ! compute the norms, after each step coarsen the values and repeat computation.
        ! at the end  calculates the ratio of the norms and prints a log 
        call test_convergence(label,                             &
                              interp_hydro,                      &
                              zero_advective_flux,               &
                              no_diffusion_flux,                 &
                              no_diffusion_matrix,               &
                              no_source,                         &
                              domain_length,                     &
                              total_time,                        &
                              start_time,                        &
                              fine_initial_condition,            &
                              fine_solution,                     &            
                              nstep_base,                        &
                              nx_base,                           &
                              nconc,                             &
                              dx,                                &
                              verbose,                           &
                              .true.,                            &
                              acceptance_ratio)

        !!!!!!!!!!!!!!!!!!!!!!
        tidal_ar_decay_rate = const_tidal_decay_rate
        compute_source => tidal_reaction_source_interp

        label = 'advection_reaction_tidal_gaussian_interp' 

        ! load the initial values and reference final values to feed the test routine
        call initial_fine_solution_tidal_gaussian_interp(fine_initial_condition, &
                                                          fine_solution,          &
                                                          nx_base,                &
                                                          nconc,                  &
                                                          origin,                 &
                                                          domain_length,          &
                                                          dx,                     &
                                                          ic_gaussian_sd,         &
                                                          solution_gaussian_sd,   &
                                                          ic_center,              &
                                                          solution_center,        &        
                                                          tidal_ar_decay_rate)

        ! The general subroutine which gets the fine initial and reference values from the privious subroutine and 
        ! compute the norms, after each step coarsen the values and repeat computation.
        ! at the end  calculates the ratio of the norms and prints a log 
        call test_convergence(label,                             &
                              interp_hydro,                      &
                              zero_advective_flux,               &
                              no_diffusion_flux,                 &
                              no_diffusion_matrix,               &
                              no_source,                         &
                              domain_length,                     &
                              total_time,                        &
                              start_time,                        &
                              fine_initial_condition,            &
                              fine_solution,                     &            
                              nstep_base,                        &
                              nx_base,                           &
                              nconc,                             &
                              dx,                                &
                              verbose,                           &
                              .true.,                            &
                              acceptance_ratio)
                      
        label = "advection_reaction_tidal_sinusoidal_dx" 
        ! load the initial values and reference final values to feed the test routine
        call initial_fine_solution_tidal_sinusoidal_interp(fine_initial_condition, &
                                                            fine_solution,          &
                                                            nx_base,                &
                                                            nconc,                  &
                                                            domain_length,          &
                                                            dx,                     &
                                                            dye_center,             &
                                                            dye_length,             &
                                                            tidal_ar_decay_rate)
 
        ! The general subroutine which gets the fine initial and reference values from the privious subroutine and 
        ! compute the norms, after each step coarsen the values and repeat computation.
        ! at the end  calculates the ratio of the norms and prints a log 
        call test_convergence(label,                             &
                              interp_hydro,                      &
                              zero_advective_flux,               &
                              no_diffusion_flux,                 &
                              no_diffusion_matrix,               &
                              no_source,                         &
                              domain_length,                     &
                              total_time,                        &
                              start_time,                        &
                              fine_initial_condition,            &
                              fine_solution,                     &            
                              nstep_base,                        &
                              nx_base,                           &
                              nconc,                             &
                              dx,                                &
                              verbose,                           &
                              .true.,                            &
                              acceptance_ratio)
        return                  
    end subroutine
    
    !-------------------------------------------
    !> Generates a fine initial and final solution of a Gaussian  mass distribution with varying dx
    subroutine initial_fine_solution_tidal_gaussian_interp(fine_initial_condition, &
                                                            fine_solution,          &
                                                            nx_base,                &
                                                            nconc,                  &
                                                            origin,                 &
                                                            domain_length,          &
                                                            dx,                     &
                                                            ic_gaussian_sd,         &
                                                            solution_gaussin_sd,    &
                                                            ic_center,              &
                                                            solution_center,        &        
                                                            tidal_ar_decay_rate)

        use gaussian_init_boundary_condition
        use gtm_precision

        implicit none

        integer,intent(in) :: nconc 
        integer,intent(in) :: nx_base 
        real(gtm_real), intent(out) :: fine_initial_condition(nx_base,nconc) !< initial condition at finest resolution
        real(gtm_real), intent(out) :: fine_solution(nx_base,nconc)          !< reference solution at finest resolution
        real(gtm_real), intent(in)  :: dx(nx_base)                           !< dx
        real(gtm_real), intent(in)  :: ic_center                             !< Center of fine initial value
        real(gtm_real), intent(in)  :: solution_center                       !< Center of solution
        real(gtm_real), intent(in)  :: ic_gaussian_sd                        !< Standard deviation of initial value
        real(gtm_real), intent(in)  :: solution_gaussin_sd                   !< Standard deviation of solution 
        real(gtm_real), intent(in)  :: origin                                !< Left hand side of the channel
        real(gtm_real), intent(in)  :: domain_length                         !< Domain length
        real(gtm_real), intent(in)  :: tidal_ar_decay_rate                   !< Decay rate
        !----local
        real(gtm_real):: xposition(nx_base)
        integer :: icell

        call fill_gaussian_vary_dx(fine_initial_condition(:,1),nx_base,origin,dx, &
                                   ic_center,ic_gaussian_sd,one)
        call fill_gaussian_vary_dx(fine_initial_condition(:,2),nx_base,origin,dx, &
                                   ic_center,ic_gaussian_sd,one)
        call fill_gaussian_vary_dx(fine_solution(:,1),nx_base,origin,dx, &
                                   solution_center,solution_gaussin_sd,one)
        call fill_gaussian_vary_dx(fine_solution(:,2),nx_base,origin,dx, &
                                   solution_center,solution_gaussin_sd,one)

        fine_initial_condition(:,2) = fine_initial_condition(:,1)
        fine_solution = dexp(-total_time*tidal_ar_decay_rate)*fine_initial_condition

        return
    end subroutine

    !-------------------------------------------
    !> Generates a fine initial and final solution of a sinusoidal  mass distribution  with varying dx
    subroutine initial_fine_solution_tidal_sinusoidal_interp(fine_initial_condition, &
                                                              fine_solution,          &
                                                              nx_base,                &
                                                              nconc,                  &
                                                              domain_length,          &
                                                              dx,                     &
                                                              dye_center,             &
                                                              dye_length,             &
                                                              tidal_ar_decay_rate)
        use gaussian_init_boundary_condition
        use gtm_precision
        implicit none

        integer,intent(in) :: nconc 
        integer,intent(in) :: nx_base 
        real(gtm_real), intent(out) :: fine_initial_condition(nx_base,nconc) !< initial condition at finest resolution
        real(gtm_real), intent(out) :: fine_solution(nx_base,nconc)          !< reference solution at finest resolution
        real(gtm_real), intent(in)  :: dx(nx_base)                           !< dx   
        real(gtm_real), intent(in)  :: domain_length                         !< Domain length
        real(gtm_real), intent(in)  :: dye_center                            !< center of sinusoidal mass
        real(gtm_real), intent(in)  :: dye_length                            !< length of mass at the middle of the domain
        real(gtm_real), intent(in)  :: tidal_ar_decay_rate                   !< Decay rate
        !----local
        real(gtm_real):: xposition
        real(gtm_real):: x_lo
        real(gtm_real):: x_hi
        integer :: icell

        x_lo = zero
        xposition = zero
        x_hi = zero
        do icell=1,nx_base
            x_lo      = x_hi
            xposition = xposition + half*dx(icell)
            x_hi      = x_hi + dx(icell)
            if (( x_lo > (dye_center + dye_length*half)) .or. & 
                ( x_hi < (dye_center - dye_length*half))  ) then
                fine_initial_condition(icell,1) = zero
            else
                fine_initial_condition(icell,1)= one + &
                     (dye_length*half/pi)*(dsin((x_hi - dye_center)*two*pi/dye_length) - &
                      dsin((x_lo-dye_center)*two*pi/dye_length))/dx(icell)
            end if 
        end do
        fine_initial_condition(:,2)=fine_initial_condition(:,1)
        fine_solution = exp(-total_time*tidal_ar_decay_rate)*fine_initial_condition
        return
    end subroutine

    !> fill hydro network by interpolation
    subroutine fill_hydro_network(nx)
        use common_variables
        use common_xsect 
        use gtm_network
        implicit none
        integer, intent(in) :: nx
        real(gtm_real), allocatable :: prev_flow(:),prev_ws(:)
        real(gtm_real), allocatable :: prev_flow_cell_lo(:), prev_flow_cell_hi(:)
        real(gtm_real), parameter :: m2ft = 3.28084d0
        integer :: npart_t, nt
        integer :: start_t_index, end_t_index
        integer :: i, j
 
        n_chan = 1
        n_segm = 128
        n_comp = n_segm + 1
        n_irreg = n_segm + 1
        n_resv = 0 
        n_resv_conn = 0 
        n_qext = 0 
        n_tran = 0
        memory_buffer = nstep_base/4 + 1
      
        allocate(chan_geom(1))
        chan_geom(1)%channel_length = domain_length
 
        allocate(segm(n_segm))
        segm(:)%length = domain_length/n_segm
        do i= 1, n_segm
            segm(i)%chan_no = 1
            segm(i)%up_distance = (i-1)*domain_length/n_segm
            segm(i)%up_comppt = i
            segm(i)%down_comppt = i+1
            segm(i)%nx = nx
            segm(i)%start_cell_no = (i-1)*nx + 1
        enddo    
        
        call allocate_chan_virt_xsect
        call allocate_virt_xsec_geom
        num_elev_chan(1) = 6
        num_xsect_chan(1) = n_irreg
        do i = 1, n_irreg
            virt_xsect(i)%chan_no = 1
            virt_xsect(i)%min_elev = 0.d0
            virt_xsect(i)%num_elev = 6
            virt_xsect(i)%ID = i
            virt_xsect(i)%vsecno = i
            virt_xsect(i)%elevation(1:6) = (/ 0.d0, 20.d0, 40.d0,60.d0, 80.d0,100.d0 /)                                              
            virt_xsect(i)%area(1:6) =  (/ 0.d0, 65.6168d0, 131.2336d0, 196.8504d0, 262.467d0, 328.084d0 /)  
            virt_xsect(i)%width(1:6) = (/ 3.28084d0, 3.28084d0,3.28084d0,3.28084d0,3.28084d0,3.28084d0 /)
            virt_xsect(i)%wet_p(1:6) = (/ 3.28084d0, 43.28084d0, 83.28084d0, 123.28084d0, 163.28084d0, 203.28084d0/)                 
        end do

        npart_t = 4
        nt = npart_t + 1
        n_cell = n_segm * 4
        hydro_time_interval = m2_period/(nstep_base/4)
        call allocate_network_tmp(npart_t)        
        call deallocate_hydro_ts  !todo:bad, need to find where it is from previous calls
        call allocate_hydro_ts
        allocate(prev_flow(n_comp),prev_ws(n_comp))
        allocate(prev_flow_cell_lo(n_cell), prev_flow_cell_hi(n_cell))
        open(401,file='flow.txt')
        open(402,file='ws.txt')
        do i = 1, memory_buffer
            read(401,*) (hydro_flow(j,i), j=1,n_comp)
            read(402,*) (hydro_ws(j,i), j=1,n_comp)
        end do
        prev_flow = hydro_flow(:,1)        
        prev_ws = hydro_ws(:,1)
        prev_flow_cell_lo = LARGEREAL
        prev_flow_cell_hi = LARGEREAL

        do i = 2, memory_buffer
            call interp_network_linear(npart_t, i, n_comp, prev_flow, prev_ws, n_cell, prev_flow_cell_lo, prev_flow_cell_hi)
            start_t_index = (i-2)*npart_t + 1
            end_t_index = (i-1)*npart_t
            flow_mesh_lo_tmp(start_t_index:end_t_index,:) = flow_mesh_lo(1:npart_t,:)
            flow_mesh_hi_tmp(start_t_index:end_t_index,:) = flow_mesh_hi(1:npart_t,:)
            area_mesh_lo_tmp(start_t_index:end_t_index,:) = area_mesh_lo(1:npart_t,:)
            area_mesh_hi_tmp(start_t_index:end_t_index,:) = area_mesh_hi(1:npart_t,:)
            prev_flow = hydro_flow(:,i)
            prev_ws = hydro_ws(:,i)
            prev_flow_cell_lo =  flow_mesh_lo(nt,:)
            prev_flow_cell_hi =  flow_mesh_hi(nt,:)
        end do    
        flow_mesh_lo_tmp(end_t_index+1,:) = flow_mesh_lo(npart_t+1,:)
        flow_mesh_hi_tmp(end_t_index+1,:) = flow_mesh_hi(npart_t+1,:)
        area_mesh_lo_tmp(end_t_index+1,:) = area_mesh_lo(npart_t+1,:)
        area_mesh_hi_tmp(end_t_index+1,:) = area_mesh_hi(npart_t+1,:)      
        flow_mesh_lo_tmp = flow_mesh_lo_tmp/(m2ft*3)
        flow_mesh_hi_tmp = flow_mesh_hi_tmp/(m2ft*3)
        area_mesh_lo_tmp = area_mesh_lo_tmp/(m2ft*2)
        area_mesh_hi_tmp = area_mesh_hi_tmp/(m2ft*2)        
        close(401)
        close(402)
        n_chan = LARGEINT
        n_segm = LARGEINT
        n_comp = LARGEINT
        n_irreg = LARGEINT
        n_resv = LARGEINT 
        n_resv_conn = LARGEINT 
        n_qext = LARGEINT 
        n_tran = LARGEINT
        memory_buffer = LARGEINT
        hydro_time_interval = LARGEREAL
        deallocate(segm)
        deallocate(chan_geom)
        deallocate(prev_flow,prev_ws)
        deallocate(prev_flow_cell_lo, prev_flow_cell_hi)
        call deallocate_virt_xsect
        call deallocate_hydro_ts
        return
    end subroutine        

    !> Tidal flow for a rectangular basin with periodic forcing in the cell centerd 
    !> the area here retrived from dQ/dx+dA/dt=0 --> A =width*(depth+int(-dQ/dx,t)) 
    subroutine interp_hydro_data(flow,     &
                                 flow_lo,  &
                                 flow_hi,  &
                                 area,     &
                                 area_lo,  &
                                 area_hi,  &
                                 ncell,    &
                                 time,     &
                                 dx,       &
                                 dt)
        use common_variables
        use common_xsect 
        use gtm_network                      
        implicit none
        integer, intent(in) :: ncell                   !< number of cells
        real(gtm_real), intent(in)  :: time            !< time of request
        real(gtm_real), intent(in)  :: dx(ncell)       !< spatial step 
        real(gtm_real), intent(in)  :: dt              !< time step 
        real(gtm_real), intent(out) :: flow(ncell)     !< cell centered flow
        real(gtm_real), intent(out) :: flow_lo(ncell)  !< lo face flow
        real(gtm_real), intent(out) :: flow_hi(ncell)  !< hi face flow
        real(gtm_real), intent(out) :: area(ncell)     !< cell center area
        real(gtm_real), intent(out) :: area_lo(ncell)  !< area lo face
        real(gtm_real), intent(out) :: area_hi(ncell)  !< area hi face
        !--- local
        integer :: i
        real(gtm_real) :: fine_time_dt
        integer :: t_index

        fine_time_dt = total_time/nstep_base
        if (dt.eq. fine_time_dt) then
            t_index = int(time/dt)+1
            flow_lo = flow_mesh_lo_tmp(t_index,:)
            flow_hi = flow_mesh_hi_tmp(t_index,:)
            area_lo = area_mesh_lo_tmp(t_index,:)
            area_hi = area_mesh_hi_tmp(t_index,:)
            flow = half*(flow_lo+flow_hi)
            area = half*(area_lo+area_hi)
        elseif (dt.eq. fine_time_dt*two) then
            t_index = int(time/dt*two)+1
            do i = 1, ncell
            flow_lo(i) = flow_mesh_lo_tmp(t_index,i*2)
            flow_hi(i) = flow_mesh_hi_tmp(t_index,i*2)
            area_lo(i) = area_mesh_lo_tmp(t_index,i*2)
            area_hi(i) = area_mesh_hi_tmp(t_index,i*2)
            end do
            flow = half*(flow_lo+flow_hi)
            area = half*(area_lo+area_hi)                        
        elseif  (dt.eq. fine_time_dt*four) then
            t_index = int(time/dt*four)+1
            do i = 1, ncell
            flow_lo(i) = flow_mesh_lo_tmp(t_index,i*4)
            flow_hi(i) = flow_mesh_hi_tmp(t_index,i*4)
            area_lo(i) = area_mesh_lo_tmp(t_index,i*4)
            area_hi(i) = area_mesh_hi_tmp(t_index,i*4)
            end do
            flow = half*(flow_lo+flow_hi)
            area = half*(area_lo+area_hi)        
        end if
        return
    end subroutine 


    !> Subroutine provides source term (constant decay) to the tidal test
    subroutine tidal_reaction_source_interp(source,      & 
                                            conc,        &
                                            area,        &
                                            flow,        &
                                            ncell,       &
                                            nvar,        &
                                            time)
        use  primitive_variable_conversion
        implicit none
        !--- args
        integer,intent(in)  :: ncell                      !< Number of cells
        integer,intent(in)  :: nvar                       !< Number of variables
        real(gtm_real),intent(inout):: source(ncell,nvar) !< cell centered source 
        real(gtm_real),intent(in)   :: conc(ncell,nvar)   !< Concentration
        real(gtm_real),intent(in)   :: area(ncell)        !< area at source     
        real(gtm_real),intent(in)   :: flow(ncell)        !< flow at source location
        real(gtm_real),intent(in)   :: time               !< time 
        !--- local
        integer :: ivar
        do ivar = 1, nvar
            source(:,ivar) = -const_tidal_decay_rate*conc(:,ivar)*area(:)
        enddo
        return
     end subroutine 

end module
