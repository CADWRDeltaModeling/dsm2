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

!> Boundary diffusive flux interface to be fulfilled by driver or application
!>@ingroup gtm
module network_boundary_diffusion

    use gtm_precision
    use error_handling    
    use dispersion_coefficient        
    real(gtm_real), allocatable, save :: disp_coef_arr(:)
    
    contains
    
    !> Set dispersion coefficient interface to an implementation that is constant in space and time
    !> This routine sets the value of the constant dispersion coefficient as well.
    subroutine set_dispersion_arr(d_arr,  &
                                  ncell)

        implicit none
        integer, intent(in) :: ncell
        real(gtm_real),intent(in) :: d_arr(ncell)     !< array of dispersion coefficients
        allocate(disp_coef_arr(ncell))
        disp_coef_arr = d_arr
        dispersion_coef => assign_dispersion_coef
        return
    end subroutine    
    
    !> Implementation of diffusion_coef_if that sets dipsersion to a constant over space and time
    subroutine assign_dispersion_coef(disp_coef_lo,         &
                                      disp_coef_hi,         &
                                      flow,                 &
                                      flow_lo,              &
                                      flow_hi,              &
                                      time,                 &
                                      dx,                   &
                                      dt,                   &
                                      ncell,                &
                                      nvar)  
        use gtm_precision
        use error_handling
      
        implicit none
        !--- args          
        integer,intent(in)  :: ncell                         !< Number of cells
        integer,intent(in)  :: nvar                          !< Number of variables   
        real(gtm_real),intent(in) :: time                    !< Current time
        real(gtm_real),intent(in) :: dx(ncell)               !< Spatial step  
        real(gtm_real),intent(in) :: dt                      !< Time step 
        real(gtm_real),intent(in) :: flow_lo(ncell)          !< flow on lo side of cells centered in time
        real(gtm_real),intent(in) :: flow_hi(ncell)          !< flow on hi side of cells centered in time       
        real(gtm_real),intent(in) :: flow(ncell)             !< flow on center of cells 
        real(gtm_real),intent(out):: disp_coef_lo(ncell)     !< Low side constituent dispersion coef.
        real(gtm_real),intent(out):: disp_coef_hi(ncell)     !< High side constituent dispersion coef. 
        real(gtm_real) :: scaling = 0.1d0
   
        disp_coef_hi = disp_coef_arr*scaling
        disp_coef_lo = disp_coef_arr*scaling
        
        deallocate(disp_coef_arr)
        
        return
    end subroutine
 
 
    !> Diffusion boundary condition that enforces zero constituent flux 
    subroutine network_neumann_zero_boundary_diffusive_flux(diffusive_flux_lo, &
                                               diffusive_flux_hi, &
                                               conc,              &
                                               area_lo,           &
                                               area_hi,           &
                                               disp_coef_lo,      &  
                                               disp_coef_hi,      &
                                               ncell,             &
                                               nvar,              &
                                               time,              &
                                               dx,                &
                                               dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, dsm2_network
        use state_variables_network, only : node_conc
        implicit none
        !--- args
        integer, intent(in)  :: ncell                               !< Number of cells
        integer, intent(in)  :: nvar                                !< Number of variables
        real(gtm_real),intent(inout):: diffusive_flux_lo(ncell,nvar)!< Face flux, lo side
        real(gtm_real),intent(inout):: diffusive_flux_hi(ncell,nvar)!< Face flux, hi side
        real(gtm_real),intent(in)   :: area_lo(ncell)               !< Low side area centered at time
        real(gtm_real),intent(in)   :: area_hi(ncell)               !< High side area centered at time
        real(gtm_real),intent(in)   :: time                         !< Time
        real(gtm_real),intent(in)   :: conc(ncell,nvar)             !< Concentration 
        real(gtm_real),intent(in)   :: disp_coef_lo(ncell)          !< Low side constituent dispersion coef.
        real(gtm_real),intent(in)   :: disp_coef_hi(ncell)          !< High side constituent dispersion coef.
        real(gtm_real),intent(in)   :: dt                           !< Spatial step  
        real(gtm_real),intent(in)   :: dx(ncell)                    !< Time step     
        integer :: i, icell
 
        diffusive_flux_lo(1,:) = zero
        diffusive_flux_hi(ncell,:) = zero
        do i = 1, n_node
            if ( (dsm2_network(i)%boundary_no.ne.0) .and. (dsm2_network(i)%node_conc.eq.1) ) then  !if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary
                    diffusive_flux_lo(icell,:) = zero
                else                                            ! downstream boundary
                    diffusive_flux_hi(icell,:) = zero
                end if    
            end if
        end do 
           
        return
    end subroutine
    

    !> Dirichlet boundaries matrix
    subroutine network_neumann_zero_boundary_diffusion_matrix(center_diag ,       &
                                                           up_diag,            &     
                                                           down_diag,          &
                                                           right_hand_side,    & 
                                                           conc,               &
                                                           explicit_diffuse_op,&
                                                           area,               &
                                                           area_lo,            &
                                                           area_hi,            &          
                                                           disp_coef_lo,       &
                                                           disp_coef_hi,       &
                                                           theta_stm,          &
                                                           ncell,              &
                                                           time,               & 
                                                           nvar,               & 
                                                           dx,                 &
                                                           dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, dsm2_network
        use state_variables_network, only : node_conc
        implicit none
        !--- args                                      
        integer, intent (in) :: ncell                                           !< Number of cells
        integer, intent (in) :: nvar                                            !< Number of variables
        real(gtm_real),intent (inout):: down_diag(ncell,nvar)                   !< Values of the coefficients below diagonal in matrix
        real(gtm_real),intent (inout):: center_diag(ncell,nvar)                 !< Values of the coefficients at the diagonal in matrix
        real(gtm_real),intent (inout):: up_diag(ncell,nvar)                     !< Values of the coefficients above the diagonal in matrix
        real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)             !< Values of the coefficients of the right hand side
        real(gtm_real), intent (in)  :: conc(ncell,nvar)                        !< Concentration
        real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar)         !< Explicit diffusive operator
        real(gtm_real), intent (in)  :: area (ncell)                            !< Cell centered area at new time 
        real(gtm_real), intent (in)  :: area_lo(ncell)                          !< Low side area at new time
        real(gtm_real), intent (in)  :: area_hi(ncell)                          !< High side area at new time 
        real(gtm_real), intent (in)  :: disp_coef_lo (ncell)                    !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: disp_coef_hi (ncell)                    !< High side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: time                                    !< Current time
        real(gtm_real), intent (in)  :: theta_stm                               !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in)  :: dx(ncell)                               !< Spatial step  
        real(gtm_real), intent (in)  :: dt                                      !< Time step     
        !---local
        real(gtm_real) :: dt_by_dxsq(ncell)
        integer :: i, icell
          
        dt_by_dxsq = dt/(dx*dx)

        do i = 1, n_node
            if ( (dsm2_network(i)%boundary_no.ne.0) .and. (dsm2_network(i)%node_conc.eq.1) ) then  !if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary             
                    center_diag(icell,:)= center_diag(icell,:)+ theta_stm*dt_by_dxsq(icell)*area_lo(icell)*disp_coef_lo(icell)    
                    right_hand_side(icell,:) = right_hand_side(icell,:)
                else                                            ! downstream boundary
                    center_diag(icell,:)= center_diag(icell,:)+ theta_stm*dt_by_dxsq(icell)*area_hi(icell)*disp_coef_hi(icell)    
                    right_hand_side(icell,:) = right_hand_side(icell,:)  
                end if    
            end if
        end do 
        return
    end subroutine


    !> Dirichlet boundary condition 
    subroutine network_dirichlet_boundary_diffusive_flux(diffusive_flux_lo, &
                                                         diffusive_flux_hi, &
                                                         conc,              &
                                                         area_lo,           &
                                                         area_hi,           &
                                                         disp_coef_lo,      &  
                                                         disp_coef_hi,      &
                                                         ncell,             &
                                                         nvar,              &
                                                         time,              &
                                                         dx,                &
                                                         dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, dsm2_network
        use state_variables_network, only : node_conc
        implicit none
        !--- args
        integer, intent(in)  :: ncell                               !< Number of cells
        integer, intent(in)  :: nvar                                !< Number of variables
        real(gtm_real),intent(inout):: diffusive_flux_lo(ncell,nvar)!< Face flux, lo side
        real(gtm_real),intent(inout):: diffusive_flux_hi(ncell,nvar)!< Face flux, hi side
        real(gtm_real),intent(in)   :: area_lo(ncell)               !< Low side area centered at time
        real(gtm_real),intent(in)   :: area_hi(ncell)               !< High side area centered at time
        real(gtm_real),intent(in)   :: time                         !< Time
        real(gtm_real),intent(in)   :: conc(ncell,nvar)             !< Concentration 
        real(gtm_real),intent(in)   :: disp_coef_lo(ncell)          !< Low side constituent dispersion coef.
        real(gtm_real),intent(in)   :: disp_coef_hi(ncell)          !< High side constituent dispersion coef.
        real(gtm_real),intent(in)   :: dt                           !< Spatial step  
        real(gtm_real),intent(in)   :: dx(ncell)                    !< Time step     
        integer :: i, icell
 
        diffusive_flux_lo(1,:) = zero
        diffusive_flux_hi(ncell,:) = zero
        do i = 1, n_node
            if ( (dsm2_network(i)%boundary_no.ne.0) .and. (dsm2_network(i)%node_conc.eq.1) ) then  !if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary
                    diffusive_flux_lo(icell,:) = -two*area_lo(icell)*disp_coef_lo(icell)*(conc(icell,:)-node_conc(i,:))/dx(icell)
                else                                            ! downstream boundary
                    diffusive_flux_hi(icell,:) = -two*area_hi(icell)*disp_coef_hi(icell)*(node_conc(i,:)-conc(icell,:))/dx(icell)                    
                end if    
            end if
        end do 
           
        return
    end subroutine


    !> Dirichlet boundaries matrix
    subroutine network_dirichlet_boundary_diffusion_matrix(center_diag ,       &
                                                           up_diag,            &     
                                                           down_diag,          &
                                                           right_hand_side,    & 
                                                           conc,               &
                                                           explicit_diffuse_op,&
                                                           area,               &
                                                           area_lo,            &
                                                           area_hi,            &          
                                                           disp_coef_lo,       &
                                                           disp_coef_hi,       &
                                                           theta_stm,          &
                                                           ncell,              &
                                                           time,               & 
                                                           nvar,               & 
                                                           dx,                 &
                                                           dt)
        use gtm_precision
        use error_handling
        use common_variables, only : n_node, dsm2_network
        use state_variables_network, only : node_conc
        implicit none
        !--- args                                      
        integer, intent (in) :: ncell                                           !< Number of cells
        integer, intent (in) :: nvar                                            !< Number of variables
        real(gtm_real),intent (inout):: down_diag(ncell,nvar)                   !< Values of the coefficients below diagonal in matrix
        real(gtm_real),intent (inout):: center_diag(ncell,nvar)                 !< Values of the coefficients at the diagonal in matrix
        real(gtm_real),intent (inout):: up_diag(ncell,nvar)                     !< Values of the coefficients above the diagonal in matrix
        real(gtm_real),intent (inout):: right_hand_side(ncell,nvar)             !< Values of the coefficients of the right hand side
        real(gtm_real), intent (in)  :: conc(ncell,nvar)                        !< Concentration
        real(gtm_real), intent (in)  :: explicit_diffuse_op(ncell,nvar)         !< Explicit diffusive operator
        real(gtm_real), intent (in)  :: area (ncell)                            !< Cell centered area at new time 
        real(gtm_real), intent (in)  :: area_lo(ncell)                          !< Low side area at new time
        real(gtm_real), intent (in)  :: area_hi(ncell)                          !< High side area at new time 
        real(gtm_real), intent (in)  :: disp_coef_lo (ncell)                    !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: disp_coef_hi (ncell)                    !< High side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: time                                    !< Current time
        real(gtm_real), intent (in)  :: theta_stm                               !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in)  :: dx(ncell)                               !< Spatial step  
        real(gtm_real), intent (in)  :: dt                                      !< Time step     
        !---local
        real(gtm_real) :: dt_by_dxsq(ncell)
        integer :: i, icell
          
        dt_by_dxsq = dt/(dx*dx)

        do i = 1, n_node
            if ( (dsm2_network(i)%boundary_no.ne.0) .and. (dsm2_network(i)%node_conc.eq.1) ) then  !if boundary and node concentration is given
                icell = dsm2_network(i)%cell_no(1)
                if (dsm2_network(i)%up_down(1) .eq. 1) then     ! upstream boundary             
                    center_diag(icell,:)= center_diag(icell,:)+ theta_stm*dt_by_dxsq(icell)*area_lo(icell)*disp_coef_lo(icell)    
                    right_hand_side(icell,:) = right_hand_side(icell,:)               &
                                               + two*theta_stm*dt_by_dxsq(icell)*area_lo(icell)*disp_coef_lo(icell)*node_conc(i,:)
                else                                            ! downstream boundary
                    center_diag(icell,:)= center_diag(icell,:)+ theta_stm*dt_by_dxsq(icell)*area_hi(icell)*disp_coef_hi(icell)    
                    right_hand_side(icell,:) = right_hand_side(icell,:)               &
                                               + two*theta_stm*dt_by_dxsq(icell)*area_hi(icell)*disp_coef_hi(icell)*node_conc(i,:)                                       
                end if    
            end if
        end do 
        return
    end subroutine


end module  