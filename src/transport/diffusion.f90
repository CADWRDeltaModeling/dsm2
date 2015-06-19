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
!> Module orchestrating the diffusion scheme. The main
!> routine in the module is diffuse().
!> Explicit and implicit diffusion operators are included here.
!>@ingroup transport
module diffusion

    contains

    !> Logical function to check whether to use diffusion or not
    logical function use_diffusion()
        use boundary_diffusion
        implicit none
        use_diffusion = associated(boundary_diffusion_flux) .and. &
                       (.not. associated(boundary_diffusion_flux, no_diffusion_flux))
        return
    end function

 
    !> Calculates the diffusive portion of the constituent transport.
    !> It contains an explicit version of the diffusion operator and a general (involving all
    !> potential cases) diffusion operator as well, with a coefficient theta_gtm for 
    !> selecting the level of implicitness. (theta_gtm=0.5 is Crank Nicolson.).
    !> The matrix is solved via a tri-diagonal solver.  !
    !> The algoritm looks like this:
    !>   - This creates the diffusive fluxes sends them for modification for boundaries and then differences the fluxes to get the operator d/dx(Ad/dx). 
    !>         - Calculate interior and boundary fluxes 
    !>   - Construct right hand side with neumann boundary condition imposed
    !>   - Construct diffusion coefficeint matrix with neumann boundary condition imposed
    !>   - Solve the system
    subroutine diffuse(conc,              &
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
                       ncell,             &
                       nvar,              &
                       time_new,          &
                       theta_gtm,         &
                       dt,                &
                       dx)

        use gtm_precision
        use primitive_variable_conversion 
        use boundary_diffusion 

        implicit none
        ! ---- args
        integer, intent (in) :: ncell                                !< Number of cells
        integer, intent (in) :: nvar                                 !< Number of variables 

        real(gtm_real), intent (out):: conc(ncell,nvar)              !< Concentration at new time
        real(gtm_real), intent (in) :: conc_prev(ncell,nvar)         !< Concentration at old time
        real(gtm_real), intent (in) :: area (ncell)                  !< Cell-centered area at new time
        real(gtm_real), intent (in) :: area_prev (ncell)             !< Cell-centered area at old time
        real(gtm_real), intent (in) :: area_lo (ncell)               !< Low side area centered in time
        real(gtm_real), intent (in) :: area_hi (ncell)               !< High side area centered in time 
        real(gtm_real), intent (in) :: area_lo_prev (ncell)          !< Low side area centered at old time
        real(gtm_real), intent (in) :: area_hi_prev (ncell)          !< High side area centered at old time 
        real(gtm_real), intent (in) :: disp_coef_lo (ncell,nvar)     !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent (in) :: disp_coef_hi (ncell,nvar)     !< High side constituent dispersion coef. at new time
        real(gtm_real), intent (in) :: disp_coef_lo_prev(ncell,nvar) !< Low side constituent dispersion coef. at old time
        real(gtm_real), intent (in) :: disp_coef_hi_prev(ncell,nvar) !< High side constituent dispersion coef. at old time
        real(gtm_real), intent (in) :: time_new                      !< Instantaneous "new" time to which we are advancing
        real(gtm_real), intent (in) :: theta_gtm                     !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in) :: dt                            !< Time step   
        real(gtm_real), intent (in) :: dx(ncell)                     !< Spacial step 

        !---- locals
        real(gtm_real) :: explicit_diffuse_op(ncell,nvar)             !< Explicit diffusive operator
        real(gtm_real) :: down_diag(ncell)                       !< Values of the coefficients below diagonal in matrix
        real(gtm_real) :: center_diag(ncell)                     !< Values of the coefficients at the diagonal in matrix
        real(gtm_real) :: up_diag(ncell)                         !< Values of the coefficients above the diagonal in matrix
        real(gtm_real) :: right_hand_side(ncell,nvar)                 !< Right hand side vector
        real(gtm_real) :: time_prev                                   !< old time 

        ! This routine gives the effects of diffusion fluxes on each cell
        ! for a single time step (ie, explicit). This is needed for the advection step.
        ! It is also part of the right hand side of the implicit diffusion solver 
        ! matrix calculation.  

        ! Explicit diffusion operator construction. This creates the diffusive fluxes
        ! sends them for modification for boundaries and then differences the fluxes
        ! to get the operator d/dx(Ad/dx). 
        ! instantaneous function
        time_prev = time_new - dt 

        call explicit_diffusion_operator(explicit_diffuse_op,&
                                         conc_prev,          &
                                         area_lo_prev,       &
                                         area_hi_prev,       &
                                         disp_coef_lo_prev,  &  
                                         disp_coef_hi_prev,  &
                                         ncell,              &
                                         nvar,               &
                                         time_prev,          &
                                         dx,                 &
                                         dt)

        call construct_right_hand_side(right_hand_side,       & 
                                       explicit_diffuse_op,   & 
                                       area_prev,             &
                                       area_lo_prev,          &
                                       area_hi_prev,          &
                                       disp_coef_lo_prev,     &
                                       disp_coef_hi_prev,     &
                                       conc_prev,             &
                                       theta_gtm,             &
                                       ncell,                 &
                                       time_prev,             &
                                       nvar,                  &  
                                       dx,                    &
                                       dt)
                                                                      
        ! Construct the matrix for the diffusion solver
        ! without boundary condition modification or structure on interior of domain
        call construct_diffusion_matrix(center_diag ,     &
                                        up_diag,          &     
                                        down_diag,        &
                                        area,             &
                                        area_lo,          &
                                        area_hi,          &
                                        disp_coef_lo,     &
                                        disp_coef_hi,     &
                                        theta_gtm,        &
                                        ncell,            &
                                        time_new,         & 
                                        nvar,             & 
                                        dx,               &
                                        dt)
                                  
        call boundary_diffusion_matrix(center_diag ,       &
                                       up_diag,            &     
                                       down_diag,          &
                                       right_hand_side,    & 
                                       conc_prev,          &
                                       explicit_diffuse_op,&
                                       area,               &
                                       area_lo,            &
                                       area_hi,            &          
                                       disp_coef_lo,       &
                                       disp_coef_hi,       &
                                       theta_gtm,          &
                                       ncell,              &
                                       time_new,           & 
                                       nvar,               & 
                                       dx,                 &
                                       dt)
        
        !call klu_solve_for_tri(center_diag ,     &
        call solve(center_diag ,     &
                   up_diag,          &     
                   down_diag,        &
                   right_hand_side,  &
                   conc,             &
                   ncell,            &
                   nvar)

        return
    end subroutine 
 

    !> Calculate the explicit diffusion operator for a moment in time.
    !> Explicit diffusion operator construction. This creates the diffusive fluxes
    !> sends them for modification for boundaries and then differences the fluxes
    !> to get the operator d/dx(Ad/dx). 
    subroutine explicit_diffusion_operator(explicit_diffuse_op,  &
                                           conc,                 &
                                           area_lo,              &
                                           area_hi,              &
                                           disp_coef_lo,         &  
                                           disp_coef_hi,         &
                                           ncell,                &
                                           nvar,                 &
                                           time,                 &
                                           dx,                   &
                                           dt)                                                                                          
        use gtm_precision
        use boundary_diffusion

        implicit none 
        !----args
        integer,intent(in) :: ncell                                 !< Number of cells
        integer,intent(in) :: nvar                                  !< Number of variables
        real(gtm_real),intent(out):: explicit_diffuse_op(ncell,nvar)!< Explicit diffusion operator
        real(gtm_real),intent(in) :: conc(ncell,nvar)               !< Concentration at old time
        real(gtm_real),intent(in) :: area_lo(ncell)                 !< Low side area at old time
        real(gtm_real),intent(in) :: area_hi(ncell)                 !< High side area at old time 
        real(gtm_real),intent(in) :: disp_coef_lo(ncell)            !< Low side constituent dispersion coef. at old time
        real(gtm_real),intent(in) :: disp_coef_hi(ncell)            !< High side constituent dispersion coef. at old time
        real(gtm_real),intent(in) :: time                           !< Current time
        real(gtm_real),intent(in) :: dx(ncell)                      !< Spacial step  
        real(gtm_real),intent(in) :: dt                             !< Time step
        !--- locals
        integer :: ivar
        integer :: icell
        real(gtm_real):: diffusive_flux_lo(ncell,nvar)
        real(gtm_real):: diffusive_flux_hi(ncell,nvar)
        explicit_diffuse_op = LARGEREAL

        call diffusive_flux(diffusive_flux_lo,&
                            diffusive_flux_hi,&
                            conc,             &
                            area_lo,          &
                            area_hi,          &
                            disp_coef_lo,     &  
                            disp_coef_hi,     &
                            ncell,            &
                            nvar,             &
                            time,             &
                            dx,               &
                            dt)
          
        do ivar = 1,nvar
            explicit_diffuse_op(:,ivar) = (diffusive_flux_hi(:,ivar) - diffusive_flux_lo(:,ivar))/dx
        end do

        return
    end subroutine 

    !> Estimates the diffusive flux for a moment in time
    subroutine diffusive_flux(diffusive_flux_lo,  &
                              diffusive_flux_hi,  &
                              conc,               &
                              area_lo,            &
                              area_hi,            &
                              disp_coef_lo,       &  
                              disp_coef_hi,       &
                              ncell,              &
                              nvar,               &
                              time,               &
                              dx,                 &
                              dt)
        use gtm_precision
        use boundary_diffusion

        ! --- args
        implicit none
                                             
        integer, intent (in) :: ncell                                            !< Number of cells
        integer, intent (in) :: nvar                                             !< Number of variables

        real(gtm_real),intent(out):: diffusive_flux_hi(ncell,nvar)               !< Explicit diffusive flux high side
        real(gtm_real),intent(out):: diffusive_flux_lo(ncell,nvar)               !< Explicit diffusive flux low side
        real(gtm_real),intent(in) :: conc(ncell,nvar)                            !< Concentration at old time
        real(gtm_real),intent(in) :: area_lo(ncell)                              !< Low side area at old time
        real(gtm_real),intent(in) :: area_hi(ncell)                              !< High side area at old time 
        real(gtm_real),intent(in) :: disp_coef_lo(ncell)                         !< Low side constituent dispersion coef. at old time
        real(gtm_real),intent(in) :: disp_coef_hi(ncell)                         !< High side constituent dispersion coef. at old time
        real(gtm_real),intent(in) :: time                                        !< Current time
        real(gtm_real),intent(in) :: dx(ncell)                                   !< Spatial step   
        real(gtm_real),intent(in) :: dt                                          !< Time step

        !--- local
        integer :: icell 
        integer :: ivar 

        do ivar = 1,nvar
            diffusive_flux_lo(2:ncell,ivar) = &
                   -(area_lo(2:ncell)*disp_coef_lo(2:ncell)* &
                  (conc(2:ncell,ivar) - conc(1:(ncell-1),ivar)))/(half*dx(2:ncell)+half*dx(1:ncell-1))            
            diffusive_flux_hi(1:(ncell-1),ivar) = &
                   -(area_hi(1:(ncell-1))*disp_coef_hi(1:(ncell-1))* &
                   (conc(2:ncell,ivar) - conc(1:(ncell-1),ivar)))/(half*dx(2:ncell)+half*dx(1:ncell-1))                                  
        end do 
        diffusive_flux_hi(ncell,:) = LARGEREAL
        diffusive_flux_lo(1,:) = LARGEREAL

        call boundary_diffusion_flux(diffusive_flux_lo, &
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
        return
    end subroutine 

    !> Construct the right hand side vector from previous step,
    !> and impose Neumann boundary condition on it.
    !todo remove disp_coef area_lo and hi
    pure subroutine construct_right_hand_side(right_hand_side,       & 
                                              explicit_diffuse_op,   & 
                                              area_prev,             &
                                              area_lo_prev,          &
                                              area_hi_prev,          &
                                              disp_coef_lo_prev,     &
                                              disp_coef_hi_prev,     &
                                              conc_prev,             &
                                              theta,                 &
                                              ncell,                 &
                                              time,                  &
                                              nvar,                  &  
                                              dx,                    &
                                              dt)
        use gtm_precision   
        ! ---args  
        implicit none                              
        integer, intent (in) :: ncell                                     !< Number of cells
        integer, intent (in) :: nvar                                      !< Number of variables
        real(gtm_real), intent (out) :: right_hand_side(ncell,nvar)       !< The right hand side vector
        real(gtm_real), intent (in)  :: explicit_diffuse_op (ncell,nvar)  !< Explicit diffusion operator
        real(gtm_real), intent (in)  :: area_prev (ncell)                 !< Cell centered area at old time 
        real(gtm_real), intent (in)  :: conc_prev(ncell,nvar)             !< Concentration at old time
        real(gtm_real), intent (in)  :: area_lo_prev (ncell)              !< Low side area at old time
        real(gtm_real), intent (in)  :: area_hi_prev (ncell)              !< High side area at old time 
        real(gtm_real), intent (in)  :: disp_coef_lo_prev(ncell)          !< Low side constituent dispersion coef. at old time
        real(gtm_real), intent (in)  :: disp_coef_hi_prev(ncell)          !< High side constituent dispersion coef. at old time
        real(gtm_real), intent (in)  :: time                              !< Current time
        real(gtm_real), intent (in)  :: theta                             !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in)  :: dx(ncell)                         !< Spatial step  
        real(gtm_real), intent (in)  :: dt                                !< Time step 
  
        !---- locals
        integer :: ivar
        integer :: icell

        do ivar = 1,nvar
            right_hand_side(:,ivar) = area_prev(:)*conc_prev(:,ivar) &
                                      - (one-theta)*dt*explicit_diffuse_op(:,ivar) 
        end do
        return
    end subroutine

    !> Construct the matrix for the diffusion solver  
    !> without boundary condition modification or structure on interior of domain
    subroutine construct_diffusion_matrix(center_diag ,     &
                                          up_diag,          &     
                                          down_diag,        &
                                          area,             &
                                          area_lo,          &
                                          area_hi,          &
                                          disp_coef_lo,     &
                                          disp_coef_hi,     &
                                          theta_gtm,        &
                                          ncell,            &
                                          time,             & 
                                          nvar,             & 
                                          dx,               &
                                          dt)
        use gtm_precision
                                  
        ! ---args    
        implicit none                            
        integer, intent (in) :: ncell                            !< Number of cells
        integer, intent (in) :: nvar                             !< Number of variables
        real(gtm_real), intent (out) :: down_diag(ncell)         !< Values of the coefficients below diagonal in matrix
        real(gtm_real), intent (out) :: center_diag(ncell)       !< Values of the coefficients at the diagonal in matrix
        real(gtm_real), intent (out) :: up_diag(ncell)           !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent (in)  :: area (ncell)             !< Cell centered area at new time 
        real(gtm_real), intent (in)  :: area_lo(ncell)           !< Low side area at new time
        real(gtm_real), intent (in)  :: area_hi(ncell)           !< High side area at new time 
        real(gtm_real), intent (in)  :: disp_coef_lo(ncell)      !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: disp_coef_hi(ncell)      !< High side constituent dispersion coef. at new time
        real(gtm_real), intent (in)  :: time                     !< Current time
        real(gtm_real), intent (in)  :: theta_gtm                !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit  
        real(gtm_real), intent (in)  :: dx(ncell)                !< Spatial step  
        real(gtm_real), intent (in)  :: dt                       !< Time step                                   
                                  
        !---local                                  
        real(gtm_real) :: explicit_diffuse_op(ncell,nvar)       !< Explicit diffusion operator
        real(gtm_real) :: lo_face
        real(gtm_real) :: hi_face 
     
        integer :: icell
        integer :: ivar
        up_diag(ncell) = LARGEREAL
        down_diag(1) = LARGEREAL  
        do icell = 2,ncell-1
            lo_face = theta_gtm*dt/dx(icell)*area_lo(icell)*disp_coef_lo(icell)/(half*dx(icell)+half*dx(icell-1))
            hi_face = theta_gtm*dt/dx(icell)*area_hi(icell)*disp_coef_hi(icell)/(half*dx(icell)+half*dx(icell+1))
            down_diag(icell) = - lo_face
            center_diag(icell) = area(icell) + hi_face + lo_face
            up_diag(icell) = - hi_face
        end do         
        down_diag(1) = down_diag(2)
        center_diag(1) = center_diag(2)
        up_diag(1) = up_diag(2)
        down_diag(ncell) = down_diag(ncell-1)
        center_diag(ncell) = center_diag(ncell-1)
        up_diag(ncell) = up_diag(ncell-1)     

        return
    end subroutine 

    !> Solve the system of linear equations
    subroutine solve(center_diag ,         &
                     up_diag,              &     
                     down_diag,            &
                     right_hand_side,      &
                     conc,                 &
                     ncell,                &
                     nvar)
                     
        use matrix_solver
        use gtm_precision
        implicit none                                                       
        ! ----- args

        integer, intent (in) :: ncell                              !< Number of volumes
        integer, intent (in) :: nvar                               !< Number of variables 
        real(gtm_real),intent (in)  :: down_diag(ncell)            !< Values of the coefficients below diagonal in matrix
        real(gtm_real),intent (in)  :: center_diag(ncell)          !< Values of the coefficients at the diagonal in matrix
        real(gtm_real),intent (in)  :: up_diag(ncell)              !< Values of the coefficients above the diagonal in matrix
        real(gtm_real),intent (in)  :: right_hand_side(ncell,nvar) !< Values of the right hand side vector
        real(gtm_real),intent (out) :: conc(ncell,nvar)            !< Values of the computed solution

        ! --- local
        integer :: ivar
        integer :: ap(ncell+1), ai(4+3*(ncell-2))
        real(gtm_real) :: ax(4+3*(ncell-2))

        do ivar = 1 ,nvar
            call tridi_solver(center_diag(:),         &
                              up_diag(:),             &     
                              down_diag(:),           &
                              right_hand_side(:,ivar),&
                              conc(:,ivar),           &
                              ncell)
        end do      
        return
    end subroutine 


    !> Solve the system of linear equations by using KLU Sparse Solver
    subroutine klu_solve_for_tri(center_diag ,         &
                                 up_diag,              &     
                                 down_diag,            &
                                 right_hand_side,      &
                                 conc,                 &
                                 ncell,                &
                                 nvar)
        use klu
        use gtm_precision
        implicit none                                                       
        !----- args

        integer, intent (in) :: ncell                              !< Number of volumes
        integer, intent (in) :: nvar                               !< Number of variables 
        real(gtm_real),intent (in)  :: down_diag(ncell)            !< Values of the coefficients below diagonal in matrix
        real(gtm_real),intent (in)  :: center_diag(ncell)          !< Values of the coefficients at the diagonal in matrix
        real(gtm_real),intent (in)  :: up_diag(ncell)              !< Values of the coefficients above the diagonal in matrix
        real(gtm_real),intent (in)  :: right_hand_side(ncell,nvar) !< Values of the right hand side vector
        real(gtm_real),intent (out) :: conc(ncell,nvar)            !< Values of the computed solution     
        integer :: ap(ncell+1)
        integer :: ai(4+3*(ncell-2))
        real(gtm_real) :: ax(4+3*(ncell-2))
        integer :: i
        
        do i = 1, nvar
           conc(:,nvar) = right_hand_side(:,i)
           call tri2sparse(ap,                &
                           ai,                &
                           ax,                &
                           up_diag(:),        &
                           center_diag(:),    & 
                           down_diag(:),      &
                           ncell)                     
           if ((first_time_klu).or.(ap(ncell+1).ne.previous_non_zero)) then          
               k_symbolic = klu_fortran_analyze(ncell, ap, ai, k_common)
               k_numeric = klu_fortran_factor(ap, ai, ax, k_symbolic, k_common)                        
               first_time_klu = .false.
               previous_non_zero = ap(ncell+1)
           else    
               call klu_fortran_refactor(ap, ai, ax, k_symbolic, k_numeric, k_common)
           end if    
           call klu_fortran_solve(k_symbolic, k_numeric, ncell, 1, conc(:,nvar), k_common)
        end do
        
        return
    end subroutine 

end module 