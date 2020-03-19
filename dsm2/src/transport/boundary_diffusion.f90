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

!> Boundary diffusive flux interface to be fulfilled by driver or application
!>@ingroup transport
module boundary_diffusion

  !> Calculate boundary diffusion flux
  abstract interface
    !> Generic interface for boundary diffusion that should be fulfilled by client programs
    subroutine boundary_diffusive_flux_if(diffusive_flux_lo, &
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
         implicit none
         !--- args
         integer, intent(in)  :: ncell                                    !< Number of cells
         integer, intent(in)  :: nvar                                     !< Number of variables
         real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)   !< Face flux, lo side
         real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)   !< Face flux, hi side
         real(gtm_real), intent (in)   :: area_lo(ncell)                  !< Low side area centered at time
         real(gtm_real), intent (in)   :: area_hi(ncell)                  !< High side area centered at time
         real(gtm_real), intent (in)   :: time                            !< Time
         real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration
         real(gtm_real), intent (in)   :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef.
         real(gtm_real), intent (in)   :: disp_coef_hi(ncell)             !< High side constituent dispersion coef.
         real(gtm_real), intent (in)   :: dt                              !< dt
         real(gtm_real), intent (in)   :: dx(ncell)                       !< dx

    end subroutine
  end interface

  !> This pointer should be set by the driver or client code to specify the
  !> treatment at the boundaries
  procedure(boundary_diffusive_flux_if),pointer :: boundary_diffusion_flux  => null()

  abstract interface
    !> Generic interface for calculating BC of matrix that should be fulfilled by
    !> the driver or the client programs
    subroutine boundary_diffusive_matrix_if(center_diag ,           &
                                            up_diag,                &
                                            down_diag,              &
                                            right_hand_side,        &
                                            explicit_diffuse_op,    &
                                            conc_prev,              &
                                            mass_prev,              &
                                            area_lo_prev,           &
                                            area_hi_prev,           &
                                            disp_coef_lo_prev,      &
                                            disp_coef_hi_prev,      &
                                            conc,                   &
                                            flow_lo,                &
                                            flow_hi,                &
                                            area,                   &
                                            area_lo,                &
                                            area_hi,                &
                                            disp_coef_lo,           &
                                            disp_coef_hi,           &
                                            theta_gtm,              &
                                            ncell,                  &
                                            time,                   &
                                            nvar,                   &
                                            dx,                     &
                                            dt)
        use gtm_precision
        implicit none
        !--- args
        integer, intent(in) :: ncell                                  !< Number of cells
        integer, intent(in) :: nvar                                   !< Number of variables
        real(gtm_real), intent(inout) :: down_diag(ncell)             !< Values of the coefficients below diagonal in matrix
        real(gtm_real), intent(inout) :: center_diag(ncell)           !< Values of the coefficients at the diagonal in matrix
        real(gtm_real), intent(inout) :: up_diag(ncell)               !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent(inout) :: right_hand_side(ncell,nvar)  !< Values of the coefficients of right  hand side vector
        real(gtm_real), intent(in) :: explicit_diffuse_op(ncell,nvar) !< Explicit diffuive operator
        real(gtm_real), intent(in) :: conc_prev(ncell,nvar)           !< Cell centered concentration at old time
        real(gtm_real), intent(in) :: mass_prev(ncell,nvar)           !< Mass from old time and previous two steps
        real(gtm_real), intent(in) :: area_lo_prev(ncell)             !< Low side area at old time
        real(gtm_real), intent(in) :: area_hi_prev(ncell)             !< High side area at old time
        real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)        !< Low side constituent dispersion coef. at old time
        real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)        !< High side constituent dispersion coef. at old time
        real(gtm_real), intent(in) :: conc(ncell,nvar)                !< Concentration
        real(gtm_real), intent(in) :: flow_lo(ncell)                  !< Low side flow at new time
        real(gtm_real), intent(in) :: flow_hi(ncell)                  !< High side flow at new time
        real(gtm_real), intent(in) :: area (ncell)                    !< Cell centered area at new time
        real(gtm_real), intent(in) :: area_lo(ncell)                  !< Low side area at new time
        real(gtm_real), intent(in) :: area_hi(ncell)                  !< High side area at new time
        real(gtm_real), intent(in) :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: disp_coef_hi(ncell)             !< High side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: time                            !< Current time
        real(gtm_real), intent(in) :: theta_gtm                       !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit
        real(gtm_real), intent(in) :: dx(ncell)                       !< Spatial step
        real(gtm_real), intent(in) :: dt                              !< Time step

    end subroutine
  end interface

  !> This pointer should be set by the driver or client code to specify the
  !> treatment at the first and last row of coefficient matrix
  procedure(boundary_diffusive_matrix_if), pointer :: boundary_diffusion_matrix  => null()


  contains


  !> Set diffusion flux and diffusion matrix pointers to diffusion boundary
  subroutine set_diffusion_boundary(bc_flux,bc_matrix)
      use error_handling
      implicit none
      procedure(boundary_diffusive_flux_if),pointer :: bc_flux      !< Diffusion flux routine
      procedure(boundary_diffusive_matrix_if),pointer :: bc_matrix  !< Diffusion matrix routine
      if ((associated(bc_flux) .and. associated(bc_matrix)) .ne. &
          (associated(bc_flux) .or.  associated(bc_matrix)) )then
          call gtm_fatal("Boundary diffusive flux and boundary diffusive matrix not consistently assigned (one null, other not)")
      end if

      boundary_diffusion_flux   => bc_flux
      boundary_diffusion_matrix => bc_matrix

      return
  end subroutine


  !> Example diffusive flux that prints an error and bails
  subroutine no_diffusion_flux(diffusive_flux_lo, &
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
      implicit none
      !--- args
      integer, intent(in)  :: ncell                                    !< Number of cells
      integer, intent(in)  :: nvar                                     !< Number of variables
      real(gtm_real), intent (inout):: diffusive_flux_lo(ncell,nvar)   !< Face flux, lo side
      real(gtm_real), intent (inout):: diffusive_flux_hi(ncell,nvar)   !< Face flux, hi side
      real(gtm_real), intent (in)   :: area_lo(ncell)                  !< Low side area centered at time
      real(gtm_real), intent (in)   :: area_hi(ncell)                  !< High side area centered at time
      real(gtm_real), intent (in)   :: time                            !< Time
      real(gtm_real), intent (in)   :: conc(ncell,nvar)                !< Concentration
      real(gtm_real), intent (in)   :: disp_coef_lo (ncell)            !< Low side constituent dispersion coef.
      real(gtm_real), intent (in)   :: disp_coef_hi (ncell)            !< High side constituent dispersion coef.
      real(gtm_real), intent (in)   :: dt                              !< Spatial step
      real(gtm_real), intent (in)   :: dx(ncell)                       !< Time step

      call gtm_fatal("boundary not implemented")

      return
  end subroutine


 !> Diffusion boundary condition that enforces zero constituent flux
  subroutine neumann_zero_diffusive_flux(diffusive_flux_lo, &
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

      diffusive_flux_lo(1,:) = zero
      diffusive_flux_hi(ncell,:) = zero

      return
  end subroutine


  !> Example diffusive flux that imposes sinusoidal time dependent Neumann boundary flux at
  !> both ends of the channel.
  subroutine neumann_sin_diffusive_flux(diffusive_flux_lo, &
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

      diffusive_flux_lo(1,:) = two*dcos(pi*time/three)               !Just for test
      diffusive_flux_hi(ncell,:) = five*dsin (pi*time/seven)

      return
  end subroutine


  !> Example diffusive flux that imposes Neumann boundaries with zero flux at
  !> both ends of the channel.
  subroutine neumann_zero_diffusion_matrix(center_diag ,       &
                                           up_diag,            &
                                           down_diag,          &
                                           right_hand_side,    &
                                           explicit_diffuse_op,&
                                           conc_prev,          &
                                           mass_prev,          &
                                           area_lo_prev,       &
                                           area_hi_prev,       &
                                           disp_coef_lo_prev,  &
                                           disp_coef_hi_prev,  &
                                           conc,               &
                                           flow_lo,            &
                                           flow_hi,            &
                                           area,               &
                                           area_lo,            &
                                           area_hi,            &
                                           disp_coef_lo,       &
                                           disp_coef_hi,       &
                                           theta_gtm,          &
                                           ncell,              &
                                           time,               &
                                           nvar,               &
                                           dx,                 &
                                           dt)
        use gtm_precision
        implicit none
        !--- args
        integer, intent(in) :: ncell                                  !< Number of cells
        integer, intent(in) :: nvar                                   !< Number of variables
        real(gtm_real), intent(inout) :: down_diag(ncell)             !< Values of the coefficients below diagonal in matrix
        real(gtm_real), intent(inout) :: center_diag(ncell)           !< Values of the coefficients at the diagonal in matrix
        real(gtm_real), intent(inout) :: up_diag(ncell)               !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent(inout) :: right_hand_side(ncell,nvar)  !< Values of the coefficients of right  hand side vector
        real(gtm_real), intent(in) :: explicit_diffuse_op(ncell,nvar) !< Explicit diffuive operator
        real(gtm_real), intent(in) :: conc_prev(ncell,nvar)           !< Cell centered concentration at old time
        real(gtm_real), intent(in) :: mass_prev(ncell,nvar)           !< Mass from old time and previous two steps
        real(gtm_real), intent(in) :: area_lo_prev(ncell)             !< Low side area at old time
        real(gtm_real), intent(in) :: area_hi_prev(ncell)             !< High side area at old time
        real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)        !< Low side constituent dispersion coef. at old time
        real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)        !< High side constituent dispersion coef. at old time
        real(gtm_real), intent(in) :: conc(ncell,nvar)                !< Concentration
        real(gtm_real), intent(in) :: flow_lo(ncell)                  !< Low side flow at new time
        real(gtm_real), intent(in) :: flow_hi(ncell)                  !< High side flow at new time
        real(gtm_real), intent(in) :: area (ncell)                    !< Cell centered area at new time
        real(gtm_real), intent(in) :: area_lo(ncell)                  !< Low side area at new time
        real(gtm_real), intent(in) :: area_hi(ncell)                  !< High side area at new time
        real(gtm_real), intent(in) :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: disp_coef_hi(ncell)             !< High side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: time                            !< Current time
        real(gtm_real), intent(in) :: theta_gtm                       !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit
        real(gtm_real), intent(in) :: dx(ncell)                       !< Spatial step
        real(gtm_real), intent(in) :: dt                              !< Time step
       !---local
       real(gtm_real) :: dt_by_dxsq(ncell)
       real(gtm_real) :: flux_start(nvar)
       real(gtm_real) :: flux_end(nvar)

       dt_by_dxsq = dt/(dx*dx)

       flux_start(:) = zero
       flux_end(:) = zero

       center_diag(1)= area(1)+ theta_gtm*dt_by_dxsq(1)* area_hi(1)*disp_coef_hi(1)
       right_hand_side(1,:) = right_hand_side(1,:) &
                             + theta_gtm*(dt/dx(1))*flux_start(:)

       center_diag(ncell)= area(ncell)+ theta_gtm*dt_by_dxsq(ncell)* area_lo(ncell)*disp_coef_lo(ncell)
       right_hand_side(ncell,:)= right_hand_side(ncell,:) &
                              - theta_gtm*(dt/dx(ncell))*flux_end(:)
       return
   end subroutine


    !> No-diffusion implementation for use as a pointer when diffusion is off.
    subroutine no_diffusion_matrix(center_diag ,        &
                                   up_diag,             &
                                   down_diag,           &
                                   right_hand_side,     &
                                   explicit_diffuse_op, &
                                   conc_prev,           &
                                   mass_prev,           &
                                   area_lo_prev,        &
                                   area_hi_prev,        &
                                   disp_coef_lo_prev,   &
                                   disp_coef_hi_prev,   &
                                   conc,                &
                                   flow_lo,             &
                                   flow_hi,             &
                                   area,                &
                                   area_lo,             &
                                   area_hi,             &
                                   disp_coef_lo,        &
                                   disp_coef_hi,        &
                                   theta_gtm,           &
                                   ncell,               &
                                   time,                &
                                   nvar,                &
                                   dx,                  &
                                   dt)
        use gtm_precision
        use error_handling
        implicit none
        !--- args
        integer, intent(in) :: ncell                                  !< Number of cells
        integer, intent(in) :: nvar                                   !< Number of variables
        real(gtm_real), intent(inout) :: down_diag(ncell)             !< Values of the coefficients below diagonal in matrix
        real(gtm_real), intent(inout) :: center_diag(ncell)           !< Values of the coefficients at the diagonal in matrix
        real(gtm_real), intent(inout) :: up_diag(ncell)               !< Values of the coefficients above the diagonal in matrix
        real(gtm_real), intent(inout) :: right_hand_side(ncell,nvar)  !< Values of the coefficients of right  hand side vector
        real(gtm_real), intent(in) :: explicit_diffuse_op(ncell,nvar) !< Explicit diffuive operator
        real(gtm_real), intent(in) :: conc_prev(ncell,nvar)           !< Cell centered concentration at old time
        real(gtm_real), intent(in) :: mass_prev(ncell,nvar)           !< Mass from old time and previous two steps
        real(gtm_real), intent(in) :: area_lo_prev(ncell)             !< Low side area at old time
        real(gtm_real), intent(in) :: area_hi_prev(ncell)             !< High side area at old time
        real(gtm_real), intent(in) :: disp_coef_lo_prev(ncell)        !< Low side constituent dispersion coef. at old time
        real(gtm_real), intent(in) :: disp_coef_hi_prev(ncell)        !< High side constituent dispersion coef. at old time
        real(gtm_real), intent(in) :: conc(ncell,nvar)                !< Concentration
        real(gtm_real), intent(in) :: flow_lo(ncell)                  !< Low side flow at new time
        real(gtm_real), intent(in) :: flow_hi(ncell)                  !< High side flow at new time
        real(gtm_real), intent(in) :: area (ncell)                    !< Cell centered area at new time
        real(gtm_real), intent(in) :: area_lo(ncell)                  !< Low side area at new time
        real(gtm_real), intent(in) :: area_hi(ncell)                  !< High side area at new time
        real(gtm_real), intent(in) :: disp_coef_lo(ncell)             !< Low side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: disp_coef_hi(ncell)             !< High side constituent dispersion coef. at new time
        real(gtm_real), intent(in) :: time                            !< Current time
        real(gtm_real), intent(in) :: theta_gtm                       !< Explicitness coefficient; 0 is explicit, 0.5 Crank-Nicolson, 1 full implicit
        real(gtm_real), intent(in) :: dx(ncell)                       !< Spatial step
        real(gtm_real), intent(in) :: dt                              !< Time step

        call gtm_fatal("boundary not implemented!")
        return
    end subroutine

end module
