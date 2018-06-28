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

!>Dispersion coefficient interface to be fulfilled by driver or application
!>@ingroup transport
! todo: here we already hardwired the values of dispersion coefficient, BUT we should 
! get U and depth from HYDRO and based on the well known formula by Fischer et al. (1979); it must be computed and fed here

module dispersion_coefficient
  use gtm_precision, only: gtm_real

  interface
    !> Dispersion coefficient interface to be fulfilled by driver or application
    subroutine diffusion_coef_if(disp_coef_lo,         &
                                 disp_coef_hi,         &
                                 flow,                 &
                                 flow_lo,              &
                                 flow_hi,              &
                                 area,                 &
                                 area_lo,              &
                                 area_hi,              &
                                 time,                 &
                                 dx,                   &
                                 dt,                   &
                                 ncell,                &
                                 nvar)      
                                  
        use gtm_precision    
        implicit none
    
        real(gtm_real),intent(out):: disp_coef_lo(ncell)     !< Low side constituent dispersion coef
        real(gtm_real),intent(out):: disp_coef_hi(ncell)     !< High side constituent dispersion coef
        integer,intent(in)  :: ncell                         !< Number of cells
        integer,intent(in)  :: nvar                          !< Number of variables   
        real(gtm_real),intent(in) :: time                    !< Current time
        real(gtm_real),intent(in) :: dx(ncell)               !< Spatial step  
        real(gtm_real),intent(in) :: dt                      !< Time step 
        real(gtm_real),intent(in) :: flow_lo(ncell)          !< flow on lo side of cells centered in time
        real(gtm_real),intent(in) :: flow_hi(ncell)          !< flow on hi side of cells centered in time       
        real(gtm_real),intent(in) :: flow(ncell)             !< flow on center of cells          
        real(gtm_real),intent(in) :: area_lo(ncell)          !< area on lo side of cells centered in time
        real(gtm_real),intent(in) :: area_hi(ncell)          !< area on hi side of cells centered in time       
        real(gtm_real),intent(in) :: area(ncell)             !< area on center of cells            
    end subroutine 
  end interface

  !> This pointer should be set by the driver or client code to specify the 
  !> treatment at the dispersion coefficients
  procedure(diffusion_coef_if), pointer :: dispersion_coef  => null()

  real(gtm_real),save :: const_dispersion

  contains

  !> Set dispersion coefficient interface to an implementation that is constant in space and time
  !> This routine sets the value of the constant dispersion coefficient as well.
  subroutine set_constant_dispersion(coefficient)
      use gtm_precision
      use error_handling
      implicit none
      real(gtm_real),intent(in) :: coefficient      !< Constant value of dispersion coef. 
      const_dispersion = coefficient
      dispersion_coef => constant_dispersion_coef
      return
  end subroutine

  !> Implementation of diffusion_coef_if that sets dipsersion to a constant over space and time
  subroutine constant_dispersion_coef(disp_coef_lo,         &
                                      disp_coef_hi,         &
                                      flow,                 &
                                      flow_lo,              &
                                      flow_hi,              &
                                      area,                 &
                                      area_lo,              &
                                      area_hi,              &
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
      real(gtm_real),intent(in) :: area_lo(ncell)          !< area on lo side of cells centered in time
      real(gtm_real),intent(in) :: area_hi(ncell)          !< area on hi side of cells centered in time       
      real(gtm_real),intent(in) :: area(ncell)             !< area on center of cells       
      real(gtm_real),intent(out):: disp_coef_lo(ncell)     !< Low side constituent dispersion coef.
      real(gtm_real),intent(out):: disp_coef_hi(ncell)     !< High side constituent dispersion coef. 
      !-- local
      integer :: ivar
   
      disp_coef_hi = const_dispersion
      disp_coef_lo = const_dispersion
        
      return
 end subroutine

end module