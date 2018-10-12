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
!> boundary advection interface to be fulfilled by driver or application
!>@ingroup transport
module boundary_advection
  !> Calculate boundary advection
  interface
    !> Generic interface for calculating BC advection routine that should be fulfilled by
    !> client programs
    subroutine boundary_advective_flux_if(flux_lo,    &
                                          flux_hi,    &
                                          conc_lo,    &
                                          conc_hi,    &
                                          flow_lo,    &
                                          flow_hi,    &
                                          ncell,      &
                                          nvar,       &
                                          time,       &
                                          dt,         &
                                          dx,         &
                                          tstp,       &
                                          sed_percent)
        use gtm_precision
        use common_variables, only: n_node, n_qext!, n_sediment_bc
        implicit none
        !--- args          
        integer,intent(in)  :: ncell                            !< Number of cells
        integer,intent(in)  :: nvar                             !< Number of variables
        integer,intent(in)  :: tstp
        real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
        real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
        real(gtm_real),intent(out) :: sed_percent(n_node,n_qext,nvar)!<percentages of compositions at boundaries  & 10 is the maximum number of 
                                                                                 !external flows        !<TODO: make array dimensions effective
        real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
        real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
        real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
        real(gtm_real),intent(in)    :: time                    !< Current time
        real(gtm_real),intent(in)    :: dx(ncell)               !< Spatial step  
        real(gtm_real),intent(in)    :: dt                      !< Time step
     
    end subroutine boundary_advective_flux_if
  end interface

  !> This pointer should be set by the driver or client code to specify the 
  !> treatment at the advection boundary condition 
  procedure(boundary_advective_flux_if),pointer :: advection_boundary_flux  => null()

  contains
 
  !> Example advective flux that imposes Neumann boundaries with zero flux at
  !> both ends of the channel.
  subroutine zero_advective_flux(flux_lo,    &
                                 flux_hi,    &
                                 conc_lo,    &
                                 conc_hi,    &
                                 flow_lo,    &
                                 flow_hi,    &
                                 ncell,      &
                                 nvar,       &
                                 time,       &
                                 dt,         &
                                 dx)
     
       use gtm_precision
       use error_handling
       implicit none
       !--- args          
       integer,intent(in)  :: ncell                            !< Number of cells
       integer,intent(in)  :: nvar                             !< Number of variables
       real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
       real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
       real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
       real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
       real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
       real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
       real(gtm_real),intent(in)    :: time                    !< Current time
       real(gtm_real),intent(in)    :: dx(ncell)               !< Spatial step  
       real(gtm_real),intent(in)    :: dt                      !< Time step    
     
       flux_lo(1,:) = zero
       flux_hi(ncell,:) = zero
       return
  end subroutine
 
  !> Example uninitialize that prints an error and bails
  subroutine uninitialized_advection_bc(flux_lo,     &
                                        flux_hi,     &
                                        conc_lo,     &
                                        conc_hi,     &
                                        flow_lo,     &
                                        flow_hi,     &
                                        ncell,       &
                                        nvar,        &
                                        time,        &
                                        dt,          &
                                        dx)                                         
       use gtm_precision 
       use error_handling
       implicit none
       !--- args          
       integer,intent(in)  :: ncell                            !< Number of cells
       integer,intent(in)  :: nvar                             !< Number of variables
       real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
       real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
       real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
       real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
       real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
       real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
       real(gtm_real),intent(in)    :: time                    !< Current time
       real(gtm_real),intent(in)    :: dx(ncell)               !< Spatial step  
       real(gtm_real),intent(in)    :: dt                      !< Time step        
     
       call gtm_fatal("Boundary not implemented in advection!")
     
       return
   end subroutine 
  
  
  !> Example advective flux that imposes boundary concentration based on the values read from input file
  subroutine bc_advection_flux(flux_lo,    &
                               flux_hi,    &
                               conc_lo,    &
                               conc_hi,    &
                               flow_lo,    &
                               flow_hi,    &
                               ncell,      &
                               nvar,       &
                               time,       &
                               dt,         &
                               dx,         &
                               tstp,        &
                               sed_percent)
     
       use gtm_precision
       use error_handling
       use common_variables, only: n_node, n_qext!, n_sediment_bc   
       implicit none
       !--- args          
       integer,intent(in)  :: ncell                            !< Number of cells
       integer,intent(in)  :: nvar                             !< Number of variables
       integer,intent(in)  :: tstp
       real(gtm_real),intent(inout) :: flux_lo(ncell,nvar)     !< Flux on lo side of cell, time centered
       real(gtm_real),intent(inout) :: flux_hi(ncell,nvar)     !< Flux on hi side of cell, time centered
       real(gtm_real),intent(out) :: sed_percent(n_node,n_qext,nvar)!<percentages of compositions at boundaries  & 10 is the maximum number of 
                                                                                 !external flows        !<TODO: make array dimensions effective
       real(gtm_real),intent(in)    :: flow_lo(ncell)          !< Flow on lo side of cells centered in time
       real(gtm_real),intent(in)    :: flow_hi(ncell)          !< Flow on hi side of cells centered in time
       real(gtm_real),intent(in)    :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
       real(gtm_real),intent(in)    :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
       real(gtm_real),intent(in)    :: time                    !< Current time
       real(gtm_real),intent(in)    :: dx(ncell)               !< Spatial step  
       real(gtm_real),intent(in)    :: dt                      !< Time step    
     
       flux_lo(1,:) = conc_lo(1,:)*flow_lo(1)
       flux_hi(ncell,:) = conc_hi(ncell,:)*flow_hi(ncell)
       return
  end subroutine  
  
  
end module
