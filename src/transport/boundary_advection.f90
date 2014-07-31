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
                                          dx)
        use gtm_precision
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
     
    end subroutine boundary_advective_flux_if
  end interface

  !> This pointer should be set by the driver or client code to specify the 
  !> treatment at the advection boundary condition 
  ! todo: check here
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
    subroutine bc_fixup_advection_flux(flux_lo,    &
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
        use common_variables, only: n_node, dsm2_node
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
        real(gtm_real) :: flow_tmp
        real(gtm_real) :: mass_tmp(nvar)
        real(gtm_real) :: conc_tmp(nvar)
        integer :: i, j, icell   
   
        do i = 1, n_node
            if (dsm2_node(i)%boundary_no > 0) then                    ! is a boundary
                icell = dsm2_node(i)%cell_no(1)
                if ( dsm2_node(i)%up_down(1) .eq. 1) then             ! upstream boundary
                    flux_lo(icell,:) = conc_lo(icell,:)*flow_lo(icell)
                else
                    flux_hi(icell,:) = conc_hi(icell,:)*flow_hi(icell)
                end if
            end if
         
            if (dsm2_node(i)%junction_no > 0) then
                flow_tmp = zero 
                mass_tmp(:) = zero
                conc_tmp(:) = zero
                do j = 1, dsm2_node(i)%n_conn_cell    ! counting flow into the junctions
                    icell = dsm2_node(i)%cell_no(j)
                    if (dsm2_node(i)%up_down(j)==0 .and. flow_hi(icell)>zero) then        !cell at updstream of junction
                        mass_tmp(:) = mass_tmp(:) + conc_hi(icell,:)*flow_hi(icell)
                        flow_tmp = flow_tmp + flow_hi(icell)
                    elseif (dsm2_node(i)%up_down(j)==1 .and. flow_lo(icell)<zero) then    !cell at downdstream of junction
                        mass_tmp(:) = mass_tmp(:) + conc_lo(icell,:)*abs(flow_lo(icell))
                        flow_tmp = flow_tmp + abs(flow_lo(icell))
                    endif                   
                end do
                if (flow_tmp==zero) then
                    write(*,*) "WARNING: No flow flows into junction!!",icell               
                    conc_tmp(:) = zero
                else     
                    conc_tmp(:) = mass_tmp(:)/flow_tmp
                end if
                ! assign average concentration to downstream cell faces
                do j = 1, dsm2_node(i)%n_conn_cell
                    icell = dsm2_node(i)%cell_no(j)
                    if (dsm2_node(i)%up_down(j)==0 .and. flow_hi(icell)<zero) then     !cell at updstream of junction
                        flux_hi(icell,:) = conc_tmp(:)*flow_hi(icell)                                         
                    elseif (dsm2_node(i)%up_down(j)==1 .and. flow_lo(icell)>zero) then !cell at downdstream of junction
                        flux_lo(icell,:) = conc_tmp(:)*flow_lo(icell)
                    elseif (dsm2_node(i)%up_down(j)==1 .and. flow_lo(icell)<zero) then
                        flux_lo(icell,:) = conc_hi(icell,:)*flow_hi(icell)                        
                    endif                
                end do              
            end if
       
            if (dsm2_node(i)%nonsequential .eq. 1) then  ! without this fixup, the error can be seen at DSM2 node 239
                do j = 1, 2                              ! assign lo/hi face of the same cell to avoid discontinuity
                    icell = dsm2_node(i)%cell_no(j)
                    if (dsm2_node(i)%up_down(j)==0 .and. flow_hi(icell)<zero) then     !cell at updstream of link
                        flux_hi(icell,:) = conc_lo(icell,:)*flow_hi(icell)
                    elseif (dsm2_node(i)%up_down(j)==1 .and. flow_lo(icell)>zero) then !cell at downdstream of link
                        flux_lo(icell,:) = conc_hi(icell,:)*flow_lo(icell)
                    endif                
                end do
            end if   
            
            !flow_chk = flow_chk + resv_flow(dsm2_node(i)%resv_conn_no(j))
             
            !do j = 1, dsm2_node(i)%n_qext
            !    flow_chk = flow_chk + qext_flow(dsm2_node(i)%qext_no(j))
            !end do
            
            !do j = 1, dsm2_node(i)%n_tran 
            !    flow_chk = flow_chk + tran_flow(dsm2_node(i)%tran_no(j))
            !end do 
            !write(11,*) dsm2_node(i)%dsm2_node_no, flow_chk
        end do        
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
     
       flux_lo(1,:) = conc_lo(1,:)*flow_lo(1)
       flux_hi(ncell,:) = conc_hi(ncell,:)*flow_hi(ncell)
       return
  end subroutine  
  
  
end module
