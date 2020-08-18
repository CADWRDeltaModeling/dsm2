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

!> Source interface to be fulfilled by driver or application
!> Specifically, the user must set the "source" pointer (a variable in this module)
!> to a subroutine that meets the compute_source interface.
!>@ingroup transport
module source_sink
 
    use gtm_precision, only: gtm_real
    real(gtm_real), allocatable :: linear_decay(:) !< linear decay rates for when linear source is used, should be nonpositive 
    real(gtm_real), allocatable :: linear_decay_by_cell(:,:) !< linear decay rates per cell
    real(gtm_real), allocatable :: source_term_by_cell(:,:) !< source term per cell 
 
!> Calculate source
interface
    !> Generic interface for calculating source that should be fulfilled by
    !> client programs. The source is calculated using concentration,
    !> and produces a source increment appropriate for primitives.
    !> If you want to increment a conservative variable with this source,
    !> call prim_increment_to_cons()
    subroutine source_if(source,       & 
                         conc,         &
                         flow,         &
                         area,         &
                         width,        &
                         depth,        &
                         wet_p,        &
                         dx,           &
                         dt,           &
                         time,         &
                         ncell,        &
                         nvar,         &
                         constraint,   &
                         name,         &
                         rkstep)
        use gtm_precision
        implicit none
        !--- args  
        integer, intent(in) :: ncell                           !< Number of cells
        integer, intent(in) :: nvar                            !< Number of variables   
        integer, intent(in) :: rkstep                          !< Reaction step in Huen's method
        real(gtm_real), intent(inout) :: source(ncell,nvar)    !< cell centered source 
        real(gtm_real), intent(in)  :: conc(ncell,nvar)        !< Concentration 
        real(gtm_real), intent(in)  :: flow(ncell)             !< flow at source location
        real(gtm_real), intent(in)  :: area(ncell)             !< Cell centered area at source     
        real(gtm_real), intent(in)  :: width(ncell)            !< Cell centered width at source 
        real(gtm_real), intent(in)  :: depth(ncell)            !< depth at source location
        real(gtm_real), intent(in)  :: wet_p(ncell)            !< wetted perimeter at source location  
        real(gtm_real), intent(in)  :: dx(ncell)               !< dx
        real(gtm_real), intent(in)  :: dt                      !< dt
        real(gtm_real), intent(in)  :: time                    !< time
        real(gtm_real), intent(in)  :: constraint(ncell,nvar)  !< Constraint 
        character(len=32), intent(in) :: name(nvar)            !< Constituent name
    end subroutine 

end interface
 
    !>\file
    !>\var source_sink::source 
    !>\brief Pointer to source term
    !>This pointer should be set by the driver or client code
    procedure(source_if), pointer :: compute_source => null() 

    contains
 
    subroutine set_source(source_term)
        implicit none
        procedure(source_if), pointer :: source_term !  => null()  
        compute_source => source_term
        return
    end subroutine
 
    !> Zero source implementation.
    !> This source term adds nothing (e.g., for conservative constituents)
    subroutine no_source(source,       & 
                         conc,         &
                         flow,         &
                         area,         &
                         width,        &
                         depth,        &
                         wet_p,        &
                         dx,           &
                         dt,           &
                         time,         &
                         ncell,        &
                         nvar,         &
                         constraint,   &
                         name,         &
                         rkstep)                                         
        use gtm_precision 
        use error_handling         
        implicit none
        !--- args
        integer, intent(in) :: ncell                           !< Number of cells
        integer, intent(in) :: nvar                            !< Number of variables   
        integer, intent(in) :: rkstep                          !< Reaction step in Huen's method
        real(gtm_real), intent(inout) :: source(ncell,nvar)    !< cell centered source 
        real(gtm_real), intent(in)  :: conc(ncell,nvar)        !< Concentration 
        real(gtm_real), intent(in)  :: flow(ncell)             !< flow at source location
        real(gtm_real), intent(in)  :: area(ncell)             !< Cell centered area at source     
        real(gtm_real), intent(in)  :: width(ncell)            !< Cell centered width at source 
        real(gtm_real), intent(in)  :: depth(ncell)            !< depth at source location
        real(gtm_real), intent(in)  :: wet_p(ncell)            !< wetted perimeter at source location         
        real(gtm_real), intent(in)  :: dx(ncell)               !< dx
        real(gtm_real), intent(in)  :: dt                      !< dt
        real(gtm_real), intent(in)  :: time                    !< time
        real(gtm_real), intent(in)  :: constraint(ncell,nvar)  !< Constraint 
        character(len=32), intent(in) :: name(nvar)            !< Constituent name
     
        source = zero
     
        return
    end subroutine 

    !> Sets the decay rate and sets the source term to linear decay
    subroutine set_linear_decay(rates,nvar)

        use gtm_precision  
        use error_handling
        implicit none
        integer:: nvar 
        real(gtm_real), dimension(nvar) :: rates
        if (allocated(linear_decay)) then
            deallocate(linear_decay)
        end if
        allocate(linear_decay(nvar))
        if (minval(rates) .lt. zero) then
            call gtm_fatal("Decay rates for linear decay should be nonnegative")
        end if
        linear_decay = rates
        compute_source => linear_decay_source
        return
    end subroutine

    !> Linear decay source.
    !> This source term multiplies each constituent by a decay rate
    subroutine linear_decay_source(source,       & 
                                   conc,         &
                                   flow,         &
                                   area,         &
                                   width,        &
                                   depth,        &
                                   wet_p,        &
                                   dx,           &
                                   dt,           &
                                   time,         &
                                   ncell,        &
                                   nvar,         &
                                   constraint,   &
                                   name,         &
                                   rkstep)
        use gtm_precision
        implicit none 
        !--- args
        integer, intent(in) :: ncell                           !< Number of cells
        integer, intent(in) :: nvar                            !< Number of variables   
        integer, intent(in) :: rkstep                          !< Reaction step in Huen's method
        real(gtm_real), intent(inout) :: source(ncell,nvar)    !< cell centered source 
        real(gtm_real), intent(in)  :: conc(ncell,nvar)        !< Concentration 
        real(gtm_real), intent(in)  :: flow(ncell)             !< flow at source location
        real(gtm_real), intent(in)  :: area(ncell)             !< Cell centered area at source     
        real(gtm_real), intent(in)  :: width(ncell)            !< Cell centered width at source 
        real(gtm_real), intent(in)  :: depth(ncell)            !< depth at source location
        real(gtm_real), intent(in)  :: wet_p(ncell)            !< wetted perimeter at source location       
        real(gtm_real), intent(in)  :: dx(ncell)               !< dx
        real(gtm_real), intent(in)  :: dt                      !< dt
        real(gtm_real), intent(in)  :: time                    !< time
        real(gtm_real), intent(in)  :: constraint(ncell,nvar)  !< Constraint 
        character(len=32), intent(in) :: name(nvar)            !< Constituent name
        !--- local
        integer :: ivar
        
        ! source must be in primitive variable 
        do ivar = 1,nvar
            source(:,ivar) = -linear_decay(ivar)*conc(:,ivar)
        end do
        return
    end subroutine 

    !> Linear decay source
    !> This source term multiplies each constituent by a decay rate for each cell
    subroutine linear_decay_source_by_cell(source,       & 
                                           conc,         &
                                           flow,         &
                                           area,         &
                                           width,        &
                                           depth,        &
                                           wet_p,        &
                                           dx,           &
                                           dt,           &
                                           time,         &
                                           ncell,        &
                                           nvar,         &
                                           constraint,   &
                                           name,         &
                                           rkstep)
        use gtm_precision
        implicit none 
        !--- args
        integer, intent(in) :: ncell                           !< Number of cells
        integer, intent(in) :: nvar                            !< Number of variables   
        integer, intent(in) :: rkstep                          !< Reaction step in Huen's method
        real(gtm_real), intent(inout) :: source(ncell,nvar)    !< cell centered source 
        real(gtm_real), intent(in)  :: conc(ncell,nvar)        !< Concentration 
        real(gtm_real), intent(in)  :: flow(ncell)             !< flow at source location
        real(gtm_real), intent(in)  :: area(ncell)             !< Cell centered area at source     
        real(gtm_real), intent(in)  :: width(ncell)            !< Cell centered width at source 
        real(gtm_real), intent(in)  :: depth(ncell)            !< depth at source location
        real(gtm_real), intent(in)  :: wet_p(ncell)            !< wetted perimeter at source location       
        real(gtm_real), intent(in)  :: dx(ncell)               !< dx
        real(gtm_real), intent(in)  :: dt                      !< dt
        real(gtm_real), intent(in)  :: time                    !< time
        real(gtm_real), intent(in)  :: constraint(ncell,nvar)  !< Constraint 
        character(len=32), intent(in) :: name(nvar)            !< Constituent name  
        !---local
        integer :: ivar                                    !< Counter on constituents
 
        ! source must be in primitive variable 
        do ivar = 1,nvar
            source(:,ivar) = -linear_decay_by_cell(:,ivar)*conc(:,ivar)
        end do
        return
    end subroutine 

    !> Given source term by cell
    subroutine set_source_term_by_cell(source,       & 
                                       conc,         &
                                       flow,         &
                                       area,         &
                                       width,        &
                                       depth,        &
                                       wet_p,        &
                                       dx,           &
                                       dt,           &
                                       time,         &
                                       ncell,        &
                                       nvar,         &
                                       constraint,   &
                                       name,         &
                                       rkstep)
        use gtm_precision
        implicit none 
        !--- args
        integer, intent(in) :: ncell                           !< Number of cells
        integer, intent(in) :: nvar                            !< Number of variables   
        integer, intent(in) :: rkstep                          !< Reaction step in Huen's method
        real(gtm_real), intent(inout) :: source(ncell,nvar)    !< cell centered source 
        real(gtm_real), intent(in)  :: conc(ncell,nvar)        !< Concentration 
        real(gtm_real), intent(in)  :: flow(ncell)             !< flow at source location
        real(gtm_real), intent(in)  :: area(ncell)             !< Cell centered area at source     
        real(gtm_real), intent(in)  :: width(ncell)            !< Cell centered width at source 
        real(gtm_real), intent(in)  :: depth(ncell)            !< depth at source location
        real(gtm_real), intent(in)  :: wet_p(ncell)            !< wetted perimeter at source location       
        real(gtm_real), intent(in)  :: dx(ncell)               !< dx
        real(gtm_real), intent(in)  :: dt                      !< dt
        real(gtm_real), intent(in)  :: time                    !< time
        real(gtm_real), intent(in)  :: constraint(ncell,nvar)  !< Constraint 
        character(len=32), intent(in) :: name(nvar)            !< Constituent name    
        !---local
        integer :: ivar                                    !< Counter on constituents

        ! source must be in primitive variable 
        do ivar = 1,nvar
            source(:,ivar) = source_term_by_cell(:,ivar)
        end do
        return
    end subroutine 
 
end module
