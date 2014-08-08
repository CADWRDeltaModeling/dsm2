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
 
!> boundary concentration interface to be fulfilled by driver or application
!>@ingroup transport
module boundary_concentration
    !> Assign boundary concentration
    interface
        !> Generic interface for assigning boundary concentration routine that should be fulfilled by
        !> client programs
        subroutine boundary_concentration_if(conc_lo,    &
                                             conc_hi,    &
                                             ncell,      &
                                             nvar)
            use gtm_precision
            implicit none
            !--- args          
            integer, intent(in)  :: ncell                            !< Number of cells
            integer, intent(in)  :: nvar                             !< Number of variables
            real(gtm_real), intent(inout) :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
            real(gtm_real), intent(inout) :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face
               
        end subroutine boundary_concentration_if
    end interface

    !> This pointer should be set by the driver or client code to specify the 
    !> treatment for given boundary concentration
    procedure(boundary_concentration_if), pointer :: boundary_conc  => null()

    contains
 
    !> No assignment for boundary flow and leave it as it is
    subroutine unassigned_boundary_concentration(conc_lo,  &
                                                 conc_hi,  &
                                                 ncell,    &
                                                 nvar)
        use gtm_precision
        use error_handling
        implicit none
        integer, intent(in)  :: ncell                            !< Number of cells
        integer, intent(in)  :: nvar                             !< Number of variables
        real(gtm_real), intent(inout) :: conc_lo(ncell,nvar)     !< Concentration extrapolated to lo face
        real(gtm_real), intent(inout) :: conc_hi(ncell,nvar)     !< Concentration extrapolated to hi face        
        return
    end subroutine
    
end module            