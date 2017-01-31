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
!> DO source terms to fulfill source interface
!>@ingroup do_module
module do_basic
    !> DO module sources.
    !> This source term multiplies each constituent by a decay rate
    subroutine do_basic_source(source, & 
                               conc,   &
                               area,   &
                               flow,   &
                               ncell,  &
                               nvar,   &
                               time)
        use gtm_precision
        implicit none 
        integer, intent(in)  :: ncell                      !< Number of cells
        integer, intent(in)  :: nvar                       !< Number of variables
        real(gtm_real), intent(inout) :: source(ncell,nvar)!< cell centered source 
        real(gtm_real), intent(in)  :: conc(ncell,nvar)    !< Concentration
        real(gtm_real), intent(in)  :: area(ncell)         !< area at source     
        real(gtm_real), intent(in)  :: flow(ncell)         !< flow at source location
        real(gtm_real), intent(in)  :: time                !< time
        real(gtm_real) :: decay                            ! temporary variables
        real(gtm_real) :: settle                           ! temporary variables
 
        source = zero   
           
        !do i = 1, ncell
        !    decay = bod_decay(i) * conc(i,i_bod)          ! contribution due to reactions
        !    settle = bod_settle(i) * conc(i,i_bod)        ! loss of mass due to settling
        !    source(i,i_bod) = - decay - settle
        !enddo
    end subroutine
    
end module               