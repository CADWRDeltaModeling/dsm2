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

!> Module orchestrating the reaction scheme. The main
!> routine in the module is react().
!>@ingroup transport
module reaction

  contains

  !> subroutine for integerating the ODE of reaction term 
  !> the method is 3rd order (Runge-Kutta) 
  subroutine react(conc,       &
                   conc_prev,  &
                   area,       &
                   area_prev,  &
                   flow,       &
                   ncell,      &
                   nvar,       &
                   time,       &
                   dt)                 
      use gtm_precision
      use source_sink
      use primitive_variable_conversion ! todo: do we need it?
      !---arg
      implicit none
                 
      integer, intent (in) :: ncell                                !< Number of cells
      integer, intent (in) :: nvar                                 !< Number of variables

      real(gtm_real), intent (out):: conc(ncell,nvar)              !< Concentration at new time
      real(gtm_real), intent (in) :: conc_prev(ncell,nvar)         !< Concentration at old time
      real(gtm_real), intent (in) :: area (ncell)                  !< Cell-centered area at new time
      real(gtm_real), intent (in) :: area_prev (ncell)             !< Cell-centered area at old time
      real(gtm_real), intent (in) :: flow(ncell)                   !< cell-centered flow 
      real(gtm_real), intent (in) :: time                          !< current time
      real(gtm_real), intent (in) :: dt                            !< Time step   

      !---local
      integer :: ivar
      integer :: icell
      real(gtm_real) :: source(ncell,nvar)
      real(gtm_real) :: k_1(ncell,nvar)
      real(gtm_real) :: k_2(ncell,nvar)
      real(gtm_real) :: k_3(ncell,nvar)
      real(gtm_real) :: time_star

      conc = conc_prev
      time_star = time

      call compute_source(source,   & 
                          conc,     &
                          area,     &
                          flow,     &
                          ncell,    &
                          nvar,     &
                          time_star)
                        
      k_1 = dt * source 
      time_star = time + dt*half
      conc = conc_prev + k_1*half

      !todo: do we need to update are here to feed "compute_source"?
      call compute_source(source,   & 
                          conc,     &
                          area,     &
                          flow,     &
                          ncell,    &
                          nvar,     &
                          time_star)
                    
      k_2 = dt * source 
      time_star = time + dt
      conc = conc_prev + (two* k_2)- k_1

      call compute_source(source,   & 
                          conc,     &
                          area,     &
                          flow,     &
                          ncell,    &
                          nvar,     &
                          time_star)
                  
      k_3 = dt * source
      conc = conc_prev + (k_1 + four*k_2 + k_3)/six 

  end subroutine 

end module