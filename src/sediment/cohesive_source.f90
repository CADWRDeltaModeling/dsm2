!<license>
!    Copyright (C) 2016 State of California,
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
module cohesive_source

    use gtm_precision

    contains 
    
    subroutine deposition_flux(flow, &
                               area, &
                               ncell)
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: flow(ncell)
        real(gtm_real), intent(in) :: area(ncell)
        
        !bed_shear_velocity = 0.5d0*water_density*friction_factor*velocity
        return
    end subroutine


    subroutine critical_bed_shear_stress()
        use sediment_variables
        implicit none
        
        
        return
    end subroutine    


    !> Shields parameter
    subroutine shields_parameter(shield_param,  &
                                 grain_size)
        implicit none
        real(gtm_real), intent(out) :: shield_param
        real(gtm_real), intent(in) :: grain_size
        
        if (grain_size.gt.1.0d0) then  ! roughly assign a number to avoid getting here
            shield_param = 0.029d0     
        elseif (grain_size.gt.0.5d0 .and. grain_size.le.1.0d0) then      ! coarse sand
            shield_param = 0.029d0 + (0.033d0-0.029d0)*(grain_size-0.5d0)/(1.0d0-0.5d0)
        elseif (grain_size.gt.0.25d0 .and. grain_size.le.0.5d0) then     ! medium sand
            shield_param = 0.033d0 + (0.048d0-0.033d0)*(grain_size-0.25d0)/(0.5d0-0.25d0)    
        elseif (grain_size.gt.0.125d0 .and. grain_size.le.0.25d0) then   ! fine sand
            shield_param = 0.048d0 + (0.072d0-0.048d0)*(grain_size-0.125d0)/(0.25d0-0.125d0)
        elseif (grain_size.gt.0.0625d0 .and. grain_size.le.0.125d0) then ! very fine sand
            shield_param = 0.072d0 + (0.109d0-0.072d0)*(grain_size-0.0625d0)/(0.125d0-0.0625d0)
        elseif (grain_size.gt.0.0310d0 .and. grain_size.le.0.0625d0) then ! coarse silt
            shield_param = 0.109d0 + (0.165d0-0.109d0)*(grain_size-0.0310d0)/(0.0625d0-0.0310d0)                 
        elseif (grain_size.gt.0.0156d0 .and. grain_size.le.0.0310d0) then ! medium silt
            shield_param = 0.165d0 + (0.25d0-0.165d0)*(grain_size-0.0156d0)/(0.0310d0-0.0156d0)    
        elseif (grain_size.gt.0.0078d0 .and. grain_size.le.0.0156d0) then ! fine silt
            shield_param = 0.25d0 + (0.3d0-0.25d0)*(grain_size-0.0078d0)/(0.0156d0-0.0078d0)
        else
            shield_param = 0.3d0
        end if
                                    
        return
    end subroutine   


end module