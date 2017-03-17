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
!> ================================================
!> The Sediment Bed Module is developed by:
!> Dave Hutchinson and Reed Harris
!> Reed Harris Environmental Ltd.
!> ================================================
!> 
!>@ingroup sediment_bed
module sediment_bed

    use gtm_precision
    
    real(gtm_real), allocatable :: temperature(:) 
    
    contains
     
    subroutine sediment_bed_main(resuspension,     &  
                                 deposition,       &
                                 area_wet,         &  
                                 delta_t,          & 
                                 rkstep,           &
                                 nlayers,          &
                                 ncell,            &
                                 nsolids)    
        implicit none
        integer, intent(in) :: ncell                                  !< number of cells
        integer, intent(in) :: nsolids                                !< number of sediment types
        integer, intent(in) :: nlayers                                !< number of sediment bed layers
        real(gtm_real), intent(inout) :: resuspension(ncell,nsolids)  !< g per day
        real(gtm_real), intent(in) :: deposition(ncell,nsolids)       !< g per day
        real(gtm_real), intent(in) :: area_wet(ncell)                 !< m2 todo: may not need this here
        real(gtm_real), intent(in) :: delta_t                         !< time step
        integer, intent(in) :: rkstep                                 !< Huens step (1 or 2)
        
        resuspension = 8888.d0  !you may want to point to the right number here....
        
        return
    end subroutine    

    !> Allocate sediment bed required variables from input time series
    subroutine allocate_sediment_bed(ncell)
        implicit none
        integer, intent(in) :: ncell
        allocate(temperature(ncell))
    end subroutine
    
    !> Deallocate sediment bed required variables from input time series
    subroutine deallocate_sediment_bed()
        implicit none
        deallocate(temperature)
    end subroutine
                
    !> Set the values to the variables
    subroutine set_sediment_bed(input_ts_temp, &
                                ncell)
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: input_ts_temp(ncell)
        temperature = input_ts_temp
    end subroutine

end module