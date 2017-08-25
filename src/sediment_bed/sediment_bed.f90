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
!> The Sediment Bed Module was developed by:
!> Reed Harris, Reed Harris Environmental Ltd.
!> Dave Hutchinson
!> ================================================
!> 
!>@ingroup sediment_bed
    
module sediment_bed
    
use gtm_precision
use sed_type_defs
use sed_internal_vars
use common_variables

implicit none


    contains

subroutine sediment_bed_main(ncells,        &
                            nzones,         &
                            layers,         &
                            nsolids,        &
                            deposition,     &
                            resuspension,   &
                            area,           &
                            wet_width,      &
                            depth,          & 
                            hyd_radius,     & 
                            delta_t,        &
                            rkstep)
        !todo: interface decomposition????avilable
!args

integer, intent (in)             :: ncells
integer, intent (in)             :: nzones
integer, intent (in)             :: layers
integer, intent (in)             :: nsolids
real (gtm_real), intent (in)     :: deposition(ncells, nsolids)     !kg/m2/day
real (gtm_real), intent (inout)  :: resuspension(ncells, nsolids)   !kg/m2/day
real (gtm_real), intent (in)     :: area(ncells)                    ! cross-sectional area
real (gtm_real), intent (in)     :: wet_width(ncells) 
real (gtm_real), intent (in)     :: depth(ncells) 
real (gtm_real), intent (in)     :: hyd_radius(ncells)              
real (gtm_real), intent (in)     :: delta_t                         !time step
integer, intent (in)             :: rkstep                          !Huens step (1 or 2)
!local
real (gtm_real)                  :: dmdt_erosion(ncells, nzones, nsolids)    
real (gtm_real)                  :: frac = 0.9d0                    ! erosion limiter fraction of total mass
real (gtm_real)                  :: srt(ncells,nzones,nsolids)
real (gtm_real)                  :: sum_srt(ncells,nzones)
real (gtm_real)                  :: vb(ncells,nzones)                      !burial velocity
real (gtm_real)                  :: mass_total(ncells, nzones, layers)
real (gtm_real)                  :: erosion_bed(ncells,nsolids)
real (gtm_real)                  :: settling_bed(ncells,nsolids)
real (gtm_real)                  :: wet_p(ncells)
real (gtm_real)                  :: volume(ncells)
real (gtm_real)                  :: vol

integer i, j 
    
    if (rkstep==1) sedsolids(:,:,:,:,1) = sedsolids(:,:,:,:,3)          !update for Huen's method
    volume(:) = (area(:) * length(:)/depth(:)) * (ft_to_m) ** 2 
    do i=1,nsolids
        erosion_bed(:,i) = (resuspension(:, i) * kg_to_g) * volume(:)     !> units g/m2/ss
        settling_bed(:,i) = (deposition (:, i) * kg_to_g) * volume(:)     !> units g/m2/s
    end do
    
    call get_sed_wet_p(ncells, nzones, hyd_radius, area, wet_width, wet_p)
    do i= 1, nzones                                                             !> allocate bed erosion to zones  todo: check for divide by zero
        do j=1, nsolids
            settling(:,i,j, rkstep) =  settling_bed(:,j) * bed(:,i,1).area_wet / (wet_width(:) *length(:))   
            erosion_sb(:,i,j, rkstep) = erosion_bed(:,j)* bed(:,i,1).wp_wet/ (wet_p(:))       
        end do
    end do
    
    bed(:,:,:).inter_frac_base = zero
    call sed_interface_decomposition(ncells, nzones, layers, rkstep)
    !settling(:,:,1, rkstep) = deposition(:, 1) * (one - bed(:,:,1).inter_frac)      !labile turnover at interface
    !carbonturnover_inter(:,:,rkstep)= deposition(:,:, 1) * bed(:,:,1).inter_frac * bed(:,:,1).inter_frac*cfrac_labile  !for mercury module
    bed(:,:,:).r_ct_labile = zero
    bed(:,:,:).r_ct_refract = zero
  
    carbonturnover(:,:,:,1,rkstep) = bed(:,:,:).r_ct_labile * bed(:,:,:).Q10_ct**((bed(:,:,:).T- bed(:,:,:).tb_ct)/ten)* sedsolids(:,:,:,1,rkstep) &
                                        * (bed(:,:,:).wp_wet/bed(:,:,:).wp_zone)
    carbonturnover(:,:,:,2,rkstep) = bed(:,:,:).r_ct_refract * bed(:,:,:).Q10_ct**((bed(:,:,:).T- bed(:,:,:).tb_ct)/ten)* sedsolids(:,:,:,2,rkstep) &
                                        * (bed(:,:,:).wp_wet/bed(:,:,:).wp_zone)
    carbonturnover(:,:,:,3,rkstep) = zero
    decomposition(:,:,:,1,rkstep) = carbonturnover(:,:,:,1,rkstep)/ cfrac_labile 
    decomposition(:,:,:,2,rkstep) = carbonturnover(:,:,:,2,rkstep)/ cfrac_refract
    decomposition(:,:,:,3,rkstep) = zero
    
    !calculate max erosion for top layer only    !todo: ****adjust resuspension and settling to match wetted perimeter for each zone
    dmdt_erosion(:,:,:) = frac * ((sedsolids(:,:,1,:,rkstep) - decomposition(:,:,1,:,rkstep) )/delta_t) 
       
    !do i=1, ncells
    !    do j=1,nsolids
    !        if ((dmdt_erosion(i,1,j) < erosion(i,1,j, rkstep)).and.(rkstep == 1))then
    !           write(*,'(A28, I3, 2x, A10, I4, 2x, A10, I2)') "erosion limited - channel : ",  bed(i,1,1).channel , "cell no : ",  i, "solid no: " , j
    !        end if
    !    end do
    !end do
    !if (dmdt_erosion(:,:,:) > erosion(:,:,:, rkstep)) then
    erosion_sb(:,:,:,rkstep) = min(dmdt_erosion(:,:,:) , erosion_sb(:,:,:, rkstep))
    !end if
    
    resuspension(:, :) = zero
    do i=1, ncells
        vol = volume(i)
        do j=1,nzones
            resuspension(i, :) = resuspension(i, :) + erosion_sb(i,j,:, rkstep)/vol/kg_to_g  !pass erosion rates back to GTM
        end do
    end do
    !burial calculations top layer only
    do i = 1, nsolids
        srt(:,:,i) = (settling(:,:,i, rkstep)-decomposition(:,:,1,i,rkstep) - erosion_sb(:,:,i, rkstep))
    end do
    sum_srt(:,:) = (srt(:,:,1)/density(1) + srt(:,:,2)/density(2) + srt(:,:,3)/density(3)) !/(one - bed(:,:,1).porosity)
    
    do i=1,ncells
        do j=1,nzones
            if (sum_srt(i,j)>= 0) then    !positive burial
                vb(i,j) = sum_srt(i,j) * bed(i,j, 1).thickness /(sedsolids(i,j,1,1,rkstep)/density(1) + sedsolids(i,j,1,2,rkstep)/density(2) + sedsolids(i,j,1,3,rkstep)/density(3))
                burial(i,j,1,:, rkstep) = vb(i,j) * sedsolids(i,j,1,:,rkstep)/bed(i,:, 1).thickness
            else                        !-ve burial i.e. erosion into underlying layer
                vb(i,j) = sum_srt(i,j) * bed(i,j, 2).thickness /(sedsolids(i,j,2,1,rkstep)/density(1) + sedsolids(i,j,2,2,rkstep)/density(2) + sedsolids(i,j,2,3,rkstep)/density(3))
                burial(i,j,1,:, rkstep) = vb(i,j) * sedsolids(i,j,2,:,rkstep)/bed(i,j,2).thickness
            end if
        end do
    end do
    
    do i=1,nsolids
      sedsolidsflux(:,:,1,i,rkstep) = settling(:,:,i, rkstep)-decomposition(:,:,1,i,rkstep) - erosion_sb(:,:,i, rkstep) - burial(:,:,1,i, rkstep)
      sedsolidsflux(:,:,2,i,rkstep) = -decomposition(:,:,2,i,rkstep) + burial(:,:,1,i, rkstep)
    end do
    !sedsolidsflux(:,2,:,rkstep) = -decomposition(:,2,:,rkstep) + burial(:,1,:, rkstep)
    
    ! Huens method integration
    if (rkstep==1) then  
        sedsolids(:,:,:,:,rkstep+1) = sedsolids(:,:,:,:,rkstep) + delta_t * sedsolidsflux(:,:,:,:,rkstep)
    else
        sedsolids(:,:,:,:,rkstep+1) = sedsolids(:,:,:,:,rkstep-1) + delta_t * (sedsolidsflux(:,:,:,:,rkstep-1) + sedsolidsflux(:,:,:,:,rkstep))/two
    end if
  
    bed(:,:,1).thickness = ((sedsolids(:,:,1,1,rkstep+1)/density(1)) + (sedsolids(:,:,1,2,rkstep+1)/density(2)) + (sedsolids(:,:,1,3,rkstep+1)/density(3)))/((one - bed(:,:,1).porosity)*bed(:,:,1).wp_zone*ft_to_m)
    bed(:,:,2).thickness = ((sedsolids(:,:,2,1,rkstep+1)/density(1)) + (sedsolids(:,:,2,2,rkstep+1)/density(2)) + (sedsolids(:,:,2,3,rkstep+1)/density(3)))/((one - bed(:,:,2).porosity)*bed(:,:,2).wp_zone*ft_to_m)
    
    do i=1,nzones
        bed(:,i,1).thickness = bed(:,i,1).thickness/(length(:)*ft_to_m)
        bed(:,i,2).thickness = bed(:,i,2).thickness/(length(:)*ft_to_m)
    end do
    mass_total(:, :,:) =  sedsolids(:,:,:,1,rkstep+1)+sedsolids(:,:,:,2,rkstep+1)+sedsolids(:,:,:,3,rkstep+1)
    bed(:,:,:).mass_frac(1)= sedsolids(:,:,:,1,rkstep+1)/mass_total(:, :,:)
    bed(:,:,:).mass_frac(2)= sedsolids(:,:,:,2,rkstep+1)/mass_total(:, :,:)
    bed(:,:,:).mass_frac(3)= sedsolids(:,:,:,3,rkstep+1)/mass_total(:, :,:)
    
    if (rkstep==2) then     !for outputs - burial from layer 1 to layer 2 by zone
        burial_total(:,:) = half*(burial(:,:,1,1, 1) + burial(:,:,1,2, 1) + burial(:,:,1,3, 1) + burial(:,:,1,1, 2) + burial(:,:,1,2, 2) + burial(:,:,1,3, 2))/bed(:,:,1).area_zone
    end if
end subroutine sediment_bed_main

subroutine get_sed_wet_p(ncells, nzones, hyd_radius, area, width, wet_p)
    !args
    integer, intent (in)            :: ncells
    integer, intent (in)            :: nzones
    real (gtm_real), intent (in)    :: hyd_radius(ncells)  !from gtm
    real (gtm_real), intent (in)    :: area(ncells)        !from gtm
    real (gtm_real), intent (in)    :: width(ncells)       !from gtm
    real (gtm_real), intent (out)   :: wet_p(ncells)       
    integer :: icll, iz
    bed(:,:,1).area_wet = zero
    bed(:,:,1).wp_wet = zero
    wet_p(:) = area(:)/hyd_radius(:)
    do icll = 1, ncells
        do iz = 1, nzones
            if (wet_p(icll)>= bed(icll,iz,1).wp_cell) then
                bed(icll,iz,1).wp_wet = bed(icll,iz,1).wp_zone
                bed(icll,iz,1).area_wet = bed(icll,iz,1).area_zone 
            else
                if (iz==1) then
                     bed(icll,iz,1).wp_wet = wet_p(icll)
                     bed(icll,iz,1).area_wet = width(icll)*length(icll)  
                else
                    if (wet_p(icll)> bed(icll,iz-1,1).wp_cell) then
                        bed(icll,iz,1).wp_wet = wet_p(icll) - bed(icll,iz-1,1).wp_cell
                        bed(icll,iz,1).area_wet = (width(icll))*length(icll) - bed(icll,iz-1,1).area_cell
                    end if
                end if
            end if
        end do
    end do
    
    bed(:,:,2).wp_wet = bed(:,:,1).wp_wet
    bed(:,:,2).area_wet = bed(:,:,1).area_wet
    return
end subroutine get_sed_wet_p
    
subroutine sed_interface_decomposition(ncells, nzones, layers, rkstep)
    !args
    integer, intent (in)             :: ncells
    integer, intent (in)             :: nzones
    integer, intent (in)             :: layers
    integer, intent (in)             :: rkstep                          !Huens step (1 or 2)
    !local

        !bed(:,:,1).inter_frac = bed(:,:,1).inter_frac_max*(one - one /(one + bed(:,:,1).inter_k* bed(:,:,1).q10_ct_inter**( (bed(:,:,1).inter_a1 * temperature(:,1) - bed(:,:,1).tb_ct_inter)/ten) &
        !                     - bed(:,:,1).inter_a2))
      
        bed(:,:,1).inter_frac = bed(:,:,1).inter_frac_base * bed(:,:,1).q10_ct_inter**( ( bed(:,:,1).T - bed(:,:,1).tb_ct_inter)/ten) 
        
end subroutine sed_interface_decomposition

 !> Allocate sediment bed required variables from input time series
    subroutine allocate_sediment_bed(ncell)
        implicit none
        integer, intent(in) :: ncell
     !   allocate(temperature(ncell))
    end subroutine
    
    !> Deallocate sediment bed required variables from input time series
    subroutine deallocate_sediment_bed()
        implicit none
     !   deallocate(temperature)
    end subroutine
                
    !> Set the values to the variables
    subroutine set_sediment_bed(input_ts_temp, &
                                ncell)
        implicit none
        integer, intent(in) :: ncell
        real(gtm_real), intent(in) :: input_ts_temp(ncell)

        integer :: ii
        do ii=1,ncell
            bed(ii,:,:)%t = five*(input_ts_temp(ii)-32.0d0)/nine
        end do            
        !temperature = input_ts_temp
    end subroutine
subroutine pass()  !????



end subroutine pass

end module