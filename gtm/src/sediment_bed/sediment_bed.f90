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
                            wet_p,          &
                            dx,             &
                            delta_t,        &
                            rkstep)
        !todo: interface decomposition????avilable
    !args

    integer, intent (in)             :: ncells
    integer, intent (in)             :: nzones
    integer, intent (in)             :: layers
    integer, intent (in)             :: nsolids
    real (gtm_real), intent (in)     :: deposition(ncells, nsolids)     !kg/m2/s 
    real (gtm_real), intent (inout)  :: resuspension(ncells, nsolids)   !kg/m2/s  
    real (gtm_real), intent (in)     :: area(ncells)                    !cross-sectional area
    real (gtm_real), intent (in)     :: wet_width(ncells) 
    real (gtm_real), intent (in)     :: depth(ncells) 
    real (gtm_real), intent (in)     :: wet_p(ncells)              
    real (gtm_real), intent (in)     :: delta_t                         !time step
    real (gtm_real), intent (in)     :: dx(ncells)                      !cell length ft
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
    real (gtm_real)                  :: area_si(ncells)
    real (gtm_real)                  :: vol
    integer i, j, k
    real (gtm_real)                  :: erosion_total(ncells,nzones)
    real (gtm_real)                  :: wp_max(ncells)
    real (gtm_real)                  :: total_burial
    real (gtm_real)                  :: delta_erosion
    integer                          :: erosion_type
    real (gtm_real)                  :: total_ct
     real (gtm_real)                 :: total_area_wet
    !check_resusp = resuspension
    wp_max = wet_p*ft_to_m 
    call get_sed_wet_p(ncells, nzones, area, wet_width, wet_p)
    do i=1,ncells
        if (wp_max(i).GT.bed(i,3,1).wp_cell) then
            wp_max(i)= bed(i,3,1).wp_cell
        end if
    end do
    if (rkstep==1) sedsolids(:,:,:,:,1) = sedsolids(:,:,:,:,3)          !update for Huen's method
    !volume(:) = (area(:) * length(:)/depth(:)) * (ft_to_m)              !todo: check this looks like surface area 
    area_si(:) = wet_width(:)*dx(:) * (ft_to_m) * (ft_to_m)
    
    do i=1,nsolids
        erosion_bed(:,i) = (resuspension(:, i) * kg_to_g) * area_si(:)   !> units g/m2/s
        settling_bed(:,i) = (deposition (:, i) * kg_to_g) * area_si(:)   !> units g/m2/s
    end do
    
    !turnover_inter(:,:,rkstep)= deposition(:,:, 2) * bed(:,:,2).inter_frac * bed(:,:,2).inter_frac*cfrac_labile
    !settling(:,:,1, rkstep) = deposition(:, 1) * (1.0d0 - bed(:,:,1).inter_frac)      !labile turnover at interface
    !carbonturnover_inter(:,rkstep)= deposition(:,:, 2) * bed(:,:,2).inter_frac * bed(:,:,2).inter_frac*cfrac_labile  !for mercury module
    
    !bed(:,:,:).inter_frac_base = zero !todo: this is here for debugging
    call sed_interface_decomposition(ncells, nzones, layers, rkstep)
       
    carbonturnover_inter = zero
    decomposition_inter = zero
    wat_decomposition_inter = zero
    
    
    do i= 1, nzones                                                      !> allocate bed erosion and settling to zones  todo: check for divide by zero
        do j=1, nsolids
            settling(:,i,j, rkstep) =  settling_bed(:,j) * bed(:,i,1).area_wet / (area_si(:))  
            !erosion_sb(:,i,j, rkstep) = erosion_bed(:,j)* bed(:,i,1).wp_wet/ (wp_max(:))
            erosion_sb(:,i,j, rkstep) = erosion_bed(:,j)* bed(:,i,1).area_wet / (area_si(:))
        end do
        if (nsolids.gt.2) decomposition_inter(:,i,rkstep)= settling(:,i, 2, rkstep) * bed(:,i,1).inter_frac          !labile turnover at interface
        carbonturnover_inter(:,i,rkstep) = decomposition_inter(:,i,rkstep)*cfrac_labile 
        wat_decomposition_inter(:, rkstep)= wat_decomposition_inter(:, rkstep) + decomposition_inter(:,i,rkstep)  !for inert g
    enddo
    if (nsolids.gt.2) settling(:,:,2, rkstep) = settling(:,:,2, rkstep) - decomposition_inter(:,:,rkstep)             !minus labile 
    wat_carbonturnover_inter =  wat_decomposition_inter*cfrac_labile                                !for interface methylation
    
                                                                        !erosion of cohesive seds proportional to amount in bed
    erosion_total(:,:) =  erosion_sb(:,:,1, rkstep)
    if (nsolids.gt.2)  erosion_total(:,:) =  erosion_total(:,:) + erosion_sb(:,:,2, rkstep) 
    
    mass_total(:,:,:) = sedsolids(:,:,:,1,rkstep)                       !total cohesive
    if (nsolids.gt.2)  mass_total(:,:,:) =  mass_total(:,:,:) + sedsolids(:,:,:,2,rkstep)
    
    do i=1,ncells
        do j = 1, nzones  
           erosion_type = 0 
           if ((erosion_total(i,j)*delta_t).gt.(frac*(sedsolids(i,j,1,1,rkstep)+sedsolids(i,j,1,2,rkstep)))) then
                delta_erosion = erosion_total(i,j) - frac*(sedsolids(i,j,1,1,rkstep)+sedsolids(i,j,1,2,rkstep))/delta_t
                erosion_type = 1
            endif
            if (erosion_sb(i,j,3, rkstep)*delta_t > (sedsolids(i,j,1,3,rkstep))) then
                delta_erosion = erosion_sb(i,j,3, rkstep) - sedsolids(i,j,1,3,rkstep)/delta_t
                erosion_type = 2
            endif
            select case (erosion_type)
            case (1)
                erosion_sb(i,j,3, rkstep) = erosion_sb(i,j,3, rkstep)- delta_erosion
            case (2)
                erosion_total(i,j) = erosion_total(i,j) - delta_erosion
            end select
        enddo
    enddo
    
    erosion_sb(:,:,1, rkstep) = erosion_total(:,:)*sedsolids(:,:,1,1,rkstep)/ mass_total(:,:,1)
    if (nsolids.gt.2) erosion_sb(:,:,2, rkstep) = erosion_total(:,:)*sedsolids(:,:,1,2,rkstep)/ mass_total(:,:,1)
      
    
   ! bed(:,:,:).r_ct_labile = zero  !todo:remove after debugging     
   ! bed(:,:,:).r_ct_refract = zero
  
    carbonturnover(:,:,:,1,rkstep) = bed(:,:,:).r_ct_refract * bed(:,:,:).Q10_ct**((bed(:,:,:).T- bed(:,:,:).tb_ct)/ten)* sedsolids(:,:,:,1,rkstep) &
                                        * (bed(:,:,:).wp_wet/bed(:,:,:).wp_zone)*cfrac_refract
    if (nsolids.gt.2) then
        carbonturnover(:,:,:,2,rkstep) = bed(:,:,:).r_ct_labile * bed(:,:,:).Q10_ct**((bed(:,:,:).T- bed(:,:,:).tb_ct)/ten)* sedsolids(:,:,:,2,rkstep) &
                                        * (bed(:,:,:).wp_wet/bed(:,:,:).wp_zone)* cfrac_labile
        carbonturnover(:,:,:,3,rkstep) = zero
    else    
        carbonturnover(:,:,:,2,rkstep) = zero
    endif
    decomposition(:,:,:,1,rkstep) = carbonturnover(:,:,:,1,rkstep)/ cfrac_refract
    if (nsolids.gt.2) then
        decomposition(:,:,:,2,rkstep) = carbonturnover(:,:,:,2,rkstep)/ cfrac_labile 
        decomposition(:,:,:,3,rkstep) = zero
    else
        decomposition(:,:,:,2,rkstep) = zero
    endif 
    
    
    !calculate max erosion for top layer only       
    
    do i = 1, nsolids
        srt(:,:,i) = (settling(:,:,i, rkstep)-decomposition(:,:,1,i,rkstep) - erosion_sb(:,:,i, rkstep))
    end do
   
    burial(:,:,1,:, rkstep) = srt(:,:,:)
    
    burial_total(:,:) = srt(:,:,1) + srt(:,:,2) + srt(:,:,3)
    
    do i=1,ncells
        do j = 1, nzones
            
            if (burial_total(i,j).lt.0) then    !erosion into layer 2 added 20190503
                burial(i,j,1,1, rkstep) = bed(i,j,2).mass_frac(1)* burial_total(i,j)
                burial(i,j,1,2, rkstep) = bed(i,j,2).mass_frac(2)* burial_total(i,j)
                burial(i,j,1,3, rkstep) = bed(i,j,2).mass_frac(3)* burial_total(i,j)     
            end if
            
            
            !if ((burial(i,j,1,1, rkstep).lt.0).and.(burial(i,j,1,2, rkstep).lt.0))then  !erosion
            !    total_burial = burial(i,j,1,1, rkstep)+ burial(i,j,1,2, rkstep)
            !   burial(i,j,1,1, rkstep) = (sedsolids(i,j,2,1,rkstep)/ mass_total(i,j,2))*total_burial
            !    burial(i,j,1,2, rkstep) = (sedsolids(i,j,2,2,rkstep)/ mass_total(i,j,2))*total_burial
            !end if
            !limit erosion if neccesary 
            if ((burial(i,j,1,1, rkstep).lt.0).and.(-burial(i,j,1,1, rkstep)*delta_t>=frac*sedsolids(i,j,2,1,rkstep))) then
                burial(i,j,1,1, rkstep) = frac*sedsolids(i,j,2,1,rkstep)/delta_t
                erosion_sb(i,j,1, rkstep) = settling(i,j,1, rkstep)-decomposition(i,j,1,1,rkstep)-burial(i,j,1,1, rkstep)
            end if
            if ((burial(i,j,1,2, rkstep).lt.0).and.(-burial(i,j,1,2, rkstep)*delta_t>=half*sedsolids(i,j,2,2,rkstep))) then
                burial(i,j,1,2, rkstep) = frac*sedsolids(i,j,2,2,rkstep)/delta_t
                erosion_sb(i,j,2, rkstep) = settling(i,j,2, rkstep)-decomposition(i,j,1,2,rkstep)-burial(i,j,1,2, rkstep)
            end if
            if ((burial(i,j,1,3, rkstep).lt.0).and.(-burial(i,j,1,3, rkstep)*delta_t>=frac*sedsolids(i,j,2,3,rkstep))) then
                burial(i,j,1,3, rkstep) = frac*sedsolids(i,j,2,3,rkstep)/delta_t
                erosion_sb(i,j,3, rkstep) = settling(i,j,3, rkstep)-decomposition(i,j,1,3,rkstep)-burial(i,j,1,3, rkstep)
            end if
        end do       
    end do
    resuspension(:, :) = zero
    do i=1, ncells
        vol = area_si(i)
        do j=1,nzones
            resuspension(i, :) =  resuspension(i, :) +  erosion_sb(i,j,:,rkstep)/(vol*kg_to_g) !pass erosion rates back to GTM
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
  
    bed(:,:,1).thickness = ((sedsolids(:,:,1,1,rkstep+1)/density(1)) + (sedsolids(:,:,1,2,rkstep+1)/density(2)) + (sedsolids(:,:,1,3,rkstep+1)/density(3)))/((1.0d0 - bed(:,:,1).porosity)*bed(:,:,1).wp_zone) !*ft_to_m)
    bed(:,:,2).thickness = ((sedsolids(:,:,2,1,rkstep+1)/density(1)) + (sedsolids(:,:,2,2,rkstep+1)/density(2)) + (sedsolids(:,:,2,3,rkstep+1)/density(3)))/((1.0d0 - bed(:,:,2).porosity)*bed(:,:,2).wp_zone) !*ft_to_m)
    
    bed(:,:,2).volume = bed(:,:,2).thickness*bed(:,:,2).wp_zone !todo: for debugging remove
    
    do i=1,nzones
        bed(:,i,1).thickness = bed(:,i,1).thickness/(length(:))
        bed(:,i,2).thickness = bed(:,i,2).thickness/(length(:))
    end do
    
    mass_total(:, :,:) =  sedsolids(:,:,:,1,rkstep+1)+sedsolids(:,:,:,2,rkstep+1)+sedsolids(:,:,:,3,rkstep+1)
    bed(:,:,:).mass_frac(1)= sedsolids(:,:,:,1,rkstep+1)/mass_total(:, :,:)
    bed(:,:,:).mass_frac(2)= sedsolids(:,:,:,2,rkstep+1)/mass_total(:, :,:)
    bed(:,:,:).mass_frac(3)= sedsolids(:,:,:,3,rkstep+1)/mass_total(:, :,:)
    
    !for water column total carbon turnover
    do i=1, ncells
        total_ct = zero
        total_area_wet = zero
        do j=1,nzones
            total_ct = total_ct + carbonturnover_inter(i,j,rkstep)
            total_area_wet = total_area_wet + bed(i,j,1).area_wet
        end do
        if (total_area_wet.gt.zero) then
            r_ct_interface(i) = total_ct/total_area_wet
        else
             r_ct_interface(i) = zero
        endif
    end do
    
    if (rkstep==2) then     !for outputs - burial from layer 1 to layer 2 by zone
        burial_total(:,:) = half*(burial(:,:,1,1, 1) + burial(:,:,1,2, 1) + burial(:,:,1,3, 1) + burial(:,:,1,1, 2) + burial(:,:,1,2, 2) + burial(:,:,1,3, 2))/bed(:,:,1).area_zone
    end if
    
    
    
end subroutine sediment_bed_main



subroutine get_sed_wet_p(ncells, nzones, area, width, wet_p)
    !args
    integer, intent (in)            :: ncells
    integer, intent (in)            :: nzones
    !real (gtm_real), intent (in)    :: hyd_radius(ncells)  !from gtm
    real (gtm_real), intent (in)    :: area(ncells)        !from gtm
    real (gtm_real), intent (in)    :: width(ncells)       !from gtm
    real (gtm_real), intent (in)   :: wet_p(ncells)       
    integer :: icll, iz
    real(gtm_real), parameter :: L = 0.3048d0       !ft->m
    
    bed(:,:,1).area_wet = zero
    bed(:,:,1).wp_wet = zero
    !wet_p(:) = area(:)/hyd_radius(:)
    do icll = 1, ncells
        do iz = 1, nzones
            if ((wet_p(icll)*L)>= bed(icll,iz,1).wp_cell) then
                bed(icll,iz,1).wp_wet = bed(icll,iz,1).wp_zone
                bed(icll,iz,1).area_wet = bed(icll,iz,1).area_zone 
            else
                if (iz==1) then
                     bed(icll,iz,1).wp_wet = wet_p(icll)*L
                     bed(icll,iz,1).area_wet = width(icll)*length(icll)*L  
                else
                    if ((wet_p(icll)*L)> bed(icll,iz-1,1).wp_cell) then
                        bed(icll,iz,1).wp_wet = wet_p(icll)*L - bed(icll,iz-1,1).wp_cell
                        bed(icll,iz,1).area_wet = (width(icll)*L)*length(icll) - bed(icll,iz-1,1).area_cell
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