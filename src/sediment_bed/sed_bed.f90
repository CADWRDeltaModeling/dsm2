module sed_bed
    
use gtm_precision
use sed_type_defs
use sed_internal_vars

implicit none

    contains

subroutine sed_solids_calcs(ncells, nzones, layers, nsolids, deposition, resuspension, temperature, wet_p, wet_width, delta_t, rkstep)
        !todo: interface decomposition????
!args

integer, intent (in)             :: ncells
integer, intent (in)             :: nzones
integer, intent (in)             :: layers
integer, intent (in)             :: nsolids
real (gtm_real), intent (in)     :: deposition(ncells, nsolids)     !g per day
real (gtm_real), intent (inout)  :: resuspension(ncells, nsolids)   !g per day
real (gtm_real), intent (in)     :: temperature(ncells,nzones,layers) 
real (gtm_real), intent (in)     :: wet_p(ncells)                   !m2 todo: may not need this here
real (gtm_real), intent (in)     :: wet_width(ncells)                   !m2 todo: may not need this here
real (gtm_real), intent (in)     :: delta_t                         !time step
integer, intent (in)             :: rkstep                          !Huens step (1 or 2)
!local
real (gtm_real)                  :: dmdt_erosion(ncells, nzones)    
real (gtm_real)                  :: frac = 0.9d0                    ! erosion limiter fraction of total mass
real (gtm_real)                  :: srt(ncells,nzones,nsolids)
real (gtm_real)                  :: sum_srt(ncells,nzones)
real (gtm_real)                  :: vb(ncells,nzones)                      !burial velocity
real (gtm_real)                  :: mass_total(ncells, nzones, layers)

integer i, j 

    call get_sed_wet_p(ncells, nzones, wet_p, wet_width)
    do i= 1, nzones
        do j=1, nsolids
            settling(:,i,j, rkstep) = deposition(:, j)* bed(:,i,1).area_wet / (wet_width(:)*length(:))    !todo: ****adjust resuspension and settling to match wetted perimeter/area for each zone
        end do
    end do
    
    do i= 1, nzones
        do j=1, nsolids
            erosion(:,i,j, rkstep) = resuspension(:, j)* bed(:,i,1).wp_wet/ (wet_width(:)*length(:))       !todo: ****adjust resuspension and settling to match wetted perimeter for each zone
        end do
    end do
    
    call sed_interface_decomposition(ncells, nzones, layers, temperature, rkstep)
    !settling(:,:,1, rkstep) = deposition(:, 1) * (one - bed(:,:,1).inter_frac)      !labile turnover at interface
    !carbonturnover_inter(:,:,rkstep)= deposition(:,:, 1) * bed(:,:,1).inter_frac * bed(:,:,1).inter_frac*cfrac_labile  !for mercury module
    
    if (rkstep==1) sedsolids(:,:,:,:,1) = sedsolids(:,:,:,:,3)          !update for Huen's method
    carbonturnover(:,:,:,1,rkstep) = bed(:,:,:).r_ct_labile * bed(:,:,:).Q10_ct**((temperature(:,:,:)- bed(:,:,:).tb_ct)/ten)* sedsolids(:,:,:,1,rkstep)
    carbonturnover(:,:,:,2,rkstep) = bed(:,:,:).r_ct_refract * bed(:,:,:).Q10_ct**((temperature(:,:,:)- bed(:,:,:).tb_ct)/ten)* sedsolids(:,:,:,2,rkstep)
    carbonturnover(:,:,:,3,rkstep) = zero
    decomposition(:,:,:,1,rkstep) = carbonturnover(:,:,:,1,rkstep)/ cfrac_labile 
    decomposition(:,:,:,2,rkstep) = carbonturnover(:,:,:,2,rkstep)/ cfrac_refract
    decomposition(:,:,:,3,rkstep) = zero
    
    !calculate max erosion for top layer only    !todo: ****adjust resuspension and settling to match wetted perimeter for each zone
    dmdt_erosion(:,:) = frac * (((sedsolids(:,:,1,1,rkstep) + sedsolids(:,:,1,2,rkstep))/delta_t)) - decomposition(:,:,1,1,rkstep) - decomposition(:,:,1,2,rkstep)
    dmdt_erosion(:,:) = min(dmdt_erosion(:,:), erosion(:,:,1, rkstep)+erosion(:,:,2, rkstep))
    erosion(:,:,1,rkstep) = dmdt_erosion(:,:)*(sedsolids(:,:,1,1,rkstep)/(sedsolids(:,:,1,1,rkstep)+sedsolids(:,:,1,2,rkstep)))
    erosion(:,:,2,rkstep) = dmdt_erosion(:,:)*(sedsolids(:,:,1,2,rkstep)/(sedsolids(:,:,1,1,rkstep)+sedsolids(:,:,1,2,rkstep)))
    erosion(:,:,3,rkstep) = min(frac* sedsolids(:,:,1,3,rkstep)/delta_t , erosion(:,:,3, rkstep))
    
    resuspension(:, :) = zero
    !do i=1, ncells
        do j=1,nzones
            resuspension(:, :) = resuspension(:, :) + erosion(:,j,:, rkstep)   !pass erosion rates back to GTM
        end do
    !end od
    !burial calculations top layer only
    do i = 1, nsolids
        srt(:,:,i) = (settling(:,:,i, rkstep)-decomposition(:,:,1,i,rkstep) - erosion(:,:,i, rkstep))/density(i)
    end do
    sum_srt(:,:) = srt(:,:,1) + srt(:,:,2) + srt(:,:,3)
    
    do i=1,ncells
        do j=1,nzones
            if (sum_srt(i,j)>= 0) then    !positive burial
                vb(i,j) = sum_srt(i,j)/((sedsolids(i,j,1,1,rkstep)/density(1)) + (sedsolids(i,j,1,2,rkstep)/density(2)) + (sedsolids(i,j,1,3,rkstep)/density(3)))
                burial(i,j,1,:, rkstep) = vb(i,j) * sedsolids(i,j,1,:,rkstep)!/bed(i,1).thickness
            else                        !-ve burial i.e. erosion into underlying layer
                vb(i,j) = sum_srt(i,j)/((sedsolids(i,j,2,1,rkstep)/density(1)) + (sedsolids(i,j,2,2,rkstep)/density(2)) + (sedsolids(i,j,2,3,rkstep)/density(3)))
                burial(i,j,1,:, rkstep) = vb(i,j) * sedsolids(i,j,2,:,rkstep)!/bed(i,2).thickness
            end if
        end do
    end do
    
    do i=1,nsolids
      sedsolidsflux(:,:,1,i,rkstep) = settling(:,:,i, rkstep)-decomposition(:,:,1,i,rkstep) - erosion(:,:,i, rkstep) - burial(:,:,1,i, rkstep)
      sedsolidsflux(:,:,2,i,rkstep) = -decomposition(:,:,2,i,rkstep) + burial(:,:,1,i, rkstep)
    end do
    !sedsolidsflux(:,2,:,rkstep) = -decomposition(:,2,:,rkstep) + burial(:,1,:, rkstep)
    
    ! Huens method integration
    if (rkstep==1) then  
        sedsolids(:,:,:,:,rkstep+1) = sedsolids(:,:,:,:,rkstep) + delta_t * sedsolidsflux(:,:,:,:,rkstep)
    else
        sedsolids(:,:,:,:,rkstep+1) = sedsolids(:,:,:,:,rkstep) + delta_t * (sedsolidsflux(:,:,:,:,rkstep-1) + sedsolidsflux(:,:,:,:,rkstep))/two
    end if
  
    bed(:,:,1).thickness = ((sedsolids(:,:,1,1,rkstep+1)/density(1)) + (sedsolids(:,:,1,2,rkstep+1)/density(2)) + (sedsolids(:,:,1,3,rkstep+1)/density(3)))/((one - bed(:,:,1).porosity)*bed(:,:,1).wp_zone)
    bed(:,:,2).thickness = ((sedsolids(:,:,2,1,rkstep+1)/density(1)) + (sedsolids(:,:,2,2,rkstep+1)/density(2)) + (sedsolids(:,:,2,3,rkstep+1)/density(3)))/((one - bed(:,:,2).porosity)*bed(:,:,2).wp_zone)
    
    mass_total(:, :,:) =  sedsolids(:,:,:,1,rkstep+1)+sedsolids(:,:,:,2,rkstep+1)+sedsolids(:,:,:,3,rkstep+1)
    bed(:,:,:).mass_frac(1)= sedsolids(:,:,:,1,rkstep+1)/mass_total(:, :,:)
    bed(:,:,:).mass_frac(2)= sedsolids(:,:,:,2,rkstep+1)/mass_total(:, :,:)
    bed(:,:,:).mass_frac(3)= sedsolids(:,:,:,3,rkstep+1)/mass_total(:, :,:)
    
    if (rkstep==2) then     !for outputs - burial from layer 1 to layer 2 by zone
        burial_total(:,:) = half*(burial(:,:,1,1, 1) + burial(:,:,1,2, 1) + burial(:,:,1,3, 1) + burial(:,:,1,1, 2) + burial(:,:,1,2, 2) + burial(:,:,1,3, 2))/bed(:,:,1).area_zone
    end if
end subroutine sed_solids_calcs

subroutine get_sed_wet_p(ncells, nzones, wet_p, width)
    !args
    integer, intent (in)             :: ncells
    integer, intent (in)             :: nzones
    real (gtm_real), intent (in)     :: wet_p(ncells)   !from gtm
    real (gtm_real), intent (in)     :: width(ncells)   !from gtm
    !local
    integer :: icll, iz
    bed(icll,iz,1).area_wet = zero
    do icll = 1, ncells
        do iz = 1, nzones
            if (wet_p(icll)>= bed(icll,iz,1).wp_cell) then
                bed(icll,iz,1).wp_wet = bed(icll,iz,1).wp_zone
                bed(icll,iz,1).area_wet = bed(icll,iz,1).area_zone 
            else
                if (iz==1) then
                     bed(icll,iz,1).wp_wet = wet_p(icll)
                     bed(icll,iz,1).area_wet = width(icll)  !todo:*length
                else
                    if (wet_p(icll)> bed(icll,iz-1,1).wp_cell) then
                        bed(icll,iz,1).wp_wet = wet_p(icll) - bed(icll,iz-1,1).wp_cell
                        bed(icll,iz,1).area_wet = (width(icll)) - bed(icll,iz-1,1).area_cell  !todo: width(icll)*length
                    end if
                end if
            end if
        end do
    end do
    
    bed(icll,iz,2).wp_wet = bed(icll,iz,1).wp_wet
    bed(icll,iz,2).area_wet = bed(icll,iz,1).area_wet
    return
end subroutine get_sed_wet_p
    
subroutine sed_interface_decomposition(ncells, nzones, layers, temperature, rkstep)
    !args
    integer, intent (in)             :: ncells
    integer, intent (in)             :: nzones
    integer, intent (in)             :: layers
    real (gtm_real), intent (in)     :: temperature(ncells,nzones) 
    integer, intent (in)             :: rkstep                          !Huens step (1 or 2)
    !local

        !bed(:,:,1).inter_frac = bed(:,:,1).inter_frac_max*(one - one /(one + bed(:,:,1).inter_k* bed(:,:,1).q10_ct_inter**( (bed(:,:,1).inter_a1 * temperature(:,1) - bed(:,:,1).tb_ct_inter)/ten) &
        !                     - bed(:,:,1).inter_a2))
      
        bed(:,:,1).inter_frac = bed(:,:,1).inter_frac_base * bed(:,:,1).q10_ct_inter**( ( temperature(:,:) - bed(:,:,1).tb_ct_inter)/ten) 
        
end subroutine sed_interface_decomposition

 
subroutine pass()



end subroutine pass

end module