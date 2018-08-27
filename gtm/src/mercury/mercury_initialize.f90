module mercury_initialize

use gtm_precision
use sed_type_defs
use sed_internal_vars
use hg_internal_vars
use equilibrium
use sediment_bed_setup, only: sed_read_init_values
use mercury_state_variables
use hg_hdf
use common_variables
use state_variables
use sediment_bed, only: get_sed_wet_p

implicit none
    
    logical :: initialize_bed_mercury = .false.
    contains
    
    subroutine hg_ic_alternate()
    
    end subroutine
    
    subroutine hg_init_sediment_bed(n_cells, n_chans, init_input_file, sim_start, sim_end, hdf_interval_char, use_gtm_hdf)
        use common_dsm2_vars, only: gtm, io_write, io_restart, io_hdf5, io_read, io_files,pathinput_t, n_dssfiles,ifltab_in,indssfiles,infilenames
        use error_handling
         
        !args
        integer, intent(in) :: n_cells
        integer, intent(in) :: n_chans
        character* (*) ,  intent(in)::  init_input_file
        integer, intent(in) :: sim_start
        integer, intent(in) :: sim_end
        character*16 :: hdf_interval_char
        logical, intent(in):: use_gtm_hdf
        !local
        logical :: file_exists
        integer :: ii
        !real (gtm_real) :: wet_p(n_cells)
        real (gtm_real) :: hyd_radius(n_cells)  
       
        ! get Hg from qrf file
        file_name_qrf_sedhg = io_files(gtm,io_hdf5, io_write).filename
        ii = index(file_name_qrf_sedhg,".", .true.)
        file_name_qrf_sedhg(ii:ii+9) = "_sedHg.qrf"
        inquire(file=file_name_qrf, exist=file_exists)
        if (file_exists==.false.) then 
            call gtm_fatal("sediment bed Hg module - "//trim(file_name_qrf_sedhg)//" does not exist!!")
        end if
        call sed_read_init_values(n_cells, n_zones, n_resv, file_name_qrf_sedhg, 12, 2)
        ! sediment bed Hg outputs
        file_name_hdf_sedHg = io_files(gtm,io_hdf5, io_write).filename      !hdf output file
        ii = index(file_name_hdf_sedHg,".", .true.)
        file_name_hdf_sedHg(ii:ii+9) = "_hg.h5"        
        
        call init_sed_hg_hdf(n_cells, n_chans, sim_start, sim_end, hdf_interval_char, use_gtm_hdf)
        
        hyd_radius = area/wet_p
        call get_sed_wet_p(n_cells, n_zones, area, width, wet_p)
        call init_sed_partitioning(n_cells,n_zones,n_layers)
        
    end subroutine hg_init_sediment_bed
    
    subroutine init_sed_partitioning(n_cells,n_zones,n_layers)
        !args
        integer, intent(in)         :: n_cells
        integer, intent(in)         :: n_zones
        integer, intent(in)         :: n_layers
        !local
        integer                     :: icell, izone, ilayer
        integer                     :: iter
        logical                     :: converge
        real(gtm_real), allocatable :: mass_total(:,:,:)
        real(gtm_real), allocatable :: volume_pw(:,:,:)
        type (molar_total)          :: total 
        !type (eq_vals)              :: vals
        type (eq_complexes)         :: m 
        real(gtm_real)              :: hh                       ![H++]
        real(gtm_real)              :: dissolvedHg 
        real (gtm_real) :: check
        allocate(mass_total(n_cells,n_zones,n_layers))
        allocate(volume_pw(n_cells,n_zones,n_layers))
        
        eq_vals_sed%hgii = zero
        eq_vals_sed%mehg = zero  
        eq_vals_sed%rs = zero
        eq_vals_sed%xoh = zero
        
        mass_total(:,:,:) = sedsolids(:,:,:,1,3) + sedsolids(:,:,:,2,3) + sedsolids(:,:,:,3,3)
        volume_pw(:,:,:) = bed(:,:,:).wp_wet*bed(:,:,:).thickness*bed(:,:,:).porosity
       
        sed_hg0(:,:,:,3) = sed_Hg0_ic(:,:,:)*volume_pw(:,:,:)
        sed_hgii(:,:,:,3) = sed_hgii_ic(:,:,:)*mass_total(:,:,:)
        sed_s1_hgii(:,:,:,3) = sed_s1_hgii_ic(:,:,:)*sedsolids(:,:,:,1,3)
        sed_s2_hgii(:,:,:,3) = sed_s2_hgii_ic(:,:,:)*sedsolids(:,:,:,2,3)
        sed_s3_hgii(:,:,:,3) = sed_s3_hgii_ic(:,:,:)*sedsolids(:,:,:,3,3)
        sed_mehg(:,:,:,3) = sed_mehg_ic(:,:,:)*mass_total(:,:,:)
        hg_conc_sed(:,:,:,3)%HgII_inert(1) = sed_s1_hgii_ic(:,:,:)
        hg_conc_sed(:,:,:,3)%HgII_inert(2) = sed_s2_hgii_ic(:,:,:)
        hg_conc_sed(:,:,:,3)%HgII_inert(3) = sed_s3_hgii_ic(:,:,:)
        
        do icell =1, n_cells
            do izone = 1, n_zones
                do ilayer = 1, n_layers
                    
                    total%XOH = sedsolids(icell,izone,ilayer,1,3)*solid_parms_sed(icell,izone,ilayer,1).mole_XOH * solid_parms_sed(icell,izone,ilayer,1).frac_exchg + &
                                sedsolids(icell,izone,ilayer,2,3)*solid_parms_sed(icell,izone,ilayer,2).mole_XOH * solid_parms_sed(icell,izone,ilayer,2).frac_exchg + &
                                sedsolids(icell,izone,ilayer,3,3)*solid_parms_sed(icell,izone,ilayer,3).mole_XOH * solid_parms_sed(icell,izone,ilayer,3).frac_exchg
                    if(volume_pw(icell,izone,ilayer).gt.zero) then
                        hh = 10.d0**(-conc_pH_pw(icell))
                        total%XOH = total%XOH /volume_pw(icell,izone,ilayer)                        
                        total%hgii = sed_hgii(icell,izone,ilayer,3)/ (1.0e9*mole_hg)/ volume_pw(icell,izone,ilayer) 
                        total%mehg = sed_mehg(icell,izone,ilayer,3)/ (1.0e9*mole_hg)/ volume_pw(icell,izone,ilayer) 
                        total%rs = conc_doc_pw(icell)*mole_rs_sed(icell,izone,ilayer)
                        
                        eq_vals_sed(icell,izone,ilayer)%hgii =  (total%hgii*hh)/(k_eq_solids_sed(icell,izone,ilayer)%xohg*total%XOH)
                        eq_vals_sed(icell,izone,ilayer)%mehg = (total%mehg*hh)/(k_eq_solids_sed(icell,izone,ilayer)%xomehg*total%XOH)                                                                    !ks%XOMeHg*m%XOH*m%MeHg/m%H
                        eq_vals_sed(icell,izone,ilayer)%rs = total%rs / (one + k_eq%hgrs*eq_vals_sed(icell,izone,ilayer)%hgii + k_eq%mehgrs*eq_vals_sed(icell,izone,ilayer)%mehg)
                        eq_vals_sed(icell,izone,ilayer)%xoh = total%XOH-eq_vals_sed(icell,izone,ilayer)%hgii-eq_vals_sed(icell,izone,ilayer)%mehg
                        eq_vals_sed(icell,izone,ilayer)%initialized = .true.
                        call equil_solver(eq_vals_sed(icell,izone,ilayer),  &       !> initial guess for unknowns(in) - results (out)
                            conc_cl_pw(icell),                      &       !> Cl (mg/L)
                            conc_pH_pw(icell),                      & 
                            zero,                                   &       !> phytoplankton (mg/L)
                            bed(icell,izone,ilayer)%porosity,       &       !> bed(icell,izone,ilayer)%porosity
                            total, 4, 1,                            &       !> Molar concentration totals (known), number of unknowns,  itype = 0 known total HgII and MeHg (ng/L),itype = 1 known total sediment HgII and MeHg (ng/g)
                            k_eq_solids_sed(icell,izone,ilayer),    &       !> equilibrium constants for solids partitioning (compartment specific)
                            iter,                                   &       !> number of iterations to reach solution
                            converge,                               &       !> true if solution converged
                            m,                                      &
                            icell,                                  &
                            izone)
                        call Hg_reactant_concs(m, nosolids, sedsolids(icell,izone,ilayer,:,3)/volume_pw(icell,izone,ilayer), solid_parms_sed(icell,izone,ilayer,:), hg_conc_sed(icell,izone,ilayer,3))
                        dissolvedHg =  hg_conc_sed(icell,izone,ilayer,3)%hgii_diss* volume_pw(icell,izone,ilayer)   !units ug  
                        sed_hgii(icell,izone,ilayer,3) =  sed_hgii(icell,izone,ilayer,3) +  dissolvedHg
                        dissolvedHg =  hg_conc_sed(icell,izone,ilayer,3)%mehg_diss* volume_pw(icell,izone,ilayer)   !units ug  
                        sed_mehg(icell,izone,ilayer,3) =  sed_mehg(icell,izone,ilayer,3) +  dissolvedHg
                    else
                        call set_complexes_to_zero(m)
                        m%XOhg = sed_hgii_ic(icell,izone,ilayer)*mass_total(icell,izone,ilayer)
                        m%XOmehg = sed_mehg_ic(icell,izone,ilayer)*mass_total(icell,izone,ilayer)
                        call Hg_reactant_concs(m, nosolids, sedsolids(icell,izone,ilayer,:,3), solid_parms_sed(icell,izone,ilayer,:), hg_conc_sed(icell,izone,ilayer,3))
                        hg_conc_sed(icell,izone,ilayer,3)%Hgii_ssX(1) = hg_conc_sed(icell,izone,ilayer,3)%HgII_ssX(1)/(mole_hg*1.0d9)
                        hg_conc_sed(icell,izone,ilayer,3)%Hgii_ssX(2) = hg_conc_sed(icell,izone,ilayer,3)%HgII_ssX(2)/(mole_hg*1.0d9)
                        hg_conc_sed(icell,izone,ilayer,3)%Hgii_ssX(3) = hg_conc_sed(icell,izone,ilayer,3)%HgII_ssX(3)/(mole_hg*1.0d9)
                        hg_conc_sed(icell,izone,ilayer,3)%meHg_ss(1) = hg_conc_sed(icell,izone,ilayer,3)%meHg_ss(1)/(mole_hg*1.0d9)
                        hg_conc_sed(icell,izone,ilayer,3)%meHg_ss(2) = hg_conc_sed(icell,izone,ilayer,3)%mehg_ss(2)/(mole_hg*1.0d9)
                        hg_conc_sed(icell,izone,ilayer,3)%meHg_ss(3) = hg_conc_sed(icell,izone,ilayer,3)%meHg_ss(3)/(mole_hg*1.0d9)
                        hg_conc_sed(icell,izone,ilayer,3)%meHg_diss = zero
                        hg_conc_sed(icell,izone,ilayer,3)%hgii_diss = zero
                    endif
                    !check = (hg_conc_sed(icell,izone,ilayer,3)%HgII_ssX(1)*sedsolids(icell,izone,ilayer,1,3)+hg_conc_sed(icell,izone,ilayer,3)%HgII_ssX(2)*sedsolids(icell,izone,ilayer,2,3)+hg_conc_sed(icell,izone,ilayer,3)%HgII_ssX(3)*sedsolids(icell,izone,ilayer,3,3))/mass_total(icell,izone,ilayer)
                    !check = (hg_conc_sed(icell,izone,ilayer,3)%mehg_ss(1)*sedsolids(icell,izone,ilayer,1,3)+hg_conc_sed(icell,izone,ilayer,3)%mehg_ss(2)*sedsolids(icell,izone,ilayer,2,3)+hg_conc_sed(icell,izone,ilayer,3)%mehg_ss(3)*sedsolids(icell,izone,ilayer,3,3))/mass_total(icell,izone,ilayer)
                end do
            end do
        end do
        deallocate(mass_total)
        deallocate(volume_pw)
        !for debugging
        return
        sed_hg0(:,:,:,2) = sed_hg0(:,:,:,3)
        sed_hgii(:,:,:,2) =  sed_hgii(:,:,:,3)
        sed_mehg(:,:,:,2) = sed_mehg(:,:,:,3)
        sed_s1_hgii(:,:,:,2) = sed_s1_hgii(:,:,:,3)
        sed_s2_hgii(:,:,:,2) = sed_s2_hgii(:,:,:,3)
    end subroutine init_sed_partitioning
    
end module