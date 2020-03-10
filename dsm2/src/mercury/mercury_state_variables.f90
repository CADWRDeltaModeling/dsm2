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
!> 
!>@ingroup mercury

module mercury_state_variables

    use gtm_precision
    use sed_internal_vars
    use sed_type_defs
    use hg_internal_vars, only:setup_hg_internals
    use common_variables, only:n_mercury
    use hg_hdf   
    
    real(gtm_real), allocatable :: conc_do(:)         !< DO
    real(gtm_real), allocatable :: conc_ph(:)         !< pH
    real(gtm_real), allocatable :: conc_so4(:)        !< SO4
    real(gtm_real), allocatable :: conc_temp(:)       !< Temperature
    real(gtm_real), allocatable :: val_ipar(:)        !< ipar
    real(gtm_real), allocatable :: val_iuva(:)        !< iuva
    real(gtm_real), allocatable :: val_iuvb(:)        !< iuvb
    real(gtm_real), allocatable :: val_rgm_air(:)     !< rgm_atm
    real(gtm_real), allocatable :: val_hg0_air(:)     !< Hg0_atm
    real(gtm_real), allocatable :: val_mehg_air(:)    !< MeHg_atm
    real(gtm_real), allocatable :: val_precip(:)      !< precip 
    real(gtm_real), allocatable :: val_wet_hgii(:)    !< wetdep_HgII
    real(gtm_real), allocatable :: val_dry_hgii(:)    !< drydep_HgII
    real(gtm_real), allocatable :: val_wet_mehg(:)    !< wetdep_MeHg
    real(gtm_real), allocatable :: val_dry_mehg(:)    !< drydep_MeHg
    real(gtm_real), allocatable :: val_dgm_ratio(:)   !< dgm ratio  
    ! sediment bed pore water variables
    !real(gtm_real), allocatable :: conc_cl_pw(:)      !< EC
    !real(gtm_real), allocatable :: conc_doc_pw(:)     !< DOC
    !real(gtm_real), allocatable :: conc_ph_pw(:)      !< pH
    !real(gtm_real), allocatable :: conc_so4_pw(:)     !< SO4
    
    ! variables for mercury module
    integer :: ec_ivar, doc_ivar
    
    
    contains

    !> allocate mercury variables
    subroutine allocate_mercury(nsediment,  &
                                ncell,      &
                                nresv)
        implicit none
        integer, intent(in) :: nsediment         !< Number of sediment types
        integer, intent(in) :: ncell             !< Number of cells
        integer, intent(in) :: nresv
        allocate(conc_do(ncell))
        allocate(conc_ph(ncell))
        allocate(conc_so4(ncell))
        allocate(conc_temp(ncell))
        allocate(val_ipar(ncell))
        allocate(val_iuva(ncell))
        allocate(val_iuvb(ncell))
        allocate(val_rgm_air(ncell))
        allocate(val_hg0_air(ncell))
        allocate(val_mehg_air(ncell))
        allocate(val_precip(ncell))
        allocate(val_wet_hgii(ncell))
        allocate(val_dry_hgii(ncell))
        allocate(val_wet_mehg(ncell))
        allocate(val_dry_mehg(ncell))
        allocate(val_dgm_ratio(ncell))      
        !allocate(conc_cl_pw(ncell))
        !allocate(conc_doc_pw(ncell))
        !allocate(conc_ph_pw(ncell))
        !allocate(conc_so4_pw(ncell))
               
        call setup_hg_internals(ncell, nresv, n_zones,2,3,n_mercury)
        return
    end subroutine    
    
    !> Deallocate mercury variables
    subroutine deallocate_mercury()
        implicit none
        deallocate(val_ipar, val_iuva, val_iuvb, val_rgm_air, val_hg0_air, val_mehg_air, val_precip)
        deallocate(val_wet_hgii, val_dry_hgii, val_wet_mehg, val_dry_mehg)
        deallocate(val_dgm_ratio)
        !deallocate(conc_cl_pw, conc_doc_pw, conc_ph_pw, conc_so4_pw)
        
        call deallocate_hg_internals()
        call close_gtm_hg_hdf()
        return 
    end subroutine        
        
    ! set mercury input variables
    subroutine set_mercury_inputs(input_timeseries,     &
                                  ncell,                &
                                  ntsvar)
        use common_variables
        implicit none
        integer, intent(in) :: ncell                                      !< Number of cells
        integer, intent(in) :: ntsvar                                     !< Number of input time series
        real(gtm_real), intent(in) :: input_timeseries(ntsvar,ncell)      !< Input time series
               
        conc_do = input_timeseries(code_to_ts_id(ts_var_do),:)
        conc_ph = input_timeseries(code_to_ts_id(ts_var_ph),:)
        conc_so4 = input_timeseries(code_to_ts_id(ts_var_so4),:)
        conc_temp = input_timeseries(code_to_ts_id(ts_var_temp),:)
        val_ipar = input_timeseries(code_to_ts_id(ts_var_ipar),:)
        val_iuva = input_timeseries(code_to_ts_id(ts_var_iuva),:)
        val_iuvb = input_timeseries(code_to_ts_id(ts_var_iuvb),:)
        val_rgm_air = input_timeseries(code_to_ts_id(ts_var_rgm_air),:)
        val_hg0_air = input_timeseries(code_to_ts_id(ts_var_hg0_air),:)
        val_mehg_air = input_timeseries(code_to_ts_id(ts_var_mehg_air),:)
        val_precip = input_timeseries(code_to_ts_id(ts_var_precip),:)
        val_wet_hgii = input_timeseries(code_to_ts_id(ts_var_wet_hgii),:)
        val_dry_hgii = input_timeseries(code_to_ts_id(ts_var_dry_hgii),:)
        val_wet_mehg = input_timeseries(code_to_ts_id(ts_var_wet_mehg),:)
        val_dry_mehg = input_timeseries(code_to_ts_id(ts_var_dry_mehg),:)
        val_dgm_ratio = input_timeseries(code_to_ts_id(ts_var_dgm_ratio),:)         
        !conc_cl_pw = input_timeseries(code_to_ts_id(ts_var_cl_pw),:)
        !conc_doc_pw = input_timeseries(code_to_ts_id(ts_var_doc_pw),:)
        !conc_ph_pw = input_timeseries(code_to_ts_id(ts_var_ph_pw),:)
        !conc_so4_pw = input_timeseries(code_to_ts_id(ts_var_so4_pw),:)
        return
    end subroutine
 
end module