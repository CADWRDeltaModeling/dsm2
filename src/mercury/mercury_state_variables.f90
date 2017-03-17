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
   
    real(gtm_real), allocatable :: conc_sediment(:,:) !< sediment concentration 
    real(gtm_real), allocatable :: conc_ec(:)         !< EC
    real(gtm_real), allocatable :: conc_doc(:)        !< DOC
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
    real(gtm_real), allocatable :: val_rct_if(:)      !< rct_interface
    real(gtm_real), allocatable :: val_rct_water(:)   !< rct_water
    real(gtm_real), allocatable :: val_vol_frac(:)    !< vol_frac     
                                     
    contains

    !> allocate mercury variables
    subroutine allocate_mercury(nsediment,  &
                                ncell)
        implicit none
        integer, intent(in) :: nsediment         !< Number of sediment types
        integer, intent(in) :: ncell             !< Number of cells
        allocate(conc_sediment(nsediment,ncell))
        allocate(conc_ec(ncell))
        allocate(conc_doc(ncell))
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
        allocate(val_rct_if(ncell)) 
        allocate(val_rct_water(ncell))
        allocate(val_vol_frac(ncell))
        return
    end subroutine    
    
    !> Deallocate mercury variables
    subroutine deallocate_mercury()
        implicit none
        deallocate(conc_sediment, conc_ec, conc_doc, conc_do, conc_ph, conc_so4, conc_temp)
        deallocate(val_ipar, val_iuva, val_iuvb, val_rgm_air, val_hg0_air, val_mehg_air, val_precip)
        deallocate(val_wet_hgii, val_dry_hgii, val_wet_mehg, val_dry_mehg)
        deallocate(val_dgm_ratio, val_rct_if, val_rct_water, val_vol_frac)
        return 
    end subroutine        
        
    ! set mercury input variables
    subroutine set_mercury_inputs(read_conc_sediment,   & 
                                  read_conc_ec,         & 
                                  read_conc_doc,        & 
                                  input_timeseries,     &
                                  ncell,                &
                                  nsediment, &
                                  ntsvar)
        use common_variables
        implicit none
        integer, intent(in) :: ncell                                      !< Number of cells
        integer, intent(in) :: nsediment                                  !< Number of sediment types
        integer, intent(in) :: ntsvar                                     !< Number of input time series
        real(gtm_real), intent(in) :: read_conc_sediment(nsediment,ncell) !< sediment concentration 
        real(gtm_real), intent(in) :: read_conc_ec(ncell)                 !< EC
        real(gtm_real), intent(in) :: read_conc_doc(ncell)                !< DOC
        real(gtm_real), intent(in) :: input_timeseries(ntsvar,ncell)      !< Input time series
             
        conc_sediment = read_conc_sediment
        conc_ec = read_conc_ec
        conc_doc = read_conc_doc        
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
        val_rct_if = input_timeseries(code_to_ts_id(ts_var_rct_if),:)
        val_rct_water = input_timeseries(code_to_ts_id(ts_var_rct_water),:)
        val_vol_frac = input_timeseries(code_to_ts_id(ts_var_vol_frac),:)  
        return
    end subroutine
 
end module