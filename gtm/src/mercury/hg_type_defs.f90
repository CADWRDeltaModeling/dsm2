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
module hg_type_defs
    
use gtm_precision

implicit none

!> unit conversions
!****************************************************************************************************************************************

integer     :: nosolids = 3                                 !> number of solids types defined in GTM

real(gtm_real), parameter  :: mole_Hg           = 200.59d0  !< g Hg/mole
real(gtm_real), parameter  :: mole_Cl           = 35.45d0   !< g Cl/mole
real(gtm_real), parameter  :: mole_Sulph        = 32.06d0   !< g Sulfide/mole
real(gtm_real), parameter  :: molar_Hg_to_ng_l  = 200.59d09       
real(gtm_real), parameter  :: molar_Cl_to_mg_l  = 35.45d03
real(gtm_real), parameter  :: molar_S_to_mg_l   = 32.06d03
real(gtm_real), parameter  :: mg_l_to_g_l       = 1.d-03
!real(gtm_real), parameter  :: pi                = 3.141592653589793239 

                                        !> reaction switches
integer     :: methyl_switch        = 5
integer     :: reduction_switch     = 1
integer     :: biodemethyl_switch   = 4
                                        !> partitioning switches
integer     :: phyto_uptakeMeHg     = 2	                    !< free ion = 1, iorganic = 2
integer     :: phyto_uptakeHgII     = 1	                    !< free ion = 1, iorganic = 2


!> time series types
!****************************************************************************************************************************************
type ts_ptr                                                 !> define pointer to real - used to create array of pointers
    real(gtm_real) , pointer ::ptr
end type

type ts_data_series_real                                    !> alternate time series array (todo: delete later)
    real(gtm_real), dimension (:), allocatable :: time
    real(gtm_real), dimension (:), allocatable :: value
end type

type ts_data_series                                          !<alternate time series array
    integer (4), dimension (:), allocatable :: time
    real(gtm_real), dimension (:), allocatable :: value
end type
!> Chloride input units mg/L
!todo: convert to molar at some point - in interpolation routine or in equilibrium module
!to do change Clx to Cl - needs to be addressed in equilibrium module
integer :: cl_count;                                        !< number of Chloride time series
type (ts_data_series), dimension(:), allocatable :: ts_Cl   !< array of input time series for Chloride, dimension: cl_count

real (gtm_real), dimension(:), allocatable, target :: Cl_   !< interpolated Cl concs, dimension: cl_count 
type (ts_ptr) , allocatable:: Cl(:)                         !< pointers to interpolated Cl concs for each cell, dimension: ncells 


!> equilibrium types
!****************************************************************************************************************************************
!global - no need to input




type k_phyto_parms
    real (gtm_real) :: p_phytoMeHg  = 0.0d0	        !< permeability coefficient for MeHg (dm d-1)
    real (gtm_real) :: p_phytoHgII  = 0.0d0	        !< permeability coefficient for MeHg  (dm d-1)
    real (gtm_real) :: k_phytoMeHg  = -7.5d0	    !< rate constant for facilitated uptake of MeHg by phyto (L cell-1 d-1)
    real (gtm_real) :: k_phytoHgII  = 200.0d0	    !< rate constant for facilitated uptake of HgII by phyto (L cell-1 d-1)
    real (gtm_real) :: H_phytoMeHg  = 4.36d0
    real (gtm_real) :: H_phytoHgII  = 10.0d0
    real (gtm_real) :: u_phyto      = 0.7d0		    !< specific growth rate for for phyto (d-1)
    real (gtm_real) :: kd_phytoMeHg = 0.01d0	    !< depuration rate constant for for phyto (d-1)
    real (gtm_real) :: kd_phytoHgII = 0.01d0	    !< depuration rate constant for for phyto (d-1)
    real (gtm_real) :: d_cell       = 0.2d0		    !< dry weight of average cell (kg cell-1)
    real (gtm_real) :: r_cell       = 0.000025d0	!< radius of average cell (dm)
    real (gtm_real) :: diff_Hg      = 0.0029d0	    !< diffusion coefficient for Hg (dm2 d-1)  
    
    !>  computed values
    real (gtm_real) :: area_cell
    real (gtm_real) :: mass_cell
    real (gtm_real) :: kpassMeHg                     !> passive diffusion coefficient for MeHg
    !real (gtm_real) :: passHgII                     !> passive diffusion coefficient for HgII
    real (gtm_real) :: diff
    
end type k_phyto_parms



!> global hg reaction rate constants & parameters
!****************************************************************************************************************************************
    real (gtm_real) :: k_adsorp             !> rate constant for rate limited adsorption of HgII onto solids
    real (gtm_real) :: k_desorp             !> rate constant for rate limited desorption of HgII from solids
    real (gtm_real) :: k_PhotoDeg_UVA       !> MeHg photodegradation rate constant at waterbody surface per unit of UVA intensity (ng/m2/day)
    real (gtm_real) :: k_PhotoDeg_UVB       !> MeHg photodegradation rate constant at waterbody surface per unit of UVB) intensity (ng/m2/day)
    real (gtm_real) :: k_PhotoDeg_PAR       !> MeHg photodegradation rate constant at waterbody surface per unit of PAR intensity (ng/m2/day)
    real (gtm_real) :: k_PhotoRed_UVA       !> Inorganic Hg(II) photoreduction  rate constant at waterbody surface per unit of UVA intensity (ng/m2/day)
    real (gtm_real) :: k_PhotoRed_UVB       !> Inorganic Hg(II) photoreduction  rate constant at waterbody surface per unit of UVB intensity (ng/m2/day)
    real (gtm_real) :: k_PhotoRed_PAR       !> Inorganic Hg(II) photoreduction  rate constant at waterbody surface per unit of PAR intensity (ng/m2/day
    real (gtm_real) :: k_PhotoOxid_UVA      !> Elemental Hg photooxidation rate constant  at waterbody surface per unit of UVA intensity (ng/m2/day)
    real (gtm_real) :: k_PhotoOxid_UVB      !> Elemental Hg photooxidation rate constant  at waterbody surface per unit of UVB intensity (ng/m2/day)
    real (gtm_real) :: k_PhotoOxid_PAR      !> Elemental Hg photooxidation rate constant  at waterbody surface per unit of PAR intensity (ng/m2/day)
    
    real (gtm_real) :: k_oxid_1 = 0.0000219686d0 !> for oxidation switch = 2; dgm ratio = k1*DOC^k2*pH^k3 not implemented
    real (gtm_real) :: k_oxid_2 = -0.431372d0
    real (gtm_real) :: k_oxid_3 = 4.1308153d0
    
    real (gtm_real) :: ke_UVA_1             !> Coefficient used to estimate UVA light extinction in the following expression: Ke = keUVA1*DOC + keUVA2*SS + ke_UVA3)  
    real (gtm_real) :: ke_UVA_2             !> Coefficient used to estimate UVA light extinction
    real (gtm_real) :: ke_UVA_3             !> Coefficient used to estimate UVA light extinction
    real (gtm_real) :: ke_UVB_1             !> Coefficient used to estimate UVB light extinction in the following expression: Ke = keUVB1*DOC + keUVB2*SS + ke_UVB3)
    real (gtm_real) :: ke_UVB_2             !> Coefficient used to estimate UVB light extinction
    real (gtm_real) :: ke_UVB_3             !> Coefficient used to estimate UVB light extinction
    real (gtm_real) :: ke_PAR_1             !> Coefficient used to estimate PAR light extinction in the following expression: Ke = kePAR1*DOC + kePAR2*SS + ke_PAR3)
    real (gtm_real) :: ke_PAR_2             !> Coefficient used to estimate PAR light extinction
    real (gtm_real) :: ke_PAR_3             !> Coefficient used to estimate PAR light extinction
    real (gtm_real) :: Henry_Hg0            !> Henry's Law constant for elemental Hg
    real (gtm_real) :: MTC_Hg0Wat           !> Mass transfer coefficient (water side) for calculation of elemental Hg diffusion between water and air (m/d)
    real (gtm_real) :: MTC_Hg0Air           !> Mass transfer coefficient (air side) for calculation of elemental Hg diffusion between water and air (m/d)
    real (gtm_real) :: Henry_MeHg           !> Henry's Law constant for MeHg
    real (gtm_real) :: MTC_MeHgWat          !> Mass transfer coefficient (water side) for MeHg diffusion across air/water interface (m/d)
    real (gtm_real) :: MTC_MeHgAir          !> Mass transfer coefficient (air side) for MeHg diffusion across air/water interface (m/d)
    real (gtm_real) :: v_rgm                !> Settling velocity of RGM (m/d)
    real (gtm_real) :: Q10meth              !> Q10 temperature dependence parameter for methylation
    real (gtm_real) :: Tbmeth               !> base temperature for dependence for methylation
    real (gtm_real) :: Q10biodemeth         !> Q10 temperature dependence parameter for methylation
    real (gtm_real) :: Tbbiodemeth          !> base temperature for dependence for bio-demethylation
    real (gtm_real) :: kSO4                 !> Half saturation constant for SO4 effect on methylation (mg/L)
    real (gtm_real) :: uSO4                 !> maximum effect that sulfate can have on the microbial methylation rate (dimensionless)
    
!> compartment specific hg reaction rate constants, parameters
!****************************************************************************************************************************************
type hg_rate_parms1
    real (gtm_real) :: methyl               !> methylation rate constant
    real (gtm_real) :: biodemethyl          !> methylation rate constant 
    real (gtm_real) :: methyl_int           !> methylation rate constant
    real (gtm_real) :: biodemethyl_int      !> methylation rate constant  
end type hg_rate_parms1

type :: eq_complexes
    !> computed complexes/ions Molar concentrations
    real (gtm_real) ::  HRS             !< RS molar concentration of DOC thiol groups
    real (gtm_real) ::  H2RS
    real (gtm_real) ::  H2S
    real (gtm_real) ::  sulph
    real (gtm_real) ::  HgCl
    real (gtm_real) ::  HgCl2
    real (gtm_real) ::  HgCl3
    real (gtm_real) ::  HgCl4
    real (gtm_real) ::  HgOH
    real (gtm_real) ::  Hg_OH_2
    real (gtm_real) ::  HgOHCl
    real (gtm_real) ::  HgRS
    real (gtm_real) ::  Hg_RS_2
    real (gtm_real) ::  HgHS2
    real (gtm_real) ::  Hg_HS_2
    real (gtm_real) ::  HgHS
    real (gtm_real) ::  HgS2
    real (gtm_real) ::  HgS
    real (gtm_real) ::  HgOHHS
    real (gtm_real) ::  MeHgCl
    real (gtm_real) ::  MeHgOH
    real (gtm_real) ::  MeHgS
    real (gtm_real) ::  MeHg2S
    real (gtm_real) ::  MeHgRS
    real (gtm_real) ::  XOMeHg
    real (gtm_real) ::  XOHg
    real (gtm_real) ::  ROHg
    
    real (gtm_real) ::  xRS_Hg
    real (gtm_real) ::  xRS_2_Hg
    real (gtm_real) ::  xRS_MeHg
    real (gtm_real) ::  xR_SH
    
    real (gtm_real) ::  MeHg_phyto
    real (gtm_real) ::  HgII_phyto
    
    !>possible unknowns
    real (gtm_real) ::  HgII
    real (gtm_real) ::  MeHg
    real (gtm_real) ::  XOH
    real (gtm_real) ::  xROH
    real (gtm_real) ::  RS
    real (gtm_real) ::  HS
    !>site conditions (inputs)
    real (gtm_real) :: H
    real (gtm_real) :: OH
    real (gtm_real) :: Cl
end type eq_complexes

type eq_vals                                            !< molar concs. to be solved
    real (gtm_real) :: HgII
    real (gtm_real) :: MeHg
    real (gtm_real) :: RS
    real (gtm_real) :: XOH
    real (gtm_real) :: HS
    real (gtm_real) :: xROH
end type eq_vals

type molar_total                                        !> inputs to eq. solver
    real (gtm_real) :: HgII
    real (gtm_real) :: MeHg
    real (gtm_real) :: RS
    real (gtm_real) :: XOH   
    real (gtm_real) :: sulph
    real (gtm_real) :: xROH  
end type molar_total

type :: hg_concs                                        !> hg concentrations to be used later units ng/L or ng/g
    real (gtm_real) :: HgII_diss
    real (gtm_real) :: HgII_organic
    real (gtm_real) :: HgII_inorganic
    real (gtm_real) :: HgII_photo
    real (gtm_real) :: HgII_methyl
    real (gtm_real), dimension(:), allocatable :: HgII_ssX      !> exchangeable HgII on solids (ug/g)
    real (gtm_real), dimension(:), allocatable :: HgII_ssR      !> exchangeable HgII on solids (ug/g)
       
    real (gtm_real) :: MeHg_diss
    real (gtm_real) :: MeHg_organic
    real (gtm_real) :: MeHg_inorganic
    real (gtm_real) :: MeHg_photo
    real (gtm_real) :: MeHg_biodemeth
    real (gtm_real) :: MeHg_Cl
    real (gtm_real), dimension(:), allocatable :: MeHg_ss       !> exchangeable MeHg on solids (ug/g)
    
    real (gtm_real) :: Hg0
end type hg_concs
                   
       
integer :: nfluxvals = 15

!real (gtm_real), dimension(:,: ,:), allocatable  :: hgii_flux_data

type hg_flux_def
    
    real (gtm_real) :: methyl
    real (gtm_real) :: methyl_int
    real (gtm_real) :: biodemethyl
    real (gtm_real) :: biodemethyl_int
    real (gtm_real) :: photodemethyl
    real (gtm_real) :: drydep_MeHg
    real (gtm_real) :: wetdep_MeHg
    real (gtm_real) :: volatil_MeHg
    real (gtm_real) :: photoreduction
    real (gtm_real) :: drydep_HgII
    real (gtm_real) :: wetdep_HgII
    real (gtm_real) :: rgmdep_HgII
    real (gtm_real) :: oxidation
    real (gtm_real) :: evasion_Hg0
    real (gtm_real), dimension(:), allocatable :: adsorption
    real (gtm_real) :: MeHg                 !> net fluxes for cell
    real (gtm_real) :: HgII
    real (gtm_real) :: Hg_inert
    real (gtm_real) :: Hg0
    
end type hg_flux_def

end module