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
!> partitioning onto solids based on Benoit (1999)
!>      [X0H] + [Hg++]  <=> [XO-Hg] + [H+]
!>      [xR0H] + [S--] + [Hg++]  <=> [xRS-Hg] + [OH-], etc...
!>@ingroup mercury

module equilibrium

use gtm_precision
use math_utils
use hg_type_defs

implicit none

type phytoplankton_jacob                            !> temporary storage for phytoplankton contributions to jacobian matrix
    real (gtm_real) :: dMeHgphyto_dMeHg
    real (gtm_real) :: dMeHgphyto_dHS   
    real (gtm_real) :: dHgIIphyto_dHgII
    real (gtm_real) :: dHgIIphyto_dHS
end type phytoplankton_jacob

type (k_eq_parms)       :: k                        !> global partitioning parameters
type (k_phyto_parms)    :: k_phyto                  !> global partitioning parameters

real (gtm_real) ::  eps_equil = 1.0d-6              !< convergence criteria 
integer ::  iter_max = 100                          !< maximum allowable iterations

public :: equil_solver

contains

!> solve system of equations for equilibrium and partitioning in units of Moles/Liter
!>      definitions of complexes are given in the subroutine mass_action
!>      note that porosity needs to be provided for sediment compartments
!>      jacob_order should be 4 if there is no sulfide in solution
subroutine equil_solver(vals,                   &       !> initial guess for unknowns(in) - results (out)
                        Cl,                     &       !> Cl (mg/L)
                        pH,                     & 
                        phyto,                  &       !> phytoplankton (mg/L)
                        porosity,               &       !> porosity
                        total,                  &       !> Molar concentration totals (known)
                        jacob_order,            &       !> number of unknowns
                        itype,                  &       !> itype = 0 known total HgII and MeHg (ng/L)
                                                        !> itype = 1 known total sediment HgII and MeHg (ng/g)
                        ks,                     &       !> equilibrium constants for solids partitioning (compartment specific)
                        iter,                   &       !> number of iterations to reach solution
                        converge,               &       !> true if solution converged
                        m)
    !arguments  
    type (eq_vals), intent (inout)   :: vals
    real (gtm_real), intent (in)     :: Cl
    real (gtm_real), intent (in)     :: pH
    real (gtm_real), intent (in)     :: phyto
    real (gtm_real), intent (in)     :: porosity
    integer, intent (in)             :: jacob_order
    integer, intent (in)             :: itype
    type (k_eq_solids), intent (in)  :: ks
    integer, intent (out)            :: iter
    logical, intent (inout)          :: converge
    type (molar_total), intent (inout)  :: total
    type (eq_complexes) , intent (out) :: m             !> molar concentrations of individual complexes
    !local
    type (molar_total)  :: sum                          !> working molar concentrations - updated each iteration
    real (gtm_real) :: jacob(jacob_order,jacob_order)        
    real (gtm_real) :: delta_x(jacob_order)             !> solution vector gives -delta_x/x 
    real (gtm_real) :: error(jacob_order)               !> errors - right hand side of system of equations (sum_x - tot_x)
     type (phytoplankton_jacob) :: phyto_jacob          !> internal variable to hold phytoplankton contributions to jacobian
    
    logical :: test_HgII                                !> result of tests for convergence of unknows: abs((tot_x-sum_x)/(tot_x+sum_xI))<EPS
    logical :: test_MeHg
    logical :: test_RS
    logical :: test_XOH
    logical :: test_Sulph
    logical :: test_xROH    
    logical :: solved = .false.
    call set_complexes_to_zero(m)
    
    m%Cl = Cl/molar_Cl_to_mg_l
    m%H = 10d0**(-pH)
    m%OH = 10d0**(-(14d0-pH))
    m%HgII = vals%HgII
    m%MeHg = vals%MeHg
    m%RS   = vals%RS
    m%XOH  = vals%XOH
    m%HS   = vals%HS
    m%xROH = vals%xROH
    converge = .false.
    jacob = zero
    error = zero
    delta_x = zero
    iter = 0
    do while ((iter < iter_max).and.(.not. converge))
        iter = iter + 1
        call mass_action(ks, porosity, phyto, phyto_jacob, jacob_order, m, sum, itype)
        test_HgII = abs((total%HgII-sum%HgII)/(total%HgII+sum%HgII))<eps_equil
        test_MeHg = abs((total%MeHg-sum%MeHg)/(total%MeHg+sum%MeHg))<eps_equil
	    test_RS = abs((total%RS-sum%RS)/(total%RS+sum%RS))<eps_equil
	    test_XOH = abs((total%XOH-sum%XOH)/(total%XOH+sum%XOH))<eps_equil
        converge = test_HgII.and.test_MeHg.and.test_RS.and. test_XOH
        
        if (jacob_order > 4) then
            test_Sulph = (abs((total%Sulph-sum%Sulph)/(total%Sulph+sum%Sulph))<eps_equil)
			converge = converge.and.test_Sulph
			if (jacob_order > 5) then
                test_xROH = (abs((total%xROH-sum%xROH)/(total%xROH+sum%xROH))<eps_equil)
				converge = converge.and.test_XROH
			endif
        endif
        
        if (.not.converge) then
            error(1) = sum%HgII - total%HgII
            error(2) = sum%MeHg - total%MeHg
			error(3) = sum%RS - total%RS
			error(4) = sum%XOH - total%XOH
			if (jacob_order > 4) then
				error(5) = sum%Sulph - total%Sulph
				if (jacob_order > 5) then
					error(6) = sum%XROH - total%xROH
				endif
            endif
            
            call jacobian(jacob_order, phyto_jacob, jacob,m, sum, porosity,itype)
            call matrix_invert(jacob, jacob_order, solved)
            
            if (.not. solved) then          !> todo: error trapping if inversion fails
                print *, "matrix inversion error"
                return
            endif
			call matrix_x_vector(jacob_order, jacob_order, jacob, error, delta_x)
			if (delta_x(1)<1.0) then
                m%HgII = m%HgII * (1.0 - delta_x(1))
            else 
                m%HgII = m%HgII/10.0
            endif
			if (delta_x(2)<1.0) then 
                m%MeHg = m%MeHg * (1.0 - delta_x(2))
            else 
                m%MeHg = m%MeHg/10.0
            endif
			if (delta_x(3)<1.0) then
                m%RS = m%RS * (1.0 - delta_x(3))
            else 
                m%RS = m%RS/10.0
            endif
			if (delta_x(4)<1.0) then 
                m%XOH = m%XOH * (1.0 - delta_x(4))
            else 
                m%XOH = m%XOH/10.0
            endif
			if (total%Sulph > 0.0)then
				if (delta_x(5)<1.0) then
                    m%HS = m%HS * (1.0 - delta_x(5))
                else 
                    m%HS = m%HS/10.0
                endif
				if (total%xROH > 0.0) then
					if (delta_x(6)<1.0) then 
                        m%xROH = m%xROH * (1.0 - delta_x(6))
                    else 
                        m%xROH = m%xROH/10.0
                    endif
                endif
            endif	
        end if
    
    end do
    if (converge) then                  !> save solution to use as next initial guess
        vals%HgII = m%HgII
        vals%MeHg = m%MeHg
        vals%RS   = m%RS
        vals%XOH  = m%XOH
        vals%HS   = m%HS
        vals%xROH = m%xROH
    end if
    if (itype==1) then                  !> return unfiltered total solids+other species
        total%HgII = total%HgII +                                       &  
                    (m%HgII + m%HgCl + m%HgCl2 + m%HgCl3 + m%HgCl4 +    &
                    m%HgOHCl + m%HgOH + m%Hg_OH_2 +                     &
		            m%HgRS + m%Hg_RS_2 + m%HgHS2 + m%Hg_HS_2 +          &
                    m%HgHS + m%HgS2 + m%HgS + m%HgOHHS) * porosity 
        total%MeHg = total%MeHg +                                       &
                    (m%MeHg + m%MeHgCl + m%MeHgOH + m%MeHgRS +          &
                    m%MeHgS + two* m%MeHg2S) * porosity
    endif
end subroutine equil_solver
 

!> compute and sum all concentrations
subroutine mass_action(ks, porosity, phyto, phyto_jacob, jacob_order, m, sum, itype)
    !args 
    integer, intent (in) :: jacob_order
    type (k_eq_solids), intent (in)     :: ks               !> partitioning parameters for solids
    real (gtm_real), intent (in)        :: porosity
    real (gtm_real), intent (in)        :: phyto            !> phtyoplankton concentration (mg/L)
    type (phytoplankton_jacob), intent (out) :: phyto_jacob !> internal variable to hold phytoplankton contributions to jacobian
    type (eq_complexes), intent (inout) :: m                !> species concentrations
    type (molar_total), intent (inout)  :: sum              !> sum of species concentrations
    integer, intent (in) :: itype
    
    phyto_jacob%dMeHgphyto_dMeHg = zero
    phyto_jacob%dMeHgphyto_dHS   = zero
    phyto_jacob%dHgIIphyto_dHgII = zero
    phyto_jacob%dHgIIphyto_dHS   = zero
    
    m%HRS = k%HRS * m%H * m%RS
    m%H2RS = k%H2RS*m%RS*m%H**two
    
    m%HgCl = k%HgCl * m%HgII * m%Cl
    m%HgCl2 = k%HgCl2 * m%HgII * m%Cl**two
    m%HgCl3 = k%HgCl3 * m%HgII * m%Cl**three
    m%HgCl4 = k%HgCl4 * m%HgII * m%Cl**four
    m%HgOH = k%HgOH* m%HgII * m%OH
    m%Hg_OH_2 = k%Hg_OH_2 * m%HgII * m%OH**two
    m%HgOHCl = k%HgOHCl * m%HgII * m%OH * m%Cl
    m%HgRS = k%HgRS * m%HgII * m%RS
    m%Hg_RS_2 = k%Hg_RS_2 * m%HgII * m%RS**two
    m%XOHg = ks%XOHg*m%XOH*m%HgII/m%H			
    
    m%MeHgCl = k%MeHgCl * m%MeHg * m%Cl
    m%MeHgOH = k%MeHgOH  * m%MeHg * m%OH
    m%MeHgRS = k%MeHgRS * m%MeHg * m%RS
    m%XOMeHg = ks%XOMeHg*m%XOH*m%MeHg/m%H
    
    
    sum%HgII =  (m%HgII + m%HgCl + m%HgCl2 + m%HgCl3 + m%HgCl4 +    &
                m%HgOHCl + m%HgOH + m%Hg_OH_2 +                     &
		        m%HgRS + m%Hg_RS_2) * porosity
    sum%HgII =  sum%HgII + m%XOHg
		
    sum%MeHg = (m%MeHg + m%MeHgCl + m%MeHgOH + m%MeHgRS) * porosity
    sum%MeHg = sum%MeHg + m%XOMeHg
	
	sum%RS = (m%RS + m%HRS + m%H2RS + m%HgRS + two*m%Hg_RS_2 + m%MeHgRS) * porosity
	sum%XOH = m%XOH + m%XOHg + m%XOMeHg
    
    if (jacob_order > 4) then
        m%HgHS2 = k%HgHS2 *m%HgII*(m%HS**two)/m%H
        m%Hg_HS_2 = k%Hg_HS_2 * m%HgII * m%HS**two
        m%HgHS = k%HgHS * m%HgII * m%HS
        m%HgS2 = k%HgS2 * m%HgII * (m%HS**two)/ (m%H**two)
        m%HgS = k%HgS * m%HgII * m%HS/m%H
        m%HgOHHS = k%HgOHHS * m%HgII * m%OH * m%HS
    
        m%H2S = k%H2S*m%HS*m%H 
        m%Sulph = k%HS * m%HS/m%H
                
        m%MeHgS = k%MeHgS  * m%MeHg * m%HS/m%H
        m%MeHg2S = k%MeHg2S * (m%MeHg ** two) * m%HS/m%H
        
        sum%HgII =  sum%HgII + (m%HgHS2 + m%Hg_HS_2 + m%HgS2 + m%HgHS + m%HgS + m%HgOHHS) * porosity
        sum%MeHg = sum%MeHg + (m%MeHgS + two* m%MeHg2S) * porosity
        sum%Sulph = (two*m%HgHS2 + two*m%Hg_HS_2 + two*m%HgS2 + m%HgHS + m%HgS + m%HgOHHS +         &
                    m%MeHgS + m%MeHg2S + m%Sulph + m%H2S + m%HS)*porosity
        
        if (jacob_order > 5) then
            m%xRS_Hg = ks%xRS_Hg*m%xROH*m%HS*m%HgII;
            m%xRS_2_Hg = ks%xRS_2_Hg *(m%HS** two)*(m%xROH**2)*m%HgII
	        
            m%xRS_MeHg = ks%xRS_MeHg*m%xROH*m%HS*m%MeHg
            m%xR_SH = ks%xR_SH*m%xROH*m%HS
            !ROH_SH = k%ROH_SH*ROH*HS/OH
            
            sum%HgII =  sum%HgII + m%xRS_Hg + m%xRS_2_Hg
            sum%MeHg = sum%MeHg + m%xRS_MeHg
            sum%Sulph = sum%Sulph + m%xRS_Hg + two*m%xRS_2_Hg + m%xRS_MeHg + m%xR_SH
            
            sum%xROH = m%xROH + m%xRS_Hg + m%xRS_2_Hg + m%xRS_MeHg + m%xR_SH
        endif
    endif
    if (itype == 1) then                                !> modify totals for HgII and MeHg if solid HgII and MeHg are knowns (for sediment initialization)
		sum%HgII = m%XOHg;
		sum%MeHg = m%XOMeHg		
		if (jacob_order==6) then
			sum%HgII = sum%HgII + m%xRS_Hg + m%xRS_2_Hg
			sum%MeHg = sum%MeHg + m%xRS_MeHg
		endif
    end if
    if (phyto > 0) then
        call phyto_calcs(m, phyto, phyto_jacob)
        sum%MeHg = sum%MeHg + m%MeHg_phyto
        sum%HgII = sum%HgII + m%HgII_phyto
    endif
end subroutine mass_action
    
!> compute nxn jacobian  
subroutine jacobian(n, phyto_jacob, jacob, m, sum, porosity, itype)
    !args 
    integer, intent (in)                :: n
    type (phytoplankton_jacob), intent (in) :: phyto_jacob  !> internal variable to hold phytoplankton contributions to jacobian
    real (gtm_real), intent (out)       :: jacob(n,n)       !>nxn jacobian
    type (eq_complexes), intent (in)    :: m
    type (molar_total), intent (in)     :: sum
    real (gtm_real), intent (in)        :: porosity
    integer, intent (in) :: itype
    !local
    real (gtm_real) :: x                                    !<temporary variable compute mass balance for sulphides
    jacob = zero
    jacob(1,1) = sum%HgII  + phyto_jacob%dHgIIphyto_dHgII
    jacob(1,3) = (m%HgRS + m%Hg_RS_2) * porosity
    jacob(1,4) = m%XOHg
    
	jacob(2,2) = sum%MeHg + phyto_jacob%dMeHgphyto_dMeHg * m%MeHg
	jacob(2,3) = m%MeHgRS*porosity                      !<????MeHg + MeHgRS;  MeHg looks like extra term
	jacob(2,4) = m%XOMeHg
    
	jacob(3,1) = (m%HgRS + two*m%Hg_RS_2) *porosity
	jacob(3,2) = (m%MeHgRS)*porosity
	jacob(3,3) = sum%RS + (two*m%Hg_RS_2)*porosity
    
    jacob(4,1) = m%XOHg
	jacob(4,2) = m%XOMeHg
	jacob(4,4) = m%XOH + m%XOHg + m%XOMeHg
    
    if (n > 4) then
		Jacob(1,1) =  Jacob(1,1) + (m%HgHS2 + m%Hg_HS_2 + m%HgS2 + m%HgHS + m%HgS + m%HgOHHS)*porosity
		Jacob(1,5) = Jacob(1,5) + (two*m%HgHS2 + two*m%Hg_HS_2 + two*m%HgS2 + m%HgHS + m%HgS + m%HgOHHS)*porosity &
                        + phyto_jacob%dHgIIphyto_dHS
        
		Jacob(2,2) = Jacob(2,2) + (m%MeHgS + four*m%MeHg2S)*porosity
		Jacob(2,5) = Jacob(2,5) + (m%MeHgS + two*m%MeHg2S)*porosity &
                     + phyto_jacob%dMeHgphyto_dHS * m%MeHg                  !todo: check *MeHg
        
		Jacob(5,1) = (two*m%HgHS2 + two*m%Hg_HS_2 + two*m%HgS2 + m%HgHS + m%HgS + m%HgOHHS)*porosity
		Jacob(5,2) = (m%MeHgS + two*m%MeHg2S)*porosity;
        
		Jacob(5,5) = (four*m%HgHS2 + four*m%Hg_HS_2 + four*m%HgS2 + m%HgHS + m%HgS + m%HgOHHS + m%Sulph + m%H2S + m%HS +  &
                      m%MeHgS + m%MeHg2S)*porosity;

		if (n>5) then
			Jacob(1,1) =  Jacob(1,1) + m%xRS_Hg + m%xRS_2_Hg
			Jacob(1,6) = m%xRS_Hg + two*m%xRS_2_Hg

			Jacob(2,2) =  Jacob(2,2) + m%xRS_MeHg
			Jacob(2,6) = m%xRS_MeHg

			Jacob(5,5) = Jacob(5,5) + m%xRS_Hg + four* m%xRS_2_Hg + m%xRS_MeHg + m%xR_SH;
			Jacob(5,6) = m%xRS_Hg + four* m%xRS_2_Hg + m%xRS_MeHg + m%xR_SH;
            
			Jacob(6,1) = m%xRS_Hg + two* m%xRS_2_Hg;
			Jacob(6,2) = m%xRS_MeHg;
			Jacob(6,5) = m%xRS_Hg + four* m%xRS_2_Hg + m%xRS_MeHg + m%xR_SH;
			Jacob(6,6) = Jacob(6,5) + m%xROH;
			
		endif
    endif
    if (itype == 1) then                                !modify jacabian if solid HgII and MeHg ara knowns
		Jacob(1,1) = m%XOHg
		Jacob(1,3) = 0.0
		Jacob(1,4) = 0.0
		Jacob(1,4) = m%XOHg
		Jacob(2,2) = m%XOMeHg
		Jacob(2,3) = 0.0
		Jacob(2,4) = m%XOMeHg
		if (n==6) then
			Jacob(1,1) = Jacob(1,1) + m%xRS_Hg + m%xRS_2_Hg
			Jacob(1,5) = m%xRS_Hg + 2.0* m%xRS_2_Hg
			Jacob(1,6) = m%xRS_Hg + 2.0*m%xRS_2_Hg
			!sum_MeHg = sum_MeHg + RSMeHg
			Jacob(2,2) = Jacob(2,2) + m%xRS_MeHg
			Jacob(2,5) = m%xRS_MeHg
			Jacob(2,6) = m%xRS_MeHg
		endif
    end if
end subroutine jacobian
    
Subroutine set_complexes_to_zero(m)
    type (eq_complexes), intent (inout):: m         !> species concentrations
    
    m%HgHS2     =zero
    m%Hg_HS_2   =zero
    m%HgHS      =zero
    m%HgS2      =zero   
    m%HgS       =zero
    m%HgOHHS    =zero
    
    m%MeHgS     =zero
    m%MeHg2S    =zero
    
    m%xRS_Hg    =zero
    m%xRS_2_Hg  =zero
    m%xRS_MeHg  =zero
    
    m%MeHg_phyto = zero
    m%HgII_phyto = zero
   
end subroutine

!> use to compute molar totals for testing the equilibrium solution
subroutine mass_action_test(jacob_order,vals, Cl, pH, phyto, sum, m, conc, itype)               
    integer, intent (in) :: jacob_order
    type (eq_vals), intent (in) :: vals
    real (gtm_real), intent (in) :: Cl
    real (gtm_real), intent (in) :: pH
    real (gtm_real), intent (in) :: phyto
    !type (k_eq_solids), intent (in) :: ks              !todo: add later
    type (molar_total), intent (inout) :: sum
    type (eq_complexes), intent (out) :: m
    type (hg_concs), intent (inout):: conc
    integer, intent (in) :: itype
    real (gtm_real) :: porosity = 1 
    type (phytoplankton_jacob):: phyto_jacob !> internal variable to hold phytoplankton contributions to jacobian
    type (k_eq_solids) :: ks
   
    m%Cl = Cl/molar_Cl_to_mg_l
    m%H = 10d0**(-pH)
    m%OH = 10d0**(-(14d0-pH))
    m%HgII = vals%HgII
    m%MeHg = vals%MeHg
    m%RS   = vals%RS
    m%XOH  = vals%XOH
    m%HS   = vals%HS
    m%xROH = vals%xROH
    call mass_action(ks, porosity, phyto, phyto_jacob, jacob_order,m, sum, itype)
    if (itype==1) then
        sum%HgII = m%XOHg
        sum%MeHg = m%XOMeHg
        if (jacob_order == 6) then
            sum%HgII = sum%HgII + m%xRS_Hg + m%xRS_2_Hg
			sum%MeHg = sum%MeHg + m%xRS_MeHg
        endif
    endif
end subroutine

subroutine Hg_reactant_concs(m, nosolids, ss, sol_inp, conc)
    !args
    type (eq_complexes), intent (in)    :: m
    integer, intent (in)                :: nosolids
    real (gtm_real), dimension(nosolids), intent(in) :: ss      !solids (mg/L)
    type (solids_inputs), dimension(nosolids), intent(in) :: sol_inp   !solids (mg/L)
    type (hg_concs), intent (inout)       :: conc
    !local
    integer :: ii
    real (gtm_real) :: exchnge_XOH
    real (gtm_real) :: exchnge_ROH
    conc%HgII_diss      = (m%HgII + m%HgCl + m%HgCl2 + m%HgCl3 + m%HgCl4 + m%HgOHCl + m%HgOH + m%Hg_OH_2 + m%HgRS + m%Hg_RS_2 +    &
                           m%HgHS2 + m%Hg_HS_2 + m%HgS2 + m%HgHS + m%HgS + m%HgOHHS) * molar_Hg_to_ng_l
    conc%HgII_organic   = (m%HgRS + m%Hg_RS_2) * molar_Hg_to_ng_l
    conc%HgII_inorganic = conc%HgII_diss - conc%HgII_organic
    
    conc%MeHg_diss      = (m%MeHg + m%MeHgCl + m%MeHgOH + m%MeHgRS + m%MeHgS + two* m%MeHg2S) * molar_Hg_to_ng_l
    conc%MeHg_organic   = m%MeHgRS * molar_Hg_to_ng_l
    conc%MeHg_inorganic = conc%MeHg_diss - conc%MeHg_organic
    
    select case (Methyl_switch)                 !> HgII available for methylation (ug/m3)
        case (1)                                !> free ion
            conc%HgII_methyl =  m%HgII * molar_Hg_to_ng_l          
        case (2)                                !> HgCl2
            conc%HgII_methyl =  m%HgCl2 * molar_Hg_to_ng_l
        case (3)                                !> inorganic HgII complexes
            conc%HgII_methyl =  conc%HgII_inorganic
        case (4)                                !> neutral HgII complexes
            conc%HgII_methyl =  (m%HgCl2 +  m%Hg_OH_2 +  m%HgOHCl + m%Hg_HS_2 + m%HgS + m%HgOHHS)* molar_Hg_to_ng_l
        case (5)                                !> all HgII complexes
            conc%HgII_methyl =  conc%HgII_diss
        case default
            conc%HgII_methyl =  conc%HgII_diss
    end select

	select case (Reduction_switch)              !> HgII available for photoreduction (ug/m3)
        case (1)                                !> Hg(OH)2 complexes
            conc%HgII_photo = m%Hg_OH_2 * molar_Hg_to_ng_l
        case (2)                                !> OH and Cl complexes
            conc%HgII_photo = (m%HgCl + m%HgCl2 + m%HgCl3 + m%HgCl4 + m%HgOH + m%Hg_OH_2 + m%HgOHCl) * molar_Hg_to_ng_l
        case (3)                                !> DOC complexes 
            conc%HgII_photo = conc%HgII_organic
        case default
            conc%HgII_photo = conc%HgII_diss
    end select

	select case (biodeMethyl_switch)            !> MeHgII avalable for bio-demethylation (ug/m3)
        case (1)                                !> free ion
            conc%MeHg_biodemeth = m%MeHg * molar_Hg_to_ng_l
        case (2)                                !> MeHgCl
            conc%MeHg_biodemeth = m%MeHgCl * molar_Hg_to_ng_l
        case (3)                                !> inorganic MeHg complexes
			conc%MeHg_biodemeth = conc%MeHg_inorganic
        case (4)                                !> all MeHg complexes
            conc%MeHg_biodemeth = conc%MeHg_diss
        case default
            conc%MeHg_biodemeth = conc%MeHg_diss
    end select
        
    conc%MeHg_photo = conc%MeHg_diss            !> MeHg available for photodegradation (ug/m3)
    conc%MeHg_Cl = m%MeHgCl * molar_Hg_to_ng_l  !> MeHg conc. for volatilization
    
    !> solids concentrations
    exchnge_XOH = zero
    exchnge_ROH = zero
    !todo: add to sol_inp type-  xchangeXOH(ii) = sol_inp(ii)%XOH_exchange_frac * sol_inp(ii)%mole_XOH/1.0d3 and revise following code
    do ii=1, nosolids
        exchnge_XOH = exchnge_XOH + sol_inp(ii)%XOH_exch_frac * sol_inp(ii)%mole_XOH * ss(ii)/1.0d3     !> (moles/L)
        exchnge_ROH = exchnge_ROH + sol_inp(ii)%mole_ROH * ss(ii)/1.0d3                                     !> (moles/L)
    end do
    do ii=1, nosolids
        conc%HgII_ssX(ii) = (((sol_inp(ii)%XOH_exch_frac * sol_inp(ii)%mole_XOH  * ss(ii)/1.0d3)/exchnge_XOH) * m%XOHg)/ (ss(ii)/1.0d3) * mole_Hg *1.0d6    !> ug/g
        conc%MeHg_ss(ii) =  (((sol_inp(ii)%XOH_exch_frac * sol_inp(ii)%mole_XOH  * ss(ii)/1.0d3)/exchnge_XOH) * m%XOMeHg)/ (ss(ii)/1.0d3) * mole_Hg *1.0d6  !> ug/g
        conc%HgII_ssR(ii) = (((sol_inp(ii)%mole_ROH  * ss(ii)/1.0d3)/exchnge_ROH) * (m%xRS_Hg + m%xRS_2_Hg))/ (ss(ii)/1.0d3) * mole_Hg *1.0d6                   !> ug/g
    end do
    
end subroutine Hg_reactant_concs


subroutine phyto_calcs(m, phyto, phyto_jacob)
    !args
    type (eq_complexes), intent (inOUT) :: m
    real (gtm_real), intent (in)        :: phyto        !> phytoplankton concentration (mg/L) 
    type (phytoplankton_jacob), intent (out)       :: phyto_jacob 
    !local
    real (gtm_real) :: alpha1
    real (gtm_real) :: alpha2
    real (gtm_real) :: kpass
    real (gtm_real) :: MeHg_active                  !>concentration of MeHg that is actively transported across cell membrane
    real (gtm_real) :: HgII_active                  !>concentration of MeHg that is actively transported across cell membrane
    real (gtm_real) :: dMeHg_act_dMeHg              !>partial derivative active uptake wrt MeHg
    real (gtm_real) :: dMeHg_act_dHS                !>partial derivative active uptake wrt HS
    real (gtm_real) :: k1
    real (gtm_real) :: k2
    real (gtm_real) :: dk1_dMeHg
    real (gtm_real) :: dk1_dHS
    
    !> MeHg mass action
    alpha1 = k_phyto%k_phytoMeHg/(one + k_phyto%H_phytoMeHg*m%H);
    alpha2 = one/((k_phyto%u_phyto + k_phyto%kd_phytoMeHg)*k_phyto%mass_cell);
    kpass = k_phyto%kpassMeHg*(m%MeHgCl + 0.04d0*m%MeHgOH);
    if (phyto_uptakeMeHg == 1)	then                !>free ion uptake
		MeHg_active= m%MeHg
		dMeHg_act_dMeHg= one
    else                                            !> total inorganic MeHg				
		MeHg_active= m%MeHg + m%MeHgCl + m%MeHgOH + m%MeHgS + m%MeHg2S
        dMeHg_act_dMeHg= (m%MeHg + m%MeHgCl + m%MeHgOH + m%MeHgS + two* m%MeHg2S)/m%MeHg	
    endif
    k1 = alpha2*(alpha1*MeHg_active + kpass)/MeHg_active
    k2 = k_phyto%diff/(k1+k_phyto%diff)
    m%MeHg_phyto = k1*k2*MeHg_active*(phyto* 1.0d-6)
    
     !> HgII mass action
    
    if (phyto_uptakeHgII == 1) then                 !> free ion 
        HgII_active = m%HgII
    else						                    !> total inorganic
        HgII_active = m%HgII+m%HgCl+m%HgCl2+m%HgCl3+m%HgCl4+m%HgOHCl+m%HgOH+m%Hg_OH_2 &
                      + m%HgHS2+m%Hg_HS_2+m%HgS2+m%HgHS+m%HgS+m%HgOHHS
    endif
    m%HgII_phyto = k_phyto%diff*HgII_active*(phyto * 1.0d-6)
    
    !> partial derivatives for jacobian for phyto plankton
    !todo: finish partial derivatives
    dk1_dMeHg = -alpha2*((alpha1*MeHg_active + kpass)/(MeHg_active**two))* dMeHg_act_dMeHg &
			            + alpha2*(alpha1*dMeHg_act_dMeHg + kpass/m%MeHg)/MeHg_active
    
    !> this the is contribution to Jacob(2,2) = Jacob(2,2) + phyto_jacob%dMeHgphyto_dMeHg * MeHg
    phyto_jacob%dMeHgphyto_dMeHg = ((phyto * 1.0d-6) * k2*(k1*dMeHg_act_dMeHg + MeHg_active* dk1_dMeHg &
                        - (k1/(k1+k_phyto%diff))* MeHg_active* dk1_dMeHg)) * m%MeHg
    
    !> this the is contribution to Jacob(2,5) = Jacob(2,5) + phyto_jacob%dMeHgphyto_dHS * m%MeHg
    if (m%MeHgS > zero) then
        dMeHg_act_dHS = (m%MeHgS + m%MeHg2S)/m%HS
        dk1_dHS= -alpha2*((alpha1*MeHg_active + kpass)/MeHg_active ** two)* dMeHg_act_dHS &
                      +alpha2*(alpha1*dMeHg_act_dHS )/MeHg_active
        
		phyto_jacob%dMeHgphyto_dHS = ((phyto * 1.0d-6) * k2*(k1*dMeHg_act_dHS + MeHg_active* dk1_dHS &
		                - (k1/(k1+k_phyto%diff))* MeHg_active* dk1_dHS))* m%MeHg
    endif
    
    !> this the is contribution to Jacob(2,1) = Jacob(2,1) + phyto_jacob%dHgIIphyto_dHgII
    phyto_jacob%dHgIIphyto_dHgII = k_phyto%diff*HgII_active*(phyto * 1.0d-6)
    
    
    !> this is contribution to Jacob(1,5) = Jacob(1,5)+ phyto_jacob%dHgIIphyto_dHS
    if (m%HS > zero) then
        phyto_jacob%dHgIIphyto_dHS = k_phyto%diff*HgII_active*(phyto * 1.0d-6) &
                         * (two*m%HgHS2+two*m%Hg_HS_2+two*m%HgS2+m%HgHS+m%HgS+m%HgOHHS)
        
    end if
    
end subroutine phyto_calcs

!> phyto_init should only be called once
subroutine phyto_init()
    k_phyto%k_phytoMeHg = 10.0d0**k_phyto%k_phytoMeHg
    k_phyto%H_phytoMeHg = 10.0d0**k_phyto%H_phytoMeHg
    
    k_phyto%area_cell = four * pi * k_phyto%r_cell**two
	k_phyto%mass_cell = (four / three) * k_phyto%d_cell* pi * k_phyto%r_cell**three
    k_phyto%kpassMeHg = k_phyto%p_phytoMeHg*k_phyto%area_cell
    k_phyto%diff = four * pi * k_phyto%r_cell * k_phyto%diff_Hg / (k_phyto%u_phyto * k_phyto%mass_cell)
    !k_phyto%passHgII = k_phyto%p_phytoHgII*area_cell                       !todo check no passive uptake of HgII ???
end subroutine phyto_init
 

end module equilibrium