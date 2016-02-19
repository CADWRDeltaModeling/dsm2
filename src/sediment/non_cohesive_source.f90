!<license>
!    Copyright (C) 2015 State of California,
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

!> Routines provide the calculation for non-cohesive suspended sediment erosion and deposition functions.
!> The functions in this module are: 1.	Garcia and Parker (1991) 2. van Rijn (1984b)
!>  3.	Smith and McLean (1977) 4.	Zyserman and Fredsoe (1994)  for more details see ASCE manual #110 page 117.
!>@ingroup sediment 

module non_cohesive_source

    contains 
    subroutine source_non_cohesive(vertical_flux,    &
                                   resuspension,     &
                                   deposition,       &
                                   conc,             &
                                   flow,             &
                                   area,             &
                                   width,            &
                                   hydro_radius,     &
                                   manning,          &
                                   diameter,         &
                                   ncell,            &
                                   nclass,           &
                                   pick_up_flag)

        use gtm_precision 
        use sediment_variables
        use suspended_utility

        implicit none
        real(gtm_real),intent(out):: vertical_flux(ncell,nclass)    !< vertical sediment net flux into the water column
        real(gtm_real),intent(out):: resuspension(ncell,nclass)     !< entrainment for resuspension
        real(gtm_real),intent(out):: deposition(ncell,nclass)       !< deposition
        real(gtm_real),intent(in) :: conc(ncell,nclass)             !< concentration at new time
        real(gtm_real),intent(in) :: flow(ncell)                    !< flow
        real(gtm_real),intent(in) :: area(ncell)                    !< area
        real(gtm_real),intent(in) :: width(ncell)                   !< channel width
        real(gtm_real),intent(in) :: hydro_radius(ncell)            !< hydraulic radius
        real(gtm_real),intent(in) :: manning(ncell)                 !< Manning's n
        real(gtm_real),intent(in) :: diameter(nclass)               !< diameter
        integer, intent(in)       :: ncell                          !< number of cells 
        integer, intent(in)       :: nclass                         !< number of classes for non-cohesive sediment in suspension
        character(len=32), optional, intent(in) :: pick_up_flag     !< switch for sediment pickup function

        !---local
        real(gtm_real) :: velocity(ncell)                           !< flow velocity
        real(gtm_real) :: c_bar_bed(ncell,nclass)                   !< near bed vaule of mean volumetric sediment concentration
        real(gtm_real) :: fall_vel(nclass)                          !< settling velocity         
        real(gtm_real) :: rouse_num(ncell,nclass)                   !< Rouse dimensionless number  
        real(gtm_real) :: big_e_sub_s(ncell,nclass)                 !< dimenssionless rate of entrainment of bed sediment into suspension  
        real(gtm_real) :: shear_v(ncell)                            !< shear velocity   
        real(gtm_real) :: exp_re_p(nclass)                          !< explicit particle reynolds number  
        real(gtm_real) :: I_1(ncell,nclass)                         !< first Einstein integral value   
        integer :: iclass                                           !< counter on grain class  
         
        character :: pick_up_function 
        logical   :: function_van_rijn 
        logical   :: si_unit 

        velocity = flow/area

        pick_up_function = 'garcia_parker'

        if (present(pick_up_flag)) then
            pick_up_function = pick_up_flag
        end if

        ! here verfical_net_sediment_flux = settling_vel * (Es - c_bar_sub_b)
        call settling_velocity(fall_vel,             &
                               kinematic_viscosity,  &
                               specific_gravity,     &
                               diameter,             &
                               gravity,              &
                               nclass,               &
                               function_van_rijn)
                       
        call explicit_particle_reynolds_number(exp_re_p,           &
                                               diameter,           &
                                               capital_r,          &
                                               gravity,            &
                                               kinematic_viscosity,&
                                               nclass)

        call shear_velocity_calculator(shear_v,             &
                                       velocity,            &
                                       manning,             &
                                       gravity,             &
                                       hydro_radius,        &
                                       ncell,               &
                                       .true.)                !si_unit
                                                              
        !------Es                       
        call es_garcia_parker(big_e_sub_s,       &
                              shear_v,           &
                              exp_re_p,          &
                              fall_vel,          & 
                              nclass,            &
                              ncell)
                      
        call rouse_dimensionless_number(rouse_num,   &
                                        fall_vel,    &
                                        shear_v,     &
                                        ncell,       &
                                        nclass) 

        !------C_bar _b
        call first_einstein_integral(I_1,       &
                                     delta_b,   &
                                     rouse_num, &
                                     ncell,     &
                                     nclass) 

        c_bar_bed = conc/I_1

        ! dimension of sediment vertical flux is area per time
        do iclass=1,nclass
            resuspension(:,iclass) = width*big_e_sub_s(:,iclass)*dx
            deposition(:,iclass) = fall_vel(iclass)*c_bar_bed(:,iclass)
            !vertical_flux(:,iclass) = width*fall_vel(iclass)*(big_e_sub_s(:,iclass) - c_bar_bed(:,iclass))
            vertical_flux(:,iclass) = width*big_e_sub_s(:,iclass) - fall_vel(iclass)*c_bar_bed(:,iclass)
        end do  
   
        call deallocate_sediment_variables

    end subroutine 

 
    !> Calculates the first Einstein integral values
    !> This subroutine is developed based on analtycal solution of Guo and Julien (2004)
    !> To avoid disambiguation: C_bar = c_b_bar * first_einstein_integral
    !> the out put of the subroutine is equal to J_1 in the page 116 of ASCE sediment manual  
    !> To avoid singularities here an analytical solution used for integers    
    ! todo: Should we place this subroutine here? another separate file? or sediment derived variable?
    ! I think we will use it again in the bedload
    subroutine first_einstein_integral(I_1,       &
                                       delta_b,   &
                                       rouse_num, &
                                       ncell,     &
                                       nclass)                                    
        use gtm_precision
        use error_handling
        implicit none
        !-- arg
        integer, intent(in):: ncell                            !< Number of computational volumes in a channel
        integer, intent(in):: nclass                           !< Number of non-cohesive sediment grain classes
        real(gtm_real),intent(in) :: rouse_num(ncell,nclass)   !< Rouse dimenssionless number  
        real(gtm_real),intent(in) :: delta_b                   !< Relative bed layer thickness = b/H 
        real(gtm_real),intent(out):: I_1(ncell,nclass)         !< First Einstein integral value

        !-- local
        integer :: ivol
        integer :: iclass
        real(gtm_real) :: ro_l   
        real(gtm_real) :: ro_r    !right
        real(gtm_real) :: i_1_l
        real(gtm_real) :: i_1_r   !right

        do ivol=1,ncell
            do iclass=1,nclass
                if (rouse_num(ivol,iclass) > 3.98d0) then
                    !todo: I am not sure if we need this subroutine in bed load or not 
                    print *, 'error in rouse number' ! todo: remove
                    pause
                    call gtm_fatal("This is not a Rouse number value for suspended sediment!")            
                elseif (abs(rouse_num(ivol,iclass) - three)< 0.01d0) then
                    ro_l = three - 0.05d0
                    ro_r = three + 0.05d0 
                    call inside_i_1(i_1_l,delta_b,ro_l)
                    call inside_i_1(i_1_r,delta_b,ro_r)
                    I_1(ivol,iclass) = (i_1_r + i_1_l) / two                 
                elseif (abs(rouse_num(ivol,iclass) - two)< 0.01d0) then
                    ro_l = two - 0.05d0
                    ro_r = two + 0.05d0 
                    call inside_i_1(i_1_l,delta_b,ro_l)
                    call inside_i_1(i_1_r,delta_b,ro_r)
                    I_1(ivol,iclass) = (i_1_r + i_1_l) / two                       
                elseif(abs(rouse_num(ivol,iclass) - one)< 0.01d0) then  
                    ro_l = one - 0.05d0
                    ro_r = one + 0.05d0 
                    call inside_i_1(i_1_l,delta_b,ro_l)
                    call inside_i_1(i_1_r,delta_b,ro_r)
                    I_1(ivol,iclass) = (i_1_r + i_1_l) / two
                else
                    call inside_i_1(I_1(ivol,iclass),       &
                                    delta_b,                &
                                    rouse_num(ivol,iclass))                 
                end if
            end do
        end do
    end subroutine

    !> inside I_1
    pure subroutine inside_i_1(J_1,      &
                               delta_b,  &
                               rouse)                               
        use gtm_precision
        implicit none
        real(gtm_real),intent(in) :: rouse         !< Rouse dimenssionless number  
        real(gtm_real),intent(in) :: delta_b       !< Relative bed layer thickness = b/H 
        real(gtm_real),intent(out):: J_1           !< First Einstein integral value

        J_1   = (rouse*pi/dsin(rouse*pi) - ((one-delta_b)**rouse)/(delta_b**(rouse-one))    &
               - rouse*(((delta_b/(one-delta_b))**(one-rouse))  /(one-rouse))               & 
               + rouse*(((delta_b/(one-delta_b))**(two-rouse))  /(one-rouse))               &
               - rouse*(((delta_b/(one-delta_b))**(three-rouse))/(one-rouse))               &
               + rouse*(((delta_b/(one-delta_b))**(four-rouse)) /(one-rouse))               &
               - rouse*(((delta_b/(one-delta_b))**(five-rouse)) /(one-rouse))               &
               + rouse*(((delta_b/(one-delta_b))**(six-rouse))  /(one-rouse))               &
               - rouse*(((delta_b/(one-delta_b))**(seven-rouse))/(one-rouse))               &
               + rouse*(((delta_b/(one-delta_b))**(eight-rouse))/(one-rouse))               &
               - rouse*(((delta_b/(one-delta_b))**(nine-rouse)) /(one-rouse))               &
               + rouse*(((delta_b/(one-delta_b))**(ten -rouse)) /(one-rouse)))              &
               * (delta_b**(rouse)/((one-delta_b)**rouse))                               
    end subroutine 


    !> Entrainment by Garcia Parker
    subroutine es_garcia_parker(big_e_sub_s,       &
                                shear_v,           &
                                exp_re_p,          &
                                settling_v,        & 
                                nclass,            &
                                ncell)
        use gtm_precision
        implicit none
        !-- arg
        integer, intent(in):: ncell                             !< Number of computational volumes in a channel
        integer, intent(in):: nclass                            !< Number of non-cohesive sediment grain classes
        real(gtm_real),intent(out):: big_e_sub_s(ncell,nclass)  !< Dimenssionless rate of entrainment of bed sediment into suspension (i.e., vol entrained sediment/unit bed area/time)                                       
        ! big_e_sub_s is in a range of 0.0002 ~ 0.06
        real(gtm_real),intent(in) :: shear_v(ncell)             !< Shear Velocity
        real(gtm_real),intent(in) :: exp_re_p(nclass)           !< Explicit particle Reynolds number
        real(gtm_real),intent(in) :: settling_v(nclass)         !< Settling velocity
        !---local
        real(gtm_real) :: z_u(ncell,nclass)                     !< Captial z sub u a measure for strength of shear stress but it also takes into account the particle size in Garcia notation
        real(gtm_real), parameter :: cap_a = 1.3d-7             ! Constant value (see ASCE sediment manual no. 110 page 118)
        integer :: ivol

        do ivol=1,ncell
            z_u(ivol,:) = shear_v(ivol)*(exp_re_p**0.6d0)/settling_v
            where (exp_re_p < 3.5d0)
                z_u(ivol,:) = 0.708d0*shear_v(ivol)*(exp_re_p**0.6d0)/settling_v
            end where 
        end do
        big_e_sub_s  = cap_a*(z_u**five)/(one + (z_u**five)*cap_a/0.3d0)                                  
        return                             
    end subroutine

end module 