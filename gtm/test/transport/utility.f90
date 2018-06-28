!<!license>
!    Copyright (C) 2017 State of California,
!    Department of Water Resources.
!    This file is part of DSM2.
!
!    The Delta Simulation Model 2 (DSM2) is free software: 
!    you can redistribute it and/or modify
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

!> Routines for grid refinement/coarsening operations.
!>@ingroup test_transport
module test_utility

contains

!> Coarsen a solution at a fine level of resolution
subroutine coarsen(coarse_data, &
                   fine_data,   &
                   ncell_fine,  &
                   ncell_coarse,&
                   nvar)

use gtm_precision
use error_handling

implicit none
!---arg
integer,intent(in) :: ncell_coarse                              !< Number of coarsened array cells 
integer,intent(in) :: ncell_fine                                !< Number of fine initial array cells
integer,intent(in) :: nvar                                      !< Number of constituents
real(gtm_real), intent(in) :: fine_data(ncell_fine,nvar)        !< Fine initial data  (input)
real(gtm_real), intent(out):: coarse_data(ncell_coarse,nvar)    !< Coarsened finial data (output)

!---locals
real(gtm_real) :: coarsen_factor                                !< Coarsening factor (must be an integer)
integer :: ivar                                                 !< Counter on constituents
integer :: icell                                                !< Counter
integer :: i_coarse                                             !< Counter

!> Check if the coarsening factor is an integer and if not it bails.
if (mod(ncell_fine , ncell_coarse) /= 0) then
    call gtm_fatal("Coarsening factor is not an integer!")  
else
    coarsen_factor = ncell_fine/ncell_coarse
!> Computes coarsened array base on the coarsening factor from fine input array.
    do ivar=1,nvar
        do icell=1,ncell_coarse
            coarse_data(icell,ivar) = zero
            i_coarse = 0
            do while (i_coarse < coarsen_factor) 
              coarse_data(icell,ivar) = coarse_data(icell,ivar)+ fine_data(icell*coarsen_factor-i_coarse,ivar)
              i_coarse= i_coarse + 1   
            end do
            coarse_data(icell,ivar)= coarse_data(icell,ivar)/dble(coarsen_factor)
        end do
    end do
    
end if

return
end subroutine 


!================================

!> Calculate the L-1, L-2 and L-inf error norms for calculated values and reference solution
subroutine error_norm(norm_1,    &
                      norm_2,    &
                      norm_inf,  &
                      which_cell,&
                      vals,      &
                      reference, &
                      ncell,     &
                      dx)

use gtm_precision

implicit none

integer, intent(in) :: ncell                     !< Number of cells
integer, intent(out):: which_cell                !< The cell in which largest error occurs
real(gtm_real), intent(out) :: norm_1            !< L-1  error norm
real(gtm_real), intent(out) :: norm_2            !< L-2  error norm
real(gtm_real), intent(out) :: norm_inf          !< L-inf error norm

real(gtm_real), intent(in) :: vals(ncell)        !< Calculated values
real(gtm_real), intent(in) :: reference(ncell)   !< Reference or 'other' values
real(gtm_real), optional   :: dx(ncell)          !< Spatial step
!todo: do we use this?

!------ locals                                    
integer :: icell                                
real(gtm_real) :: err
real(gtm_real) :: sq_error
real(gtm_real) :: abs_error
!> initial value of all norms is zero
norm_1=zero
norm_2=zero
norm_inf=zero
!> sum up the L-1 and L-2, and fid the largest error in the domain (L-inf) 
do icell=1,ncell
   err = vals(icell) - reference(icell)
   abs_error = abs(err)
   sq_error = err*err
   if (abs_error > norm_inf) then
       norm_inf = abs_error
       which_cell = icell
   end if
   norm_1 = norm_1 + abs_error
   norm_2 = norm_2 + sq_error
end do
norm_1 = norm_1/dble(ncell)
norm_2 = dsqrt(norm_2)/dble(ncell)

return
end subroutine

!> Claculates the total mass error and checks the oscillations
subroutine mass_comparison(vals,            &
                           benchmark_val,   &
                           num_cell,        &
                           dx,              &
                           mass_error,      &
                           mass_alarm)
use gtm_precision
              
implicit none

integer, intent(in) :: num_cell                         !< Number of cells
real(gtm_real), intent(in)  :: vals(num_cell)           !< Numerical values 
real(gtm_real), intent(in)  :: benchmark_val(num_cell)  !< Benchmark (analytical) values
real(gtm_real), intent(in)  :: dx                       !< Space discretization
real(gtm_real), intent(out) :: mass_error               !< Error of mass_val from mass_benchmark in percent
logical, intent(out) :: mass_alarm                      !< Negative mass alarm =.true. , no negative mass = .false.
!--- local
real(gtm_real) :: mass_val                              !< Total mass of the numerical solution
real(gtm_real) :: mass_benchmark                        !< Total mass of the benchmark values


mass_val       = sum(vals)         *dx
mass_benchmark = sum(benchmark_val)*dx

mass_error = (mass_benchmark - mass_val)/mass_benchmark

if (minval(vals)< zero) then
    mass_alarm = .true.
else
    mass_alarm = .false.
end if

return
end subroutine

!======================================

!> Create a assert message based on an error ratio
subroutine create_converge_message(converge_message,norm_name,label,ratio)
use gtm_precision
implicit none
character(LEN=*),intent(out) :: converge_message !< Message
character(LEN=*),intent(in)  :: norm_name        !< Name of norm (something like 'L-2 (fine)')
character(LEN=*),intent(in)  :: label            !< Label identifying problem
real(gtm_real),intent(in)    :: ratio            !< Error ratio

write(converge_message,"(a,' 2nd order on ',a, ' (coarser-finer error ratio =', f7.4,', 4.0 indicates O(2) accuracy)')")norm_name,label,ratio
return
end subroutine
 
 !> Logs convergence results to a file
 !> Outputs the norm-p errors, maximum velocity, discretization parameters and CFL
 ! todo: add peclet number and grid peclet number and other needed terms here
 ! todo: this subroutine assumes a lot, like a scalar reaction rate. Makes it less general
 subroutine log_convergence_results(norm_error,    &
                                    nrefine,       &
                                    dx,            &
                                    dt,            &
                                    max_velocity,  &
                                    label,         &
                                    which_cell,    &
                                    ncell_base,    &
                                    ntime_base,    &
                                    reaction_rate, &
                                    dispersion,    &
                                    scheme_order,  &
                                    length_scale,  &
                                    limiter_switch)
 use gtm_precision
 implicit none
 
 integer, parameter  :: log_unit = 91                 !< Unit ID of the output file 
 integer, intent(in) :: nrefine                       !< Number of grid refinement 
 integer, intent(in) :: which_cell(nrefine)           !< ID of the cell in which worst error is detected 
 integer, intent(in) :: ncell_base                    !< Number of cells in the finest grid size
 integer, intent(in) :: ntime_base                    !< Number of time steps in the finest grid size
 real(gtm_real),  intent(in) :: norm_error(3,nrefine) !< Matrix storing the norms of errors
 real(gtm_real),  intent(in) :: dx                    !< Spacial step
 real(gtm_real),  intent(in) :: dt                    !< Time step  
 character(LEN=*),intent(in) :: label                 !< Test's label
 real(gtm_real),intent(in),optional :: scheme_order   !< Scheme's nominal order of accuracy 
 real(gtm_real),intent(in),optional :: max_velocity   !< Maximum velocity
 real(gtm_real),intent(in),optional :: reaction_rate  !< First order reaction rate (Lambda)
 real(gtm_real),intent(in),optional :: dispersion     !< Streamwise dispersion coefficient
 real(gtm_real),intent(in),optional :: length_scale   !< Length scale (assumed to be dx everywhere otherwise it is mentioned)  
 real(gtm_real) :: refine_rate = two                  !< Refinement ration 
 logical,intent(in),optional :: limiter_switch        !< Switch for the flux limiter
  ! local
 real(gtm_real) :: order

if (present (scheme_order)) then
    order = scheme_order
else
    order = two
end if

open(unit = log_unit, file= trim(label)//'_convergence_log.txt', &
      status='unknown')
    
write(log_unit,*)"==== Convergence test results "// label,' ====' 
write(log_unit,*)
write(log_unit,*)'(from finer to coarser)'
write(log_unit,*)'L-inf error ratio '
write(log_unit,*) norm_error(3,2)/norm_error(3,1),norm_error(3,3)/norm_error(3,2)
write(log_unit,*)'L-2 error ratio '
write(log_unit,*) norm_error(2,2)/norm_error(2,1),norm_error(2,3)/norm_error(2,2)
write(log_unit,*)'L-1 error ratio '
write(log_unit,*) norm_error(1,2)/norm_error(1,1),norm_error(1,3)/norm_error(1,2)
write(log_unit,*)
write(log_unit,*)'L-inf convergence rate estimate'
write(log_unit,*)'fine :',dlog(norm_error(3,2)/norm_error(3,1))/dlog(refine_rate),' coarse :',dlog(norm_error(3,3)/norm_error(3,2))/dlog(refine_rate)  
write(log_unit,*)'L-2 convergence rate estimate'
write(log_unit,*)'fine :',dlog(norm_error(2,2)/norm_error(2,1))/dlog(refine_rate),' coarse :',dlog(norm_error(2,3)/norm_error(2,2))/dlog(refine_rate)  
write(log_unit,*)'L-1 convergence rate estimate'
write(log_unit,*)'fine :',dlog(norm_error(1,2)/norm_error(1,1))/dlog(refine_rate),' coarse :',dlog(norm_error(1,3)/norm_error(1,2))/dlog(refine_rate)  
write(log_unit,*)
write(log_unit,*)'number of cells : ',ncell_base,ncell_base/2,ncell_base/4
write(log_unit,*)'L-inf occures at :',which_cell(1),which_cell(2),which_cell(3)
write(log_unit,*) 'dx :', dx/four,dx/two,dx 
write(log_unit,*)
write(log_unit,*)'number of steps : ',ntime_base,ntime_base/2,ntime_base/4
write(log_unit,*)'dt :',dt/four,dt/two,dt
write(log_unit,*)
write(log_unit,*)'Error L-inf '//label//' : '
write(log_unit,*) norm_error (3,:)
write(log_unit,*)'Error L-2 '//label//' : '
write(log_unit,*) norm_error (2,:)
write(log_unit,*)'Error L-1 '//label//' : '
write(log_unit,*) norm_error (1,:)
write(log_unit,*)

if (present(reaction_rate)) then
    write(log_unit,*) 'Decay rate : ', reaction_rate
    write(log_unit,*) 'Kdt : '
    write(log_unit,*) reaction_rate*dt/four,reaction_rate*dt/two,reaction_rate*dt
    write(log_unit,*)
end if 

if (present(dispersion)) then
    write(log_unit,*) 'Dispersion coefficient : ', dispersion
    write(log_unit,*) 'Diffusion Number (Ddt/dx2) : '
    write(log_unit,*) four*dispersion*dt/dx/dx,two*dispersion*dt/dx/dx,dispersion*dt/dx/dx
    write(log_unit,*)
end if 

if (present(max_velocity)) then
    write(log_unit,*) 'CFL : (<1)' , max_velocity*dt/dx 
    write(log_unit,*) 'Max Velocity', max_velocity
    if (present(limiter_switch)) then
        if (limiter_switch == .true.)then
            write(log_unit,*) 'Flux Limiter : ON '
        else
            write(log_unit,*) 'Flux Limiter : OFF'
        end if
        write(log_unit,*)
    end if  
    
    if (present(dispersion)) then
        write(log_unit,*) 'Mesh Peclet Number(Vdx/D) :'
        write(log_unit,*) max_velocity*dx/dispersion/four,max_velocity*dx/dispersion/two,max_velocity*dx/dispersion
        write(log_unit,*)
        ! todo: do we also need Peclet number? I don't think
    end if
    
    if (present(reaction_rate)) then
        ! todo: this part is the scale of Damkohler
        write(log_unit,*) 'Da : Advection Time Scale/ Reaction Time Scale'
        write(log_unit,*) 'Da =', reaction_rate*dx/max_velocity   
    end if 
    
    write(log_unit,*)
end if
write (log_unit,*) '====================================' 
close (log_unit)
   
return
end subroutine

!> Subroutine to detect unphysical (numerical) oscillation 
subroutine detect_wiggle(val,             &
                         benchmark_val,   &
                         ncell,           &
                         extra_oscillation)
 use gtm_precision
 implicit none
 
real(gtm_real),intent(in) :: val(ncell)          !< numerical results which is to be checked for oscillation
real(gtm_real),intent(in) :: benchmark_val(ncell)!< benchmark values for  
integer,intent(out):: extra_oscillation          !< number of oscillation in val minus number of oscillation in benchmark
integer,intent(in) :: ncell                      !< number of cells                             
!---- local
integer :: nval
integer :: nbenchmark 
integer :: icell
real(gtm_real) :: phi_val(ncell-1)
real(gtm_real) :: phi_benchmark(ncell-1)

do icell=1,(ncell-1)
    phi_val(icell)       = val(icell) - val(icell+1)
    phi_benchmark(icell) = benchmark_val(icell)-benchmark_val(icell+1)
end do

nval = 0
nbenchmark = 0

do icell=1,ncell-2
    if (phi_val(icell)*phi_val(icell+1) < zero ) then
        nval = nval +1
    end if
    if (phi_benchmark(icell)*phi_benchmark(icell+1) < zero ) then
        nbenchmark = nbenchmark +1
    end if   
   
end do

extra_oscillation = nval - nbenchmark                  

return
end subroutine


end module 