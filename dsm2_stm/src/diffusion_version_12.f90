     PROGRAM diffusion

! Copyright (C) 1996, 1997, 1998, 2001, 2007, 2009 State of California,
! Department of Water Resources.
! This file is part of DSM2.

! The Delta Simulation Model 2 (DSM2) is free software: 
! you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! DSM2 is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with DSM2.  If not, see <http://www.gnu.org/licenses>.

! Delta Modeling Section 
! Modeling Support Branch
! Bay-Delta Office
! California Department of Water Resources
! http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/index.cfm
!
! PURPOSE:
!
! It solves the dispersion of sediment in the longitudinal direction
! Partial_(A C_s)/Partial_t = Partial/Partial_x [A K_s Partial_(C_s)/Partial_x]
! 
! These sub-routines are being developed by the Department of Civil and 
! Environmental Engineering at the University of California, Davis and DWR
!
! Record of revisions:
!       Date             Programmers                 Description of change
!       ====             ===========                 =====================
!     07/21/09          Kaveh Zamani               Version 0 - Basic structure
!                 and Fabian Bombardelli                of the code
!
!     07/22/09          Kaveh Zamani               More elements of the code
!                 and Fabian Bombardelli
!
!     07/26/09        Fabian Bombardelli        Computation of values of matrix
!     07/27/09        Fabian Bombardelli        Boundary conditions and solutions  
!     07/28/09        Fabian Bombardelli        Case 1 and final set up
!     07/29/09        Fabian Bombardelli        Case 2 and final set up
!     07/30/09        Fabian Bombardelli        Optimizing Case 2
!****************************************************************************

          IMPLICIT NONE

! Variables: data dictionary.  Define variables types, definitions and units

! C_s_n_plus_one(i): concentration of sediment in suspension averaged in the volume i
!      for time t + 1
! C_S_n(i): concentration of sediment in suspension averaged in the volume
!         for time t
! K_s_n_plus_one(i): dispersion coefficient at the left face of volume i for time t + 1
! K_s_n(i): dispersion coefficient at the left face of volume i for time t
! Area_n_plus_one(i): Area of cross section evaluated at the left face of volume i at time
!                     t + 1. This is an output coming from HYDRO
! Area_n(i): Area of cross section evaluated at the left face of volume i at time
!            t. This is an output coming from HYDRO
! A(i): Values of the coefficients below diagonal in matrix
! B(i): Values of the coefficients at the diagonal in matrix
! C(i): Values of the coefficients above diagonal in matrix
! D(i): Values of the independent term
! N_volumes: Number of volumes (computational cross sections) whose
!          sediment concentration will be computed
! dt_sed: Time step for the computation of sediment transport (in seconds)
! dx_sed: Spatial step for the computation of sediment transport (in feet for STM)  

       INTEGER, PARAMETER :: N_volumes = 21
       INTEGER :: N
       INTEGER :: i, j, N_times, ak
       INTEGER :: i_case
       INTEGER :: Boundary_condition_flag_upstream
       INTEGER :: Boundary_condition_flag_downstream
       
       REAL, DIMENSION(N_volumes+1) :: Area_n, Area_n_plus_one
       REAL, DIMENSION(N_volumes+1) :: K_s_n, K_s_n_plus_one
       REAL, DIMENSION(N_volumes) :: C_s_n, C_s_n_plus_one
       REAL, DIMENSION(N_volumes) :: A, B, C, D, X
       
       REAL, PARAMETER :: Pi = 3.1415926535897932384626433832795
       
       REAL :: dt_sed
       REAL :: dx_sed
       REAL :: Theta
       REAL :: Cal_D, Cal_D_one
       
       REAL :: Time
       REAL :: C_s_upstream, C_s_downstream
       
       REAL :: ax
       REAL :: Flux_s_upstream_n, Flux_s_upstream_n_plus_one
                    
!CCCCCCCCCCCCCC  Temporary statements while the code is not connected to Hydro CCC Beginning
! We assume a value of N_volumes and other variables for the time being   
         Theta = 0.5
         dt_sed = 0.001
!CCCCCCCCCCCCCC  Temporary statements while the code is not connected to Hydro CCC End

! Place holder for input of data from HYDRO
! get_Area_Hydro
! get_Ks_Hydro
! We need to discuss how to compute K_s from the output of HYDRO

!CCCC
! BODY OF SUB-ROUTINE DIFFUSION
          i_case = 2

          if(i_case.eq.1)dx_sed = 0.05
          if(i_case.eq.2)dx_sed = 0.045
     
          N = N_volumes
          Cal_D = Theta * dt_sed / dx_sed /dx_sed
          Cal_D_one = (1 - Theta) * dt_sed / dx_sed / dx_sed

!CCCCCCCCCCCCCC  Temporary statements while the code is not connected to Hydro CCC Beginning
        open(3,file='results.dat',status='unknown')

        do i=1,N+1
           Area_n(i) = 1.0d0
           Area_n_plus_one(i) = 1.0d0
           K_s_n(i) = 1.0d0
           K_s_n_plus_one(i) = 1.0d0
        end do

! Initial condition
     if(i_case.eq.1) then
       do ak=1, (N - 1) / 2 + 1
         C_s_n(ak) = 1.0d0/0.5d0 * (ak - 1) * dx_sed
       end do
              
       do ak=(N - 1) / 2 + 2, N
         C_s_n(ak) = 2.0d0 - 1.0d0/0.5d0 * (ak - 1) * dx_sed
       end do
       C_s_n(N)=0.0d0
     end if
     
     if(i_case.eq.2)then
       do ak=1, N
         ax = (ak - 1) * dx_sed + 0.1
         C_s_n(ak) = 2.0d0 * ax + 4.0d0 * cos(0.5d0 * Pi * ax)
!         print *, C_s_n(ak)
       end do
     end if

!     do ak=1, N
!       print *, C_s_n(ak)
!     end do

! A time-advancing stage is started here to validate the code

        N_times = 500

       do j=1,N_times

         Time = dt_sed * j
         
         write(3,*)j,dt_sed,Time
         write(3,*)


!CCCCCCCCCCCCCC  Temporary statements while the code is not connected to Hydro CCC End

! Initialization of values of the matrix
         do i=1,N
           A(i)=0.0d0
           B(i)=0.0d0
           C(i)=0.0d0
           D(i)=0.0d0
           X(i)=0.0d0
         end do

! Computation of all elements of matrix 
       call values(A, B, C, D, N, Cal_D, Cal_D_one, Area_n, Area_n_plus_one, K_s_n, K_s_n_plus_one, C_s_n)

        do i=1,N
          print *, i, A(i), B(i), C(i), D(i)
        end do
!          print *, Cal_D, Cal_D_one

! Imposing the boundary conditions

       if(i_case.eq.1)then
           Boundary_condition_flag_upstream = 1
           Boundary_condition_flag_downstream = 1
           C_s_upstream = 0.0d0
           C_s_downstream = 0.0d0
       end if

       if(i_case.eq.2)then
           Boundary_condition_flag_upstream = 0
           Boundary_condition_flag_downstream = 1
           Flux_s_upstream_n = 2.0d0 - 2.0d0 * Pi * sin(0.05d0 * Pi) * exp(-1.0d0 * Pi * Pi / 4.0d0 * (Time - dt_sed))
           Flux_s_upstream_n_plus_one = 2.0d0 - 2.0d0 * Pi * sin(0.05d0 * Pi) * exp(-1.0d0 * Pi * Pi / 4.0d0 * Time)
           
           write(*,*)
           print *, Time, Flux_s_upstream_n, Flux_s_upstream_n_plus_one
           
           C_s_downstream = 2.0d0
       end if

       call boundary_conditions(Boundary_condition_flag_upstream, Boundary_condition_flag_downstream, A, B, C, D, N, C_s_upstream, C_s_downstream, Cal_D, Cal_D_one, Area_n, K_s_n, Area_n_plus_one, K_s_n_plus_one, Flux_s_upstream_n, Flux_s_upstream_n_plus_one, dx_sed, C_s_n)

        do i=1,N
          print *, i, A(i), B(i), C(i), D(i)
        end do
          print *, Cal_D, Cal_D_one

! Call for the tri-diagonal solver
        call TRIDI_SOLVER(A, B, C, D, N, X)
! Output the results
        do i=1, N
         write(3,*)X(i)
         C_s_n(i)=X(i)
        end do
        write(3,*)

        end do

        end program diffusion

!*****************************************************************************************
        SUBROUTINE values(A, B, C, D, N, Cal_D, Cal_D_one, Area_n, Area_n_plus_one, K_s_n, K_s_n_plus_one, C_s_n)

! Variables: data dictionary.  Define variables types, definitions and units

! Area_aux_n: Area evaluated at the center of the volume i at time t
! Areas_aux_n_plus_one: Area evaluated at the center of the volume i at time t + 1
! A(i): Values of the coefficients below diagonal in matrix
! B(i): Values of the coefficients at the diagonal in matrix
! C(i): Values of the coefficients above diagonal in matrix
! D(i): Values of the independent term

          INTEGER, PARAMETER :: N_volumes = 21
          INTEGER N
          INTEGER i, j, N_times, ak
          REAL, DIMENSION(N_volumes) :: A, B, C, D, X
          REAL, DIMENSION(N_volumes+1) :: Area_n, Area_n_plus_one
          REAL, DIMENSION(N_volumes+1) :: K_s_n, K_s_n_plus_one
          REAL, DIMENSION(N_volumes) :: C_s_n, C_s_n_plus_one
          REAL :: Area_aux_n, Area_aux_n_plus_one
          
          do ak=1, N
            print *, C_s_n(ak)
          end do
          
           do i=1,N
            Area_aux_n_plus_one = (Area_n_plus_one(i) + Area_n_plus_one(i+1)) / 2.0d0
            Area_aux_n = (Area_n(i) + Area_n(i+1)) / 2.0d0
            B(i)=Area_aux_n_plus_one + Cal_D * Area_n_plus_one(i+1) * K_s_n_plus_one(i+1) 
            B(i)=B(i) + Cal_D * Area_n_plus_one(i) * K_s_n_plus_one(i) 
            A(i)= - 1.0d0 * Cal_D * Area_n_plus_one(i) * K_s_n_plus_one(i)
            C(i)= - 1.0d0 * Cal_D * Area_n_plus_one(i+1) * K_s_n_plus_one(i+1)
            D(i)=Area_aux_n * C_s_n(i) + Cal_D_one * C_s_n(i+1) * Area_n(i+1) * K_s_n(i+1)
            D(i)= D(i) -  Cal_D_one * C_s_n(i) * Area_n(i+1) * K_s_n(i+1)
            D(i)= D(i) -  Cal_D_one * C_s_n(i) * Area_n(i) * K_s_n(i)
            D(i)= D(i) +  Cal_D_one * C_s_n(i-1) * Area_n(i) * K_s_n(i)
           end do
                 
        RETURN
        END
        

!*****************************************************************************************    
        SUBROUTINE boundary_conditions(Boundary_condition_flag_upstream, Boundary_condition_flag_downstream, A, B, C, D, N, C_s_upstream, C_s_downstream, Cal_D, Cal_D_one, Area_n, K_s_n, Area_n_plus_one, K_s_n_plus_one, Flux_s_upstream_n, Flux_s_upstream_n_plus_one, dx_sed, C_s_n)
        
           INTEGER, PARAMETER :: N_volumes = 21
           INTEGER :: Boundary_condition_flag_upstream
           INTEGER :: Boundary_condition_flag_downstream
           REAL, DIMENSION(N_volumes) :: A, B, C, D, X
           REAL, DIMENSION(N_volumes+1) :: Area_n, Area_n_plus_one
           REAL, DIMENSION(N_volumes+1) :: K_s_n, K_s_n_plus_one
           REAL :: C_s_upstream, C_s_downstream
           REAL :: Flux_s_upstream_n, Flux_s_upstream_n_plus_one
           REAL :: dx_sed
           REAL :: Cal_D, Cal_D_one
           REAL, DIMENSION(N_volumes) :: C_s_n, C_s_n_plus_one
           
           print *, Flux_s_upstream_n, Flux_s_upstream_n_plus_one, dx_sed
           print *, Cal_D, Cal_D_one
           print *, Area_n_plus_one(1), Area_n(1)
           print *, C(1)

           If(Boundary_condition_flag_upstream.eq.1)then
             B(1) = 1.0d0
             C(1) = 0.0d0
             D(N) = C_s_upstream
           end if
        
           If(Boundary_condition_flag_upstream.eq.0)then
             B(1) = B(1)
             C(1) = C(1) - Cal_D * Area_n_plus_one(1) * K_s_n_plus_one(1)
             D(1) = D(1) - 2.0d0 * dx_sed * Flux_s_upstream_n_plus_one * Cal_D * Area_n_plus_one(1) * K_s_n_plus_one(1)
             D(1) = D(1) - 2.0d0 * dx_sed * Flux_s_upstream_n * Cal_D_one * Area_n(1) * K_s_n(1)
             D(1) = D(1) + Cal_D * Area_n(1) * K_s_n(1) * C_s_n(2) 
           end if
           
           If(Boundary_condition_flag_downstream.eq.1)then
             B(N)=1.0d0
             A(N)= 0.0d0
             D(N) = C_s_downstream
           end if
        
           If(Boundary_condition_flag_downstream.eq.0)then
                
           end if
           

        RETURN
        END

!*****************************************************************************************    
        SUBROUTINE TRIDI_SOLVER(A, B, C, D, N, X)
    
!    Input (A,B,C,D,N,
!    Output     X)
!
! PURPOSE:

! Solves a tridiagonal system for X. This sub-routine was taken from the Numerical Recipes,
! page 43 (Edition of 1992)
! 
!             [B1,C1,00,00,00]  
! [X1,X2,...] [A2,B2,C2,00,00] = [D1,D2,....]
!             [00,A3,B3,C3,00]  
!             [00,00,A4,B4,C4]
!             [00,00,00,A5,B5]    

! Variables:
!
! A(i): Values of the coefficients below diagonal in matrix
! B(i): Values of the coefficients at the diagonal in matrix
! C(i): Values of the coefficients above diagonal in matrix
! D(i): Values of the independent term 
! X(i): Values of the computed solution

        INTEGER :: N
        REAL, DIMENSION(N) :: A, B, C, D, X
        INTEGER :: K, KK
        REAL, DIMENSION(N) :: gam
        REAL :: bet
        
        if(B(1).eq.0)then
          print *, 'There is a problem here'
          stop
        end if
        
        bet = B(1)
        X(1) = D(1) / bet
        
        do K=2, N
          gam(K) = C(K - 1) / bet
          bet = B(K) - A(K) * gam(K)
          if(bet.eq.0)then
            print *, 'Tridiagonal solver failed'
          end if
          X(K) = (D(K) - A(K) * X(K - 1)) / bet 
        end do
      
        do KK= N-1, 1, -1
          X(KK) = X(KK) - gam(KK + 1) * X(KK + 1)
        end do
      
       RETURN
       END 
!*****************************************************************************************
    
    

