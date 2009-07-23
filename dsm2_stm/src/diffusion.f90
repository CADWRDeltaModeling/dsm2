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
! Record of revisions:
!     Date               Programmers                 Description of change
!     ====               ===========                 =====================
!     07/21/09          Kaveh Zamani               Version 0 - Basic structure
!                 and Fabian Bombardelli                of the code
!****************************************************************************

     implicit none

! Variables: data dictionary.  Define variables types, definitions and units

! C_s: concentration of sediment in suspension averaged in the cross section
! K_s(i): dispersion coefficient at the face of volume i
! Area(i): Area of cross section evaluated at the center of volume i
!          This is an output coming from HYDRO
! N_volumes: Number of volumes (computational cross sections) whose
!          sediment concentration will be computed
! dt_sed: Time step for the computation of sediment transport (in seconds)
! dx_sed: Spatial step for the computation of sediment transport (in feet)  
      
       INTEGER :: N_volumes = 100
       REAL, DIMENSION(N_volumes) :: Area
       REAL, DIMENSION(N_volumes+1) :: K_s
       REAL, DIMENSION(N_volumes) :: C_s
 
 ! Theta is included with a value for the time being 
       REAL :: Theta = 0.5 
       REAL :: dt_sed
       REAL :: dx_sed
       
       INTEGER :: Boundary_condition_flag_upstream
       INTEGER :: Boundary_condition_flag_downstream

!CCCCCCCCCCCCCC  Temporary statements while the code is not connected to Hydro CCC Begining
! We assume a value of N_volumes and other variables for the time being   
       dt_sed = 10
       dx_sed = 20
!CCCCCCCCCCCCCC  Temporary statements while the code is not connected to Hydro CCC End

! Place holder for input of data from HYDRO
! get_Area_Hydro
! get_Ks_Hydro
! We need to discuss how to compute K_s from the output of HYDRO

! BODY OF DIFFUSION

! Computation of matrix for internal nodes 

! Assembling the matrix

! Call for the tri-diagonal solver
        call TRIDI_SOLVER(A, B, C, D, N, X)
! Output the results

        end program diffusion


!*****************************************************************************************    
        SUBROUTINE TRIDI_SOLVER(A, B, C, D, N, X)
    
!    I (A,B,C,D,N,
!    O     X)
!+++  PURPOSE +++
! SOLVE OF A TRIDIAGONAL MATRIX FOR X
!             [A1,C2,00,00,00]  
! [X1,X2,...] [B1,A2,C3,00,00] = [D1,D2,....]
!             [00,00,B3,A4,C5]  
! +++ ARGUMENTS+++

        INTEGER :: N
        REAL :: A(10000),B(10000),C(10000),D(10000),X(10000)

!+++ ARGUMENT DEFINITIONS +++
! A: DIAGONAL COEFFICIENTS
! B: COEFFICIENT BELOW DIAGONAL
! C: COEFFICIENT ABOVE DIAGONAL
! D: CONSTANT TERM 
! X: UNKNOWN OR SOLUTION 

!+++ LOCAL VARIABLES ++++
        INTEGER :: K,KK
        REAL :: P(10000),R(10000),T(10000),V(10000)
!
! SOLVE
!
        P(1)=A(1)
        FIRST_3_DIA: DO K=1,N-1
           R(K)=B(K) 
        FIRST_3_DIA CONTINUE 
!
        SECOUND_3_DIA: DO K=2,N
          T(K)=C(K)/P(K-1)
          P(K)=A(K)-(T(K)*V(K-1))
        SECOUND_3_DIA CONTINUE
!
        V(1)=D(1)
        THIRD_3_DIA: DO K=2,N
          V(K)=D(K)-(T(K)*V(K-1))
        THIRD_3_DIA CONTINUE
!
        X(N)=V(N)/P(N)
        FORTH_3_DIA DO KK=1,N-1
          K=N-KK
          X(K)=(V(K)-R(K)*X(K+1)))/P(K)
        FORTH_3_DIA CONTINUE
!
RETURN
END 
  

!*****************************************************************************************
    
    

