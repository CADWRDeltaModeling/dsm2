C!<license>
C!    Copyright (C) 1996, 1997, 1998, 2001, 2007 State of California,
C!    Department of Water Resources.
C!    This file is part of DSM2.

C!    DSM2 is free software: you can redistribute it and/or modify
C!    it under the terms of the GNU General Public !<license as published by
C!    the Free Software Foundation, either version 3 of the !<license, or
C!    (at your option) any later version.

C!    DSM2 is distributed in the hope that it will be useful,
C!    but WITHOUT ANY WARRANTY; without even the implied warranty of
C!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C!    GNU General Public !<license for more details.

C!    You should have received a copy of the GNU General Public !<license
C!    along with DSM2.  If not, see <http://www.gnu.org/!<licenses/>.
C!</license>

*===== BOF Solver.inc ================================================
*   Version 98.01, July, 1998

*   Note: This include, when used, must follow after "Network.inc".

        integer   Matrix
        integer   MassEq(4,MaxLocations)
        integer   DynmEq(4,MaxLocations)
        integer   MassEqRow(MaxLocations)
        integer   DynmEqRow(MaxLocations)
! Total number of rows (equations) dedicated to channels.
		integer   TotalChanRows
! Total number of rows (equations) dedicated to channels and reservoirs		      
		integer   TotalChanResRows   
c-------fixme: make the following limits better
        integer   ConstraintPointers(7*MaxChannels)
        integer   ResEqPointer(6*MaxResConnectChannel*MaxNRes + MaxNRes)
        integer   Obj2objEqPointer(100)
		!   based on reservoir gate with 5 pipes and 5 weirs per gate
		integer,parameter :: MaxGatePtr=MaxNGate*5*4+ MaxNGate*5*4+MaxNgate*5
		integer :: GateEqPointer(MaxGatePtr)
        integer   ResEqIndex(MaxNRes),ResEqRow(MaxNRes), obj2objEqIndex(20)
		integer   GateEqIndex(MaxNGate),GateEqRow(MaxNGate),GateNodeRow(MaxNGate)
        integer   UpConstraintEq(MaxChannels)
        integer   DownConstraintEq(MaxChannels)
        integer   UpConstraintIndex(MaxChannels)
        integer   DownConstraintIndex(MaxChannels)
        integer   EqPointer(MaxChannels)
		integer, parameter :: MAX_EQUATION=(2*MaxLocations+MaxNRes*(1+MaxResConnectChannel))

        logical*4 NormClose
		real*8 LInfNorm,LastLInfNorm,L2Norm,LastL2Norm
c-------Don't change the size of these variables!!!!
        real*8 XX(2*MaxLocations+MaxNRes*(1+MaxResConnectChannel))
        real*8 X(MAX_EQUATION)
        real*8 XOld(MAX_EQUATION)
		real*8 XAdj(MAX_EQUATION)
        real*8 ColumnScale(MAX_EQUATION),RowScale(MAX_EQUATION)
        real*8, parameter :: ZscaleFactor=1.D0      ! 1.D0/(1024.D0)
		real*8, parameter :: MassScaleFactor= 1.D0/(32.D0*1024.D0)
		real*8, parameter :: DynScaleFactor=1.D0/(1024.D0)
		real*8, parameter :: ResScaleFactor=1.D0
		real*8, parameter :: ResConnectScaleFactor=128.D0
        real*8  Rescale
        COMMON /NClose/ NormClose
		COMMON /LInfNorm/ LInfNorm,LastLInfNorm,L2Norm,LastL2Norm
        COMMON /NetworkCoef/ X,XX,XOld,XAdj,RowScale,ColumnScale,Rescale
        SAVE   /NetworkCoef/
        COMMON /EqSequence/ Matrix,MassEqRow, DynmEqRow
     &       ,MassEq, DynmEq,UpConstraintEq, DownConstraintEq
     &       ,EqPointer,ConstraintPointers,ResEqPointer,ResEqIndex
     &	     ,obj2objEqPointer,GateEqPointer
     &       ,UpConstraintIndex, DownConstraintIndex,ResEqRow
     &       ,obj2objEqIndex,GateEqIndex
     &       ,GateEqRow,GateNodeRow, TotalChanRows,TotalChanResRows
        SAVE   /EqSequence/


      integer Equations, ErrUnit
      COMMON /Equation/ Equations,ErrUnit
      SAVE   /Equation/



