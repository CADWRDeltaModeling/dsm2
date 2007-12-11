C!    Copyright (C) 1996, 1997 State of California, Department of Water
C!    Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Dr. Paul
C!    Hutton, below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Dr. Paul Hutton, below,
C!    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
C!    02139, USA.
C!
C!    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
C!    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
C!    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
C!    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
C!    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
C!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
C!    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
C!    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
C!    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
C!    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
C!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
C!    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
C!    DAMAGE.
C!
C!    For more information about DSM2, contact:
C!
C!    Dr. Paul Hutton
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-5601
C!    hutton@water.ca.gov
C!
C!    or see our home page: http://wwwdelmod.water.ca.gov/

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



