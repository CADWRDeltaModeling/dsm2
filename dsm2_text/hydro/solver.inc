C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
C!
C!    Delta Simulation Model 2 (DSM2): A River, Estuary, and Land
C!    numerical model.  No protection claimed in original FOURPT and
C!    Branched Lagrangian Transport Model (BLTM) code written by the
C!    United States Geological Survey.  Protection claimed in the
C!    routines and files listed in the accompanying file "Protect.txt".
C!    If you did not receive a copy of this file contact Tara Smith,
C!    below.
C!
C!    This program is licensed to you under the terms of the GNU General
C!    Public License, version 2, as published by the Free Software
C!    Foundation.
C!
C!    You should have received a copy of the GNU General Public License
C!    along with this program; if not, contact Tara Smith, below,
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
C!    Tara Smith
C!    California Dept. of Water Resources
C!    Division of Planning, Delta Modeling Section
C!    1416 Ninth Street
C!    Sacramento, CA  95814
C!    916-653-9885
C!    tara@water.ca.gov
C!
C!    or see our home page: http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/


*===== BOF Solver.inc ================================================
*   Version 98.01, July, 1998

*   Note: This include, when used, must follow after "Network.inc".

        INTEGER   Matrix
        INTEGER MASSEQ(4,MaxLocations)
        INTEGER DYNMEQ(4,MaxLocations)
        INTEGER   MASSEQROW(MaxLocations)
        INTEGER   DYNMEQROW(MaxLocations)
c-------fixme: make the following limits better
        INTEGER   ConstraintPointers(7*MaxChannels)
        INTEGER   ResEqPointer(3*MaxResConnectChannel*MaxNRes + MaxNRes)
        INTEGER   Obj2objEqPointer(100)
        INTEGER   ResEqIndex(MaxNRes), obj2objEqIndex(20)
        INTEGER   UpConstraintEq(MaxChannels)
        INTEGER   DownConstraintEq(MaxChannels)
        INTEGER   UpConstraintIndex(MaxChannels)
        INTEGER   DownConstraintIndex(MaxChannels)
        INTEGER   EqPointer(MaxChannels)

c-------Don't change the size of these variables!!!!
        REAL*8 XX(2*MaxLocations+MaxNRes)
        REAL*8 X(2*MaxLocations+MaxNRes)
        REAL*8 XOld(2*MaxLocations+MaxNRes), XAdj(2*MaxLocations+MaxNRes)
        REAL*8 ColumnScale(2*MaxLocations+MaxNRes),RowScale(2*MaxLocations+MaxNRes)
        REAL*8 ZUnScaleFactor,ZscaleFactor,MassScaleFactor,DynScaleFactor,ResScaleFactor
        REAL*8 Rescale

        PARAMETER (DynScaleFactor=1./1024.,MassScaleFactor = 1./512.,ZUnscaleFactor=1024.
     &       ,ZScaleFactor = 1./1024., ResScaleFactor = 1./128.)
        COMMON /NetworkCoef/ X,XX,XOld,XAdj,RowScale,ColumnScale,Rescale
        SAVE   /NetworkCoef/
        COMMON /EqSequence/ Matrix,MassEqRow, DynmEqRow
     &       ,MassEq, DynmEq,UpConstraintEq, DownConstraintEq
     &       ,EqPointer,ConstraintPointers,ResEqPointer,obj2objEqPointer
     &       ,UpConstraintIndex, DownConstraintIndex,ResEqIndex,obj2objEqIndex
        SAVE   /EqSequence/

*   Definitions:
*     MaxLocations - maximum number of computational or user locations.
*     NUMFLT - current number of elements in skyline matrix.
*     IDIAG - column number of the left extent ot the Ith row.
*     LE - row number of the the upper extent of the Ith column.
*     IB - current maximum bandwidth.
*     ErrUnit - FORTRAN unit number for output from solver module.
*     Equations - current number of equations in solution matrix.
*     IndexedNetworkSolver - flag indicating,
*                             [ .TRUE. ] indexed solver is in use, or
*                             [ .FALSE. ] standard solver is in use.

*     MASSEQ - number (pointer to element of solution matrix) of the conservation of
*              mass equation for a subdomain
*     DYNMEQ - number (pointer to element of solution matrix) of the dynamic equation
*              for a subdomain
*     EqPointer(i) - number of first equation in channel i.
*     UpConstraintEq(i) - pointer to the (row of solution matrix) for the
*                         upstream boundary constraint for current
*                         channel.
*     DownConstraintEq(i) - pointer to the (row of solution matrix) for the
*                           downstreamboundary constraint.
*     X(j) - right-hand-side load vector.
*     XOld(j) - contribution to X, unchanging over time step.
*     XAdj(j) - contribution to X, changing with iteration during time
*               step.

