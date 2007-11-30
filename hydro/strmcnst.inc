C!    Copyright (C) 1996, 1997, 1998 State of California,
C!    Department of Water Resources.
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

*===== BOF strmcnst.inc =================================================
*   Version 93.01, January, 1993

*   Note: "network.inc" must precede this include.

      INTEGER ConnectingExtremity( 1+MaxConnectingChannels )
      INTEGER ConnectingChannels
      INTEGER ConditionCode

      REAL*8 ConstraintCoef( 2+2*MaxConnectingChannels )
      REAL*8 ConstraintRightSide
      REAL*8 Discharge( 1+MaxConnectingChannels )
      REAL*8 WSElev( 1+MaxConnectingChannels )

      COMMON  /UserConstraintVariables_I/ 
     &     ConnectingExtremity,
     &     ConditionCode,ConnectingChannels

	  COMMON /UserConstraintVariables_R/
     &      ConstraintCoef,
     &     ConstraintRightSide,
     &     Discharge, WSElev

      SAVE / UserConstraintVariables_I /,
     &      /UserConstraintVariables_R/

*   Definitions:
*
*     ConditionCode - boundary-condition code.
*
*     ConstraintCoef(i) - implicit constraint-equation coefficients,
*                        multipliers of the following:
*       [1] discharge at constraint cross section,
*       [2] water-surface elevation at constraint cross section,
*       [3] discharge at first connecting node,
*       [4] water-surface elevation at first connecting cross section,
*       [3] discharge at second connecting node,
*       [4] water-surface elevation at second connecting cross section,
*           above pairs repeated for all connecting cross sections...
*
*     ConstraintRightSide - right-hand side of constraint equation.
*
*     Discharge(i) - current discharge at
*          [1] constraint node
*          [2] connection to first connecting channel,
*          [3] connection to second connecting channel,
*              repeating for all connectioning channels...
*
*     WSElev(i) - current water-surface elevation at
*          [1] constraint node
*          [2] connection to first connecting channel,
*          [3] connection to second connecting channel,
*              repeating for all connectioning channels...
*
*     ConnectingChannels - number of connecting channels
*
*     ConnectingExtremity(i) - index indicating
*
*          [+1] upstream end of channel, or
*          [-1] downstream end of channel,
*
*             for connecting channel i, where i indicates
*
*          [1] constraint channel,
*          [2] first connecting channel,
*          [3] second connecting channel,
*              repeating for all channels...
*
*
*     Note discharge sign convention:
*
*     Discharge is considered positive if flowing from upstream
*     to downstream end of channel.  ConnectingExtremity will be
*     +1 for and upstream end and -1 for a downstream end.
*
*===== EOF strmcnst.inc ================================================
