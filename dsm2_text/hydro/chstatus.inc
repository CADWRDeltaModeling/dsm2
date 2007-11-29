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


*===== BOF chstatus.inc ==================================================
*   Version 93.01, January, 1993

*   Note: This include, if used, must appear after "Network.inc".

      REAL*8 WS(MaxLocations), Q(MaxLocations), H(MaxLocations)
      REAL*8 Rho1(MaxLocations), Rho2(MaxLocations)
      REAL*8 dXBernie(MaxChannels), dXForce(MaxChannels)
      REAL*8 QOld(MaxLocations)
      LOGICAL ConvergedSteady(MaxChannels)
      COMMON / ChnlFlowStatus /  WS, Q, QOLD,H, Rho1, Rho2, dXBernie,
     &     dXForce
	  COMMON /ChnlFlowConvStatus/ConvergedSteady
      SAVE / ChnlFlowStatus /,/ChnlFlowConvStatus/

*   Definitions:
*     WS(i) - water surface elevation at computational location "i".
*     Q(i) - volumetric discharge at computational location "i".
*     H(i) - depth of flow at computational location "i".
*     Rho1(i) - density at computational location "i", at the
*               begining of the current time increment.
*     Rho1(i) - density at computational location "i", at the
*               the end of the current time increment.
*     dXBernie(m) - last dX used for for solution of simple
*                   Bernoulli equation to obtain steady initial
*                   conditions.
*     dxForce(m) - last dX used for for solution of simple
*                  specific-force equation to obtain steady initial
*                  conditions.
*     ConvergedSteady(m) - .TRUE. if steady solution converged,
*                           otherwise .FALSE.

*===== EOF chstatus.inc ==================================================
